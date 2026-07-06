## jaspModuleTools — Local Repo Server (Phase 1)
## ==========================================================================
## Pure streaming proxy that exposes a merged, version-pinned CRAN-like
## repository. Two frozen sources per version: RSPM (date) and jasp-repo
## (arbitrary string). External source URLs from the lockfile are a
## source-only fallback.
##
## R's install.packages() / pkgdepends talk to it like any normal repo — no
## renv internals. The server holds only in-memory metadata (merged PACKAGES +
## a session map). Every binary/source is fetched from upstream on demand and
## streamed to the client; nothing is written to disk.
##
## See inst/tools/SPEC.md (v2.1) for the full design.
##
## Run (dev mode):
##   source("inst/tools/repo_server.R")
##   start_repo_server()                     # auto-detects platform
##   start_repo_server(distro = "noble")     # try Linux binaries
##   stop_repo_server()
##
## In an installed package, use jaspModuleTools::start_jasp_development().
##
## Dependencies: httpuv, curl, jsonlite (declared in DESCRIPTION in Phase 2).
## ==========================================================================

## Null-coalescing helper (kept local to avoid importing rlang).
`%||%` <- function(a, b) if (is.null(a)) b else a

## Ensure a package namespace is available, with a clear error if not.
ensure_namespace <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    stop("Package '", pkg, "' is required by the repo server but is not installed.")
  invisible(TRUE)
}


## --------------------------------------------------------------------------
## Global server state
## --------------------------------------------------------------------------
##
## One environment holds everything; the server's lifetime is the R session
## that started it. No persistence, no crash recovery.

.repo_server_state <- new.env(parent = emptyenv())
.repo_server_state$config       <- NULL   # parsed repos.json
.repo_server_state$platform     <- NULL   # resolved platform params (see resolve_platform)
.repo_server_state$github_pat   <- NULL   # resolved PAT (or NULL)
.repo_server_state$merged_cache <- list() # version -> merged entries (lazy)
.repo_server_state$sessions     <- new.env(parent = emptyenv())  # session id -> session
.repo_server_state$server       <- NULL   # httpuv handle
.repo_server_state$host         <- "127.0.0.1"
.repo_server_state$port         <- 8765L
.repo_server_state$cache_path   <- NULL   # NULL disables caching; set via param/option


## --------------------------------------------------------------------------
## 1. Configuration (repos.json)
## --------------------------------------------------------------------------

DEFAULT_CONFIG_URL <- "https://raw.githubusercontent.com/jasp-stats/jasp-repo-config/main/repos.json"

## Fetch + validate the config. Accepts an http(s) URL or a local file path
## (the latter makes the server testable before the GitHub config repo exists).
fetch_config <- function(config_url = DEFAULT_CONFIG_URL) {
  text <- if (grepl(r"{^(https?|ftp)://}", config_url)) {
    ensure_namespace("curl")
    resp <- curl::curl_fetch_memory(config_url)
    if (resp$status_code != 200L)
      stop("Failed to fetch repos.json (HTTP ", resp$status_code, "): ", config_url)
    rawToChar(resp$content)
  } else {
    if (!file.exists(config_url))
      stop("Config file not found: ", config_url)
    paste(readLines(config_url, warn = FALSE), collapse = "\n")
  }

  ensure_namespace("jsonlite")
  cfg <- jsonlite::fromJSON(text, simplifyVector = FALSE)
  validate_config(cfg)
  cfg
}

## Structural validation of the parsed config. Source of truth: SPEC §1.
validate_config <- function(cfg) {
  required_top <- c("rspm", "jasp-repo", "versions", "latest")
  missing <- setdiff(required_top, names(cfg))
  if (length(missing))
    stop("repos.json missing top-level keys: ", paste(missing, collapse = ", "))

  if (is.null(cfg$rspm$base_url))
    stop("repos.json: rspm.base_url is required")
  if (is.null(cfg[["jasp-repo"]]$url))
    stop("repos.json: jasp-repo.url is required")
  if (!is.character(cfg$versions) && !is.list(cfg$versions))
    stop("repos.json: 'versions' must be an object")
  if (length(cfg$versions) == 0L)
    stop("repos.json: 'versions' must define at least one version")
  if (!(cfg$latest %in% names(cfg$versions)))
    stop("repos.json: 'latest' (\"", cfg$latest, "\") is not present in 'versions'")

  for (v in names(cfg$versions)) {
    pins <- cfg$versions[[v]]
    if (is.null(pins$rspm))
      stop("repos.json: version \"", v, "\" is missing the 'rspm' pin")
    if (is.null(pins[["jasp-repo"]]))
      stop("repos.json: version \"", v, "\" is missing the 'jasp-repo' pin")
  }

  invisible(TRUE)
}

## Substitute a version's RSPM date into the base_url template.
rspm_base_url <- function(version) {
  cfg <- .repo_server_state$config
  date <- cfg$versions[[version]]$rspm
  sub("{date}", date, cfg$rspm$base_url, fixed = TRUE)
}

## Substitute the version + platform params into the jasp-repo url template.
jasp_url <- function(version) {
  cfg   <- .repo_server_state$config
  plat  <- .repo_server_state$platform
  label <- cfg$versions[[version]][["jasp-repo"]]
  url <- cfg[["jasp-repo"]]$url
  url <- sub("{version}",   label,              url, fixed = TRUE)
  url <- sub("{r_version}", plat$r_version_jasp, url, fixed = TRUE)
  url <- sub("{os}",        plat$jasp_os,       url, fixed = TRUE)
  url <- sub("{arch}",      plat$arch,          url, fixed = TRUE)
  url
}

## Map a requested version label to a real version key. "latest" -> configured
## latest. Returns NULL if unknown.
resolve_version <- function(v) {
  cfg <- .repo_server_state$config
  if (is.null(cfg)) return(NULL)
  if (isTRUE(v == "latest")) return(cfg$latest)
  if (v %in% names(cfg$versions)) return(v)
  NULL
}


## --------------------------------------------------------------------------
## 2. Platform resolution
## --------------------------------------------------------------------------

## Normalise the raw machine string to the arch tokens used by both RSPM and
## jasp-repo: "arm64" or "x86_64".
normalize_arch <- function(a) {
  if (is.null(a)) return("x86_64")
  a <- tolower(a)
  if (a %in% c("x86_64", "amd64")) return("x86_64")
  if (a %in% c("aarch64", "arm64")) return("arm64")
  a
}

## Build the RSPM binary path segment for the current platform.
##   macOS (R >= 4.3): macosx/big-sur-{arch}/contrib/{r_version}
##   macOS (legacy)  : macosx/contrib/{r_version}
##   Windows         : windows/contrib/{r_version}
##   Linux/Flatpak   : linux/{distro}-{arch}/contrib/{r_version}  (NULL if no distro -> source-only)
## Returns NULL when no sensible binary path can be built (caller then serves
## source-only, matching the "404 -> source" policy).
build_binary_path <- function(os, arch, r_version, macos_platform, distro) {
  if (os == "Windows")
    return(sprintf("windows/contrib/%s", r_version))
  if (os == "MacOS") {
    if (!is.null(macos_platform))
      return(sprintf("macosx/%s/contrib/%s", macos_platform, r_version))
    return(sprintf("macosx/contrib/%s", r_version))
  }
  if (os %in% c("Linux", "Flatpak")) {
    if (is.null(distro)) return(NULL)  # can't build path; rely on source fallback
    return(sprintf("linux/%s-%s/contrib/%s", distro, arch, r_version))
  }
  NULL
}

## Resolve every platform parameter from the runtime environment at startup.
## `Sys.info()`, `R.Version()`, plus caller overrides.
resolve_platform <- function(os = NULL, arch = NULL, r_version = NULL, distro = NULL) {
  sysname <- Sys.info()[["sysname"]]
  if (is.null(os)) {
    os <- switch(sysname,
      "Darwin"  = "MacOS",
      "Windows" = "Windows",
      "Linux"   = "Linux",
      sysname)
    if (nzchar(Sys.getenv("FLATPAK_ID"))) os <- "Flatpak"  # JASP sandbox
  }

  if (is.null(arch))
    arch <- normalize_arch(Sys.info()[["machine"]])

  if (is.null(r_version)) {
    rv <- R.Version()
    # "4" + "." + first component of minor ("5" from "5.0") -> "4.5"
    minor_first <- strsplit(as.character(rv$minor), "\\.")[[1L]][1L]
    r_version <- paste0(rv$major, ".", minor_first)
  }

  # macOS platform subdir: derive from R's own pkgType (R 4.3+).
  # R 4.3-4.5 uses "big-sur-{arch}", R 4.6+ uses "sonoma-{arch}".
  # contrib.url("", "binary") returns e.g. "bin/macosx/big-sur-arm64/contrib/4.5"
  # — we strip the leading "bin/".
  macos_platform <- NULL
  if (os == "MacOS" && utils::compareVersion(r_version, "4.3") >= 0) {
    cu <- utils::contrib.url("", type = "binary")
    macos_platform <- sub("^bin/macosx/", "", sub("/contrib/.*", "", cu))
  }

  # jasp-repo uses its own os labels (matches the legacy getRemoteCellarURLs)
  jasp_os <- os

  list(
    os              = os,
    arch            = arch,
    r_version       = r_version,
    r_version_jasp  = paste0("R-", r_version),  # jasp-repo uses "R-4.5"
    jasp_os         = jasp_os,
    distro          = distro,
    macos_platform  = macos_platform,
    binary_path     = build_binary_path(os, arch, r_version, macos_platform, distro)
  )
}

resolve_github_pat <- function(pat = NULL) {
  if (!is.null(pat) && nzchar(pat)) return(pat)
  for (envvar in c("GITHUB_PAT", "GITHUB_TOKEN")) {
    v <- Sys.getenv(envvar)
    if (nzchar(v)) return(v)
  }
  NULL
}


## --------------------------------------------------------------------------
## 3. PACKAGES (DCF) parsing & serialization
## --------------------------------------------------------------------------
##
## We keep each package as a RAW TEXT block (the exact DCF excerpt) plus the
## parsed Package/Version and an origin tag. This preserves every original
## field (Depends, Imports, ...) for RSPM passthrough, and lets us emit minimal
## `Package:` + `Version:` blocks for jasp-repo.

## Split a PACKAGES document into its DCF entries (blank-line separated).
split_dcf_blocks <- function(text) {
  text <- gsub("\r\n", "\n", text)
  blocks <- unlist(strsplit(text, "\n[ \t]*\n+"))
  blocks <- trimws(blocks)
  blocks <- blocks[nzchar(blocks)]
  if (length(blocks) == 0L) character(0) else blocks
}

## (Field extraction is handled by read.dcf in parse_packages_to_entries.)

## Parse a PACKAGES document into a list of entry objects.
## Each entry: list(Package, Version, raw, origin, priority[, rec])
##
## Uses read.dcf (C-optimized) to pull Package/Version for all rows in one
## pass, and a single strsplit to keep the raw DCF blocks for passthrough.
## This avoids O(n) regex calls that made large PACKAGES (~22k entries) take
## ~30s; now it's sub-second.
parse_packages_to_entries <- function(text, origin, priority) {
  blocks <- split_dcf_blocks(text)
  if (length(blocks) == 0L) return(list())

  # Fast bulk field extraction via read.dcf (C-optimized). We only need
  # Package/Version for merge decisions; the raw block (from split_dcf_blocks)
  # preserves every original field for passthrough.
  df <- tryCatch(
    read.dcf(textConnection(text)),
    error = function(e) NULL
  )
  if (is.null(df) || nrow(df) == 0L) return(list())

  # read.dcf and split_dcf_blocks should align 1:1; if not, fall back to
  # block-count safe iteration (rare malformed input).
  n <- min(nrow(df), length(blocks))
  pkgs <- df[seq_len(n), "Package"]
  vers <- df[seq_len(n), "Version"]

  entries <- vector("list", n)
  for (i in seq_len(n)) {
    pkg <- pkgs[i]
    ver <- vers[i]
    if (is.na(pkg) || is.na(ver) || !nzchar(trimws(pkg))) next
    entries[[i]] <- list(
      Package = pkg, Version = ver, raw = blocks[i],
      vkey    = version_key(ver),
      origin  = origin, priority = priority
    )
  }

  # Collapse to highest version per package (within a single source).
  # Environment = O(1) hash map (list append-by-name is O(n^2)).
  out <- new.env(parent = emptyenv())
  for (e in entries) {
    if (is.null(e)) next
    ex <- out[[e$Package]]
    if (is.null(ex) || compare_keys(e$vkey, ex$vkey, e$Version, ex$Version) > 0L)
      out[[e$Package]] <- e
  }
  as.list(out)
}

## Render a list of entry objects back to a PACKAGES document.
serialize_packages <- function(entries) {
  if (length(entries) == 0L) return("\n")
  blocks <- vapply(entries, function(e) e$raw, character(1L), USE.NAMES = FALSE)
  paste0(paste(blocks, collapse = "\n\n"), "\n\n")
}

## Compare two package version strings. Returns 1 / 0 / -1.
## Falls back to lexicographic compare if either side is not parseable.
compare_pkg_versions <- function(a, b) {
  ka <- version_key(a); kb <- version_key(b)
  compare_keys(ka, kb, a, b)
}

## Parse a version string into a package_version (sortable) object.
## Returns NULL if the string is not parseable (caller falls back to string compare).
version_key <- function(v) {
  if (is.null(v)) return(NULL)
  tryCatch(package_version(v), error = function(e) NULL)
}

## Compare two pre-parsed version keys. Falls back to string compare if either
## key is NULL (unparseable version).
compare_keys <- function(ka, kb, va = NA, vb = NA) {
  if (is.null(ka) || is.null(kb)) {
    if (va == vb) return(0L)
    return(if (va > vb) 1L else -1L)
  }
  if (ka > kb) return(1L)
  if (ka < kb) return(-1L)
  0L
}


## --------------------------------------------------------------------------
## 4. Upstream fetching
## --------------------------------------------------------------------------
##
## Adding a new source (e.g., r-universe, custom CRAN mirror):
##
## 1. repos.json: add a new top-level key with a URL template, and reference
##    it in each version block (like "rspm" and "jasp-repo").
##
## 2. validate_config(): add the new key to the required list if mandatory.
##
## 3. Write a fetch_<source>() function that returns entries in the same
##    format (list of lists with Package, Version, raw, vkey, origin, priority).
##    Set `origin` to a unique string and `priority` to rank it against others
##    (lower wins ties).
##
## 4. get_merged() and get_merged_binary(): add the new fetch call to the
##    merge_entries(...) arguments.  If the source provides binaries, include
##    it in get_merged_binary(); if source-only, get_merged() only.
##
## 5. stream_binary(): add a fallback block that constructs the URL for the
##    new source's binary format and handles any conversion needed.
##
## 6. If the source serves source tarballs, add a similar fallback in
##    stream_source().
##
## 7. handle_prime / classification: if the new source has a distinct package
##    format or metadata, update make_external_entry() or the classification
##    logic in handle_prime().

## Fetch a URL into memory (no disk). Returns list(status, content_type, body[raw]).
fetch_upstream <- function(url, headers = character()) {
  ensure_namespace("curl")
  # followlocation is a curl *option*; User-Agent is an HTTP *header*.
  h <- curl::new_handle(followlocation = TRUE)
  hdr <- c(list("User-Agent" = "jaspModuleTools-repo-server/1.0"), as.list(headers))
  do.call(curl::handle_setheaders, c(list(h), hdr))
  resp <- curl::curl_fetch_memory(url, handle = h)
  list(
    status       = resp$status_code,
    content_type = resp$type,
    body         = resp$content
  )
}

## Fetch + parse a PACKAGES URL. Returns an empty list on 4xx/5xx (a missing
## index is treated as "no packages from this source", not a hard error).
fetch_packages_url <- function(url, origin, priority) {
  resp <- tryCatch(
    fetch_upstream(url),
    error = function(e) list(status = 0L, content_type = "text/plain", body = charToRaw(conditionMessage(e)))
  )
  if (resp$status == 404L) {
    message("[repo_server] 404 (no index) for ", url)
    return(list())
  }
  if (resp$status == 0L) {
    message("[repo_server] network error fetching ", url, ": ", rawToChar(resp$body))
    return(list())
  }
  if (resp$status != 200L) {
    message("[repo_server] HTTP ", resp$status, " for ", url)
    return(list())
  }
  parse_packages_to_entries(rawToChar(resp$body), origin = origin, priority = priority)
}

## RSPM source PACKAGES for a pinned date.
fetch_rspm_source <- function(version) {
  url <- paste0(rspm_base_url(version), "/src/contrib/PACKAGES")
  fetch_packages_url(url, origin = "rspm", priority = 1L)
}

## RSPM binary PACKAGES for the resolved platform path.
## Linux with no distro -> binary_path is NULL -> source-only (returns empty list).
fetch_rspm_binary <- function(version) {
  bp <- .repo_server_state$platform$binary_path
  if (is.null(bp)) {
    message("[repo_server] no binary path for this platform; serving RSPM source-only")
    return(list())
  }
  url <- paste0(rspm_base_url(version), "/bin/", bp, "/PACKAGES")
  fetch_packages_url(url, origin = "rspm", priority = 1L)
}

## Scrape a jasp-repo Apache directory index and build minimal PACKAGES
## entries (Package + Version from filename only). Filenames not matching the
## version pattern (commit SHAs, junk) are ignored silently.
scrape_jasp <- function(version) {
  index_url <- paste0(jasp_url(version), "/")
  resp <- tryCatch(
    fetch_upstream(index_url),
    error = function(e) list(status = 0L, content_type = "text/plain", body = charToRaw(conditionMessage(e)))
  )
  if (resp$status != 200L) {
    message("[repo_server] jasp-repo index HTTP ", resp$status, " for ", index_url)
    return(list())
  }
  html <- rawToChar(resp$body)

  # Pull every .tar.gz href out of the directory listing.
  matches <- regmatches(html, gregexpr(r'{href="[^"]+\.tar\.gz"}', html, perl = TRUE))[[1L]]
  files <- sub(r'{^href="([^"]+)"$}', r"{\1}", matches)
  files <- basename(files)

  ver_pattern <- r"{^\d+([.\-]\d+)*$}"   # SPEC §2.1: version-pattern filter
  out <- new.env(parent = emptyenv())  # O(1) hash map
  for (f in files) {
    stem <- sub(r"{\.tar\.gz$}", "", f, ignore.case = TRUE)
    pos <- regexpr("_[^_]+$", stem)
    if (pos < 0L) next                              # no "_version" tail
    pkg <- sub("_[^_]+$", "", stem)
    ver <- substring(stem, pos + 1L)
    if (!grepl(ver_pattern, ver, perl = TRUE)) next # skip SHAs / junk
    if (!nzchar(pkg) || !nzchar(ver)) next
    e <- list(
      Package  = pkg,
      Version  = ver,
      raw      = sprintf("Package: %s\nVersion: %s", pkg, ver),
      vkey     = version_key(ver),
      origin   = "jasp-repo",
      priority = 2L
    )
    ex <- out[[pkg]]
    if (is.null(ex) || compare_keys(e$vkey, ex$vkey, ver, ex$Version) > 0L)
      out[[pkg]] <- e
  }
  as.list(out)
}


## --------------------------------------------------------------------------
## 5. Merge
## --------------------------------------------------------------------------

## Merge package lists. Highest version wins; ties broken by source priority
## (lower number wins: RSPM=1 > jasp-repo=2 > external=3).
##
## Uses an environment as a hash map (O(1) insert) instead of growing a list
## (which is O(n^2) due to copy-on-write on named-element assignment).
merge_entries <- function(...) {
  out <- new.env(parent = emptyenv())
  for (src in list(...)) {
    for (name in names(src)) {
      e  <- src[[name]]
      ex <- out[[name]]
      if (is.null(ex)) {
        out[[name]] <- e
      } else {
        cmp <- compare_keys(e$vkey, ex$vkey, e$Version, ex$Version)
        if (cmp > 0L || (cmp == 0L && e$priority < ex$priority))
          out[[name]] <- e
      }
    }
  }
  as.list(out)
}

## Lazy per-version merge, cached in memory.
get_merged <- function(version) {
  cache <- .repo_server_state$merged_cache
  if (!is.null(cache[[version]])) return(cache[[version]])

  rspm_src <- fetch_rspm_source(version)
  rspm_bin <- fetch_rspm_binary(version)
  jasp     <- scrape_jasp(version)
  merged   <- merge_entries(rspm_src, rspm_bin, jasp)

  .repo_server_state$merged_cache[[version]] <- merged
  message("[repo_server] merged ", length(merged), " packages for version '", version,
          "' (rspm_src=", length(rspm_src),
          " rspm_bin=", length(rspm_bin),
          " jasp=",     length(jasp), ")")
  merged
}

## Binary-only merge: serves binary PACKAGES (RSPM binaries + jasp-repo).
## Separate from the full merge so /bin/.../PACKAGES shows the highest
## installable binary version, not the source version — matching CRAN.
get_merged_binary <- function(version) {
  cache <- .repo_server_state$merged_cache
  key   <- paste0(version, "__bin")
  if (!is.null(cache[[key]])) return(cache[[key]])

  rspm_bin <- fetch_rspm_binary(version)
  jasp     <- scrape_jasp(version)
  merged   <- merge_entries(rspm_bin, jasp)

  cache[[key]] <- merged
  message("[repo_server] binary merge ", length(merged), " packages for version '", version, "'")
  merged
}


## --------------------------------------------------------------------------
## 6. Sessions (/prime) & classification
## --------------------------------------------------------------------------

new_session_id <- function() {
  paste(sample(c(letters, LETTERS, 0:9), 12L, replace = TRUE), collapse = "")
}

## Does a lockfile record carry enough info to fetch a source tarball?
has_source_info <- function(rec) {
  if (identical(rec$Source, "GitHub") &&
      !is.null(rec$RemoteUsername) && !is.null(rec$RemoteRepo) &&
      (!is.null(rec$RemoteSha) || !is.null(rec$RemoteRef)))
    return(TRUE)
  if (!is.null(rec$Repository) && nzchar(rec$Repository)) return(TRUE)
  FALSE
}

## Build an external (source-only) entry from a lockfile record, carrying the
## remote metadata so R / pkgdepends can locate the source.
make_external_entry <- function(rec) {
  fields <- list(Package = rec$Package, Version = rec$Version)
  for (k in c("Source", "RemoteType", "RemoteHost", "RemoteUsername",
              "RemoteRepo", "RemoteSha", "RemoteRef", "Repository")) {
    if (!is.null(rec[[k]]) && nzchar(rec[[k]])) fields[[k]] <- rec[[k]]
  }
  raw <- paste(sprintf("%s: %s", names(fields), unname(fields, force = TRUE)), collapse = "\n")
  list(
    Package = rec$Package, Version = rec$Version, raw = raw,
    vkey    = version_key(rec$Version),
    origin = "external", priority = 3L, rec = rec
  )
}

## Parse the package name out of a tarball/zip filename: "ggplot2_3.5.1.tar.gz".
## Handles Path: subdirectories: "Transit/Rcpp_1.1.1-1.tar.gz" -> "Rcpp".
pkg_name_from_filename <- function(filename) {
  filename <- basename(filename)
  stem <- sub(r"{\.(tar\.gz|tgz|tar\.bz2|zip)$}", "", filename, ignore.case = TRUE)
  pos <- regexpr("_[^_]+$", stem)
  if (pos < 0L) return(stem)
  sub("_[^_]+$", "", stem)
}


## --------------------------------------------------------------------------
## 7. HTTP response helpers
## --------------------------------------------------------------------------

json_response <- function(obj, status = 200L) {
  body <- as.character(jsonlite::toJSON(obj, auto_unbox = TRUE))
  list(status = status,
       headers = list("Content-Type" = "application/json"),
       body = body)
}

text_response <- function(text, status = 200L,
                          type = "text/plain; charset=utf-8") {
  list(status = status, headers = list("Content-Type" = type), body = text)
}

gzipped_response <- function(text, status = 200L,
                             type = "text/plain; charset=utf-8") {
  # Serve pre-compressed gzip bytes WITHOUT Content-Encoding header.
  # R's download.file() on Windows saves the raw bytes and R's
  # read.dcf(gzfile(...)) handles decompression — but only if the
  # HTTP layer doesn't already decompress (which wininet doesn't).
  list(status = status,
       headers = list(
         "Content-Type" = type
       ),
       body = memCompress(charToRaw(text), type = "gzip"))
}

not_found <- function(...) text_response(paste0("404 Not Found: ", paste0(...), "\n"), 404L)
method_not_allowed <- function() text_response("405 Method Not Allowed\n", 405L)

raw_response <- function(body, content_type = "application/octet-stream", status = 200L) {
  list(status = status, headers = list("Content-Type" = content_type), body = body)
}

## Content-Type from a tarball filename extension.
content_type_for <- function(filename) {
  ext <- tolower(sub(".*\\.", ".", filename))
  switch(ext,
    ".tgz" = , ".tar.gz" = "application/gzip",
    ".zip"          = "application/zip",
    "application/octet-stream")
}


## --------------------------------------------------------------------------
## 8. Binary cache & format conversion
## --------------------------------------------------------------------------
##
## jasp-repo serves binary packages as zstd-compressed tarballs with files at
## the top level (no wrapping pkgname/ directory) and a .tar.gz extension.
## R's install.packages() expects:
##   macOS  : .tgz  (gzip-compressed tar, pkgname/ wrapped)
##   Windows: .zip  (zip archive, pkgname/ wrapped)
##   Linux  : .tar.gz (gzip-compressed tar, pkgname/ wrapped)
##
## So jasp-repo binaries must be converted before R can use them. We also cache
## ALL binaries (both converted jasp-repo and pass-through RSPM) so repeated
## builds don't re-download.
##
## Extraction uses the `archive` package (libarchive) which auto-detects
## compression (zstd, gzip, xz, bzip2, ...) regardless of the filename — so when
## jasp-repo renames files to .tar.zst later, nothing needs to change.

## Resolve the cache directory. Priority: server param > R option > NULL (off).
get_cache_path <- function() {
  .repo_server_state$cache_path %||%
    getOption("jasp.repo_cache_path") %||%
    NULL
}

## Build the on-disk cache path for a specific binary. Returns NULL if caching
## is disabled. Keyed by platform (os/arch/r_version) so cross-platform
## sessions don't collide.
cache_file_for <- function(pkg, version, filename) {
  cache_root <- get_cache_path()
  if (is.null(cache_root)) return(NULL)
  plat <- .repo_server_state$platform
  file.path(cache_root,
            sprintf("%s_%s_%s", plat$os, plat$arch, plat$r_version),
            filename)
}

## Serve a file from the cache as an HTTP response.
serve_cached_file <- function(cache_file) {
  body <- readBin(cache_file, "raw", file.info(cache_file)$size)
  raw_response(body, content_type_for(cache_file))
}

## Write raw bytes to the cache (best-effort: never fail the request on cache errors).
write_cached_binary <- function(cache_file, raw_bytes) {
  if (is.null(cache_file)) return(invisible(NULL))
  tryCatch({
    dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
    writeBin(raw_bytes, cache_file)
  }, error = function(e) {
    message("[repo_server] cache write failed: ", conditionMessage(e))
  })
  invisible(NULL)
}

## Convert a jasp-repo binary (any compression, no wrapper dir) into the format
## R expects for the target platform.
##
## Steps:
##   1. Extract with archive::archive_extract() (libarchive auto-detects compression)
##   2. If files are at top level (jasp-repo L0 format), wrap them in pkgname/
##   3. Re-archive as gzip tar (.tgz/.tar.gz) or zip (.zip)
##
## `target_filename` is what R requested (e.g. "ggplot2_3.5.1.tgz").
convert_binary <- function(raw_bytes, pkg_name, target_filename) {
  ensure_namespace("archive")

  tmpdir <- file.path(tempdir(), paste0("jasp_convert_", pkg_name))
  if (dir.exists(tmpdir)) unlink(tmpdir, recursive = TRUE)
  dir.create(tmpdir, recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  # 1. Write downloaded bytes — extensionless so libarchive sniffs the format.
  input_file <- file.path(tmpdir, "downloaded")
  writeBin(raw_bytes, input_file)

  # 2. Extract (libarchive auto-detects compression: zstd, gzip, xz, bzip2, ...)
  extract_dir <- file.path(tmpdir, "content")
  dir.create(extract_dir)
  archive::archive_extract(input_file, dir = extract_dir)

  # 3. Determine if there's already a wrapper directory.
  top <- list.files(extract_dir)
  if (length(top) == 1L && dir.exists(file.path(extract_dir, top))) {
    # Standard CRAN binary (already wrapped in pkgname/) — use as-is.
    pkg_dir <- file.path(extract_dir, top)
  } else {
    # jasp-repo L0 format (DESCRIPTION at root) — wrap in pkgname/.
    pkg_dir <- file.path(extract_dir, pkg_name)
    if (!dir.exists(pkg_dir)) dir.create(pkg_dir)
    everything <- list.files(extract_dir, full.names = TRUE)
    everything <- everything[everything != pkg_dir]
    file.copy(everything, pkg_dir, recursive = TRUE)
  }

  # 4. Re-archive in the target format.
  parent  <- normalizePath(dirname(pkg_dir), winslash = "/")
  pkg_bname <- basename(pkg_dir)
  output_file <- file.path(tmpdir, target_filename)
  oldwd <- setwd(parent)
  on.exit(setwd(oldwd), add = TRUE)

  if (grepl("\\.zip$", target_filename, ignore.case = TRUE)) {
    utils::zip(output_file, pkg_bname)
  } else {
    # .tgz or .tar.gz — gzip-compressed tar with pkgname/ wrapper.
    tar(output_file, files = pkg_bname, compression = "gzip", tar = "internal")
  }

  readBin(output_file, "raw", file.info(output_file)$size)
}

## Stream an upstream URL straight to the client as the response body.
stream_url <- function(url, headers = character()) {
  resp <- tryCatch(
    fetch_upstream(url, headers),
    error = function(e) list(status = 502L, content_type = "text/plain",
                             body = charToRaw(conditionMessage(e)))
  )
  if (resp$status == 404L)
    return(not_found("Upstream 404: ", url))
  if (resp$status >= 400L)
    return(json_response(list(error = paste0("Upstream HTTP ", resp$status, " for ", url)), resp$status))
  ct <- resp$content_type
  if (is.null(ct) || !nzchar(ct)) ct = "application/octet-stream"
  list(status = as.integer(resp$status),
       headers = list("Content-Type" = ct),
       body = resp$body)
}

## Resolve + stream a BINARY for a primed session, with caching + conversion.
##
## RSPM binaries are already in the correct format — download, cache, serve.
## jasp-repo binaries are zstd-compressed, no wrapper dir — download, convert,
## cache, serve.
stream_binary <- function(sess, filename) {
  pkg   <- pkg_name_from_filename(filename)
  entry <- sess$scoped[[pkg]]
  if (is.null(entry)) return(not_found("Package not in session: ", pkg))

  bp <- .repo_server_state$platform$binary_path
  if (is.null(bp))
    return(not_found("No binary path for platform (source-only): ", pkg))

  # 1. Serve from cache if available.
  cache_file <- cache_file_for(pkg, entry$Version, filename)
  if (!is.null(cache_file) && file.exists(cache_file))
    return(serve_cached_file(cache_file))

  # 2. Search all binary sources for this file.
  # Use the filename R requested (from binary PACKAGES) directly — the
  # entry's Version may differ (full merge has source version, binary
  # PACKAGES has binary version).

  # Try RSPM binary.
  url <- paste0(rspm_base_url(sess$version), "/bin/", bp, "/", filename)
  resp <- tryCatch(fetch_upstream(url),
                   error = function(e) list(status = 502L, body = charToRaw(conditionMessage(e))))
  if (resp$status == 200L) {
    write_cached_binary(cache_file, resp$body)
    return(raw_response(resp$body, content_type_for(filename)))
  }

  # Fall back to jasp-repo.  Build .tar.gz filename from the version in
  # the requested filename (e.g. "pkg_1.0.0.zip" -> "pkg_1.0.0.tar.gz").
  jasp_ver <- sub(".*_", "", sub("\\.(zip|tgz|tar\\.gz)$", "", filename, ignore.case = TRUE))
  jasp_filename <- paste0(pkg, "_", jasp_ver, ".tar.gz")
  url <- paste0(jasp_url(sess$version), "/", jasp_filename)
  resp <- tryCatch(fetch_upstream(url),
                   error = function(e) list(status = 502L, body = charToRaw(conditionMessage(e))))
  if (resp$status == 200L) {
    converted <- convert_binary(resp$body, pkg, filename)
    write_cached_binary(cache_file, converted)
    return(raw_response(converted, content_type_for(filename)))
  }

  # No binary found anywhere — R falls back to source.

  # No binary found anywhere — R falls back to source.
  return(not_found("No binary for: ", pkg, " (", filename, ")"))
}

## Resolve + stream a SOURCE tarball for a primed session.
## GitHub packages only from GitHub.  CRAN packages: RSPM -> live CRAN.
stream_source <- function(sess, filename) {
  pkg   <- pkg_name_from_filename(filename)
  entry <- sess$scoped[[pkg]]
  if (is.null(entry)) return(not_found("Package not in session: ", pkg))

  rec <- entry$rec %||% list()

  # GitHub remote — only source, no fallback.
  if (!is.null(rec$RemoteUsername) && !is.null(rec$RemoteRepo) &&
      (!is.null(rec$RemoteSha) || !is.null(rec$RemoteRef))) {
    ref <- rec$RemoteSha %||% rec$RemoteRef
    url <- sprintf("https://api.github.com/repos/%s/%s/tarball/%s",
                   rec$RemoteUsername, rec$RemoteRepo, ref)
    headers <- c("Accept" = "application/vnd.github+json",
                 "X-GitHub-Api-Version" = "2022-11-28")
    pat <- .repo_server_state$github_pat
    if (!is.null(pat))
      headers <- c(headers, "Authorization" = paste("Bearer", pat))
    return(stream_url(url, headers))
  }

  # External repository URL.
  if (!is.null(rec$Repository) && nzchar(rec$Repository)) {
    url <- paste0(rec$Repository, "/src/contrib/", filename)
    return(stream_url(url))
  }

  # CRAN package: try RSPM first, then live CRAN.
  url <- paste0(rspm_base_url(sess$version), "/src/contrib/", filename)
  resp <- tryCatch(fetch_upstream(url),
                   error = function(e) list(status = 502L))
  if (resp$status == 200L)
    return(raw_response(resp$body, "application/gzip"))

  url <- paste0("https://cloud.r-project.org/src/contrib/", filename)
  resp <- tryCatch(fetch_upstream(url),
                   error = function(e) list(status = 502L))
  if (resp$status == 200L)
    return(raw_response(resp$body, "application/gzip"))

  return(not_found("No source for: ", pkg, " (", filename, ")"))
}


## --------------------------------------------------------------------------
## 9. Route handlers
## --------------------------------------------------------------------------

read_request_body <- function(req) {
  input <- req$rook.input
  if (is.null(input)) return(raw(0))
  body <- input$read(-1L)
  if (is.character(body)) body <- charToRaw(paste(body, collapse = ""))
  body
}

resp_health <- function() {
  st <- .repo_server_state
  json_response(list(
    status   = "ok",
    versions = I(names(st$config$versions)),  # always a JSON array
    latest   = st$config$latest,
    sessions = length(ls(st$sessions))
  ))
}

## GET /{version|latest}/src/contrib/PACKAGES(.gz)
## GET /{version|latest}/bin/<wildcard>/PACKAGES(.gz)
resp_version_packages <- function(version, kind) {
  v <- resolve_version(version)
  if (is.null(v)) return(not_found("Unknown version: ", version))
  merged <- if (kind == "bin") get_merged_binary(v) else get_merged(v)
  text_response(serialize_packages(merged))
}

## POST /prime -> classify a lockfile and create a session.
handle_prime <- function(req) {
  body_raw <- read_request_body(req)
  if (length(body_raw) == 0L)
    return(json_response(list(error = "Empty request body"), 400L))
  lockfile <- tryCatch(
    jsonlite::fromJSON(rawToChar(body_raw), simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(lockfile))
    return(json_response(list(error = "Invalid JSON in request body"), 400L))

  pkgs <- lockfile$Packages
  if (!is.list(pkgs))
    return(json_response(list(error = "Lockfile missing 'Packages' object"), 400L))

  rv <- lockfile$JASP$RepoVersion %||% .repo_server_state$config$latest
  version <- resolve_version(rv)
  if (is.null(version))
    return(json_response(list(error = paste0("Unknown JASP.RepoVersion: ", rv)), 400L))

  merged <- get_merged(version)

  # Binary classification: does the exact lockfile version exist as a binary
  # in any of our sources?  Check raw binary lists, not the merged version.
  rspm_bin <- fetch_rspm_binary(version)
  jasp_bin <- scrape_jasp(version)
  binary      <- character()
  source_only <- character()
  not_found_v <- character()
  external    <- list()

  for (name in names(pkgs)) {
    rec <- pkgs[[name]]
    if (!is.null(rec$Package)) name <- rec$Package
    rspm_match <- !is.null(rspm_bin[[name]]) &&
                  rspm_bin[[name]]$Version == rec$Version
    jasp_match <- !is.null(jasp_bin[[name]]) &&
                  jasp_bin[[name]]$Version == rec$Version
    if (rspm_match || jasp_match) {
      binary <- c(binary, name)
    } else if (has_source_info(rec)) {
      source_only <- c(source_only, name)
      external[[name]] <- make_external_entry(rec)
    } else if (!is.null(merged[[name]])) {
      source_only <- c(source_only, name)
    } else {
      not_found_v <- c(not_found_v, name)
    }
  }

  # Scoped PACKAGES = lockfile packages with exact lockfile versions.
  # For binary-classified: use merge's DCF (has Depends/Imports).
  # For source_only with remote: use external entry (GitHub fields).
  # For source_only without remote: use merge's DCF with lockfile version.
  scoped <- list()
  for (name in names(pkgs)) {
    rec <- pkgs[[name]]
    if (!is.null(rec$Package)) name <- rec$Package
    if (name %in% binary) {
      entry <- merged[[name]]
      entry$raw <- sub(paste0("Version: ", entry$Version),
                       paste0("Version: ", rec$Version), entry$raw, fixed = TRUE)
      entry$Version <- rec$Version
      scoped[[name]] <- entry
    } else if (name %in% source_only && has_source_info(rec)) {
      scoped[[name]] <- external[[name]]
    } else if (!is.null(merged[[name]])) {
      entry <- merged[[name]]
      entry$raw <- sub(paste0("Version: ", entry$Version),
                       paste0("Version: ", rec$Version), entry$raw, fixed = TRUE)
      entry$Version <- rec$Version
      scoped[[name]] <- entry
    }
  }

  sid <- new_session_id()
  .repo_server_state$sessions[[sid]] <- list(
    id            = sid,
    version       = version,
    merged        = merged,
    external      = external,
    scoped        = scoped,
    classification = list(binary = binary, source_only = source_only, not_found = not_found_v),
    created       = Sys.time()
  )

  base <- sprintf("http://localhost:%d", .repo_server_state$port)
  json_response(list(
    session       = sid,
    repo_url      = sprintf("%s/primed/%s", base, sid),
    package_count = length(scoped),
    binary        = length(binary),
    source_only   = source_only,
    not_found     = not_found_v
  ))
}

## GET /primed/{session}/src/contrib/PACKAGES(.gz) or /bin/.../PACKAGES(.gz)
resp_scoped_packages <- function(sess) {
  text_response(serialize_packages(sess$scoped))
}

## DELETE /primed/{session}
handle_delete_session <- function(sid) {
  if (is.null(.repo_server_state$sessions[[sid]]))
    return(not_found("Unknown session: ", sid))
  rm(list = sid, envir = .repo_server_state$sessions)
  json_response(list(status = "deleted", session = sid))
}

## Dispatch a /primed/{session}/... request.
handle_primed <- function(req, method, session_id, rest) {
  if (length(session_id) == 0L) return(not_found("Missing session id"))
  sess <- .repo_server_state$sessions[[session_id]]
  if (is.null(sess)) return(not_found("Unknown session: ", session_id))

  if (method == "DELETE" && length(rest) == 0L)
    return(handle_delete_session(session_id))
  if (method != "GET")
    return(method_not_allowed())
  if (length(rest) < 1L)
    return(not_found("Incomplete primed path"))

  # .../src/contrib/PACKAGES[.gz]
  # .../src/contrib/{pkg}_{ver}.tar.gz
  # .../src/contrib/{Path}/{pkg}_{ver}.tar.gz   (CRAN Path: field)
  if (length(rest) >= 3L && rest[1L] == "src" && rest[2L] == "contrib") {
    target <- paste(rest[-(1:2)], collapse = "/")
    if (target == "PACKAGES" || target == "PACKAGES.gz")
      return(resp_scoped_packages(sess))
    return(stream_source(sess, target))
  }

  # .../bin/<path>/PACKAGES[.gz]
  # .../bin/<path>/{pkg}_{ver}.{tgz,tar.gz,zip}
  # .../bin/<path>/{Path}/{pkg}_{ver}.{tgz,tar.gz,zip}  (CRAN Path: field)
  if (rest[1L] == "bin") {
    last <- rest[length(rest)]
    if (last == "PACKAGES" || last == "PACKAGES.gz") {
      # Only lockfile packages where we have an exact binary match.
      # Don't show source_only packages — R would try binary → 404
      # and skip them instead of falling back to source.
      bin_entries <- sess$scoped[sess$classification$binary]
      return(text_response(serialize_packages(bin_entries)))
    }
    # Reconstruct filename with Path: subdirectory if present.
    # bin/<os>/<platform>/contrib/<rver>/[Path/]pkg_ver.ext
    ci <- match("contrib", rest)
    target <- if (!is.na(ci) && ci + 1L < length(rest))
      paste(rest[-(1:(ci + 1L))], collapse = "/")
    else
      last
    return(stream_binary(sess, target))
  }

  not_found("No primed route for: ", paste(rest, collapse = "/"))
}


## --------------------------------------------------------------------------
## 10. Router
## --------------------------------------------------------------------------

parse_path <- function(path) {
  path <- sub("^/+", "", path)
  path <- sub("/+$", "", path)
  if (!nzchar(path)) return(character(0))
  strsplit(path, "/")[[1L]]
}

handle_request <- function(req) {
  method <- req$REQUEST_METHOD %||% "GET"
  segs   <- parse_path(req$PATH_INFO %||% "/")

  if (length(segs) == 0L)
    return(text_response("JASP local repo server\n", 200L))

  # GET /health
  if (length(segs) == 1L && segs[1L] == "health" && method == "GET")
    return(resp_health())

  # POST /prime
  if (length(segs) == 1L && segs[1L] == "prime" && method == "POST")
    return(handle_prime(req))

  # /primed/{session}/...
  if (length(segs) >= 2L && segs[1L] == "primed")
    return(handle_primed(req, method, segs[2L], if (length(segs) > 2L) segs[-(1:2)] else character(0)))

  # /{version|latest}/src/contrib/PACKAGES(.gz)
  # /{version|latest}/bin/<wildcard>/PACKAGES(.gz)
  if (length(segs) >= 2L && method == "GET") {
    version <- segs[1L]
    rest <- segs[-1L]
    if (length(rest) >= 3L && rest[1L] == "src" && rest[2L] == "contrib" && grepl(r"{^PACKAGES(\.gz)?$}", rest[3L])) {
      return(resp_version_packages(version, kind = "src"))
    }
    if (length(rest) >= 2L && rest[1L] == "bin" && grepl(r"{^PACKAGES(\.gz)?$}", rest[length(rest)])) {
      return(resp_version_packages(version, kind = "bin"))
    }
  }

  not_found("No route for ", method, " ", req$PATH_INFO %||% "/")
}


## --------------------------------------------------------------------------
## 11. Server lifecycle
## --------------------------------------------------------------------------

## Start the local repo server (non-blocking: httpuv runs in a background
## thread; the calling R session is free to call install.packages()).
##
##   config_url  path or URL to repos.json (default: GitHub)
##   os/arch/r_version/distro  platform overrides (auto-detected if NULL)
##   github_pat  PAT for source fallback (falls back to GITHUB_PAT / GITHUB_TOKEN)
##   host        bind address (default 127.0.0.1)
##   port        bind port (default 8765)
##   cache_path  directory for caching binaries (default: NULL = disabled;
##               also settable via options(jasp.repo_cache_path))
start_repo_server <- function(config_url = NULL, os = NULL, arch = NULL,
                              r_version = NULL, distro = NULL,
                              github_pat = NULL, host = "127.0.0.1", port = 8765L,
                              cache_path = NULL) {
  ensure_namespace("httpuv"); ensure_namespace("curl"); ensure_namespace("jsonlite")

  if (!is.null(.repo_server_state$server)) {
    message("[repo_server] already running on port ", .repo_server_state$port)
    return(invisible(.repo_server_state$port))
  }

  cfg <- fetch_config(if (is.null(config_url)) DEFAULT_CONFIG_URL else config_url)
  pat <- resolve_github_pat(github_pat)
  plat <- resolve_platform(os = os, arch = arch, r_version = r_version, distro = distro)

  .repo_server_state$config       <- cfg
  .repo_server_state$platform     <- plat
  .repo_server_state$github_pat   <- pat
  .repo_server_state$merged_cache <- list()
  .repo_server_state$cache_path   <- cache_path
  rm(list = ls(.repo_server_state$sessions, all.names = TRUE), envir = .repo_server_state$sessions)

  app <- list(call = function(req) {
    tryCatch(
      handle_request(req),
      error = function(e) json_response(list(error = conditionMessage(e)), 500L)
    )
  })

  srv <- httpuv::startServer(host, port, app)

  .repo_server_state$server <- srv
  .repo_server_state$host   <- host
  .repo_server_state$port   <- as.integer(port)

  base <- sprintf("http://localhost:%d", port)
  options(jasp.local_repo = base)

  message("[repo_server] listening on ", base)
  message("[repo_server] platform: os=", plat$os,
          " arch=", plat$arch,
          " r_version=", plat$r_version,
          if (!is.null(plat$distro)) paste0(" distro=", plat$distro),
          " binary_path=", plat$binary_path %||% "<none>")
  message("[repo_server] github PAT: ", if (is.null(pat)) "<none>" else "set")
  cp <- get_cache_path()
  message("[repo_server] binary cache: ", cp %||% "<disabled>")

  # SPEC §3: pre-merge PACKAGES for 'latest' so the first request is fast.
  # Non-fatal: if an upstream source is down, lazy merge will retry on demand.
  tryCatch(get_merged(cfg$latest),
           error = function(e) message("[repo_server] pre-merge warning: ", conditionMessage(e)))

  invisible(port)
}

## Stop the server (sessions and merged caches are discarded).
stop_repo_server <- function() {
  if (!is.null(.repo_server_state$server)) {
    .repo_server_state$server$stop()
    .repo_server_state$server <- NULL
    message("[repo_server] stopped")
  }
  invisible(NULL)
}

## Human-readable summary of current server state (handy for debugging).
repo_server_status <- function() {
  st <- .repo_server_state
  list(
    running    = !is.null(st$server),
    url        = sprintf("http://localhost:%d", st$port),
    platform   = st$platform,
    versions   = names(st$config$versions %||% list()),
    latest     = st$config$latest,
    sessions   = length(ls(st$sessions)),
    merged     = names(st$merged_cache),
    github_pat = !is.null(st$github_pat)
  )
}
