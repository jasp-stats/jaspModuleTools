#' Start JASP development environment
#'
#' @description Sets up an isolated build library and starts the local repo
#'   server.  After this, install.packages() and RStudio's Install button
#'   resolve against the version-pinned RSPM + jasp-repo snapshot.
#'
#' @param update_lockfile If TRUE, run updateLockfile("./") after setup.
#' @param library Path to an isolated package library for builds.
#'   Defaults to "./build_lib".  Exported as JASP_PKG_LIBRARY env var.
#' @param port Port for the local repo server (default 8765).
#' @param config_url Path or URL to repos.json (auto-detected).
#' @param github_pat GitHub PAT for source fallback. If NULL/empty, resolved
#'   from GITHUB_PAT / GITHUB_TOKEN env vars by the server.
#' @param cache_path Directory for caching binaries (default NULL = disabled).
#' @export
start_jasp_development <- function(update_lockfile = FALSE,
                                    library = "./build_lib",
                                    port     = 8765L,
                                    config_url = NULL,
                                    github_pat = NULL,
                                    cache_path = NULL) {
  # Create isolated build library
  build_lib <- fs::path_abs(fs::dir_create(library))
  Sys.setenv(JASP_PKG_LIBRARY = build_lib)
  message("[jaspModuleTools] Build library: ", build_lib)

  # Start the local repo server in a background process
  options(jasp.server_cache_path = cache_path)
  ensure_repo_server(
    version    = "latest",
    config_url = config_url,
    github_pat = if (is.null(github_pat) || !nzchar(github_pat)) NULL else github_pat,
    port       = port,
    cache_path = cache_path
  )

  options(repos = c(CRAN = paste0(getOption("jasp.local_repo"), "/latest")))
  message("[jaspModuleTools] repos set to ", getOption("repos")[["CRAN"]])

  if (update_lockfile)
    updateLockfile("./")

  invisible(build_lib)
}


#' Build a JASP module
#'
#' @description Resolves dependencies against the pinned RSPM snapshot
#'   (via the local repo server's /prime session), installs everything
#'   into an isolated library, and optionally creates a .JASPModule bundle.
#'
#' @param moduledir Path to the module root (default "./").
#' @param update_lockfile If TRUE, refresh renv.lock before building.
#'   A lockfile is always created if it does not exist.
#' @param build_bundle If TRUE, create a .JASPModule bundle and clean up
#'   the build library afterwards.
#' @param library Path to the isolated package library.  Defaults to the
#'   JASP_PKG_LIBRARY env var (set by start_jasp_development()), falling
#'   back to "./build_lib".
#' @export
build <- function(moduledir = "./",
                  update_lockfile = FALSE,
                  build_bundle    = FALSE,
                  library         = NULL) {
  # ---- resolve library path ----
  build_lib <- library
  if (is.null(build_lib)) {
    build_lib <- Sys.getenv("JASP_PKG_LIBRARY")
    if (!nzchar(build_lib)) build_lib <- "./build_lib"
  }
  build_lib <- fs::path_abs(fs::dir_create(build_lib))

  # ---- ensure server is running ----
  ensure_repo_server()

  # ---- lockfile (always create if missing) ----
  lockfile_path <- fs::path(moduledir, "renv.lock")
  if (update_lockfile || !fs::file_exists(lockfile_path)) {
    updateLockfile(moduledir)
  }

  lockfile <- renv::lockfile_read(lockfile_path)
  version  <- lockfile$JASP$RepoVersion %||% "latest"
  pkgs     <- names(lockfile$Packages)

  # ---- prime session (post raw lockfile JSON) ----
  base <- getOption("jasp.local_repo")
  lockfile_json <- paste(readLines(lockfile_path, warn = FALSE), collapse = "\n")
  prime_resp <- curl::curl_fetch_memory(
    paste0(base, "/prime"),
    handle = curl::handle_setheaders(
      curl::new_handle(post = TRUE, postfields = lockfile_json),
      "Content-Type" = "application/json"
    )
  )
  prime <- jsonlite::fromJSON(rawToChar(prime_resp$content))

  if (!is.null(prime$error))
    stop("Prime failed: ", prime$error)

  message("[jaspModuleTools] Primed session: ", prime$session,
          " (", prime$binary, " binary, ",
          length(prime$source_only), " source-only)",
          if (length(prime$not_found))
            paste0(" — NOT FOUND: ", paste(prime$not_found, collapse = ", ")))

  # ---- install from primed session ----
  # Run in a clean subprocess so loaded packages in the main session
  # don't block install.packages().  Clean up the session afterwards.
  # Source-only packages (GitHub remotes) are installed separately
  # since type="binary" skips them when the .zip returns 404.
  moduledir_abs <- fs::path_abs(moduledir)
  source_only <- prime$source_only
  binary_pkgs <- setdiff(pkgs, source_only)

  # Use pkgdepends' install order from the lockfile (baked in by updateLockfile).
  install_order <- as.character(lockfile$JASP$InstallOrder)
  if (length(install_order)) {
    source_only <- intersect(install_order, source_only)
    binary_pkgs <- intersect(install_order, binary_pkgs)
  }

  tryCatch({
    callr::r(function(bin_pkgs, src_pkgs, mod, lib, repo) {
      .libPaths(lib)  # only see build_lib — no user packages leak in
      # Only install packages not already present in the build library.
      installed <- rownames(utils::installed.packages(lib.loc = lib))
      bin_pkgs <- setdiff(bin_pkgs, installed)
      if (length(bin_pkgs)) {
        message("Installing ", length(bin_pkgs), " new/missing binary packages")
        install.packages(bin_pkgs, lib = lib, repos = repo, type = "binary")
      }
      # Source-only packages (GitHub remotes) — skip already installed.
      # Installed one at a time in dependency order (sorted above).
      src_pkgs <- setdiff(src_pkgs, installed)
      for (pkg in src_pkgs)
        install.packages(pkg, lib = lib, repos = repo, type = "source")
      install.packages(mod, repos = NULL, type = "source", lib = lib)
    }, args = list(bin_pkgs = binary_pkgs, src_pkgs = source_only,
                   mod = moduledir_abs,
                   lib = build_lib, repo = prime$repo_url),
       stdout = "", stderr = "", error = "error")
  }, finally = {
    curl::curl_fetch_memory(
      paste0(base, "/primed/", prime$session),
      handle = curl::new_handle(customrequest = "DELETE")
    )
  })

  # ---- macOS linking fix ----
  if (Sys.info()["sysname"] == "Darwin") {
    fix_mac_linking(build_lib)
  }

  # ---- print path for JASP ----
  module_name <- read.dcf(fs::path(moduledir, "DESCRIPTION"))[1, "Package"]
  cat("\nJASP module library:", normalizePath(build_lib))
  cat("\nJASP module:", module_name, "\n\n")

  # ---- optional bundle ----
  if (build_bundle) {
    resultdir <- fs::dir_create("./bundles")
    jaspModuleBundleManager::createJaspModuleBundle(
      moduleLib        = build_lib,
      moduleName       = module_name,
      resultdir        = resultdir,
      packageAll       = TRUE,
      includeInManifest = c(jaspVersion = version),
      repoNames        = c(version)
    )
    message("[jaspModuleTools] Bundle created in ", resultdir)
  }

  invisible(build_lib)
}


#' Reset the development environment
#'
#' @description Kills the background repo server (clearing all its caches
#'   and sessions) and optionally wipes the build library so the next
#'   build starts fresh.
#'
#' @param clear_library If TRUE, delete the build_lib directory.
#'   Default TRUE.
#' @export
reset <- function(clear_library = TRUE) {
  stop_jasp_development()

  build_lib <- Sys.getenv("JASP_PKG_LIBRARY")
  if (!nzchar(build_lib)) build_lib <- "./build_lib"

  # Wipe binary cache if one was configured (server state, always cleared).
  cache <- getOption("jasp.server_cache_path")
  if (!is.null(cache) && fs::dir_exists(cache)) {
    fs::dir_delete(cache)
    message("[jaspModuleTools] Deleted server cache: ", cache)
  }

  if (clear_library) {
    if (fs::dir_exists(build_lib)) {
      fs::dir_delete(build_lib)
      message("[jaspModuleTools] Deleted build library: ", build_lib)
    }
  }

  message("[jaspModuleTools] Environment reset.")
  invisible(NULL)
}


#' Updates lockfile using the local repo server
#'
#' @description Resolves all module dependencies against the JASP local repo
#'   server (which merges version-pinned RSPM + jasp-repo), writes a lockfile
#'   with JASP.RepoVersion baked in so subsequent builds use the same snapshot.
#' @param moduledir Path to the jaspModule root folder
#' @param jaspModuleDependenciesOnly If TRUE, only update jasp-* packages
#'   (keep all other records unchanged from the current lockfile).
#' @param version Version date-stamp to pin (e.g. "2026-07-03").
#'   Default: "latest" which resolves to the config's latest key.
#' @usage jaspModuleTools::updateLockfile('~/jaspTTest/')
#' @export
updateLockfile <- function(moduledir, jaspModuleDependenciesOnly = FALSE,
                           version = "latest") {
  lockfilePath <- fs::path(moduledir, "renv.lock")

  # 1. Ensure the local repo server is running.
  ensure_repo_server(version = version)

  # 2. Read current lockfile (may not exist for first-time modules).
  currentRecords <- NULL
  if (fs::file_exists(lockfilePath))
    currentRecords <- renv::lockfile_read(lockfilePath)

  # 3. Locked records (JASP_LOCK flag) — never overwritten.
  getLockedRecords <- function(pkg) !is.null(pkg$JASP_LOCK)
  lockedRecords  <- Filter(getLockedRecords, currentRecords$Packages)
  lockedNames    <- lapply(lockedRecords, function(x) x$Package)

  # 4. Resolve fresh records from the local repo server via pkgdepends.
  repo_base <- paste0(getOption("jasp.local_repo"), "/", version)
  message("[jaspModuleTools] Resolving dependencies against ", repo_base,
          " (this may take a minute on first run)...")
  result <- getRecordsFromPkgdepends(moduledir, repos = repo_base)
  newRecords <- result$records
  install_order <- result$install_order

  # 5. Merge: locked records win over new ones.
  noConflicts <- function(pkg) !(pkg$Package %in% lockedNames)
  nonConflicting <- Filter(noConflicts, newRecords)
  processedRecords <- c(lockedRecords, nonConflicting)

  # 6. jaspModuleDependenciesOnly: only refresh jasp-* packages.
  if (jaspModuleDependenciesOnly) {
    isJASPRecord  <- function(pkg) grepl("jasp", pkg$Package)
    notJASPRecord <- function(pkg) !isJASPRecord(pkg)
    newjaspRecords <- Filter(isJASPRecord, newRecords)
    oldRecords     <- Filter(notJASPRecord, currentRecords$Packages)
    processedRecords <- c(oldRecords, newjaspRecords)
  }

  # 7. Write the lockfile with JASP.RepoVersion injected.
  if (fs::file_exists(lockfilePath))
    fs::file_delete(lockfilePath)
  lockfile <- renv:::renv_lockfile_init(NULL)
  lockfile <- renv::record(processedRecords, lockfile = lockfile)

  # Inject JASP metadata so the server knows which snapshot to use.
  lockfile$JASP <- list(
    RepoVersion  = getOption("jasp.local_repo_version", version),
    InstallOrder = install_order
  )

  renv::lockfile_write(lockfile, lockfilePath)
  sprintf("renv lockfile written for: %s", moduledir)
}
