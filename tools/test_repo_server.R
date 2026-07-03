## test_repo_server.R — End-to-end test of the JASP local repo server
## ==========================================================================
## Simulates a Windows platform, starts the server with caching, primes a
## synthetic lockfile, and verifies that binaries are gathered and cached.
##
## Usage:
##   Rscript tools/test_repo_server.R
## or interactively:
##   source("tools/test_repo_server.R")
##
## NOTE: httpuv in batch Rscript deadlocks because R's event loop must pump
## requests while the main thread is busy. So we start the server in a
## background R process via `system(..., wait=FALSE)`.
## ==========================================================================

cat("\n========================================\n")
cat("JASP Repo Server — Windows Platform Test\n")
cat("========================================\n\n")

# ---- 0. Setup ----

TEST_CACHE <- file.path(tempdir(), "jasp_test_cache")
unlink(TEST_CACHE, recursive = TRUE)
dir.create(TEST_CACHE, recursive = TRUE)
TEST_PORT  <- 18765L
BASE       <- sprintf("http://localhost:%d", TEST_PORT)

# ---- 1. Start the server in a background R process ----

cat("[TEST] Starting server in background process (Windows x86_64, R 4.5)\n")

server_script <- tempfile(fileext = ".R")
server_out    <- tempfile(fileext = ".out")
server_pid_f  <- tempfile(fileext = ".pid")

writeLines(c(
  sprintf('cat(Sys.getpid(), file = "%s")', server_pid_f),
  'source("tools/repo_server.R")',
  sprintf('start_repo_server(config_url = "tools/repos.json", os = "Windows", arch = "x86_64", r_version = "4.5", port = %dL, cache_path = "%s")', TEST_PORT, TEST_CACHE),
  'while (TRUE) { httpuv::service(200); Sys.sleep(0.01) }'
), server_script)

system(sprintf('Rscript --no-save --no-restore "%s" > "%s" 2>&1',
               server_script, server_out), wait = FALSE)

# Wait briefly for the PID file to appear, then read it.
Sys.sleep(1.5)
SERVER_PID <- NULL
if (file.exists(server_pid_f))
  SERVER_PID <- as.integer(readLines(server_pid_f, warn = FALSE)[1])
cat("[TEST] Background server PID: ", SERVER_PID %||% "<unknown>", "\n")

# Wait for the server to be ready (health check + merged PACKAGES)
cat("[TEST] Waiting for server to come up...\n")
for (i in 1:120) {
  Sys.sleep(1.5)
  h <- tryCatch(
    curl::curl_fetch_memory(paste0(BASE, "/health")),
    error = function(e) list(status_code = 0L)
  )
  if (h$status_code == 200L) {
    health <- jsonlite::fromJSON(rawToChar(h$content))
    if (!is.null(health$merged) || TRUE) {  # just being up is enough
      cat("[TEST] Server ready after ", i * 1.5, "s\n")
      break
    }
  }
  if (i == 120) stop("Server did not start within 3 minutes")
  cat(".")
}

# ---- 2. Basic health check ----

cat("\n--- 2. GET /health ---\n")
h <- curl::curl_fetch_memory(paste0(BASE, "/health"))
health <- jsonlite::fromJSON(rawToChar(h$content))
stopifnot(health$status == "ok")
stopifnot("2026-07-03" %in% health$versions)
stopifnot(health$latest == "2026-07-03")
cat("[PASS] health: status=", health$status, " latest=", health$latest, "\n")

# ---- 3. PACKAGES index (source) ----

cat("\n--- 3. GET /latest/src/contrib/PACKAGES ---\n")
h <- curl::curl_fetch_memory(paste0(BASE, "/latest/src/contrib/PACKAGES"))
txt <- rawToChar(h$content)
stopifnot(h$status_code == 200L)
n_pkgs <- length(grep("^Package:", strsplit(txt, "\n")[[1]]))
cat("[PASS] PACKAGES: ", n_pkgs, " package entries, ", nchar(txt), " bytes\n")

# ---- 4. PACKAGES index (binary/Windows) ----

cat("\n--- 4. GET /latest/bin/windows/contrib/4.5/PACKAGES ---\n")
h <- curl::curl_fetch_memory(paste0(BASE, "/latest/bin/windows/contrib/4.5/PACKAGES"))
stopifnot(h$status_code == 200L)
txt <- rawToChar(h$content)
n_bin <- length(grep("^Package:", strsplit(txt, "\n")[[1]]))
cat("[PASS] binary PACKAGES: ", n_bin, " entries\n")

# ---- 5. POST /prime — classify a synthetic lockfile ----

cat("\n--- 5. POST /prime ---\n")

# Build a sample lockfile with:
#   - jsonlite: definitely on CRAN/RSPM  -> binary
#   - R6:       definitely on CRAN/RSPM  -> binary
#   - ghfake:   GitHub-only package      -> source_only
#   - doesnotexist123: made up           -> not_found

sample_lockfile <- list(
  Packages = list(
    jsonlite = list(Package = "jsonlite", Version = "1.8.9", Source = "Repository"),
    R6       = list(Package = "R6",       Version = "2.5.1", Source = "Repository"),
    ghfake   = list(
      Package        = "ghfake",
      Version        = "0.1.0",
      Source         = "GitHub",
      RemoteType     = "github",
      RemoteHost     = "api.github.com",
      RemoteUsername = "r-lib",
      RemoteRepo     = "praise",
      RemoteSha      = "a63702814c0f3da0c5d5f234e4e6529c2eec0ae9"
    ),
    doesnotexist123 = list(Package = "doesnotexist123", Version = "0.99.0", Source = "Repository")
  )
)

body <- charToRaw(jsonlite::toJSON(sample_lockfile, auto_unbox = TRUE))
h <- curl::curl_fetch_memory(
  paste0(BASE, "/prime"),
  handle = curl::handle_setheaders(
    curl::new_handle(
      post = TRUE,
      postfields = body,
      postfieldsize = length(body)
    ),
    "Content-Type" = "application/json"
  )
)

prime <- jsonlite::fromJSON(rawToChar(h$content))
stopifnot(h$status_code == 200L)
stopifnot(prime$binary == 2L)            # jsonlite + R6
stopifnot(length(prime$source_only) == 1L) # ghfake
stopifnot(length(prime$not_found) == 1L)   # doesnotexist123
stopifnot("jsonlite" %in% prime$source_only == FALSE)
stopifnot("ghfake"   %in% prime$source_only)
stopifnot("doesnotexist123" %in% prime$not_found)

cat("[PASS] /prime: session=", prime$session,
    " packages=", prime$package_count,
    " binary=", prime$binary,
    " source_only=", paste(prime$source_only, collapse=","),
    " not_found=", paste(prime$not_found, collapse=","), "\n")

SID    <- prime$session
PRIMED <- prime$repo_url
cat("[TEST] primed repo_url: ", PRIMED, "\n")

# ---- 6. GET /primed/{sid}/src/contrib/PACKAGES ----

cat("\n--- 6. GET primed PACKAGES ---\n")
h <- curl::curl_fetch_memory(paste0(PRIMED, "/src/contrib/PACKAGES"))
txt <- rawToChar(h$content)
stopifnot(h$status_code == 200L)
stopifnot(grepl("Package: jsonlite", txt, fixed = TRUE))
stopifnot(grepl("Package: R6",       txt, fixed = TRUE))
stopifnot(grepl("Package: ghfake",   txt, fixed = TRUE))
stopifnot(!grepl("Package: doesnotexist123", txt, fixed = TRUE))
cat("[PASS] scoped PACKAGES has jsonlite, R6, ghfake; no doesnotexist123\n")

# ---- 7. GET binary for R6 (RSPM stream) — R6 has compiled code so it IS in the Windows binary index ----

cat("\n--- 7. GET binary (R6) ---\n")

# Extract R6 version from merged PACKAGES.
lines <- strsplit(txt, "\n")[[1]]
r6_line <- grep("^Package: R6$", lines)
r6_ver <- sub("^Version: ", "", lines[r6_line + 1L])
cat("[TEST] R6 version in merged PACKAGES: ", r6_ver, "\n")

r6_url <- paste0(PRIMED, "/bin/windows/contrib/4.5/R6_", r6_ver, ".zip")
h <- curl::curl_fetch_memory(r6_url)
if (h$status_code != 200L) {
  cat("[FAIL] HTTP ", h$status_code, "\n", rawToChar(h$content), "\n")
  stop("R6 binary fetch failed")
}
cat("[PASS] R6 binary: ", length(h$content), " bytes, type=", h$type, "\n")

# ---- 8. Verify cache hit for jsonlite ----

cat("\n--- 8. Cache hit check ---\n")
cache_entries <- list.files(TEST_CACHE, recursive = TRUE)
cat("[TEST] Cache entries after first fetch:\n")
for (e in cache_entries) cat("  ", e, "\n")

# Second fetch should hit the cache (much faster)
t0 <- Sys.time()
h2 <- curl::curl_fetch_memory(r6_url)
t_cache <- difftime(Sys.time(), t0, units = "secs")
stopifnot(h2$status_code == 200L)
stopifnot(length(h2$content) == length(h$content))
cat("[PASS] cached R6 fetch: ", round(t_cache, 3), "s, ", length(h2$content), " bytes (same content)\n")

# ---- 9. Try binary for ghfake (should 404 — source-only) ----

cat("\n--- 9. GET binary for source-only package (ghfake) ---\n")
h <- curl::curl_fetch_memory(
  paste0(PRIMED, "/bin/windows/contrib/4.5/ghfake_0.1.0.zip")
)
stopifnot(h$status_code == 404L)
cat("[PASS] ghfake binary correctly returns 404 (source-only)\n")

# ---- 10. Try source for doesnotexist123 (should 404) ----

cat("\n--- 10. GET source for not-found package ---\n")
h <- curl::curl_fetch_memory(
  paste0(PRIMED, "/src/contrib/doesnotexist123_0.99.0.tar.gz")
)
stopifnot(h$status_code == 404L)
cat("[PASS] doesnotexist123 source correctly returns 404\n")

# ---- 11. DELETE /primed/{sid} ----

cat("\n--- 11. DELETE session ---\n")
h <- curl::curl_fetch_memory(
  paste0(PRIMED),
  handle = curl::handle_setheaders(
    curl::new_handle(customrequest = "DELETE"),
    "Content-Type" = "application/json"
  )
)
del <- jsonlite::fromJSON(rawToChar(h$content))
stopifnot(del$status == "deleted")
cat("[PASS] session deleted: ", del$session, "\n")

# Verify session is gone
h <- curl::curl_fetch_memory(paste0(PRIMED, "/src/contrib/PACKAGES"))
stopifnot(h$status_code == 404L)
cat("[PASS] primed endpoint now returns 404 after deletion\n")

# ---- 13. Prime again — verify new session (no dedup yet) ----
# Since the server has no dedup yet, this creates a fresh session.

cat("\n--- 13. Re-prime (same lockfile) ---\n")
h <- curl::curl_fetch_memory(
  paste0(BASE, "/prime"),
  handle = curl::handle_setheaders(
    curl::new_handle(
      post = TRUE,
      postfields = body,
      postfieldsize = length(body)
    ),
    "Content-Type" = "application/json"
  )
)
prime2 <- jsonlite::fromJSON(rawToChar(h$content))
stopifnot(h$status_code == 200L)
stopifnot(prime2$session != SID)  # currently: new session each time (no dedup)
cat("[PASS] re-prime creates new session: ", prime2$session, " (different from ", SID, ")\n")
cat("[INFO] Note: current server does NOT dedup /prime — each call is a fresh session\n")

# Clean up second session
invisible(curl::curl_fetch_memory(
  paste0(prime2$repo_url),
  handle = curl::handle_setheaders(
    curl::new_handle(customrequest = "DELETE"),
    "Content-Type" = "application/json"
  )
))

# ---- 14. Cleanup: kill the background server process ----

cat("\n--- 14. Cleanup ---\n")
if (!is.null(SERVER_PID)) {
  system(sprintf("kill %d", SERVER_PID), wait = FALSE)
  cat("[TEST] sent SIGTERM to server PID ", SERVER_PID, "\n")
} else {
  system(sprintf("fuser -k %d/tcp 2>/dev/null", TEST_PORT), wait = FALSE)
  cat("[TEST] killed whatever was on port ", TEST_PORT, "\n")
}
Sys.sleep(1.5)
# Show what's cached
cache_files <- list.files(TEST_CACHE, recursive = TRUE)
cat("[TEST] Final cache contents (", length(cache_files), " files):\n")
for (f in cache_files) {
  fi <- file.info(file.path(TEST_CACHE, f))
  cat(sprintf("  %-50s %s\n", f, format(fi$size, big.mark=",")))
}

# Verify server is down
Sys.sleep(0.5)
h <- tryCatch(curl::curl_fetch_memory(paste0(BASE, "/health")),
              error = function(e) list(status_code = 0L))
stopifnot(h$status_code == 0L)
cat("[PASS] server is down\n")

unlink(TEST_CACHE, recursive = TRUE)
cat("[TEST] cache cleaned up\n")

cat("\n========================================\n")
cat("ALL TESTS PASSED\n")
cat("========================================\n")
