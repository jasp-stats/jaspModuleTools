# JASP Local Repo Server — Testing Guide

## Architecture (30-second version)

```
┌──────────────┐     ┌────────────────┐     ┌───────────────┐
│  R / RStudio │────▶│  local server  │────▶│  RSPM (CRAN)  │
│  pkgdepends  │     │  :8765         │     │  date-pinned  │
│  install.pkg │     │                │     │               │
└──────────────┘     │  merged        │────▶│  jasp-repo    │
                     │  PACKAGES      │     │  binaries     │
                     │                │     │               │
                     │  /prime        │────▶│  GitHub       │
                     │  session cache │     │  (Remotes)    │
                     └────────────────┘     └───────────────┘
```

The server merges two frozen sources per date-stamped version (RSPM + jasp-repo) into a single CRAN-like repository. It also proxies GitHub source tarballs for packages listed under `Remotes:` in DESCRIPTION.

## What we've built

| File | Purpose |
|---|---|
| `tools/repo_server.R` | The HTTP server (httpuv) — merged PACKAGES, `/prime`, `/primed/{sid}/...`, binary cache & jasp-repo format conversion |
| `tools/repos.json` | Config — RSPM dates, jasp-repo versions, `latest` pointer |
| `tools/repo_server.md` | Detailed architecture docs with Mermaid diagrams |
| `tools/test_repo_server.R` | Automated end-to-end test (simulates Windows) |
| `R/utils.R` | Helpers: `server_is_running()`, `ensure_repo_server()`, `resolve_version_for_server()` |
| `R/jaspModuleTools.R` | Updated `start_jasp_development()`, `compile()`, `updateLockfile()` |

## Two developer flows

### Flow 1 — Normal dev (RStudio "Install" button)

```r
start_jasp_development()
# Server starts. options(repos = c(CRAN = "http://localhost:8765/latest")) is set.
# Now any install.packages() / RStudio Install button / renv::install()
# resolves through the local server.
```

- ✅ No new workflow to learn
- ✅ RStudio "Install and Restart" just works
- ✅ `Remotes:` in DESCRIPTION → GitHub source (jaspBase, jaspGraphs, etc.)
- ❌ No version pinning — gets whatever is current in the RSPM snapshot

### Flow 2 — Reproducible build (JASP team)

```r
updateLockfile("~/jaspTTests")   # resolve deps, inject JASP.RepoVersion
compile("~/jaspTTests")          # prime + install from exact snapshot
```

- ✅ Byte-identical installs via `/prime` + `/primed/{sid}`
- ✅ Lockfile pinned to a date stamp (e.g. `JASP.RepoVersion: 2026-07-03`)
- ✅ Binary cache for RSPM packages
- ✅ jasp-repo binaries auto-converted from zstd → gzip

## Automated test (`tools/test_repo_server.R`)

### What it tests

14 steps simulating a **Windows x86_64, R 4.5** platform:

| # | Test | What it verifies |
|---|---|---|
| 1 | Server startup | Background process, PID capture |
| 2 | `GET /health` | Status, versions, latest |
| 3 | `GET /latest/src/contrib/PACKAGES` | Merged index (24k+ packages) |
| 4 | `GET /latest/bin/.../PACKAGES` | Binary index (Windows .zip) |
| 5 | `POST /prime` | Classification: binary=2, source_only=1, not_found=1 |
| 6 | `GET /primed/{sid}/PACKAGES` | Scoped index (only lockfile packages) |
| 7 | `GET /primed/{sid}/bin/.../R6.zip` | Binary download from RSPM |
| 8 | 2nd fetch of same binary | Cache hit (< 0.1s) |
| 9 | Binary for source-only pkg | 404 (correct) |
| 10 | Source for not-found pkg | 404 (correct) |
| 11 | `DELETE /primed/{sid}` | Session deleted |
| 12 | Verify session gone | 404 after deletion |
| 13 | Re-prime same lockfile | New session (no dedup yet) |
| 14 | Cleanup | Cache listing, server killed |

### Running

```bash
# From project root
Rscript tools/test_repo_server.R
```

Expected output: `ALL TESTS PASSED`

### Important: why background process

httpuv requires R's event loop to dispatch callbacks. In `Rscript` (batch mode), the main thread is busy, so curl requests deadlock. The test starts the server in a **separate background R process** via `system(..., wait=FALSE)` to avoid this.

In interactive RStudio, this isn't needed — the event loop runs naturally.

## Manual test: `updateLockfile()` on a real module

Tested against `/home/sp42/modules-registry/Official/jaspTTests` (Linux, R 4.6).

### Steps

```bash
# 1. Start server in background
Rscript -e 'source("tools/repo_server.R");
  start_repo_server(config_url="tools/repos.json", port=8765L);
  while(TRUE) { httpuv::service(200); Sys.sleep(0.01) }' &

# 2. Wait for pre-merge (~5s for 24k packages)
sleep 5

# 3. Run updateLockfile
Rscript -e '
  source("R/utils.R"); source("R/jaspModuleTools.R")
  options(jasp.local_repo = "http://localhost:8765")
  updateLockfile("/path/to/jaspModule", version = "latest")
'
```

### Verified results

| Check | Result |
|---|---|
| Packages resolved | 127 (2024-12-15 snapshot) / 136 (2026-07-03 snapshot) |
| ggplot2 | 3.5.1 (old) vs 4.0.3 (new) — **version pinning works** |
| jaspBase, jaspGraphs | Resolved via `Remotes:` → GitHub |
| `JASP.RepoVersion` in lockfile | Correct date stamp |
| Lockfile written | `renv.lock` updated in module dir |

### Gotcha: pkgdepends repos name

pkgdepends **requires** the repo to be named `CRAN` in `options(repos)`:

```r
# ❌ pkgdepends ignores this
options(repos = c(JASP = "http://localhost:8765/latest"))

# ✅ pkgdepends uses this
options(repos = c(CRAN = "http://localhost:8765/latest"))
```

## What's next (compile() development)

The `compile()` function currently uses the old `gatherRemoteCellar()` + `expandCellarIntoRenvCache()` flow. Phase 2 replaces this with:

```r
compile <- function(moduledir, ...) {
  ensure_repo_server(version = repoName)

  # 1. Read lockfile, POST /prime → get primed repo URL
  lockfile <- renv::lockfile_read(file.path(moduledir, "renv.lock"))
  body <- jsonlite::toJSON(lockfile)
  prime <- curl::curl_fetch_memory(paste0(base, "/prime"),
    handle = curl::handle_setheaders(
      curl::new_handle(post = TRUE, postfields = body),
      "Content-Type" = "application/json"))
  primed <- jsonlite::fromJSON(rawToChar(prime$content))

  # 2. install.packages() from the primed session
  withr::defer(DELETE(paste0(primed$repo_url)))  # cleanup
  install.packages(pkgs, repos = primed$repo_url, type = "binary")

  # 3. Install the module itself from source
  install.packages(moduledir, repos = NULL, type = "source")
}
```

### Key decisions for compile()

- **`updateLockfile = FALSE` by default** — uses existing lockfile as-is; set `TRUE` to auto-refresh before priming
- **What replaces `gatherRemoteCellar`?** — The server's binary streaming + cache
- **What replaces `expandCellarIntoRenvCache`?** — `install.packages(type = "binary")` handles it
- **MacOS linking fix** — `fix_mac_linking()` stays as-is (unrelated to server)
- **Bundle creation** — `jaspModuleBundleManager::createJaspModuleBundle()` stays as-is
