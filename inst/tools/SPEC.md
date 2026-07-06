# JASP Local Repo Server — Specification v2.1

> **Purpose:** Replace `renv::restore()` with a local HTTP server that exposes a
> merged, version-pinned CRAN-like repository. Two frozen sources per version:
> RSPM (date) and jasp-repo (arbitrary string). External source URLs from the
> lockfile are fetched as a source-only fallback.
>
> **Status:** To be implemented in R (`httpuv`). HTTP surface is language-agnostic.
>
> **Changelog (v2.1):**
> - r-universe source **dropped** (deferred — requires snapshot-publishing infra;
>   currently builds only one package, failing). Revisit later.
> - RSPM binary paths **verified** against `packagemanager.posit.co`.
> - **Binary caching is optional (opt-in).** Set `cache_path` or
>   `options(jasp.repo_cache_path)` to enable on-disk caching. Without it, every
>   binary is re-fetched from upstream on each build.
> - Merge rule **clarified**: highest version wins; source priority breaks ties only.
> - jasp-repo PACKAGES built from directory listing (name + version from filename).
>   A real PACKAGES file will be published upstream in a future revision.
> - GitHub PAT support for source fallback.

---

## 1. Configuration (`repos.json`)

Hosted on GitHub. Fetched by the server on startup. No indirection — each version
block directly contains the values substituted into URL templates.

```jsonc
{
  "config_url": "https://raw.githubusercontent.com/jasp-stats/jasp-repo-config/main/repos.json",

  // ── Source URL templates ────────────────────────────────────────
  // `{...}` placeholders are substituted per version.

  "rspm": {
    "base_url": "https://packagemanager.posit.co/cran/{date}"
    // Source PACKAGES:  {base_url}/src/contrib/PACKAGES
    // Binary paths are constructed server-side from OS/arch/r_version (see §2.1).
  },

  "jasp-repo": {
    "url": "https://repo.jasp-stats.org/{version}/{r_version}/{os}/{arch}"
    // Flat directory of .tar.gz files (Apache index is available).
    // Server scrapes the directory listing and builds a minimal PACKAGES
    // from filenames (Package + Version only).
  },

  // ── Version map ─────────────────────────────────────────────────
  // Keys are arbitrary labels (used as URL segments).
  // Values pin both sources — fully deterministic.

  "versions": {
    "9120": {
      "rspm":       "2025-06-01",
      "jasp-repo":  "development"
    },
    "8975": {
      "rspm":       "2025-03-15",
      "jasp-repo":  "0.19.3"
    }
  },

  // Which version is served under /latest/.
  "latest": "9120"
}
```

### Merge priority

**The highest version wins.** Source priority is only used to break ties
(same package, same version appearing in multiple sources):

1. **RSPM** — canonical CRAN packages (source + binaries)
2. **jasp-repo** — JASP internal packages and cached binaries
3. **External** — original source URL from lockfile (source-only, compiled locally)

### Platform parameters (resolved at server startup)

The server resolves the following from the runtime environment at startup
(`Sys.info()`, `R.Version()`, and config). These determine which binary paths
are fetched and served:

| OS | Binary path segment | Notes |
|---|---|---|
| **macOS** (R ≥ 4.3) | `macosx/big-sur-{arch}` | `{arch}` = `arm64` or `x86_64` |
| **macOS** (legacy R < 4.3) | `macosx` | No platform subdir |
| **Windows** | `windows` | No arch segment |
| **Linux** | `linux/{distro}-{arch}` | `{distro}` from config; **404 → source fallback** |

> **Note:** macOS uses `macosx` (CRAN's legacy name), NOT `macos`. R 4.3+ appends
> a `big-sur-{arch}` platform directory. The server must construct the path from
> the runtime arch — it cannot assume a fixed string.

`r_version` is the major.minor R version (e.g. `4.5`, `4.4`).

---

## 2. Server HTTP Surface

Default: `http://localhost:8765`

**Streaming proxy with optional binary caching.** The server holds only in-memory
metadata (merged PACKAGES + session map). By default, every binary/source
request fetches from upstream and streams the bytes directly to R's HTTP client.
If a `cache_path` is configured (via param or `options(jasp.repo_cache_path)`),
binaries are cached on disk after first download — subsequent builds reuse the
cached files. R writes to its own `tempdir()` during `install.packages()` as usual.

The server's in-memory state lives for the duration of the R session that started
it. When that R session exits, the server and all sessions die. No persistence,
no crash recovery, no cache invalidation to reason about.

### 2.1 Versioned metadata endpoints

Serve the **full merged PACKAGES** for a pinned version. Used by `updateLockfile()`
/ `pkgdepends` for dependency resolution.

| Method | Path |
|--------|------|
| `GET` | `/{version}/src/contrib/PACKAGES` |
| `GET` | `/{version}/bin/{binary_path}/PACKAGES` |
| `GET` | `/latest/src/contrib/PACKAGES` |
| `GET` | `/latest/bin/{binary_path}/PACKAGES` |

`{version}` is any key from `config.versions`. `/latest/` aliases to the configured latest.
`{binary_path}` is the platform-specific segment resolved at startup (see table in §1),
e.g. `macosx/big-sur-arm64/contrib/4.5` or `windows/contrib/4.5`.

The server must serve PACKAGES at **whatever binary path R requests** for the
current platform. Route by wildcard under `/bin/` and serve the merged binary
PACKAGES.

**On first request for a version (lazy, in-memory):**

1. **RSPM source:** fetch `{base_url}/src/contrib/PACKAGES` (pinned `{date}`)
2. **RSPM binary:** fetch `{base_url}/bin/{binary_path}/PACKAGES`
   - Linux: if 404, skip binaries (source-only from RSPM)
3. **jasp-repo:** GET the directory index for `{jasp_url}/`, parse filenames:
   - Filter to `pkg_{version}.tar.gz` where `{version}` matches a version pattern
     (`^\d+([.\-]\d+)*$`). Ignore everything else (commit-SHA suffixes, junk).
   - For each, emit a minimal PACKAGES entry: `Package:` + `Version:` from the filename.
   - One entry per package (highest version if multiple match).
4. **Merge:** highest version wins; RSPM > jasp-repo breaks ties
5. Hold the merged result in memory (not on disk)

### 2.2 Primed session endpoints

Client submits a lockfile; the server builds a scoped PACKAGES with only those
packages, then streams binaries on demand.

#### `POST /prime`

**Request body:** renv lockfile JSON.

```json
{
  "R": { "Version": "4.5.0" },
  "JASP": { "RepoVersion": "9120" },
  "Packages": {
    "ggplot2": {
      "Package": "ggplot2",
      "Version": "3.5.1",
      "Source": "Repository"
    },
    "jaspGraphs": {
      "Package": "jaspGraphs",
      "Version": "0.11.2",
      "Source": "GitHub",
      "RemoteType": "github",
      "RemoteUsername": "jasp-stats",
      "RemoteRepo": "jaspGraphs",
      "RemoteSha": "abc123def"
    }
  }
}
```

`JASP.RepoVersion` tells the server which version's pinned sources to use.

**Package classification on `/prime`:**

The server classifies each lockfile package by checking the RSPM and jasp-repo
**binary** indexes (fetched fresh per prime, not the merged index):

| Bucket | Meaning |
|--------|---------|
| **binary** | A **binary** with the **exact lockfile version** exists in RSPM binary PACKAGES or jasp-repo. Binary will be streamed from the upstream source. |
| **source_only** | No matching binary found. Classified as source_only if: (a) the lockfile record has enough info to fetch a source tarball (`Source: "GitHub"` with `RemoteSha`/`RemoteRef`, or `Source: "Repository"` with a `Repository` URL), **or** (b) the package exists in the merged index (i.e. known to at least one pinned source) — in which case the scoped PACKAGES carries the merged entry with the lockfile's version. |
| **not_found** | No matching binary, no source info, and not present in the merged index at all. Returned so the client can abort or warn. |

**Response:**
```json
{
  "session": "a3F8c2D1e9B4",
  "repo_url": "http://localhost:8765/primed/a3F8c2D1e9B4",
  "package_count": 47,
  "binary": 43,
  "source_only": ["randomGithubPkg", "obscureForksPkg"],
  "not_found": []
}
```

| Field | Type | Description |
|-------|------|-------------|
| `session` | string (12 chars) | Random alphanumeric session ID |
| `repo_url` | string | Base URL for install.packages(type="binary") |
| `package_count` | integer | Number of packages in the scoped PACKAGES (binary + source_only) |
| `binary` | integer | Count of packages classified as binary |
| `source_only` | string[] | Package names that must be installed from source |
| `not_found` | string[] | Package names with no install path at all |

#### `GET /primed/{session}/src/contrib/PACKAGES`

Scoped DCF containing **every installable** lockfile package (`binary` +
`source_only`; `not_found` packages are excluded).

- **binary** entries use the **merged** DCF block with the Version field patched
to match the lockfile's pinned version.
- **source_only with source info** (GitHub/Repository) carry the full remote
metadata (`RemoteType`, `RemoteSha`, `RemoteRef`, `Repository`, etc.).
- **source_only without source info** (present in merged index but no exact binary
match) use the merged DCF block with the Version patched to the lockfile version.

#### `GET /primed/{session}/bin/{binary_path}/PACKAGES`

Scoped DCF containing **only** packages classified as `binary`. `source_only`
packages are excluded — R would try binary, get 404, and skip them instead of
falling back to source.

#### `GET /primed/{session}/bin/{binary_path}/{pkg}_{ver}.{tgz,tar.gz,zip}`

**Streaming binary handler (with optional caching):**

1. **Check cache.** If a binary cache directory is configured (via `cache_path`
   param or `options(jasp.repo_cache_path)`), serve from disk if present.
2. **Try RSPM binary:** `{base_url}/bin/{binary_path}/{filename}`
   - Success → cache raw bytes (if caching is on) → stream to client.
3. **Fall back to jasp-repo:** reconstruct the `.tar.gz` filename from the
   requested filename's version and fetch from
   `https://repo.jasp-stats.org/{version}/{r_version}/{os}/{arch}/{filename}`.
   - Success → **convert** (jasp-repo serves zstd-compressed, no-wrapper-dir
     tarballs; must be converted to gzip `.tgz`/`.zip` with `pkgname/` wrapper
     via libarchive) → cache converted bytes → stream to client.
4. All sources 404 → return 404. R falls back to `type = "source"` and requests
   the source tarball.

#### `GET /primed/{session}/src/contrib/{pkg}_{ver}.tar.gz`

**Streaming source handler (no disk caching for source tarballs):**

Resolved by package origin — checked in this order:

1. **GitHub package** (entry has `RemoteUsername` + `RemoteRepo` + `RemoteSha`/`RemoteRef`):
   `https://api.github.com/repos/{RemoteUsername}/{RemoteRepo}/tarball/{RemoteSha}`
   - Attaches `Authorization: Bearer {github_pat}` if a PAT is available.
   Without a PAT the unauthenticated rate limit is 60 requests/hour.
   - GitHub source is used **exclusively** — no fallback to pinned sources.
2. **External repository** (entry has a `Repository` URL):
   `{Repository}/src/contrib/{filename}`
3. **CRAN package** (all others — RSPM/jasp-repo origin, or no origin metadata):
   - First: `{rspm_base_url}/src/contrib/{filename}`
   - Fallback: `https://cloud.r-project.org/src/contrib/{filename}`
4. All sources fail → 404 with descriptive error.

> **Note:** jasp-repo does not serve source tarballs. jasp-repo packages that end
> up in `source_only` (version mismatch with the binary index) will be tried as
> CRAN packages (step 3) and likely 404. For now, lockfiles should pin jasp-repo
> packages to a version that exists in the jasp-repo binary directory.

#### `DELETE /primed/{session}`

Expire a session. `compile()` should call this on exit (via `withr::defer()`) to
avoid session accumulation.

### 2.3 Health

`GET /health` → `{"status":"ok","versions":["9120","8975"],"latest":"9120","sessions":3}`

---

## 3. R Package Integration

### `start_jasp_development()`

```r
start_jasp_development <- function(
    update_lockfile = FALSE,
    library         = "./build_lib",
    port            = 8765L,
    config_url      = NULL,   # defaults to repos.json bundled in the package
    github_pat      = NULL    # falls back to GITHUB_PAT / GITHUB_TOKEN env vars
)
```

1. Create an isolated build library at `library` (exported as `JASP_PKG_LIBRARY` env var).
2. Call `ensure_repo_server()` — if a server isn't already running, spawn one in
   a **background R process** (so the event loop doesn't block the main session).
3. Set `options(repos = c(CRAN = "http://localhost:8765/latest"))`.
4. Optionally run `updateLockfile("./")` if `update_lockfile = TRUE`.

### `stop_jasp_development()`

Kill the background server process.

### `updateLockfile()`

```r
updateLockfile <- function(moduledir, jaspModuleDependenciesOnly = FALSE,
                           version = "latest")
```

1. Ensure the server is running (auto-starts if needed).
2. `pkgdepends` resolves against `http://localhost:8765/{version}`.
3. Merge with any locked (JASP_LOCK) records from the existing lockfile.
4. If `jaspModuleDependenciesOnly = TRUE`, only refresh `jasp-*` packages.
5. Inject `JASP.RepoVersion` into lockfile.
6. Write `renv.lock`.

> **Caveat:** until the jasp-repo publishes a real PACKAGES file (with `Depends`/
> `Imports` fields), dependency resolution for jasp-internal packages may be
> incomplete — the directory-listing-derived PACKAGES carries name + version only.
> For now, modules should declare all their jasp dependencies explicitly.

### `build()`

```r
build <- function(moduledir = "./", update_lockfile = FALSE,
                  build_bundle = FALSE, library = NULL)
```

1. Ensure lockfile exists (auto-create if missing, or refresh if
   `update_lockfile = TRUE`).
2. Read lockfile, extract `JASP.RepoVersion`.
3. `POST /prime` with the raw lockfile JSON.
4. Topological-sort source-only packages by dependency count.
5. In a clean **subprocess** (`callr::r()`), run `install.packages()` with
   `type = "binary"` pointing at `http://localhost:8765/primed/{session}`.
   Source-only packages are installed one-at-a-time in dependency order.
6. `DELETE /primed/{session}` (via `tryCatch(..., finally = ...)`).
7. macOS: `fix_mac_linking()` (unchanged).
8. Optional: create `.JASPModule` bundle via `jaspModuleBundleManager`.

---

## 4. Removed

Everything that touched `renv` internals:

- `RENV_PATHS_*` env vars
- `renv:::renv_sandbox_activate()`
- `renv:::renv_paths_cache()`
- `renv::restore()`
- `gatherRemoteCellar()`
- `expandCellarIntoRenvCache()`
- `~/.jasp/cellar/` disk cache (replaced by streaming)

`renv` kept only for `lockfile_read()` / `lockfile_write()` (JSON parsing). Droppable
with `jsonlite` later.

---

## 5. Implementation Phases

### Phase 1: Server (`inst/tools/repo_server.R`)
- Config fetch + validation
- Platform parameter resolution (os, arch, r_version, distro, macos platform)
- PACKAGES fetch from RSPM (source + binary)
- jasp-repo directory scraping + minimal PACKAGES generation (version-pattern filter)
- Merge logic (highest version wins; RSPM > jasp-repo tiebreak)
- `httpuv` server: version endpoints, prime/session endpoints, health
- Streaming binary/source handlers (no disk writes)
- GitHub PAT header injection for source fallback

### Phase 2: R package refactor
- `start_jasp_development()` / `stop_jasp_development()`
- `updateLockfile()` — resolve against local server
- `compile()` — prime + install from session + session cleanup

### Phase 3 (unchanged)
- macOS `fix_mac_linking()` + codesign
- `jaspModuleBundleManager::createJaspModuleBundle()`
