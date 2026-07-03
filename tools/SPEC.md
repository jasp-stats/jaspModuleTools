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
> - **Caching removed** — server is now a pure streaming proxy. No disk writes
>   for binaries; everything is fetched on demand and streamed to R.
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

**Pure streaming proxy.** The server holds only in-memory metadata (merged
PACKAGES + session map). It never writes binaries to disk — every binary/source
request fetches from upstream and streams the bytes directly to R's HTTP client.
R writes to its own `tempdir()` during `install.packages()` as usual.

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

The server checks each package in the lockfile against the merged index. Packages
fall into three buckets:

| Bucket | Meaning |
|--------|---------|
| **binary** | Found in a pinned source (jasp-repo / RSPM). Binary will be streamed. |
| **source_only** | Not in any pinned source, but the lockfile provides enough info to fetch a source tarball (e.g. `Source: "GitHub"` with `RemoteSha`, or `Source: "Repository"` with a `Repository` URL). The server includes it in the scoped PACKAGES. R will request the source `.tar.gz`, the server fetches from the original location, and R compiles locally. |
| **not_found** | Not in any pinned source AND no source info to fall back on. Listed so the client can abort or warn. |

**Response:**
```json
{
  "session": "a3f8c2d1",
  "repo_url": "http://localhost:8765/primed/a3f8c2d1",
  "package_count": 47,
  "binary": 43,
  "source_only": ["randomGithubPkg", "obscureForksPkg"],
  "not_found": []
}
```

#### `GET /primed/{session}/src/contrib/PACKAGES`

Scoped DCF containing **all** packages from the lockfile — both `binary` and
`source_only`. For `source_only` packages, the PACKAGES entry carries the remote
metadata (`RemoteType`, `RemoteSha`, etc.) so R can locate and fetch the source.

#### `GET /primed/{session}/bin/{binary_path}/{pkg}_{ver}.{tgz,tar.gz,zip}`

**Streaming binary handler — no caching:**
1. Resolve the upstream source from the merged entry's origin:
   - **jasp-repo package:** `https://repo.jasp-stats.org/{version}/{r_version}/{os}/{arch}/{filename}`
   - **CRAN/RSPM package:** `{base_url}/bin/{binary_path}/{filename}`
2. Fetch from upstream → **stream bytes directly to client** (server does not write to disk)
3. Upstream 404 → return 404. R falls back to `type = "source"` and requests the
   source tarball from the same primed endpoint.

#### `GET /primed/{session}/src/contrib/{pkg}_{ver}.tar.gz`

**Streaming source handler — no caching:**
1. Check if the package exists in a pinned source's `src/contrib`:
   - **RSPM source:** `{base_url}/src/contrib/{filename}`
   - **jasp-repo:** `{jasp_url}/{filename}`
2. If not found in any pinned source, fall back to the **original source** declared
   in the lockfile:
   - **GitHub package:** `https://api.github.com/repos/{RemoteUsername}/{RemoteRepo}/tarball/{RemoteSha}`
     - Attach `Authorization: Bearer {github_pat}` header if a PAT is available
       (see `start_jasp_development(github_pat = ...)` in §3). Without a PAT the
       unauthenticated rate limit is 60 requests/hour.
   - **External repository:** `{Repository}/src/contrib/{filename}`
     (if `Repository` is set in the lockfile record)
3. Fetch from upstream → **stream bytes directly to client**
4. All sources fail → 404 with descriptive error.

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
    config_url = NULL,   # defaults to config_url in cached repos.json
    os         = NULL,   # auto-detected
    arch       = NULL,   # auto-detected
    r_version  = NULL,   # auto-detected
    distro     = NULL,   # Linux only (e.g. "noble"); ignored on macOS/Windows
    github_pat = NULL,   # falls back to GITHUB_PAT / GITHUB_TOKEN env vars
    port       = 8765
)
```

1. Resolve `github_pat`: use the param, else `Sys.getenv("GITHUB_PAT")`, else
   `Sys.getenv("GITHUB_TOKEN")`. Store for use by the source fallback handler.
2. Fetch `repos.json`
3. Pre-merge PACKAGES for `latest`
4. Start `httpuv` server
5. Set `options(jasp.local_repo = "http://localhost:8765")`
6. Register cleanup on session exit (server dies with the R process)

### `stop_jasp_development()`

Kill the server.

### `updateLockfile()`

```r
updateLockfile <- function(moduledir, version = "latest")
```

1. `pkgdepends` resolves against `http://localhost:8765/{version}`
2. Injects `JASP.RepoVersion` into lockfile
3. Writes `renv.lock`

> **Requires the server to be running.** Call `start_jasp_development()` first,
> or `updateLockfile()` should auto-start it if not running.
>
> **Caveat:** until the jasp-repo publishes a real PACKAGES file (with `Depends`/
> `Imports` fields), dependency resolution for jasp-internal packages may be
> incomplete — the directory-listing-derived PACKAGES carries name + version only.
> For now, modules should declare all their jasp dependencies explicitly.

### `compile()`

```r
compile <- function(moduledir, workdir, ...)
```

1. Read lockfile, `POST /prime`
2. `install.packages(pkgs, lib = pkglib, repos = "http://localhost:8765/primed/{session}", type = "binary")`
3. Install module from source
4. macOS: `fix_mac_linking()` + codesign (unchanged)
5. Bundle (unchanged)
6. `DELETE /primed/{session}` (via `withr::defer()`)

> **API surface note:** the existing `compile()` signature carries several params
> (`bundleAll`, `repoName`, `localCellar`, `localizeJASPModules`, etc.) that need
> review against this design. With caching removed, `localCellar` likely goes away.
> Final signature TBD during Phase 2 implementation.

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

### Phase 1: Server (`tools/repo_server.R`)
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
