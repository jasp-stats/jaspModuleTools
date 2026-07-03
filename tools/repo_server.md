# JASP Local Repo Server — Documentation

> **Implementation:** `tools/repo_server.R` (Phase 1)
> **Spec:** `tools/SPEC.md` (v2.1)
>
> A pure streaming proxy that exposes a **merged, version-pinned, CRAN-like
> repository** on `localhost`. R's `install.packages()` / `pkgdepends` talk to
> it like any normal repo — no `renv` internals, no cellar, no disk cache.

---

## 1. High-level architecture

The server sits between R and three upstream package sources. It holds only
in-memory metadata; every binary/source tarball is fetched on demand and
streamed straight to R without touching disk.

```mermaid
flowchart TD
    R["R session\ninstall.packages / pkgdepends"]

    subgraph Local["Local repo server - httpuv port 8765"]
        direction TB
        Router["Request router"]
        Merge["Merged PACKAGES\nin-memory, lazy + cached"]
        Sessions["Session map\nin-memory"]
        Streamer["Streaming proxy\nfetch upstream then pipe to client"]
    end

    subgraph Upstreams["Frozen upstream sources"]
        RSPM["RSPM\npackagemanager.posit.co/cran/{date}\nCRAN packages: source + binaries"]
        JASP["jasp-repo\nrepo.jasp-stats.org/{ver}/{R-ver}/{os}/{arch}\nJASP internal packages"]
        GH["GitHub\napi.github.com/repos/.../tarball/{sha}\nExternal source fallback"]
    end

    R -- "HTTP: PACKAGES + tarballs" --> Router
    Router --> Merge
    Router --> Sessions
    Router --> Streamer
    Merge -. "lazy fetch on first request" .-> RSPM
    Merge -. "scrape directory listing" .-> JASP
    Streamer -- "fetch binary/source" --> RSPM
    Streamer -- "fetch binary/source" --> JASP
    Streamer -- "fetch source (PAT)" --> GH
```

**Key properties:**

- **Streaming proxy with optional cache** — binaries are fetched from upstream
  and streamed to R. jasp-repo binaries are converted to the format R expects
  (gzip `.tgz` / `.zip` with a wrapper directory) on first download, then cached
  on disk for subsequent requests. RSPM binaries are cached as-is.
- **In-memory metadata** — the merged index and session map live for the
  duration of the R session that started the server. No persistence, no crash
  recovery.
- **Version-pinned** — each version key pins both a RSPM snapshot date and a
  jasp-repo label, giving a fully deterministic dependency set.
- **Single-threaded for R callbacks** — `httpuv` serializes R callbacks. R's
  default `install.packages()` is sequential, so this is not a correctness issue
  (parallel downloads via `Ncpus`/`pkgdepends` will serialize).

---

## 2. The three package sources

| Source | Priority | What it provides | URL pattern |
|--------|----------|------------------|-------------|
| **RSPM** | 1 (highest) | Canonical CRAN packages, source + binaries | `/cran/{date}/...` |
| **jasp-repo** | 2 | JASP internal packages (**binary only**) | `/{version}/{R-version}/{os}/{arch}/` |
| **External** | 3 (lowest) | Source-only fallback from the lockfile | GitHub tarballs, external repos |

> **jasp-repo is binary-only.** Its `.tar.gz` files are zstd-compressed R binary
> packages with files at the top level (no `pkgname/` wrapper directory). R's
> `install.packages(type="binary")` expects gzip `.tgz` (macOS) / `.zip`
> (Windows) / gzip `.tar.gz` (Linux) with a `pkgname/` wrapper. The server
> converts jasp-repo binaries on first download using libarchive (auto-detects
> compression) and caches the result. Source requests for jasp-repo packages
> return 404.

### Merge rule

**Highest version wins.** Source priority (RSPM > jasp-repo > External) only
breaks ties when the *same* version appears in multiple sources.

```mermaid
flowchart TD
    Start["For each package name across all sources"] --> Check["Package already in merged index?"]
    Check -- No --> Insert["Insert entry"]
    Check -- Yes --> Cmp["Compare versions"]
    Cmp -- "new greater than existing" --> Replace["Replace with new entry"]
    Cmp -- "new less than existing" --> Keep["Keep existing entry"]
    Cmp -- "versions equal" --> Tie["Lower priority number wins\nRSPM=1, jasp=2, external=3"]
    Insert --> Done["Next package"]
    Replace --> Done
    Keep --> Done
    Tie --> Done
```

### jasp-repo directory scraping

jasp-repo serves an Apache directory index. The server scrapes `<a href>` entries
and builds minimal `PACKAGES` entries (`Package` + `Version` from filename only):

```
jaspAnova_0.95.5.tar.gz   →  Package: jaspAnova, Version: 0.95.5   ✓ (version pattern)
jaspAnova_120339c6.tar.gz →  ignored (8-char commit SHA, fails version pattern)
jaspBase_2aaba080.tar.gz  →  ignored (commit SHA)
```

Filter: `^\d+([.\-]\d+)*$` — must start with digits and use only `.` or `-`
separators. Commit SHAs and junk are silently dropped.

---

## 3. Platform resolution

At startup, the server resolves platform parameters that determine which RSPM
binary paths are fetched and how jasp-repo URLs are constructed:

```mermaid
flowchart TD
    Detect["Auto-detect from Sys.info + R.Version"] --> OS{"OS?"}

    OS -- Darwin --> Mac["os = MacOS"]
    OS -- Windows --> Win["os = Windows"]
    OS -- Linux --> Lin["os = Linux"]
    OS -- "FLATPAK_ID set" --> Flat["os = Flatpak"]

    Mac --> MacArch["arch = arm64 or x86_64"]
    Win --> WinArch["arch = x86_64"]
    Lin --> LinArch["arch = arm64 or x86_64"]
    Flat --> FlatArch["arch = arm64 or x86_64"]

    MacArch --> MacBP{"R >= 4.3?"}
    MacBP -- Yes --> MacPath["binary_path =\nmacosx/big-sur-{arch}/contrib/{r_ver}"]
    MacBP -- No --> MacLegacy["binary_path =\nmacosx/contrib/{r_ver}"]

    WinArch --> WinPath["binary_path =\nwindows/contrib/{r_ver}"]

    LinArch --> LinDistro{"distro set?"}
    LinDistro -- Yes --> LinPath["binary_path =\nlinux/{distro}-{arch}/contrib/{r_ver}"]
    LinDistro -- No --> LinSource["binary_path = NULL\nsource-only: try/404/fallback"]
    FlatArch --> LinSource
```

> **Note:** jasp-repo uses `macosx` (CRAN's legacy name), **not** `macos`. R 4.3+
> appends a `big-sur-{arch}` platform directory. The path is constructed from
> the runtime arch — it cannot assume a fixed string.

> **jasp-repo OS labels:** `MacOS`, `Windows`, `Flatpak`. There is no `Linux/`
> directory on jasp-repo — plain Linux dev machines (without `FLATPAK_ID`) get
> source-only from jasp-repo, consistent with the "404 → source" policy.

---

## 4. HTTP API

Default: `http://localhost:8765`

### 4.1 Versioned metadata endpoints

Serve the full merged `PACKAGES` for a pinned version. Used by
`updateLockfile()` / `pkgdepends` for dependency resolution.

| Method | Path |
|--------|------|
| `GET` | `/{version}/src/contrib/PACKAGES` |
| `GET` | `/{version}/bin/{binary_path}/PACKAGES` |
| `GET` | `/latest/src/contrib/PACKAGES` |
| `GET` | `/latest/bin/{binary_path}/PACKAGES` |

`{binary_path}` is wildcard-matched — the server serves whatever binary path R
requests for the current platform (e.g.
`macosx/big-sur-arm64/contrib/4.5` or `windows/contrib/4.5`).
`PACKAGES.gz` variants are also served (gzip-compressed).

### 4.2 Primed session endpoints

#### `POST /prime`

Submits a lockfile; the server classifies packages and creates a scoped session.

**Request body** (renv lockfile JSON):

```json
{
  "R":     { "Version": "4.5.0" },
  "JASP":  { "RepoVersion": "9120" },
  "Packages": {
    "ggplot2":    { "Package": "ggplot2", "Version": "3.5.1", "Source": "Repository" },
    "jaspGraphs": { "Package": "jaspGraphs", "Version": "0.11.2", "Source": "GitHub",
                    "RemoteUsername": "jasp-stats", "RemoteRepo": "jaspGraphs",
                    "RemoteSha": "abc123def456" }
  }
}
```

**Response:**

```json
{
  "session": "a3f8c2d1e9B4",
  "repo_url": "http://localhost:8765/primed/a3f8c2d1e9B4",
  "package_count": 47,
  "binary": 43,
  "source_only": ["jaspGraphs"],
  "not_found": []
}
```

**Classification buckets:**

| Bucket | Criterion | PACKAGES included? |
|--------|-----------|--------------------|
| `binary` | Found in a pinned source (RSPM / jasp-repo) | Yes (merged entry) |
| `source_only` | Not in pinned sources, but lockfile has source info (GitHub SHA / Repository URL) | Yes (external entry + remote metadata) |
| `not_found` | Not in pinned sources AND no source info | No (returned so client can warn) |

#### Scoped PACKAGES

| Method | Path |
|--------|------|
| `GET` | `/primed/{session}/src/contrib/PACKAGES` |
| `GET` | `/primed/{session}/bin/{binary_path}/PACKAGES` |

Contains **every** lockfile package (`binary` + `source_only`), excluding
`not_found`. Each entry carries `Package` + `Version`; source-only entries also
carry remote metadata (`RemoteType`, `RemoteSha`, etc.).

#### Streaming tarball endpoints

| Method | Path | Behavior |
|--------|------|----------|
| `GET` | `/primed/{session}/bin/{binary_path}/{pkg}_{ver}.{tgz,tar.gz,zip}` | Stream binary from upstream; 404 → R falls back to source |
| `GET` | `/primed/{session}/src/contrib/{pkg}_{ver}.tar.gz` | Stream source from upstream |

#### `DELETE /primed/{session}`

Expires a session. `compile()` should call this on exit (via `withr::defer()`)
to avoid session accumulation.

### 4.3 Health

`GET /health` →

```json
{"status":"ok","versions":["9120","8975"],"latest":"9120","sessions":3}
```

---

## 5. Request flow (low-level)

### 5.1 Router

All HTTP requests pass through a single `handle_request()` dispatcher:

```mermaid
flowchart TD
    Req["Incoming request\nmethod + path"] --> Parse["Parse path into segments"]
    Parse --> Health{"/health GET?"}
    Health -- Yes --> H["resp_health()"]
    Health -- No --> Prime{"/prime POST?"}
    Prime -- Yes --> P["handle_prime()"]
    Prime -- No --> Primed{"/primed/... ?"}
    Primed -- Yes --> PS["handle_primed()"]
    Primed -- No --> Versioned{"/{version}/... GET?"}
    Versioned -- Yes --> VP["resp_version_packages()"]
    Versioned -- No --> NF["404 Not Found"]
```

### 5.2 Prime + install sequence

This is the main `compile()` flow — prime a session, then install from it:

```mermaid
sequenceDiagram
    participant R as R / compile()
    participant S as Local server
    participant U as Upstreams (RSPM / jasp-repo / GitHub)

    Note over R,S: 1. Start server (once, at session start)
    R->>S: start_repo_server()
    S->>U: Pre-merge PACKAGES for "latest"
    U-->>S: source + binary indices
    S-->>R: listening on :8765

    Note over R,S: 2. Prime a session from the lockfile
    R->>S: POST /prime { lockfile JSON }
    S->>S: get_merged(version) [cached]
    S->>S: classify packages
    Note right of S: binary: found in merged\nsource_only: has GitHub/Repo info\nnot_found: neither
    S-->>R: { session, repo_url, classification }

    Note over R,S: 3. Install from the primed repo
    R->>S: GET /primed/{sid}/bin/.../PACKAGES
    S-->>R: scoped PACKAGES (all lockfile pkgs)

    loop For each package R installs
        R->>S: GET /primed/{sid}/bin/.../{pkg}_{ver}.tgz
        alt Binary available upstream
            S->>U: fetch binary (RSPM or jasp-repo)
            U-->>S: binary bytes
            S-->>R: stream bytes (no disk)
        else Binary 404 (e.g. source-only pkg)
            S-->>R: 404
            R->>S: GET /primed/{sid}/src/contrib/{pkg}_{ver}.tar.gz
            S->>U: fetch source (RSPM / jasp-repo / GitHub)
            U-->>S: source bytes
            S-->>R: stream bytes
        end
    end

    Note over R,S: 4. Cleanup
    R->>S: DELETE /primed/{sid}
    S-->>R: { status: deleted }
```

### 5.3 Lazy merge + PACKAGES request

The merged index is built lazily on first access for a version, then cached:

```mermaid
flowchart TD
    Req["Request for\n/{version}/.../PACKAGES"] --> Resolve["resolve_version()\n(latest maps to configured key)"]
    Resolve --> Cached{"Merged index in cache?"}
    Cached -- Yes --> Serve["Serialize entries to PACKAGES text"]
    Cached -- No --> Build["Build merged index"]
    Build --> Src["Fetch RSPM source PACKAGES\n/src/contrib/PACKAGES"]
    Build --> Bin["Fetch RSPM binary PACKAGES\n/bin/{binary_path}/PACKAGES\n(404 on Linux means skip)"]
    Build --> Jasp["Scrape jasp-repo directory\nparse filenames to minimal entries"]
    Src --> Merge["Merge: highest version wins\nRSPM beats jasp-repo on tie"]
    Bin --> Merge
    Jasp --> Merge
    Merge --> Cache["Store in merged_cache"]
    Cache --> Serve
```

### 5.4 Binary streaming (cache + conversion)

Binary requests check the cache first. On a miss, RSPM binaries are downloaded
and cached as-is. jasp-repo binaries are downloaded, converted (decompress via
libarchive, wrap in pkgname/, re-archive as gzip tgz/zip), then cached.

```mermaid
flowchart TD
    Req["R requests binary\n{pkg}_{ver}.{ext}"] --> Lookup["Look up pkg in session"]
    Lookup --> Found{"Found?"}
    Found -- No --> NF404["404 Not Found"]
    Found -- Yes --> Cache{"In binary cache?"}
    Cache -- Yes --> ServeCache["Serve from disk cache"]
    Cache -- No --> BOrigin{"Package origin?"}

    BOrigin -- rspm --> BFetch["Download from RSPM\n(already correct format)"]
    BFetch --> BWrite["Write to cache"]

    BOrigin -- "jasp-repo" --> JFetch["Download .tar.gz\n(zstd, no wrapper dir)"]
    JFetch --> Convert["Convert via libarchive:\nextract, wrap in pkgname/\nre-archive as gzip tgz or zip"]
    Convert --> JWrite["Write converted to cache"]

    BOrigin -- external --> B404["404 source-only\nR falls back to source"]
    BWrite --> Serve["Stream to R"]
    JWrite --> Serve
    ServeCache --> Serve
```

### 5.5 Source streaming

Source requests resolve the upstream URL by origin. jasp-repo packages are
**binary-only** — source requests return 404.

```mermaid
flowchart TD
    Req["R requests source\n{pkg}_{ver}.tar.gz"] --> Lookup["Look up pkg in session"]
    Lookup --> Found{"Found?"}
    Found -- No --> NF404["404 Not Found"]
    Found -- Yes --> SOrigin{"Package origin?"}

    SOrigin -- rspm --> SUrl["URL = RSPM/src/contrib/{filename}"]
    SOrigin -- "jasp-repo" --> S404["404 binary-only\nno source available"]
    SOrigin -- external --> SGit{"Has GitHub RemoteSha?"}
    SGit -- Yes --> GitUrl["URL = api.github.com/.../tarball/{sha}\n+ Authorization: Bearer {PAT}"]
    SGit -- No --> RepoUrl["URL = {Repository}/src/contrib/{filename}"]

    SUrl --> Stream["fetch_upstream and stream"]
    GitUrl --> Stream
    RepoUrl --> Stream
```

### 5.6 Binary format conversion (jasp-repo)

jasp-repo binaries need conversion before R can use them:

| | jasp-repo (input) | R expects (output) |
|---|---|---|
| **Compression** | zstd | gzip (macOS/Linux) or zip (Windows) |
| **Extension** | `.tar.gz` | `.tgz` (macOS) / `.zip` (Windows) / `.tar.gz` (Linux) |
| **Structure** | files at root | wrapped in `pkgname/` directory |

The conversion uses `archive::archive_extract()` (libarchive) which auto-detects
compression from the file content — so when jasp-repo renames files to `.tar.zst`
or changes compression later, nothing in the server needs to change.

Re-archiving uses R's built-in `tar(compression = "gzip")` (portable, Windows-safe)
or `utils::zip()` for Windows binaries.

---

## 6. Session lifecycle

```mermaid
stateDiagram-v2
    [*] --> Created: POST /prime
    note right of Created
        Scoped PACKAGES built from:
        - binary (merged index)
        - source_only (external entries)
        Held in session map.
    end note
    Created --> Streaming: GET /primed/{sid}/...
    Streaming --> Streaming: more requests
    Streaming --> Expired: DELETE /primed/{sid}
    Created --> Expired: DELETE /primed/{sid}
    Expired --> [*]
```

Sessions are short-lived: `compile()` creates one, installs from it, then
deletes it. If deletion is skipped (e.g. crash), the session dies with the R
session that started the server.

---

## 7. In-memory data model

### Global state (`.repo_server_state`)

```mermaid
erDiagram
    SERVER_STATE ||--|| CONFIG : holds
    SERVER_STATE ||--|| PLATFORM : holds
    SERVER_STATE ||--o{ MERGED_CACHE : "version to entries"
    SERVER_STATE ||--o{ SESSION : "session map"
    SERVER_STATE {
        github_pat string
        server httpuv_handle
        host string
        port int
        cache_path string
    }
    CONFIG {
        rspm_base_url template
        jasp_repo_url template
        versions map_label_pins
        latest version_key
    }
    PLATFORM {
        os string
        arch string
        r_version string
        binary_path string
        macos_platform string
    }
    MERGED_CACHE ||--|| ENTRY : "per package"
    ENTRY {
        Package string
        Version string
        raw DCF_block_text
        vkey package_version
        origin enum
        priority int
    }
    SESSION ||--|| ENTRY : "scoped subset"
    SESSION {
        id string
        version version_key
        scoped list_of_ENTRY
        classification object
        created timestamp
    }
```

### Entry representation

Each package is stored as a **raw DCF text block** plus parsed metadata. This
preserves every original field (Depends, Imports, ...) for RSPM passthrough,
while jasp-repo entries get minimal `Package` + `Version` blocks:

```
RSPM entry:                          jasp-repo entry:
  Package: ggplot2                     Package: jaspAnova
  Version: 3.5.1                       Version: 0.95.5
  Depends: R (>= 3.3)
  Imports: ...                         (no other fields —
  Suggests: ...                         derived from filename)
  MD5sum: ...
  ...
```

---

## 8. Getting started

### Prerequisites

```r
install.packages(c("httpuv", "curl", "jsonlite", "archive"))
```

`archive` (libarchive binding) is needed for jasp-repo binary conversion.
Without it, jasp-repo binaries cannot be served (RSPM binaries still work).

### Quick start

```r
source("tools/repo_server.R")

# Start with auto-detected platform (binds 127.0.0.1:8765)
start_repo_server()

# Check health
curl::curl_fetch_memory("http://localhost:8765/health")

# Stop
stop_repo_server()
```

### Platform overrides

```r
# Override R version (e.g. to match a jasp-repo snapshot)
start_repo_server(r_version = "4.5")

# Try Linux binaries (RSPM linux path; 404 → source fallback)
start_repo_server(distro = "noble")

# Provide a GitHub PAT for source fallback (or set GITHUB_PAT env var)
start_repo_server(github_pat = "ghp_xxx")

# Enable binary cache (also settable via options(jasp.repo_cache_path))
start_repo_server(cache_path = "~/.jasp/repo_cache")

# Use a local config file for testing
start_repo_server(config_url = "path/to/repos.json")
```

### Config file (`repos.json`)

```jsonc
{
  "rspm":      { "base_url": "https://packagemanager.posit.co/cran/{date}" },
  "jasp-repo": { "url": "https://repo.jasp-stats.org/{version}/{r_version}/{os}/{arch}" },
  "versions": {
    "9120": { "rspm": "2025-06-01", "jasp-repo": "development" },
    "8975": { "rspm": "2025-03-15", "jasp-repo": "0.19.3" }
  },
  "latest": "9120"
}
```

`{date}` / `{version}` / `{r_version}` / `{os}` / `{arch}` are substituted
per version + platform at request time.

---

## 9. Design decisions & tradeoffs

| Decision | Rationale |
|----------|-----------|
| **Optional binary cache** | Caching is opt-in via `cache_path` param or `jasp.repo_cache_path` option. When enabled, all binaries (RSPM + converted jasp-repo) are cached on disk. Without it, binaries are re-fetched every build. |
| **jasp-repo binary conversion** | jasp-repo serves zstd-compressed binaries without a wrapper directory. The server converts them to gzip `.tgz`/`.zip` with `pkgname/` wrapping using libarchive (`archive` package) for extraction and R's `tar()`/`zip()` for re-archiving. Conversion happens once per binary, then the result is cached. |
| **libarchive for extraction** | `archive::archive_extract()` auto-detects compression (zstd, gzip, xz, bzip2) from file content, not filename. When jasp-repo changes compression or extensions later, nothing breaks. |
| **Lazy merge + pre-merge `latest`** | First request to a version triggers the merge (~6s for 22k packages). `latest` is pre-merged at startup so `compile()`'s first request is instant. |
| **Environment as hash map** | R list append-by-name is O(n²). Using `new.env()` for the merged index and session map gives O(1) inserts (reduced merge from 85s → 6s). |
| **Raw DCF blocks** | Keeping the original text excerpt per entry means RSPM fields round-trip exactly (no lossy parse/re-serialize). jasp-repo entries get minimal blocks. |
| **Streaming = fetch-to-memory** | `curl::curl_fetch_memory()` loads the body as a raw vector, which httpuv sends as-is. |
| **Session = R session lifetime** | No TTL, no persistence, no crash recovery. When the R process exits, the server and all sessions die. |
| **r-universe dropped** | The r-universe host is a dynamic Node.js backend, not git-pinnable. Revisit when a snapshot-publishing GitHub Action exists. |

---

## 10. What's deferred (not in Phase 1)

- **Phase 2 — R package refactor:** `start_jasp_development()` /
  `stop_jasp_development()`, `updateLockfile()`, `compile()` R-side wrappers.
- **Phase 3 — macOS:** `fix_mac_linking()` + codesign + bundling (unchanged).
- **r-universe integration:** needs snapshot-publishing infra.
- **Real PACKAGES file on jasp-repo:** would replace directory scraping (future
  cross-team task; would add Depends/Imports fields for dependency resolution).
