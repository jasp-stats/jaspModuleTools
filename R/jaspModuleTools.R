#' Compiles given jaspModule and (optionally) creates a JaspModuleBundle
#' Sets up recommended jasp development environment
#'
#' @description Sets up recommended jasp development environment
#' @export
start_jasp_development <- function() {
  usethis::use_build_ignore("jasp_dev_work_dir")
  workdir <- fs::dir_create(fs::path("./jasp_dev_work_dir"))

  #set everything relative to the working directory no surprises
  Sys.setenv(RENV_PATHS_ROOT     = fs::dir_create(workdir, 'renv-root'))
  Sys.setenv(RENV_PATHS_SANDBOX  = fs::dir_create(workdir, 'renv_sandbox'))
  Sys.setenv(RENV_PATHS_CACHE    = fs::dir_create(workdir, 'renv-cache'))
  #Sys.setenv(RENV_PATHS_LIBRARY    = fs::dir_create(workdir, 'pkg_library'))
  library("renv")

  sandboxPaths <- renv:::renv_sandbox_activate()
  renv::sandbox$unlock()

  # Start the local repo server so all install.packages() calls (including
  # RStudio's Install button) resolve against the merged RSPM+jasp-repo snapshot.
  ensure_repo_server(version = "latest")
  options(repos = c(CRAN = paste0(getOption("jasp.local_repo"), "/latest")))
  message("[jaspModuleTools] repos set to ", getOption("repos")[["CRAN"]])
}


#' Collects pkgs in one place and processes them so jasp can load it
#'
#'
#' @description Collects pkgs in one place and processes them so jasp can load it
#' @export
prepare_for_jasp_loading <- function() {
  renv_active <- !is.null(renv::project())
  pkg_lib <- fs::path_expand(fs::path("~/jasp_load_dir/", fs::path_file(fs::path_abs("./"))))
  if(!renv_active) unlink(pkg_lib, recursive = TRUE)
  fs::dir_create(pkg_lib)

  #read old timestamps
  dir_dates_old <- c()
  if(fs::file_exists(fs::path(pkg_lib, "READY"))) {
    tmp <- tryCatch({ readRDS(fs::path(pkg_lib, "READY"))}, error = function(e) {return(NULL)})
    if(!is.null(tmp)) dir_dates_old <- tmp
  }

  #gather new timestamps
  dir_dates_new <- c()
  for(lib in .libPaths()) {
    all_info <- fs::dir_info(path = lib)
    dir_dates_new <- c(dir_dates_new, setNames(all_info$modification_time, all_info$path))
  }
  added_paths <- setdiff(names(dir_dates_new), names(dir_dates_old))
  common_paths <- intersect(names(dir_dates_new), names(dir_dates_old))
  modified_paths <- common_paths[dir_dates_new[common_paths] != dir_dates_old[common_paths]]
  target_paths <- c(added_paths, modified_paths)

  copy_target_paths <- fs::path(pkg_lib, rev(basename(target_paths)))
  fs::dir_copy(rev(target_paths), copy_target_paths, overwrite = TRUE)

  if(Sys.info()["sysname"] == "Darwin") {
    filter <- function(lib) {
      any(fs::path_has_parent(lib, copy_target_paths))
    }
    fix_mac_linking(pkg_lib, filter)
  }
  saveRDS(dir_dates_new, file=fs::path(pkg_lib, "READY"))
  cat("Please set the following file Path in JASP: \n",  pkg_lib)
}

#stop_jasp_development <- function() {
#  renv:::renv_sandbox_deactivate()
#  TRUE
#}

#' Compiles given jaspModule and (optionally) creates a JaspModuleBundle
#'
#' @description Compiles given jaspModule and (optionally) creates a JaspModuleBundle. Will gather binaries from the JASP Remote Cellar Repo.
#' @param moduledir Path to the jaspModule root folder
#' @param workdir Path where the renv cache and pkglib will be created
#' @param resultdir Path to dir where the resulting bundle will be created
#' @param createBundle Should a JASP bundle be created
#' @param bundleAll Should all dependencies be bundled? by default we bundle only those not present in the Remote Cellar Repo
#' @param buildforJaspVersion Specifies the jaspVersion for which this module should be compiled and bundeld
#' @param useJASPRemoteCellar should we use the Remote Cellar?
#' @param repoName which remote repo should be used? for expample 0.19.3 or development
#' @param localCellar local cellar path to also include for development purposes
#' @export
compile <- function(moduledir, workdir, resultdir='./', createBundle=TRUE, bundleAll=TRUE, buildforJaspVersion='development', useJASPRemoteCellar=TRUE, repoName='development', localCellar='', localizeJASPModules = 'localizeModuleOnly', includeInManifest = c(), deleteLibrary= FALSE) {
  if(missing(workdir)) {
    workdir <- fs::dir_create(tempdir(), fs::path_file(moduledir))
    withr::defer(if(fs::dir_exists(workdir))fs::dir_delete(workdir))
  }
  workdir <- fs::path_abs(fs::dir_create(workdir))
  resultdir <- fs::path_abs(fs::dir_create(resultdir))
  pkglib <- fs::dir_create(workdir, fs::path_file(fs::path_abs(moduledir)))

  # Ensure the repo server is running so cellar/install can resolve deps.
  ensure_repo_server(version = repoName)

  lockfile <- processLockFile(file.path(moduledir, 'renv.lock'), moduledir, localizeJASPModules)

  #set everything relative to the working directory no surprises
  Sys.setenv(RENV_PATHS_ROOT     = fs::dir_create(workdir, 'renv-root'))
  Sys.setenv(RENV_PATHS_SANDBOX  = fs::dir_create(workdir, 'renv_sandbox'))
  Sys.setenv(RENV_PATHS_CACHE    = fs::dir_create(workdir, 'renv-cache'))
  Sys.setenv(RENV_SANDBOX_LOCKING_ENABLED = FALSE)

  #handle cellar
  cellardir <- fs::dir_create(workdir, 'cellar')
  notGathered <- c()
  #Sys.setenv(RENV_PATHS_CELLAR = cellardir) # for now we sadly expand manually because renv is vey inconsistent when using binary in cellar
  if(useJASPRemoteCellar) notGathered <- gatherRemoteCellar(lockfile, cellardir, repoName=repoName)
  if(fs::dir_exists(fs::path(localCellar))) fs::dir_copy(localCellar, cellardir, overwrite = TRUE)

  options(
    "install.opts"                = "--no-multiarch --no-docs --no-test-load", # no test-load because on mac the paths need to be fixed
    "renv.config.install.verbose" = TRUE,
    "renv.index.enabled"          = FALSE,
    "renv.config.install.transactional" = FALSE
  )

  sandboxPaths <- renv:::renv_sandbox_activate()
  renv::sandbox$unlock()
  withr::defer(renv:::renv_sandbox_deactivate())

  #expand cellar into renv_cache manually for now. RenV is weird and this way it makes less decisions
  expandCellarIntoRenvCache(cellardir)

  records <- renv::restore(
    library  = pkglib,
    lockfile = lockfile,
    clean    = TRUE,
    prompt   = FALSE
  )
  install.packages(moduledir, repos = NULL, type = "source", lib = pkglib)


  #copy over the missing pkgs from sandbox so we truly have all that is required in one place
  allRequired <- names(renv::lockfile_read(file=lockfile)$Packages)
  presentInlib <- fs::path_file(fs::dir_ls(pkglib))
  missing <- fs::path(renv:::renv_paths_sandbox(), allRequired[!allRequired %in% presentInlib])
  fs::dir_copy(missing, fs::path(pkglib, fs::path_file(missing)), overwrite = FALSE)

  renv:::renv_sandbox_deactivate()

  if(Sys.info()["sysname"] == "Darwin") {
    fix_mac_linking(pkglib)
  }

  includeInManifest=c(includeInManifest, jaspVersion=buildforJaspVersion)
  print(includeInManifest)
  if(createBundle) jaspModuleBundleManager::createJaspModuleBundle(pkglib, resultdir, bundleAll, mustPackage=notGathered, includeInManifest, repoNames=c(repoName))
  if(deleteLibrary) unlink(pkglib, recursive = TRUE)
}

#' Updates lockfile using the local repo server
#'
#' @description Resolves all module dependencies against the JASP local repo
#'   server (which merges version-pinned RSPM + jasp-repo), writes a lockfile
#'   with JASP.RepoVersion baked in so subsequent compiles use the same snapshot.
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
  newRecords <- getRecordsFromPkgdepends(moduledir, repos = repo_base)

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
    RepoVersion = getOption("jasp.local_repo_version", version)
  )

  renv::lockfile_write(lockfile, lockfilePath)
  sprintf("renv lockfile written for: %s", moduledir)
}









