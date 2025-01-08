

#' @export
compile <- function(moduledir, workdir='', resultdir='./', createBundle=TRUE, bundleAll=TRUE, useJASPRemotePkgCache=FALSE, localCellar='', localizeJASPModules) {
  if(workdir == '') { 
    workdir <- fs::dir_create(tempdir(), fs::path_file(moduledir))
    withr::defer(fs::dir_delete(workdir))
  }
  workdir <- fs::path_abs(fs::dir_create(workdir))
  pkglib <- fs::dir_create(workdir, fs::path_file(moduledir))

  #set everything relative to the working directory no suprises
  Sys.setenv(RENV_PATHS_ROOT     = fs::dir_create(workdir, 'renv-root'))
  Sys.setenv(RENV_PATHS_SANDBOX  = fs::dir_create(workdir, 'renv_sandbox'))
  Sys.setenv(RENV_PATHS_CACHE    = fs::dir_create(workdir, 'renv-cache'))
  Sys.setenv(RENV_PATHS_PREFIX    = fs::dir_create(workdir, ''))
  Sys.setenv(RENV_SANDBOX_LOCKING_ENABLED = FALSE)

  #handle cellar
  cellardir <- fs::dir_create(workdir, 'cellar')
  Sys.setenv(RENV_PATHS_CELLAR = cellardir)
  if(fs::dir_exists(fs::path(localCellar))) fs::dir_copy(localCellar, cellardir, overwrite = TRUE)
  if(useJASPRemotePkgCache) gatherRemotePkgCache(lockfile, cellardir)

  options(
    "install.opts"                = "--no-multiarch --no-docs --no-test-load", # no test-load because on mac the paths need to be fixed
    "renv.config.install.verbose" = TRUE,  
    "renv.index.enabled"          = FALSE,
    "renv.config.install.transactional" = FALSE
  )
  
  cat('Restoring from lockfile\n')
  lockfile <- processLockFile(file.path(moduledir, 'renv.lock'), moduledir, 'localizeModuleOnly')


  sandboxPaths <- renv:::renv_sandbox_activate()
  renv::sandbox$unlock()
  withr::defer(renv:::renv_sandbox_deactivate())

  records <- renv::restore(
    library  = pkglib,
    lockfile = lockfile,
    clean    = TRUE,
    prompt   = FALSE
  )
  renv:::renv_sandbox_deactivate()

  if(createBundle) jaspModuleBundleManager::createJaspModuleBundle(pkglib, resultdir, bundleAll)
}

#' @export 
updateLockfile <- function(modulePkg) {
  #get current records and extract the locked record
  lockfilePath <- fs::path(modulePkg, 'renv.lock')
  currentRecords <- NULL
  if(fs::file_exists(lockfilePath))
      currentRecords = renv::lockfile_read(lockfilePath) 
  
  getLockedRecords <- function(pkg) {
    !is.null(pkg$JASP_LOCK)
  }
  lockedRecords <- Filter(getLockedRecords, currentRecords$Packages)
  lockedNames <- lapply(lockedRecords, function(x) { x$Package })

  #gather new ones using pkgdepends magic and filter out those who conflict with locked ones
  newRecords <- getRecordsFromPkgdepends(modulePkg)
  noConflicts <- function(pkg) {
    !(pkg$Package %in% lockedNames)
  }
  nonConflicting <- Filter(noConflicts, newRecords)
  processedRecords <- c(lockedRecords, nonConflicting)

  #write the new records
  fs::file_delete(lockfilePath)
  lockfile <- renv:::renv_lockfile_init(NULL)
  lockfile <- renv::record(processedRecords, lockfile = lockfile)
  renv::lockfile_write(lockfile, lockfilePath)
  sprintf('renv lockfile written for: %s', modulePkg)
}


