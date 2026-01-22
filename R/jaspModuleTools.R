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
compile <- function(moduledir, workdir, resultdir='./', createBundle=TRUE, bundleAll=TRUE, buildforJaspVersion='development', useJASPRemoteCellar=TRUE, repoName='development', localCellar='', localizeJASPModules = 'localizeModuleOnly', includeInManifest = c()) {
  if(missing(workdir)) {
    workdir <- fs::dir_create(tempdir(), fs::path_file(moduledir))
    withr::defer(if(fs::dir_exists(workdir))fs::dir_delete(workdir))
  }
  workdir <- fs::path_abs(fs::dir_create(workdir))
  resultdir <- fs::path_abs(fs::dir_create(resultdir))
  pkglib <- fs::dir_create(workdir, fs::path_file(moduledir))
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

  #copy over the missing pkgs from sandbox so we truly have all that is required in one place
  allRequired <- names(renv::lockfile_read(file=lockfile)$Packages)
  presentInlib <- fs::path_file(fs::dir_ls(pkglib))
  missing <- fs::path(renv:::renv_paths_sandbox(), allRequired[!allRequired %in% presentInlib]) 
  fs::dir_copy(missing, fs::path(pkglib, fs::path_file(missing)), overwrite = FALSE)

  renv:::renv_sandbox_deactivate()

  if(Sys.info()["sysname"] == "Darwin") {
    fix_mac_linking(pkglib)
  }

  if(createBundle) jaspModuleBundleManager::createJaspModuleBundle(pkglib, resultdir, bundleAll, mustPackage=notGathered, includeInManifest=c(includeInManifest, jaspVersion=buildforJaspVersion), repoNames=c(repoName))
  #unlink(pkglib, recursive = TRUE)
}


#' Updates lockfile using pkgdepends
#'
#' @description Updates the lockfile of a given jaspModule using pkgdepens
#' @param moduledir Path to the jaspModule root folder
#' @usage jaspModuleTools::updateLockfile('~/jaspTTest/')
#' @export
updateLockfile <- function(moduledir, jaspModuleDependenciesOnly = FALSE) {
  #get current records and extract the locked record
  lockfilePath <- fs::path(moduledir, 'renv.lock')
  currentRecords <- NULL
  if(fs::file_exists(lockfilePath))
      currentRecords = renv::lockfile_read(lockfilePath)

  getLockedRecords <- function(pkg) {
    !is.null(pkg$JASP_LOCK)
  }
  lockedRecords <- Filter(getLockedRecords, currentRecords$Packages)
  lockedNames <- lapply(lockedRecords, function(x) { x$Package })

  #gather new ones using pkgdepends magic and filter out those who conflict with locked ones
  newRecords <- getRecordsFromPkgdepends(moduledir)
  noConflicts <- function(pkg) {
    !(pkg$Package %in% lockedNames)
  }
  nonConflicting <- Filter(noConflicts, newRecords)
  processedRecords <- c(lockedRecords, nonConflicting)

  if(jaspModuleDependenciesOnly) {
    isJASPRecord <- function(pkg) { grepl("jasp", pkg$Package) }
    notJASPRecord <- function(pkg) { !isJASPRecord(pkg) }
    newjaspRecords <- Filter(isJASPRecord, newRecords)
    print(newjaspRecords)
    oldRecords <- Filter(notJASPRecord, currentRecords$Packages)
    processedRecords <- c(oldRecords, newjaspRecords)
  }

  #write the new records
  if(fs::file_exists(lockfilePath))
    fs::file_delete(lockfilePath)
  lockfile <- renv:::renv_lockfile_init(NULL)
  lockfile <- renv::record(processedRecords, lockfile = lockfile)
  renv::lockfile_write(lockfile, lockfilePath)
  sprintf('renv lockfile written for: %s', moduledir)
}







