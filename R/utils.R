################################# Lock file stuff ##################################################


getRecordsFromPkgdepends <- function(modulePkg) {
  pd <- pkgdepends::new_pkg_download_proposal(modulePkg)
  pd$resolve()
  dat <- pd$get_resolution()

  # this could be expanded but we currently do not support other repos anyway...
  fromRepository <- which(dat$type == "standard")
  fromGitHub     <- which(dat$type == "github")
  fromInstalled  <- which(dat$type == "installed")

  recordsFromRepository <- setNames(lapply(fromRepository, function(i) {
    required <- dat$deps[[i]]$type != "enhances" & dat$deps[[i]]$type != "suggests"
    list(Package = dat$package[i], Version = dat$version[i], Source = "Repository", Requirements = unique(dat$deps[[i]]$package[required]))
  }), dat$package[fromRepository])

  recordsFromInstalled <- setNames(lapply(fromInstalled, function(i) {
    required <- dat$deps[[i]]$type != "enhances" & dat$deps[[i]]$type != "suggests"
    list(Package = dat$package[i], Version = dat$version[i], Source = "Repository", Requirements = unique(dat$deps[[i]]$package[required]))
  }), dat$package[fromInstalled])

  recordsFromGithub <- setNames(lapply(fromGitHub, function(i) {
    required <- dat$deps[[i]]$type != "enhances" & dat$deps[[i]]$type != "suggests"
    list(
      Package        = dat$package[i],
      Version        = dat$version[i],
      Source         = "GitHub",
      Requirements   = unique(dat$deps[[i]]$package[required]),
      RemoteType     = dat$metadata[[i]][["RemoteType"]],
      RemoteHost     = dat$metadata[[i]][["RemoteHost"]],
      RemoteUsername = dat$metadata[[i]][["RemoteUsername"]],
      RemoteRepo     = dat$metadata[[i]][["RemoteRepo"]],
      RemoteSha      = dat$metadata[[i]][["RemoteSha"]]
    )
  }), dat$package[fromGitHub])

  combinedRecords <- c(recordsFromInstalled, recordsFromGithub, recordsFromRepository)
}


processLockFile <- function(lockfile, pathToModule, installMode = "identicalToLockfile") {
  processedLockFile <- file.path(pathToModule, "_processedLockFile.lock")
  file.copy(lockfile, processedLockFile, overwrite=TRUE)

  if (installMode == "localizeAll") { #only use local JASP modules from the install folder where possible
    # records <- createRecordsOfLocalJaspModules(pathToModule)
    processedLockFile <- renv::record(records = records, lockfile = processedLockFile)
  } else if(installMode == "localizeModuleOnly") { #fetch everything from remote except the packages containing the lockfile
    localizedRecord <- createLocalRecord(pathToModule, getModuleInfo(pathToModule))
    processedLockFile <- renv::record(records = localizedRecord, lockfile = processedLockFile)
  }
  # else "identicalToLockfile" head empty and only build dependencies

  return(processedLockFile)
}

createLocalRecord <- function(modulePkg, moduleInfo, cacheAble = TRUE, addJaspToVersion = TRUE) {
  hash = rlang::hash(rlang::hash_file(fs::dir_ls(modulePkg, type='file', recurse = TRUE)))
  record <- list(list(
    Package    = moduleInfo[["Package"]],
    Version    = if (addJaspToVersion) addLocalJaspToVersion(moduleInfo[["Version"]]) else moduleInfo[["Version"]],
    Source     = "Local",
    RemoteType = "local",
    RemoteUrl  = modulePkg,
    Cacheable  = cacheAble,
    Hash       = unname(hash) # unname to remove the name attribute, otherwise the json becomes Hash: {<name> : <hash>} instead of Hash: <hash>
  ))
  names(record) <- moduleInfo[["Package"]]
  record
}

getModuleInfo <- function(modulePkg) {
  return(read.dcf(fs::path(modulePkg, "DESCRIPTION"))[1, ])
}

addLocalJaspToVersion <- function(version) {
  suffix <- "_Local_JASP"
  if (!endsWith(x = version, suffix = suffix))
    return(paste0(version, suffix))
  return(version)
}

##################################################################################################



################################## Cellar Stuff ##################################################

getOS <- function() {
  os <- Sys.info()[['sysname']]
  if(os == 'Darwin')
    os <- 'MacOS'
  return(os)
}

getRemoteCellarURLs <- function(baseURLs, repoName) {
  RVersion <- paste0('R-', paste(R.Version()$major, substring(R.Version()$minor, 1, 1), sep = '.'))
  os <- getOS()
  arch <- Sys.info()['machine']
  createURL <- function(url) {
    paste(url, repoName, RVersion, os, arch, sep='/')
  }
  sapply(baseURLs, createURL)

}

gatherRemoteCellar <- function(lockfilePath, cellardir, repoName = 'development', additionalRepoURLs = NULL) {
  download <- function(file, repoURL, targetDir) {
    out <- fs::path(targetDir, file)
    if(!getOption('jaspRemoteCellarRedownload', default = TRUE) && fs::file_exists(out))
       return(TRUE)
    req <- tryCatch({
      curl::curl_fetch_disk(paste0(repoURL, '/', file), out)
    }, error = function(e) { list(status_code=404) })
    if(req$status_code != 200) {
      fs::file_delete(out)
      return(FALSE)
    }
    TRUE
  }

  #determine remote cellar urls
  repos <- getRemoteCellarURLs(c('https://repo.jasp-stats.org/', additionalRepoURLs), repoName)
  
  #read lockfile, extract pkg strings
  depRecords <- renv::lockfile_read(lockfilePath)$Packages
  createDepString <- function(x) {
    version <- if(is.null(x$RemoteSha)) x$Version else substr(x$RemoteSha, 1, 8)
    paste0(x$Package, '_', version, '.tar.gz')
  }
  deps <- lapply(depRecords, createDepString)

  depsNeeded <- deps
  gathered <- c(NULL)
  for(repo in repos) {
    if(length(depsNeeded) <= 0) break
    res <- sapply(depsNeeded, download, repo, cellardir)
    gathered <- c(gathered, depsNeeded[res])
    depsNeeded <- depsNeeded[!res]
  }
  stringr::str_replace(depsNeeded, '.tar.gz', '') #return all the pkgs we could not gather
}

expandCellarIntoRenvCache <- function(cellardir) {
  tmpExpandDir <- fs::dir_create(fs::path(cellardir, '..', 'cellarExpand'))
  expandIntoCache <- function(archive) {
    tmp <- fs::dir_create(tmpExpandDir, fs::path_file(archive))
    untar(archive, tar='internal', exdir=tmp)
    if(fs::file_exists(fs::path(tmp, 'DESCRIPTION'))) { #L0 cellar archive
      cachePath <- renv:::renv_cache_path(fs::path(tmp, 'DESCRIPTION'))
      fs::dir_copy(tmp, cachePath, overwrite = TRUE)
    }
    else { #CRAN archive
      subdir <- fs::dir_ls(tmp)[[1]]
      if(fs::file_exists(fs::path(subdir, 'DESCRIPTION'))) {
        cachePath <- renv:::renv_cache_path(fs::path(subdir, 'DESCRIPTION'))
        fs::dir_copy(subdir, cachePath, overwrite = TRUE)
      }
    }
  }
  archives <- fs::dir_ls(cellardir)
  sapply(archives, expandIntoCache)
  fs::dir_delete(tmpExpandDir)
}

##################################################################################################
