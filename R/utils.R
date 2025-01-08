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
  hash = rlang::hash(rlang::hash_file(fs::dir_ls(modulePkg, recurse = TRUE)))
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