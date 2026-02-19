################################# Lock file stuff ##################################################


getRecordsFromPkgdepends <- function(modulePkg) {
  pd <- pkgdepends::new_pkg_deps(paste0('./', modulePkg))
  pd$set_solve_policy(policy = "upgrade")
  pd$solve()
  pd$stop_for_solution_error()
  res <- pd$get_solution()
  dat <- res$data

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
  if(Sys.getenv('FLATPAK_ID') != "")
    os <- 'Flatpak'
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
    if(!getOption('jaspRemoteCellarRedownload', default = FALSE) && fs::file_exists(out))
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
  if(getOS() != 'Linux') {
    gathered <- c(NULL)
    for(repo in repos) {
      if(length(depsNeeded) <= 0) break
      res <- sapply(depsNeeded, download, repo, cellardir)
      gathered <- c(gathered, depsNeeded[res])
      depsNeeded <- depsNeeded[!res]
    }
  }
  stringr::str_replace(depsNeeded, '.tar.gz', '') #return all the pkgs we could not gather
}

expandCellarIntoRenvCache <- function(cellardir) {
  tmpExpandDir <- fs::dir_create(fs::path(cellardir, '..', 'cellarExpand'))
  expandIntoCache <- function(archive) {
    split <- strsplit(fs::path_file(archive), '_')[[1]]
    cache <- renv:::renv_paths_cache(split[1] , sub('.tar.gz', '', split[2]))
    if(!getOption('jaspRemoteCellarRedownload', default = FALSE) && fs::dir_exists(cache)) return(TRUE)

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

### MacOS tragedy

framework_resources <- "@executable_path/../Frameworks/R.framework/Versions/Current/Resources/"
linkPrefixMapToJASP <- c(
  "^/Library/Frameworks/R.framework/Versions/.*/Resources/lib"      = paste0(framework_resources, "lib"),
  "^/usr/local/lib/libjags"                                         = paste0(framework_resources, "opt/jags/lib/libjags"),
  "^/usr/local/lib/libjrmath"                                       = paste0(framework_resources, "opt/jags/lib/libjrmath"),
  "^/usr/local/lib"                                                 = paste0(framework_resources, "opt/local/lib"),
  "^/opt/gfortran/lib/gcc/x86_64-apple-darwin20.0/14.2.0"           = paste0(framework_resources, "opt/R/x86_64/gfortran/lib"),
  "^/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0"          = paste0(framework_resources, "opt/R/arm64/gfortran/lib"),
  "^/opt/X11/lib/"                                                  = paste0(framework_resources, "opt/X11/lib")
)
linkPrefixMapToJASP <- linkPrefixMapToJASP[order(nchar(names(linkPrefixMapToJASP)), decreasing = TRUE)]

generate_link_fix_command <- function(lib, map) {
  otool <- system2("otool", c("-L", lib), stdout=TRUE, stderr=TRUE)[-1]
  links <- sapply(otool, function(z) {substring(z,2)})
  links <- sub(" \\(compatibility.*", "", links)

  subPrefixes <- function(link) {
    change <- ""
    for(prefix in names(map)) {
      newLink <- sub(prefix, map[prefix], link)
      if(newLink != link) {
        change <- paste0("-change ", link, " ", sub(prefix, map[prefix], link))
        break
      }
    }
    change
  }
  changes <- sapply(links, subPrefixes)
  if(any(changes != ""))
    paste("install_name_tool", paste(changes, collapse = " "), lib)
  else
    ""
}

generate_codesign_adhoc_command <- function(path) {
  paste0("codesign --force --deep --verbose=4 --timestamp --sign - \"", path, "\"");
}

fix_mac_linking <- function(dir, filter = function(x) {TRUE}) {
  fix_linking <- function(lib) {
    linkFixCommand <- generate_link_fix_command(lib, linkPrefixMapToJASP)
    if(linkFixCommand != "") {
      print(linkFixCommand)
      system(linkFixCommand)
    }
    system(generate_codesign_adhoc_command(lib))
  }

  libs <- c(fs::dir_ls(dir, recurse = TRUE, type="file", glob  = "*.so" ), fs::dir_ls(dir, recurse = TRUE, type="file", glob = "*.dylib" ))
  libs <- Filter(function(x) !grepl(".dSYM",x), libs)
  libs <- Filter(filter, libs)
  sapply(libs, fix_linking)
  TRUE
}

##################################################################################################

#unused for now
super_copy <- function(source_dirs, dest_dir) {
  dest_dir <- normalizePath(dest_dir, winslash = "\\", mustWork = FALSE)
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

  is_windows <- .Platform$OS.type == "windows"
  if (is_windows) {
    if (Sys.which("robocopy") == "") stop("robocopy not found.")
    for (src in source_dirs) {
      src_abs <- normalizePath(src, winslash = "\\", mustWork = TRUE)
      cmd <- sprintf('robocopy %s %s /E /MT:16 /R:0 /W:0 /NFL /NDL /COPY:DT /A-:RHS', shQuote(src_abs), shQuote(dest_dir))
      suppressWarnings(system(cmd, show.output.on.console = FALSE))
    }
  }
  else {
    if (Sys.which("rsync") == "") stop("rsync not found. Please install it")
    src_abs <- normalizePath(source_dirs, mustWork = TRUE)
    src_formatted <- paste0(src_abs, ifelse(endsWith(src_abs, "/"), "", "/"))
    src_string <- paste(shQuote(src_formatted), collapse = " ")
    print(paste0("Copying '", src_string, "' to '", dest_dir, "'"))
    cmd <- sprintf("rsync -rltL --chmod=Du+rwx %s %s", src_string, shQuote(dest_dir))
    system(cmd)
  }
}


