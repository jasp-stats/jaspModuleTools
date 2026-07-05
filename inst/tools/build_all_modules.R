# Build all JASP modules and log output
# Usage: source("inst/tools/build_all_modules.R")

options(jasp.repo_cache_path = "./jasp_server_cache")
start_jasp_development()

regdir <- "../modules-registry/Official"
modules    <- list.dirs(regdir, recursive = FALSE)
logfile    <- paste0("build_all_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log")
results    <- list()

for (i in seq_along(modules)) {
  mod  <- modules[i]
  name <- basename(mod)

  cat(sprintf("\n========== Building: %s (%d/%d) ==========\n",
              name, i, length(modules)),
      file = logfile, append = i > 1L)

  sink(logfile, append = TRUE, split = TRUE)
  results[[name]] <- tryCatch(
    jaspModuleTools::build(mod, update_lockfile = TRUE, library = paste0("./build_lib_", name)),
    error = function(e) {
      cat("FAILED:", conditionMessage(e), "\n")
      e
    }
  )
  sink()
}

# Summary
failed <- vapply(results, inherits, logical(1L), "error")
cat(
  "\n========================================\n",
  "SUMMARY\n",
  "Total:  ", length(results), "\n",
  "Passed: ", sum(!failed), "\n",
  "Failed: ", sum(failed), "\n",
  if (any(failed)) paste0("\nFailed:\n", paste(names(results)[failed], collapse = "\n"), "\n"),
  file = logfile, append = TRUE
)

cat("Log:", normalizePath(logfile), "\n")
if (any(failed)) cat("FAILED:", paste(names(results)[failed], collapse = ", "), "\n")
