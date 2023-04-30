.onAttach <- function(libname, pkgname){
  inst.ver <- utils::packageVersion("actel")
  aux <- as.matrix(data.frame(Package = "actel", LibPath = NA, Version = as.character(utils::packageVersion("actel")), Priority = NA, Built = NA))
  packageStartupMessage("Welcome to actel (", inst.ver, ")!\nRun ?actel for starting tips.")
  new.ver <- tryCatch(old.packages(instPkgs = aux, repos = "https://cloud.r-project.org"), warning = function(w) NULL, error = function(e) NULL)
  if (!is.null(new.ver)) { # nocov start
    packageStartupMessage(paste0("-------------------------------------------------------------\n!!! A NEW VERSION of actel is available! (v.", inst.ver, " -> v.", new.ver[, "ReposVer"], ")\n!!! You should update actel before continuing.\n!!! You can update your packages by running update.packages()\n-------------------------------------------------------------\n"))
	} # nocov end

	# # temporary warning message
	# ctime <- file.info(find.package(pkgname, libname))$ctime
	# if (difftime(Sys.time(), ctime, units = "day") <= 7)
	#   packageStartupMessage("---------------------------------------------------------------\n!!! IMPORTANT NOTE:\n!!!\n!!! A silent bug was found and fixed in migration() efficiency \n!!! calculations in this release. This bug did not affect all \n!!! datasets, and only impacted the efficiency calculations. \n!!! \n!!! If you have used migration() in actel < 1.2.0, I recommend \n!!! that you run the analyses again and compare the results, \n!!! to ensure these are correct. I am sorry if this caused any\n!!! inconvenience to you.\n!!!\n!!! (This message will stop being displayed in ", round(7 - difftime(Sys.time(), ctime, units = "day"), 0), " days)\n---------------------------------------------------------------\n")
}

utils::globalVariables(c("example.spatial", "example.biometrics", "example.detections", "example.deployments"))

if (FALSE)
  svglite::svglite()
# This dummy line suppresses the R check note regarding unused dependencies.
# While actel does not require svglite directly, it is a necessary
# "suggested" package of ggplot2, and I want to ensure that it is installed.
