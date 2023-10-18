.onAttach <- function(libname, pkgname){
  inst.ver <- utils::packageVersion("actel")
  aux <- as.matrix(data.frame(Package = "actel", LibPath = NA, Version = as.character(utils::packageVersion("actel")), Priority = NA, Built = NA))
  packageStartupMessage("Welcome to actel (", inst.ver, ")!\nRun ?actel for starting tips.")
  new.ver <- tryCatch(old.packages(instPkgs = aux, repos = "https://cloud.r-project.org"), warning = function(w) NULL, error = function(e) NULL)
  if (!is.null(new.ver)) { # nocov start
    packageStartupMessage(paste0("-------------------------------------------------------------\n!!! A NEW VERSION of actel is available! (v.", inst.ver, " -> v.", new.ver[, "ReposVer"], ")\n!!! You should update actel before continuing.\n!!! You can update your packages by running update.packages()\n-------------------------------------------------------------\n"))
	} # nocov end

	# temporary warning message
	ctime <- file.info(find.package(pkgname, libname))$ctime
	if (difftime(Sys.time(), ctime, units = "day") <= 7)
	  packageStartupMessage("---------------------------------------------------------------\n!!! IMPORTANT NOTE:\n!!!\n!!! A silent bug was found in how migration() handles parallel\n!!! sections. This bug is still present, but it does not affect\n!!! all datasets. It can impact the number of animals reported\n!!! to have passed by a given section.\n!!! \n!!! Until the issue is resolved, actel will now stop if it \n!!! detects parallel sections. If you are unsure if your study\n!!! area has parallel sections, try running the analyses again\n!!! with version 1.3.0.\n!!!\n!!! You can read more about this issue here:\n!!! https://hugomflavio.github.io/actel-website/issue_79.html\n!!!\n!!! (This message will stop being displayed in ", round(7 - difftime(Sys.time(), ctime, units = "day"), 0), " days)\n---------------------------------------------------------------\n")
}

utils::globalVariables(c("example.spatial", "example.biometrics", "example.detections", "example.deployments"))

if (FALSE)
  svglite::svglite()
# This dummy line suppresses the R check note regarding unused dependencies.
# While actel does not require svglite directly, it is a necessary
# "suggested" package of ggplot2, and I want to ensure that it is installed.
