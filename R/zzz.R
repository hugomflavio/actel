.onAttach <- function(libname, pkgname){
  inst.ver <- utils::packageVersion("actel")
  aux <- as.matrix(data.frame(Package = "actel", LibPath = NA, Version = as.character(utils::packageVersion("actel")), Priority = NA, Built = NA))
  packageStartupMessage("Welcome to actel (", inst.ver, ")!\nRun ?actel for starting tips.")
  new.ver <- tryCatch(old.packages(instPkgs = aux, repos = "https://cloud.r-project.org"), warning = function(w) NULL, error = function(e) NULL)
  if (!is.null(new.ver)) {
    packageStartupMessage(paste0("-------------------------------------------------------------\n!!! A NEW VERSION of actel is available! (v.", inst.ver, " -> v.", new.ver[, "ReposVer"], ")\n!!! You should update actel before continuing.\n!!! You can update your packages by running update.packages()\n-------------------------------------------------------------\n"))
	}
}

utils::globalVariables(c("example.spatial", "example.biometrics", "example.detections", "example.deployments"))

if (FALSE)
  svglite::svglite()
# This dummy line suppresses the R check note regarding unused dependencies.
# While actel does not require svglite directly, it is a necessary
# "suggested" package of ggplot2, and I want to ensure that it is installed.
