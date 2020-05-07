.onAttach <- function(libname, pkgname){
  inst.ver <- utils::packageVersion("actel")
  inst.ver.short <- substr(inst.ver, start = 1, stop = nchar(as.character(inst.ver)) - 5) 
  packageStartupMessage("Welcome to actel (", inst.ver.short, ")!\nRun ?actel for starting tips.")
  rep.ver <- tryCatch(unlist(strsplit(readLines('https://raw.githubusercontent.com/hugomflavio/actel/master/DESCRIPTION')[3], " "))[2], error = function(e) NULL, warning = function(w) NULL)
  if (!is.null(rep.ver)) {
  	rep.ver.short <- substr(rep.ver, start = 1, stop = nchar(rep.ver) - 5)
	  rep.ver.num <- unlist(strsplit(rep.ver.short, ".", fixed = TRUE))
	  inst.ver.num <- unlist(strsplit(inst.ver.short, ".", fixed = TRUE))
	  if (any(rep.ver.short > inst.ver.short))
	    packageStartupMessage(paste0("-------------------------------------------------------------\n!!! A NEW VERSION of actel is available! (v.", inst.ver.short, " -> v.", rep.ver.short, ")\n!!! You should update actel before continuing.\n!!! To learn how to update actel, run updateActel()\n-------------------------------------------------------------\n"))
	}
}

utils::globalVariables(c("example.spatial", "example.biometrics", "example.detections", "example.deployments"))

if (FALSE)
  svglite::svglite()
# This dummy line suppresses the R check note regarding unused packages.
# While actel does not require svglite directly, it is a necessary 
# "suggested" dependency of ggplot2, and I want to ensure that it is installed.
