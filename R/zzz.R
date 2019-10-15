.onAttach <- function(libname, pkgname){
  packageStartupMessage("Welcome to actel! Here are some useful tips:\n  - Find the package manual at browseVignettes('actel')\n  - Create template files with createWorkspace()\n  - Use exampleWorkspace() to see an example!")
  if (.Platform$OS.type == "windows") {
    ipmessage <- system("ipconfig", intern = TRUE)
  } else {
    ipmessage <- system("ifconfig", intern = TRUE)
  }
  validIP <- "((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)[.]){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
	if (any(grep(validIP, ipmessage))) {
	  rep.ver <- tryCatch(unlist(strsplit(readLines('https://raw.githubusercontent.com/hugomflavio/actel/master/DESCRIPTION')[3], " "))[2], error = function(e) NULL, warning = function(w) NULL)
	  if (!is.null(rep.ver)) {
	  	rep.ver.short <- substr(rep.ver, start = 1, stop = nchar(rep.ver) - 5)
		  rep.ver.num <- unlist(strsplit(rep.ver.short, ".", fixed = TRUE))
		  inst.ver <- utils::packageVersion("actel")
		  inst.ver.short <- substr(inst.ver, start = 1, stop = nchar(as.character(inst.ver)) - 5) 
		  inst.ver.num <- unlist(strsplit(inst.ver.short, ".", fixed = TRUE))
		  if (any(rep.ver.short > inst.ver.short))
		    packageStartupMessage(paste0("-------------------------------------------------------------\n!!! A NEW VERSION of actel is available! (v.", inst.ver.short, " -> v.", rep.ver.short, ")\n!!! You should update actel before continuing.\n!!! To learn how to update actel, run updateActel()\n-------------------------------------------------------------\n"))
		}
	}
}