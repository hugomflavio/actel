.onAttach <- function(libname, pkgname){
  packageStartupMessage("Welcome to actel! Here are some useful tips:\n  - Find the package manual at browseVignettes('actel')\n  - Create template files with createWorkspace()\n  - Use exampleWorkspace() to see an example!")
	if (havingIP()) 
		versionCheck()
}