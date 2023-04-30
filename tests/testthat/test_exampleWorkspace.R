tests.home <- getwd()
setwd(tempdir())
test_that("exampleWorkspace deploys necessary files", {
	expect_error(exampleWorkspace(), "Please specify a target directory.", fixed = TRUE)
	exampleWorkspace("actel_workspace", force = TRUE)
	expect_true(dir.exists("actel_workspace"))
	expect_true(file.exists("actel_workspace/spatial.csv"))
	expect_true(file.exists("actel_workspace/biometrics.csv"))
	expect_true(file.exists("actel_workspace/deployments.csv"))
	expect_true(file.exists("actel_workspace/spatial.csv"))
	expect_true(dir.exists("actel_workspace/detections"))

	expect_error(exampleWorkspace("actel_workspace"), 
		"The specified directory already exists! Stopping to avoid accidental data loss. To continue regardless, run again with force = TRUE.", fixed = TRUE)

	expect_warning(exampleWorkspace("actel_workspace", force = TRUE),
		"The specified directory already exists, but force = TRUE. Deploying example files. Data loss may occur.", fixed = TRUE)

	unlink("actel_workspace", recursive = TRUE)
})
setwd(tests.home)
