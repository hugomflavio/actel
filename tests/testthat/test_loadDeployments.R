skip_on_cran()
oldtz <- Sys.getenv('TZ', unset = NA)
Sys.setenv(TZ = 'UTC')

tests.home <- getwd()
setwd(tempdir())

test_that("loadDeployments stops if file is missing", {
	expect_error(loadDeployments(input = "test"),
		"Could not find a 'test' file in the working directory.", fixed = TRUE)
})

test_that("loadDeployments stops if columns are missing or duplicated", {
	dep <- example.deployments
	colnames(dep)[1:2] <- "test"
	write.csv(dep, "deployments.csv", row.names = FALSE)
	expect_error(loadDeployments(input = "deployments.csv", tz = "Europe/Copenhagen"),
		"The following columns are duplicated in the deployments: 'test'.", fixed = TRUE)

	dep <- example.deployments
	colnames(dep)[1] <- "test"
	write.csv(dep, "deployments.csv", row.names = FALSE)
	expect_error(loadDeployments(input = "deployments.csv", tz = "Europe/Copenhagen"),
		"Column 'Receiver' is missing in the deployments.", fixed = TRUE)
	file.remove("deployments.csv")
})

test_that("loadDeployments stops if data is missing or badly formatted", {
	dep <- example.deployments
	colnames(dep)[2] <- "Station.Name"
	write.csv(dep, "deployments.csv", row.names = FALSE)
	expect_equal(colnames(loadDeployments(input = "deployments.csv", tz = "Europe/Copenhagen"))[2],"Station.name", fixed = TRUE)

	dep <- example.deployments
	dep$Start[1] <- NA
	write.csv(dep, "deployments.csv", row.names = FALSE)
	expect_error(loadDeployments(input = "deployments.csv", tz = "Europe/Copenhagen"),
		"Not all values in the 'Start' column appear to be in a 'yyyy-mm-dd hh:mm' format (seconds are optional). Please double-check the deployments.", fixed = TRUE)

	dep <- example.deployments
	dep$Stop[1] <- NA
	write.csv(dep, "deployments.csv", row.names = FALSE)
	expect_error(loadDeployments(input = "deployments.csv", tz = "Europe/Copenhagen"),
		"Not all values in the 'Stop' column appear to be in a 'yyyy-mm-dd hh:mm' format (seconds are optional). Please double-check the deployments.", fixed = TRUE)

	dep <- example.deployments
	dep$Start <- as.character(dep$Start)
	dep$Start[1] <- "2999-19-39 29:59:00"
	write.csv(dep, "deployments.csv", row.names = FALSE)
	expect_error(loadDeployments(input = "deployments.csv", tz = "Europe/Copenhagen"),
		"Could not recognise the data in the 'Start' column as POSIX-compatible timestamps. Please double-check the deployments.", fixed = TRUE)

	dep <- example.deployments
	dep$Stop <- as.character(dep$Stop)
	dep$Stop[1] <- "2999-19-39 29:59:00"
	write.csv(dep, "deployments.csv", row.names = FALSE)
	expect_error(loadDeployments(input = "deployments.csv", tz = "Europe/Copenhagen"),
		"Could not recognise the data in the 'Stop' column as POSIX-compatible timestamps. Please double-check the deployments.", fixed = TRUE)
	file.remove("deployments.csv")
})

test_that("checkDeployments kicks in if deployment periods overlap", {
	dep <- example.deployments
	dep$Receiver[2] <- dep$Receiver[1]
	write.csv(dep, "deployments.csv", row.names = FALSE)
	deployments <- loadDeployments(input = "deployments.csv", tz = "Europe/Copenhagen")
	sink("temp.txt")
  expect_message(
  	expect_error(checkDeploymentTimes(input = deployments),
  		"Fatal exception found. Read lines above for more details.", fixed = TRUE),
  "Error: Receiver 132907 was re-deployed before being retrieved:", fixed = TRUE)
  sink()
})

test_that("checkDeployments kicks in if deployment periods overlap", {
	dep <- example.deployments
	dep$Station.name[2] <- "test"
	write.csv(dep, "deployments.csv", row.names = FALSE)
	deployments <- loadDeployments(input = "deployments.csv", tz = "Europe/Copenhagen")
	write.csv(example.spatial, "spatial.csv", row.names = FALSE)
  spatial <- loadSpatial(input = "spatial.csv")
  expect_warning(
  	expect_error(checkDeploymentStations(input = deployments, spatial = spatial),
  		"The following station is listed in the spatial file but no receivers were ever deployed there: 'Station 1'\n", fixed = TRUE),
  "The following station is listed in the deployments but is not part of the study's stations: 'test'", fixed = TRUE)
	file.remove("deployments.csv")
	file.remove("spatial.csv")
})

test_that("loadDeployments output is exactly as expected", {
	write.csv(example.deployments, "deployments.csv", row.names = FALSE)
	output <- loadDeployments(input = "deployments.csv", tz = "Europe/Copenhagen")
	expect_equal(output, example.deployments)	
	file.remove("deployments.csv")
})

test_that("loadDeployments converts factors to character", {
	xdep <- example.deployments
	xdep$Station.name <- as.factor(xdep$Station.name)
	output <- loadDeployments(input = xdep, tz = "Europe/Copenhagen")
	expect_equal(typeof(output$Station.name), "character")	
})

setwd(tests.home)

if (is.na(oldtz)) Sys.unsetenv("TZ") else Sys.setenv(TZ = oldtz)

rm(list = ls())
