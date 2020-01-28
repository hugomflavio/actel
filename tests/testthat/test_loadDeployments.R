test_that("loadDeployments stops if file is missing", {
	expect_error(loadDeployments(file = "test"), 
		"Could not find a 'test' file in the working directory.", fixed = TRUE)
})

test_that("loadDeployments stops if columns are missing or duplicated", {
	dep <- example.deployments
	colnames(dep)[1:2] <- "test"
	write.csv(dep, "deployments.csv", row.names = FALSE)
	expect_error(loadDeployments(file = "deployments.csv", tz = "Europe/Copenhagen"),
		"The following columns are duplicated in the file 'deployments.csv': 'test'.", fixed = TRUE)

	dep <- example.deployments
	colnames(dep)[1] <- "test"
	write.csv(dep, "deployments.csv", row.names = FALSE)
	expect_error(loadDeployments(file = "deployments.csv", tz = "Europe/Copenhagen"),
		"Column 'Receiver' is missing in the deployments.csv file.", fixed = TRUE)
	file.remove("deployments.csv")
})

test_that("loadDeployments stops if data is missing or badly formatted", {
	dep <- example.deployments
	colnames(dep)[2] <- "Station.Name"
	write.csv(dep, "deployments.csv", row.names = FALSE)
	expect_equal(colnames(loadDeployments(file = "deployments.csv", tz = "Europe/Copenhagen"))[2],"Station.name", fixed = TRUE)

	dep <- example.deployments
	dep$Start[1] <- NA
	write.csv(dep, "deployments.csv", row.names = FALSE)
	expect_error(loadDeployments(file = "deployments.csv", tz = "Europe/Copenhagen"),
		"Not all values in the 'Start' column appear to be in a 'yyyy-mm-dd hh:mm' format (seconds are optional). Please double-check the deployments.csv file.", fixed = TRUE)

	dep <- example.deployments
	dep$Stop[1] <- NA
	write.csv(dep, "deployments.csv", row.names = FALSE)
	expect_error(loadDeployments(file = "deployments.csv", tz = "Europe/Copenhagen"),
		"Not all values in the 'Stop' column appear to be in a 'yyyy-mm-dd hh:mm' format (seconds are optional). Please double-check the deployments.csv file.", fixed = TRUE)

	dep <- example.deployments
	dep$Start <- as.character(dep$Start)
	dep$Start[1] <- "2999-19-39 29:59:00"
	write.csv(dep, "deployments.csv", row.names = FALSE)
	expect_error(loadDeployments(file = "deployments.csv", tz = "Europe/Copenhagen"),
		"Could not recognise the data in the 'Start' column as POSIX-compatible timestamps. Please double-check the deployments.csv file.", fixed = TRUE)

	dep <- example.deployments
	dep$Stop <- as.character(dep$Stop)
	dep$Stop[1] <- "2999-19-39 29:59:00"
	write.csv(dep, "deployments.csv", row.names = FALSE)
	expect_error(loadDeployments(file = "deployments.csv", tz = "Europe/Copenhagen"),
		"Could not recognise the data in the 'Stop' column as POSIX-compatible timestamps. Please double-check the deployments.csv file.", fixed = TRUE)
	file.remove("deployments.csv")
})

test_that("loadDeployments output is exactly as expected", {
	write.csv(example.deployments, "deployments.csv", row.names = FALSE)
	output <- loadDeployments(file = "deployments.csv", tz = "Europe/Copenhagen")
	expect_equal(output, example.deployments)	
	file.remove("deployments.csv")
})

file.remove(list.files(pattern = "*txt$"))
