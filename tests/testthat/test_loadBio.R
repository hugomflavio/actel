skip_on_cran()
oldtz <- Sys.getenv('TZ', unset = NA)
Sys.setenv(TZ = 'UTC')

tests.home <- getwd()
setwd(tempdir())

test_that("loadBio stops if arguments or file are missing", {
	# Missing arguments
	expect_error(loadBio(), "'input' is missing.", fixed = TRUE)
	expect_error(loadBio(input = "test"), "'tz' is missing.", fixed = TRUE)

	# Missing file
	expect_error(loadBio("biometrics.csv", tz = "Europe/Copenhagen"),
		"Could not find a 'biometrics.csv' file in the working directory.", fixed = TRUE)
})

test_that("loadBio stops if there are duplicated columns", {
	# Duplicated cols
	bio <- example.biometrics
	colnames(bio)[2:3] <- "Group"
	write.csv(bio, "biometrics.csv", row.names = FALSE)
	expect_error(loadBio("biometrics.csv", tz = "Europe/Copenhagen"),
		"The following columns are duplicated in the biometrics: 'Group'.", fixed = TRUE)	
	file.remove("biometrics.csv")
})

test_that("loadBio fails if needed columns are missing", {
	# Missing release date
	bio <- example.biometrics
	colnames(bio)[1] <- "test"
	write.csv(bio, "biometrics.csv", row.names = FALSE)
	expect_error(loadBio("biometrics.csv", tz = "Europe/Copenhagen"),
		"The biometrics must contain an 'Release.date' column.", fixed = TRUE)

	# Missing Signal column
	bio <- example.biometrics
	colnames(bio)[4] <- "test"
	write.csv(bio, "biometrics.csv", row.names = FALSE)
	expect_error(loadBio("biometrics.csv", tz = "Europe/Copenhagen"),
		"The biometrics must contain an 'Signal' column.", fixed = TRUE)

	# No release sites
	bio <- example.biometrics
	write.csv(bio[, -2], "biometrics.csv", row.names = FALSE)
	expect_message(loadBio("biometrics.csv", tz = "Europe/Copenhagen"),
		"M: No Release site has been indicated in the biometrics. Creating a 'Release.site' column to avoid function failure. Filling with 'unspecified'.", fixed = TRUE)

	# no group column
	bio <- example.biometrics
	write.csv(bio[, -5], "biometrics.csv", row.names = FALSE)
	expect_message(loadBio("biometrics.csv", tz = "Europe/Copenhagen"),
		"M: No 'Group' column found in the biometrics. Assigning all animals to group 'All'.", fixed = TRUE)
	file.remove("biometrics.csv")
})

test_that("loadBio stops if column content is unexpected", {
	# Badly formated release date
	bio <- example.biometrics
	bio$Release.date <- as.character(bio$Release.date)
	bio$Release.date[1] <- "test"
	write.csv(bio, "biometrics", row.names = FALSE)
	expect_error(loadBio("biometrics", tz = "Europe/Copenhagen"),
		"Not all values in the 'Release.date' column appear to be in a 'yyyy-mm-dd hh:mm' format (seconds are optional). Please double-check the biometrics.", fixed = TRUE)

	# Badly coded release date
	bio <- example.biometrics
	bio$Release.date <- as.character(bio$Release.date)
	bio$Release.date[1] <- "2999-19-39 29:59"
	write.csv(bio, "biometrics.csv", row.names = FALSE)
	expect_error(loadBio("biometrics.csv", tz = "Europe/Copenhagen"),
		"Could not recognise the data in the 'Release.date' column as POSIX-compatible timestamps. Please double-check the biometrics.", fixed = TRUE)

	# Badly formatted signal
	bio <- example.biometrics
	bio$Signal[1] <- "test"
	write.csv(bio, "biometrics.csv", row.names = FALSE)
	expect_error(loadBio("biometrics.csv", tz = "Europe/Copenhagen"),
		"Could not recognise the data in the 'Signal' column as integers. Please double-check the biometrics.", fixed = TRUE)

	# Missing signal data
	bio <- example.biometrics
	bio$Signal[1] <- NA
	write.csv(bio, "biometrics.csv", row.names = FALSE)
	expect_error(loadBio("biometrics.csv", tz = "Europe/Copenhagen"),
		"Some animals have no 'Signal' information. Please double-check the biometrics.", fixed = TRUE)

	# one duplicated signal
	bio <- example.biometrics
	bio$Signal[1:2] <- 1234
	write.csv(bio, "biometrics.csv", row.names = FALSE)
	expect_error(loadBio("biometrics.csv", tz = "Europe/Copenhagen"),
		"Signal 1234 is duplicated in the biometrics.", fixed = TRUE)

	# multiple duplicated signal
	bio <- example.biometrics
	bio$Signal[1:4] <- c(1234, 1234, 5678, 5678)
	write.csv(bio, "biometrics.csv", row.names = FALSE)
	expect_error(loadBio("biometrics.csv", tz = "Europe/Copenhagen"),
		"Signals 1234, 5678 are duplicated in the biometrics.", fixed = TRUE)

	# some animals missing release site information	
	bio <- example.biometrics
	bio$Release.site[1] <- NA
	write.csv(bio, "biometrics.csv", row.names = FALSE)
	expect_warning(loadBio("biometrics.csv", tz = "Europe/Copenhagen"),
		"Some animals contain no release site information. You may want to double-check the data.\n   Filling the blanks with 'unspecified'.", fixed = TRUE)
	
	bio <- example.biometrics
	bio$Release.site <- as.character(bio$Release.site)
	bio$Release.site[1] <- ""
	write.csv(bio, "biometrics.csv", row.names = FALSE)
	expect_warning(loadBio("biometrics.csv", tz = "Europe/Copenhagen"),
		"Some animals contain no release site information. You may want to double-check the data.\n   Filling the blanks with 'unspecified'.", fixed = TRUE)

	# some animals missing group information	
	bio <- example.biometrics
	bio$Group[1] <- NA
	write.csv(bio, "biometrics.csv", row.names = FALSE)
	expect_warning(loadBio("biometrics.csv", tz = "Europe/Copenhagen"),
		"Some animals contain no group information. You may want to double-check the data.\n   Filling the blanks with 'unspecified'.", fixed = TRUE)

	bio <- example.biometrics
	bio$Group <- as.character(bio$Group)
	bio$Group[1] <- ""
	write.csv(bio, "biometrics.csv", row.names = FALSE)
	expect_warning(loadBio("biometrics.csv", tz = "Europe/Copenhagen"),
		"Some animals contain no group information. You may want to double-check the data.\n   Filling the blanks with 'unspecified'.", fixed = TRUE)

	# Some groups are contained within others
	bio <- example.biometrics
	levels(bio$Group) <- c("A", "AB")
	write.csv(bio, "biometrics.csv", row.names = FALSE)
	expect_warning(output <- loadBio("biometrics.csv", tz = "Europe/Copenhagen"),
		"Group 'A' is contained within other groups. To avoid function failure, a number will be appended to this group.", fixed = TRUE)
	expect_equal(levels(output$Group), c("A_1", "AB"))
	rm(output)

	# Some groups have dots
	bio <- example.biometrics
	levels(bio$Group) <- c("A", "B.C")
	write.csv(bio, "biometrics.csv", row.names = FALSE)
	expect_message(output <- loadBio("biometrics.csv", tz = "Europe/Copenhagen"),
		"M: Some groups contain one or more '.' characters. To avoid function failure, these will be replaced with '_'.", fixed = TRUE)
	expect_equal(levels(output$Group), c("A", "B_C"))
	rm(output)
	file.remove("biometrics.csv")
})

test_that("loadBio output matches example.biometrics", {
	# Output is correct
	write.csv(example.biometrics, "biometrics.csv", row.names = FALSE)
	bio <- loadBio("biometrics.csv", tz = "Europe/Copenhagen")
	expect_equal(bio, example.biometrics)
	file.remove("biometrics.csv")
})

test_that("loadBio can handle multi-sensor tags.", {
	xbio <- example.biometrics[-(1:4), ]
	xbio$Signal <- as.character(xbio$Signal)
	xbio$Signal[1] <- "4453|4454"
	write.csv(xbio, "biometrics.csv", row.names = FALSE)
	expect_message(
		expect_warning(bio <- loadBio("biometrics.csv", tz = "Europe/Copenhagen"),
			"Tags with multiple sensors are listed in the biometrics, but a 'Sensor.unit' column could not be found. Skipping sensor unit assignment.", fixed = TRUE),
		"M: Multi-sensor tags detected. These tags will be referred to by their lowest signal value.", fixed = TRUE)

	xbio$Signal[1] <- "test|4454"
	write.csv(xbio, "biometrics.csv", row.names = FALSE)
	expect_message(
		expect_error(bio <- loadBio("biometrics.csv", tz = "Europe/Copenhagen"),
			"Could not recognise the data in the 'Signal' column as integers. Please double-check the biometrics.", fixed = TRUE),
		"M: Multi-sensor tags detected. These tags will be referred to by their lowest signal value.", fixed = TRUE)

	xbio$Signal[1] <- "4455|4456"
	write.csv(xbio, "biometrics.csv", row.names = FALSE)
	expect_message(
		expect_error(bio <- loadBio("biometrics.csv", tz = "Europe/Copenhagen"),
			"Signal 4456 is duplicated in the biometrics.", fixed = TRUE),
		"M: Multi-sensor tags detected. These tags will be referred to by their lowest signal value.", fixed = TRUE)
	
	xbio$Signal[1] <- "4453|4454"
	xbio$Sensor.unit <- NA
	write.csv(xbio, "biometrics.csv", row.names = FALSE)
	expect_error(bio <- loadBio("biometrics.csv", tz = "Europe/Copenhagen"),
		"The number of provided sensor units does not match the number of signals for row(s) 1 of the biometrics.", fixed = TRUE)
	file.remove("biometrics.csv")
})

test_that("loadBio converts factors to character", {
	xbio <- example.biometrics
	xbio$temp <- "dummy text"
	xbio$temp <- as.factor(xbio$temp)
	output <- loadBio(xbio, tz = "Europe/Copenhagen")
	expect_equal(typeof(output$temp), "character")	
	# note: The Release.site and Group columns are converted into a factor within loadBio
})


setwd(tests.home)

if (is.na(oldtz)) Sys.unsetenv("TZ") else Sys.setenv(TZ = oldtz)

rm(list = ls())
