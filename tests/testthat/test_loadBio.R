test_that("loadBio stops if arguments or file are missing", {
	# Missing arguments
	expect_error(loadBio(), "'file' is missing.", fixed = TRUE)
	expect_error(loadBio(file = "test"), "'tz' is missing.", fixed = TRUE)

	# Missing file
	expect_error(loadBio("biometrics.csv", tz = "Europe/Copenhagen"))
})

test_that("loadBio stops if there are duplicated columns", {
	# Duplicated cols
	bio <- example.biometrics
	colnames(bio)[2:3] <- "Group"
	write.csv(bio, "biometrics.csv", row.names = FALSE)
	expect_error(loadBio("biometrics.csv", tz = "Europe/Copenhagen"), 
		"The following columns are duplicated in the file 'biometrics.csv': 'Group'.", fixed = TRUE)	
	file.remove("biometrics.csv")
})

test_that("loadBio fails if needed columns are missing", {
	# Missing release date
	bio <- example.biometrics
	colnames(bio)[1] <- "test"
	write.csv(bio, "biometrics.csv", row.names = FALSE)
	expect_error(loadBio("biometrics.csv", tz = "Europe/Copenhagen"), 
		"The biometrics.csv file must contain an 'Release.date' column.", fixed = TRUE)

	# Missing Signal column
	bio <- example.biometrics
	colnames(bio)[4] <- "test"
	write.csv(bio, "biometrics.csv", row.names = FALSE)
	expect_error(loadBio("biometrics.csv", tz = "Europe/Copenhagen"), 
		"The biometrics.csv file must contain an 'Signal' column.", fixed = TRUE)

	# No release sites
	bio <- example.biometrics
	write.csv(bio[, -2], "biometrics.csv", row.names = FALSE)
	expect_message(loadBio("biometrics.csv", tz = "Europe/Copenhagen"), 
		"M: No Release site has been indicated in the biometrics.csv file. Creating a 'Release.site' column to avoid function failure. Filling with 'unspecified'.", fixed = TRUE)

	# no group column
	bio <- example.biometrics
	write.csv(bio[, -5], "biometrics.csv", row.names = FALSE)
	expect_message(loadBio("biometrics.csv", tz = "Europe/Copenhagen"), 
		"M: No 'Group' column found in the biometrics.csv file. Assigning all fish to group 'All'.", fixed = TRUE)
	file.remove("biometrics.csv")
})

test_that("loadBio stops if column content is unexpected", {
	# Badly formated release date
	bio <- example.biometrics
	bio$Release.date[1] <- "test"
	write.csv(bio, "biometrics.csv", row.names = FALSE)
	expect_error(loadBio("biometrics.csv", tz = "Europe/Copenhagen"), 
		"Not all values in the 'Release.date' column appear to be in a 'yyyy-mm-dd hh:mm' format (seconds are optional). Please double-check the biometrics.csv file.", fixed = TRUE)

	# Badly coded release date
	bio <- example.biometrics
	bio$Release.date <- as.character(bio$Release.date)
	bio$Release.date[1] <- "2999-19-39 29:59"
	write.csv(bio, "biometrics.csv", row.names = FALSE)
	expect_error(loadBio("biometrics.csv", tz = "Europe/Copenhagen"), 
		"Could not recognise the data in the 'Release.date' column as POSIX-compatible timestamps. Please double-check the biometrics.csv file.", fixed = TRUE)

	# Badly formatted signal
	bio <- example.biometrics
	bio$Signal[1] <- "test"
	write.csv(bio, "biometrics.csv", row.names = FALSE)
	expect_error(loadBio("biometrics.csv", tz = "Europe/Copenhagen"), 
		"Could not recognise the data in the 'Signal' column as integers. Please double-check the biometrics.csv file.", fixed = TRUE)

	# Missing signal data
	bio <- example.biometrics
	bio$Signal[1] <- NA
	write.csv(bio, "biometrics.csv", row.names = FALSE)
	expect_error(loadBio("biometrics.csv", tz = "Europe/Copenhagen"), 
		"Some fish have no 'Signal' information. Please double-check the biometrics.csv file.", fixed = TRUE)

	# one duplicated signal
	bio <- example.biometrics
	bio$Signal[1:2] <- 1234
	write.csv(bio, "biometrics.csv", row.names = FALSE)
	expect_error(loadBio("biometrics.csv", tz = "Europe/Copenhagen"), 
		"Signal 1234 is duplicated in the biometrics.csv file.", fixed = TRUE)

	# multiple duplicated signal
	bio <- example.biometrics
	bio$Signal[1:4] <- c(1234, 1234, 5678, 5678)
	write.csv(bio, "biometrics.csv", row.names = FALSE)
	expect_error(loadBio("biometrics.csv", tz = "Europe/Copenhagen"), 
		"Signals 1234, 5678 are duplicated in the biometrics.csv file.", fixed = TRUE)

	# some fish missing release site information	
	bio <- example.biometrics
	bio$Release.site[1] <- NA
	write.csv(bio, "biometrics.csv", row.names = FALSE)
	expect_warning(loadBio("biometrics.csv", tz = "Europe/Copenhagen"), 
		"Some fish contain no release site information. You may want to double-check the data.\n   Filling the blanks with 'unspecified'.", fixed = TRUE)
	
	bio <- example.biometrics
	bio$Release.site <- as.character(bio$Release.site)
	bio$Release.site[1] <- ""
	write.csv(bio, "biometrics.csv", row.names = FALSE)
	expect_warning(loadBio("biometrics.csv", tz = "Europe/Copenhagen"), 
		"Some fish contain no release site information. You may want to double-check the data.\n   Filling the blanks with 'unspecified'.", fixed = TRUE)

	# some fish missing group information	
	bio <- example.biometrics
	bio$Group[1] <- NA
	write.csv(bio, "biometrics.csv", row.names = FALSE)
	expect_warning(loadBio("biometrics.csv", tz = "Europe/Copenhagen"), 
		"Some fish contain no group information. You may want to double-check the data.\n   Filling the blanks with 'unspecified'.", fixed = TRUE)

	bio <- example.biometrics
	bio$Group <- as.character(bio$Group)
	bio$Group[1] <- ""
	write.csv(bio, "biometrics.csv", row.names = FALSE)
	expect_warning(loadBio("biometrics.csv", tz = "Europe/Copenhagen"), 
		"Some fish contain no group information. You may want to double-check the data.\n   Filling the blanks with 'unspecified'.", fixed = TRUE)

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
		"M: Some fish groups contain one or more '.' characters. To avoid function failure, these will be replaced with '_'.", fixed = TRUE)
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