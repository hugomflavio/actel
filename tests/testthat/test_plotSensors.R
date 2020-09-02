skip_on_cran()

test_that("plotSensors' failsafes kick in when needed", {
	expect_error(plotSensors("a"), "Could not recognise the input as an actel results object.", fixed = TRUE)
	expect_error(plotSensors(list("a")), "Could not recognise the input as an actel results object.", fixed = TRUE)
	expect_error(plotSensors(example.results, tag = 1:2), "Please list only one tag", fixed = TRUE)
	expect_error(plotSensors(example.results, tag = "a"),
		"Could not find tag 'a' in the input.", fixed = TRUE)
	expect_error(plotSensors(example.results, tag = "R64K-4451", sensor = "test"),
		"Could not find sensor unit(s) 'test' in the tag detections.", fixed = TRUE)

	xresults <- example.results
	xresults$valid.detections[[1]]$Sensor.Value <- NA
	expect_error(plotSensors(xresults, tag = "R64K-4451"),
		"No sensor data found for this tag.", fixed = TRUE)
})

test_that("plotSensors is working", {
	p <- plotSensors(example.results, tag = "R64K-4451")
	expect_that(p, is_a("ggplot"))

	xresults <- example.results
	xresults$valid.detections[[1]]$Sensor.Value[1:3] <- NA
	expect_warning(plotSensors(xresults, tag = "R64K-4451"),
		"3 rows in this tag's detections do not contain sensor values and will be discarded.", fixed = TRUE)

	xresults <- example.results
	xresults$valid.detections[[1]]$Sensor.Unit[1:5] <- NA
	expect_error(plotSensors(xresults, tag = "R64K-4451"),
		"Not all rows with sensor data contain a sensor unit! Plotting unknown data separately.", fixed = TRUE)
})
