dir.create("detections")
aux <- split(example.detections, example.detections$Receiver)
for (i in names(aux)[1:3]) {
  write.csv(aux[[i]], paste0("detections/", i, ".csv"), row.names = FALSE)
}

det <- loadDetections(start.time = NULL, stop.time = NULL, tz = "Europe/Copenhagen", force = FALSE)
unlink("detections", recursive = TRUE)

write.csv(example.spatial, "spatial.csv", row.names = FALSE)
spatial <- loadSpatial()
file.remove("spatial.csv")

write.csv(example.deployments, "deployments.csv", row.names = FALSE)
dep <- loadDeployments(file = "deployments.csv", tz = "Europe/Copenhagen")
dep <- createUniqueSerials(input = dep)
file.remove("deployments.csv")

test_that("createStandards is working as expected", {
	expect_warning(output <- createStandards(detections = det, spatial = spatial, deployments = dep),
		"No detections were found for receiver(s) 132907, 132915, 132916, 132917, 132918, 133205, 133206, 133209, 133210, 133215, 133220, 133221, 133222, 133224.", fixed = TRUE)
	expect_equal(colnames(output), c('Timestamp', 'Receiver', 'CodeSpace', 'Signal', 'Transmitter', 'Standard.name', 'Array'))
	expect_equal(as.character(unique(output$Standard.name)), c("St.2", "St.6", "St.8"))
})

write.csv(example.deployments, "deployments.csv", row.names = FALSE)
dep <- loadDeployments(file = "deployments.csv", tz = "Europe/Copenhagen")
dep$Start[2] <- dep$Start[2] + 3600 * 24 * 30
dep <- createUniqueSerials(input = dep)
file.remove("deployments.csv")

test_that("createStandards removes detections outside deployments", {
	expect_message(suppressWarnings(output <- createStandards(detections = det, spatial = spatial, deployments = dep)),
		"Error: 232 detections for receiver 132908 do not fall within deployment periods.", fixed = TRUE)
	expect_equal(colnames(output), c('Timestamp', 'Receiver', 'CodeSpace', 'Signal', 'Transmitter', 'Standard.name', 'Array'))
	expect_equal(as.character(unique(output$Standard.name)), c("St.2", "St.6", "St.8"))
})

file.remove(list.files(pattern = "*txt$"))
