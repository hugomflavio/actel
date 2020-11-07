skip_on_cran()

test_that("plotLive failsafes kick in", {
	expect_error(plotLive("a"), "Could not recognise the input as an actel results or preload object.", fixed = TRUE)
	expect_error(plotLive(list("a")), "Could not recognise the input as an actel results or preload object.", fixed = TRUE)
	expect_error(plotLive(example.results, arrays = c("a" = "b")), "'arrays' was set but not all contents match array names in the study area.", fixed = TRUE)
})

test_that("plotLive works with actel results", {
	tryCatch(plotLive(example.results), warning = function(w) stop("a warning was issued where it should not had been!"))
	expect_is("The real test is above, this is just to prevent test_that from complaining", "character")
})

test_that("plotLive works with preload datasets", {
	suppressWarnings(x <- preload(biometrics = example.biometrics, spatial = example.spatial, deployments = example.deployments, 
							 									detections = example.detections, tz = "Europe/Copenhagen"))
	tryCatch(plotLive(x), warning = function(w) stop("a warning was issued where it should not had been!"))
})

test_that("plotLive works with unknown receivers", {
	xspatial <- example.spatial[-3, ]
	suppressWarnings(x <- preload(biometrics = example.biometrics, spatial = xspatial, deployments = example.deployments, 
							 									detections = example.detections, tz = "Europe/Copenhagen"))
	expect_warning(plotLive(x), "This dataset contains unknown stations. These stations will not be plotted.", fixed = TRUE)
})

test_that("show.stations argument works smoothly", {
	tryCatch(plotLive(example.results, show.stations = TRUE), warning = function(w) stop("a warning was issued where it should not had been!"))
	expect_is("The real test is above, this is just to prevent test_that from complaining", "character")
})

test_that("arrays argument works smoothly", {
	tryCatch(plotLive(example.results, arrays = "A3", show.stations = TRUE), warning = function(w) stop("a warning was issued where it should not had been!"))
	expect_is("The real test is above, this is just to prevent test_that from complaining", "character")
})

test_that("col argument works smoothly", {
	expect_warning(plotLive(example.results, col = c("blue")), "Not enough colours supplied in 'col' (1 supplied and 3 needed). Reusing colours.", fixed = TRUE)
	tryCatch(plotLive(example.results, col = c("blue", "pink", "orange")), warning = function(w) stop("a warning was issued where it should not had been!"))
})
