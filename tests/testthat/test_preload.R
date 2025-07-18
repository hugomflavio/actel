skip_on_cran()
oldtz <- Sys.getenv('TZ', unset = NA)
Sys.setenv(TZ = 'UTC')

tests.home <- getwd()
setwd(tempdir())

exampleWorkspace("exampleWorkspace", force = TRUE)
setwd("exampleWorkspace")

bio <- read.csv("biometrics.csv")
deployments <- read.csv("deployments.csv")
spatial <- read.csv("spatial.csv")
detections <- example.detections
detections$ExtraCol <- NA # HF: what is this for?...
dot <- paste(unique(spatial$Array[spatial$Type == "Hydrophone"]),
       collapse = "--")

test_that("Correct error is triggered if datapack is not valid", {
  expect_error(results <- explore(datapack = "a"),
    paste0("The datapack's token is invalid or missing. Please use the",
         " function preload() to compile the input data.\nData must be",
         " compiled during the current R session to avoid",
         " complications."),
    fixed = TRUE)

  expect_error(results <- migration(datapack = "a"),
    paste0("The datapack's token is invalid or missing. Please use the",
         " function preload() to compile the input data.\nData must be",
         " compiled during the current R session to avoid",
         " complications."),
    fixed = TRUE)

  expect_error(results <- residency(datapack = "a"),
    paste0("The datapack's token is invalid or missing. Please use the",
         " function preload() to compile the input data.\nData must be",
         " compiled during the current R session to avoid",
         " complications."),
    fixed = TRUE)
})

test_that("basic preload works as expected", {
  expect_warning(d <<- preload(biometrics = bio, deployments = deployments,
                               spatial = spatial, detections = detections,
                               dot = dot, tz = "Europe/Copenhagen"),
    "No detections were found for receiver(s) 132907",
    fixed = TRUE)
})

test_that("checkArguments complains if preload is used together with tz", {
  expect_warning(explore(datapack = d, tz = "Europe/Copenhagen"),
                 paste0("Argument 'tz' was set but a datapack was provided.",
                         " Disregarding set arguments."),
                 fixed = TRUE)
})
# n
# n

test_that("checkArguments stops if wrong override is provided with preload", {
  expect_error(
    expect_warning(explore(datapack = d, override = 1234),
                   paste0("Override is numeric (i.e. the code space has not",
                          " been included). Will attempt to identify tags to",
                          " be excluded based on signal alone."),
                   fixed = TRUE),
    paste0("Some tags listed in 'override' (1234) are not listed in the",
           " biometrics data"),
    fixed = TRUE)
  expect_error(explore(datapack = d, override = "1234"),
               paste0("Some tags listed in 'override' (1234) are not listed in",
                      " the biometrics data"),
               fixed = TRUE)
})

test_that("explore with preload == explore with traditional loading", {
  results <- suppressWarnings(explore(datapack = d))

  results2 <- suppressWarnings(explore(tz = "Europe/Copenhagen"))

  for (i in 1:length(results$valid.movements)) {
    expect_equal(results$movements[[i]], results2$movements[[i]])
    expect_equal(results$valid.movements[[i]], results2$valid.movements[[i]])
  }

  expect_equal(extractSignals(names(results$valid.movements)),
               extractSignals(names(results2$valid.movements)))

  expect_equal(results$arrays, results2$arrays)

  expect_equal(results$spatial, results2$spatial)
})
# n
# n
# n
# n

test_that("migration with preload == migration with traditional loading", {
  expect_warning(d2 <- preload(biometrics = bio, deployments = deployments,
                               spatial = spatial, detections = detections,
                               tz = "Europe/Copenhagen"),
                 "No detections were found for receiver(s) 132907",
                 fixed = TRUE)

  results <- suppressWarnings(migration(datapack = d2))
  results2 <- suppressWarnings(migration(tz = "Europe/Copenhagen"))

  expect_equal(extractSignals(names(results$valid.movements)),
               extractSignals(names(results2$valid.movements)))

  expect_equal(results$status.df[, -1], results2$status.df[, -1])

  for (i in 1:length(results$valid.movements)) {
    expect_equal(results$movements[[i]], results2$movements[[i]])
    expect_equal(results$valid.movements[[i]], results2$valid.movements[[i]])
  }
})
# n # migration1
# n
# n # migration2
# n


test_that("residency with preload == residency with traditional loading", {
  expect_warning(d2 <- preload(biometrics = bio, deployments = deployments,
                               spatial = spatial, detections = detections,
                               tz = "Europe/Copenhagen"),
                 "No detections were found for receiver(s) 132907",
                 fixed = TRUE)

  results <- suppressWarnings(residency(datapack = d2))
  results2 <- suppressWarnings(residency(tz = "Europe/Copenhagen"))

  expect_equal(extractSignals(names(results$valid.movements)),
               extractSignals(names(results2$valid.movements)))

  expect_equal(results$arrays, results2$arrays)
  expect_equal(results$status.df[, -1], results2$status.df[, -1])

  for (i in 1:length(results$valid.movements)) {
    expect_equal(results$movements[[i]], results2$movements[[i]])
    expect_equal(results$valid.movements[[i]], results2$valid.movements[[i]])
  }
})
# n # residency1
# n
# n
# n # residency2
# n
# n

test_that("tz and exclude.tags stops are working", {
  expect_error(preload(tz = "test"),
               paste0("'tz' could not be recognized as a time zone. Check",
                       " available time zones with OlsonNames()"),
               fixed = TRUE)

  expect_error(preload(tz = "Europe/Copenhagen", exclude.tags = "test"),
               paste0("Not all contents in 'exclude.tags' could be recognized",
                       " as tags (i.e. 'codespace-signal'). Valid examples:",
                       " 'R64K-1234', A69-1303-1234'"),
               fixed = TRUE)
})

test_that("dot stop is working", {
  expect_error(
    expect_warning(preload(biometrics = bio, deployments = deployments,
                           spatial = spatial, detections = detections,
                           tz = "Europe/Copenhagen", dot = 1),
                   "No detections were found for receiver(s) 132907",
                   fixed = TRUE),
    paste0("'dot' was set but could not recognised as a string. Please",
           " prepare a dot string and include it in the dot argument.\n",
           "You can use readDot to check the quality of your dot string."),
    fixed = TRUE)
})

test_that("start.time and stop.time are working fine in preload", {
  expect_message(suppressWarnings(preload(biometrics = bio,
                                          deployments = deployments,
                                          spatial = spatial,
                                          detections = detections,
                                          tz = "Europe/Copenhagen",
                                          start.time = "2018-05-01 00:00")),
                 paste0("M: Discarding detection data previous to",
                         " 2018-05-01 00:00 per user command (9866 detections",
                         " discarded)."),
                 fixed = TRUE)

  expect_message(suppressWarnings(preload(biometrics = bio,
                                          deployments = deployments,
                                          spatial = spatial,
                                          detections = detections,
                                          tz = "Europe/Copenhagen",
                                          stop.time = "2018-05-08 00:00")),
                 paste0("M: Discarding detection data posterior to",
                         " 2018-05-08 00:00 per user command (544 detections",
                         " discarded)."),
                 fixed = TRUE)

  expect_message(suppressWarnings(preload(biometrics = bio,
                                          deployments = deployments,
                                          spatial = spatial,
                                          detections = detections,
                                          tz = "Europe/Copenhagen",
                                          start.time = "2018-05-01 00:00",
                                          stop.time = "2018-05-08 00:00")),
                 paste0("M: Data time range: 2018-05-01 00:04:30 to",
                         " 2018-05-07 23:59:47 (Europe/Copenhagen)."),
                 fixed = TRUE)
})

write.csv(example.distances, "distances.csv")

test_that("distances are correctly handled with preload", {
  d2 <- suppressWarnings(preload(biometrics = bio, deployments = deployments,
                                 spatial = spatial, detections = detections,
                                 dot = dot, distances = example.distances,
                                 tz = "Europe/Copenhagen"))

  results <- suppressWarnings(explore(datapack = d2))

  results2 <- suppressWarnings(explore(tz = "Europe/Copenhagen"))

  for (i in 1:length(results$valid.movements)) {
    expect_equal(results$movements[[i]], results2$movements[[i]])
    expect_equal(results$valid.movements[[i]], results2$valid.movements[[i]])
  }

  expect_equal(extractSignals(names(results$valid.movements)),
               extractSignals(names(results2$valid.movements)))

  expect_equal(results$arrays, results2$arrays)

  expect_equal(results$spatial, results2$spatial)
})
# n
# n
# n
# n

test_that("preload stops if mandatory columns are missing or have NAs", {
  x <- detections[, -1]
  expect_error(suppressWarnings(preload(biometrics = bio,
                                        deployments = deployments,
                                        spatial = spatial, detections = x,
                                        dot = dot,
                                        distances = example.distances,
                                        tz = "Europe/Copenhagen")),
               paste0("The following mandatory columns are missing in the",
                       " detections: Timestamp"),
               fixed = TRUE)

  x <- detections[, -match("Signal", colnames(detections))]
  expect_error(suppressWarnings(preload(biometrics = bio,
                                        deployments = deployments,
                                        spatial = spatial, detections = x,
                                        dot = dot,
                                        distances = example.distances,
                                        tz = "Europe/Copenhagen")),
               paste0("The following mandatory columns are missing in the",
                      " detections: Signal\nThe functions extractSignals and",
                      " extractCodeSpaces can be used to break transmitter",
                      " codes apart."),
               fixed = TRUE)

  x <- detections
  x[1, 1] <- NA
  expect_error(suppressWarnings(preload(biometrics = bio,
                                        deployments = deployments,
                                        spatial = spatial, detections = x,
                                        dot = dot,
                                        distances = example.distances,
                                        tz = "Europe/Copenhagen")),
               paste0("There is missing data in the following mandatory",
                      " columns of the detections: Timestamp"),
               fixed = TRUE)
})

test_that("Data conversion warnings and errors kick in", {
  d <- detections
  d$Signal <- as.character(d$Signal)
  expect_warning(preload(biometrics = bio, deployments = deployments,
                         spatial = spatial, detections = d,
                         dot = dot, distances = example.distances,
                         tz = "Europe/Copenhagen"),
                 paste0("The 'Signal' column in the detections is not of",
                        " type integer. Attempting to convert."),
                 fixed = TRUE)

  d <- detections
  d$Receiver <- as.character(d$Receiver)
  expect_warning(preload(biometrics = bio, deployments = deployments,
                         spatial = spatial, detections = d,
                         dot = dot, distances = example.distances,
                         tz = "Europe/Copenhagen"),
                 paste0("The 'Receiver' column in the detections is not of",
                        " type integer. Attempting to convert."),
                        fixed = TRUE)

  d <- detections
  d$Receiver <- paste0("a-", d$Receiver)
  expect_warning(preload(biometrics = bio, deployments = deployments,
                         spatial = spatial, detections = d,
                         dot = dot, distances = example.distances,
                         tz = "Europe/Copenhagen"),
                 paste0("Attempting to convert the 'Receiver' to integer",
                        " failed. Attempting to extract only the serial",
                        " numbers."),
                 fixed = TRUE)

  d <- detections
  d$Receiver <- "a"
  expect_error(suppressWarnings(preload(biometrics = bio,
                                        deployments = deployments,
                                        spatial = spatial, detections = d,
                                        dot = dot,
                                        distances = example.distances,
                                        tz = "Europe/Copenhagen")),
                "Extracting the serial numbers failed. Aborting.",
                fixed = TRUE)

  d <- detections
  d$Sensor.Value <- "1"
  expect_warning(preload(biometrics = bio, deployments = deployments,
                         spatial = spatial, detections = d,
                         dot = dot, distances = example.distances,
                         tz = "Europe/Copenhagen"),
                 paste0("The 'Sensor.Value' column in the detections is not",
                        " of type numeric. Attempting to convert."),
                 fixed = TRUE)

  d <- detections
  d$Sensor.Value <- "b"
  expect_error(suppressWarnings(preload(biometrics = bio,
                                        deployments = deployments,
                                        spatial = spatial, detections = d,
                                        dot = dot,
                                        distances = example.distances,
                                        tz = "Europe/Copenhagen")),
               paste0("Attempting to convert the 'Sensor.Value' to numeric",
                      " failed. Aborting."),
               fixed = TRUE)

  d <- detections
  d$Timestamp <- as.character(d$Timestamp)
  expect_message(suppressWarnings(preload(biometrics = bio,
                                          deployments = deployments,
                                          spatial = spatial, detections = d,
                                          dot = dot,
                                          distances = example.distances,
                                          tz = "Europe/Copenhagen")),
                 "Converting detection timestamps to POSIX objects",
                 fixed = TRUE)

  d <- detections
  d$Timestamp <- "b"
  expect_error(suppressWarnings(preload(biometrics = bio,
                                        deployments = deployments,
                                        spatial = spatial, detections = d,
                                        dot = dot,
                                        distances = example.distances,
                                        tz = "Europe/Copenhagen")),
               "Converting the timestamps failed. Aborting.",
               fixed = TRUE)

  d <- detections
  d$Valid <- "b"
  expect_warning(preload(biometrics = bio, deployments = deployments,
                         spatial = spatial, detections = d, dot = dot,
                         distances = example.distances,
                         tz = "Europe/Copenhagen"),
                 paste0("The detections have a column named 'Valid' but its",
                        " content is not logical. Resetting to Valid = TRUE."),
                 fixed = TRUE)
})

test_that("preload stops if user-defined section order has duplicates", {
  expect_error(preload(biometrics = bio, deployments = deployments,
                       spatial = spatial, detections = detections, dot = dot,
                       tz = "Europe/Copenhagen", section.order = c("A", "A")),
               paste0("Some section names are duplicated in the",
                      " 'section.order' argument. Please include each section",
                      " only once."),
               fixed = TRUE)
})

test_that("preload can handle spatial objects with one array only", {
  xdeployments <- deployments[deployments$Station == "Station 1", ]
  xspatial <- spatial[c(2, 18), ]
  xdetections <- detections[detections$Receiver == xdeployments$Receiver, ]
  expect_warning(preload(biometrics = bio, deployments = xdeployments,
                         spatial = xspatial, detections = xdetections,
                         tz = "Europe/Copenhagen"),
                 regexp = NA)
})

test_that("preload does not flop on circular study areas (extreme test)", {
  xdot <- paste0(dot, "--A0")
  expect_warning(preload(biometrics = bio, deployments = deployments,
                         spatial = spatial, detections = detections,
                         dot = xdot, tz = "Europe/Copenhagen"),
                 "No detections were found for receiver(s) 132907",
                 fixed = TRUE)
})

test_that("preload handles non-character timestamps and/or codespaces", {
  xdetections <- detections
  xdetections$Timestamp <- as.POSIXct(xdetections$Timestamp,
                                      tz = "Europe/Copenhagen")
  xdetections$CodeSpace <- as.factor(xdetections$CodeSpace)
  expect_warning(preload(biometrics = bio, deployments = deployments,
                         spatial = spatial, detections = xdetections,
                         dot = dot, tz = "Europe/Copenhagen"),
                 "No detections were found for receiver(s) 132907",
                 fixed = TRUE)
})

test_that("preload works even if Sensor.Value/Sensor.Unit is missing", {
  xdetections <- detections
  xdetections$Sensor.Value <- NULL
  expect_warning(preload(biometrics = bio, deployments = deployments,
                         spatial = spatial, detections = xdetections,
                         dot = dot, tz = "Europe/Copenhagen"),
                 paste0("Could not find a 'Sensor.Value' column in the",
                        " detections. Filling one with NA."),
                 fixed = TRUE)

  xdetections <- detections
  xdetections$Sensor.Unit <- NULL
  expect_warning(preload(biometrics = bio, deployments = deployments,
                         spatial = spatial, detections = xdetections,
                         dot = dot, tz = "Europe/Copenhagen"),
                 paste0("Could not find a 'Sensor.Unit' column in the",
                        " detections. Filling one with NA."),
                 fixed = TRUE)
})

setwd("..")
unlink("exampleWorkspace", recursive = TRUE)
setwd(tests.home)

if (is.na(oldtz)) {
  Sys.unsetenv("TZ")
} else {
  Sys.setenv(TZ = oldtz)
}

rm(list = ls())
