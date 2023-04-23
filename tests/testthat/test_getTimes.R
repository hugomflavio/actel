skip_on_cran()
oldtz <- Sys.getenv('TZ', unset = NA)
Sys.setenv(TZ = 'UTC')

tests.home <- getwd()
setwd(tempdir())
exampleWorkspace("exampleWorkspace", force = TRUE)
setwd("exampleWorkspace")
write.csv(example.distances, "distances.csv")

study.data <- suppressWarnings(loadStudyData(tz = "Europe/Copenhagen", start.time = NULL,
	stop.time = NULL, exclude.tags = NULL))
# n
detections.list <- study.data$detections.list
bio <- study.data$bio
spatial <- study.data$spatial
dist.mat <- study.data$dist.mat

moves <- groupMovements(detections.list = detections.list[1:2], bio = bio, spatial = spatial,
    speed.method = "last to first", max.interval = 60, tz = "Europe/Copenhagen", dist.mat = dist.mat)

aux <- list(valid.movements = moves, spatial = spatial, rsp.info = list(bio = bio, analysis.type = "explore"))

test_that("getTimes's failsafes kick in if necessary", {
  expect_error(getTimes("a"), "Could not recognise the input as an actel results object.", fixed = TRUE)

  expect_error(getTimes(list("a")), "Could not recognise the input as an actel results object.", fixed = TRUE)

  expect_error(getTimes(aux, locations = "test"), "Array 'test' is not part of this study's arrays.", fixed = TRUE)

  expect_error(getTimes(example.results, move.type = "section", locations = "test"),
    "Section 'test' is not part of this study's sections.", fixed = TRUE)

  expect_error(getTimes(aux, move.type = "section", locations = "test"),
    "Section times are not calculated for analyses of type 'explore'.", fixed = TRUE)
})


test_that("getTimes operates correctly under all options.", {
  times <- getTimes(input = aux, move.type = "array", event.type = "arrival", n.events = "all")
  expect_equal(colnames(times), as.vector(c("Transmitter", "Group", unlist(spatial$array.order)[-1])))
  expect_equal(sum(grepl("R64K-4451", times$Transmitter)), 10)
  expect_equal(as.vector(table(times$Transmitter)), c(10, 5))
  expect_equal(times$A1[1], moves[[1]]$First.time[1])
  # try other section
  expect_equal(times$A7[2], moves[[1]]$First.time[8])

  times <- getTimes(input = aux, move.type = "array", event.type = "arrival", n.events = "first")
  expect_equal(colnames(times), as.vector(c("Transmitter", "Group", unlist(spatial$array.order)[-1])))
  expect_equal(sum(grepl("R64K-4451", times$Transmitter)), 1)
  expect_equal(as.vector(table(times$Transmitter)), c(1, 1))
  expect_equal(times$A2[1], moves[[1]]$First.time[2])
  # try second tag
  expect_equal(times$A6[2], moves[[2]]$First.time[6])
 	expect_false(any(is.na(times)))

  times <- getTimes(input = aux, move.type = "array", event.type = "arrival", n.events = "last")
  expect_equal(colnames(times), as.vector(c("Transmitter", "Group", unlist(spatial$array.order)[-1])))
  expect_equal(sum(grepl("R64K-4451", times$Transmitter)), 1)
  expect_equal(as.vector(table(times$Transmitter)), c(1, 1))
  expect_equal(times$A2[1], moves[[1]]$First.time[2])
  # try second tag
  expect_equal(times$A6[2], moves[[2]]$First.time[6])
 	expect_false(any(is.na(times)))

  times <- getTimes(input = aux, move.type = "array", event.type = "departure", n.events = "all")
  expect_equal(colnames(times), as.vector(c("Transmitter", "Group", unlist(spatial$array.order)[-1])))
  expect_equal(sum(grepl("R64K-4451", times$Transmitter)), 10)
  expect_equal(as.vector(table(times$Transmitter)), c(10, 5))
  expect_equal(times$A1[1], moves[[1]]$Last.time[1])
  # try second tag other section
  expect_equal(times$A7[13], moves[[2]]$Last.time[9])

  times <- getTimes(input = aux, move.type = "array", event.type = "departure", n.events = "first")
  expect_equal(colnames(times), as.vector(c("Transmitter", "Group", unlist(spatial$array.order)[-1])))
  expect_equal(sum(grepl("R64K-4451", times$Transmitter)), 1)
  expect_equal(as.vector(table(times$Transmitter)), c(1, 1))
  expect_equal(times$A1[2], moves[[2]]$Last.time[1])
  expect_equal(times$A7[2], moves[[2]]$Last.time[7])
 	expect_false(any(is.na(times)))

  getTimes(input = aux, location = "A1")
})

setwd("..")
unlink("exampleWorkspace", recursive = TRUE)
setwd(tests.home)


if (is.na(oldtz)) Sys.unsetenv("TZ") else Sys.setenv(TZ = oldtz)

rm(list = ls())
