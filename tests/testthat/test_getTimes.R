exampleWorkspace()
setwd("exampleWorkspace")
write.csv(example.distances, "distances.csv")

study.data <- suppressWarnings(loadStudyData(tz = "Europe/Copenhagen", start.time = NULL, 
	stop.time = NULL, sections = NULL, exclude.tags = NULL))
detections.list <- study.data$detections.list
bio <- study.data$bio
spatial <- study.data$spatial
dist.mat <- study.data$dist.mat
invalid.dist <- study.data$invalid.dist

moves <- groupMovements(detections.list = detections.list[1:2], bio = bio, spatial = spatial,
    speed.method = "last to first", max.interval = 60, tz = "Europe/Copenhagen", 
    dist.mat = dist.mat, invalid.dist = invalid.dist)

test_that("getTimes operates correctly under all options.", {
  times <- getTimes(movements = moves, spatial = spatial, type = "arrival", events = "all")
  expect_equal(colnames(times), as.vector(c("Transmitter", unlist(spatial$array.order)[-1])))
  expect_equal(sum(grepl("R64K-4451", row.names(times))), 10)
  expect_equal(as.vector(table(times$Transmitter)), c(10, 5))
  expect_equal(times$River1[1], moves[[1]]$First.time[1])
  # try other section
  expect_equal(times$Fjord1[2], moves[[1]]$First.time[8])

  times <- getTimes(movements = moves, spatial = spatial, type = "arrival", events = "first")
  expect_equal(colnames(times), as.vector(c("Transmitter", unlist(spatial$array.order)[-1])))
  expect_equal(sum(grepl("R64K-4451", row.names(times))), 1)
  expect_equal(as.vector(table(times$Transmitter)), c(1, 1))
  expect_equal(times$River2[1], moves[[1]]$First.time[2])
  # try second tag
  expect_equal(times$River6[2], moves[[2]]$First.time[6])
 	expect_false(any(is.na(times)))

  times <- getTimes(movements = moves, spatial = spatial, type = "arrival", events = "last")
  expect_equal(colnames(times), as.vector(c("Transmitter", unlist(spatial$array.order)[-1])))
  expect_equal(sum(grepl("R64K-4451", row.names(times))), 1)
  expect_equal(as.vector(table(times$Transmitter)), c(1, 1))
  expect_equal(times$River2[1], moves[[1]]$First.time[2])
  # try second tag
  expect_equal(times$River6[2], moves[[2]]$First.time[6])
 	expect_false(any(is.na(times)))

  times <- getTimes(movements = moves, spatial = spatial, type = "departure", events = "all")
  expect_equal(colnames(times), as.vector(c("Transmitter", unlist(spatial$array.order)[-1])))
  expect_equal(sum(grepl("R64K-4451", row.names(times))), 10)
  expect_equal(as.vector(table(times$Transmitter)), c(10, 5))
  expect_equal(times$River1[1], moves[[1]]$Last.time[1])
  # try second tag other section
  expect_equal(times$Fjord1[13], moves[[2]]$Last.time[9])

  times <- getTimes(movements = moves, spatial = spatial, type = "departure", events = "first")
  expect_equal(colnames(times), as.vector(c("Transmitter", unlist(spatial$array.order)[-1])))
  expect_equal(sum(grepl("R64K-4451", row.names(times))), 1)
  expect_equal(as.vector(table(times$Transmitter)), c(1, 1))
  expect_equal(times$River1[2], moves[[2]]$Last.time[1])
  expect_equal(times$Fjord1[2], moves[[2]]$Last.time[7])
 	expect_false(any(is.na(times)))  
})

setwd("..")
unlink("exampleWorkspace", recursive = TRUE)
rm(list = ls())
