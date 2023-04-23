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
detections.list <- study.data$detections.list
bio <- study.data$bio
spatial <- study.data$spatial
dist.mat <- study.data$dist.mat
dotmat <- study.data$dotmat
paths <- study.data$paths
arrays <- study.data$arrays
sections <- study.data$sections

moves <- groupMovements(detections.list = detections.list[1:2], bio = bio, spatial = spatial,
    speed.method = "last to first", max.interval = 60, tz = "Europe/Copenhagen", dist.mat = dist.mat)

aux <- names(moves)
moves <- lapply(names(moves), function(tag) {
    speedReleaseToFirst(tag = tag, bio = bio, movements = moves[[tag]],
                        dist.mat = dist.mat, speed.method = "last to first")
  })
names(moves) <- aux
rm(aux)

moves[[1]]$Array[10] <- "A1"
moves[[1]]$Valid[18] <- FALSE
attributes(moves[[1]])$p.type <- "Manual"

secmoves <- lapply(seq_along(moves), function(i) {
  tag <- names(moves)[i]
  appendTo("debug", paste0("debug: Compiling valid section movements for tag ", tag,"."))
  output <- sectionMovements(movements = moves[[i]], spatial = spatial, valid.dist = attributes(dist.mat)$valid)
  return(output)
})
names(secmoves) <- names(moves)

test_that("assembleResidency output is as expected", {

  res.df <<- assembleResidency(secmoves = secmoves, movements = moves, spatial = spatial)

	expect_equal(colnames(res.df), c('Times.entered.River', 'Average.entry.River', 'Average.time.River',
		'Average.departure.River', 'Total.time.River', 'Times.entered.Fjord', 'Average.entry.Fjord',
		'Average.time.Fjord', 'Average.departure.Fjord', 'Total.time.Fjord', 'Times.entered.Sea',
		'Average.entry.Sea', 'Average.time.Sea', 'Average.departure.Sea', 'Total.time.Sea', 'Very.last.array',
		'Very.last.time', 'Status', 'Valid.detections', 'Invalid.detections', 'Valid.events', 'Invalid.events',
		'P.type', 'Transmitter'))

	expect_equal(res.df$Times.entered.River, c(2, 1))
  expect_equal(res.df$Average.entry.River, c("16:56", "13:01"))
  expect_equal(as.numeric(res.df$Average.time.River), c(0.564, 1.545))
  expect_equal(attributes(res.df$Average.time.River)$units, "days")
  expect_equal(res.df$Average.departure.River, c("06:28", "02:06"))
  expect_equal(as.numeric(res.df$Total.time.River), c(1.128, 1.545))
  expect_equal(attributes(res.df$Total.time.River)$units, "days")
  expect_equal(res.df$Times.entered.Fjord, c(2, 1))
  expect_equal(res.df$Average.entry.Fjord, c("02:39", "04:05"))
  expect_equal(as.numeric(res.df$Average.time.Fjord), c(7.384, 10.753))
  expect_equal(attributes(res.df$Average.time.Fjord)$units, "days")
  expect_equal(res.df$Average.departure.Fjord, c("23:52", "22:10"))
  expect_equal(as.numeric(res.df$Total.time.Fjord), c(14.767, 10.753))
  expect_equal(attributes(res.df$Total.time.Fjord)$units, "days")
  expect_equal(res.df$Times.entered.Sea, c(0, 1))
  expect_equal(res.df$Average.entry.Sea, c(NA_character_, "22:22"))
  expect_equal(as.numeric(res.df$Average.time.Sea), c(NA_real_, 0.55))
  expect_equal(attributes(res.df$Average.time.Sea)$units, "mins")
  expect_equal(res.df$Average.departure.Sea, c(NA_character_, "22:23"))
  expect_equal(as.numeric(res.df$Total.time.Sea), c(NA_real_, 0.55))
  expect_equal(attributes(res.df$Total.time.Sea)$units, "mins")
  expect_equal(res.df$Very.last.array, c("A8", "A9"))
  expect_equal(res.df$Very.last.time, c("2018-05-06 02:48:43", "2018-05-02 22:22:52"))
  expect_equal(res.df$Status, c("Disap. in Fjord", "Disap. in Sea"))
  expect_equal(res.df$Valid.detections, c(538, 231))
  expect_equal(res.df$Invalid.detections, c(11, 0))
  expect_equal(res.df$Valid.events, c(17, 13))
  expect_equal(res.df$Invalid.events, c(1, 0))
  expect_equal(res.df$P.type,  c("Manual", "Auto"))
  expect_equal(res.df$Transmitter, c("R64K-4451", "R64K-4453"))
})

test_that("res_assembleOutput works as expected.", {
  status.df <<- res_assembleOutput(res.df = res.df, bio = bio, spatial = spatial, tz = "Europe/Copenhagen")

  expect_equal(colnames(status.df), c('Transmitter', 'Release.date', 'Release.site', 'Serial.nr',
  	'Signal', 'Group', 'Total.Length.mm', 'Mass.g', 'Times.entered.River', 'Average.entry.River',
  	'Average.time.River', 'Average.departure.River', 'Total.time.River', 'Times.entered.Fjord',
  	'Average.entry.Fjord', 'Average.time.Fjord', 'Average.departure.Fjord', 'Total.time.Fjord',
  	'Times.entered.Sea', 'Average.entry.Sea', 'Average.time.Sea', 'Average.departure.Sea',
  	'Total.time.Sea', 'Very.last.array', 'Very.last.time', 'Status', 'Valid.detections',
  	'Invalid.detections', 'Valid.events', 'Invalid.events', 'P.type'))

  expect_equal(class(status.df$Very.last.time), c("POSIXct", "POSIXt"))
  expect_false(any(is.na(status.df$Valid.detections)))
  expect_false(any(is.na(status.df$All.detections)))
  expect_false(any(is.na(status.df$Status)))
  expect_equal(levels(status.df$Very.last.array), c("Release", names(arrays)))
  expect_equal(levels(status.df$Status), c("Disap. in River", "Disap. in Fjord", "Disap. in Sea", "Disap. at Release"))
  expect_equal(unique(status.df$P.type), c("Manual", "Auto", "Skipped"))
})


test_that("getResidency works as expected", {
  residency.list <<- getResidency(movements = secmoves, spatial = spatial)
  expect_equal(names(residency.list), c("R64K-4451", "R64K-4453"))
  expect_equal(residency.list[[1]]$Section, c("River", "River-Fjord", "Fjord", "River-Fjord", "River", "River-Fjord", "Fjord"))
  expect_equal(residency.list[[2]]$Section, c("River", "River-Fjord", "Fjord", "Fjord-Sea", "Sea"))
  capture <- lapply(1:length(residency.list), function(i) {
	  for(j in 1:nrow(residency.list[[i]])) {
	  	if ((j / 2) %% 1 == 0) {
	  		expect_equal(residency.list[[i]]$First.time[j], secmoves[[i]]$Last.time[j / 2])
	  		expect_equal(residency.list[[i]]$Last.time[j], secmoves[[i]]$First.time[j / 2 + 1])
	  	} else {
	  		expect_equal(residency.list[[i]]$First.time[j], secmoves[[i]]$First.time[j / 2 + 0.5])
	  		expect_equal(residency.list[[i]]$Last.time[j], secmoves[[i]]$Last.time[j / 2 + 0.5])
	  	}
	  }
	})
})

test_that("resRatios works as expected", {
  time.ratios.day <<- resRatios(res = residency.list, tz = "Europe/Copenhagen", timestep = "days")

  ### ONLY RUN THIS TO RESET REFERENCE
  # aux_resRatios_day <- time.ratios.day
  # save(aux_resRatios_day, file = paste0(tests.home, "/aux_resRatios_day.RData"))

  load(paste0(tests.home, "/aux_resRatios_day.RData"))
  expect_equal(time.ratios.day, aux_resRatios_day)

  time.ratios.hour <<- resRatios(res = residency.list, tz = "Europe/Copenhagen", timestep = "hours")

  ### ONLY RUN THIS TO RESET REFERENCE
  # aux_resRatios_hour <- time.ratios.hour
  # save(aux_resRatios_hour, file = paste0(tests.home, "/aux_resRatios_hour.RData"))

  load(paste0(tests.home, "/aux_resRatios_hour.RData"))
  expect_equal(time.ratios.hour, aux_resRatios_hour)
})

test_that("resPositions works as expected.", {
  res.positions.day <<- resPositions(ratios = time.ratios.day, timestep = "days")

  expect_equal(as.character(res.positions.day$Timeslot), c('2018-04-18 01:00:00', '2018-04-19 01:00:00', '2018-04-20 01:00:00',
  	'2018-04-21 01:00:00', '2018-04-22 01:00:00', '2018-04-23 01:00:00', '2018-04-24 01:00:00', '2018-04-25 01:00:00', '2018-04-26 01:00:00',
  	'2018-04-27 01:00:00', '2018-04-28 01:00:00', '2018-04-29 01:00:00', '2018-04-30 01:00:00', '2018-05-01 01:00:00', '2018-05-02 01:00:00',
  	'2018-05-03 01:00:00', '2018-05-04 01:00:00', '2018-05-05 01:00:00', '2018-05-06 01:00:00'))
  expect_equal(as.vector(res.positions.day[, 2]), time.ratios.day[[1]]$Most.time)
  expect_equal(as.vector(res.positions.day[, 3]), c(NA, NA, time.ratios.day[[2]]$Most.time, NA, NA, NA, NA))

  res.positions.hour <<- resPositions(ratios = time.ratios.hour, timestep = "hours")

  expect_equal(as.vector(res.positions.hour[, 2]), time.ratios.hour[[1]]$Most.time)
  expect_equal(as.vector(res.positions.hour[, 3]), c(rep(NA, 39), time.ratios.hour[[2]]$Most.time, rep(NA, 76)))
})

test_that("globalRatios works as expected.", {
  global.ratios.day <- globalRatios(positions = res.positions.day, section.order = c("River", "Fjord", "Sea"))	

  ### ONLY RUN THIS TO RESET REFERENCE
  # aux_globalRatios.day <- global.ratios.day
  # save(aux_globalRatios.day, file = paste0(tests.home, "/aux_globalRatios_day.RData"))

  load(paste0(tests.home, "/aux_globalRatios_day.RData"))
  expect_equal(global.ratios.day, aux_globalRatios.day)

  global.ratios.hour <- globalRatios(positions = res.positions.hour, section.order = c("River", "Fjord", "Sea"))  

  ### ONLY RUN THIS TO RESET REFERENCE
  # aux_globalRatios.hour <- global.ratios.hour
  # save(aux_globalRatios.hour, file = paste0(tests.home, "/aux_globalRatios_hour.RData"))

  load(paste0(tests.home, "/aux_globalRatios_hour.RData"))
  expect_equal(global.ratios.hour, aux_globalRatios.hour)
})

test_that("res_efficiency works as expected, and can include intra array estimates", {
  efficiency <<- res_efficiency(arrmoves = moves, bio = bio, spatial = spatial, arrays = arrays, paths = paths, dotmat = dotmat)
  expect_equal(names(efficiency), c("absolutes", "max.efficiency", "min.efficiency",  "values.per.tag"))
  ### ONLY RUN THIS TO RESET REFERENCE
  # aux_res_efficiency <- efficiency
  # save(aux_res_efficiency, file = paste0(tests.home, "/aux_res_efficiency.RData"))
  load(paste0(tests.home, "/aux_res_efficiency.RData"))
  expect_equal(efficiency, aux_res_efficiency)

  tryCatch(x <- getDualMatrices(replicates = list(A9 = c("St.16")), CJS = efficiency, spatial = spatial, detections.list = detections.list),
    warning = function(w) stop("A warning was issued where it should not have been."))

  expect_warning(intra.array.matrices <- getDualMatrices(replicates = list(A7 = c("St.10", "St.11")), CJS = efficiency, spatial = spatial, detections.list = detections.list),
    "An inter-array efficiency has already been calculated for array A7", fixed = TRUE)

  output <- includeIntraArrayEstimates(m = intra.array.matrices, efficiency = efficiency, CJS = NULL)
  ### ONLY RUN THIS TO RESET REFERENCE
  # aux_includeIntraArrayEstimates <- output
  # save(aux_includeIntraArrayEstimates, file = paste0(tests.home, "/aux_includeIntraArrayEstimates.RData"))
  load(paste0(tests.home, "/aux_includeIntraArrayEstimates.RData"))
  expect_equal(output, aux_includeIntraArrayEstimates)

  output <- includeIntraArrayEstimates(m = list(), efficiency = efficiency, CJS = NULL)
  expect_equal(output$intra.CJS, NULL)
})
# y

test_that("advEfficiency can plot efficiency results", {
  output <- round(advEfficiency(efficiency), 7)
  check <- read.csv(text = '"","2.5%","50%","97.5%"
"A1.min", 1.0000000, 1.0, 1.0000000
"A1.max", 1.0000000, 1.0, 1.0000000
"A2.min", 0.0942993, 0.5, 0.9057007
"A2.max", 0.0942993, 0.5, 0.9057007
"A3.min", 0.0942993, 0.5, 0.9057007
"A3.max", 0.0942993, 0.5, 0.9057007
"A4.min", 0.0942993, 0.5, 0.9057007
"A4.max", 0.0942993, 0.5, 0.9057007
"A5.min", 0.0942993, 0.5, 0.9057007
"A5.max", 0.0942993, 0.5, 0.9057007
"A6.min", 0.0942993, 0.5, 0.9057007
"A6.max", 0.0942993, 0.5, 0.9057007
"A7.min", 1.0000000, 1.0, 1.0000000
"A7.max", 1.0000000, 1.0, 1.0000000
"A8.min", 1.0000000, 1.0, 1.0000000
"A8.max", 1.0000000, 1.0, 1.0000000
', row.names = 1)
  colnames(check) <- c("2.5%","50%","97.5%")
  expect_equal(output, check)

  output <- round(advEfficiency(efficiency, paired = FALSE), 7)
  check <- read.csv(text = '"","2.5%","50%","97.5%"
"A1.max", 1.0000000, 1.0, 1.0000000
"A2.max", 0.0942993, 0.5, 0.9057007
"A3.max", 0.0942993, 0.5, 0.9057007
"A4.max", 0.0942993, 0.5, 0.9057007
"A5.max", 0.0942993, 0.5, 0.9057007
"A6.max", 0.0942993, 0.5, 0.9057007
"A7.max", 1.0000000, 1.0, 1.0000000
"A8.max", 1.0000000, 1.0, 1.0000000
"A1.min", 1.0000000, 1.0, 1.0000000
"A2.min", 0.0942993, 0.5, 0.9057007
"A3.min", 0.0942993, 0.5, 0.9057007
"A4.min", 0.0942993, 0.5, 0.9057007
"A5.min", 0.0942993, 0.5, 0.9057007
"A6.min", 0.0942993, 0.5, 0.9057007
"A7.min", 1.0000000, 1.0, 1.0000000
"A8.min", 1.0000000, 1.0, 1.0000000
', row.names = 1)
  colnames(check) <- c("2.5%","50%","97.5%")
  expect_equal(output, check)
})

test_that("firstArrayFailure is able to deal with multile first expected arrays", {
  xdot <- loadDot(string =
"A0 -- A1 -- A0
A1 -- A2 -- A3 -- A6 -- A7 -- A9
A0 -- A4 -- A5 -- A6 -- A8 -- A9
A5 -- A3 -- A5
A7 -- A8 -- A7
", spatial = example.spatial, disregard.parallels = TRUE)

  xspatial <- spatial
  xspatial$release.sites$Array <- "A0|A1"

  first.array <- firstArrayFailure(tag = "R64K-4451", bio = bio, spatial = xspatial, first.array = "A5", paths = xdot$paths, dotmat = xdot$dotmat)
  expect_equal(first.array,  c(known1 = "A0", known2 = "A4"))

  first.array <- firstArrayFailure(tag = "R64K-4451", bio = bio, spatial = xspatial, first.array = "A6", paths = xdot$paths, dotmat = xdot$dotmat)
  expect_equal(first.array,  c(unsure1 = "A0", unsure2 = "A1", unsure3 = "A4", unsure4 = "A5", unsure5 = "A2", unsure6 = "A3"))

  first.array <- firstArrayFailure(tag = "R64K-4451", bio = bio, spatial = xspatial, first.array = "A7", paths = xdot$paths, dotmat = xdot$dotmat)
  expect_equal(first.array,  c(known = "A6", unsure1 = "A0", unsure2 = "A1", unsure3 = "A4", unsure4 = "A5", unsure5 = "A2", unsure6 = "A3"))

  first.array <- firstArrayFailure(tag = "R64K-4451", bio = bio, spatial = xspatial, first.array = "A9", paths = xdot$paths, dotmat = xdot$dotmat)
  expect_equal(first.array,  c(known = "A6", unsure1 = "A0", unsure2 = "A1", unsure3 = "A4", unsure4 = "A5", unsure5 = "A7", unsure6 = "A8", unsure7 = "A2", unsure8 = "A3"))
})

setwd("..")
unlink("exampleWorkspace", recursive = TRUE)
setwd(tests.home)

if (is.na(oldtz)) Sys.unsetenv("TZ") else Sys.setenv(TZ = oldtz)

rm(list = ls())
