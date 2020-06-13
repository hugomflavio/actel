skip_on_cran()

tests.home <- getwd()
setwd(tempdir())

exampleWorkspace("exampleWorkspace")
setwd("exampleWorkspace")
write.csv(example.distances, "distances.csv")
study.data <- suppressWarnings(loadStudyData(tz = "Europe/Copenhagen", start.time = NULL, 
	stop.time = NULL, sections = c("River", "Fjord", "Sea"), exclude.tags = NULL))
# n
detections.list <- study.data$detections.list
bio <- study.data$bio
spatial <- study.data$spatial
dist.mat <- study.data$dist.mat
dotmat <- study.data$dotmat
paths <- study.data$paths
invalid.dist <- study.data$invalid.dist
arrays <- study.data$arrays
sections <- study.data$sections

moves <- groupMovements(detections.list = detections.list[1:2], bio = bio, spatial = spatial,
    speed.method = "last to first", max.interval = 60, tz = "Europe/Copenhagen", 
    dist.mat = dist.mat, invalid.dist = invalid.dist)

aux <- names(moves)
moves <- lapply(names(moves), function(fish) {
    speedReleaseToFirst(fish = fish, bio = bio, movements = moves[[fish]],
                        dist.mat = dist.mat, invalid.dist = invalid.dist,
                        speed.method = "last to first")
  })
names(moves) <- aux
rm(aux)

moves[[1]]$Array[10] <- "River1"
moves[[1]]$Valid[18] <- FALSE
attributes(moves[[1]])$p.type <- "Manual"

secmoves <- lapply(seq_along(moves), function(i) {
  fish <- names(moves)[i]
  appendTo("debug", paste0("debug: Compiling valid section movements for fish ", fish,"."))
  output <- sectionMovements(movements = moves[[i]], sections = sections, invalid.dist = invalid.dist)
  return(output)
})
names(secmoves) <- names(moves)


test_that("assembleResidency output is as expected", {

  res.df <<- assembleResidency(secmoves = secmoves, movements = moves, sections = sections)

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
  expect_equal(res.df$Very.last.array, c("Fjord2", "Sea1"))
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
  status.df <<- res_assembleOutput(res.df = res.df, bio = bio, spatial = spatial, 
                                  sections = sections, tz = "Europe/Copenhagen")

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

test_that("dailyRatios works as expected", {
  daily.ratios <<- dailyRatios(res = residency.list)

  ### ONLY RUN THIS TO RESET REFERENCE
  # aux_dailyRatios <- daily.ratios
  # save(aux_dailyRatios, file = paste0(tests.home, "/aux_dailyRatios.RData"))

  load(paste0(tests.home, "/aux_dailyRatios.RData"))
  expect_equal(daily.ratios, aux_dailyRatios)
})

test_that("dailyPositions works as expected.", {
  daily.positions <<- dailyPositions(ratios = daily.ratios)

  expect_equal(rownames(daily.positions), c('2018-04-18', '2018-04-19', '2018-04-20', 
  	'2018-04-21', '2018-04-22', '2018-04-23', '2018-04-24', '2018-04-25', '2018-04-26', 
  	'2018-04-27', '2018-04-28', '2018-04-29', '2018-04-30', '2018-05-01', '2018-05-02', 
  	'2018-05-03', '2018-05-04', '2018-05-05', '2018-05-06'))
  expect_equal(as.vector(daily.positions[, 1]), daily.ratios[[1]]$Most.time)
  expect_equal(as.vector(daily.positions[, 2]), c(NA, NA, daily.ratios[[2]]$Most.time, NA, NA, NA, NA))
})

test_that("globalRatios works as expected.", {
  global.ratios <- globalRatios(positions = daily.positions)	

  ### ONLY RUN THIS TO RESET REFERENCE
  # aux_globalRatios <- global.ratios
  # save(aux_globalRatios, file = paste0(tests.home, "/aux_globalRatios.RData"))

  load(paste0(tests.home, "/aux_globalRatios.RData"))
  expect_equal(global.ratios, aux_globalRatios)
})

test_that("res_efficiency works as expected, and can include intra array estimates", {
  efficiency <<- res_efficiency(arrmoves = moves, bio = bio, spatial = spatial, arrays = arrays, paths = paths, dotmat = dotmat)
  expect_equal(names(efficiency), c("absolutes", "max.efficiency", "min.efficiency",  "values.per.fish"))
  ### ONLY RUN THIS TO RESET REFERENCE
  # aux_res_efficiency <- efficiency
  # save(aux_res_efficiency, file = paste0(tests.home, "/aux_res_efficiency.RData"))
  load(paste0(tests.home, "/aux_res_efficiency.RData"))
  expect_equal(efficiency, aux_res_efficiency)

  tryCatch(x <- getDualMatrices(replicates = list(Sea1 = c("St.16")), CJS = efficiency, spatial = spatial, detections.list = detections.list), 
    warning = function(w) stop("A warning was issued where it should not have been."))

  expect_warning(intra.array.matrices <- getDualMatrices(replicates = list(Fjord1 = c("St.10", "St.11")), CJS = efficiency, spatial = spatial, detections.list = detections.list),
    "An inter-array efficiency has already been calculated for array Fjord1", fixed = TRUE)
  
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
"River1.min", 1.0000000, 1.0, 1.0000000
"River1.max", 1.0000000, 1.0, 1.0000000
"River2.min", 0.0942993, 0.5, 0.9057007
"River2.max", 0.0942993, 0.5, 0.9057007
"River3.min", 0.0942993, 0.5, 0.9057007
"River3.max", 0.0942993, 0.5, 0.9057007
"River4.min", 0.0942993, 0.5, 0.9057007
"River4.max", 0.0942993, 0.5, 0.9057007
"River5.min", 0.0942993, 0.5, 0.9057007
"River5.max", 0.0942993, 0.5, 0.9057007
"River6.min", 0.0942993, 0.5, 0.9057007
"River6.max", 0.0942993, 0.5, 0.9057007
"Fjord1.min", 1.0000000, 1.0, 1.0000000
"Fjord1.max", 1.0000000, 1.0, 1.0000000
"Fjord2.min", 1.0000000, 1.0, 1.0000000
"Fjord2.max", 1.0000000, 1.0, 1.0000000
', row.names = 1)
  colnames(check) <- c("2.5%","50%","97.5%")
  expect_equal(output, check)

  output <- round(advEfficiency(efficiency, paired = FALSE), 7)
  check <- read.csv(text = '"","2.5%","50%","97.5%"
"River1.max", 1.0000000, 1.0, 1.0000000
"River2.max", 0.0942993, 0.5, 0.9057007
"River3.max", 0.0942993, 0.5, 0.9057007
"River4.max", 0.0942993, 0.5, 0.9057007
"River5.max", 0.0942993, 0.5, 0.9057007
"River6.max", 0.0942993, 0.5, 0.9057007
"Fjord1.max", 1.0000000, 1.0, 1.0000000
"Fjord2.max", 1.0000000, 1.0, 1.0000000
"River1.min", 1.0000000, 1.0, 1.0000000
"River2.min", 0.0942993, 0.5, 0.9057007
"River3.min", 0.0942993, 0.5, 0.9057007
"River4.min", 0.0942993, 0.5, 0.9057007
"River5.min", 0.0942993, 0.5, 0.9057007
"River6.min", 0.0942993, 0.5, 0.9057007
"Fjord1.min", 1.0000000, 1.0, 1.0000000
"Fjord2.min", 1.0000000, 1.0, 1.0000000
', row.names = 1)
  colnames(check) <- c("2.5%","50%","97.5%")
  expect_equal(output, check)
})

test_that("firstArrayFailure is able to deal with multile first expected arrays", {
  xdot <- loadDot(string = 
"River0 -- River1 -- River0
River1 -- River2 -- River3 -- River6 -- Fjord1 -- Sea1
River0 -- River4 -- River5 -- River6 -- Fjord2 -- Sea1
River5 -- River3 -- River5
Fjord1 -- Fjord2 -- Fjord1
", spatial = spatial, disregard.parallels = TRUE, sections = c("River", "Fjord", "Sea"))

  xspatial <- spatial
  xspatial$release.sites$Array <- "River0|River1"

  first.array <- firstArrayFailure(fish = "R64K-4451", bio = bio, spatial = xspatial, first.array = "River5", paths = xdot$paths, dotmat = xdot$dotmat)
  expect_equal(first.array,  c(known1 = "River0", known2 = "River4"))

  first.array <- firstArrayFailure(fish = "R64K-4451", bio = bio, spatial = xspatial, first.array = "River6", paths = xdot$paths, dotmat = xdot$dotmat)
  expect_equal(first.array,  c(unsure1 = "River0", unsure2 = "River1", unsure3 = "River4", unsure4 = "River5", unsure5 = "River2", unsure6 = "River3"))

  first.array <- firstArrayFailure(fish = "R64K-4451", bio = bio, spatial = xspatial, first.array = "Fjord1", paths = xdot$paths, dotmat = xdot$dotmat)
  expect_equal(first.array,  c(known = "River6", unsure1 = "River0", unsure2 = "River1", unsure3 = "River4", unsure4 = "River5", unsure5 = "River2", unsure6 = "River3"))  

  first.array <- firstArrayFailure(fish = "R64K-4451", bio = bio, spatial = xspatial, first.array = "Sea1", paths = xdot$paths, dotmat = xdot$dotmat)
  expect_equal(first.array,  c(known = "River6", unsure1 = "River0", unsure2 = "River1", unsure3 = "River4", unsure4 = "River5", unsure5 = "Fjord1", unsure6 = "Fjord2", unsure7 = "River2", unsure8 = "River3"))  
})

setwd("..")
unlink("exampleWorkspace", recursive = TRUE)
setwd(tests.home)
rm(list = ls())
