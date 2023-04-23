skip_on_cran()
oldtz <- Sys.getenv('TZ', unset = NA)
Sys.setenv(TZ = 'UTC')

tests.home <- getwd()
setwd(tempdir())
exampleWorkspace("exampleWorkspace", force = TRUE)
setwd("exampleWorkspace")
write.csv(example.distances, "distances.csv")

study.data <- suppressWarnings(loadStudyData(tz = "Europe/Copenhagen", start.time = NULL,
	stop.time = NULL, section.order = c("River", "Fjord", "Sea"), exclude.tags = NULL))
detections.list <- study.data$detections.list
bio <- study.data$bio
spatial <- study.data$spatial
dist.mat <- study.data$dist.mat
arrays <- study.data$arrays
dotmat <- study.data$dotmat
paths <- study.data$paths

moves <- groupMovements(detections.list = detections.list, bio = bio, spatial = spatial,
    speed.method = "last to first", max.interval = 60, tz = "Europe/Copenhagen",
    dist.mat = dist.mat)

aux <- names(moves)
moves <- lapply(names(moves), function(tag) {
    speedReleaseToFirst(tag = tag, bio = bio, movements = moves[[tag]],
                        dist.mat = dist.mat, speed.method = "last to first")
  })
names(moves) <- aux
rm(aux)

xmoves <- moves
attributes(xmoves[[1]])$p.type <- "Manual"
xmoves[[1]]$Valid[18] <- FALSE
vm <- xmoves
vm[[1]] <- vm[[1]][-18, ]

secmoves <- lapply(seq_along(vm), function(i) {
  tag <- names(vm)[i]
  appendTo("debug", paste0("debug: Compiling valid section movements for tag ", tag,"."))
  output <- sectionMovements(movements = vm[[i]], spatial = spatial, valid.dist = attributes(dist.mat)$valid)
  return(output)
})
names(secmoves) <- names(vm)

timetable <- assembleTimetable(secmoves = secmoves, valid.moves = vm, all.moves = xmoves, spatial = spatial,
  arrays = arrays, dist.mat = dist.mat, speed.method = "last to first",
  if.last.skip.section = TRUE, success.arrays = "A9", bio = bio, tz = "Europe/Copenhagen")

status.df <- assembleOutput(timetable = timetable, bio = bio, spatial = spatial,
  dist.mat = dist.mat, tz = "Europe/Copenhagen")


test_that("assembleMatrices works as expected", {
	output <- assembleMatrices(spatial = spatial, movements = vm, status.df = status.df,
    arrays = arrays, paths = paths, dotmat = dotmat) # extract only the minimum matrix
	expect_equal(names(output), c("maxmat", "minmat"))
	expect_equal(names(output[[1]]), c("A.RS1", "B.RS1"))


	#### ONLY RUN THIS PART TO RESET REFERENCE TABLES
		# sink(paste0(tests.home, "/aux_assembleMatrices.R"))
		# cat("aux_assembleMatrixes <- list()\n")
		# capture <- lapply(1:2, function(i) {
		# 	lapply(1:2, function(j) {
		# 		cat(paste0("aux_assembleMatrixes$", names(output)[i], "$", names(output[[i]])[j],
		# 			"<- read.csv(text = ',", paste(colnames(output[[i]][[j]]), collapse = ","), "\n", paste(paste0(rownames(output[[i]][[j]]), ",",
		# 			apply(output[[i]][[j]], 1, function(k) paste(k, collapse = ","))), collapse = "\n"),
		# 			"', row.names = 1)\n"))
		# 	})
		# })
		# sink()
	
	source(paste0(tests.home, "/aux_assembleMatrices.R"))

	capture <- lapply(1:2, function(i) {
		lapply(1:2, function(j) {
			expect_equal(output[[i]][[j]], aux_assembleMatrixes[[i]][[j]])
		})
	})

	the.matrices <<- output[[2]]
})

test_that("breakMatricesByArray works as expected.", {
  expect_warning(output <- breakMatricesByArray(m = the.matrices, arrays = arrays, type = "peers"),
  	"No tags passed through array A0. Skipping efficiency estimations for this array.", fixed = TRUE)

  m.by.array <<- output

	#### ONLY RUN THIS PART TO RESET REFERENCE TABLES
		# sink(paste0(tests.home, "/aux_breakMatricesByArray.R"))
		# cat("aux_breakMatricesByArray <- list()\n")
		# capture <- lapply(1:2, function(i) {
		# 	lapply(1:2, function(j) {
		# 		cat(paste0("aux_breakMatricesByArray$", names(output)[i], "$", names(output[[i]])[j],
		# 			"<- read.csv(text = ',", paste(colnames(output[[i]][[j]]), collapse = ","), "\n", paste(paste0(rownames(output[[i]][[j]]), ",",
		# 			apply(output[[i]][[j]], 1, function(k) paste(k, collapse = ","))), collapse = "\n"),
		# 			"', row.names = 1)\n"))
		# 	})
		# })
		# sink()
	
	source(paste0(tests.home, "/aux_breakMatricesByArray.R"))

	capture <- lapply(1:2, function(i) {
		lapply(1:2, function(j) {
			expect_equal(output[[i]][[j]], aux_breakMatricesByArray[[i]][[j]])
		})
	})

  expect_warning(output <- breakMatricesByArray(m = the.matrices, arrays = arrays, type = "all"),
  	"No tags passed through array A0. Skipping efficiency estimations for this array.", fixed = TRUE)

  xmatrices <- the.matrices
  xmatrices[[1]][, "A9"] <- 0
	xmatrices[[2]][, "A9"] <- 0

	expect_warning(output <- breakMatricesByArray(m = xmatrices, arrays = arrays, type = "all"),
  	"No tags passed through any of the efficiency peers of array A8. Skipping efficiency estimations for this array.", fixed = TRUE)

  xarrays <- lapply(arrays, function(x) {
  	x$after.peers <- NULL
  	return(x)
  })

  expect_warning(output <- breakMatricesByArray(m = the.matrices, arrays = xarrays, type = "peers"),
  	"None of the arrays has valid efficiency peers.", fixed = TRUE)
})

test_that("simpleCJS works as expected.", {
	expect_error(simpleCJS("test"), "input must be a matrix or data frame containing only 0's and 1's.", fixed = TRUE)

	xm <- m.by.array[[1]][[1]]
	xm[1, 1] <- 2
	expect_error(simpleCJS(xm), "input must be a matrix or data frame containing only 0's and 1's.", fixed = TRUE)

	xm <- m.by.array[[1]][[1]]
	xm[1:5, 1] <- 0
	expect_error(simpleCJS(xm),
		"The first column of the input should only contain 1's (i.e. release point).", fixed = TRUE)

	expect_error(simpleCJS(m.by.array[[1]][[1]], estimate = 1, fixed.efficiency = 1),
		"Please choose only one of 'estimate' or 'fixed.efficiency'.", fixed = TRUE)

	expect_error(simpleCJS(m.by.array[[1]][[1]], estimate = 1:5),
		"Please use only one value for estimate.", fixed = TRUE)
	
	expect_error(simpleCJS(m.by.array[[1]][[1]], estimate = 2),
		"'estimate' must be between 0 and 1.", fixed = TRUE)
	

	expect_error(simpleCJS(m.by.array[[1]][[1]], fixed.efficiency = 1),
		"Fixed efficiency was set but its length is not the same as the number of columns in the input.", fixed = TRUE)

	expect_error(simpleCJS(m.by.array[[1]][[1]], fixed.efficiency = 1:3),
		"Fixed efficiency estimates must be between 0 and 1.", fixed = TRUE)
	
	expect_message(simpleCJS(m.by.array[[1]][[1]], fixed.efficiency = c(1,1,1), silent = FALSE),
		"M: Running CJS with fixed efficiency estimates.", fixed = TRUE)


	xm <- m.by.array[[1]][[1]]
	xm[, 3] <- 0
	expect_warning(simpleCJS(xm, silent = FALSE),
		"Array 'A1' had 0% efficiency. Skipping survival estimation.", fixed = TRUE)

	xm <- m.by.array[[1]][[1]]
	xm[, 2] <- 0
	expect_warning(simpleCJS(xm, silent = FALSE),
		"No tags were detected at array 'A1'. Skipping survival estimation.", fixed = TRUE)

	xm <- m.by.array[[1]][[1]]
	xm[1:20, 2] <- 0
	expect_warning(simpleCJS(xm, fixed.efficiency = c(1, 1, 1), silent = FALSE),
		"The fixed efficiency caused a too low estimate at iteration 2. Forcing higher estimate.", fixed = TRUE)

	xm <- m.by.array[[1]][[1]]
	output <- simpleCJS(xm, fixed.efficiency = c(1, 0.2, 1), silent = FALSE)
	expect_equal(output$absolutes["estimated", "A1"], 30)
	expect_equal(output$efficiency, c(FakeStart = 1.0, A1 = 0.2, AnyPeer = 1.0))

	output <- simpleCJS(xm, estimate = 0, silent = FALSE)
	expect_equal(output$absolutes["estimated", "AnyPeer"], 26)
	expect_equal(output$efficiency, c(FakeStart = 1, A1 = 1, AnyPeer = 0))

	output <- simpleCJS(xm, estimate = 0.2, silent = FALSE)
	expect_equal(output$absolutes["estimated", "AnyPeer"], 26)
	expect_equal(output$efficiency, c(FakeStart = 1.0, A1 = 1.0, AnyPeer = 0.2))
	expect_equal(output$survival[2], 1)

	output <- simpleCJS(m.by.array[[1]][[1]])
	expect_equal(names(output), c("absolutes", "efficiency", "survival", "lambda"))
	check <- read.csv(text = ',FakeStart,A1,AnyPeer
"detected",30,26,26
"here plus on peers",26,26,NA
"not here but on peers",0,0,NA
"known",30,26,26
"estimated",30,26,NA', row.names = 1)
	expect_equal(output$absolutes, as.matrix(check))
	expect_equal(output$efficiency, c(FakeStart = 1, A1 = 1, AnyPeer = NA))

	check <- as.matrix(read.csv(text = '"FakeStart -> A1      =",0.8666667
"       A1 -> AnyPeer =",NA', header = FALSE, row.names = 1))
	expect_equal(rownames(check), rownames(output$survival))
	expect_true(check[1] - output$survival[1] < 0.000000034)
	expect_true(is.na(output$survival[2]))
	expect_equal(output$lambda, 1)
})

test_that("combineCJS works as expected.", {
	expect_error(combineCJS(estimate = 1, fixed.efficiency = 1),
		"Please choose only one of 'estimate' or 'fixed.efficiency'.", fixed = TRUE)
	
	expect_error(combineCJS(estimate = 1:2),
		"Please use only one value for estimate.", fixed = TRUE)

	expect_error(combineCJS(estimate = 2),
		"'estimate' must be between 0 and 1.", fixed = TRUE)

	expect_error(combineCJS(fixed.efficiency = 2),
		"Fixed efficiency estimates must be between 0 and 1.", fixed = TRUE)

	expect_error(combineCJS(list(A = 1)),
		"Input appears to contain a list with only one element.", fixed = TRUE)

	expect_error(combineCJS("test"),
		"Only one object provided but it is not a list.", fixed = TRUE)

	expect_error(combineCJS(list(A = "a", B = "b")),
		"Not all objects provided are matrices or data frames. Please use either one list of matrices/data frames or multiple matrices/data frames.", fixed = TRUE)

	xm <- m.by.array[[1]]
	colnames(xm[[1]])[3] <- "test"

	expect_error(combineCJS(xm),
		"The last array is not the same in all input matrices.", fixed = TRUE)

	expect_error(combineCJS(m.by.array[[1]], fixed.efficiency = c(1, 1)),
		"Fixed efficiency was set but its length is not the same as the maximum number of columns in the input.", fixed = TRUE)

	expect_message(combineCJS(m.by.array[[1]], fixed.efficiency = c(1, 1, 1), silent = FALSE),
		"M: Running CJS with fixed efficiency values.", fixed = TRUE)

	output <- combineCJS(m.by.array[[1]])
	
	expect_equal(names(output), c("absolutes", "efficiency", "survival", "lambda"))
	check <- read.csv(text = ',FakeStart,A1,AnyPeer
"detected",60,54,54
"here plus on peers",54,54,NA
"not here but on peers",0,0,NA
"known",60,54,54
"estimated",60,54,NA', row.names = 1)
	expect_equal(output$absolutes, as.matrix(check))
	expect_equal(output$efficiency, c(FakeStart = 1, A1 = 1, AnyPeer = NA))

	check <- as.matrix(read.csv(text = '"FakeStart -> A1      =",0.9
"       A1 -> AnyPeer =",NA', header = FALSE, row.names = 1))
	colnames(check) <- ""
	expect_equal(check, output$survival)
})

# # -

test_that("assembleArrayCJS works as expected.",{
  CJS.list <- lapply(m.by.array, function(m) {
    if (length(m) == 1)
      simpleCJS(m[[1]])
    else
      combineCJS(m)
  })

  release_nodes <- as.data.frame(table(bio$Group, bio$Release.site))
  colnames(release_nodes) <- c("Group", "Release.site", "n")
  release_nodes$Array <- spatial$release.sites$Array[match(release_nodes$Release.site, spatial$release.sites$Standard.name)]
  release_nodes$Combined <- paste(release_nodes[, 1], release_nodes[, 2], sep = ".")
  release_nodes <<- release_nodes

  output <- assembleArrayCJS(mat = the.matrices, CJS = CJS.list, arrays = arrays, releases = release_nodes)

	check <- read.csv(text = ',A0,A1,A2,A3,A4,A5,A6,A7,A8,A9
"detected",0,54,54,52,52,52,52,49,44,34
"here plus on peers",NA,54,54,50,52,52,50,43,34,NA
"not here but on peers",NA,0,0,2,0,0,0,1,0,NA
"known",0,54,54,54,52,52,52,50,44,34
"estimated",NA,54,54,54,52,52,52,50,44,NA', row.names = 1)	

	expect_equal(output$absolutes, check)

	check <- c(A0 = NA, A1 = 1, A2 = 1, A3 = 0.96154, A4 = 1, A5 = 1, A6 = 1, A7 = 0.97727, A8 = 1, A9 = NA)
	expect_equal(round(output$efficiency, 5), check)

	overall.CJS <<- output
})

test_that("advEfficiency can plot overall.CJS results", {
	expect_message(output <- round(advEfficiency(x = overall.CJS), 7),
		"M: Some arrays were estimated to have either 0% or 100% efficiency, skipping plotting for those arrays.", fixed = TRUE)
	check <- read.csv(text = '"","2.5%","50%","97.5%"
"A1", 1.0000000, 1.0000000, 1.0000000
"A2", 1.0000000, 1.0000000, 1.0000000
"A3", 0.8955251, 0.9673092, 0.9952150
"A4", 1.0000000, 1.0000000, 1.0000000
"A5", 1.0000000, 1.0000000, 1.0000000
"A6", 1.0000000, 1.0000000, 1.0000000
"A7", 0.9177889, 0.9840095, 0.9994114
"A8", 1.0000000, 1.0000000, 1.0000000
', row.names = 1)
	colnames(check) <- c("2.5%","50%","97.5%")
	expect_equal(output, check)
})

test_that("getDualMatrices throws a warning if efficiency has already been calculated", {
	expect_warning(getDualMatrices(replicates = list(A7 = c("St.10", "St.11")), CJS = overall.CJS, spatial = spatial, detections.list = detections.list),
		"An inter-array efficiency has already been calculated for array A7", fixed = TRUE)
})
# n

test_that("includeIntraArrayEstimates throws errors if expected conditions are not met", {
  expect_error(includeIntraArrayEstimates(m = NULL, CJS = overall.CJS, efficiency  = "test"),
  	"Use only one of 'efficiency' or 'CJS' at a time.", fixed = TRUE)
})

test_that("replicate functions work as expected.", {
  intra.array.matrices <<- getDualMatrices(replicates = list(A9 = c("St.16", "St.17")), CJS = overall.CJS, spatial = spatial, detections.list = detections.list)

  check <- read.csv(text = '"","R1","R2"
"R64K-4451",TRUE,TRUE
"R64K-4453",FALSE,TRUE
"R64K-4454",FALSE,TRUE
"R64K-4456",TRUE,TRUE
"R64K-4457",FALSE,FALSE
"R64K-4459",FALSE,TRUE
"R64K-4462",TRUE,TRUE
"R64K-4465",TRUE,TRUE
"R64K-4466",FALSE,FALSE
"R64K-4469",TRUE,FALSE
"R64K-4470",FALSE,FALSE
"R64K-4472",FALSE,FALSE
"R64K-4473",TRUE,FALSE
"R64K-4474",TRUE,TRUE
"R64K-4477",TRUE,TRUE
"R64K-4480",TRUE,TRUE
"R64K-4481",TRUE,TRUE
"R64K-4484",TRUE,FALSE
"R64K-4486",FALSE,FALSE
"R64K-4488",FALSE,TRUE
"R64K-4490",TRUE,TRUE
"R64K-4492",FALSE,FALSE
"R64K-4494",TRUE,TRUE
"R64K-4496",TRUE,TRUE
"R64K-4498",TRUE,TRUE
"R64K-4499",TRUE,TRUE
"R64K-4502",TRUE,TRUE
"R64K-4503",FALSE,TRUE
"R64K-4505",FALSE,FALSE
"R64K-4508",TRUE,TRUE
"R64K-4509",FALSE,FALSE
"R64K-4510",TRUE,TRUE
"R64K-4511",TRUE,TRUE
"R64K-4512",FALSE,FALSE
"R64K-4513",FALSE,TRUE
"R64K-4514",TRUE,FALSE
"R64K-4515",FALSE,FALSE
"R64K-4516",TRUE,TRUE
"R64K-4517",TRUE,TRUE
"R64K-4518",FALSE,FALSE
"R64K-4519",TRUE,TRUE
"R64K-4521",FALSE,FALSE
"R64K-4522",FALSE,FALSE
"R64K-4524",FALSE,FALSE
"R64K-4526",TRUE,TRUE
"R64K-4529",FALSE,FALSE
"R64K-4532",TRUE,TRUE
"R64K-4534",FALSE,FALSE
"R64K-4536",FALSE,FALSE
"R64K-4541",FALSE,TRUE
"R64K-4543",FALSE,FALSE
"R64K-4545",TRUE,TRUE
"R64K-4547",TRUE,TRUE
"R64K-4549",FALSE,FALSE
', row.names = 1)
	expect_equal(intra.array.matrices[[1]], check)
	expect_equal(names(intra.array.matrices), "A9")

  recipient <- includeIntraArrayEstimates(m = intra.array.matrices, CJS = overall.CJS)
	expect_equal(names(recipient), c("CJS", "intra.CJS"))

	check <- read.csv(text = ',A0,A1,A2,A3,A4,A5,A6,A7,A8,A9
"detected",0,54,54,52,52,52,52,49,44,34
"here plus on peers",NA,54,54,50,52,52,50,43,34,NA
"not here but on peers",NA,0,0,2,0,0,0,1,0,NA
"known",0,54,54,54,52,52,52,50,44,34
"estimated",NA,54,54,54,52,52,52,50,44,35', row.names = 1)	
	expect_equal(recipient$CJS$absolutes, check)

	check <- c(A0 = NA, A1 = 1, A2 = 1, A3 = 0.96154, A4 = 1, A5 = 1, A6 = 1, A7 = 0.97727, A8 = 1, A9 = 0.96774)
	expect_equal(round(recipient$CJS$efficiency, 5), check)

	expect_equal(names(recipient$intra.CJS), "A9")

	expect_equal(names(recipient$intra.CJS$A9), c("absolutes", "single.efficiency", "combined.efficiency"))

	check <- as.matrix(read.csv(text = '"detected at R1: ",28
"detected at R2: ",31
"detected at both: ",24', header = FALSE, row.names = 1))
	colnames(check) <- ""
	expect_equal(recipient$intra.CJS$A9$absolutes, check)

	expect_equal(round(recipient$intra.CJS$A9$single.efficiency, 5), c(R1 = 0.77419, R2 = 0.85714))

	expect_equal(round(recipient$intra.CJS$A9$combined.efficiency, 5), 0.96774)

	overall.CJS <<- recipient[[1]]
	intra.array.CJS <<- recipient[[2]]
})

test_that("advEfficiency can plot intra.array.CJS results", {
	expect_message(output <- round(advEfficiency(intra.array.CJS[[1]]), 7),
		"M: For each quantile, 'Combined' estimates are calculated as 1-((1-R1)*(1-R2)).", fixed = TRUE)
	check <- read.csv(text = '"","2.5%","50%","97.5%"
"R1",       0.6143335, 0.7801434, 0.9006621
"R2",       0.7084131, 0.8656773, 0.9581126
"Combined", 0.8875447, 0.9704683, 0.9958390
', row.names = 1)
	colnames(check) <- c("2.5%","50%","97.5%")
	expect_equal(output, check)

	output <- advEfficiency(intra.array.CJS[[1]], labels = c(1, 2))
	expect_equal(row.names(output), c("1", "2", "Combined"))

	expect_error(advEfficiency(intra.array.CJS[[1]], labels = 1:3),
		"Wrong number of panel names", fixed = TRUE)

	output <- advEfficiency(intra.array.CJS[[1]], force.grid = c(2, 1), title = "Top/Bottom")

	expect_error(advEfficiency(x = 1),
		"Could not recognise the input as an efficiency object from actel", fixed = TRUE)

	expect_error(advEfficiency(x = list(a = 1)),
		"Could not recognise the input as an efficiency object from actel", fixed = TRUE)
})

test_that("split CJS functions work as expected.", {
  aux <- mbSplitCJS(mat = m.by.array, fixed.efficiency = overall.CJS$efficiency)
  ### ONLY RUN TO REPLACE REFERENCE
  # aux_mbSplitCJS <- aux
  # save(aux_mbSplitCJS, file = paste0(tests.home, "/aux_mbSplitCJS.RData"))
  load(paste0(tests.home, "/aux_mbSplitCJS.RData"))
  expect_equal(aux, aux_mbSplitCJS)

  xefficiency <- overall.CJS$efficiency
  xefficiency[4] <- NA
  output <- mbSplitCJS(mat = m.by.array, fixed.efficiency = xefficiency)
  expect_equal(round(output$A.RS1$A3$efficiency, 7), c(FakeStart = 1, A3 = 0.9615385, AnyPeer = NA))

  aux <- aux[names(the.matrices)]
  split.CJS <- assembleSplitCJS(mat = the.matrices, CJS = aux, arrays = arrays, releases = release_nodes, intra.CJS = intra.array.CJS)

  expect_equal(names(split.CJS), c("A.RS1", "B.RS1"))

  check <- read.csv(text = '"","A0","A1","A2","A3","A4","A5","A6","A7","A8","A9"
"detected",0,26,26,25,26,26,26,26,25,19
"here plus on peers",NA,26,26,25,26,26,26,25,19,NA
"not here but on peers",NA,0,0,1,0,0,0,0,0,NA
"known",0,26,26,26,26,26,26,26,25,19
"estimated",NA,26,26,26,26,26,26,26,25,20
"difference",NA,0,0,0,0,0,0,0,0,1
', row.names = 1)
  expect_equal(split.CJS[[1]], check)

  check <- read.csv(text = '"","A0","A1","A2","A3","A4","A5","A6","A7","A8","A9"
"detected",0,28,28,27,26,26,26,23,19,15
"here plus on peers",NA,28,28,25,26,26,24,18,15,NA
"not here but on peers",NA,0,0,1,0,0,0,1,0,NA
"known",0,28,28,28,26,26,26,24,19,15
"estimated",NA,28,28,28,26,26,26,24,19,16
"difference",NA,0,0,0,0,0,0,0,0,1
', row.names = 1)
  expect_equal(split.CJS[[2]], check)
})

test_that("group CJS functions work as expected.", {
  aux <- mbGroupCJS(mat = m.by.array, status.df = status.df, fixed.efficiency = overall.CJS$efficiency)
  ### ONLY RUN TO REPLACE REFERENCE
  # aux_mbGroupCJS <- aux
  # save(aux_mbGroupCJS, file = paste0(tests.home, "/aux_mbGroupCJS.RData"))
  load(paste0(tests.home, "/aux_mbGroupCJS.RData"))
  expect_equal(aux, aux_mbGroupCJS)

  xefficiency <- overall.CJS$efficiency
  xefficiency[4] <- NA
  output <- mbGroupCJS(mat = m.by.array, status.df = status.df, fixed.efficiency = xefficiency)
  expect_equal(round(output$A$A3$efficiency, 7), c(FakeStart = 1, A3 = 0.9615385, AnyPeer = NA))

  group.CJS <- assembleGroupCJS(mat = the.matrices, CJS = aux, arrays = arrays, releases = release_nodes, intra.CJS = intra.array.CJS)

  expect_equal(names(group.CJS), c("A", "B"))

  check <- read.csv(text = '"","A0","A1","A2","A3","A4","A5","A6","A7","A8","A9"
"detected",0,26,26,25,26,26,26,26,25,19
"here plus on peers",NA,26,26,25,26,26,26,25,19,NA
"not here but on peers",NA,0,0,1,0,0,0,0,0,NA
"known",0,26,26,26,26,26,26,26,25,19
"estimated",NA,26,26,26,26,26,26,26,25,20
"difference",NA,0,0,0,0,0,0,0,0,1
', row.names = 1)
  expect_equal(group.CJS[[1]], check)

  check <- read.csv(text = '"","A0","A1","A2","A3","A4","A5","A6","A7","A8","A9"
"detected",0,28,28,27,26,26,26,23,19,15
"here plus on peers",NA,28,28,25,26,26,24,18,15,NA
"not here but on peers",NA,0,0,1,0,0,0,1,0,NA
"known",0,28,28,28,26,26,26,24,19,15
"estimated",NA,28,28,28,26,26,26,24,19,16
"difference",NA,0,0,0,0,0,0,0,0,1
', row.names = 1)
  expect_equal(group.CJS[[2]], check)
})


test_that("special cases in oneWayMoves are working as expected", {
	expect_equal(oneWayMoves(moves[[1]][1, ], arrays), moves[[1]][1, ])
})


setwd("..")
unlink("exampleWorkspace", recursive = TRUE)
setwd(tests.home)

if (is.na(oldtz)) Sys.unsetenv("TZ") else Sys.setenv(TZ = oldtz)

rm(list = ls())
