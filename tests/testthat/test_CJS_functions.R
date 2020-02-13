exampleWorkspace()
setwd("exampleWorkspace")
write.csv(example.distances, "distances.csv")

sections <- c("River", "Fjord", "Sea")

study.data <- suppressWarnings(loadStudyData(tz = "Europe/Copenhagen", start.time = NULL, 
	stop.time = NULL, sections = sections, exclude.tags = NULL))
detections.list <- study.data$detections.list
bio <- study.data$bio
spatial <- study.data$spatial
dist.mat <- study.data$dist.mat
invalid.dist <- study.data$invalid.dist
arrays <- study.data$arrays
dotmat <- study.data$dotmat
paths <- study.data$paths

moves <- groupMovements(detections.list = detections.list, bio = bio, spatial = spatial,
    speed.method = "last to first", max.interval = 60, tz = "Europe/Copenhagen", 
    dist.mat = dist.mat, invalid.dist = invalid.dist)

aux <- names(moves)
moves <- lapply(names(moves), function(fish) {
    speedReleaseToFirst(fish = fish, bio = bio, movements = moves[[fish]],
                        dist.mat = dist.mat, invalid.dist = invalid.dist)
  })
names(moves) <- aux
rm(aux)


xmoves <- moves
attributes(xmoves[[1]])$p.type <- "Manual"
xmoves[[1]]$Valid[18] <- FALSE
vm <- xmoves
vm[[1]] <- vm[[1]][-18, ]

timetable <- assembleTimetable(vm = vm, all.moves = xmoves, sections = sections, 
  arrays = arrays, dist.mat = dist.mat, invalid.dist = invalid.dist, speed.method = "last to first", 
  if.last.skip.section = TRUE, success.arrays = "Sea1")

status.df <- assembleOutput(timetable = timetable, bio = bio, spatial = spatial, 
  sections = sections, dist.mat = dist.mat, invalid.dist = invalid.dist, tz = "Europe/Copenhagen")


test_that("assembleMatrices works as expected", {
	output <- assembleMatrices(spatial = spatial, movements = vm, status.df = status.df,
    arrays = arrays, paths = paths, dotmat = dotmat) # extract only the minimum matrix
	expect_equal(names(output), c("maxmat", "minmat"))
	expect_equal(names(output[[1]]), c("A.RS1", "B.RS1"))


	#### ONLY RUN THIS PART TO RESET REFERENCE TABLES
		# sink("../aux_assembleMatrixes.R")
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
	
	source("../aux_assembleMatrixes.R")

	capture <- lapply(1:2, function(i) {
		lapply(1:2, function(j) {
			rownames(aux_assembleMatrixes[[i]][[j]]) <- as.character(rownames(aux_assembleMatrixes[[i]][[j]]))
			expect_equal(output[[i]][[j]], aux_assembleMatrixes[[i]][[j]])
		})
	})

	the.matrices <<- output[[2]]
})

test_that("breakMatricesByArray works as expected.", {
  expect_warning(output <- breakMatricesByArray(m = the.matrices, arrays = arrays, type = "peers"),
  	"No fish passed through array River0. Skipping efficiency estimations for this array.", fixed = TRUE)

  m.by.array <<- output

	#### ONLY RUN THIS PART TO RESET REFERENCE TABLES
		# sink("../aux_breakMatricesByArray.R")
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
	
	source("../aux_breakMatricesByArray.R")

	capture <- lapply(1:2, function(i) {
		lapply(1:2, function(j) {
			rownames(aux_breakMatricesByArray[[i]][[j]]) <- as.character(rownames(aux_breakMatricesByArray[[i]][[j]]))
			expect_equal(output[[i]][[j]], aux_breakMatricesByArray[[i]][[j]])
		})
	})

  expect_warning(output <- breakMatricesByArray(m = the.matrices, arrays = arrays, type = "all"),
  	"No fish passed through array River0. Skipping efficiency estimations for this array.", fixed = TRUE)

  xarrays <- lapply(arrays, function(x) {
  	x$after.peers <- NULL
  	return(x)
  })

  expect_warning(output <- breakMatricesByArray(m = the.matrices, arrays = xarrays, type = "peers"),
  	"None of the arrays has valid efficiency peers.", fixed = TRUE)
})

test_that("simpleCJS works as expected.", {
	output <- simpleCJS(m.by.array[[1]][[1]])
	expect_equal(names(output), c("absolutes", "efficiency", "survival", "lambda"))
	check <- read.csv(text = ',FakeStart,River1,AnyPeer
"detected",30,26,26
"here plus downstream",26,26,NA
"not here but downstream",0, 0,NA
"estimated",30,26,NA', row.names = 1)
	expect_equal(output$absolutes, as.matrix(check))
	expect_equal(output$efficiency, c(FakeStart = 1, River1 = 1, AnyPeer = NA))

	check <- as.matrix(read.csv(text = '"FakeStart -> River1  =",0.8666667
"   River1 -> AnyPeer =",NA', header = FALSE, row.names = 1))
	expect_equal(rownames(check), rownames(output$survival))
	expect_true(check[1] - output$survival[1] < 0.000000034)
	expect_true(is.na(output$survival[2]))
	expect_equal(output$lambda, 1)
})

test_that("combineCJS works as expected.", {
	output <- combineCJS(m.by.array[[1]])
	expect_equal(names(output), c("absolutes", "efficiency", "survival", "lambda"))
	check <- read.csv(text = ',FakeStart,River1,AnyPeer
"detected",60,54,54
"here plus downstream",54,54,NA
"not here but downstream",0, 0,NA
"estimated",60,54,NA', row.names = 1)
	expect_equal(output$absolutes, as.matrix(check))
	expect_equal(output$efficiency, c(FakeStart = 1, River1 = 1, AnyPeer = NA))

	check <- as.matrix(read.csv(text = '"FakeStart -> River1  =",0.9
"   River1 -> AnyPeer =",NA', header = FALSE, row.names = 1))
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

  output <- assembleArrayCJS(mat = the.matrices, CJS = CJS.list, arrays = arrays, releases = release_nodes)

	check <- read.csv(text = ',River0,River1,River2,River3,River4,River5,River6,Fjord1,Fjord2,Sea1
"detected",0,54,54,52,52,52,52,49,44,34
"here plus on peers",NA,54,54,50,52,52,50,43,34,NA
"not here but on peers",NA,0,0,2,0,0,0,1,0,NA
"estimated",NA,54,54,54,52,52,52,50,44,NA', row.names = 1)	

	expect_equal(output$absolutes, check)

	check <- c(River0 = NA, River1 = 1, River2 = 1, River3 = 0.96154, River4 = 1, River5 = 1, River6 = 1, Fjord1 = 0.97727, Fjord2 = 1, Sea1 = NA)
	expect_equal(round(output$efficiency, 5), check)

	overall.CJS <<- output
})

test_that("replicate functions work as expected.", {
  intra.array.matrices <- getDualMatrices(replicates = list(Sea1 = c("St.16", "St.17")), CJS = overall.CJS, spatial = spatial, detections.list = detections.list)

  check <- read.csv(text = '"","original","replicates"
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
	expect_equal(names(intra.array.matrices), "Sea1")

  recipient <- includeIntraArrayEstimates(m = intra.array.matrices, CJS = overall.CJS)
	expect_equal(names(recipient), c("CJS", "intra.CJS"))

	check <- read.csv(text = ',River0,River1,River2,River3,River4,River5,River6,Fjord1,Fjord2,Sea1
"detected",0,54,54,52,52,52,52,49,44,34
"here plus on peers",NA,54,54,50,52,52,50,43,34,NA
"not here but on peers",NA,0,0,2,0,0,0,1,0,NA
"estimated",NA,54,54,54,52,52,52,50,44,35', row.names = 1)	
	expect_equal(recipient$CJS$absolutes, check)

	check <- c(River0 = NA, River1 = 1, River2 = 1, River3 = 0.96154, River4 = 1, River5 = 1, River6 = 1, Fjord1 = 0.97727, Fjord2 = 1, Sea1 = 0.96774)
	expect_equal(round(recipient$CJS$efficiency, 5), check)

	expect_equal(names(recipient$intra.CJS), "Sea1")

	expect_equal(names(recipient$intra.CJS$Sea1), c("absolutes", "single.efficiency", "combined.efficiency"))

	check <- as.matrix(read.csv(text = '"detected at original:",28
"detected at replicates: ",31
"detected at both:",24', header = FALSE, row.names = 1))
	colnames(check) <- ""
	expect_equal(recipient$intra.CJS$Sea1$absolutes, check)

	expect_equal(round(recipient$intra.CJS$Sea1$single.efficiency, 5), c(original = 0.77419, replicates = 0.85714))

	expect_equal(round(recipient$intra.CJS$Sea1$combined.efficiency, 5), 0.96774)
})


# test_that("split CJS functions work as expected.", {
#   aux <- mbSplitCJS(mat = m.by.array, fixed.efficiency = overall.CJS$efficiency)
#   aux <- aux[names(the.matrices)]
#   split.CJS <- assembleSplitCJS(mat = the.matrices, CJS = aux, arrays = arrays, releases = release_nodes, intra.CJS = intra.array.CJS)
#   aux <- mbAssembleArrayOverview(input = split.CJS)
# })

# test_that("group CJS functions work as expected.", {
#   aux <- mbGroupCJS(mat = m.by.array, status.df = status.df, fixed.efficiency = overall.CJS$efficiency)
#   group.CJS <- assembleGroupCJS(mat = the.matrices, CJS = aux, arrays = arrays, releases = release_nodes, intra.CJS = intra.array.CJS)
#   aux <- mbAssembleArrayOverview(input = group.CJS)
# })

setwd("..")
unlink("exampleWorkspace", recursive = TRUE)
rm(list = ls())
