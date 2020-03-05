# ---- FORCE TWO RELEASE SITES

exampleWorkspace()
setwd("exampleWorkspace")

xbio <- example.biometrics
xbio$Release.site <- as.character(xbio$Release.site)
xbio$Release.site[c(1:15, 31:45)] <- "RS2"
write.csv(xbio, "biometrics.csv")

xspatial <- example.spatial
xspatial[19, ] <- xspatial[18, ]
xspatial$Station.name[19] <- "RS2"
xspatial$Array[19] <- "Fjord2"
write.csv(xspatial, "spatial.csv")

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
link <- match(bio$Transmitter[bio$Release.site == "RS2" & !is.na(bio$Transmitter)], names(xmoves))
for (i in link) {
	xmoves[[i]]$Valid[!grepl("Fjord2|Sea1", xmoves[[i]]$Array)] <- FALSE
}

vm <- lapply(xmoves, function(x) x[x$Valid, ])
vm <- vm[sapply(vm, nrow) != 0]

timetable <- assembleTimetable(vm = vm, all.moves = xmoves, sections = sections, 
  arrays = arrays, dist.mat = dist.mat, invalid.dist = invalid.dist, speed.method = "last to first", 
  if.last.skip.section = TRUE, success.arrays = "Sea1")

status.df <- assembleOutput(timetable = timetable, bio = bio, spatial = spatial, 
  sections = sections, dist.mat = dist.mat, invalid.dist = invalid.dist, tz = "Europe/Copenhagen")

the.matrices <- assembleMatrices(spatial = spatial, movements = vm, status.df = status.df,
    arrays = arrays, paths = paths, dotmat = dotmat)[[2]]

expect_warning(m.by.array <- breakMatricesByArray(m = the.matrices, arrays = arrays, type = "all"),
  "No fish passed through array River0. Skipping efficiency estimations for this array.", fixed = TRUE)

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

test_that("assembleArrayCJS can cope with midway releases", {
  overall.CJS <- assembleArrayCJS(mat = the.matrices, CJS = CJS.list, arrays = arrays, releases = release_nodes)
  expect_equal(overall.CJS$absolutes$Fjord2[4], 44)
})

test_that("assembleArrayCJS can cope with 0% efficiency", {
  xmatrices <- the.matrices
  xmatrices$A.RS1$River5 <- c(rep(0, 14), 1)
  xmatrices$B.RS1$River5 <- c(0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0)

  expect_warning(m.by.array <- breakMatricesByArray(m = xmatrices, arrays = arrays, type = "all"),
    "No fish passed through array River0. Skipping efficiency estimations for this array.", fixed = TRUE)

  CJS.list <- lapply(m.by.array, function(m) {
    if (length(m) == 1)
      simpleCJS(m[[1]])
    else
      combineCJS(m)
  })

  overall.CJS <- assembleArrayCJS(mat = xmatrices, CJS = CJS.list, arrays = arrays, releases = release_nodes)
  expect_equal(overall.CJS$absolutes$River5, c(3, 0, 25, 28))
})

overall.CJS <- assembleArrayCJS(mat = the.matrices, CJS = CJS.list, arrays = arrays, releases = release_nodes)

test_that("mbSplitCJS can deal with multiple release sites (one site per group)", {
  aux <- mbSplitCJS(mat = m.by.array, fixed.efficiency = overall.CJS$efficiency)
  ### ONLY RUN TO REPLACE REFERENCE
  # aux_mbSplitCJS_2R_2G <- aux
  # save(aux_mbSplitCJS_2R_2G, file = "../aux_mbSplitCJS_2R_2G.RData")
  load("../aux_mbSplitCJS_2R_2G.RData")
  expect_equal(aux, aux_mbSplitCJS_2R_2G)
})

test_that("mbGroupCJS can deal with multiple release sites (one site per group)", {
  aux <- mbGroupCJS(mat = m.by.array, status.df = status.df, fixed.efficiency = overall.CJS$efficiency)
  ### ONLY RUN TO REPLACE REFERENCE
  # aux_mbGroupCJS_2R_2G <- aux
  # save(aux_mbGroupCJS_2R_2G, file = "../aux_mbGroupCJS_2R_2G.RData")
  load("../aux_mbGroupCJS_2R_2G.RData")
  expect_equal(aux, aux_mbGroupCJS_2R_2G)
})

setwd("..")
unlink("exampleWorkspace", recursive = TRUE)
rm(list = ls())




# ---- FORCE TWO RELEASE SITES WITH ONE GROUP!

exampleWorkspace()
setwd("exampleWorkspace")

xbio <- example.biometrics
xbio$Release.site <- as.character(xbio$Release.site)
xbio$Release.site[c(1:15, 31:45)] <- "RS2"
xbio$Group <- "A"
write.csv(xbio, "biometrics.csv")

xspatial <- example.spatial
xspatial[19, ] <- xspatial[18, ]
xspatial$Station.name[19] <- "RS2"
xspatial$Array[19] <- "River6"
write.csv(xspatial, "spatial.csv")

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
link <- match(bio$Transmitter[bio$Release.site == "RS2" & !is.na(bio$Transmitter)], names(xmoves))
for (i in link) {
  xmoves[[i]]$Valid[!grepl("River6|Fjord1|Fjord2|Sea1", xmoves[[i]]$Array)] <- FALSE
}

vm <- lapply(xmoves, function(x) x[x$Valid, ])
vm <- vm[sapply(vm, nrow) != 0]

timetable <- assembleTimetable(vm = vm, all.moves = xmoves, sections = sections, 
  arrays = arrays, dist.mat = dist.mat, invalid.dist = invalid.dist, speed.method = "last to first", 
  if.last.skip.section = TRUE, success.arrays = "Sea1")

status.df <- assembleOutput(timetable = timetable, bio = bio, spatial = spatial, 
  sections = sections, dist.mat = dist.mat, invalid.dist = invalid.dist, tz = "Europe/Copenhagen")

the.matrices <- assembleMatrices(spatial = spatial, movements = vm, status.df = status.df,
    arrays = arrays, paths = paths, dotmat = dotmat)[[2]]

expect_warning(m.by.array <- breakMatricesByArray(m = the.matrices, arrays = arrays, type = "all"),
  "No fish passed through array River0. Skipping efficiency estimations for this array.", fixed = TRUE)

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

overall.CJS <- assembleArrayCJS(mat = the.matrices, CJS = CJS.list, arrays = arrays, releases = release_nodes)

test_that("mbSplitCJS can deal with multiple release sites (two sites, single group)", {
  aux <- mbSplitCJS(mat = m.by.array, fixed.efficiency = overall.CJS$efficiency)
  ### ONLY RUN TO REPLACE REFERENCE
  # aux_mbSplitCJS_2R_1G <- aux
  # save(aux_mbSplitCJS_2R_1G, file = "../aux_mbSplitCJS_2R_1G.RData")
  load("../aux_mbSplitCJS_2R_1G.RData")
  expect_equal(aux, aux_mbSplitCJS_2R_1G)
})

test_that("mbGroupCJS can deal with multiple release sites (two sites, single group)", {
  aux <- mbGroupCJS(mat = m.by.array, status.df = status.df, fixed.efficiency = overall.CJS$efficiency)
  ### ONLY RUN TO REPLACE REFERENCE
  # aux_mbGroupCJS_2R_1G <- aux
  # save(aux_mbGroupCJS_2R_1G, file = "../aux_mbGroupCJS_2R_1G.RData")
  load("../aux_mbGroupCJS_2R_1G.RData")
  expect_equal(aux, aux_mbGroupCJS_2R_1G)
})

setwd("..")
unlink("exampleWorkspace", recursive = TRUE)
rm(list = ls())


