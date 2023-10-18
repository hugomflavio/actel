#' Migration Analysis
#'
#' The \code{migration} analysis runs the same initial checks as \code{explore},
#' but on top of it, it analyses the animal behaviour. By selecting the arrays
#' that lead to success, you can define whether or not your animals survived the
#' migration. Additional plots help you find out if some animal/tag has been acting
#' odd. Multiple options allow you to tweak the analysis to fit your study
#' perfectly.
#'
#' @param section.order A vector containing the order by which sections should 
#' be aligned in the results.
#' @param success.arrays The arrays that mark the end of the study area. If a
#'  tag crosses one of these arrays, the respective animal is considered to have 
#'  successfully migrated through the study area.
#' @param if.last.skip.section Logical: Should a tag detected at the last array
#'  of a given section be considered to have disappeared in the next section?
#' @param disregard.parallels Logical:  Should the presence of parallel arrays
#'  invalidate potential efficiency peers? See the vignettes for more details.
#' @param replicates A list containing, for each array to which intra-array
#'  efficiency is to be calculated: The standard names of the stations to be
#'  used as a replicate. See the vignettes for more details.
#' @inheritParams explore
#'
#' @examples
#' \donttest{
#' # Start by moving to a temporary directory
#' old.wd <- getwd()
#' setwd(tempdir())
#'
#' # Deploy the example workspace
#' exampleWorkspace("migration_example")
#'
#' # Move your R session into the example workspace
#' setwd("migration_example")
#'
#' # run the migration analysis. Ensure the tz argument
#' # matches the time zone of the study area and that the
#' # sections match your array names. The line below works
#' # for the example data.
#' results <- migration(tz = "Europe/Copenhagen")
#'
#' # to obtain an HTML report, run the analysis with report = TRUE
#'
#' # return to original working directory
#' setwd(old.wd)
#' rm(old.wd)
#' }
#'
#' @return A list containing:
#' \itemize{
#'  \item \code{detections}: A list containing all detections for each target tag;
#'  \item \code{valid.detections}: A list containing the valid detections for each target tag;
#'  \item \code{spatial}: A list containing the spatial information used during the analysis;
#'  \item \code{deployments}: A data frame containing the deployments of each receiver;
#'  \item \code{arrays}: A list containing the array details used during the analysis;
#'  \item \code{movements}: A list containing all movement events for each target tag;
#'  \item \code{valid.movements}: A list containing the valid movement events for each target tag;
#'  \item \code{section.movements}: A list containing the valid section shifts for each target tag;
#'  \item \code{status.df}: A data.frame containing summary information for each tag, including the
#'   following columns:
#'    \itemize{
#'      \item \emph{Times.entered.\[section\]}: Number of times the tag was recorded
#'        entering a given section.
#'      \item \emph{Average.time.until.\[section\]}: Time spent between release
#'        or leaving another section and reaching at the given section.
#'      \item \emph{Average.speed.to.\[section\]}: Average speed from release or leaving
#'        one section and reaching the given section (if speed.method = "last to first"),
#'        or from release/leaving one section and leaving the given section (if speed.method
#'        = "last to last").
#'      \item \emph{First.array.\[section\]}: Array in which the tag was
#'        first detected in a given section
#'      \item \emph{First.station.\[section\]}: Standard name of the first station
#'        where the tag was detected in a given section
#'      \item \emph{First.arrived.\[section\]}: Very first arrival time at a given section
#'      \item \emph{Average.time.in.\[section\]}: Average time spent within a given section
#'        at each stay.
#'      \item \emph{Average.speed.in.\[section\]}: Average speed within a given section
#'        at each stay (only displayed if speed.method = "last to first").
#'      \item \emph{Last.array.\[section\]}: Array in which the tag was
#'        last detected in a given section
#'      \item \emph{Last.station.\[section\]}: Standard name of the last station
#'        where the tag was detected in a given section
#'      \item \emph{Last.left.\[section\]}: Very last departure time from a given section
#'      \item \emph{Total.time.in\[section\]}: Total time spent in a given section
#'      \item \emph{Very.last.array}: Last array where the tag was detected
#'      \item \emph{Status}: Fate assigned to the tag
#'      \item \emph{Valid.detections}: Number of valid detections
#'      \item \emph{Invalid.detections}: Number of invalid detections
#'      \item \emph{Backwards.movements}: Number of backward movement events
#'      \item \emph{Max.cons.back.moves}: Longest successive backwards movements
#'      \item \emph{P.type}: Type of processing:
#'        \itemize{
#'          \item 'Skipped' if no data was found for the tag,
#'          \item 'Auto' if no user interaction was required,
#'          \item 'Manual' if user interaction was suggested and the user made
#'            changes to the validity of the events,
#'          \item 'Overridden' if the user listed the tag in the
#'            \code{override} argument.
#'        }
#'      \item \emph{Comments}: Comments left by the user during the analysis
#'    }
#'  \item \code{section.overview}: A data frame containing the number of tags that
#'    disappeared in each section;
#'  \item \code{group.overview}: A list containing the number of known and
#'    estimated tags to have passed through each array, divided by group;
#'  \item \code{release.overview}: A list containing the number of known and
#'    estimated tags to have passed through each array, divided by group and release sites;
#'  \item \code{matrices}: A list of CJS matrices used for the efficiency calculations;
#'  \item \code{overall.CJS}: A list of CJS results of the inter-array CJS calculations;
#'  \item \code{intra.array.CJS}: A list of CJS results of the intra-array CJS calculations;
#'  \item \code{times}: A data frame containing all arrival times (per tag) at each array;
#'  \item \code{rsp.info}: A list containing appendix information for the RSP package;
#'  \item \code{dist.mat}: The distance matrix used in the analysis (if a valid
#'   distance matrix was supplied)
#' }
#'
#' @seealso \code{\link{explore}}, \code{\link{residency}}
#'
#' @export
#'
migration <- function(
  tz = NULL,
  section.order = NULL,
  datapack = NULL,
  success.arrays = NULL,
  max.interval = 60,
  minimum.detections,
  min.total.detections = 2,
  min.per.event = 1,
  start.time = NULL,
  stop.time = NULL,
  speed.method = c("last to first", "last to last"),
  speed.warning = NULL,
  speed.error = NULL,
  jump.warning = 2,
  jump.error = 3,
  inactive.warning = NULL,
  inactive.error = NULL,
  exclude.tags = NULL,
  override = NULL,
  report = FALSE,
  auto.open = TRUE,
  discard.orphans = FALSE,
  discard.first = NULL,
  save.detections = FALSE,
  if.last.skip.section = TRUE,
  replicates = NULL,
  disregard.parallels = TRUE,
  GUI = c("needed", "always", "never"),
  save.tables.locally = FALSE,
  print.releases = TRUE,
  detections.y.axis = c("auto", "stations", "arrays"))
{

# check deprecated argument
  if (!missing(minimum.detections))
    stop("'minimum.detections' has been deprecated. Please use 'min.total.detections' and 'min.per.event' instead.", call. = FALSE)

# clean up any lost helpers
  deleteHelpers()
  if (file.exists(paste0(tempdir(), "/actel_debug_file.txt")))
    file.remove(paste0(tempdir(), "/actel_debug_file.txt"))
# ------------------------

# debug lines
  if (getOption("actel.debug", default = FALSE)) { # nocov start
    on.exit(message("Debug: Progress log available at ", gsub("\\\\", "/", paste0(tempdir(), "/actel_debug_file.txt"))))
    on.exit(message("Debug: Saving carbon copy to ", gsub("\\\\", "/", paste0(tempdir(), "/actel.debug.RData"))), add = TRUE)
    on.exit(save(list = ls(), file = paste0(tempdir(), "/actel.debug.RData")), add = TRUE)
    message("!!!--- Debug mode has been activated ---!!!")
  } # nocov end
# ------------------------

# check arguments quality
  if (!is.null(datapack)) {
    checkToken(token = attributes(datapack)$actel.token,
               timestamp = attributes(datapack)$timestamp)
  }

  aux <- checkArguments(dp = datapack,
                        tz = tz,
                        min.total.detections = min.total.detections,
                        min.per.event = min.per.event,
                        max.interval = max.interval,
                        speed.method = speed.method,
                        speed.warning = speed.warning,
                        speed.error = speed.error,
                        start.time = start.time,
                        stop.time = stop.time,
                        report = report,
                        auto.open = auto.open,
                        save.detections = save.detections,
                        jump.warning = jump.warning,
                        jump.error = jump.error,
                        inactive.warning = inactive.warning,
                        inactive.error = inactive.error,
                        exclude.tags = exclude.tags,
                        override = override,
                        print.releases = print.releases,
                        if.last.skip.section = if.last.skip.section,
                        replicates = replicates,
                        section.order = section.order,
                        detections.y.axis = detections.y.axis)

  min.per.event <- aux$min.per.event
  speed.method <- aux$speed.method
  speed.warning <- aux$speed.warning
  speed.error <- aux$speed.error
  jump.warning <- aux$jump.warning
  jump.error <- aux$jump.error
  inactive.warning <- aux$inactive.warning
  inactive.error <- aux$inactive.error
  detections.y.axis <- aux$detections.y.axis
  rm(aux)

  GUI <- checkGUI(GUI, save.tables.locally)
# ------------------------

# Store function call
  the.function.call <- paste0("migration(tz = ", ifelse(is.null(tz), "NULL", paste0("'", tz, "'")),
    ", section.order = ", ifelse(is.null(section.order), "NULL", paste0("c('", paste(section.order, collapse = "', '"), "')")),
    ", datapack = ", ifelse(is.null(datapack), "NULL", deparse(substitute(datapack))),
    ", success.arrays = ", ifelse(is.null(success.arrays), "NULL", paste0("c('", paste(success.arrays, collapse = "', '"), "')")),
    ", max.interval = ", max.interval,
    ", min.total.detections = ", min.total.detections,
    ", min.per.event = ", paste0("c(", paste(min.per.event, collapse = ", "), ")"),
    ", start.time = ", ifelse(is.null(start.time), "NULL", paste0("'", start.time, "'")),
    ", stop.time = ", ifelse(is.null(stop.time), "NULL", paste0("'", stop.time, "'")),
    ", speed.method = '", speed.method, "'",
    ", speed.warning = ", ifelse(is.null(speed.warning), "NULL", speed.warning),
    ", speed.error = ", ifelse(is.null(speed.error), "NULL", speed.error),
    ", jump.warning = ", jump.warning,
    ", jump.error = ", jump.error,
    ", inactive.warning = ", ifelse(is.null(inactive.warning), "NULL", inactive.warning),
    ", inactive.error = ", ifelse(is.null(inactive.error), "NULL", inactive.error),
    ", exclude.tags = ", ifelse(is.null(exclude.tags), "NULL", paste0("c('", paste(exclude.tags, collapse = "', '"), "')")),
    ", override = ", ifelse(is.null(override), "NULL", paste0("c(", paste(override, collapse = ", "), ")")),
    ", report = ", ifelse(report, "TRUE", "FALSE"),
    ", auto.open = ", ifelse(auto.open, "TRUE", "FALSE"),
    ", discard.orphans = ", ifelse(discard.orphans, "TRUE", "FALSE"),
    ", discard.first = ", ifelse(is.null(discard.first), "NULL", discard.first),
    ", save.detections = ", ifelse(save.detections, "TRUE", "FALSE"),
    ", if.last.skip.section = ", ifelse(if.last.skip.section, "TRUE", "FALSE"),
    ", replicates = ", ifelse(is.null(replicates),"NULL", paste0("list(", paste(sapply(1:length(replicates), function(i) paste0("'", names(replicates)[i], "' = c('", paste(replicates[[i]], collapse = "', '"), "')")), collapse = ", "), ")")),
    ", disregard.parallels = ", ifelse(disregard.parallels, "TRUE", "FALSE"),
    ", GUI = '", GUI, "'",
    ", save.tables.locally = ", ifelse(save.tables.locally, "TRUE", "FALSE"),
    ", print.releases = ", ifelse(print.releases, "TRUE", "FALSE"),
    ", detections.y.axis = '", detections.y.axis, "'",
    ")")

  appendTo("debug", the.function.call)
# --------------------

# Prepare clean-up before function ends
  finished.unexpectedly <- TRUE
  on.exit({if (interactive() & finished.unexpectedly) emergencyBreak(the.function.call)}, add = TRUE)

  if (!getOption("actel.debug", default = FALSE))
    on.exit(deleteHelpers(), add = TRUE)

  on.exit(tryCatch(sink(), warning = function(w) {hide <- NA}), add = TRUE)
# --------------------------------------

# Final arrangements before beginning
  appendTo("Report", paste0("Actel R package report.\nVersion: ", utils::packageVersion("actel"), "\n"))

  appendTo(c("Report"), paste0("Target folder: ", getwd(), "\nTimestamp: ", the.time <- Sys.time(), "\nFunction: migration()\n"))

  report <- checkReport(report = report)
# -----------------------------------

# Load, structure and check the inputs
  if (is.null(datapack)) {
    study.data <- loadStudyData(tz = tz, override = override, save.detections = save.detections,
                                start.time = start.time, stop.time = stop.time, discard.orphans = discard.orphans,
                                section.order = section.order, exclude.tags = exclude.tags, disregard.parallels = disregard.parallels)
  } else {
    appendTo(c("Screen", "Report"), paste0("M: Running analysis on preloaded data (compiled on ", attributes(datapack)$timestamp, ")."))
    appendTo("Report", paste0("Messages displayed during preload:\n-------------------\n", paste0(attributes(datapack)$loading_messages, collapse = "\n"), "\n-------------------"))
    study.data <- datapack
    tz <- study.data$tz
    disregard.parallels <- study.data$disregard.parallels
  }

  bio <- study.data$bio
  deployments <- study.data$deployments
  spatial <- study.data$spatial
  dot <- study.data$dot
  arrays <- study.data$arrays
  dotmat <- study.data$dotmat
  paths <- study.data$paths
  dist.mat <- study.data$dist.mat
  attributes(dist.mat)$speed.method <- speed.method
  detections.list <- study.data$detections.list

# -------------------------------------

# Final quality checks
  # Verify the existance of sections
  if (all(!grepl("^Section$", colnames(spatial$stations))))
    stopAndReport("To run migration(), please assign the arrays to their sections using a 'Section' column in the spatial input.")

  # Verify that replicate information is valid
  if (!is.null(replicates) && any(is.na(match(names(replicates), names(arrays)))))
    stopAndReport("Some of the array names listed in the 'replicates' argument do not match the study's arrays.")

  capture <- lapply(names(replicates), function(i) {
    x <- replicates[[i]]
    all.stations <- spatial$stations$Standard.name[spatial$stations$Array == i]
    if (any(link <- !x %in% all.stations)) {
      stopAndReport("In replicates: Station", 
                  ifelse(sum(link) > 1, "s ", " "), 
                  paste(x[link], collapse = ", "), 
                  ifelse(sum(link) > 1, " are", " is"), 
                  " not part of ", i, " (available stations: ", 
                  paste(all.stations, collapse = ", "), ").")
    }
  })

  if (any(!sapply(arrays, function(x) is.null(x$parallel)))) {
    if (disregard.parallels)
      appendTo(c("Screen", "Report"), "M: 'disregard.parallels' is set to TRUE; the presence of parallel arrays will not invalidate efficiency peers.")
    else
      appendTo(c("Screen", "Report"), "M: 'disregard.parallels' is set to FALSE; the presence of parallel arrays can potentially invalidate efficiency peers.")
  }

  if (is.null(success.arrays)) {
    success.arrays <- names(arrays)[unlist(lapply(arrays, function(x) is.null(x$after)))]
    if (length(success.arrays) == 1)
      appendTo(c("Screen", "Warning", "Report"), paste0("'success.arrays' was not defined. Assuming success if the tags are last detected at array ", success.arrays, "."))
    else
      appendTo(c("Screen", "Warning", "Report"), paste0("'success.arrays' was not defined. Assuming success if the tags are last detected at arrays ", paste(success.arrays[-length(success.arrays)], collapse = ", "), " or ", tail(success.arrays, 1), "."))
  } else {
    if (any(link <- is.na(match(success.arrays, unlist(spatial$array.order))))) {
      stopAndReport(ifelse(sum(link) > 1, "Arrays '", "Array '"), paste(success.arrays[link], collapse = "', '"),
        ifelse(sum(link) > 1, "' are ", "' is "), "listed in the 'success.arrays' argument, but ",
      ifelse(sum(link) > 1, "these arrays are", "this array is"), " not part of the study arrays.")
    }
  }

  # CHECK ISSUE 79
  checkIssue79(arrays, spatial)

# -------------------------------------

# Discard early detections, if required
  if (!is.null(discard.first) && discard.first > 0)
    detections.list <- discardFirst(input = detections.list, bio, trim = discard.first)

# Compile array movements
  appendTo(c("Screen", "Report"), "M: Creating movement records for the valid tags.")
  movements <- groupMovements(detections.list = detections.list, bio = bio, spatial = spatial,
                              speed.method = speed.method, max.interval = max.interval, tz = tz,
                              dist.mat = dist.mat)

  if (is.null(discard.first)) {
    aux <- names(movements)
    movements <- lapply(names(movements), function(tag) {
        speedReleaseToFirst(tag = tag, bio = bio, movements = movements[[tag]],
                            dist.mat = dist.mat, speed.method = speed.method)
      })
    names(movements) <- aux
    rm(aux)
  } else {
    appendTo(c("Screen", "Report"), "M: Not calculating time/speed from release to first detection because 'discard.first' was set.")
  }

  appendTo(c("Screen", "Report"), "M: Checking movement events quality.")

  do.checkSpeeds <- FALSE
  if (is.null(speed.warning)) {
    appendTo(c("Screen", "Report", "Warning"), "'speed.warning'/'speed.error' were not set, skipping speed checks.")
  } else {
    if(attributes(dist.mat)$valid)
      do.checkSpeeds <- TRUE
    else
      appendTo(c("Screen", "Report", "Warning"), "'speed.warning'/'speed.error' were set, but a valid distance matrix is not present. Aborting speed checks.")
  }

  do.checkInactiveness <- FALSE
  if (is.null(inactive.warning)) {
    appendTo(c("Screen", "Report", "Warning"), "'inactive.warning'/'inactive.error' were not set, skipping inactivity checks.")
  } else {
    if (!attributes(dist.mat)$valid)
      appendTo(c("Report", "Screen", "Warning"), "Running inactiveness checks without a distance matrix. Performance may be limited.")
    do.checkInactiveness <- TRUE
  }

  movement.names <- names(movements) # this will be used further down to reinstate the names in the movements list.

  # clean override based on movements
  if (is.numeric(override))
    trigger_override_warning <- any(link <- !override %in% extractSignals(movement.names))
  else
    trigger_override_warning <- any(link <- !override %in% movement.names)

  if (trigger_override_warning) {
    appendTo(c("Screen", "Warning", "Report"), paste0("Override has been triggered for ",
      ifelse(sum(link) == 1, "tag ", "tags "), paste(override[link], collapse = ", "), " but ",
      ifelse(sum(link) == 1, "this signal was", "these signals were"), " not detected."))
    override <- override[!link]
  }

  # convert numeric override to full tag override to prevent problems downstream
  if (is.numeric(override))
    override <- movement.names[match(override, extractSignals(movement.names))]

  movements <- lapply(seq_along(movements), function(i) {
    tag <- names(movements)[i]
    counter <- paste0("(", i, "/", length(movements), ")")

    appendTo("debug", paste0("debug: Checking movement quality for tag ", tag,"."))

    if (is.na(match(tag, override))) {
      output <- checkMinimumN(movements = movements[[tag]], tag = tag, min.total.detections = min.total.detections,
                               min.per.event = min.per.event[1], n = counter)

      output <- checkFirstDetBackFromRelease(movements = output, tag = tag, detections = detections.list[[tag]], spatial = spatial,
                              bio = bio, arrays = arrays, GUI = GUI, save.tables.locally = save.tables.locally, n = counter)

      output <- checkImpassables(movements = output, tag = tag, bio = bio, detections = detections.list[[tag]], n = counter, 
                                 spatial = spatial, dotmat = dotmat, GUI = GUI, save.tables.locally = save.tables.locally)

      output <- checkJumpDistance(movements = output, bio = bio, tag = tag, dotmat = dotmat, paths = paths, arrays = arrays,
                                  spatial = spatial, jump.warning = jump.warning, jump.error = jump.error, GUI = GUI,
                                  detections = detections.list[[tag]], save.tables.locally = save.tables.locally, n = counter)

      if (do.checkSpeeds) {
        temp.valid.movements <- simplifyMovements(movements = output, tag = tag, bio = bio, discard.first = discard.first,
                                                  speed.method = speed.method, dist.mat = dist.mat)
        output <- checkSpeeds(movements = output, tag = tag, valid.movements = temp.valid.movements, 
                              detections = detections.list[[tag]], speed.warning = speed.warning, n = counter, 
                              speed.error = speed.error, GUI = GUI, save.tables.locally = save.tables.locally)
        rm(temp.valid.movements)
      }

      if (do.checkInactiveness) {
        output <- checkInactiveness(movements = output, tag = tag, detections = detections.list[[tag]], n = counter,
                                    inactive.warning = inactive.warning, inactive.error = inactive.error,
                                    dist.mat = dist.mat, GUI = GUI, save.tables.locally = save.tables.locally)
      }
    } else {
      output <- overrideValidityChecks(moves = movements[[tag]], detections = detections.list[[tag]], n = counter, # nocov
                                       tag = tag, GUI = GUI, save.tables.locally = save.tables.locally) # nocov
    }
    return(output)
  })
  names(movements) <- movement.names
  rm(movement.names)
# -------------------------

# Compile section movements
  appendTo(c("Screen", "Report"), "M: Compiling and checking section movements for the valid tags.")

  section.movements <- lapply(seq_along(movements), function(i) {
    tag <- names(movements)[i]
    counter <- paste0("(", i, "/", length(movements), ")")
    appendTo("debug", paste0("debug: Compiling section movements for tag ", tag,"."))

    aux <- sectionMovements(movements = movements[[i]], spatial = spatial, valid.dist = attributes(dist.mat)$valid)

    if (!is.null(aux)) { # interesting... why do I have this here but not on residency? hm...
      aux <- checkMinimumN(movements = aux, tag = tag, min.total.detections = 0, # don't run the minimum total detections check here.
                           min.per.event = min.per.event[2], n = counter)

      output <- checkLinearity(secmoves = aux, tag = tag, spatial = spatial, arrays = arrays, 
                               GUI = GUI, save.tables.locally = save.tables.locally, n = counter)
      return(output)
    } else {
      return(NULL)
    }
  })
  names(section.movements) <- names(movements)
  section.movements <- section.movements[!sapply(section.movements, is.null)]
  # Update array movements based on section movements validity
  movements <- updateValidity(arrmoves = movements, secmoves = section.movements)

  # compile valid movements
  appendTo(c("Screen", "Report"), "M: Filtering valid array movements.")

  valid.movements <- assembleValidMoves(movements = movements, bio = bio, discard.first = discard.first,
                                         speed.method = speed.method, dist.mat = dist.mat)

  appendTo(c("Screen", "Report"), "M: Filtering valid section movements.")

  section.movements <- assembleValidSecMoves(valid.moves = valid.movements, spatial = spatial, 
                                             valid.dist = attributes(dist.mat)$valid)

  appendTo(c("Screen", "Report"), "M: Compiling migration timetable.")

  timetable <- assembleTimetable(secmoves = section.movements, valid.moves = valid.movements, all.moves = movements, spatial = spatial,
                                 arrays = arrays, bio = bio, tz = tz, dist.mat = dist.mat, speed.method = speed.method,
                                 if.last.skip.section = if.last.skip.section, success.arrays = success.arrays)

  status.df <- assembleOutput(timetable = timetable, bio = bio, spatial = spatial,
                              dist.mat = dist.mat, tz = tz)

  appendTo(c("Screen", "Report"), "M: Compiling summary information tables.")

  section.overview <- assembleSectionOverview(status.df = status.df, spatial = spatial)

  aux <- list(valid.movements = valid.movements, spatial = spatial, 
              rsp.info = list(bio = bio, analysis.type = "migration"))
  times <- getTimes(input = aux, move.type = "array", event.type = "arrival", n.events = "first")
  rm(aux)

  appendTo("Screen", "M: Validating detections.")

  recipient <- validateDetections(detections.list = detections.list, movements = valid.movements)
  detections <- recipient$detections
  valid.detections <- recipient$valid.detections
  rm(recipient)

# -------------------------------------

# CJS stuff
  the.matrices <- assembleMatrices(spatial = spatial, movements = valid.movements, status.df = status.df,
                                   arrays = arrays, paths = paths, dotmat = dotmat)[[2]] # extract only the minimum matrix

  m.by.array <- breakMatricesByArray(m = the.matrices, arrays = arrays, type = "peers")

  if (is.null(m.by.array[[1]])) {
    calculate.efficiency <- FALSE
    appendTo(c("Screen", "Report", "Warning"), "Aborting inter-array efficiency calculations (will limit the report's output).")
  } else {
    calculate.efficiency <- TRUE
  }

  if (calculate.efficiency) {
    appendTo(c("Screen", "Report"), "M: Calculating array efficiency.")

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

    overall.CJS <- assembleArrayCJS(mat = the.matrices, CJS = CJS.list, arrays = arrays, releases = release_nodes, silent = FALSE)

    if (!is.null(replicates)) {
      intra.array.matrices <- getDualMatrices(replicates = replicates, CJS = overall.CJS, spatial = spatial, detections.list = valid.detections)
      recipient <- includeIntraArrayEstimates(m = intra.array.matrices, CJS = overall.CJS)
      overall.CJS <- recipient$CJS
      intra.array.CJS <- recipient$intra.CJS
      rm(recipient)
    } else {
      intra.array.matrices <- NULL
      intra.array.CJS <- NULL
    }

    aux <- mbSplitCJS(mat = m.by.array, fixed.efficiency = overall.CJS$efficiency)
    aux <- aux[names(the.matrices)]
    split.CJS <- assembleSplitCJS(mat = the.matrices, CJS = aux, arrays = arrays, releases = release_nodes, intra.CJS = intra.array.CJS)
    release.overview <- lapply(names(split.CJS), function(i, releases = spatial$release.sites) {
      output <- split.CJS[[i]]
      x <- unlist(stringr::str_split(i, "\\.", 2))
      output$Release <- rep(c(releases[releases$Standard.name == x[2], paste0("n.", x[1])], NA, NA), 2)
      output <- output[, c(ncol(output), 1:(ncol(output) - 1))]
      return(output)
    })
    names(release.overview) <- names(aux)
    rm(aux)

    aux <- mbGroupCJS(mat = m.by.array, status.df = status.df, fixed.efficiency = overall.CJS$efficiency)
    group.CJS <- assembleGroupCJS(mat = the.matrices, CJS = aux, arrays = arrays, releases = release_nodes, intra.CJS = intra.array.CJS)
    group.overview <- lapply(names(group.CJS), function(i, releases = spatial$release.sites) {
      output <- group.CJS[[i]]
      x <- unlist(stringr::str_split(i, "\\.", 2))[1]
      output$Release <- rep(c(sum(releases[, paste0("n.", x)]), NA, NA), 2)
      output <- output[, c(ncol(output), 1:(ncol(output) - 1))]
      return(output)
    })
    names(group.overview) <- names(aux)
    rm(aux)
  } else {
    overall.CJS <- NULL

    if (!is.null(replicates)) {
      appendTo(c("Screen", "Report"), "M: Calculating intra-array efficiency.")
      intra.array.matrices <- getDualMatrices(replicates = replicates, CJS = overall.CJS, spatial = spatial, detections.list = valid.detections)
      intra.array.CJS <- includeIntraArrayEstimates(m = intra.array.matrices, CJS = overall.CJS)$intra.CJS
    } else {
      intra.array.matrices <- NULL
      intra.array.CJS <- NULL
    }

    release.overview <- NULL
    group.overview <- NULL
  }
# -------------------------------------

# wrap up in-R objects
  deployments <- do.call(rbind.data.frame, deployments)
  matrices <- the.matrices

  # extra info for potential RSP analysis
  rsp.info <- list(analysis.type = "migration", analysis.time = the.time, bio = bio, 
                   tz = tz, actel.version = utils::packageVersion("actel"))

  if (!is.null(override))
    override.fragment <- paste0('<span style="color:red">Manual mode has been triggered for **', length(override),'** tag(s).</span>\n')
  else
    override.fragment <- ""

  if (file.exists(resultsname <- "actel_migration_results.RData")) {
    continue <- TRUE
    index <- 1
    while (continue) {
      if (file.exists(resultsname <- paste0("actel_migration_results.", index, ".RData"))) {
        index <- index + 1
      } else {
        continue <- FALSE
      }
    }
    rm(continue, index)
  }

  if (interactive()) { # nocov start
    decision <- userInput(paste0("Would you like to save a copy of the results to ", resultsname, "?(y/n) "), 
                          choices = c("y", "n"), hash = "# save results?")
  } else { # nocov end
    decision <- "n"
  }

  if (decision == "y") { # nocov start
    appendTo(c("Screen", "Report"), paste0("M: Saving results as '", resultsname, "'."))
    if (attributes(dist.mat)$valid) {
      save(detections, valid.detections, spatial, deployments, arrays, movements, valid.movements, 
        section.movements, status.df, section.overview, group.overview, release.overview, matrices, 
        overall.CJS, intra.array.matrices, intra.array.CJS, times, rsp.info, dist.mat, file = resultsname)
    } else {
      save(detections, valid.detections, spatial, deployments, arrays, movements, valid.movements, 
        section.movements, status.df, section.overview, group.overview, release.overview, matrices, 
        overall.CJS, intra.array.matrices, intra.array.CJS, times, rsp.info, file = resultsname)
    }
  } else { # nocov end
    appendTo(c("Screen", "Report"), paste0("M: Skipping saving of the results."))
  }
  rm(decision)

# ------------

# Print graphics
  trigger.report.error.message <- TRUE
  if (report) {
    appendTo(c("Screen", "Report"), "M: Producing the report.")
    on.exit({if (trigger.report.error.message) message("M: Producing the report failed. If you have saved a copy of the results, you can reload them using dataToList().")}, add = TRUE)

    if (dir.exists(paste0(tempdir(), "/actel_report_auxiliary_files")))
      unlink(paste0(tempdir(), "/actel_report_auxiliary_files"), recursive = TRUE)

    dir.create(paste0(tempdir(), "/actel_report_auxiliary_files"))

    if (!getOption("actel.debug", default = FALSE))
      on.exit(unlink(paste0(tempdir(), "/actel_report_auxiliary_files"), recursive = TRUE), add = TRUE)

    biometric.fragment <- printBiometrics(bio = bio)

    efficiency.fragment <- printEfficiency(CJS = overall.CJS,
                                           intra.CJS = intra.array.CJS,
                                           type = "migration")

    printDotplots(status.df = status.df,
                  valid.dist = attributes(dist.mat)$valid)

    printSurvivalGraphic(section.overview = section.overview)

    printLastArray(status.df = status.df)

    printDot(dot = dot,
             spatial = spatial,
             print.releases = print.releases)

    if (calculate.efficiency) {
      printProgression(dot = dot,
                       overall.CJS = overall.CJS,
                       spatial = spatial,
                       status.df = status.df,
                       print.releases = print.releases)
      display.progression <- TRUE
      array.overview.fragment <- printArrayOverview(group.overview)
    } else {
      display.progression <- FALSE
      array.overview.fragment <- ""
    }

    individual.plots <- printIndividuals(detections.list = detections,
                                         movements = movements,
                                         valid.movements = valid.movements,
                                         spatial = spatial,
                                         status.df = status.df,
                                         rsp.info = rsp.info,
                                         y.axis = detections.y.axis)

    circular.plots <- printCircular(times = timesToCircular(times),
                                    bio = bio)

    if (nrow(section.overview) > 3)
      survival.graph.size <- "width=90%" else survival.graph.size <- "height=4in"

    if (any(sapply(valid.detections, function(x) any(!is.na(x$Sensor.Value))))) {
      sensor.plots <- printSensorData(detections = valid.detections, 
                                      spatial = spatial,
                                      rsp.info = rsp.info, 
                                      colour.by = detections.y.axis)
    } else {
      sensor.plots <- NULL
    }
  }
# ---------------

# wrap up the txt report
  appendTo("Report", "M: Analysis completed!\n\n-------------------")
  
  if (file.exists(paste(tempdir(), "temp_comments.txt", sep = "/")))
    appendTo("Report", paste0("User comments:\n-------------------\n", gsub("\t", ": ", gsub("\r", "", readr::read_file(paste(tempdir(), "temp_comments.txt", sep = "/")))), "-------------------")) # nocov

  if (file.exists(paste(tempdir(), "temp_UD.txt", sep = "/")))
    appendTo("Report", paste0("User interventions:\n-------------------\n", gsub("\r", "", readr::read_file(paste(tempdir(), "temp_UD.txt", sep = "/"))), "-------------------")) # nocov

  if (!is.null(datapack))
    appendTo("Report", paste0("Preload function call:\n-------------------\n", attributes(datapack)$function_call, "\n-------------------"))

  appendTo("Report", paste0("Migration function call:\n-------------------\n", the.function.call, "\n-------------------"))
# ------------------

# print html report
  if (report) {
    if (file.exists(reportname <- "actel_migration_report.html")) {
      continue <- TRUE
      index <- 1
      while (continue) {
        if(file.exists(reportname <- paste0("actel_migration_report.", index, ".html"))) {
          index <- index + 1
        } else {
          continue <- FALSE
        }
      }
      appendTo("Screen", paste0("M: An actel report is already present in the current directory.\n   Saving new report as ", reportname, "."))
      rm(continue, index)
    } else {
      appendTo("Screen", "M: Saving actel report as 'actel_migration_report.html'.")
    }

    appendTo("debug", "debug: Printing report rmd")
      printMigrationRmd(override.fragment = override.fragment,
                        biometric.fragment = biometric.fragment,
                        section.overview = section.overview,
                        efficiency.fragment = efficiency.fragment,
                        display.progression = display.progression,
                        array.overview.fragment = array.overview.fragment,
                        survival.graph.size = survival.graph.size,
                        individual.plots = individual.plots,
                        circular.plots = circular.plots,
                        sensor.plots = sensor.plots,
                        spatial = spatial,
                        deployments = deployments,
                        valid.detections = valid.detections,
                        detections = detections,
                        detections.y.axis = detections.y.axis)

    appendTo("debug", "debug: Converting report to html")
    rmarkdown::render(input = paste0(tempdir(), "/actel_report_auxiliary_files/actel_migration_report.Rmd"),
      output_dir = paste0(tempdir(), "/actel_report_auxiliary_files"), quiet = TRUE)

    appendTo("debug", "debug: Moving report")
    file.copy(paste0(tempdir(), "/actel_report_auxiliary_files/actel_migration_report.html"), reportname)
    if (interactive() & auto.open) { # nocov start
      appendTo("debug", "debug: Opening report.")
      browseURL(reportname)
    } # nocov end
  }
  trigger.report.error.message <- FALSE
# ------------------

  jobname <- paste0(gsub(" |:", ".", as.character(Sys.time())), ".actel.log.txt")

  if (interactive() & !report) { # nocov start
    decision <- userInput(paste0("Would you like to save a copy of the analysis log to ", jobname, "?(y/n) "), 
                          choices = c("y", "n"), hash = "# save job log?")
  } else { # nocov end
    decision <- "n"
  }
  if (decision == "y" | decision == "Y") { # nocov start
    appendTo("Screen", paste0("M: Saving job log as '",jobname, "'."))
    file.copy(paste(tempdir(), "temp_log.txt", sep = "/"), jobname)
  } # nocov end

  output <- list(detections = detections,
                 valid.detections = valid.detections,
                 spatial = spatial,
                 deployments = deployments, 
                 arrays = arrays,
                 movements = movements,
                 valid.movements = valid.movements,
                 section.movements = section.movements, 
                 status.df = status.df,
                 section.overview = section.overview, 
                 group.overview = group.overview,
                 release.overview = release.overview, 
                 matrices = matrices,
                 overall.CJS = overall.CJS, 
                 intra.array.matrices = intra.array.matrices,
                 intra.array.CJS = intra.array.CJS, 
                 times = times,
                 rsp.info = rsp.info)

  if (attributes(dist.mat)$valid)
    output$dist.mat <- dist.mat

  appendTo("Screen", "M: Analysis completed!")
  finished.unexpectedly <- FALSE

  return(output)
}

#' Print Rmd report
#'
#' Creates a Rmd report and converts it to html.
#'
#' @param override.fragment Rmarkdown string specifying the type of report for the header.
#' @param biometric.fragment Rmarkdown string specifying the biometric graphics drawn.
#' @param section.overview A summary table with the number of tags that disappeared/moved onwards at each section.
#' @param efficiency.fragment Rmarkdown string specifying the efficiency results.
#' @param display.progression Logical. If TRUE, the progression plot has been created and can be displayed.
#' @param array.overview.fragment Rmarkdown string specifying the array overview results.
#' @param survival.graph.size Rmarkdown string specifying the type size of the survival graphics.
#' @param individual.plots Rmarkdown string specifying the name of the individual plots.
#' @param circular.plots Rmarkdown string specifying the name of the circular plots.
#' @param sensor.plots Rmarkdown string specifying the name of the sensor plots.
#' @inheritParams loadDetections
#'
#' @return No return value, called for side effects.
#'
#' @keywords internal
#'
printMigrationRmd <- function(override.fragment, biometric.fragment, section.overview,
  efficiency.fragment, display.progression, array.overview.fragment, survival.graph.size,
  individual.plots, circular.plots, sensor.plots, spatial, deployments, valid.detections, 
  detections, detections.y.axis){

  work.path <- paste0(tempdir(), "/actel_report_auxiliary_files/")

 if (!is.null(spatial$unknowns)) {
    unknown.fragment <- paste0('<span style="color:red"> Number of relevant unknown receivers: **', sum(sapply(spatial$unknowns, length)), '** (of which ', length(spatial$unknowns$included),' were included)</span>\n')
  } else {
    unknown.fragment <- ""
  }
  if (!is.null(sensor.plots)) {
    sensor.fragment <- paste0("### Sensor plots

Note:
  : The colouring in these plots will follow that of the individual detection plots, which can be modified using `detections.y.axis`.
  : The data used for these graphics is stored in the `valid.detections` object.
  : You can replicate these graphics and edit them as needed using the `plotSensors()` function.

<center>\n", sensor.plots, "\n</center>")
  } else {
    sensor.fragment <- NULL
  }

  report <- readr::read_file(paste0(tempdir(), "/temp_log.txt"))
  report <- gsub("(\\\\|\")", "\\\\\\1", report)

  if (file.exists(paste0(tempdir(), '/temp_warnings.txt'))) {
    warning.messages <- gsub("\\r", "", readr::read_file(paste0(tempdir(), '/temp_warnings.txt')))
    warning.messages <- gsub("(\\\\|\")", "\\\\\\1", warning.messages)
  } else {
    warning.messages <- 'No warnings were raised during the analysis.'
  }

  if (file.exists(paste0(tempdir(), '/temp_comments.txt'))) {
    comment.fragment <- gsub("\\r", "", readr::read_file(paste0(tempdir(), '/temp_comments.txt')))
    comment.fragment <- gsub("(\\\\|\")", "\\\\\\1", comment.fragment)
  } else {
    comment.fragment <- 'No comments were included during the analysis.'
  }

  oldoptions <- options(knitr.kable.NA = "-")
  on.exit(options(oldoptions), add = TRUE)

  sink(paste0(work.path, "actel_migration_report.Rmd"))
  cat(paste0(
'---
title: "Acoustic telemetry migration analysis"
author: "Actel R package (', utils::packageVersion("actel"), ')"
output:
  html_document:
    includes:
      after_body: ', work.path, 'toc_menu_migration.html
---

### Summary

Target folder: ', stringr::str_extract(pattern = '(?<=Target folder: )[^\r|^\n]*', string = report), '

Timestamp: **', stringr::str_extract(pattern = '(?<=Timestamp: )[^\r|^\n]*', string = report), '**

Number of target tags: **`r I(nrow(status.df))`**

', override.fragment,'

Number of listed receivers: **', stringr::str_extract(pattern = '(?<=Number of ALS: )[0-9]*', string = report), '** (of which **', stringr::str_extract(pattern = '(?<=of which )[0-9]*', string = report), '** had no detections)

', unknown.fragment,'

Data time range: ', stringr::str_extract(pattern = '(?<=Data time range: )[^\r|^\n]*', string = report), '

Percentage of post-release valid detections: ', round(sum(unlist(lapply(valid.detections, nrow))) / sum(unlist(lapply(detections, nrow))) * 100, 2), '%

Found a bug? [**Report it here.**](https://github.com/hugomflavio/actel/issues)

Want to cite actel in a publication? Run `citation(\'actel\')`

### Study area

Arrays with the same background belong to the same section. Release sites are marked with "R.S.". Arrays connected with an arrow indicate that the animals can only pass in one direction.

<img src=', work.path, ifelse(file.exists(paste0(work.path, "mb_arrays.svg")), "mb_arrays.svg", "mb_arrays.png"), ' style="padding-top: 15px;"/>

### Receiver stations

', paste(knitr::kable(spatial$stations, row.names = FALSE), collapse = "\n"), '

### Deployments

', paste(knitr::kable(deployments, row.names = FALSE), collapse = "\n"), '

### Release sites

', paste(knitr::kable(spatial$release.sites, row.names = FALSE), collapse = "\n"), '

### Array forward efficiency

', efficiency.fragment,'

### Warning messages

```{r warnings, echo = FALSE, comment = NA}
cat("', warning.messages, '")
```

### User comments

```{r comments, echo = FALSE, comment = NA}
cat("', comment.fragment, '")
```

', ifelse(biometric.fragment == '', '', paste0('### Biometric graphics

Note:
  : The data used in this graphic is the data present in the biometrics.csv file.

<center>
', biometric.fragment,'
</center>
')), '

### Section Survival

Note:
  : The data used in this table and graphic is stored in the `section.overview` object.

', paste(knitr::kable(section.overview), collapse = "\n"), '

<center>
![](survival.png){ ',survival.graph.size ,' }
</center>


### Last Seen Arrays

Note:
  : The data used in this graphic is stored in the `status.df` object (\'Very.last.array\' column).

<center>
![](last_arrays.png){ width=66% }
</center>


### Progression

', ifelse(display.progression, paste0('Zoom in or open the figure in a new tab to clearly read the text within each circle.

Note:
  : The progression calculations **do not account for** backwards movements. This implies that the total number of animals to have been **last seen** at a given array **may be lower** than the displayed below. Please refer to the [section survival overview](#section-survival) and [last seen arrays](#last-seen-arrays) to find out how many animals were considered to have disappeared per section.
  : The data used in this graphic is stored in the `overall.CJS` object, and the data used in the tables is stored in the `group.overview` object. You can find detailed progressions per release site in the `release.overview` object.

<img src=', work.path, ifelse(file.exists(paste0(work.path, "mb_efficiency.svg")), "mb_efficiency.svg", "mb_efficiency.png"), ' style="padding-top: 15px; padding-bottom: 15px;"/>

'), 'Progression cannot be displayed if efficiencies are not calculated. See full log for more details.'), array.overview.fragment, '


### Time of arrival at each Array

Note:
  : Coloured lines on the outer circle indicate the mean value for each group and the respective ranges show the standard error of the mean. Each group\'s bars sum to 100%. The number of data points in each group is presented between brackets in the legend of each pannel.
  : You can replicate these graphics and edit them as needed using the `plotTimes()` function.
  : The data used in these graphics is stored in the `times` object.
  : To obtain reports with the legacy linear circular scale, run `options(actel.circular.scale = "linear")` before running your analyses.

<center>
', circular.plots,'
</center>


### Dotplots

Note:
  : The **top** 10% of the values for each panel are marked in **red**.
  : The **bottom** 10% of the values for each panel are marked in **blue**.
  : The columns starting with "To" should be read as either "Average time to ..." or "Average speed to ...", depending on the unit used. The columns starting with "In" should be read as "Total time in ...". These reductions were made to keep the column headers as short as possible.
  : The data used in these graphics is stored in the `status.df` object.

<center>
![](', work.path, 'dotplots.png){ width=95% }
</center>


### Individual detection plots

Note:
  : You can choose to plot detections by station or by array using the `detections.y.axis` argument.
  : The detections are coloured by ', ifelse(detections.y.axis == "stations", 'array', 'section'), '. The vertical black dashed line shows the time of release. The vertical grey dashed lines show the assigned moments of entry and exit for each study area section. The full dark-grey line shows the movement events considered valid, while the dashed dark-grey line shows the movement events considered invalid.
', ifelse(detections.y.axis == "stations", '  : The movement event lines move straight between the first and last station of each event (i.e. in-between detections will not be individually linked by the line).\n', ''),
'  : Manually **edited** tag detections are highlighted with **yellow** graphic borders.
  : Manually **overridden** tag detections are highlighted with **red** graphic borders.
  : The ', ifelse(detections.y.axis == "stations", 'stations', 'arrays'), ' have been aligned by ', ifelse(detections.y.axis == "stations", 'array', 'section'), ', following the order provided ', ifelse(detections.y.axis == "stations", '', 'either '), 'in the spatial input', ifelse(detections.y.axis == "stations", '.', ' or the `section.order` argument.'), '
  : You can replicate these graphics and edit them as needed using the `plotDetections()` function.
  : You can also see the movement events of multiple tags simultaneously using the `plotMoves()` function.
  : The data used in these graphics is stored in the `detections` and `movements` objects (and respective valid counterparts).

<center>
', individual.plots,'
</center>

', sensor.fragment,'

### Full log

```{r log, echo = FALSE, comment = NA}
cat("', gsub("\\r", "", report), '")
```

'), fill = TRUE)
sink()

sink(paste0(work.path, "toc_menu_migration.html"))
cat(
'<style>
h3 {
  padding-top: 25px;
  padding-bottom: 15px;
}

h4 {
  padding-top: 25px;
  padding-bottom: 15px;
}

img[src*="#diagram"] {
  width = 100%;
  padding-top = 200px;
  padding-bottom = 15px;
}

/* The sidebar menu */
.sidenav {
  height: 100%;
  width: 110px;
  position: fixed;
  z-index: 1;
  top: 0;
  left: 0;
  background-color: #fcfcfc;
  overflow-x: hidden;
  padding-top: 20px;
}

/* The navigation menu links */
.sidenav a {
  padding: 6px 8px 6px 16px;
  text-decoration: none;
  /*font-size: 25px;*/
  color: #818181;
  display: block;
}

.sidenav p {
  padding: 6px 8px 6px 16px;
  text-decoration: none;
  font-size: 25px;
  color: #818181;
  display: block;
}

.sidenav a:hover {
  background-color: #52a548;
  color: #f1f1f1;
}

.fluid-row {
  margin-left: 110px; /* Same as the width of the sidebar */
  padding: 0px 10px;
}

.section {
  margin-left: 110px; /* Same as the width of the sidebar */
  padding: 0px 10px;
}

.level4 {
  margin-left: 0px; /* Same as the width of the sidebar */
  padding: 0px 0px;
}

/* On smaller screens, where height is less than 450px, change the style of the sidebar (less padding and a smaller font size) */
@media screen and (max-height: 450px) {
  .sidenav {padding-top: 15px;}
  .sidenav a {font-size: 18px;}
}
</style>

<div class="sidenav">
  <p>Index:</p>
  <a href="#summary">Summary</a>
  <a href="#study-area">Study area</a>
  <a href="#receiver-stations">Stations</a>
  <a href="#deployments">Deployments</a>
  <a href="#release-sites">Release sites</a>
  <a href="#array-forward-efficiency">Array efficiency</a>
  <a href="#warning-messages">Warnings</a>
  <a href="#user-comments">Comments</a>',
  ifelse(biometric.fragment == '', '', '\n  <a href="#biometric-graphics">Biometrics</a>'),'
  <a href="#section-survival">Section survival</a>
  <a href="#last-seen-arrays">Last seen</a>
  <a href="#progression">Progression</a>
  <a href="#time-of-arrival-at-each-array">Arrival times</a>
  <a href="#dotplots">Dotplots</a>
  <a href="#individual-detection-plots">Individual detections</a>',
  ifelse(is.null(sensor.fragment), '', '\n  <a href="#sensor-plots">Sensor data</a>'),'
  <a href="#full-log">Full log</a>
</div>
', fill = TRUE)
sink()
}

#' Create the timetable
#'
#' Crawls trough the movement events of each tag to find when it entered and left each section of the study area.
#'
#' @inheritParams explore
#' @inheritParams migration
#' @inheritParams simplifyMovements
#' @inheritParams loadDetections
#' @inheritParams groupMovements
#' @inheritParams assembleArrayCJS
#' @param secmoves the section-movements
#' @param valid.moves the valid array movements
#' @param all.moves all array movements
#'
#' @return A data frame containing the entering and leaving timestamps for each section per target tag
#'
#' @keywords internal
#'
assembleTimetable <- function(secmoves, valid.moves, all.moves, spatial, arrays, bio, tz,
  dist.mat, speed.method, if.last.skip.section, success.arrays) {
  appendTo("debug", "Running assembleTimetable.")

  # NOTE: The NULL variables below are actually column names used by data.table.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  Last.array <- NULL
  Last.time <- NULL
  Last.station <- NULL
  Section <- NULL
  Valid <- NULL
  Array <- NULL
  Detections <- NULL

  # temporarily calculate inter-section speeds
  if (attributes(dist.mat)$valid) {
    aux <- names(secmoves)
    secmoves <- lapply(names(secmoves), function(tag) {
      # cat(tag, "\n")
      aux <- secmoves[[tag]]
      aux$Speed.until <- NA

      the.row <- match(tag, bio$Transmitter)
      origin.time <- bio[the.row, "Release.date"]
      origin.place <- as.character(bio[the.row, "Release.site"])
      if (origin.time <= aux$First.time[1]) {
        a.sec <- as.vector(difftime(aux$First.time[1], origin.time, units = "secs"))
        my.dist <- dist.mat[aux$First.station[1], origin.place]
        aux$Speed.until[1] <- round(my.dist/a.sec, 6)
      }

      if (nrow(aux) > 1) {
        capture <- lapply(2:nrow(aux), function(i) {
          if (speed.method == "last to first"){
            a.sec <- as.vector(difftime(aux$First.time[i], aux$Last.time[i - 1], units = "secs"))
            my.dist <- dist.mat[aux$First.station[i], gsub(" ", ".", aux$Last.station[i - 1])]
          }
          if (speed.method == "last to last"){
            a.sec <- as.vector(difftime(aux$Last.time[i], aux$Last.time[i - 1], units = "secs"))
            my.dist <- dist.mat[aux$Last.station[i], gsub(" ", ".", aux$Last.station[i - 1])]
          }
          aux$Speed.until[i] <<- round(my.dist/a.sec, 6)
          rm(a.sec, my.dist)
        })
      }
      return(aux)
    })
    names(secmoves) <- aux
    rm(aux)
  }

  sections <- names(spatial$array.order)

  # Create the timetable
  recipient <- vector()
  if (attributes(dist.mat)$valid) {
    for (i in seq_len(length(sections))) {
      recipient <- c(recipient, paste(c("Times.entered", "Average.time.until", "Average.speed.to", "First.array", "First.station",
        "First.arrived", "Average.time.in", "Average.speed.in", "Last.array", "Last.station", "Last.left", "Total.time.in"), sections[i], sep = "."))
    }
  } else {
    for (i in seq_len(length(sections))) {
      recipient <- c(recipient, paste(c("Times.entered", "Average.time.until", "First.array", "First.station",
        "First.arrived", "Average.time.in", "Last.array", "Last.station", "Last.left", "Total.time.in"), sections[i], sep = "."))
    }
  }
  recipient <- c(recipient, "Very.last.array", "Very.last.time", "Status", "Valid.detections", "Valid.events", "Invalid.detections", "Invalid.events", "Backwards.movements", "Max.cons.back.moves", "P.type")
  if (attributes(dist.mat)$valid && speed.method == "last to last")
    recipient <- recipient[!grepl("Average.speed.in",recipient)]

  timetable <- matrix(nrow = length(secmoves), ncol = length(recipient))
  timetable <- as.data.frame(timetable)

  colnames(timetable) <- recipient
  rm(recipient)
  rownames(timetable) <- names(secmoves)

  # Start filling it up
  capture <- lapply(names(secmoves), function(tag) {
    # cat(tag, "\n")
    aux <- split(secmoves[[tag]], secmoves[[tag]]$Section)
    appendTo("debug", paste0("Assembling timetable values for tag ", tag, "."))
    recipient <- lapply(seq_along(aux), function(i) {
      # cat(i, "\n")
      recipient <- rep(NA, ncol(timetable))
      names(recipient) <- colnames(timetable)
      recipient <- t(as.data.frame(recipient))

      total.time <- apply(aux[[i]][, c("First.time", "Last.time")], 1, function(x) difftime(x[2], x[1], units = "secs"))
      recipient[1, paste0("Total.time.in.", names(aux)[i])] <- sum(total.time)
      recipient[1, paste0("Average.time.in.", names(aux)[i])] <- mean(total.time)

      recipient[1, paste0("Times.entered.", names(aux)[i])] <- nrow(aux[[i]])

      recipient[1, paste0("First.array.", names(aux)[i])] <- aux[[i]]$First.array[1]
      recipient[1, paste0("First.station.", names(aux)[i])] <- aux[[i]]$First.station[1]
      recipient[1, paste0("First.arrived.", names(aux)[i])] <- as.character(aux[[i]]$First.time[1])

      recipient[1, paste0("Last.array.", names(aux)[i])] <- aux[[i]][.N, Last.array]
      recipient[1, paste0("Last.station.", names(aux)[i])] <- aux[[i]][.N, Last.station]
      recipient[1, paste0("Last.left.", names(aux)[i])] <- as.character(aux[[i]][.N, Last.time])

      if (attributes(dist.mat)$valid && speed.method == "last to first")
        recipient[1, paste0("Average.speed.in.", names(aux)[i])] <- round(mean(aux[[i]]$Speed.in.section.m.s), 6)

      recipient[1, paste0("Average.time.until.", names(aux)[i])] <- mean(decimalTime(aux[[i]]$Time.travelling, unit = "s"))

      if (attributes(dist.mat)$valid)
        recipient[1, paste0("Average.speed.to.", names(aux)[i])] <- round(mean(aux[[i]]$Speed.until), 6)

      return(recipient)
    })
    recipient <- as.data.frame(combine(recipient), stringsAsFactors = FALSE)

    # convert numbers to numeric and replace NAs where relevant
    the.cols <- which(grepl("Times.entered.", colnames(recipient)))
    recipient[, the.cols] <- as.numeric(recipient[, the.cols])
    recipient[, the.cols[which(is.na(recipient[, the.cols]))]] <- 0

    the.cols <- which(grepl("Average.speed.to.", colnames(recipient)))
    recipient[, the.cols] <- as.numeric(recipient[, the.cols])

    the.cols <- which(grepl("Average.speed.in.", colnames(recipient)))
    recipient[, the.cols] <- as.numeric(recipient[, the.cols])
    # --

    recipient$Very.last.array <- secmoves[[tag]][.N, Last.array]
    recipient$Very.last.time <- as.character(secmoves[[tag]][.N, Last.time])
    recipient$Valid.detections <- sum(secmoves[[tag]]$Detections)
    recipient$Valid.events <- nrow(valid.moves[[tag]])
    if (any(!all.moves[[tag]]$Valid)) {
      recipient$Invalid.detections <- sum(all.moves[[tag]][!(Valid)]$Detections)
      recipient$Invalid.events <- sum(!all.moves[[tag]]$Valid)
    } else {
      recipient$Invalid.detections <- 0
      recipient$Invalid.events <- 0
    }
    recipient$P.type <- attributes(secmoves[[tag]])$p.type

    aux <- countBackMoves(movements = valid.moves[[tag]], arrays = arrays)
    recipient$Backwards.movements <- aux[[1]]
    recipient$Max.cons.back.moves <- aux[[2]]
    recipient$P.type <- attributes(valid.moves[[tag]])$p.type


    # assign fate
    the.last.section <- secmoves[[tag]][.N, Section]
    the.last.array <- secmoves[[tag]][.N, Last.array]
    recipient$Status <- paste0("Disap. in ", the.last.section)

    not.last.section <- match(the.last.section, sections) != length(sections)
    edge.array <- arrays[[the.last.array]]$edge

    if (if.last.skip.section && not.last.section && edge.array) {
      recipient$Status <- paste("Disap. in", sections[match(the.last.section, sections) + 1])
    } else {
      recipient$Status <- paste("Disap. in", the.last.section)
    }

    if(!is.na(match(the.last.array, success.arrays)))
      recipient$Status <- "Succeeded"

    # deploy values
    appendTo("debug", paste0("Deploy timetable values for tag ", tag, "."))
    timetable[tag, ] <<- recipient

    return(NULL)
  })

  # Convert time and timestamp data
  for (section in sections) {
    for (the.col in c("Average.time.until.", "Average.time.in.", "Total.time.in.")) {
      # convert to numeric
      timetable[, paste0(the.col, section)] <- as.numeric(timetable[, paste0(the.col, section)])
      # grab the mean for later use
      aux <- mean(timetable[, paste0(the.col, section)], na.rm = TRUE)
      # convert to difftime
      timetable[, paste0(the.col, section)] <- as.difftime(timetable[, paste0(the.col, section)], units = "secs")
      units(timetable[, paste0(the.col, section)]) <- "secs"
      if (!is.nan(aux)) {
        if (aux > 86400)
          units(timetable[, paste0(the.col, section)]) <- "days"
        if (aux <= 86400 & aux > 3600)
          units(timetable[, paste0(the.col, section)]) <- "hours"
        if (aux <= 3600)
          units(timetable[, paste0(the.col, section)]) <- "mins"
      }
      timetable[, paste0(the.col, section)] <- round(timetable[, paste0(the.col, section)], 3)
    }
    for (the.col in c("First.arrived.", "Last.left.")) {
      # convert to numeric posix
      timetable[, paste0(the.col, section)] <- as.POSIXct(timetable[, paste0(the.col, section)], tz = tz)
    }
  }

  timetable$Very.last.time <- as.POSIXct(timetable$Very.last.time, tz = tz)

  timetable$Transmitter <- rownames(timetable)
  return(timetable)
}

#' Count backwards movements
#'
#' @inheritParams simplifyMovements
#' @inheritParams assembleArrayCJS
#'
#' @return A list containing:
#' \itemize{
#'  \item \code{sum.back.moves} The number of backwards movements for the target tag
#'  \item \code{max.back.moves} The maximum number of consecutive backwards movements for the target tag
#' }
#'
#' @keywords internal
#'
countBackMoves <- function(movements, arrays){
  appendTo("debug", "Starting countBackMoves.")
  if (nrow(movements) > 1) {# Determine number of backwards movements
    aux <- data.frame(
      A = movements$Array[-nrow(movements)],
      B = movements$Array[-1])
    backwards.movements <- apply(aux, 1, function(x)
      if(x[1] != x[2])
        is.na(match(x[2], arrays[[x[1]]]$all.after))
      else
        FALSE
      )
    sum.back.moves <- sum(backwards.movements)
    if (sum.back.moves > 0)
      max.back.moves <- max(rle(backwards.movements)$lengths[which(rle(backwards.movements)$values == TRUE)])
    else
      max.back.moves <- 0
  } else {
    sum.back.moves <- 0
    max.back.moves <- 0
  }
  appendTo("debug", "Terminating countBackMoves.")
  return(list(sum.back.moves = sum.back.moves, max.back.moves = max.back.moves))
}

#' Create status.df
#'
#' Combines the timetable and the original biometrics.
#'
#' @inheritParams explore
#' @inheritParams migration
#' @param timetable A table of the entering and leaving points for each section per target tag, created by assembleTimetable.
#' @inheritParams splitDetections
#' @inheritParams simplifyMovements
#' @inheritParams loadDetections
#' @inheritParams groupMovements
#' @inheritParams sectionMovements
#'
#' @return A data frame containing all the final data for each tag.
#'
#' @keywords internal
#'
assembleOutput <- function(timetable, bio, spatial, dist.mat, tz) {
  appendTo("debug", "Merging 'bio' and 'timetable'.")
  status.df <- merge(bio, timetable, by = "Transmitter", all = TRUE)

  appendTo("debug", "Completing entries for tags that were never detected.")
  sections <- names(spatial$array.order)

  status.df$Status[is.na(status.df$Status)] <- paste("Disap. in", sections[1])
  status.df$Status <- factor(status.df$Status, levels = c(paste("Disap. in", sections), "Succeeded"))
  status.df$Very.last.array[is.na(status.df$Very.last.array)] <- "Release"
  status.df$Very.last.array <- factor(status.df$Very.last.array, levels = c("Release", levels(spatial$stations$Array)))
  status.df$P.type[is.na(status.df$P.type)] <- "Skipped"
  status.df$Valid.detections[is.na(status.df$Valid.detections)] <- 0
  status.df$Invalid.detections[is.na(status.df$Invalid.detections)] <- 0

  appendTo("debug", "Appending comments.")
  if (file.exists(paste0(tempdir(), "/temp_comments.txt"))) { # nocov start
    temp <- read.table(paste0(tempdir(), "/temp_comments.txt"), header = FALSE, sep = "\t")
    status.df[, "Comments"] <- NA_character_
    for (i in seq_len(nrow(temp))) {
      link <- match(temp[i, 1], status.df$Transmitter)
      if (is.na(status.df$Comments[link])) {
        status.df$Comments[link] <- paste(temp[i, 2])
      } else {
        status.df$Comments[link] <- paste(status.df$Comments[link], temp[i, 2], sep = "// ")
      }
    }
  } # nocov end
  appendTo("debug", "Done.")
  return(status.df)
}

#' Create section.overview
#'
#' Produces a table with the survival per group of animals present in the biometrics.
#'
#' @inheritParams explore
#' @inheritParams migration
#' @inheritParams simplifyMovements
#' @inheritParams sectionMovements
#'
#' @return A data frame containing the survival per group of animals present in the biometrics.
#'
#' @keywords internal
#'
assembleSectionOverview <- function(status.df, spatial) {
  appendTo("debug", "Starting assembleSectionOverview.")
  section.overview <- as.data.frame.matrix(with(status.df, table(Group, Status)))
  section.overview$Total <- as.vector(with(status.df, table(Group)))
  colnames(section.overview) <- gsub(" ", ".", colnames(section.overview))

  sections <- names(spatial$array.order)

  if (length(sections) >= 2) {
    to.col <- paste("Migrated.to", sections[2], sep = ".")
    from.col <- paste("Disap..in", sections[1], sep = ".")
    section.overview[, to.col] <- section.overview$Total - section.overview[, from.col]
    recipient <- vector()
    for (i in 2:length(sections)) {
      recipient <- c(recipient, paste(c("Migrated.to", "Disap..in"), sections[i], sep = "."))
    }
  } else {
    recipient <- NULL
  }
  if (length(sections) > 2) {
    for (i in 3:length(sections)) {
      to.col <- paste("Migrated.to", sections[i], sep = ".")
      from.colA <- paste("Migrated.to", sections[i - 1], sep = ".")
      from.colB <- paste("Disap..in", sections[i - 1], sep = ".")
      section.overview[, to.col] <- section.overview[, from.colA] - section.overview[, from.colB]
    }
  }
  recipient <- c("Total", paste("Disap..in", sections[1], sep = "."), recipient, "Succeeded")
  appendTo("debug", "Terminating assembleSectionOverview.")
  return(section.overview[, recipient])
}
