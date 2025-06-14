#' Migration Analysis
#'
#' The \code{migration} analysis runs the same initial checks as \code{explore},
#' but on top of it, it analyses the animal behaviour. By selecting the arrays
#' that lead to success, you can define whether or not your animals survived the
#' migration. Additional plots help you find out if some animal/tag has been
#' acting odd. Multiple options allow you to tweak the analysis to fit your
#' study perfectly.
#'
#' @param section.order A vector containing the order by which sections should
#' be aligned in the results.
#' @param success.arrays The arrays that mark the end of the study area. If a
#'  tag crosses one of these arrays, the respective animal is considered to have
#'  successfully migrated through the study area.
#' @param if.last.skip.section DEPRECATED - does nothing.
#' @param disregard.parallels Logical:  Should the presence of parallel arrays
#'  invalidate potential efficiency peers? See the vignettes for more details.
#' @param back.error If a tag moves backwards a number of arrays equal or
#'  greater than \code{back.error}, user intervention is suggested.
#'  Defaults to 3. To disable user intervention suggestions, set to Inf.
#' @param back.warning If a tag moves backwards a number of arrays equal or
#'  greater than \code{back.warning}, a warning is issued. Defaults
#'  to 2. To disable back warnings, set to Inf. 
#'  Must be equal to or lower than \code{back.error}.
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
#'  \item \code{detections}: A list containing all detections for each target
#'        tag;
#'  \item \code{valid.detections}: A list containing the valid detections for
#'        each target tag;
#'  \item \code{spatial}: A list containing the spatial information used during
#'        the analysis;
#'  \item \code{deployments}: A data frame containing the deployments of each
#'        receiver;
#'  \item \code{arrays}: A list containing the array details used during the
#'        analysis;
#'  \item \code{movements}: A list containing all movement events for each
#'        target tag;
#'  \item \code{valid.movements}: A list containing the valid movement events
#'        for each target tag;
#'  \item \code{section.movements}: A list containing the valid section shifts
#'        for each target tag;
#'  \item \code{status.df}: A data.frame containing summary information for each
#'        tag, including the following columns:
#'    \itemize{
#'      \item \emph{Times.entered.\[section\]}: Number of times the tag was
#'            recorded entering a given section.
#'      \item \emph{Average.time.until.\[section\]}: Time spent between release
#'            or leaving another section and reaching at the given section.
#'      \item \emph{Average.speed.to.\[section\]}: Average speed from release or
#'            leaving one section and reaching the given section (if
#'            speed.method = "last to first"), or from release/leaving one
#'            section and leaving the given section
#'            (if speed.method = "last to last").
#'      \item \emph{First.array.\[section\]}: Array in which the tag was
#'            first detected in a given section
#'      \item \emph{First.station.\[section\]}: Standard name of the first
#'            station where the tag was detected in a given section
#'      \item \emph{First.arrived.\[section\]}: Very first arrival time at a
#'            given section
#'      \item \emph{Average.time.in.\[section\]}: Average time spent within a
#'            given section at each stay.
#'      \item \emph{Average.speed.in.\[section\]}: Average speed within a given
#'            section at each stay (only displayed if
#'            speed.method = "last to first").
#'      \item \emph{Last.array.\[section\]}: Array in which the tag was
#'            last detected in a given section
#'      \item \emph{Last.station.\[section\]}: Standard name of the last station
#'            where the tag was detected in a given section
#'      \item \emph{Last.left.\[section\]}: Very last departure time from a
#'            given section
#'      \item \emph{Total.time.in\[section\]}: Total time spent in a given
#'            section
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
#'                changes to the validity of the events,
#'          \item 'Overridden' if the user listed the tag in the
#'                \code{override} argument.
#'        }
#'      \item \emph{Comments}: Comments left by the user during the analysis
#'    }
#'  \item \code{section.overview}: A data frame containing the number of tags
#'        that disappeared in each section;
#'  \item \code{group.overview}: A list containing the number of known and
#'        estimated tags to have passed through each array, divided by group;
#'  \item \code{release.overview}: A list containing the number of known and
#'        estimated tags to have passed through each array, divided by group
#'        and release sites;
#'  \item \code{matrices}: A list of CJS matrices used for the efficiency
#'        calculations;
#'  \item \code{overall.CJS}: A list of CJS results of the inter-array CJS
#'        calculations;
#'  \item \code{intra.array.CJS}: A list of CJS results of the intra-array CJS
#'        calculations;
#'  \item \code{times}: A data frame containing all arrival times (per tag) at
#'        each array;
#'  \item \code{rsp.info}: A list containing appendix information for the RSP
#'        package;
#'  \item \code{dist.mat}: The distance matrix used in the analysis (if a valid
#'        distance matrix was supplied)
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
  min.total.detections = 2,
  min.per.event = 1,
  start.time = NULL,
  stop.time = NULL,
  speed.method = c("last to first", "last to last", "first to first"),
  speed.warning = NULL,
  speed.error = NULL,
  jump.warning = 2,
  jump.error = 3,
  back.warning = 2,
  back.error = 3,
  inactive.warning = NULL,
  inactive.error = NULL,
  exclude.tags = NULL,
  override = NULL,
  report = FALSE,
  auto.open = TRUE,
  discard.orphans = FALSE,
  discard.first = NULL,
  save.detections = FALSE,
  if.last.skip.section,
  replicates = NULL,
  disregard.parallels = TRUE,
  GUI = c("needed", "always", "never"),
  save.tables.locally = FALSE,
  print.releases = TRUE,
  detections.y.axis = c("auto", "stations", "arrays"))
{
  event(type = "debug", "Running migration.")

# check deprecated argument
  if (!missing(if.last.skip.section)) {
    event(type = "stop",
          "'if.last.skip.section' has been deprecated.",
          " This was necessary to fix issue 79.")
  }

# clean up any lost helpers
  deleteHelpers()
  if (file.exists(paste0(tempdir(), "/actel_debug_file.txt"))) {
    file.remove(paste0(tempdir(), "/actel_debug_file.txt"))
  }
# ------------------------

# debug lines
  if (getOption("actel.debug", default = FALSE)) { # nocov start
    # show debug log location
    on.exit(event(type = "screen",
                  "Debug: Progress log available at ",
                  gsub("\\\\", "/", paste0(tempdir(),
                                           "/actel_debug_file.txt"))))
    # show debug rdata location
    on.exit(add = TRUE,
            event(type = "screen",
                  "Debug: Saving carbon copy to ",
                  gsub("\\\\", "/", paste0(tempdir(),
                                           "/actel.debug.RData"))))
    # save debug rdata
    on.exit(add = TRUE,
            save(list = ls(),
                 file = paste0(tempdir(), "/actel.debug.RData")))
    event(type = c("screen", "report"),
          "!!!--- Debug mode has been activated ---!!!")
  } # nocov end
# ------------------------

# check arguments quality
  if (!is.null(datapack))
    checkToken(token = attributes(datapack)$actel.token,
               timestamp = attributes(datapack)$timestamp)

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
                        back.warning = back.warning,
                        back.error = back.error,
                        inactive.warning = inactive.warning,
                        inactive.error = inactive.error,
                        exclude.tags = exclude.tags,
                        override = override,
                        print.releases = print.releases,
                        replicates = replicates,
                        section.order = section.order,
                        detections.y.axis = detections.y.axis)

  min.per.event <- aux$min.per.event
  speed.method <- aux$speed.method
  speed.warning <- aux$speed.warning
  speed.error <- aux$speed.error
  jump.warning <- aux$jump.warning
  jump.error <- aux$jump.error
  back.warning <- aux$back.warning
  back.error <- aux$back.error
  inactive.warning <- aux$inactive.warning
  inactive.error <- aux$inactive.error
  detections.y.axis <- aux$detections.y.axis
  rm(aux)

  GUI <- checkGUI(GUI, save.tables.locally)
# ------------------------

# Store function call
  the_function_call <- paste0("migration(",
    parse_arg(tz), ", ",
    parse_arg(section.order), ", ",
    parse_arg(datapack, 
              arg_val = deparse(substitute(datapack))), ", ",
    parse_arg(success.arrays), ", ",
    parse_arg(max.interval), ", ",
    parse_arg(min.total.detections), ", ",
    parse_arg(min.per.event), ", ",
    parse_arg(stop.time), ", ",
    parse_arg(speed.method), ", ",
    parse_arg(speed.warning), ", ",
    parse_arg(speed.error), ", ",
    parse_arg(jump.warning), ", ",
    parse_arg(jump.error), ", ",
    parse_arg(back.warning), ", ",
    parse_arg(back.error), ", ",
    parse_arg(inactive.warning), ", ",
    parse_arg(inactive.error), ", ",
    parse_arg(exclude.tags), ", ",
    parse_arg(override), ", ",
    parse_arg(report), ", ",
    parse_arg(auto.open), ", ",
    parse_arg(discard.orphans), ", ",
    parse_arg(discard.first), ", ",
    parse_arg(save.detections), ", ",
    parse_arg(replicates), ", ",
    parse_arg(disregard.parallels), ", ",
    parse_arg(GUI), ", ",
    parse_arg(save.tables.locally), ", ",
    parse_arg(print.releases), ", ",
    parse_arg(detections.y.axis), ")")

  event(type = "debug", the_function_call)
# --------------------

# Prepare clean-up before function ends
  finished_unexpectedly <- TRUE
  on.exit(add = TRUE,
          if (interactive() & finished_unexpectedly) {
            event(type = "crash", the_function_call)
          })

  if (!getOption("actel.debug", default = FALSE)) {
    on.exit(add = TRUE,
            deleteHelpers())
  }

  on.exit(add = TRUE,
          tryCatch(sink(), warning = function(w) {hide <- NA}))
# --------------------------------------

# Final arrangements before beginning
  the_time <- Sys.time()
  event(type = "report",
        "Actel R package report.\n",
        "Version: ", utils::packageVersion("actel"), "\n",
        "Target folder: ", getwd(), "\n",
        "Timestamp: ", the_time, "\n",
        "Function: migration()\n")

  report <- checkReport(report = report)
# -----------------------------------

# Load, structure and check the inputs
  if (is.null(datapack)) {
    study.data <- loadStudyData(tz = tz, override = override,
                                save.detections = save.detections,
                                start.time = start.time, stop.time = stop.time,
                                discard.orphans = discard.orphans,
                                section.order = section.order, 
                                exclude.tags = exclude.tags, 
                                disregard.parallels = disregard.parallels)
  } else {
   event(type = c("screen", "report"),
         "M: Running analysis on preloaded data (compiled on ",
         attributes(datapack)$timestamp, ").")
   event(type = "report",
         "Messages displayed during preload:\n-------------------\n",
         paste0(attributes(datapack)$loading_messages, collapse = "\n"),
         "\n-------------------")

    study.data <- datapack
    tz <- study.data$tz
    disregard.parallels <- study.data$disregard.parallels
  }

  bio <- study.data$bio
  deployments <- study.data$deployments
  spatial <- study.data$spatial
  dot_list <- study.data$dot_list
  paths <- study.data$paths
  dist.mat <- study.data$dist.mat
  attributes(dist.mat)$speed.method <- speed.method
  detections.list <- study.data$detections.list

# -------------------------------------

# Final quality checks

  # Verify that replicate information is valid
  not_null <- !is.null(replicates)
  aux <- names(dot_list$array_info$arrays)
  any_mismatch <- not_null && any(is.na(match(names(replicates), aux)))
  if (any_mismatch) {
    event(type = "stop",
          "Some of the array names listed in the 'replicates' argument",
          " do not match the study's arrays.")
  }

  capture <- lapply(names(replicates), function(i) {
    x <- replicates[[i]]
    all.stations <- spatial$stations$Standard.name[spatial$stations$Array == i]
    link <- !x %in% all.stations
    if (any(link)) {
      event(type = "stop",
            "In replicates: Station",
            ifelse(sum(link) > 1, "s ", " "),
            paste(x[link], collapse = ", "),
            ifelse(sum(link) > 1, " are", " is"),
            " not part of ", i, " (available stations: ",
            paste(all.stations, collapse = ", "), ").")
    }
  })

  check <- sapply(dot_list$array_info$arrays, function(x) is.null(x$parallel))
  if (any(!check)) {
    if (disregard.parallels) {
      event(type = c("screen", "report"),
            "M: 'disregard.parallels' is set to TRUE; the presence of",
            " parallel arrays will not invalidate efficiency peers.")
    } else {
      event(type = c("screen", "report"),
            "M: 'disregard.parallels' is set to FALSE; the presence of",
            " parallel arrays can potentially invalidate efficiency peers.")
    }
  }

  if (is.null(success.arrays)) {
    not_after <- lapply(dot_list$array_info$arrays, function(x) {
      is.null(x$after)
    })
    no_after <- unlist(not_after)
    success.arrays <- names(dot_list$array_info$arrays)[no_after]
    if (length(success.arrays) == 1) {
      event(type = c("warning", "screen", "report"),
            "'success.arrays' was not defined. Assuming success if the",
            " tags are last detected at array ", success.arrays, ".")
    } else {
      event(type = c("warning", "screen", "report"),
            "'success.arrays' was not defined. Assuming success if the",
            " tags are last detected at arrays ",
            paste(success.arrays[-length(success.arrays)], collapse = ", "),
            " or ", tail(success.arrays, 1), ".")
    }
  } else {
    link <- is.na(match(success.arrays, unlist(spatial$array.order)))
    if (any(link)) {
      event(type = "stop",
            ifelse(sum(link) > 1, "Arrays '", "Array '"),
            paste(success.arrays[link], collapse = "', '"),
            ifelse(sum(link) > 1, "' are ", "' is "),
            "listed in the 'success.arrays' argument, but ",
            ifelse(sum(link) > 1, "these arrays are", "this array is"),
            " not part of the study arrays.")
    }
  }

# -------------------------------------

# Discard early detections, if required
  if (!is.null(discard.first) && discard.first > 0) {
    detections.list <- discardFirst(input = detections.list,
                                    bio = bio, trim = discard.first)
  }

# Compile array movements
  event(type = c("screen", "report"),
        "M: Creating movement records for the valid tags.")
  movements <- groupMovements(detections.list = detections.list, bio = bio, 
                              spatial = spatial, speed.method = speed.method,
                              max.interval = max.interval, tz = tz,
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
    event(type = c("screen", "report"),
          "M: Not calculating time/speed from release to first",
          " detection because 'discard.first' was set.")
  }

  event(type = c("screen", "report"),
        "M: Checking movement events quality.")

  do.checkSpeeds <- FALSE
  if (is.null(speed.warning)) {
    event(type = c("warning", "screen", "report"),
          "'speed.warning'/'speed.error' were not set, skipping speed checks.")
  } else {
    if(attributes(dist.mat)$valid) {
      do.checkSpeeds <- TRUE
    } else {
      event(type = c("warning", "screen", "report"),
            "'speed.warning'/'speed.error' were set, but a valid distance",
            " matrix is not present. Aborting speed checks.")
    }
  }

  do.checkInactiveness <- FALSE
  if (is.null(inactive.warning)) {
    event(type = c("warning", "screen", "report"),
          "'inactive.warning'/'inactive.error' were not set,",
          " skipping inactivity checks.")
  } else {
    if (!attributes(dist.mat)$valid) {
      event(type = c("report", "Screen", "Warning"),
            "Running inactiveness checks without a distance matrix.",
            " Performance may be limited.")
    }
    do.checkInactiveness <- TRUE
  }

  # this will be used further down to reinstate the names in the movements list.
  movement.names <- names(movements)

  # clean override based on movements
  if (is.numeric(override)) {
    link <- !override %in% extractSignals(movement.names)
    trigger_override_warning <- any(link)
  } else {
    link <- !override %in% movement.names
    trigger_override_warning <- any(link)
  }

  if (trigger_override_warning) {
    event(type = c("warning", "screen", "report"),
          "Override has been triggered for ",
          ifelse(sum(link) == 1, "tag ", "tags "),
          paste(override[link], collapse = ", "), " but ",
          ifelse(sum(link) == 1, "this signal was", "these signals were"),
          " not detected.")
    override <- override[!link]
  }

  # convert numeric override to full tag override to prevent problems downstream
  if (is.numeric(override)) {
    override <- movement.names[match(override, extractSignals(movement.names))]
  }

  movements <- lapply(seq_along(movements), function(i) {
    tag <- names(movements)[i]
    counter <- paste0("(", i, "/", length(movements), ")")

    event(type = "debug", "Checking movement quality for tag ", tag,".")

    if (is.na(match(tag, override))) {
      output <- checkMinimumN(movements = movements[[tag]], tag = tag,
                              min.total.detections = min.total.detections,
                              min.per.event = min.per.event[1], n = counter)

      output <- checkFirstMove(movements = output, tag = tag,
                               detections = detections.list[[tag]], 
                               spatial = spatial, bio = bio,
                               dot_list = dot_list, GUI = GUI,
                               save.tables.locally = save.tables.locally,
                               n = counter)

      output <- checkImpassables(movements = output, tag = tag, bio = bio,
                                 detections = detections.list[[tag]],
                                 n = counter, spatial = spatial,
                                 dot_list = dot_list, GUI = GUI,
                                 save.tables.locally = save.tables.locally)

      output <- checkJumpDistance(movements = output, bio = bio, tag = tag,
                                  dot_list = dot_list, spatial = spatial,
                                  jump.warning = jump.warning, 
                                  jump.error = jump.error, GUI = GUI,
                                  detections = detections.list[[tag]], 
                                  save.tables.locally = save.tables.locally,
                                  n = counter)

      output <- checkBackwards(movements = output, bio = bio, tag = tag,
                               dot_list = dot_list, spatial = spatial, 
                               back.warning = back.warning,
                               back.error = back.error, GUI = GUI,
                               detections = detections.list[[tag]],
                               save.tables.locally = save.tables.locally,
                               n = counter)

      if (do.checkSpeeds) {
        temp.valid.movements <- simplifyMovements(movements = output, tag = tag,
                                                  bio = bio,
                                                  discard.first = discard.first,
                                                  speed.method = speed.method,
                                                  dist.mat = dist.mat)
        output <- checkSpeeds(movements = output, tag = tag,
                              valid.movements = temp.valid.movements,
                              detections = detections.list[[tag]],
                              speed.warning = speed.warning, n = counter,
                              speed.error = speed.error, GUI = GUI,
                              save.tables.locally = save.tables.locally)
        rm(temp.valid.movements)
      }

      if (do.checkInactiveness) {
        output <- checkInactiveness(movements = output, tag = tag,
                                    detections = detections.list[[tag]],
                                    n = counter,
                                    inactive.warning = inactive.warning,
                                    inactive.error = inactive.error,
                                    dist.mat = dist.mat, GUI = GUI,
                                    save.tables.locally = save.tables.locally)
      }
    } else {
      output <- overrideChecks(moves = movements[[tag]],
                               detections = detections.list[[tag]],
                               n = counter, tag = tag, GUI = GUI,
                               save.tables.locally = save.tables.locally)# nocov
    }
    return(output)
  })
  names(movements) <- movement.names
  rm(movement.names)
# -------------------------

# Compile section movements
  event(type = c("screen", "report"),
        "M: Compiling and checking section movements for the valid tags.")

  secmoves <- lapply(seq_along(movements), function(i) {
    tag <- names(movements)[i]
    counter <- paste0("(", i, "/", length(movements), ")")
    event(type = "debug", "Compiling section movements for tag ", tag,".")

    aux <- sectionMovements(movements = movements[[i]], spatial = spatial,
                            valid.dist = attributes(dist.mat)$valid)

    if (!is.null(aux)) {
      # don't run the minimum total detections check here (i.e. set it to 0);
      # that's already done when compiling the array movements.
      output <- checkMinimumN(movements = aux, tag = tag, min.total.detections = 0,
                              min.per.event = min.per.event[2], n = counter)
      return(output)
    } else {
      return(NULL)
    }
  })
  names(secmoves) <- names(movements)
  secmoves <- secmoves[!sapply(secmoves, is.null)]
  
  # Update array movements based on section movements validity
  movements <- updateValidity(arrmoves = movements,
                              secmoves = secmoves)

  # compile valid movements
  event(type = c("screen", "report"),
        "M: Filtering valid array movements.")
  valid.movements <- assembleValidMoves(movements = movements, bio = bio,
                                        discard.first = discard.first,
                                        speed.method = speed.method,
                                        dist.mat = dist.mat)

  event(type = c("screen", "report"),
        "M: Filtering valid section movements.")
  secmoves <- assembleValidSecMoves(valid.moves = valid.movements,
                                    spatial = spatial,
                                    valid.dist = attributes(dist.mat)$valid)

  event(type = c("screen", "report"),
        "M: Compiling migration timetable.")
  timetable <- assembleTimetable(secmoves = secmoves, 
                                 valid.moves = valid.movements, 
                                 all.moves = movements, spatial = spatial,
                                 dot_list = dot_list, bio = bio, tz = tz, 
                                 dist.mat = dist.mat, 
                                 speed.method = speed.method,
                                 success.arrays = success.arrays)

  status.df <- assembleOutput(timetable = timetable, bio = bio,
                              spatial = spatial,
                              dist.mat = dist.mat, tz = tz)

  event(type = c("screen", "report"),
        "M: Compiling summary information tables.")
  section.overview <- assembleSectionOverview(status.df = status.df,
                                              secmoves = secmoves,
                                              dot_list = dot_list)

  aux <- list(valid.movements = valid.movements, spatial = spatial,
              rsp.info = list(bio = bio, analysis.type = "migration"))
  times <- getTimes(input = aux, move.type = "array",
                    event.type = "arrival", n.events = "first")
  rm(aux)

  event(type = "Screen",
        "M: Validating detections.")
  recipient <- validateDetections(detections.list = detections.list,
                                  movements = valid.movements)
  detections <- recipient$detections
  valid.detections <- recipient$valid.detections
  rm(recipient)

# -------------------------------------

# CJS stuff
  the.matrices <- assembleMatrices(spatial = spatial,
                                   movements = valid.movements,
                                   status.df = status.df,
                                   dot_list = dot_list)
  # keep only the minimum matrix
  the.matrices <- the.matrices[[2]]

  m.by.array <- breakMatricesByArray(m = the.matrices, dot_list = dot_list,
                                     type = "peers")

  if (is.null(m.by.array[[1]])) {
    calculate.efficiency <- FALSE
    event(type = c("warning", "screen", "report"),
          "Aborting inter-array efficiency calculations",
          " (will limit the report's output).")
  } else {
    calculate.efficiency <- TRUE
  }

  if (calculate.efficiency) {
    event(type = c("screen", "report"),
          "M: Calculating array efficiency.")

    CJS.list <- lapply(m.by.array, function(m) {
      if (length(m) == 1)
        simpleCJS(m[[1]])
      else
        combineCJS(m)
    })

    release_nodes <- as.data.frame(table(bio$Group, bio$Release.site))
    colnames(release_nodes) <- c("Group", "Release.site", "n")
    link <- match(release_nodes$Release.site,
                  spatial$release.sites$Standard.name)
    release_nodes$Array <- spatial$release.sites$Array[link]
    release_nodes$Combined <- paste(release_nodes[, 1], release_nodes[, 2],
                                    sep = ".")

    overall.CJS <- assembleArrayCJS(mat = the.matrices, CJS = CJS.list,
                                    dot_list = dot_list, releases = release_nodes,
                                    silent = FALSE)

    if (!is.null(replicates)) {
      intra_mats <- getDualMatrices(replicates = replicates,
                                    CJS = overall.CJS,
                                    spatial = spatial,
                                    detections.list = valid.detections)
      recipient <- includeIntraArrayEstimates(m = intra_mats, CJS = overall.CJS)
      overall.CJS <- recipient$CJS
      intra.array.CJS <- recipient$intra.CJS
      rm(recipient)
    } else {
      intra_mats <- NULL
      intra.array.CJS <- NULL
    }

    aux <- mbSplitCJS(mat = m.by.array,
                      fixed.efficiency = overall.CJS$efficiency)
    aux <- aux[names(the.matrices)]
    split.CJS <- assembleSplitCJS(mat = the.matrices, CJS = aux,
                                  dot_list = dot_list, releases = release_nodes,
                                  intra.CJS = intra.array.CJS)

    release.overview <- lapply(names(split.CJS), 
                               function(i, releases = spatial$release.sites) {
      output <- split.CJS[[i]]
      aux <- unlist(stringr::str_split(i, "\\.", 2))
      rows <- releases$Standard.name == aux[2]
      cols <- paste0("n.", aux[1])
      output$Release <- rep(c(releases[rows, cols], NA, NA), 2)
      output <- output[, c(ncol(output), 1:(ncol(output) - 1))]
      return(output)
    })
    names(release.overview) <- names(aux)
    rm(aux)

    aux <- mbGroupCJS(mat = m.by.array, status.df = status.df,
                      fixed.efficiency = overall.CJS$efficiency)
    group.CJS <- assembleGroupCJS(mat = the.matrices, CJS = aux,
                                  dot_list = dot_list, releases = release_nodes,
                                  intra.CJS = intra.array.CJS)
    group.overview <- lapply(names(group.CJS),
                             function(i, releases = spatial$release.sites) {
      output <- group.CJS[[i]]
      aux <- unlist(stringr::str_split(i, "\\.", 2))
      cols <- paste0("n.", aux[1])
      output$Release <- rep(c(sum(releases[, cols]), NA, NA), 2)
      output <- output[, c(ncol(output), 1:(ncol(output) - 1))]
      return(output)
    })
    names(group.overview) <- names(aux)
    rm(aux)
  } else {
    overall.CJS <- NULL

    if (!is.null(replicates)) {
      event(type = c("screen", "report"),
            "M: Calculating intra-array efficiency.")
      intra_mats <- getDualMatrices(replicates = replicates, CJS = overall.CJS,
                                    spatial = spatial,
                                    detections.list = valid.detections)
      intra.array.CJS <- includeIntraArrayEstimates(m = intra_mats,
                                                    CJS = overall.CJS)
      # keep intra.CHS object only
      intra.array.CJS <- intra.array.CJS$intra.CJS
    } else {
      intra_mats <- NULL
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
  rsp.info <- list(analysis.type = "migration", analysis.time = the_time,
                   bio = bio, tz = tz,
                   actel.version = utils::packageVersion("actel"))

  if (!is.null(override)) {
    override.fragment <- paste0('<span style="color:red">Manual mode has been',
                                ' triggered for **', length(override),
                                '** tag(s).</span>\n')
  } else {
    override.fragment <- ""
  }

  if (file.exists(resultsname <- "actel_migration_results.RData")) {
    continue <- TRUE
    index <- 1
    while (continue) {
      resultsname <- paste0("actel_migration_results.", index, ".RData")
      if (file.exists(resultsname)) {
        index <- index + 1
      } else {
        continue <- FALSE
      }
    }
    rm(continue, index)
  }

  if (interactive()) { # nocov start
    decision <- userInput(paste0("Would you like to save a copy of",
                                 " the results to ", resultsname, "?(y/n) "),
                          choices = c("y", "n"),
                          hash = "# save results?")
  } else { # nocov end
    decision <- "n"
  }

  if (decision == "y") { # nocov start
    event(type = c("screen", "report"),
          "M: Saving results as '", resultsname, "'.")
    # These changes of name are here for consistency with earlier versions of
    # actel. The exported names can be updated once we make the big revamp
    # in coding style (actel 2.0).
    section.movements <- secmoves
    intra.array.matrices <- intra_mats
    save(detections, valid.detections, spatial, deployments, dot_list,
         movements, valid.movements, section.movements, status.df,
         section.overview, group.overview, release.overview, matrices,
         overall.CJS, intra.array.matrices, intra.array.CJS, times,
         rsp.info, dist.mat, file = resultsname)
  } else { # nocov end
    event(type = c("screen", "report"),
          "M: Skipping saving of the results.")
  }
  rm(decision)

# ------------

# Print graphics
  trigger.report.error.message <- TRUE
  if (report) {
    event(type = c("screen", "report"),
          "M: Producing the report.")
    on.exit(add = TRUE,
      if (trigger.report.error.message) {
        event(type = "screen",
              "M: Producing the report failed. If you have saved a copy of",
              " the results, you can reload them using dataToList().")
      })

    if (dir.exists(paste0(tempdir(), "/actel_report_auxiliary_files"))) {
      unlink(paste0(tempdir(), "/actel_report_auxiliary_files"),
             recursive = TRUE)
    }

    dir.create(paste0(tempdir(), "/actel_report_auxiliary_files"))

    if (!getOption("actel.debug", default = FALSE)) {
      on.exit(add = TRUE,
              unlink(paste0(tempdir(), "/actel_report_auxiliary_files"),
                     recursive = TRUE))
    }

    biometric.fragment <- printBiometrics(bio = bio)

    efficiency.fragment <- printEfficiency(CJS = overall.CJS,
                                           intra.CJS = intra.array.CJS,
                                           type = "migration")

    printDotplots(status.df = status.df,
                  valid.dist = attributes(dist.mat)$valid)

    printSurvivalGraphics(section.overview = section.overview,
                          status.df = status.df)

    printLastArray(status.df = status.df)

    printDot(dot = dot_list$dot,
             spatial = spatial,
             print.releases = print.releases)

    if (calculate.efficiency) {
      printProgression(dot = dot_list$dot,
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

    check <- sapply(valid.detections, function(x) any(!is.na(x$Sensor.Value)))
    if (any(check)) {
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
  event(type = "report",
       "M: Analysis completed!\n\n-------------------")

  comments <- paste(tempdir(), "temp_comments.txt", sep = "/")
  if (file.exists(comments)) { # nocov start
    aux <- readr::read_file(comments)
    aux <- gsub("\r", "", aux)
    aux <- gsub("\t", ": ", aux)
    event(type = "report",
          "User comments:\n-------------------\n",
          aux, "-------------------")
  } # nocov end
  uds <- paste(tempdir(), "temp_UD.txt", sep = "/")
  if (file.exists(uds)) { # nocov start
    aux <- readr::read_file(uds)
    aux <- gsub("\r", "", aux)
    event(type = "report",
          "User interventions:\n-------------------\n",
          aux, "-------------------")
  } # nocov end

  if (!is.null(datapack)) {
    event(type = "report",
          "Preload function call:\n-------------------\n",
          attributes(datapack)$function_call, "\n-------------------")
  }

 event(type = "report",
       "Migration function call:\n-------------------\n",
       the_function_call, "\n-------------------")
# ------------------

# print html report
  if (report) {
    if (file.exists(reportname <- "actel_migration_report.html")) {
      continue <- TRUE
      index <- 1
      while (continue) {
        reportname <- paste0("actel_migration_report.", index, ".html")
        if(file.exists(reportname)) {
          index <- index + 1
        } else {
          continue <- FALSE
        }
      }
      event(type = "screen",
            "M: An actel report is already present in the current directory.\n",
            "   Saving new report as ", reportname, ".")
      rm(continue, index)
    } else {
      event(type = "screen",
            "M: Saving actel report as 'actel_migration_report.html'.")
    }

    event(type = "debug", "Printing report rmd")
    printMigrationRmd(override.fragment = override.fragment,
                      biometric.fragment = biometric.fragment,
                      section.overview = section.overview,
                      efficiency.fragment = efficiency.fragment,
                      display.progression = display.progression,
                      array.overview.fragment = array.overview.fragment,
                      individual.plots = individual.plots,
                      circular.plots = circular.plots,
                      sensor.plots = sensor.plots,
                      spatial = spatial,
                      deployments = deployments,
                      valid.detections = valid.detections,
                      detections = detections,
                      detections.y.axis = detections.y.axis)

    event(type = "debug", "Converting report to html")
    rmarkdown::render(input = paste0(tempdir(),
                                     "/actel_report_auxiliary_files/",
                                     "actel_migration_report.Rmd"),
                      output_dir = paste0(tempdir(),
                                          "/actel_report_auxiliary_files"),
                      quiet = !getOption("actel.debug", default = FALSE),
                      clean = !getOption("actel.debug", default = FALSE))


    event(type = "debug", "Moving report")
    file.copy(from = paste0(tempdir(),
                            "/actel_report_auxiliary_files/",
                            "actel_migration_report.html"),
              to = reportname)
    if (interactive() & auto.open) { # nocov start
      event(type = "debug", "Opening report.")
      browseURL(reportname)
    } # nocov end
  }
  trigger.report.error.message <- FALSE
# ------------------

  jobname <- paste0(gsub(" |:", ".",
                         as.character(Sys.time())),
                    ".actel.log.txt")

  if (interactive() & !report) { # nocov start
    decision <- userInput(paste0("Would you like to save a copy of the",
                                 " analysis log to ", jobname, "?(y/n) "),
                          choices = c("y", "n"),
                          hash = "# save job log?")
  } else { # nocov end
    decision <- "n"
  }
  if (decision == "y" | decision == "Y") { # nocov start
    event(type = "Screen",
          "M: Saving job log as '",jobname, "'.")
    file.copy(from = paste(tempdir(), "temp_log.txt", sep = "/"),
              to = jobname)
  } # nocov end

  output <- list(detections = detections,
                 valid.detections = valid.detections,
                 spatial = spatial,
                 deployments = deployments,
                 dot_list = dot_list,
                 movements = movements,
                 valid.movements = valid.movements,
                 section.movements = secmoves,
                 status.df = status.df,
                 section.overview = section.overview,
                 group.overview = group.overview,
                 release.overview = release.overview,
                 matrices = matrices,
                 overall.CJS = overall.CJS,
                 intra.array.matrices = intra_mats,
                 intra.array.CJS = intra.array.CJS,
                 times = times,
                 rsp.info = rsp.info,
                 dist.mat = dist.mat)

  event(type = "screen",
        "M: Analysis completed!")
  finished_unexpectedly <- FALSE

  return(output)
}

#' Print Rmd report
#'
#' Creates a Rmd report and converts it to html.
#'
#' @param override.fragment Rmarkdown string specifying the type of report for
#'        the header.
#' @param biometric.fragment Rmarkdown string specifying the biometric graphics
#'        drawn.
#' @param section.overview A summary table with the number of tags that
#'        disappeared/moved onwards at each section.
#' @param efficiency.fragment Rmarkdown string specifying the efficiency
#'        results.
#' @param display.progression Logical. If TRUE, the progression plot has been
#'        created and can be displayed.
#' @param array.overview.fragment Rmarkdown string specifying the array
#'        overview results.
#' @param individual.plots Rmarkdown string specifying the name of the
#'        individual plots.
#' @param circular.plots Rmarkdown string specifying the name of the
#'        circular plots.
#' @param sensor.plots Rmarkdown string specifying the name of the sensor
#'        plots.
#' @inheritParams loadDetections
#'
#' @return No return value, called for side effects.
#'
#' @keywords internal
#'
printMigrationRmd <- function(override.fragment, biometric.fragment,
                              section.overview, efficiency.fragment,
                              display.progression, array.overview.fragment,
                              individual.plots,
                              circular.plots, sensor.plots, spatial,
                              deployments, valid.detections,
                              detections, detections.y.axis){
  event(type = "debug", "Running printMigrationRmd.")

  work.path <- paste0(tempdir(), "/actel_report_auxiliary_files/")

 if (!is.null(spatial$unknowns)) {
    unknown.fragment <- paste0('<span style="color:red"> Number of relevant',
                               ' unknown receivers: **',
                               sum(sapply(spatial$unknowns, length)),
                               '** (of which ',
                               length(spatial$unknowns$included),
                               ' were included)</span>\n')
  } else {
    unknown.fragment <- ""
  }
  if (!is.null(sensor.plots)) {
    sensor.fragment <- paste0("### Sensor plots\n\nNote:\n",
                              "  : The colouring in these plots will follow",
                              " that of the individual detection plots, which",
                              " can be modified using `detections.y.axis`.\n",
                              "  : The data used for these graphics are stored",
                              " in the `valid.detections` object.\n",
                              "  : You can replicate these graphics and edit",
                              " them as needed using the `plotSensors()`",
                              " function.\n\n",
                              "<center>\n", sensor.plots, "\n</center>")
  } else {
    sensor.fragment <- NULL
  }

  report <- readr::read_file(paste0(tempdir(), "/temp_log.txt"))
  report <- gsub("(\\\\|\")", "\\\\\\1", report)

  if (file.exists(paste0(tempdir(), '/temp_warnings.txt'))) {
    warning.messages <- readr::read_file(paste0(tempdir(),
                                                '/temp_warnings.txt'))
    warning.messages <- gsub("\\r", "", warning.messages)
    warning.messages <- gsub("(\\\\|\")", "\\\\\\1", warning.messages)
  } else {
    warning.messages <- 'No warnings were raised during the analysis.'
  }

  if (file.exists(paste0(tempdir(), '/temp_comments.txt'))) {
    comment.fragment <- readr::read_file(paste0(tempdir(),
                                                '/temp_comments.txt'))
    comment.fragment <- gsub("\\r", "", comment.fragment)
    comment.fragment <- gsub("(\\\\|\")", "\\\\\\1", comment.fragment)
  } else {
    comment.fragment <- 'No comments were included during the analysis.'
  }

  oldoptions <- options(knitr.kable.NA = "-")
  on.exit(options(oldoptions), add = TRUE)

  sink(paste0(work.path, "actel_migration_report.Rmd"))
  cat(paste0(
    "---\n",
    "title: \"Acoustic telemetry migration analysis\"\n",
    "author: \"Actel R package (", utils::packageVersion("actel"), ")\"\n",
    "output:\n",
    "  html_document:\n",
    "    includes:\n",
    "      after_body: ", work.path, "toc_menu_migration.html\n",
    "---\n",
    "\n",
    "### Summary\n",
    "\n",
    "Target folder: ",
      stringr::str_extract(pattern = "(?<=Target folder: )[^\r|^\n]*",
                           string = report), "\n",
    "\n",
    "Timestamp: **",
      stringr::str_extract(pattern = "(?<=Timestamp: )[^\r|^\n]*",
                           string = report), "**\n",
    "\n",
    "Number of target tags: **`r I(nrow(status.df))`**\n",
      override.fragment, "\n",
    "\n",
    "Number of listed receivers: **",
      stringr::str_extract(pattern = "(?<=Number of ALS: )[0-9]*",
                           string = report), 
      "** (of which **", 
      stringr::str_extract(pattern = "(?<=of which )[0-9]*",
                           string = report), 
      "** had no detections)\n",
    "\n",
    unknown.fragment, "\n",
    "\n",
    "Data time range: ",
      stringr::str_extract(pattern = "(?<=Data time range: )[^\r|^\n]*",
                           string = report), "\n",
    "\n",
    "Percentage of post-release valid detections: ", 
      round(sum(unlist(lapply(valid.detections, nrow))) / 
            sum(unlist(lapply(detections, nrow))) * 100, 2), "%\n",
    "\n",
    "Found a bug? [**Report it here.**]",
      "(https://github.com/hugomflavio/actel/issues)\n",
    "\n",
    "Want to cite actel in a publication? Run `citation(\"actel\")`\n",
    "\n",
    "### Study area\n",
    "\n",
    "Arrays with the same background belong to the same section. Release sites",
    " are marked with \"R.S.\". Arrays connected with an arrow indicate that",
    " the animals can only pass in one direction.\n",
    "\n",
    "<img src=", work.path, ifelse(file.exists(paste0(work.path,
                                                      "mb_arrays.svg")),
                                   "mb_arrays.svg",
                                   "mb_arrays.png"),
    " style=\"padding-top: 15px;\"/>\n",
    "\n",
    "### Receiver stations\n",
    "\n",
    paste(knitr::kable(spatial$stations, row.names = FALSE),
          collapse = "\n"), "\n",
    "\n",
    "### Deployments\n",
    "\n",
    paste(knitr::kable(deployments, row.names = FALSE), collapse = "\n"), "\n",
    "\n",
    "### Release sites\n",
    "\n",
    paste(knitr::kable(spatial$release.sites, row.names = FALSE),
          collapse = "\n"), "\n",
    "\n",
    "### Array forward efficiency\n",
    "\n",
    efficiency.fragment, "\n",
    "\n",
    "### Warning messages\n",
    "\n",
    "```{r warnings, echo = FALSE, comment = NA}\n",
    "cat(\"", warning.messages, "\")\n",
    "```\n",
    "\n",
    "### User comments\n",
    "\n",
    "```{r comments, echo = FALSE, comment = NA}\n",
    "cat(\"", comment.fragment, "\")\n",
    "```\n",
    "\n",
    ifelse(biometric.fragment == "", 
           "", 
           paste0("### Biometric graphics\n",
                  "\n",
                  "Note:\n",
                  "  : The data used in this graphic is the data present in",
                  " the biometrics.csv file.\n",
                  "\n",
                  "<center>\n",
                  biometric.fragment,
                  "</center>\n")
           ), "\n",
    "\n",
    "### Section Summary\n",
    "\n",
    "Note:\n",
    ": The data used in this table and graphics are stored in the",
    " `section.overview` and `status.df` objects.\n",
    "\n",
    paste(knitr::kable(section.overview), collapse = "\n"), "\n",
    "\n",
    "<center>\n",
    "![](survival1.png){ width=90% }\n",
    "![](survival2.png){ width=90% }\n",
    "![](survival3.png){ width=90% }\n",
    "</center>\n",
    "\n",
    "### Last Seen Arrays\n",
    "\n",
    "Note:\n",
    "  : The data used in this graphic are stored in the `status.df` object",
    " ('Very.last.array' column).\n",
    "\n",
    "<center>\n",
    "![](last_arrays.png){ width=90% }\n",
    "</center>\n",
    "\n",
    "### Progression\n",
    "\n",
    ifelse(display.progression,
           paste0("Zoom in or open the figure in a new tab to clearly read the",
                  " text within each circle.\n",
                  "\n",
                  "Note:\n",
                  "  : The progression calculations **do not account for**",
                  " backwards movements. This implies that the total number of",
                  " animals to have been **last seen** at a given array **may",
                  " be lower** than the displayed below. Please refer to the",
                  " [section survival overview](#section-survival) and [last",
                  " seen arrays](#last-seen-arrays) to find out how many",
                  " animals were considered to have disappeared per section.\n",
                  "  : The data used in this graphic are stored in the",
                  " `overall.CJS` object, and the data used in the tables is",
                  " stored in the `group.overview` object. You can find",
                  " detailed progressions per release site in the",
                  " `release.overview` object.\n",
                  "\n",
                  "<img src=",
                  work.path,
                  ifelse(file.exists(paste0(work.path,"mb_efficiency.svg")),
                         "mb_efficiency.svg",
                         "mb_efficiency.png"),
                  " style=\"padding-top: 15px; padding-bottom: 15px;\"/>\n",
                  "\n"),
           paste0("Progression cannot be displayed if efficiencies are not",
                  " calculated. See full log for more details.")),
    array.overview.fragment, "\n",
    "\n",
    "### Time of arrival at each Array\n",
    "\n",
    "Note:\n",
    "  : Coloured lines on the outer circle indicate the mean value for each",
    " group and the respective ranges show the standard error of the mean.",
    " Each group's bars sum to 100%. The number of data points in each group",
    " is presented between brackets in the legend of each pannel.\n",
    "  : You can replicate these graphics and edit them as needed using the",
    " `plotTimes()` function.\n",
    "  : The data used in these graphics are stored in the `times` object.\n",
    "  : To obtain reports with the legacy linear circular scale, run",
    " `options(actel.circular.scale = \"linear\")` before running",
    " your analyses.\n",
    "\n",
    "<center>\n",
    circular.plots,
    "\n",
    "</center>\n",
    "\n",
    "### Dotplots\n",
    "\n",
    "Note:\n",
    "  : The **top** 10% of the values for each panel are marked in **red**.\n",
    "  : The **bottom** 10% of the values for each panel",
    " are marked in **blue**.\n",
    "  : The columns starting with \"To\" should be read as either \"Average",
    " time to ...\" or \"Average speed to ...\", depending on the unit used.",
    " The columns starting with \"In\" should be read as \"Total time in",
    " ...\". These reductions were made to keep the column headers as short",
    " as possible.\n",
    "  : The data used in these graphics are stored in the `status.df`",
    " object.\n",
    "\n",
    "<center>\n",
    "![](", work.path, "dotplots.png){ width=95% }\n",
    "</center>\n",
    "\n",
    "### Individual detection plots\n",
    "\n",
    "Note:\n",
    "  : You can choose to plot detections by station or by array using the",
    " `detections.y.axis` argument.\n",
    "  : The detections are coloured by ", 
    ifelse(detections.y.axis == "stations", "array", "section"), ".",
    " The vertical black dashed line shows the time of release. The vertical",
    " grey dashed lines show the assigned moments of entry and exit for each",
    " study area section. The full dark-grey line shows the movement events",
    " considered valid, while the dashed dark-grey line shows the movement",
    " events considered invalid.\n",
    ifelse(detections.y.axis == "stations", 
           paste0("  : The movement event lines move straight between the",
                  " first and last station of each event (i.e. in-between",
                  " detections will not be individually linked by the",
                  " line).\n"),
           ""),
    "  : Manually **edited** tag detections are highlighted with",
    " **yellow** graphic borders.\n",
    "  : Manually **overridden** tag detections are highlighted with",
    " **red** graphic borders.\n",
    "  : The ", 
    ifelse(detections.y.axis == "stations", "stations", "arrays"), 
    " have been aligned by ",
    ifelse(detections.y.axis == "stations", "array", "section"),
    ", following the order provided ",
    ifelse(detections.y.axis == "stations", "", "either "),
    "in the spatial input",
    ifelse(detections.y.axis == "stations", ".",
           " or the `section.order` argument."), "\n",
    "  : You can replicate these graphics and edit them as needed using the",
    " `plotDetections()` function.\n",
    "  : You can also see the movement events of multiple tags simultaneously",
    " using the `plotMoves()` function.\n",
    "  : The data used in these graphics are stored in the `detections` and",
    " `movements` objects (and respective valid counterparts).\n",
    "\n",
    "<center>\n",
    individual.plots,
    "\n",
    "</center>\n",
    "\n",
    sensor.fragment, "\n",
    "\n",
    "### Full log\n",
    "\n",
    "```{r log, echo = FALSE, comment = NA}\n",
    "cat(\"", gsub("\\r", "", report), "\")\n",
    "```\n",
    "\n"
  ), fill = TRUE)
  sink()

  sink(paste0(work.path, "toc_menu_migration.html"))
  cat(paste0(
    "<style>\n",
    "h3 {\n",
    "  padding-top: 25px;\n",
    "  padding-bottom: 15px;\n",
    "}\n",
    "\n",
    "h4 {\n",
    "  padding-top: 25px;\n",
    "  padding-bottom: 15px;\n",
    "}\n",
    "\n",
    "img[src*=\"#diagram\"] {\n",
    "  width = 100%;\n",
    "  padding-top = 200px;\n",
    "  padding-bottom = 15px;\n",
    "}\n",
    "\n",
    "/* The sidebar menu */\n",
    ".sidenav {\n",
    "  height: 100%;\n",
    "  width: 110px;\n",
    "  position: fixed;\n",
    "  z-index: 1;\n",
    "  top: 0;\n",
    "  left: 0;\n",
    "  background-color: #fcfcfc;\n",
    "  overflow-x: hidden;\n",
    "  padding-top: 20px;\n",
    "}\n",
    "\n",
    "/* The navigation menu links */\n",
    ".sidenav a {\n",
    "  padding: 6px 8px 6px 16px;\n",
    "  text-decoration: none;\n",
    "  /*font-size: 25px;*/\n",
    "  color: #818181;\n",
    "  display: block;\n",
    "}\n",
    "\n",
    ".sidenav p {\n",
    "  padding: 6px 8px 6px 16px;\n",
    "  text-decoration: none;\n",
    "  font-size: 25px;\n",
    "  color: #818181;\n",
    "  display: block;\n",
    "}\n",
    "\n",
    ".sidenav a:hover {\n",
    "  background-color: #52a548;\n",
    "  color: #f1f1f1;\n",
    "}\n",
    "\n",
    ".fluid-row {\n",
    "  margin-left: 110px; /* Same as the width of the sidebar */\n",
    "  padding: 0px 10px;\n",
    "}\n",
    "\n",
    ".section {\n",
    "  margin-left: 110px; /* Same as the width of the sidebar */\n",
    "  padding: 0px 10px;\n",
    "}\n",
    "\n",
    ".level4 {\n",
    "  margin-left: 0px; /* Same as the width of the sidebar */\n",
    "  padding: 0px 0px;\n",
    "}\n",
    "\n",
    "/* On smaller screens, where height is less than 450px, change the\n",
    " * style of the sidebar (less padding and a smaller font size)\n",
    " */\n",
    "@media screen and (max-height: 450px) {\n",
    "  .sidenav {padding-top: 15px;}\n",
    "  .sidenav a {font-size: 18px;}\n",
    "}\n",
    "</style>\n",
    "\n",
    "<div class=\"sidenav\">\n",
    "  <p>Index:</p>\n",
    "  <a href=\"#summary\">Summary</a>\n",
    "  <a href=\"#study-area\">Study area</a>\n",
    "  <a href=\"#receiver-stations\">Stations</a>\n",
    "  <a href=\"#deployments\">Deployments</a>\n",
    "  <a href=\"#release-sites\">Release sites</a>\n",
    "  <a href=\"#array-forward-efficiency\">Array efficiency</a>\n",
    "  <a href=\"#warning-messages\">Warnings</a>\n",
    "  <a href=\"#user-comments\">Comments</a>\n",
    ifelse(biometric.fragment == "", 
           "", 
           "  <a href=\"#biometric-graphics\">Biometrics</a>\n"),
    "  <a href=\"#section-summary\">Section survival</a>\n",
    "  <a href=\"#last-seen-arrays\">Last seen</a>\n",
    "  <a href=\"#progression\">Progression</a>\n",
    "  <a href=\"#time-of-arrival-at-each-array\">Arrival times</a>\n",
    "  <a href=\"#dotplots\">Dotplots</a>\n",
    "  <a href=\"#individual-detection-plots\">Individual detections</a>\n",
    ifelse(is.null(sensor.fragment),
           "",
           "  <a href=\"#sensor-plots\">Sensor data</a>\n"),
    "  <a href=\"#full-log\">Full log</a>\n",
    "</div>\n"
  ), fill = TRUE)
  sink()
}

#' Create the timetable
#'
#' Crawls trough the movement events of each tag to find when it entered and
#' left each section of the study area.
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
#' @return A data frame containing the entering and leaving timestamps
#'         for each section per target tag
#'
#' @keywords internal
#'
assembleTimetable <- function(secmoves, valid.moves, all.moves, spatial,
                              dot_list, bio, tz, dist.mat, speed.method,
                              success.arrays) {
  event(type = "debug", "Running assembleTimetable.")

  # NOTE: The NULL variables below are actually column names used by data.table.
  # This definition is just to prevent the package check from issuing a note due
  # to unknown variables.
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

        if (grepl("first$", speed.method)) {
          time_end <- aux$First.time[1]
          station_end <- gsub(" ", ".", aux$First.station[1])
          # the gsub is just to match the column name in the distances matrix
        } else {
          time_end <- aux$Last.time[1]
          station_end <- gsub(" ", ".", aux$Last.station[1])
          # same as above
        }

        time_spent <- as.vector(difftime(time_end, origin.time, units = "secs"))
        dist_went <- dist.mat[origin.place, station_end]

        aux$Speed.until[1] <- round(dist_went/time_spent, 6)

      }


      if (nrow(aux) > 1) {
        capture <- lapply(2:nrow(aux), function(i) {

          if (grepl("^first", speed.method)) {
            time_start <- aux$First.time[i - 1]
            station_start <- aux$First.station[i - 1]
          } else {
            time_start <- aux$Last.time[i - 1]
            station_start <- aux$Last.station[i - 1]
          }

          if (grepl("first$", speed.method)) {
            time_end <- aux$First.time[i]
            # the gsub is just to match the column name in the distances matrix
            station_end <- gsub(" ", ".", aux$First.station[i])
          } else {
            time_end <- aux$Last.time[i]
            # same as above
            station_end <- gsub(" ", ".", aux$Last.station[i])
          }

          time_spent <- as.vector(difftime(time_end, time_start,
                                           units = "secs"))
          dist_went <- dist.mat[station_start, station_end]

          aux$Speed.until[i] <<- round(dist_went/time_spent, 6)
          # we don't care about this lapply output. See <<- above.
          return(NULL)
        })
      }
      return(aux)
    })
    names(secmoves) <- aux
    rm(aux)
  }

  sections <- names(dot_list$section_info$sections)

  # Create the timetable
  recipient <- vector()
  if (attributes(dist.mat)$valid) {
    for (i in seq_len(length(sections))) {
      recipient <- c(recipient, 
                     paste(c("Times.entered", "Average.time.until", 
                             "Average.speed.to", "First.array", "First.station",
                             "First.arrived", "Average.time.in",
                             "Average.speed.in", "Last.array", "Last.station",
                             "Last.left", "Total.time.in"),
                           sections[i], sep = "."))
    }
  } else {
    for (i in seq_len(length(sections))) {
      recipient <- c(recipient, 
                     paste(c("Times.entered", "Average.time.until",
                             "First.array", "First.station", "First.arrived",
                             "Average.time.in", "Last.array", "Last.station",
                             "Last.left", "Total.time.in"),
                           sections[i], sep = "."))
    }
  }
  recipient <- c(recipient, "Last.section", "Very.last.array", "Very.last.time", "Status",
                 "Valid.detections", "Valid.events", "Invalid.detections",
                 "Invalid.events", "Backwards.movements",
                 "Max.cons.back.moves", "P.type")

  if (attributes(dist.mat)$valid && speed.method == "last to last") {
    recipient <- recipient[!grepl("Average\\.speed\\.in", recipient)]
  }

  timetable <- matrix(nrow = length(secmoves), ncol = length(recipient))
  timetable <- as.data.frame(timetable)

  colnames(timetable) <- recipient
  rm(recipient)
  rownames(timetable) <- names(secmoves)

  # Start filling it up
  capture <- lapply(names(secmoves), function(tag) {
    # cat(tag, "\n")
    aux <- split(secmoves[[tag]], secmoves[[tag]]$Section)
    event(type = "debug", "Assembling timetable values for tag ", tag, ".")
    recipient <- lapply(seq_along(aux), function(i) {
      # cat(i, "\n")
      recipient <- rep(NA, ncol(timetable))
      names(recipient) <- colnames(timetable)
      recipient <- t(as.data.frame(recipient))

      total.time <- apply(aux[[i]][, c("First.time", "Last.time")], 1, 
                          function(x) difftime(x[2], x[1], units = "secs"))
      
      the_col <- paste0("Total.time.in.", names(aux)[i])
      recipient[1, the_col] <- sum(total.time)
      
      the_col <- paste0("Average.time.in.", names(aux)[i])
      recipient[1, the_col] <- mean(total.time)

      the_col <- paste0("Times.entered.", names(aux)[i])
      recipient[1, the_col] <- nrow(aux[[i]])

      the_col <- paste0("First.array.", names(aux)[i])
      recipient[1, the_col] <- aux[[i]]$First.array[1]
      
      the_col <- paste0("First.station.", names(aux)[i])
      recipient[1, the_col] <- aux[[i]]$First.station[1]
      
      the_col <- paste0("First.arrived.", names(aux)[i])
      recipient[1, the_col] <- as.character(aux[[i]]$First.time[1])

      the_col <- paste0("Last.array.", names(aux)[i])
      recipient[1, the_col] <- aux[[i]][.N, Last.array]
      
      the_col <- paste0("Last.station.", names(aux)[i])
      recipient[1, the_col] <- aux[[i]][.N, Last.station]
      
      the_col <- paste0("Last.left.", names(aux)[i])
      recipient[1, the_col] <- as.character(aux[[i]][.N, Last.time])

      if (attributes(dist.mat)$valid && speed.method == "last to first") {
        the_col <- paste0("Average.speed.in.", names(aux)[i])
        recipient[1, the_col] <- round(mean(aux[[i]]$Speed.in.section.m.s), 6)
      }

      the_col <- paste0("Average.time.until.", names(aux)[i])
      recipient[1, the_col] <- mean(decimalTime(aux[[i]]$Time.travelling,
                                                unit = "s"))

      if (attributes(dist.mat)$valid) {
        the_col <- paste0("Average.speed.to.", names(aux)[i])
        recipient[1, the_col] <- round(mean(aux[[i]]$Speed.until), 6)
      }

      return(recipient)
    })
    recipient <- as.data.frame(combine(recipient), stringsAsFactors = FALSE)

    # convert numbers to numeric and replace NAs where relevant
    the_cols <- which(grepl("Times\\.entered\\.", colnames(recipient)))
    recipient[, the_cols] <- as.numeric(recipient[, the_cols])
    recipient[, the_cols[which(is.na(recipient[, the_cols]))]] <- 0

    the_cols <- which(grepl("Average\\.speed\\.to\\.", colnames(recipient)))
    recipient[, the_cols] <- as.numeric(recipient[, the_cols])

    the_cols <- which(grepl("Average\\.speed\\.in\\.", colnames(recipient)))
    recipient[, the_cols] <- as.numeric(recipient[, the_cols])
    # --

    recipient$Last.section <- secmoves[[tag]][.N, Section]
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

    aux <- countBackMoves(movements = valid.moves[[tag]], dot_list = dot_list)
    recipient$Backwards.movements <- aux[[1]]
    recipient$Max.cons.back.moves <- aux[[2]]
    recipient$P.type <- attributes(valid.moves[[tag]])$p.type

    # assign fate
    the.last.section <- secmoves[[tag]][.N, Section]
    the.last.array <- secmoves[[tag]][.N, Last.array]

    edge.array <- dot_list$array_info$arrays[[the.last.array]]$edge

    if (edge.array) {
      neighbours <- dot_list$array_info$arrays[[the.last.array]]$neighbours
      other_sections <- sort(unique(unlist(sapply(neighbours, function(x) {
        dot_list$array_info$arrays[[x]]$section
      }))))
      possible_sections <- unique(c(the.last.section, other_sections))
      if (length(possible_sections) == 2) {
        recipient$Status <- paste0("Disap. between ", possible_sections[1],
                                   " and ", possible_sections[2])
      }      
      if (length(possible_sections) == 3) {
        recipient$Status <- paste0("Disap. between ", possible_sections[1],
                                   ", ", possible_sections[2],
                                   ", and ", possible_sections[3])
      }      
      if (length(possible_sections) >= 4) {
        recipient$Status <- paste0("Disap. between ", possible_sections[1],
                                   " and ", length(possible_sections) - 1,
                                   " other sections.")
      }      
    } else {
      recipient$Status <- paste0("Disap. in ", the.last.section)      
    }

    if(!is.na(match(the.last.array, success.arrays))) {
      recipient$Status <- "Succeeded"
    }

    # deploy values
    event(type = "debug", "Deploy timetable values for tag ", tag, ".")
    timetable[tag, ] <<- recipient

    return(NULL)
  })

  # Convert time and timestamp data
  for (section in sections) {
    cols <- paste0(c("Average.time.until.", "Average.time.in.",
                     "Total.time.in."),
                   section)
    for (the_col in cols) {
      # convert to numeric
      timetable[, the_col] <- as.numeric(timetable[, the_col])
      # grab the mean for later use
      aux <- mean(timetable[, the_col], na.rm = TRUE)
      # convert to difftime
      timetable[, the_col] <- as.difftime(timetable[, the_col], units = "secs")
      units(timetable[, the_col]) <- "secs"
      if (!is.nan(aux)) {
        if (aux > 86400)
          units(timetable[, the_col]) <- "days"
        if (aux <= 86400 & aux > 3600)
          units(timetable[, the_col]) <- "hours"
        if (aux <= 3600)
          units(timetable[, the_col]) <- "mins"
      }
      timetable[, the_col] <- round(timetable[, the_col], 3)
    }
    cols <- paste0(c("First.arrived.", "Last.left."),
                   section)
    for (the_col in cols) {
      # convert to numeric posix
      timetable[, the_col] <- as.POSIXct(timetable[, the_col], tz = tz)
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
#'  \item \code{sum.back.moves} The number of backwards movements for
#'        the target tag
#'  \item \code{max.back.moves} The maximum number of consecutive backwards
#'        movements for the target tag
#' }
#'
#' @keywords internal
#'
countBackMoves <- function(movements, dot_list){
  event(type = "debug", "Running countBackMoves.")
  if (nrow(movements) > 1) {# Determine number of backwards movements
    aux <- data.frame(
      A = movements$Array[-nrow(movements)],
      B = movements$Array[-1])
    backwards.movements <- apply(aux, 1, function(x)
      if(x[1] != x[2])
        is.na(match(x[2], dot_list$array_info$arrays[[x[1]]]$all.after))
      else
        FALSE
      )
    sum.back.moves <- sum(backwards.movements)
    if (sum.back.moves > 0) {
      aux <- rle(backwards.movements)
      max.back.moves <- max(aux$lengths[which(aux$values)])
    } else {
      max.back.moves <- 0
    }
  } else {
    sum.back.moves <- 0
    max.back.moves <- 0
  }
  return(list(sum.back.moves = sum.back.moves,
              max.back.moves = max.back.moves))
}

#' Create status.df
#'
#' Combines the timetable and the original biometrics.
#'
#' @inheritParams explore
#' @inheritParams migration
#' @param timetable A table of the entering and leaving points for each
#'                  section per target tag, created by assembleTimetable.
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
  event(type = "debug", "Running assembleOutput.")
  status.df <- merge(bio, timetable, by = "Transmitter", all = TRUE)

  event(type = "debug", "Completing entries for tags that were never detected.")
  sections <- names(spatial$array.order)

  status.df$Last.section[is.na(status.df$Last.section)] <- "Release"
  status.df$Last.section <- factor(status.df$Last.section, 
                                   levels = c("Release", sections))
  status.df$Very.last.array[is.na(status.df$Very.last.array)] <- "Release"
  status.df$Very.last.array <- factor(status.df$Very.last.array,
                                     levels = c("Release",
                                                levels(spatial$stations$Array)))
  status.df$Status[is.na(status.df$Status)] <- "Never detected"
  status.df$P.type[is.na(status.df$P.type)] <- "Skipped"
  status.df$Valid.detections[is.na(status.df$Valid.detections)] <- 0
  status.df$Invalid.detections[is.na(status.df$Invalid.detections)] <- 0

  event(type = "debug", "Appending comments.")
  if (file.exists(paste0(tempdir(), "/temp_comments.txt"))) { # nocov start
    temp <- read.table(paste0(tempdir(), "/temp_comments.txt"),
                       header = FALSE, sep = "\t")
    status.df[, "Comments"] <- NA_character_
    for (i in seq_len(nrow(temp))) {
      link <- match(temp[i, 1], status.df$Transmitter)
      if (is.na(status.df$Comments[link])) {
        status.df$Comments[link] <- paste(temp[i, 2])
      } else {
        status.df$Comments[link] <- paste(status.df$Comments[link],
                                          temp[i, 2], sep = "// ")
      }
    }
  } # nocov end
  event(type = "debug", "Done.")
  return(status.df)
}

#' Create section.overview
#'
#' Produces a table with the survival per group of animals present in the
#' biometrics.
#'
#' @inheritParams explore
#' @inheritParams migration
#' @inheritParams simplifyMovements
#' @inheritParams sectionMovements
#'
#' @return A data frame containing the survival per group of animals present
#'         in the biometrics.
#'
#' @keywords internal
#'
assembleSectionOverview <- function(status.df, secmoves, dot_list) {
  event(type = "debug", "Running assembleSectionOverview.")

  sections <- names(dot_list$section_info$sections)
  x <- matrix(0,
              nrow = length(sections) * 3,
              ncol = length(levels(status.df$Group)))
  x <- as.data.frame(x)
  rownames(x) <- paste0(c("n_entered_", "times_entered_", "last_at_"),
                        rep(sections, each = 3))
  colnames(x) <- levels(status.df$Group)
  x

  for (i in names(secmoves)) {
    col <- as.character(status.df$Group[status.df$Transmitter == i])
    times_entered <- secmoves[[i]]$Section
    for (s in times_entered) {
      row <- paste0("times_entered_", s)
      x[row, col] <- x[row, col] + 1
    }
    n_entered <- unique(secmoves[[i]]$Section)
    for (s in n_entered) {
      row <- paste0("n_entered_", s)
      x[row, col] <- x[row, col] + 1
    }
    last_at <- tail(secmoves[[i]]$Section, 1)
    row <- paste0("last_at_", last_at)
    x[row, col] <- x[row, col] + 1
  }
  x

  aux <- as.data.frame(table(status.df$Group))
  rownames(aux) <- aux$Var1
  above <- t(aux[,-1, drop = FALSE])
  rownames(above) <- "n_total"
  aux <- as.data.frame(table(status.df$Group[status.df$Status == "Succeeded"]))
  rownames(aux) <- aux$Var1
  below <- t(aux[,-1, drop = FALSE])
  rownames(below) <- "succeeded"

  section.overview <- rbind(above, x, below)

  return(section.overview)
}
