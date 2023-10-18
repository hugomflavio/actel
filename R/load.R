#' Load, structure and check the inputs
#'
#' @inheritParams explore
#' @inheritParams migration
#'
#' @return A list containing:
#' \itemize{
#'  \item \code{bio}: A data frame corresponding to 'biometrics.csv'
#'  \item \code{sections}: A vector of the study area sections (or NULL in the case of the `explore` function)
#'  \item \code{deployments}: A list corresponding to 'deployments.csv'
#'  \item \code{spatial}: A list corresponding to 'spatial.csv'
#'  \item \code{dot}: A data frame containing the connections between arrays
#'  \item \code{arrays}: A list containing detailed information on the arrays
#'  \item \code{dotmat}: A matrix of the distance (in number of arrays) between pairs of arrays
#'  \item \code{dist.mat}: A matrix of the distances (in metres) between stations (if a 'distances.csv' is present)
#'  \item \code{detections.list}: A list containing the detection data for each tag
#'  \item \code{paths}: A list of the all array paths between each pair of arrays.
#' }
#'
#' @keywords internal
#'
loadStudyData <- function(tz, override = NULL, start.time, stop.time, save.detections = FALSE,
  section.order = NULL, exclude.tags, disregard.parallels = TRUE, discard.orphans = FALSE) {

  loading.failed <- TRUE
  # debug lines
    if (getOption("actel.debug", default = FALSE)) { # nocov start
      on.exit({if (loading.failed) message("Debug: Function failed during loading. Saving load environment to ", gsub("\\\\", "/", paste0(tempdir(), "/actel.load.debug.RData")))}, add = TRUE)
      on.exit({if (loading.failed) save(list = ls(), file = paste0(tempdir(), "/actel.load.debug.RData"))}, add = TRUE)
    } # nocov end
  # ------------------------

  appendTo(c("Screen", "Report"), "M: Importing data. This process may take a while.")
  bio <- loadBio(input = "biometrics.csv", tz = tz)
  appendTo(c("Screen", "Report"), paste0("M: Number of target tags: ", nrow(bio), "."))

  # Check that all the overridden tags are part of the study
  if (!is.null(override)) {
    if (is.numeric(override)) {

      # Legacy signal-only override compatibility
      lowest_signals <- sapply(bio$Signal, function(i) min(as.numeric(unlist(strsplit(as.character(i), "|", fixed = TRUE)))))
      
      if (any(table(lowest_signals[lowest_signals %in% override]) > 1))
        stopAndReport('Override is numeric but there are multiple tags that match the overridden signal. Use codespace-signal overrides instead.')

      if (any(link <- is.na(match(override, lowest_signals))))
        stopAndReport("Some tags listed in 'override' (", paste0(override[link], collapse = ", "), ") are not listed in the biometrics file.")
    } 
    else {
      # new override based on full tag
      if (any(link <- is.na(match(override, bio$Transmitter))))
        stopAndReport("Some tags listed in 'override' (", paste0(override[link], collapse = ", "), ") are not listed in the biometrics file.")

    }
  }

  deployments <- loadDeployments(input = "deployments.csv", tz = tz)
  checkDeploymentTimes(input = deployments) # check that receivers are not deployed before being retrieved
  
  spatial <- loadSpatial(input = "spatial.csv", section.order = section.order)

  deployments <- checkDeploymentStations(input = deployments, spatial = spatial) # match Station.name in the deployments to Station.name in spatial, and vice-versa
  deployments <- createUniqueSerials(input = deployments) # Prepare serial numbers to overwrite the serials in detections

  detections <- loadDetections(start.time = start.time, stop.time = stop.time, tz = tz,
    save.detections = save.detections, record.source = TRUE)
  detections <- checkDupDetections(input = detections)

  use.fakedot <- TRUE
  if (file.exists("spatial.dot")) {
    appendTo(c("Screen", "Report"), "M: A 'spatial.dot' file was detected, activating multi-branch analysis.")
    recipient <- loadDot(input = "spatial.dot", spatial = spatial, disregard.parallels = disregard.parallels)
    use.fakedot <- FALSE
  }
  if (use.fakedot & file.exists("spatial.txt")) {
    appendTo(c("Screen", "Report"), "M: A 'spatial.txt' file was detected, activating multi-branch analysis.")
    recipient <- loadDot(input = "spatial.txt", spatial = spatial, disregard.parallels = disregard.parallels)
    use.fakedot <- FALSE
  }
  if (use.fakedot) {
    n <- length(unique(spatial$Array[spatial$Type == "Hydrophone"]))
    if (n > 1) {
      fakedot <- paste(unique(spatial$Array[spatial$Type == "Hydrophone"]), collapse = "--")
    } else {
      aux <- unique(spatial$Array[spatial$Type == "Hydrophone"])
      fakedot <- paste(aux, "--", aux)
    }
    recipient <- loadDot(string = fakedot, spatial = spatial, disregard.parallels = disregard.parallels)
  }
  dot <- recipient$dot
  arrays <- recipient$arrays
  dotmat <- recipient$dotmat
  paths <- recipient$paths
  if (is.null(dot) | is.null(arrays) | is.null(dotmat) | is.null(paths))
    stopAndReport("Something went wrong when assigning recipient objects (loadDot). If this error persists, contact the developer.") # nocov

  rm(use.fakedot, recipient)

  # Check if there is a logical first array in the study area, should a replacement release site need to be created.
  if (sum(unlist(lapply(arrays, function(a) is.null(a$before)))) == 1)
    first.array <- names(arrays)[unlist(lapply(arrays, function(a) is.null(a$before)))]
  else
    first.array <- NULL
  spatial <- transformSpatial(spatial = spatial, bio = bio, arrays = arrays, dotmat = dotmat, first.array = first.array) # Finish structuring the spatial file

  detections <- createStandards(detections = detections, spatial = spatial, deployments = deployments, discard.orphans = discard.orphans) # get standardized station and receiver names, check for receivers with no detections
  checkUnknownReceivers(input = detections) # Check if there are detections from unknown receivers
  appendTo(c("Screen","Report"), paste0("M: Data time range: ", as.character(head(detections$Timestamp, 1)), " to ", as.character(tail(detections$Timestamp, 1)), " (", tz, ")."))

  arrays <- liveArrayTimes(arrays = arrays, deployments = deployments, spatial = spatial)

  # Reorder arrays object by spatial order
  link <- match(unlist(spatial$array.order), names(arrays))
  arrays <- arrays[link] 
  rm(link)

  dist.mat <- loadDistances(spatial = spatial) # Load distances and check if they are valid

  recipient <- splitDetections(detections = detections, bio = bio, exclude.tags = exclude.tags) # Split the detections by tag, store full transmitter names in bio
  detections.list <- recipient$detections.list
  bio <- recipient$bio
  if (is.null(detections.list) | is.null(bio))
    stopAndReport("Something went wrong when assigning recipient objects (splitDetections). If this error persists, contact the developer.") # nocov
  rm(recipient)

  recipient <- checkTagsInUnknownReceivers(detections.list = detections.list, deployments = deployments, spatial = spatial) # Check if there is any data loss due to unknown receivers
  spatial <- recipient$spatial
  deployments <- recipient$deployments
  detections.list <- recipient$detections.list
  if (is.null(spatial) | is.null(deployments) | is.null(detections.list))
    stopAndReport("Something went wrong when assigning recipient objects (unknownReceivers). If this error persists, contact the developer.")# nocov
  rm(recipient)

  detections.list <- checkDetectionsBeforeRelease(input = detections.list, bio = bio, discard.orphans = discard.orphans)
  appendTo(c("Screen", "Report"), "M: Data successfully imported!")

  loading.failed <- FALSE

  return(list(bio = bio, 
              deployments = deployments, 
              spatial = spatial, 
              dot = dot,
              arrays = arrays, 
              dotmat = dotmat, 
              dist.mat = dist.mat,
              detections.list = detections.list, 
              paths = paths))
}

#' Load spatial.dot
#'
#' @param input The name of a file containing dot connections.
#' @param spatial A spatial data frame.
#' @param string A string of dot connections.
#' @param preloading Logical: Is this function being called from preload()?
#' @inheritParams migration
#'
#' @return A list containing:
#' \itemize{
#'  \item \code{dot}: A data frame containing the connections between arrays
#'  \item \code{arrays}: A list containing detailed information on the arrays
#'  \item \code{dotmat}: A matrix of the distance (in number of arrays) between pairs of arrays
#'  \item \code{paths}: A list of the all array paths between each pair of arrays.
#' }
#'
#' @keywords internal
#'
loadDot <- function(string = NULL, input = NULL, spatial, disregard.parallels, preloading = FALSE) {
  appendTo("debug", "Running loadDot.")
  if (is.null(string) & is.null(input))
    stopAndReport("No dot file or dot string were specified.")
  if (is.null(string)) {
    tryCatch(dot <- readDot(input = input, silent = TRUE),
      error = function(e) stopAndReport("The contents of the '", input, "' file could not be recognised by the readDot function."))
  } else {
    dot <- readDot(string = string, silent = TRUE)
  }
  mat <- dotMatrix(input = dot)
  unique.arrays <- unique(unlist(sapply(spatial$Array, function(x) unlist(strsplit(x, "|", fixed = TRUE)))))
  if (any(link <- is.na(match(unique.arrays, colnames(mat))))) {
    if (preloading) {
      stopAndReport(paste0("Not all arrays listed in the spatial input are present in the dot input. Double-check that the array names in the dot string match the array names in the spatial input.\n       Missing arrays: ", paste(unique.arrays[link], collapse = ", "), "\n"))
    } else {
      if (file.exists("spatial.txt"))
        stopAndReport(paste0("Not all the arrays listed in the spatial.csv file are present in the spatial.txt.\n       Missing arrays: ", paste(unique.arrays[link], collapse = ", "), "\n"))
      if (file.exists("spatial.dot"))
        stopAndReport(paste0("Not all the arrays listed in the spatial.csv file are present in the spatial.dot.\n       Missing arrays: ", paste(unique.arrays[link], collapse = ", "), "\n"))
      if (!file.exists("spatial.txt") & !file.exists("spatial.dot"))
        stopAndReport("Something went wrong when compiling the dot input. Try restarting R and trying again. If the problem persists, contact the developer.")
    }
  }
  if (any(link <- is.na(match(colnames(mat), unique.arrays)))) {
    if (preloading) {
      stopAndReport(paste0("Not all arrays listed in the dot input are present in the spatial input. The dot input should only contain arrays that are listed in spatial.\n       Alien arrays: ", paste(colnames(mat)[link], collapse = ", "), "\n"))
    } else {
      if (file.exists("spatial.txt"))
        stopAndReport(paste0("Some arrays listed in the spatial.txt file are not present in the spatial.csv file. The dot input should only contain arrays that are listed in spatial.\n       Alien arrays: ", paste(colnames(mat)[link], collapse = ", "), "\n"))
      if (file.exists("spatial.dot"))
        stopAndReport(paste0("Some arrays listed in the spatial.dot file are not present in the spatial.csv file. The dot input should only contain arrays that are listed in spatial.\n       Alien arrays: ", paste(colnames(mat)[link], collapse = ", "), "\n"))
      if (!file.exists("spatial.txt") & !file.exists("spatial.dot"))
        stopAndReport("Something went wrong when compiling the dot input. Try restarting R and trying again. If the problem persists, contact the developer.")
    }
  }
  arrays <- dotList(input = dot, spatial = spatial)
  arrays <- dotPaths(input = arrays, disregard.parallels = disregard.parallels)
  shortest.paths <- findShortestChains(input = arrays)
  return(list(dot = dot, arrays = arrays, dotmat = mat, paths = shortest.paths))
}

#' Read dot file or string
#'
#' @inheritParams loadDot
#' @param silent Logical: Should warnings be suppressed?
#'
#' @examples
#' # create dummy dot string
#' x1 <- c("A--B--C--D--E--F")
#'
#' # run readDot
#' readDot(string = x1)
#'
#' # more complex strings are acceptable:
#' x2 <- c(
#' "A--B--C--D--E--F
#' A--G--H--I--E
#' H--C")
#'
#' readDot(string = x2)
#'
#' # Alternatively, connections can be read from a file
#'
#' # let's create a dummy file in R's temporary directory:
#' write("A--B--C--D--E--F\nA--G--H--I--E\nH--C\n",
#'  file = paste0(tempdir(), "/dummy_dot.txt"))
#'
#' # now we can read it using readDot
#' readDot(input = paste0(tempdir(), "/dummy_dot.txt"))
#'
#' @return A data frame with the connections present in the input.
#'
#' @export
#'
readDot <- function (input = NULL, string = NULL, silent = FALSE) {
  if (is.null(string) & is.null(input))
    stop("No dot file or data were specified.")
  if (is.null(string)) {
    if (!file.exists(input))
      stop("Could not find a '", input, "' file in the working directory.")
    lines <- readLines(input)
  } else {
    lines <- unlist(strsplit(string, "\n|\t|\r\n"))
  }
  paths <- lines[grepl("[<-][->]", lines)]
  if (length(paths) == 0)
    stop("Could not recognise the input contents as DOT formatted connections.", call. = FALSE)
  # if something looks like a badly formatted connector, complain and stop
  if (any(grepl("<<|>>|>-|-<|><|<>|<->", paths)))
    stop("The input appears to have badly formatted connectors ('<<', '>>', '>-', '-<'', '><', '<>' or '<->'). Please fix these before continuing.", call. = FALSE)
  # there's probably a smarter way to do these, but hey, this works.
  paths <- gsub("[ ]*<-", "<-", paths)
  paths <- gsub("<-[ ]*", "<-", paths)
  paths <- gsub("[ ]*->", "->", paths)
  paths <- gsub("->[ ]*", "->", paths)
  paths <- gsub("[ ]*--", "--", paths)
  paths <- gsub("--[ ]*", "--", paths)
  paths <- gsub("[;]", "", paths)
  paths <- gsub("\\[label=[^\\]]","", paths)
  paths <- gsub("^[ ]*", "", paths)
  paths <- gsub("[ ]*$", "", paths)
  # only spaces left now should be part of array names, and need to be replaced
  if (any(grepl(" ", paths))) {
    if (!silent)
      warning("Replacing spaces with '_' in the node names.", immediate. = TRUE, call. = FALSE)
    paths <- gsub(" ", "_", paths)
  }
  if (any(grepl("\\\\|/|:|\\*|\\?|\\\"|<(?!-)|(?<!-)>|\\\'", paths, perl = TRUE))) {
    if (!silent)
      warning("Troublesome characters found in the node names (\\/:*?\"<>\'). Replacing these with '_'.", immediate. = TRUE, call. = FALSE)
    paths <- gsub("\\\\|/|:|\\*|\\?|\\\"|<(?!-)|(?<!-)>|\\\'", "_", paths, perl = TRUE)
  }
  nodes <- strsplit(paths,"[<-][->]")
  recipient <- data.frame(
    A = character(),
    to = character(),
    B = character(),
    stringsAsFactors = FALSE)
  for (i in 1:length(nodes)) {
    n <- length(nodes[[i]])
    escaped_nodes <- gsub(pattern = "([[:punct:]])", replacement = "\\\\\\1", nodes[[i]])
    type <- gsub(paste0(escaped_nodes, collapse = "|"), "", paths[[i]])
    aux <- data.frame(
      A = nodes[[i]][1:(n - 1)],
      to = sapply(seq(from = 1, to = nchar(type), by = 2), function(i) substr(type, i, i + 1)),
      B = nodes[[i]][2:n],
      stringsAsFactors = FALSE)
    recipient <- rbind(recipient, aux)
  }
  return(recipient)
}

#' Create numerical distances between dot elements
#'
#' @param input a dot data frame
#'
#' @return A matrix of the distance (in number of arrays) between pairs of arrays
#'
#' @keywords internal
#'
dotMatrix <- function(input) {
  appendTo("debug", "Running dotMatrix.")
  nodes <- unique(unlist(input[, c(1, 3)]))
  graph <- matrix(0, length(nodes), length(nodes), dimnames = list(nodes, nodes))
  if (any(input$to != "--" & input$to != "<-" & input$to != "->"))
    stopAndReport("Unrecognized connectors. Only use '--', '->' or '<-' to connect nodes.\n")
  for (i in 1:nrow(input)) {
    if (input$to[i] == "--") {
      graph[input$A[i], input$B[i]] <- 1
      graph[input$B[i], input$A[i]] <- 1
    }
    if (input$to[i] == "->")
      graph[input$A[i], input$B[i]] <- 1
    if (input$to[i] == "<-")
      graph[input$B[i], input$A[i]] <- 1
  }
  for (i in 1:(length(nodes)-1)) {
    for (A in nodes) {
      for (B in nodes) {
        if (graph[A, B] == i) {
          # cat(B, "\n")
          candidates <- rownames(graph) != B & rownames(graph) != A & graph[B, ] == 1
          if (any(candidates)) {
            to.change <- names(candidates)[candidates]
            for (j in to.change) {
              if (graph[A, j] == 0) {
                # cat(" - ", j, "\n")
                graph[A, j] <- graph[A, j] + 1 + i
              }
            }
          }
        }
      }
    }
  }
  graph[graph == 0] <- NA
  for(i in 1:nrow(graph))
    graph[i, i] <- 0
  return(graph)
}

#' Break the dot data frame into a list
#'
#' @param input a dot data frame
#' @param spatial The spatial data frame
#'
#' @return  A list containing detailed information on the arrays
#'
#' @keywords internal
#'
dotList <- function(input, spatial) {
  appendTo("debug", "Running dotList.")

  # if there are sections, determine which connections are at the edge between sections
  if (any(grepl("^Section$", colnames(spatial)))) {
    sections <- levels(spatial$Section)
    input$SectionA <- rep(NA_character_, nrow(input))
    input$SectionB <- rep(NA_character_, nrow(input))
    for (section in sections) {
      aux <- spatial$Array[spatial$Section == section]
      input$SectionA[matchl(input$A, aux)] <- section
      input$SectionB[matchl(input$B, aux)] <- section
    }
    input$Edge <- with(input, SectionA != SectionB)
  }

  arrays <- list()
  for (a in unique(c(t(input[, c(1, 3)])))) { # go through each unique array
    auxA <- input[input$A == a, ]
    auxA <- auxA[auxA$to != "<-", ]
    auxB <- input[input$B == a, ]
    auxB <- auxB[auxB$to != "->", ]
    # find parallel arrays (i.e. both before and after)
    par.trigger <- FALSE
    parallel <- sapply(auxA[, 3], function(x) match(x, auxB[, 1]))
    if (any(!is.na(parallel))) {
      # discount those from the before and after, and trigger parallel inclusion
      auxA <- auxA[-which(!is.na(parallel)), , drop = FALSE]
      auxB <- auxB[-parallel[!is.na(parallel)], , drop = FALSE]
      par.trigger <- TRUE
    }
    recipient <- list(
      neighbours = unique(c(auxA$B, auxB$A, names(parallel)[!is.na(parallel)])),
      before     = if (nrow(auxB) == 0) { NULL  } else { unique(auxB$A) },
      after      = if (nrow(auxA) == 0) { NULL  } else { unique(auxA$B) },
      parallel   = if ( !par.trigger  ) { NULL  } else { unique(names(parallel)[!is.na(parallel)]) },
      edge       = if (nrow(auxA) == 0) { FALSE } else { any(auxA$Edge) })
    arrays[[length(arrays) + 1]] <- recipient
    names(arrays)[length(arrays)] <- a
  }
  return(arrays)
}

#' Find arrays valid for efficiency calculation
#'
#' @param input A dot list
#' @inheritParams migration
#'
#' @return A list of the all array paths between each pair of arrays.
#'
#' @keywords internal
#'
dotPaths <- function(input, disregard.parallels) {
  appendTo("debug", "Running dotPaths.")

  for (direction in (c("before", "after"))) {
    capture <- lapply(names(input), function(a) {
      input[[a]][[paste0(direction, ".peers")]] <<- findPeers(array = a, array.list = input, peer.direction = direction, disregard.parallels = disregard.parallels)
      recipient <- findDirectChains(array = a, array.list = input, direction = direction)
      input[[a]][[paste0("all.", direction)]] <<- recipient[[1]]
      input[[a]][[paste0("all.", direction, ".and.par")]] <<- recipient[[2]]
    })
  }

  return(input)
}

#' Find efficiency peers for each array
#'
#' @param array The array for which to find peers
#' @param array.list An array list
#' @param peer.direction The direction of peers to be found ("before" or "after")
#'
#' @return The array list with efficiency peers.
#'
#' @keywords internal
#'
findPeers <- function(array, array.list, peer.direction = c("before", "after"), disregard.parallels) {
  peer.direction <- match.arg(peer.direction)
  opposite.direction <- ifelse(peer.direction == "before", "after", "before")

  if (length(array) > 1)
    stopAndReport("'array' must be of length 1. This error should never happen. Contact developer.") # nocov

  if (!(array %in% names(array.list)))
    stopAndReport("Requested array does not exist in the array list (findPeers). This error should never happen. Contact developer.") # nocov

    usable.peers <- c() # start with nothing
    check.results <- c(TRUE, FALSE) # placeholder just to trigger the start of the while loop
    # round <- 0 # debug counter

    # cat("Array", array, "-", peer.direction, "peers\n") # debug and testing
    
    while (any(check.results) & !all(check.results)) {
        # round <- round + 1 # debug counter
        # cat("Round", round, "\n") # debug and testing

        # Check every array that is not the one we're examining and that has not been deemed a peer yet.
        to.check <- names(array.list)[!(names(array.list) %in% c(array, usable.peers))]
        # cat("Checking:", to.check, "\n") # debug and testing
                        
        check.results <- sapply(to.check, function(x) {
          # only relevant to test if array x is a valid peer if it connects with anything in the opposite direction.
          # e.g. if I have A -- B -- C, A cannot be the "after" peer of anyone, because nothing comes "before" it.
          no.connections <- length(array.list[[x]][[opposite.direction]]) == 0

          if (no.connections)
            return(FALSE) # not worth continuing

          # There are two types of parallels that can cause trouble:
          # 1) parallels in the array for which we are determining peers (object "array")
          # 2) parallels in the array we're trying to determine as a valid peer (object "x")
          # 
          # Type 1 is only an issue if we want to ignore parallel arrays (i.e. disregard.parallels = TRUE) and
          # the array "array" is right next to the array "x". That will affect the first two components of the check:
          if (disregard.parallels & array %in% array.list[[x]][[opposite.direction]]) {
            # For array x to be a valid peer of the array we're determining peers for (object "array"), 
            # the max number of connections to array x can only be the sum of the peers we already know 
            # about, the array "array", and any parallels of the array "array".
            too.many.connections <- length(array.list[[x]][[opposite.direction]]) > sum(length(usable.peers), length(array.list[[array]]$parallel), 1)
            # Additionally, all the connections to array x must be either the array "array", a parallel
            # of the array "array" that shares all connections with array "array", or an array that has 
            # already been determined as a valid peer.
            valid.parallels <- sapply(array.list[[array]]$parallel, function(parallel) {
              all(array.list[[parallel]][[opposite.direction]] %in% array.list[[array]][[opposite.direction]])
            })
            all.connections.are.valid.peers <- all(array.list[[x]][[opposite.direction]] %in% c(array, names(valid.parallels)[valid.parallels], usable.peers))
          } else {
            # In a situation where either disregard.parallels = FALSE, or the array we're determining
            # peers for (object "array") is not directly next to the array we are currently analysing
            # (object "x"), then the nax number of connections to array x can only be the sum of the
            # peers we already know about plus the array "array".
            too.many.connections <- length(array.list[[x]][[opposite.direction]]) > sum(length(usable.peers), 1)
            # Additionally, all the connections to array x must be either the array "array", or an 
            # array that has already been determined as a valid peer. Note that parallels are not allowed here,
            # even if disregard.parallels = TRUE. If this ever becomes a point of confusion, find the
            # drawings in issue #72.
            all.connections.are.valid.peers <- all(array.list[[x]][[opposite.direction]] %in% c(array, usable.peers))
          }

          if (too.many.connections | !all.connections.are.valid.peers)
            return(FALSE) # not worth continuing

          # Type 2 is only relevant if disregard.parallels = FALSE. Here, we have to confirm if the
          # arrays that are parallel to array "x" do not have any third-party connections that are not,
          # in themselves, a valid peer of array "array". E.g. if we have:
          # A -- B -- C -- D
          # B -- E -- D
          # C -- E -- C
          # F -- E
          # E.g. if array "array" is B, in the two checks above, array C will emerge as a potential peer
          # for B. If we disregard parallels, than that is indeed the case. However, if we do not disregard
          # parallels, then array E (a parallel of C) will cause array C to be invalidated, due to the 
          # connection coming from array F. This wouldn't had been a problem if F were a valid peer 
          # of "B" (e.g. if B -- F).
          if (disregard.parallels) {
            parallels.are.not.an.issue <- TRUE
          } else {
            # So, if disregard.parallels = FALSE, and array x has parallels, we 
            # need to go find which arrays lead to the parallels of array x
            leading.to.parallels <- unique(unlist(sapply(array.list[[x]]$parallel, function(parallel) array.list[[parallel]][[opposite.direction]])))
            # Finally, we verify that only valid peers of array "array" lead to the parallels listed above.
            parallels.are.not.an.issue <- all(leading.to.parallels %in% c(array, usable.peers))
          }

          # final decision
          if (parallels.are.not.an.issue)
            return(TRUE) # array "x" is a valid peer of array "array"
          else
            return(FALSE) # array "x" is _not_ a valid peer of array "array" (yet)
        })

        # cat("Check results:", check.results, "\n") # debug and testing

        # store the new peers together with the rest, and restart the loop.
        # loop will stop once no new peers are found.
        if (any(check.results)) {
          usable.peers <- c(usable.peers, to.check[check.results])
        }
        # cat("Usable peers at end of round:", usable.peers, "\n") # debug and testing
    }
    # cat("-----------------------\n") # debug and testing
    return(usable.peers)
}

#' Find all arrays linked to an array in a given direction
#'
#' @inheritParams findPeers
#' @param direction The direction in which to expand the chain ("before" or "after")
#'
#' @return The array list with all linked arrays.
#'
#' @keywords internal
#'
findDirectChains <- function(array, array.list, direction = c("before", "after")) {
  direction <- match.arg(direction)
  chain <- NULL
  parallel.aux <- array.list[[array]]$parallel
  to.check <- array.list[[array]][[direction]]
  while (!is.null(to.check)) {
    new.check <- NULL
    for (b in to.check) {
        if (is.null(chain) || all(!grepl(paste0("^", b, "$"), chain))) {
          chain <- c(chain, b)
          parallel.aux <- c(parallel.aux, array.list[[b]]$parallel)
          new.check <- c(new.check, array.list[[b]][[direction]])
        }
      to.check <- unique(new.check)
    }
  }
  output <- list(chain = unique(chain), unique(c(chain, parallel.aux)))
  return(output)
}

#' Find the shortest paths between arrays
#'
#' @param input An array list
#'
#' @return The array list with all paths between arrays with distance > 1.
#'
#' @keywords internal
#'
findShortestChains <- function(input) {
  # List to store the paths
  the.paths <- list()
  for (A in names(input)) {
    # make list of arrays to look for
    look.for <- rep(NA, length(input))
    names(look.for) <- names(input)
    # Don't look for the own array
    look.for[A] <- 0
    # Set neighbours with distance 1, to start
    look.for[input[[A]]$neighbours] <- 1
    # begin
    iteration <- 1
    while(any(na.as.false(look.for == iteration)) & any(is.na(look.for))) {
      to.check <- names(look.for)[na.as.false(look.for == iteration)]
      to.look <- names(look.for)[is.na(look.for)]
      test <- lapply(to.check, function(i) {
        aux <- match(to.look, input[[i]]$neighbours)
        if (any(!is.na(aux))) {
          aux <- input[[i]]$neighbours[na.as.false(aux)]
          for (found in aux) {
            if (is.null(the.paths[[paste0(A, "_to_", i)]]))
              to.add <- i
            else
              to.add <- paste(the.paths[[paste0(A, "_to_", i)]], i, sep = " -> ")
            if (is.null(the.paths[[paste0(A,"_to_",found)]]))
              the.paths[[paste0(A,"_to_",found)]] <<- to.add
            else
              the.paths[[paste0(A,"_to_",found)]] <<- c(the.paths[[paste0(A,"_to_",found)]], to.add)
            look.for[found] <<- iteration + 1
          }
        }
      })
      iteration <- iteration + 1
    }
  }
  return(the.paths)
}

#' Create Standard Names for spatial elements
#'
#' Includes standard names and also reprints 'spatial.csv'
#'
#' @param input A data frame with spatial information.
#'
#' @return A data frame with the same information as the input plus a Standard.name column.
#'
#' @keywords internal
#'
setSpatialStandards <- function(input){
  appendTo("debug","Running setSpatialStandards.")
  input$Standard.name <- as.character(input$Station.name)
  input$Standard.name <- gsub(" ", "_", input$Standard.name)
  link <- input$Type == "Hydrophone"
  input$Standard.name[link] <- paste0("St.", seq_len(sum(input$Type == "Hydrophone")))
  return(input)
}

#' Load distances matrix
#'
#' @param input A matrix of distances, to be loaded rather than a file.
#' @param spatial A list of spatial objects in the study area.
#'
#' @return A matrix of the distances (in metres) between stations (if a 'distances.csv' is present)
#'
#' @keywords internal
#'
loadDistances <- function(input = "distances.csv", spatial) {
  appendTo("debug", "Running loadDistances.")
  # Check for distances
  invalid.dist <- TRUE
  if (is.character(input)) {
    if (file.exists(input)) {
      appendTo(c("Screen", "Report"), paste0("M: File '", input, "' found, activating speed calculations."))
      dist.mat <- read.csv(input, row.names = 1, check.names = FALSE)
      if (ncol(dist.mat) == 1)
        warning("Only one column was identified in '", input, "'. If this seems wrong, please make sure that the values are separated using commas.", immediate. = TRUE, call. = FALSE)
    } else {
      dist.mat <- NULL
    }
  } else {
    dist.mat <- as.matrix(input)
  }

  if (!is.null(dist.mat)) {
    rownames(dist.mat) <- gsub(" ", "_", rownames(dist.mat))
    colnames(dist.mat) <- gsub(" ", "_", colnames(dist.mat))
    invalid.dist <- FALSE
    if (nrow(dist.mat) != ncol(dist.mat)){
      appendTo(c("Screen", "Report", "Warning"), "The distances matrix appears to be missing data (ncol != nrow). Deactivating speed calculations to avoid function failure.")
      invalid.dist <- TRUE
    }
    if (!invalid.dist && any(link <- is.na(match(rownames(dist.mat), colnames(dist.mat))))) {
      appendTo(c("Screen", "Report", "Warning"), "The column and row names in the distances matrix do not match each other. Deactivating speed calculations to avoid function failure.")
      message(paste0("   Row names missing in the columns: '", paste(rownames(dist.mat)[link], collapse = "', '"), "'."))
      if (any(link <- is.na(match(colnames(dist.mat), rownames(dist.mat)))))
        message(paste0("   Column names missing in the rows: '", paste(colnames(dist.mat)[link], collapse = "', '"), "'."))
      invalid.dist <- TRUE
    }
    # Failsafe for the case of unspecified release sites
    if (any(grepl("^unspecified$", spatial$release.sites$Standard.name))) {
      dist.mat[nrow(dist.mat) + 1, ] <- NA
      dist.mat[, ncol(dist.mat) + 1] <- NA
      colnames(dist.mat)[ncol(dist.mat)] <- "unspecified"
      rownames(dist.mat)[nrow(dist.mat)] <- "unspecified"
    }
    if (!invalid.dist && sum(nrow(spatial$stations), nrow(spatial$release.sites)) != nrow(dist.mat)) {
      appendTo(c("Screen", "Report", "Warning"), "The number of spatial points does not match the number of rows in the distances matrix. Deactivating speed calculations to avoid function failure.")
      message("   Number of stations and release sites listed: ", sum(nrow(spatial$stations), nrow(spatial$release.sites)))
      message("   Number of rows/columns in the distance matrix: ", nrow(dist.mat))
      invalid.dist <- TRUE
    }
    if (!invalid.dist && (any(!matchl(spatial$stations$Standard.name, colnames(dist.mat))) | any(!matchl(spatial$release.sites$Standard.name, colnames(dist.mat))))) {
      appendTo(c("Screen", "Report", "Warning"), "Some stations and/or release sites are not present in the distances matrix. Deactivating speed calculations to avoid function failure.")
      missing.releases <- spatial$release.sites$Standard.name[!matchl(spatial$release.sites$Standard.name, colnames(dist.mat))]
      missing.stations <- spatial$stations$Standard.name[!matchl(spatial$stations$Standard.name, colnames(dist.mat))]
      if (length(missing.releases) > 0)
        message(paste0("   Release sites missing: '", paste(missing.releases, collapse = "', '")))
      if (length(missing.stations) > 0)
        message(paste0("   Stations missing: '", paste(missing.stations, collapse = "', '")))
      invalid.dist <- TRUE
    }
  } else {
    dist.mat <- NA
  }
  attributes(dist.mat)$valid <- !invalid.dist
  return(dist.mat)
}


#' Load deployments file and Check the structure
#'
#' @param input an input data frame or file with deployment data.
#' @inheritParams explore
#'
#' @return A data frame with the deployments information
#'
#' @keywords internal
#'
loadDeployments <- function(input, tz){

  appendTo("debug","Running loadDeployments.")

  if (is.character(input)) {
    if (file.exists(input))
      input <- suppressWarnings(as.data.frame(data.table::fread(input, colClasses = c("Start" = "character", "Stop" = "character")),
               stringsAsFactors = FALSE))
    else
      stopAndReport("Could not find a '", input, "' file in the working directory.")
  } else {
    input <- as.data.frame(input)
    to.convert <- which(sapply(input, class) == "factor")
    if (length(to.convert) > 0) {
      for(i in to.convert) {
        input[, i] <- as.character(input[, i])
      }
    }
  }

  if (!is.na(link <- match("Station.Name", colnames(input))))
    colnames(input)[link] <- "Station.name"

  if (any(link <- duplicated(colnames(input))))
    stopAndReport("The following columns are duplicated in the deployments: '", paste(unique(colnames(input)[link]), sep = "', '"), "'.")

  default.cols <- c("Receiver", "Station.name", "Start", "Stop")
  link <- match(default.cols, colnames(input))
  if (any(is.na(link))) {
    stopAndReport(paste0("Column",
      ifelse(sum(is.na(link)) > 1, "(s) '", " '"),
      paste(default.cols[is.na(link)], collapse = "', '"),
      ifelse(sum(is.na(link)) > 1, "' are", "' is"),
      " missing in the deployments."))
  }

  # replace any weird characters in station names
  if (any(grepl("\\\\|/|:|\\*|\\?|\\\"|<|>|\\\'", input$Station.name)))
    input$Station.name <- gsub("\\\\|/|:|\\*|\\?|\\\"|<|>|\\\'", "_", input$Station.name)

  if (any(!grepl("^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9][ |T|t][0-2][0-9]:[0-5][0-9]", input$Start))) {
    stopAndReport("Not all values in the 'Start' column appear to be in a 'yyyy-mm-dd hh:mm' format (seconds are optional). Please double-check the deployments.")
  }
  if (any(!grepl("^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9][ |T|t][0-2][0-9]:[0-5][0-9]", input$Stop))) {
    stopAndReport("Not all values in the 'Stop' column appear to be in a 'yyyy-mm-dd hh:mm' format (seconds are optional). Please double-check the deployments.")
  }

  timestamp_formats  <- c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%dT%H:%M:%OS",
                          "%Y-%m-%d %H:%M", "%Y-%m-%dT%H:%M",
                          "%Y-%m-%d")

  if (inherits(try(input$Start <- as.POSIXct(input$Start, tz = tz, tryFormats = timestamp_formats), silent = TRUE),"try-error")){
    stopAndReport("Could not recognise the data in the 'Start' column as POSIX-compatible timestamps. Please double-check the deployments.")
  }

  if (inherits(try(input$Stop <- as.POSIXct(input$Stop, tz = tz, tryFormats = timestamp_formats), silent = TRUE),"try-error")){
    stopAndReport("Could not recognise the data in the 'Stop' column as POSIX-compatible timestamps. Please double-check the deployments.")
  }

  if (any(link <- input$Start > input$Stop)) {
    stopAndReport("Some deployment periods end before they have started! Please fix this before continuing.\n       Troublesome rows: ", paste(which(link), collapse = ", "))
  }
  input$Receiver <- as.character(input$Receiver)
  input$Receiver <- sapply(input$Receiver, function(x) tail(unlist(strsplit(x, "-")), 1))
  input <- input[order(input$Start), ]
  return(input)
}

#' Load Spatial File
#'
#' Loads a spatial file prepared for actel and appends the Standard.name column. Additionally,
#' performs a series of quality checks on the contents of the target file.
#'
#' @param input Either a data frame or the name of an input file with spatial data in the actel format.
#' @param section.order A vector containing the order by which sections should be aligned in the results.
#' @examples
#' # This function requires the presence of a file with spatial information
#'
#' # Fetch location of actel's example files
#' aux <- system.file(package = "actel")[1]
#'
#' # run loadSpatial on the temporary spatial.csv file
#' loadSpatial(input = paste0(aux, '/example_spatial.csv'))
#'
#' @return A data frame with the spatial information present in 'spatial.csv' and the Standard.name column.
#'
#' @export
#'
loadSpatial <- function(input = "spatial.csv", section.order = NULL){
  appendTo("debug", "Running loadSpatial.")

  if (is.character(input)) {
    if (file.exists(input))
      input <- as.data.frame(data.table::fread(input), stringsAsFactors = FALSE)
    else {
      stopAndReport("Could not find a '", input, "' file in the working directory.")
    }
  } else {
    input <- as.data.frame(input)
    to.convert <- which(sapply(input, class) == "factor")
    if (length(to.convert) > 0) {
      for(i in to.convert) {
        input[, i] <- as.character(input[, i])
      }
    }
  }

  # Check duplicated columns
  if (any(link <- duplicated(colnames(input))))
    stopAndReport("The following columns are duplicated in the spatial input: '", paste(unique(colnames(input)[link]), sep = "', '"), "'.")
  # Check wrong capitals in Station.name
  if (!is.na(link <- match("Station.Name", colnames(input))))
    colnames(input)[link] <- "Station.name"
  # Check missing Station.name
  if (!any(grepl("^Station.name$", colnames(input)))) {
    stopAndReport("The spatial input must contain a 'Station.name' column.")
  } else {
    # Check all station names are unique
    if (any(link <- table(input$Station.name) > 1)) {
      stopAndReport("The 'Station.name' column in the spatial input must not have duplicated values.\nStations appearing more than once: ", paste(names(table(input$Station.name))[link], collapse = ", "), "")
    }
    # Check that stations do not contain weird characters
    if (any(grepl("\\\\|/|:|\\*|\\?|\\\"|<|>|\\\'", input$Station.name))) {
      appendTo(c("Screen", "Warning", "Report"), "Troublesome characters found in the station names (\\/:*?\"<>\'). Replacing these with '_' to prevent function failure.")
      input$Station.name <- gsub("\\\\|/|:|\\*|\\?|\\\"|<|>|\\\'", "_", input$Station.name)
    }
  }
  # Check missing Array column
  if (!any(grepl("^Array$", colnames(input)))) {
    stopAndReport("The spatial input must contain an 'Array' column.")
  }
  # check missing data in the arrays
  if (any(is.na(input$Array)))
    stopAndReport("Some rows do not contain 'Array' information in the spatial input. Please double-check the input files.")
  # check spaces in the array names
  if (any(grepl(" ", input$Array))) {
    appendTo("Screen", "M: Replacing spaces with '_' in array names to prevent function failure.")
    input$Array <- gsub(" ", "_", input$Array)
  }
  if (any(grepl("\\\\|/|:|\\*|\\?|\\\"|<|>|\\\'", input$Array))) {
    appendTo(c("Screen", "Warning", "Report"), "Troublesome characters found in the array names (\\/:*?\"<>\'). Replacing these with '_' to prevent function failure.")
    input$Array <- gsub("\\\\|/|:|\\*|\\?|\\\"|<|>|\\\'", "_", input$Array)
  }

  # check reserved array names
  if (any(grepl("^Release$", input$Array)))
    stopAndReport("The term 'Release' is reserved for internal calculations. Do not name any sections or arrays as 'Release'.")
  if (any(grepl("^Total$", input$Array)))
    stopAndReport("The term 'Total' is reserved for internal calculations. Do not name any sections or arrays as 'Total'.")
  if (any(grepl("^Invalid$", input$Array)))
    stopAndReport("The term 'Invalid' is reserved for internal calculations. Do not name any sections or arrays as 'Invalid'.")
  if (any(grepl("^Unknown$", input$Array)))
    stopAndReport("The term 'Unknown' is reserved for internal calculations. Do not name any sections or arrays as 'Unknown'.")
  # check array name length
  aux <- unlist(strsplit(input$Array, "|", fixed = TRUE))
  if (any(nchar(as.character(aux)) > 6))
    appendTo(c("Screen", "Report", "Warning"), "Long array names detected. To improve graphic rendering, consider keeping array names under six characters.")
  rm(aux)
  # check missing Type column
  if (!any(grepl("^Type$", colnames(input)))) {
    appendTo(c("Screen", "Report"), paste0("M: No 'Type' column found in the spatial input. Assigning all rows as hydrophones."))
    input$Type <- "Hydrophone"
  } else {
    # check strange data in the Type column
    if (any(is.na(match(unique(input$Type), c("Hydrophone", "Release"))))){
      stopAndReport("Could not recognise the data in the 'Type' column as only one of 'Hydrophone' or 'Release'. Please double-check the spatial input.")
    }
  }
  # Check Section column
  if (any(grepl("^Section$", colnames(input)))) {
    # check missing data in the arrays
    if (any(is.na(input$Section[input$Type == "Hydrophone"]) | input$Section[input$Type == "Hydrophone"] == ""))
      stopAndReport("Some rows do not contain 'Section' information in the spatial input. Please double-check the input files.")
    # check spaces in the array names
    if (any(grepl(" ", input$Section))) {
      appendTo("Screen", "M: Replacing spaces with '_' in section names to prevent function failure.")
      input$Section <- gsub(" ", "_", input$Section)
    }
    if (any(grepl("\\\\|/|:|\\*|\\?|\\\"|<|>|\\|", input$Section))) {
      appendTo(c("Screen", "Warning", "Report"), "Troublesome characters found in the section names (\\/:*?\"<>|). Replacing these with '_' to prevent function failure.")
      input$Section <- gsub("\\\\|/|:|\\*|\\?|\\\"|<|>|\\|", "_", input$Section)
    }
    sections <- unique(input$Section[input$Type == "Hydrophone"])
    # check reserved section names
    if (any(grepl("^Release$", sections)))
      stopAndReport("The term 'Release' is reserved for internal calculations. Do not name any sections or arrays as 'Release'.")
    if (any(grepl("^Total$", sections)))
      stopAndReport("The term 'Total' is reserved for internal calculations. Do not name any sections or arrays as 'Total'.")
    if (any(grepl("^Unknown$", sections)))
      stopAndReport("The term 'Unknown' is reserved for internal calculations. Do not name any sections or arrays as 'Unknown'.")
    # check that section names are independent
    if (any(link <- sapply(sections, function(i) length(grep(i, sections))) > 1)) {
      stopAndReport(
        ifelse(sum(link) == 1, "Section '", "Sections '"),
        paste(sections[link], collapse = "', '"),
        ifelse(sum(link) == 1, "' is", "' are"),
        " contained within other section names. Sections must be unique and independent.\n       Please rename your sections so that section names are not contained within each other.")
    }

    if (is.null(section.order)) {
      input$Section <- factor(input$Section, levels = sections)
    } else {
      section.order <- gsub(" ", "_", section.order)

      if (any(link <- is.na(match(sections, section.order))))
        stopAndReport("Not all sections are listed in 'section.order'. Sections missing: ", paste(sections[link], collapse = ", "))

      if (any(link <- is.na(match(section.order, sections)))) {
        appendTo(c("Screen", "Report", "Warning"), paste0("Not all values listed in 'section.order' correspond to sections. Discarding the following values: ", paste(section.order[link], collapse = ", ")))
        section.order <- section.order[!link]
      }
      input$Section <- factor(input$Section, levels = section.order)
    }
    if (any(nchar(as.character(sections)) > 6))
      appendTo(c("Screen", "Report", "Warning"), "Long section names detected. To improve graphic rendering, consider keeping section names under six characters.")

    # find if arrays are assigned to more than one section
    aux <- input[input$Type == "Hydrophone", ]
    aux <- with(aux, as.data.frame.matrix(table(Array, Section)))
    sections.per.array <- apply(aux, 1, function(i) sum(i > 0))
    if (any(sections.per.array > 1)) {
      stopAndReport(ifelse(sum(sections.per.array > 1) == 1, "Array '", "Arrays '"), 
        paste0(names(sections.per.array)[sections.per.array > 1], collapse = "', '"), 
        ifelse(sum(sections.per.array > 1) == 1, "' has been", "' have been"), 
        " assigned to more than one section! Each array can only belong to one section. Please correct the spatial input before continuing.")
    }
  } else {
    if (!is.null(section.order))
      appendTo(c("Screen", "Report", "Warning"), "'section.order' was set but input has no 'Section' column. Ignoring argument.")
  
    appendTo(c("Screen", "Report", "Warning"), "The spatial input does not contain a 'Section' column. This input is only valid for explore() analyses.")
  }
  # check release arrays exist
  hydro.arrays <- unique(input$Array[input$Type == "Hydrophone"])
  release.arrays <- unique(unlist(sapply(input$Array[input$Type == "Release"], function(x) unlist(strsplit(x, "|", fixed = TRUE)))))
  if (any(link <- is.na(match(release.arrays, hydro.arrays)))) {
    stopAndReport("Not all the expected first arrays of the release sites exist.\nUnknown expected first arrays: '", paste0(release.arrays[link], collapse = "', '"), "'.
In the spatial input, the expected first arrays of the release sites should match the arrays where hydrophone stations where deployed.")
  }
  input <- setSpatialStandards(input = input) # Create Standard.name for each station
  return(input)
}

#' Load Biometrics file
#'
#' @param input an input file or data frame with biometric data.
#' @inheritParams explore
#'
#' @keywords internal
#'
#' @return A data frame with the biometrics information
#'
loadBio <- function(input, tz){
  appendTo("debug", "Running loadBio.")

  if (missing(input))
    stop("'input' is missing.")
  if (missing(tz))
    stop("'tz' is missing.")

  # compatibility with preload()
  if (is.character(input))
    preloaded <- FALSE
  else
    preloaded <- TRUE

  if (preloaded) {
    bio <- as.data.frame(input, stringsAsFactors = FALSE)
    to.convert <- which(sapply(bio, class) == "factor")
    if (length(to.convert) > 0) {
      for(i in to.convert) {
        bio[, i] <- as.character(bio[, i])
      }
    }
  } else {
    if (file.exists(input))
      bio <- as.data.frame(suppressWarnings(data.table::fread(input, colClasses = c("Release.date" = "character"))),
                           stringsAsFactors = FALSE)
    else
      stopAndReport("Could not find a '", input, "' file in the working directory.")
  }

  if (any(link <- duplicated(colnames(bio))))
    stopAndReport("The following columns are duplicated in the biometrics: '",
      paste(unique(colnames(bio)[link]), sep = "', '"), "'.")

  if (!any(grepl("^Release.date$", colnames(bio)))) {
    stopAndReport("The biometrics must contain an 'Release.date' column.")
  }

  if (any(!grepl("^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9][ |T|t][0-2][0-9]:[0-5][0-9]", bio$Release.date))) {
    stopAndReport("Not all values in the 'Release.date' column appear to be in a 'yyyy-mm-dd hh:mm' format (seconds are optional). Please double-check the biometrics.")
  }

  timestamp_formats  <- c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%dT%H:%M:%OS",
                          "%Y-%m-%d %H:%M", "%Y-%m-%dT%H:%M",
                          "%Y-%m-%d")

  if (inherits(try(bio$Release.date <- as.POSIXct(bio$Release.date, tz = tz, tryFormats = timestamp_formats), silent = TRUE),"try-error")){
    stopAndReport("Could not recognise the data in the 'Release.date' column as POSIX-compatible timestamps. Please double-check the biometrics.")
  }

  if (!any(grepl("^Signal$", colnames(bio)))) {
    stopAndReport("The biometrics must contain an 'Signal' column.")
  }

  if (any(is.na(bio$Signal))) {
    stopAndReport("Some animals have no 'Signal' information. Please double-check the biometrics.")
  }

  if (!any(grepl("^Code.space$", colnames(bio)))) {
    appendTo(c("Screen", "Report"), "M: No Code.space column was found in the biometrics. Assigning code spaces based on detections.")
  } else {
      if (any(is.na(bio$Code.space)) | any(bio$Code.space == ""))
    stopAndReport('Not all tags have an associated code space. Please specify the code space of every tag.')
  }

  # activate multi-sensor versatility.
  if (any(grepl("|", bio$Signal, fixed = TRUE))) {
    appendTo(c("Screen", "Report"), "M: Multi-sensor tags detected. These tags will be referred to by their lowest signal value.")
    expect_integer <- FALSE
  } else {
    expect_integer <- TRUE
  }

  # examine signal quality
  if (expect_integer & !inherits(bio$Signal, "integer")) {
    stopAndReport("Could not recognise the data in the 'Signal' column as integers. Please double-check the biometrics.")
  } else {
    signal_check <- suppressWarnings(as.numeric(unlist(strsplit(as.character(bio$Signal), "|", fixed = TRUE))))
    if (any(is.na(signal_check))) {
      stopAndReport("Could not recognise the data in the 'Signal' column as integers. Please double-check the biometrics.")
    }
  }

  # check that tags are not duplicated
  if (expect_integer) {
    if (any(colnames(bio) == "Code.space")) {
      aux <- paste(bio$Code.space, "-", bio$Signal)
      if (any(link <- table(aux) > 1))
        stopAndReport(ifelse(sum(link) > 1, "Tags ", "Tag "), paste(aux[link], collapse = ", "), ifelse(sum(link) > 1," are ", " is "), "duplicated in the biometrics.")
    } else {
      if (any(link <- table(bio$Signal) > 1))
        stopAndReport(ifelse(sum(link) > 1, "Signals ", "Signal "), paste(names(table(bio$Signal))[link], collapse = ", "), ifelse(sum(link) > 1," are ", " is "), "duplicated in the biometrics.")
    }
  } 
  else {
    if (any(colnames(bio) == "Code.space")) {
      aux <- unlist(apply(bio, 1, function(x) {
                            paste0(x['Code.space'], '-', splitSignals(x['Signal']))
                           }))
      if (any(link <- table(aux) > 1))
        stopAndReport(ifelse(sum(link) > 1, "Tags ", "Tag "), paste(aux[link], collapse = ", "), ifelse(sum(link) > 1," are ", " is "), "duplicated in the biometrics.")
    }
    else {
      aux <- unlist(sapply(bio$Signal, splitSignals))
      if (any(link <- table(aux) > 1))
        stopAndReport(ifelse(sum(link) > 1, "Signals ", "Signal "), paste(aux[link], collapse = ", "), ifelse(sum(link) > 1," are ", " is "), "duplicated in the biometrics.")
    }
  }

  # check sensor names
  if (!expect_integer) { 
    if (!any(grepl("^Sensor.unit$", colnames(bio)))) {
      appendTo(c("Screen", "Warning"), "Tags with multiple sensors are listed in the biometrics, but a 'Sensor.unit' column could not be found. Skipping sensor unit assignment.")
    } 
    else {
      bio$Sensor.unit <- as.character(bio$Sensor.unit) # failsafe in case all values are numeric, or NA.
      bio$Sensor.unit[bio$Sensor.unit == ''] <- NA_character_

      if (any(link <- na.as.false(startsWith(bio$Sensor.unit, '|'))))
        appendTo(c('screen', 'warning'), paste0("The Sensor.unit information in ",
          ifelse(sum(link) <= 10,
                 paste0("row(s) ", paste0(which(link), collapse = ", ")), 
                 paste0(sum(link), " row(s)")),
          " of the biometrics starts with a '|' character. Could you have forgotten to include a sensor unit?"))

      if (any(link <- na.as.false(endsWith(bio$Sensor.unit, '|'))))
        appendTo(c('screen', 'warning'), paste0("The Sensor.unit information in ",
          ifelse(sum(link) <= 10,
                 paste0("row(s) ", paste0(which(link), collapse = ", ")), 
                 paste0(sum(link), " row(s)")),
          " of the biometrics ends with a '|' character. Could you have forgotten to include a sensor unit?"))

      signals_per_tag <- sapply(strsplit(bio$Signal, "|", fixed = TRUE), length) # number of signals per tag
      aux <- strsplit(bio$Sensor.unit, "|", fixed = TRUE)
      sensors_per_tag <- sapply(aux, length)

      if (any(link <- signals_per_tag != sensors_per_tag))
        stopAndReport("The number of provided sensor units does not match the number of signals for ", 
          ifelse(sum(link) <= 10,
                 paste0("row(s) ", paste0(which(link), collapse = ", ")), 
                 paste0(sum(link), " row(s)")),
          " of the biometrics.")
    }
  }

  # Release site quality checking/creation
  if (!any(grepl("^Release.site$", colnames(bio)))) {
    appendTo("Screen", "M: No Release site has been indicated in the biometrics. Creating a 'Release.site' column to avoid function failure. Filling with 'unspecified'.")
    bio$Release.site <- "unspecified"
  } else {
    bio$Release.site <- gsub(" ", "_", bio$Release.site)
  
    # replace any weird characters in station names
    if (any(grepl("\\\\|/|:|\\*|\\?|\\\"|<|>|\\\'", bio$Release.site)))
      bio$Release.site <- gsub("\\\\|/|:|\\*|\\?|\\\"|<|>|\\\'", "_", bio$Release.site)
  
    bio$Release.site <- factor(bio$Release.site)
    if (any(link <- is.na(bio$Release.site) | bio$Release.site == "")) {
      appendTo(c("Screen","Report","Warning"),"Some animals contain no release site information. You may want to double-check the data.\n   Filling the blanks with 'unspecified'.")
      levels(bio$Release.site) <- c(levels(bio$Release.site), "unspecified")
      bio$Release.site[link] <- "unspecified"
      bio$Release.site <- droplevels(bio$Release.site)
    }
  }

  # Group quality checking/creation
  if (!any(grepl("^Group$", colnames(bio)))) {
    appendTo(c("Screen", "Report"), paste0("M: No 'Group' column found in the biometrics. Assigning all animals to group 'All'."))
    bio$Group <- "All"
    bio$Group <- as.factor(bio$Group)
  } else {
    bio$Group <- factor(bio$Group)
    if (any(link <- is.na(bio$Group) | bio$Group == "")) {
      appendTo(c("Screen", "Report", "Warning"),"Some animals contain no group information. You may want to double-check the data.\n   Filling the blanks with 'unspecified'.")
      levels(bio$Group) <- c(levels(bio$Group), "unspecified")
      bio$Group[link] <- "unspecified"
      bio$Group <- droplevels(bio$Group)
    }
    if (any(link <- sapply(levels(bio$Group), function(i) length(grep(i, levels(bio$Group)))) > 1)) {
      appendTo(c("Screen", "Report", "Warning"), paste0(
        ifelse(sum(link) == 1, "Group '", "Groups '"),
        paste(levels(bio$Group)[link], collapse = "', '"),
        ifelse(sum(link) == 1, "' is", "' are"),
        " contained within other groups. To avoid function failure, a number will be appended to ",
        ifelse(sum(link) == 1, "this group.", "these groups.")))
      levels(bio$Group)[link] <- paste(levels(bio$Group)[link], 1:sum(link), sep = "_")
    }
    if (any(link <- grepl("\\.", levels(bio$Group)))) {
      appendTo(c("Screen", "Report"), "M: Some groups contain one or more '.' characters. To avoid function failure, these will be replaced with '_'.")
      levels(bio$Group) <- gsub("\\.", "_", levels(bio$Group))
    }
  }
  if (any(nchar(as.character(bio$Group)) > 6))
    appendTo(c("Screen", "Report", "Warning"), "Long group names detected. To improve graphic rendering, consider keeping group names under six characters.")

  # order table by signal before handing it over
  bio <- bio[order(bio$Signal),]
  return(bio)
}

#' Load ALS detections
#'
#' If there are previously compiled detections present, offers the chance to reuse. Otherwise triggers combineDetections.
#'
#' @inheritParams explore
#'
#' @return A data frame with all the detections
#'
#' @keywords internal
#'
loadDetections <- function(start.time = NULL, stop.time = NULL, tz, force = FALSE, 
  save.detections = FALSE, record.source = FALSE) {
  # NOTE: The variable actel.detections is loaded from a RData file, if present. To avoid package check
  #   notes, the variable name is created before any use.
  actel.detections <- NULL

  appendTo("debug", "Running loadDetections.")

  recompile <- TRUE
  detection.paths <- c(file.exists("actel.detections.RData"), file.exists("detections/actel.detections.RData"))

  if (any(detection.paths)) {
    if (all(detection.paths))
      appendTo(c("Screen", "Warning", "Report"), "Previously compiled detections were found both in the current directory and in a 'detections' folder.\n   Loading ONLY the compiled detections present in the 'detections' folder.")
    if(detection.paths[2])
      load("detections/actel.detections.RData")
    else
      load("actel.detections.RData")
    if (force) { # nocov start
      decision <- "Y"
    } else {
      appendTo("Screen", paste0("M: The detections have been processed on ", actel.detections$timestamp, ".\n   If the input detection files were not changed, it is safe to use these again."))
      decision <- userInput("   Reuse processed detections?(y/n) ", choices = c("y", "n"), hash = "# reuse detections?")
    }
    if (decision == "y"){
      appendTo(c("Screen","Report"), paste0("M: Using detections previously compiled on ", actel.detections$timestamp, "..."))
      detections <- actel.detections$detections
      attributes(detections$Timestamp)$tzone <- "UTC"
      detections <- convertTimes(input = detections, start.time = start.time, stop.time = stop.time, tz = tz)
      recompile <- FALSE
    } else {
      appendTo("Screen", "M: Reprocessing the detections.")
    } # nocov end
    rm(actel.detections)
  }

  if (recompile)
    detections <- compileDetections(path = "detections", start.time = start.time,
      stop.time = stop.time, tz = tz, save.detections = save.detections, record.source = record.source)

  detections$Valid <- TRUE
  return(detections)
}

# LOAD DETECTION HELPERS:

#' Combine ALS detections
#'
#' Finds the detections' files and processes them.
#'
#' @inheritParams explore
#' @param path the path(s) to the detection files
#'
#' @import data.table
#'
#' @return A data frame with all the detections
#'
#' @keywords internal
#'
compileDetections <- function(path = "detections", start.time = NULL, stop.time = NULL, 
  tz, save.detections = FALSE, record.source = FALSE) {
  appendTo("Screen", "M: Compiling detections...")
  # Find the detection files
  if (file_test("-d", path)) {
    file.list <- list.files(path = path, pattern = "*.csv", full.names = TRUE)
    if (length(file.list) == 0)
      stopAndReport("A 'detections' folder is present but appears to be empty.")
  } else {
    if (file.exists("detections.csv"))
      file.list <- "detections.csv"
    else
      stopAndReport("Could not find a 'detections' folder nor a 'detections.csv' file.")
  }
  if (file_test("-d", path) & file.exists("detections.csv"))
    appendTo(c("Screen", "Warning", "Report"), "Both a 'detections' folder and a 'detections.csv' file are present in the current directory.\n   Loading ONLY the files present in the 'detections' folder.")
  # Prepare the detection files
  data.files <- lapply(file.list, function(i) {
    appendTo("debug", paste0("Importing file '", i, "'."))
    aux <- data.table::fread(i, fill = TRUE, sep = ",", showProgress = FALSE)
    if (ncol(aux) < 3) {
      appendTo(c("Screen", "Warning", "Report"), paste0("File '", i, "' could not be recognized as a valid detections table (ncol < 3), skipping processing. Are you sure it is a comma separated file?"))
      flush.console()
      return(NULL)
    } else {
      if(nrow(aux) == 0){
        appendTo(c("Screen", "Report"), paste0("File '", i, "' is empty, skipping processing."))
        flush.console()
        return(NULL)
      } else {
        unknown.file <- TRUE
        if (unknown.file && all(!is.na(match(c("Timestamp", "CodeSpace", "Receiver", "Signal"), colnames(aux))))) {
          appendTo("debug", paste0("File '", i, "' matches a Standard log."))
          if (!is.numeric(aux$Receiver))
            stopAndReport("The file '", i, "' was recognized as a standard detections file, but the 'Receiver' column is not numeric.\nPlease include only the receiver serial numbers in the 'Receiver' column.")
          if (!is.numeric(aux$Signal))
            stopAndReport("The file '", i, "' was recognized as a standard detections file, but the 'Signal' column is not numeric.\nPlease include only the tag signals in the 'Signal' column.")
          output <- tryCatch(processStandardFile(input = aux), error = function(e) {
              stopAndReport("Something went wrong when processing file '", i, "'. If you are absolutely sure this file is ok, contact the developer.\nOriginal error:", sub("^Error:", "", e))
            })
          unknown.file <- FALSE
        }
        if (unknown.file && all(!is.na(match(c("CodeType", "TBR Serial Number", "Id"), colnames(aux))))) {
          appendTo("debug", paste0("File '", i, "' matches a Thelma log."))
          output <- tryCatch(processThelmaOldFile(input = aux), error = function(e) {
              stopAndReport("Something went wrong when processing file '", i, "'. If you are absolutely sure this file is ok, contact the developer.\nOriginal error:", sub("^Error:", "", e))
            })
          unknown.file <- FALSE
        }
        if (unknown.file && all(!is.na(match(c("Protocol", "Receiver", "ID"), colnames(aux))))) {
          appendTo("debug", paste0("File '", i, "' matches a Thelma log."))
          output <- tryCatch(processThelmaNewFile(input = aux), error = function(e) {
              stopAndReport("Something went wrong when processing file '", i, "'. If you are absolutely sure this file is ok, contact the developer.\nOriginal error:", sub("^Error:", "", e))
            })
          unknown.file <- FALSE
        }
        if (unknown.file && all(!is.na(match(c("Transmitter", "Receiver"), colnames(aux)))) & any(grepl("^Date.and.Time", colnames(aux)))) {
          appendTo("debug", paste0("File '", i, "' matches a Vemco log."))
          output <- tryCatch(processVemcoFile(input = aux), error = function(e) {
              stopAndReport("Something went wrong when processing file '", i, "'. If you are absolutely sure this file is ok, contact the developer.\nOriginal error:", sub("^Error:", "", e))
            })
          unknown.file <- FALSE
        }
        if (unknown.file) {
          appendTo(c("Screen", "Report", "Warning"),
            paste0("File '", i, "' does not match to any of the supported hydrophone file formats!\n         If your file corresponds to a hydrophone log and actel did not recognize it, please get in contact through www.github.com/hugomflavio/actel/issues/new"))
          flush.console()
          return(NULL)
        }
        if (record.source)
          output$Source.file <- i
        return(output)
      }
    }
  })
  names(data.files) <- file.list
  if (any(sapply(data.files, is.null))) {
    if (sum(sapply(data.files, is.null)) > 1) {
      appendTo("Screen", paste("M:", sum(sapply(data.files, is.null)), "files were excluded from further analyses."))
    } else {
      appendTo("Screen", "M: One file was excluded from further analyses.")
    }
  }

  if (all(sapply(data.files, is.null)))
    stopAndReport("No valid detection files were found.")

  # Bind the detection files
  appendTo("debug", "Binding detections.")
  output <- data.table::rbindlist(data.files)
  output$Receiver <- as.factor(output$Receiver)
  output$CodeSpace <- as.factor(output$CodeSpace)
  # Convert codespaces
  if (getOption("actel.auto.convert.codespaces", default = TRUE))
    output <- convertCodes(input = output)

  # Compile transmitters
  output$Transmitter <- as.factor(paste(output$CodeSpace, output$Signal, sep = "-"))

  # save detections in UTC
  actel.detections <- list(detections = output, timestamp = Sys.time())
  if (save.detections) {
    save(actel.detections, file = ifelse(file_test("-d", path),
      paste0(path, "/actel.detections.RData"), "actel.detections.RData"))
  }

  # Convert time-zones
  output <- convertTimes(input = output, start.time = start.time,
    stop.time = stop.time, tz = tz)

  return(output)
}

#' Standard detections file created for actel
#'
#' @param input the detections data frame.
#'
#' @return A data frame of standardized detections from the input file.
#'
#' @keywords internal
#'
processStandardFile <- function(input) {
  appendTo("debug", "Running processStandardFile.")
  input <- as.data.frame(input, stringsAsFactors = FALSE)
  output <- data.table(
    Timestamp = fasttime::fastPOSIXct(sapply(as.character(input$Timestamp), function(x) gsub("Z", "", gsub("T", " ", x))), tz = "UTC"),
    Receiver = input$Receiver,
    CodeSpace = input$CodeSpace,
    Signal = input$Signal)

  # include sensor data, if present
  if (any(colnames(input) == "Sensor.Value"))
    output$Sensor.Value <- input$Sensor.Value
  else
    output$Sensor.Value <- NA_real_

  if (any(colnames(input) == "Sensor.Unit"))
    output$Sensor.Unit <- input$Sensor.Unit
  else
    output$Sensor.Unit <- NA_character_

  # final checks
  if (any(is.na(output$Timestamp)))
    stop("Importing timestamps failed", call. = FALSE)
  if (any(is.na(output$Receiver)))
    stop("Importing receivers failed", call. = FALSE)
  if (any(is.na(output$Signal)))
    stop("Importing code space failed", call. = FALSE)
  if (any(is.na(output$Receiver)))
    stop("Importing signals failed", call. = FALSE)
  return(output)
}

#' Thelma old export files
#'
#' Processes Thelma ALS files.
#'
#' @param input the detections data frame.
#'
#' @return A data frame of standardized detections from the input file.
#'
#' @keywords internal
#'
processThelmaOldFile <- function(input) {
  appendTo("debug", "Running processThelmaOldFile.")
  input <- as.data.frame(input, stringsAsFactors = FALSE)
  output <- data.table(
    Timestamp = fasttime::fastPOSIXct(sapply(as.character(input[, grep("^Date.and.Time", colnames(input))]), function(x) gsub("Z", "", gsub("T", " ", x))), tz = "UTC"),
    Receiver = input$`TBR Serial Number`,
    CodeSpace = input$CodeType,
    Signal = input$Id,
    Sensor.Value = input$Data,
    Sensor.Unit = rep(NA_character_, nrow(input)))

  # Some thelma output files come with "-" rather than NA...
  output$Sensor.Value[output$Sensor.Value == "-"] <- NA
  output$Sensor.Value <- as.numeric(output$Sensor.Value)

  if (any(is.na(output$Timestamp)))
    stop("Importing timestamps failed", call. = FALSE)
  if (any(is.na(output$Receiver)))
    stop("Importing receivers failed", call. = FALSE)
  if (any(is.na(output$Signal)))
    stop("Importing code space failed", call. = FALSE)
  if (any(is.na(output$Receiver)))
    stop("Importing signals failed", call. = FALSE)
  return(output)
}

#' Thelma new export files
#'
#' Processes Thelma ALS files.
#'
#' @param input the detections data frame.
#'
#' @return A data frame of standardized detections from the input file.
#'
#' @keywords internal
#'
processThelmaNewFile <- function(input) {
  appendTo("debug", "Running processThelmaNewFile.")
  input <- as.data.frame(input, stringsAsFactors = FALSE)
  output <- data.table(
    Timestamp = fasttime::fastPOSIXct(sapply(as.character(input[, grep("^Date.and.Time", colnames(input))]), function(x) gsub("Z", "", gsub("T", " ", x))), tz = "UTC"),
    Receiver = input$Receiver,
    CodeSpace = sapply(input$Protocol, function(x) unlist(strsplit(x, "-", fixed = TRUE))[1]),
    Signal = input$ID,
    Sensor.Value = input$Data,
    Sensor.Unit = rep(NA_character_, nrow(input)))

  # Some thelma output files come with "-" rather than NA...
  output$Sensor.Value[output$Sensor.Value == "-"] <- NA
  output$Sensor.Value <- as.numeric(output$Sensor.Value)

  if (any(is.na(output$Timestamp)))
    stop("Importing timestamps failed", call. = FALSE)
  if (any(is.na(output$Receiver)))
    stop("Importing receivers failed", call. = FALSE)
  if (any(is.na(output$Signal)))
    stop("Importing code space failed", call. = FALSE)
  if (any(is.na(output$Receiver)))
    stop("Importing signals failed", call. = FALSE)
  return(output)
}

#' Vemco files
#'
#' Processes Vemco ALS files
#'
#' @param input the detections data frame.
#'
#' @return A data frame of standardized detections from the input file.
#'
#' @keywords internal
#'
processVemcoFile <- function(input) {
  appendTo("Debug", "Running processVemcoFile.")

  transmitter_aux <- strsplit(input$Transmitter, "-", fixed = TRUE)
  input[, "CodeSpace"] <- unlist(lapply(transmitter_aux, function(x) paste(x[1:2], collapse = "-"))) # Rejoin code space
  input[, "Signal"] <- unlist(lapply(transmitter_aux, function(x) x[3])) # extract only signal
  input[, "Receiver"] <- sapply(input$Receiver, function(x) tail(unlist(strsplit(x, "-")), 1)) # extract only the serial

  colnames(input)[grep("^Date.and.Time", colnames(input))] <- c("Timestamp")
  colnames(input) <- gsub(" ", ".", colnames(input))
  if (any(grepl("^Sensor.Value$", colnames(input)))) {
    input <- input[, c("Timestamp", "Receiver", "CodeSpace", "Signal", "Sensor.Value", "Sensor.Unit")]
  } else {
    input <- input[, c("Timestamp", "Receiver", "CodeSpace", "Signal")]
    input$Sensor.Value <- rep(NA_real_, nrow(input))
    input$Sensor.Unit <- rep(NA_character_, nrow(input))
  }
  input$Timestamp <- fasttime::fastPOSIXct(as.character(input$Timestamp), tz = "UTC")
  
  if (any(is.na(input$Timestamp)))
    stop("Importing timestamps failed", call. = FALSE)
  if (any(is.na(input$Receiver)))
    stop("Importing receivers failed", call. = FALSE)
  if (any(is.na(input$Signal)))
    stop("Importing code space failed", call. = FALSE)
  if (any(is.na(input$Receiver)))
    stop("Importing signals failed", call. = FALSE)
  return(input)
}

#' Convert code spaces
#'
#' Unifies CodeSpace names, to avoid having different names depending on ALS vendor.
#'
#' @param input A data frame of standardized detections.
#'
#' @return A data frame with standardized code spaces.
#'
#' @keywords internal
#'
convertCodes <- function(input) {
  appendTo("debug", "Running convertCodes.")
  vemco <- c("A69-1008",
             "A69-1206",
             "A69-1105",
             "A69-1303")
             # "A69-1601",
             # "A69-1602",
             # "A69-9001",
             # "A69-9002",
             # "A69-9004",
             # "A69-9005",
             # "A69-9006")
  replace <- c("R256",
               "R94K",
               "S256",
               "R64K")
               # "A69-1601",
               # "A69-1602",
               # "A69-9001",
               # "A69-9002",
               # "A69-9004",
               # "A69-9005",
               # "A69-9006")
  for (i in seq_len(length(replace))) {
    levels(input$CodeSpace)[levels(input$CodeSpace) == vemco[i]] <- replace[i]
  }
  return(input)
}

#' Convert Times
#'
#' Converts the ALS timestamps (UTC) to the designated study area time zone. Can also trim the data by time.
#'
#' @inheritParams convertCodes
#' @inheritParams explore
#'
#' @return A data frame with corrected timestamps.
#'
#' @keywords internal
#'
convertTimes <- function(input, start.time, stop.time, tz) {
  # NOTE: The NULL variables below are actually column names used by data.table.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  Timestamp <- NULL

  appendTo("debug", "Running convertTimes.")
  attributes(input$Timestamp)$tzone <- tz
  input <- input[order(input$Timestamp), ]
  if (!is.null(start.time)){
    onr <- nrow(input)
    input <- input[Timestamp >= as.POSIXct(start.time, tz = tz)]
    appendTo(c("Screen","Report"), paste0("M: Discarding detection data previous to ",start.time," per user command (", onr - nrow(input), " detections discarded)."))
  }
  if (!is.null(stop.time)){
    onr <- nrow(input)
    input <- input[Timestamp <= as.POSIXct(stop.time, tz = tz), ]
    appendTo(c("Screen","Report"), paste0("M: Discarding detection data posterior to ",stop.time," per user command (", onr - nrow(input), " detections discarded)."))
  }
  input$Receiver <- droplevels(input$Receiver)
  return(input)
}

### END OF loadDetections HELPERS

#' Include the deployment in the serial number of the receive
#'
#' @param input A data frame with the deployments
#'
#' @return A list of deployments, with unique serial numbers per deployment.
#'
#' @keywords internal
#'
createUniqueSerials <- function(input) {
  appendTo("debug", "Running createUniqueSerials.")
  output <- split(input, input$Receiver)
  for (i in 1:length(output)) {
    if (nrow(output[[i]]) > 1) {
      output[[i]]$Receiver <- paste0(output[[i]]$Receiver, ".dpl.", 1:nrow(output[[i]]))
    }
  }
  return(output)
}

#' Split detections by tag
#'
#' Splits the detections' table by tags and selects only detections from target tags
#'
#' @inheritParams explore
#' @inheritParams loadDetections
#' @param bio A table with the tags and biometrics of the studied animals.
#' @param detections A data frame with all the detections. Supplied by loadDetections.
#'
#' @return A list of detections for each tag.
#'
#' @keywords internal
#'
splitDetections <- function(detections, bio, exclude.tags = NULL) {
  appendTo("debug", "Running splitDetections.")
  
  if (file.exists(paste0(tempdir(), "/temp_strays.csv")))
    file.remove(paste0(tempdir(), "/temp_strays.csv"))

  detections$Transmitter <- droplevels(detections$Transmitter) # failsafe in case all detections for a transmitter were previously excluded
  my.list <- split(detections, detections$Transmitter)
  my.list <- excludeTags(input = my.list, exclude.tags = exclude.tags)

  checkNoDetections(input = my.list, bio = bio)

  checkDupSignals(input = my.list, bio = bio)

  appendTo("debug", "Debug: Creating 'trimmed.list'.")

  # this dataframe serves as an index to the tags detected
  detected <- data.frame(Code.space = extractCodeSpaces(names(my.list)),
                         Signal = extractSignals(names(my.list)))

  # and this one as an index for the target tags
  if (any(grepl("^Code.space$", colnames(bio))))
    bio_aux <- bio[, c("Code.space", "Signal")]
  else
    bio_aux <- data.frame(Code.space = NA,
                    Signal = bio$Signal)

  # break down the signals for multi-signal tags
  bio_aux$Signal_expanded <- lapply(strsplit(as.character(bio$Signal), "|", fixed = TRUE), as.numeric)

  # include sensor units, if relevant
  if (any(grepl("^Sensor.unit$", colnames(bio))))
    bio_aux$Sensor.unit_expanded <- strsplit(as.character(bio$Sensor.unit), "|", fixed = TRUE)
  else
    bio_aux$Sensor.unit_expanded <- NA

  trimmed_list_names <- c() # to store the names as the lapply goes

  appendTo("Screen", "M: Extracting relevant detections...")

  trimmed_list <- lapply(1:nrow(bio_aux), function(i) {
    # cat(i, "\r")
    
    # create/reset variable to store the codespace
    the_codespace <- c()

    # This sapply grabs all entries that match the target signal(s) and code space (if relevant)    
    list_matches <- sapply(bio_aux$Signal_expanded[[i]], function(j) {
      signal_link <- detected$Signal == j
      
      if (sum(signal_link) == 0)
        return(NA)
      
      if (is.na(bio_aux$Code.space[i])) {
        if (sum(signal_link) > 1) # this should never happen because duplicated signals with no codespaces are handled by checkDupSignals
          stopAndReport("Something went wrong when splitting the detections. This should not have happened. Contact the developer. (1)")
    
        the_codespace <<- unique(detected$Code.space[which(signal_link)])
        return(which(signal_link))
      } else {
        codespace_link <- detected$Code.space[which(signal_link)] == bio_aux$Code.space[i]
        if (sum(codespace_link) > 1) # Even if there are multiple codespaces, only one should fit the requested
          stopAndReport("Something went wrong when splitting the detections. This should not have happened. Contact the developer. (2)")

        if (sum(codespace_link) == 0) {
          appendTo(c("Screen", "Report", "Warning"), 
            paste0("Signal ", j, " was found in the detections, but its code space does not match the required ('", 
              bio_aux$Code.space[i],"' != '", paste0(unique(detected$Code.space[which(signal_link)]), collapse = "', '"), 
              "').\n         Are you sure the code space was written correctly? Discarding detections from alien code space(s)."))
          return(NA)
        } else {
          the_codespace <<- detected$Code.space[which(signal_link)][codespace_link]
          return(which(signal_link)[which(codespace_link)])
        }
      }
    })

    # compile the detections list
    if (all(is.na(list_matches))) { # if the tag was not found, return empty
      return(NULL)
    } else { # otherwise, prepare tag name and include sensor units if present
      trimmed_list_names <<- c(trimmed_list_names, paste0(the_codespace, "-", min(bio_aux$Signal_expanded[[i]])))
      output <- my.list[list_matches]

      # Find Sensor.unit column in the biometrics
      if (any(grepl("^Sensor.unit$", colnames(bio)))) {
        # Replace sensor units...
        for (j in 1:length(output)) {
          sensor_index <- match(extractSignals(names(output)[j]), bio_aux$Signal_expanded[[i]])
          # but only if the the sensor unit provided is not NA
          if (!is.na(bio_aux$Sensor.unit_expanded[[i]][sensor_index])) {
            output[[j]]$Sensor.Unit <- rep(bio_aux$Sensor.unit_expanded[[i]][sensor_index], nrow(output[[j]]))
          }
        }
      }
      output <- data.table::rbindlist(output) # merge is required for multiple-signal tags
      output <- output[order(output$Timestamp), ] # order by time before delivering
      return(output)
    }
  })

  # remove empty entries and then name the list components
  trimmed_list <- trimmed_list[!sapply(trimmed_list, is.null)]
  names(trimmed_list) <- trimmed_list_names

  appendTo("debug", "Debug: Creating transmitter codes.")

  # store output of extractSignals before running sapply loop
  # to massively save on processing time.
  trimmed_list_signals <- extractSignals(names(trimmed_list))

  # Extract transmitter names (to store in bio)
  transmitter_names <- sapply(1:nrow(bio), function(i) {
    # cat(i, "\r")

    the_signal <- bio$Signal[i]
    the_codespace <- bio$Code.space[i] # returns NULL if column is missing

    lowest_signal <- min(as.numeric(unlist(strsplit(as.character(the_signal), "|", fixed = TRUE))))

    if (is.null(the_codespace)) {
      link <- match(lowest_signal, trimmed_list_signals) # locations of the lowest_signal in the detected signals
      if (is.na(link))
        output <- paste0('Unknown-', lowest_signal)
      else
        output <- names(trimmed_list)[link]
    }
    else {
      link <- match(paste0(the_codespace, '-', the_signal), names(trimmed_list)) # like above but using full tag codes.
      if (is.na(link))
        output <- paste0(the_codespace, '-', lowest_signal)
      else
        output <- names(trimmed_list)[link]
    }

    return(output)
  })

  # Transfer transmitter names to bio
  bio$Transmitter <- transmitter_names

  appendTo("debug", "Debug: Collecting stray information.")

  # Collect stray summary
  valid_tags <- as.character(unlist(lapply(trimmed_list, function(x) unique(x$Transmitter))))
  stray_tags <- !names(my.list) %in% valid_tags
  if (any(stray_tags)) {
    collectStrays(input = my.list[stray_tags])
  }
  storeStrays()

  return(list(detections.list = trimmed_list, bio = bio))
}

#' Collect summary information on the tags detected but that are not part of the study.
#'
#' @param input list of detections for the tags to be excluded.
#'
#' @return No return value, called for side effects.
#'
#' @keywords internal
#'
collectStrays <- function(input){
  appendTo("debug", "Running collectStrays.")
  if (length(input) > 0) {
    recipient <- data.frame(Transmitter = names(input),
      N.detections = unlist(lapply(input,nrow)),
      First.detection = unlist(lapply(input, function(x) as.character(head(x$Timestamp,1)))),
      Last.detection = unlist(lapply(input, function(x) as.character(tail(x$Timestamp,1)))),
      Receivers = unlist(lapply(input, function(x) paste(unique(x$Receiver), collapse = ", ")))
      )
    write.table(recipient, file = paste0(tempdir(), "/temp_strays.csv"), sep = ",",
      append = file.exists(paste0(tempdir(), "/temp_strays.csv")), row.names = FALSE,
      col.names = !file.exists(paste0(tempdir(), "/temp_strays.csv")))
  }
}

#' Store summary information on the stray tags detected in a permanent file.
#'
#' @return No return value, called for side effects.
#'
#' @keywords internal
#'
storeStrays <- function(){
  appendTo("debug", "Running storeStrays.")
  if (file.exists(paste0(tempdir(), "/temp_strays.csv"))) {
    if (file.exists(newname <- "stray_tags.csv")) {
      continue <- TRUE
      index <- 1
      while (continue) {
        if (file.exists(newname <- paste("stray_tags", index, "csv", sep = "."))) {
          index <- index + 1
        } else {
          continue <- FALSE
        }
      }
    }
    decision <- userInput(paste0("Stray tags were detected in your study area. Would you like to save a summary to ", newname, "?(y/n) "),
                          choices = c("y", "n"), hash = "# save strays?")
    if (!interactive())
      decision <- "y"

    if (decision == "y")
      file.copy(paste0(tempdir(), "/temp_strays.csv"), newname)
  }
}

#' Standardize serial numbers, stations and arrays in the detections
#'
#' Matches the ALS serial number to the deployments to rename the serial number.
#' The corresponding deployment is then used to update the Standard Station name and the array based in the spatial object.
#'
#' @param detections a data frame of detections
#' @param spatial A list of spatial objects in the study area
#' @param deployments a list of deployments
#'
#' @return A data frame with standardized station names.
#'
#' @keywords internal
#'
createStandards <- function(detections, spatial, deployments, discard.orphans = FALSE) {
  appendTo("debug", "Running createStandards.")
  detections$Receiver <- as.character(detections$Receiver)
  detections$Standard.name <- NA_character_
  detections$Array <- NA_character_
  detections$Section <- NA_character_
  empty.receivers <- NULL
  appendTo(c("Screen", "Report"), "M: Matching detections with deployment periods.")
  
  if (interactive())
    pb <- txtProgressBar(min = 0, max = nrow(detections), style = 3, width = 60) # nocov
  counter <- 0
  
  for (i in 1:length(deployments)) {
    receiver.link <- detections$Receiver == names(deployments)[i]
    counter <- counter + sum(receiver.link)
    if (all(!receiver.link)) {
      empty.receivers <- c(empty.receivers, names(deployments)[i])
    } else {
      for (j in 1:nrow(deployments[[i]])) {
        # find target rows in detections
        deployment.link <- detections$Timestamp[receiver.link] >= deployments[[i]]$Start[j] &
          detections$Timestamp[receiver.link] < deployments[[i]]$Stop[j]
        # rename receiver
        detections$Receiver[receiver.link][deployment.link] <- deployments[[i]]$Receiver[j]
        # find corresponding standard station name
        the.station <- match(deployments[[i]]$Station.name[j], spatial$stations$Station.name)
        # include Standard.name
        detections$Standard.name[receiver.link][deployment.link] <- spatial$stations$Standard.name[the.station]
        # include Array
        detections$Array[receiver.link][deployment.link] <- as.character(spatial$stations$Array[the.station])
        # include Section
        if (any(grepl("^Section$", colnames(spatial$stations))))
          detections$Section[receiver.link][deployment.link] <- as.character(spatial$stations$Section[the.station])
      }
      if (any(the.error <- is.na(detections$Standard.name[receiver.link]))) {
        rows.to.remove <- detections[receiver.link, which = TRUE][the.error]
        if (interactive())
          message("") # nocov
        if (!discard.orphans) {
          appendTo(c("Screen", "Report"), paste0("Error: ", sum(the.error), " detections for receiver ", names(deployments)[i], " do not fall within deployment periods."))
          message("")
          if (any(colnames(detections) == "Source.file"))
            message(paste0(capture.output(print(detections[receiver.link, ][the.error, !c("Transmitter", "Valid", "Standard.name", "Array", "Section", "Source.file")])), collapse = "\n"))
          else
            message(paste0(capture.output(print(detections[receiver.link, ][the.error, !c("Transmitter", "Valid", "Standard.name", "Array", "Section")])), collapse = "\n"))
          message("")
          message("Possible options:\n   a) Stop and double-check the data (recommended)\n   b) Discard orphan detections in this instance.\n   c) Discard orphan detections for all instances.\n   d) Save orphan detections to a file and re-open dialogue.")
          
          restart <- TRUE
          while (restart) {
            if (interactive()) { # nocov start
              decision <- userInput("Which option should be followed?(a/b/c/d) ", 
                                    choices = letters[1:4], 
                                    hash = paste("# orphan detections for receiver", names(deployments)[i]))
            } else { # nocov end
              decision <- "b"
            }

            if (decision == "a")
              stopAndReport("Stopping analysis per user command.") # nocov
            
            if (decision == "b") {
              detections <- detections[-rows.to.remove]
              restart <- FALSE
            }

            if (decision == "c") { # nocov start
              detections <- detections[-rows.to.remove]
              discard.orphans <- TRUE
              restart <- FALSE
            } # nocov end

            if (decision == "d") { # nocov start
              file.name <- userInput("Please specify a file name (leave empty to abort saving): ", hash = "# save receiver orphans to this file")
              # break if empty
              if (file.name == "")
                next()
              # confirm extension
              if (!grepl("\\.csv$", file.name))
                file.name <- paste0(file.name, ".csv")
              # prevent auto-overwrite
              if (file.exists(file.name)) {
                aux <- userInput(paste0("File '", file.name, "' already exists. Overwrite contents?(y/n) "), 
                                 choices = c("y", "n"), 
                                 hash = "# overwrite file with same name?")
                if (aux == "y")
                  overwrite <- TRUE
                else
                  overwrite <- FALSE 
              }
              else
                overwrite <- TRUE
              # save
              if (overwrite) {
                success <- TRUE
                # recover if saving fails
                tryCatch(data.table::fwrite(detections[rows.to.remove], file.name, dateTimeAs = "write.csv"), error = function(e) {
                  appendTo(c("Screen", "Report"), paste0("Error: Could not save file (reason: '", sub("\n$", "", e), "').\n       Reopening previous interaction."))
                  success <<- FALSE
                })
                if (success)
                  appendTo(c("Screen", "Report"), paste0("M: A copy of the orphan detections has been saved to '", file.name, "'.\n   Reopening previous interaction."))
              }
            } # nocov end
          }
        } else {
          appendTo(c("Screen", "Report"), paste0("Error: ", sum(the.error), " detections for receiver ", names(deployments)[i], " do not fall within deployment periods. Discarding orphan detections."))
          detections <- detections[-rows.to.remove]
        }
      }
    }
    if (interactive())
      setTxtProgressBar(pb, counter) # nocov
    flush.console()
  }

  if (interactive()) { # nocov start
    setTxtProgressBar(pb, nrow(detections))
    close(pb)
  } # nocov end

  appendTo(c("Screen", "Report"), paste0("M: Number of ALS: ", length(deployments), " (of which ", length(empty.receivers), " had no detections)"))
  
  if (!is.null(empty.receivers))
    appendTo(c("Screen", "Report", "Warning"), paste0("No detections were found for receiver(s) ", paste0(empty.receivers, collapse = ", "), "."))
  
  detections$Receiver <- as.factor(detections$Receiver)
  detections$Array <- factor(detections$Array, levels = unlist(spatial$array.order))
  
  if (any(grepl("^Section$", colnames(spatial$stations))))
    detections$Section <- factor(detections$Section, levels = names(spatial$array.order))
  
  detections$Standard.name <- factor(detections$Standard.name, levels = spatial$stations$Standard.name)
  return(detections)
}

#' Process spatial elements
#'
#' Creates a list containing multiple spatial elements required throughout the analyses
#'
#' @param first.array Either NULL or the top level array in the study area.
#' @inheritParams splitDetections
#' @inheritParams explore
#'
#' @return The stations, release sites and array order.
#'
#' @keywords internal
#'
transformSpatial <- function(spatial, bio, arrays, dotmat, first.array = NULL) {
  appendTo("debug", "Running transformSpatial.")
  # Break the stations away
  appendTo("debug", "Creating 'stations'.")
  stations <- spatial[spatial$Type == "Hydrophone", -match("Type", colnames(spatial))]
  stations$Array <- factor(stations$Array, levels = unique(stations$Array))
  appendTo("debug", "Creating 'release.sites'.")
  # If there is any release site in the spatial file
  if (sum(spatial$Type == "Release") > 0) {
    # If no release sites were specified in the biometrics
    if (length(unique(bio$Release.site)) == 1 && unique(bio$Release.site) == "unspecified") {
      appendTo(c("Screen", "Report", "Warning"), "At least one release site has been indicated in the spatial.csv file, but no release sites were specified in the biometrics file.\n         Discarding release site information and assuming all animals were released at the top level array to avoid function failure.\n         Please double-check your data.")
      # Try to recover by assigning a first array, if possible
      if (is.null(first.array)) {
        stopAndReport("There is more than one top level array in the study area. Please specify release site(s) in the 'spatial.csv' file and in the 'biometrics.csv' file.")
      }
      release.sites <- data.frame(Station.name = "unspecified",
                                  Longitude = NA_real_,
                                  Latitude = NA_real_,
                                  Array = first.array,
                                  Standard.name = "unspecified",
                                  stringsAsFactors = FALSE)
      aux <- table(bio$Group)
      for (i in 1:length(aux)) {
        release.sites[, paste0("n.", names(aux)[i])] <- aux[i]
      }
    } else {
      A <- spatial$Standard.name[spatial$Type == "Release"]
      B <- unique(bio$Release.site)
      # If any release sites in the biometrics are missing in the spatial
      if (any(link <- is.na(match(B, A)))) {
        appendTo(c("Report", "Warning"), "There is a mismatch between the release sites reported and the release locations for the animals.")
        stopAndReport("The following release sites were listed in the biometrics.csv file but are not part of the release sites listed in the spatial.csv file: ",
          paste(sort(B[link]), collapse = ", "), "\nPlease include the missing release sites in the spatial.csv file.")
      } else {
        from.row <- spatial$Type == "Release"
        from.col <- colnames(spatial)[!grepl("^Receiver$", colnames(spatial)) & !grepl("^Section$", colnames(spatial))]
        release.sites <- spatial[from.row, from.col]
        for (i in unique(bio$Group)) {
          aux <- bio[bio$Group == i, ]
          release.sites[, paste0("n.", i)] <- 0
          n <- table(aux$Release.site)
          link <- match(names(n), release.sites$Standard.name)
          release.sites[link, paste0("n.", i)] <- n
        }
        row.names(release.sites) <- 1:nrow(release.sites)
      }
      # just a fancy message
      if (any(link <- grepl("|", release.sites$Array, fixed = TRUE))) {
        if (sum(link) >= 6)
          appendTo(c("Screen", "Report"), "M: Multiple possible first arrays detected for more than five release sites.")
        for (i in which(link)) {
          if (sum(link) < 6)
            appendTo(c("Screen", "Report"), paste0("M: Multiple possible first arrays detected for release site '", release.sites$Standard.name[i], "'."))
          aux <- unlist(strsplit(release.sites$Array[i], "|", fixed = TRUE))
          if (any(is.na(dotmat[aux, aux])) ||  any(dotmat[aux, aux] > 1))
            appendTo(c("Screen", "Report", "Warning"), paste0("Release site ", release.sites$Standard.name[i], " has multiple possible first arrays (",
              paste(aux, collapse = ", "), "), but not all of these arrays appear to be directly connected with each other. Could there be a mistake in the input?"))
        }
      }
    }
  } else {
    appendTo(c("Screen", "Report", "Warning"), "Release sites were not specified in the spatial.csv file. Attempting to assume all released animals start at the top level array.")
    if (is.null(first.array)) {
      stopAndReport("There is more than one top level array in the study area. Please specify release site(s) in the spatial.csv file and in the biometrics.csv file.")
    }
    release.sites <- data.frame(Station.name = unique(bio$Release.site),
                                Longitude = NA_real_,
                                Latitude = NA_real_,
                                Array = rep(first.array, length(unique(bio$Release.site))),
                                Standard.name = unique(bio$Release.site),
                                stringsAsFactors = FALSE)
    for (i in unique(bio$Group)) {
      aux <- bio[bio$Group == i, ]
      release.sites[, paste0("n.", i)] <- 0
      n <- table(aux$Release.site)
      link <- match(names(n), release.sites$Standard.name)
      release.sites[link, paste0("n.", i)] <- n
    }
  }
  # Wrap up
  if (any(grepl("^Section$", colnames(spatial)))) {
    sections <- levels(spatial$Section)
    array.order <- list()  # Used to determine if the tag's last detection was in the last array of a given section
    for (j in sections) {
      array.order[[j]] <- unique(spatial$Array[spatial$Type == "Hydrophone" & spatial$Section == j])
    }
  } else {
    array.order <- list(all = names(arrays))
  }
  # Order release sites by entry point.
  first.release.arrays <- sapply(as.character(release.sites$Array), function(x) unlist(strsplit(x, "|", fixed = TRUE))[1])
  if (!is.ordered(match(first.release.arrays, unlist(array.order))))
    release.sites <- release.sites[order(match(first.release.arrays, unlist(array.order))),]
  # join everything
  output <- list(stations = stations,
                 release.sites = release.sites,
                 array.order = array.order)
  return(output)
}

#' Collect summary information on the tags detected but that are not part of the study.
#'
#' @param input list of detections
#' @inheritParams explore
#' @inheritParams splitDetections
#'
#' @return A list of detections for each tag that does not contain the excluded tags.
#'
#' @keywords internal
#'
excludeTags <- function(input, exclude.tags){
  appendTo("debug", "Running excludeTags.")
  if (length(exclude.tags) != 0) {
    link <- match(exclude.tags, names(input))
    logical_link <- !is.na(link)
    if (any(!logical_link)) {
      appendTo(c("Screen", "Report", "Warning"), paste0("The user asked for ",
        ifelse(sum(!logical_link) > 1, "tags '", "tag '"),
        paste(exclude.tags[!logical_link], collapse = "', '"),
        "' to be excluded from the analysis, but ",
        ifelse(sum(!logical_link) > 1, "these tags are", "this tag is"),
        " not present in the detections."))
    }
    if (!all(!logical_link)) {
      link <- link[!is.na(link)]
      appendTo(c("Screen", "Report"), paste0("M: Excluding tag(s) ", paste(exclude.tags[logical_link], collapse = ", "), " from the analysis per used command (detections removed: ", paste(unlist(lapply(input[link], nrow)), collapse = ", "), ifelse(length(input[link]) > 1, ", respectively).", ").")))
      collectStrays(input = input[link])
      return(input[-link])
    } else {
      return(input)
    }
  }
  return(input)
}

#' Discard early detections
#'
#' @param input The detections list
#' @param bio The biometrics table
#' @param trim The threshold time after release, in hours
#'
#' @return the updated detections list
#'
#' @keywords internal
#'
discardFirst <- function(input, bio, trim) {
  link <- match(names(input), bio$Transmitter)
  count <- 0
  output <- lapply(seq_along(input), function(i) {
    output_i <- input[[i]]
    output_i$Valid[output_i$Timestamp <= bio$Release.date[i] + (trim * 3600)] <- FALSE
    count <<- count + (sum(!output_i$Valid))
    appendTo("debug", paste0("M: ", sum(!output_i$Valid), " early detection(s) invalidated for tag ", names(input)[i], "."))
    return(output_i)
  })
  names(output) <- names(input)
  appendTo("Screen", paste0("M: ", count, " detection(s) were invalidated because they were recorded before the time set in 'discard.first' had passed."))
  return(output)
}

#' Assign live times to arrays
#' 
#' @param arrays The array list
#' @param deployments the deployments list
#' @param spatial The spatial list
#' 
#' @return an expanded array list
#' 
#' @keywords internal
#' 
liveArrayTimes <- function(arrays, deployments, spatial) {
  xdep <- do.call(rbind, deployments)
  capture <- lapply(names(arrays), function(a) {
    # cat(a, "\n")
    sts <- spatial$stations$Standard.name[spatial$stations$Array == a]

    if (sum(xdep$Standard.name %in% sts) > 1) {
      aux <- xdep[xdep$Standard.name %in% sts, ]
      aux <- aux[order(aux$Start, aux$Stop), ]
      aux$overlaps <- c(FALSE, aux$Stop[-nrow(aux)] >= aux$Start[-1])

      if (any(aux$overlaps)) {
        restart <- TRUE
        while (restart) {
          breaks <- rle(aux$overlaps)
          to.combine <- data.frame(from = cumsum(breaks$lengths)[which(breaks$values) - 1],
                                   to = cumsum(breaks$lengths)[which(breaks$values)])

          aux$isolated <- !aux$overlaps & !c(aux$overlaps[-1], FALSE)

          output <- aux[aux$isolated, c("Start", "Stop")]

          for (i in 1:nrow(to.combine)) {
            tmp <- data.frame(Start = aux$Start[to.combine$from[i]],
                              Stop = max(aux$Stop[to.combine$from[i]:to.combine$to[i]]))
            output <- rbind(output, tmp)
          }

          output <- output[order(output$Start, output$Stop), ]

          output$overlaps <- c(FALSE, output$Stop[-nrow(output)] >= output$Start[-1])

          if (any(output$overlaps))
            aux <- output
          else
            restart <- FALSE
        }
      } else {
        output <- aux
      }
    } else {
      output <- xdep[xdep$Standard.name %in% sts, ]
    }
    # store the result
    row.names(output) <- 1:nrow(output)
    arrays[[a]]$live <<- output[, c("Start", "Stop")]
  })
  return(arrays)
}
