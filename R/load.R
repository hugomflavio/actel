#' Load, structure and check the inputs
#'
#' @inheritParams explore
#' @inheritParams migration
#'
#' @return A list containing:
#' \itemize{
#'  \item \code{bio}: A data frame corresponding to 'biometrics.csv'
#'  \item \code{sections}: A vector of the study area sections (or NULL in the
#'   case of the `explore` function)
#'  \item \code{deployments}: A list corresponding to 'deployments.csv'
#'  \item \code{spatial}: A list corresponding to 'spatial.csv'
#'  \item \code{dot_list}: A list containing detailed information on the
#'   study area configuration.
#'  \item \code{dist.mat}: A matrix of the distances (in metres) between
#'   stations (if a 'distances.csv' is present)
#'  \item \code{detections.list}: A list containing the detection data
#'   for each tag
#' }
#'
#' @keywords internal
#'
loadStudyData <- function(tz, override = NULL, start.time,
                          stop.time, save.detections = FALSE,
                          section.order = NULL, exclude.tags,
                          disregard.parallels = TRUE, discard.orphans = FALSE) {
  event(type = "debug", "Running loadStudyData.")
  loading.failed <- TRUE
  # debug lines
    if (getOption("actel.debug", default = FALSE)) { # nocov start
      on.exit(add = TRUE,
              if (loading.failed) {
                the_path <- paste0(tempdir(), "/actel.load.debug.RData")
                event(type = "screen",
                      "Debug: Function failed during loading.",
                      " Saving load environment to ",
                      gsub("\\\\", "/", the_path))
              }
      )
      on.exit(add = TRUE,
              if (loading.failed) {
                save(list = ls(),
                     file = paste0(tempdir(), "/actel.load.debug.RData"))
              }
      )
    } # nocov end
  # ------------------------

  event(type = c("screen", "report"),
        "M: Importing data. This process may take a while.")
  bio <- loadBio(input = "biometrics.csv", tz = tz)
  event(type = c("screen", "report"),
        "M: Number of target tags: ", nrow(bio), ".")

  # Check that all the overridden tags are part of the study
  if (!is.null(override)) {
    if (is.numeric(override)) {

      # Legacy signal-only override compatibility
      lowest_signals <- sapply(bio$Signal,
            function(i) {
              min(as.numeric(unlist(strsplit(as.character(i),
              "|", fixed = TRUE))))
            }
          )

      if (any(table(lowest_signals[lowest_signals %in% override]) > 1)) {
        event(type = "stop",
              'Override is numeric but there are multiple tags',
              'that match the overridden signal. Use codespace-signal',
              ' overrides instead.')
      }

      if (any(link <- is.na(match(override, lowest_signals)))) {
        event(type = "stop",
              "Some tags listed in 'override' (",
              paste0(override[link], collapse = ", "),
              ") are not listed in the biometrics file.")
      }
    }
    else {
      # new override based on full tag
      if (any(link <- is.na(match(override, bio$Transmitter)))) {
        event(type = "stop",
              "Some tags listed in 'override' (",
              paste0(override[link], collapse = ", "),
              ") are not listed in the biometrics file.")
      }

    }
  }

  deployments <- loadDeployments(input = "deployments.csv", tz = tz)

  # check that receivers are not deployed before being retrieved
  checkDeploymentTimes(input = deployments)

  spatial <- loadSpatial(input = "spatial.csv", section.order = section.order)

  # match Station.name in the deployments to Station.name in spatial,
  # and vice-versa
  deployments <- checkDeploymentStations(input = deployments, spatial = spatial)

  # Prepare serial numbers to overwrite the serials in detections
  deployments <- createUniqueSerials(input = deployments)

  detections <- loadDetections(start.time = start.time, stop.time = stop.time,
                               tz = tz, save.detections = save.detections,
                               record.source = TRUE)
  detections <- checkDupDetections(input = detections)
  use.fakedot <- TRUE
  if (file.exists("spatial.dot")) {
    event(type = c("screen", "report"),
          "M: A 'spatial.dot' file was detected",
          ", activating multi-branch analysis.")
    dot_list <- loadDot(input = "spatial.dot", spatial = spatial,
                        disregard.parallels = disregard.parallels)
    use.fakedot <- FALSE
  }
  if (use.fakedot & file.exists("spatial.txt")) {
    event(type = c("screen", "report"),
          "M: A 'spatial.txt' file was detected, activating",
          " multi-branch analysis.")
    dot_list <- loadDot(input = "spatial.txt", spatial = spatial,
                        disregard.parallels = disregard.parallels)
    use.fakedot <- FALSE
  }
  if (use.fakedot) {
    n <- length(unique(spatial$Array[spatial$Type == "Hydrophone"]))
    if (n > 1) {
      fakedot <- paste(unique(spatial$Array[spatial$Type == "Hydrophone"]),
                       collapse = "--")
    } else {
      aux <- unique(spatial$Array[spatial$Type == "Hydrophone"])
      fakedot <- paste(aux, "--", aux)
    }
    dot_list <- loadDot(string = fakedot, spatial = spatial,
                        disregard.parallels = disregard.parallels)
  }

  # Check if there is a logical first array in the study area, should a
  # replacement release site need to be created.
  link <- sapply(dot_list$array_info$arrays, function(a) is.null(a$before))
  if (sum(link) == 1) {
    first.array <- names(dot_list$array_info$arrays)[link]
  } else {
    first.array <- NULL
  }
  # Finish structuring the spatial file
  spatial <- transformSpatial(spatial = spatial, bio = bio,
                              dot_list = dot_list, first.array = first.array)

  # Get standardized station and receiver names, check for receivers
  # with no detections
  detections <- createStandards(detections = detections, spatial = spatial,
                                deployments = deployments,
                                discard.orphans = discard.orphans)

  # Check if there are detections from unknown receivers
  checkUnknownReceivers(input = detections)

  event(type = c("screen","report"),
        "M: Data time range: ",
        as.character(head(detections$Timestamp, 1)),
        " to ", as.character(tail(detections$Timestamp, 1)),
        " (", tz, ").")

  dot_list$array_info$arrays <- liveArrayTimes(
    arrays = dot_list$array_info$arrays,
    deployments = deployments,
    spatial = spatial)

  # Reorder arrays object by spatial order
  link <- match(unlist(spatial$array.order), names(dot_list$array_info$arrays))
  dot_list$array_info$arrays <- dot_list$array_info$arrays[link]
  rm(link)

  # Load distances and check if they are valid
  dist.mat <- loadDistances(spatial = spatial)

  # Split the detections by tag, store full transmitter names in bio
  recipient <- splitDetections(detections = detections, bio = bio,
                               exclude.tags = exclude.tags)

  detections.list <- recipient$detections.list
  bio <- recipient$bio
  if (is.null(detections.list) | is.null(bio)) {
    event(type = "stop",
          "Something went wrong when assigning recipient objects",
          " (splitDetections). If this error persists,",
          " contact the developer.") # nocov
  }
  rm(recipient)

  # Check if there is any data loss due to unknown receivers
  recipient <- checkTagsInUnknownReceivers(detections.list = detections.list,
                                           deployments = deployments,
                                           spatial = spatial)

  spatial <- recipient$spatial
  deployments <- recipient$deployments
  detections.list <- recipient$detections.list
  if (is.null(spatial) | is.null(deployments) | is.null(detections.list)) {
    event(type = "stop",
          "Something went wrong when assigning recipient objects",
          " (unknownReceivers). If this error persists,",
          " contact the developer.") # nocov
  }
  rm(recipient)

  detections.list <- checkDetectionsBeforeRelease(input = detections.list,
                                              bio = bio,
                                              discard.orphans = discard.orphans)
  event(type = c("screen", "report"),
        "M: Data successfully imported!")

  loading.failed <- FALSE

  return(list(bio = bio,
              deployments = deployments,
              spatial = spatial,
              dot_list = dot_list,
              dist.mat = dist.mat,
              detections.list = detections.list))
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
#'  \item \code{dotmat}: A matrix of the distance (in number of arrays)
#'   between pairs of arrays
#'  \item \code{paths}: A list of the all array paths
#'   between each pair of arrays.
#' }
#'
#' @keywords internal
#'
loadDot <- function(string = NULL, input = NULL, spatial,
                    disregard.parallels, preloading = FALSE) {
  event(type = "debug", "Running loadDot.")
  if (is.null(string) & is.null(input)) {
    event(type = "stop",
          "No dot file or dot string were specified.")
  }
  if (is.null(string)) {
    tryCatch(dot <- readDot(input = input, silent = TRUE),
      error = function(e) event(type = "stop",
                               "The contents of the '", input,
                               "' file could not be recognised",
                               " by the readDot function."))
  } else {
    dot <- readDot(string = string, silent = TRUE)
  }
  mat <- dotMatrix(input = dot)
  aux <- sapply(spatial$Array,
                function(x) {
                  unlist(strsplit(x, "|", fixed = TRUE))
                }
  )
  unique.arrays <- unique(unlist(aux))
  link <- is.na(match(unique.arrays, colnames(mat)))
  if (any(link)) {
    if (preloading) {
      event(type = "stop",
            "Not all arrays listed in the spatial input are",
            " present in the dot input. Double-check that the",
            " array names in the dot string match the array",
            " names in the spatial input.\n       Missing arrays: ",
            paste(unique.arrays[link], collapse = ", "), "\n")
    } else {
      if (file.exists("spatial.txt")) {
        event(type = "stop",
              "Not all the arrays listed in the spatial.csv",
              " file are present in the spatial.txt.",
              "\n       Missing arrays: ",
              paste(unique.arrays[link], collapse = ", "), "\n")
      }
      if (file.exists("spatial.dot")) {
        event(type = "stop",
              "Not all the arrays listed in the spatial.csv",
              " file are present in the spatial.dot.",
              "\n       Missing arrays: ",
              paste(unique.arrays[link], collapse = ", "), "\n")
      }
      if (!file.exists("spatial.txt") & !file.exists("spatial.dot")) {
        event(type = "stop",
              "Something went wrong when compiling the dot input.",
              " Try restarting R and trying again. If the problem",
              " persists, contact the developer.")
        }
    }
  }
  link <- is.na(match(colnames(mat), unique.arrays))
  if (any(link)) {
    if (preloading) {
      event(type = "stop",
            "Not all arrays listed in the dot input are",
            " present in the spatial input. The dot input",
            " should only contain arrays that are listed in",
            " spatial.\n       Alien arrays: ",
            paste(colnames(mat)[link], collapse = ", "), "\n")
    } else {
      if (file.exists("spatial.txt")) {
        event(type = "stop",
              "Some arrays listed in the spatial.txt file",
              " are not present in the spatial.csv file. The",
              " dot input should only contain arrays that are",
              " listed in spatial.\n       Alien arrays: ",
              paste(colnames(mat)[link], collapse = ", "), "\n")
      }
      if (file.exists("spatial.dot")) {
        event(type = "stop",
              "Some arrays listed in the spatial.dot file are",
              " not present in the spatial.csv file. The dot",
              " input should only contain arrays that are",
              " listed in spatial.\n       Alien arrays: ",
              paste(colnames(mat)[link], collapse = ", "), "\n")
      }
      if (!file.exists("spatial.txt") & !file.exists("spatial.dot")) {
        event(type = "stop",
              "Something went wrong when compiling the dot input. Try",
              " restarting R and trying again. If the problem",
              " persists, contact the developer.")
      }
    }
  }
  arrays <- dotList(input = dot, spatial = spatial)
  arrays <- dotPaths(input = arrays, disregard.parallels = disregard.parallels)
  array_paths <- findShortestChains(input = arrays)

  array_info <- list(arrays = arrays,
                     dotmat = mat,
                     paths = array_paths)

  # section connections
  # use array dot to make section connections
  sdot <- dot
  # swap array names with section names
  link <- match(dot$A, spatial$Array)
  sdot$A <- spatial$Section[link]
  link <- match(dot$B, spatial$Array)
  sdot$B <- spatial$Section[link]
  # no support for one-way section connections
  sdot$to <- "--"
  # remove repeated rows
  check <- apply(sdot, 1, paste, collapse = "")
  sdot <- sdot[!duplicated(check),]
  # remove self connections
  check <- sdot$A != sdot$B
  sdot <- sdot[check, ]

  if (nrow(sdot) > 0) {
    smat <- dotMatrix(input = sdot)
    sections <- dotList(input = sdot, spatial = spatial, style = "s")
    sections <- dotPaths(input = sections, disregard.parallels = FALSE)
    section_paths <- findShortestChains(input = sections)
  } else {
    aux <- list(arrays = unique(spatial$Array[spatial$Type == "Hydrophone"]))
    sections <- list(aux)
    names(sections) <- unique(spatial$Section[spatial$Type == "Hydrophone"])
    smat <- matrix(0)
    rownames(smat) <- unique(spatial$Section[spatial$Type == "Hydrophone"])
    colnames(smat) <- unique(spatial$Section[spatial$Type == "Hydrophone"])
    section_paths <- list()
  }

  section_info <- list(sections = sections,
                       dotmat = smat,
                       paths = section_paths)

  # and all out
  output <- list(dot = dot,
                 array_info = array_info,
                 section_info = section_info)
  return(output)
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
  event(type = "debug", "Running readDot.")
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
  if (length(paths) == 0) {
    stop("Could not recognise the input contents as DOT formatted connections.",
         call. = FALSE)
  }

  # if something looks like a badly formatted connector, complain and stop
  if (any(grepl("<<|>>|>-|-<|><|<>|<->", paths))) {
    stop("The input appears to have badly formatted connectors",
         " ('<<', '>>', '>-', '-<'', '><', '<>' or '<->'). Please fix",
         " these before continuing.", call. = FALSE)
  }

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
    if (!silent) {
      event(type = c("warning", "screen"),
            "Replacing spaces with '_' in the node names.")
    }
    paths <- gsub(" ", "_", paths)
  }
  check <- grepl("\\\\|/|:|\\*|\\?|\\\"|<(?!-)|(?<!-)>|\\\'",
                 paths, perl = TRUE)
  if (any(check)) {
    if (!silent) {
      event(type = c("warning", "screen"),
            "Troublesome characters found in the node names (\\/:*?\"<>\').",
            " Replacing these with '_'.")
      }
    paths <- gsub("\\\\|/|:|\\*|\\?|\\\"|<(?!-)|(?<!-)>|\\\'", "_",
                  paths, perl = TRUE)
  }
  nodes <- strsplit(paths,"[<-][->]")
  recipient <- data.frame(A = character(),
                          to = character(),
                          B = character(),
                          stringsAsFactors = FALSE)
  for (i in 1:length(nodes)) {
    n <- length(nodes[[i]])
    escaped_nodes <- gsub(pattern = "([[:punct:]])",
                          replacement = "\\\\\\1", nodes[[i]])
    type <- gsub(paste0(escaped_nodes, collapse = "|"), "", paths[[i]])
    aux <- data.frame(
      A = nodes[[i]][1:(n - 1)],
      to = sapply(seq(from = 1, to = nchar(type), by = 2),
                  function(i) substr(type, i, i + 1)),
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
#' @return A matrix of the distance (in number of arrays)
#'   between pairs of arrays
#'
#' @keywords internal
#'
dotMatrix <- function(input) {
  event(type = "debug", "Running dotMatrix.")
  nodes <- unique(unlist(input[, c(1, 3)]))
  graph <- matrix(0, length(nodes), length(nodes),
                  dimnames = list(nodes, nodes))
  if (any(input$to != "--" & input$to != "<-" & input$to != "->"))
    event(type = "stop",
          "Unrecognized connectors. Only use '--', '->' or",
          " '<-' to connect nodes.\n")
  for (i in 1:nrow(input)) {
    if (input$to[i] == "--") {
      graph[input$A[i], input$B[i]] <- 1
      graph[input$B[i], input$A[i]] <- 1
    }
    if (input$to[i] == "->") {
      graph[input$A[i], input$B[i]] <- 1
    }
    if (input$to[i] == "<-") {
      graph[input$B[i], input$A[i]] <- 1
    }
  }
  for (i in 1:(length(nodes)-1)) {
    for (A in nodes) {
      for (B in nodes) {
        if (graph[A, B] == i) {
          # cat(B, "\n")
          candidates <- rownames(graph) != B
          candidates <- candidates & rownames(graph) != A
          candidates <- candidates & graph[B, ] == 1
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
#' @param style one of array or section, depending on the type of list we want
#'
#' @return  A list containing detailed information on the arrays
#'
#' @keywords internal
#'
dotList <- function(input, spatial, style = c("array", "section")) {
  event(type = "debug", "Running dotList.")
  style <- match.arg(style)

  # For array-style lists, if there are sections, determine which connections
  # are at the edge between sections
  if (style == "array" && any(grepl("^Section$", colnames(spatial)))) {
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

  output <- list()
  for (i in unique(c(t(input[, c(1, 3)])))) { # go through each unique node
    auxA <- input[input$A == i, ]
    auxA <- auxA[auxA$to != "<-", ]
    auxB <- input[input$B == i, ]
    auxB <- auxB[auxB$to != "->", ]
    # find parallel output (i.e. both before and after)
    par.trigger <- FALSE
    if (nrow(auxA) > 0 & nrow(auxB) > 0) {
      parallel <- sapply(auxA[, 3], function(x) match(x, auxB[, 1]))
      if (any(!is.na(parallel))) {
        # discount those from the before and after, and trigger parallel inclusion
        auxA <- auxA[-which(!is.na(parallel)), , drop = FALSE]
        auxB <- auxB[-parallel[!is.na(parallel)], , drop = FALSE]
        par.trigger <- TRUE
      }
    } else {
      parallel <- NA
    }
    all_nearby <- c(auxA$B, auxB$A, names(parallel)[!is.na(parallel)])
    if (style == "array") {
      recipient <- list(section = ifelse(nrow(auxA) > 0,
                                         auxA$SectionA[1],
                                         auxB$SectionB[1]),
                        neighbours = unique(all_nearby))
    } else {
      aux <- spatial$Array[na.as.false(spatial$Section == i)]
      recipient <- list(arrays = unique(aux),
                        neighbours = unique(all_nearby))      
    }
    if (nrow(auxB) > 0) {
      recipient$before <- unique(auxB$A)
    }
    if (nrow(auxA) > 0) {
      recipient$after <- unique(auxA$B)
    }
    if (par.trigger) {
      recipient$parallel <- unique(names(parallel)[!is.na(parallel)])
    }
    if (style == "array") {
      recipient$edge <- any(auxA$Edge) | any(auxB$Edge)
    }
    output[[length(output) + 1]] <- recipient
    names(output)[length(output)] <- i
  }
  attributes(output)$type <- style
  return(output)
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
  event(type = "debug", "Running dotPaths.")

  for (direction in (c("before", "after"))) {
    capture <- lapply(names(input), function(a) {
      p <- paste0(direction, ".peers")
      input[[a]][[p]] <<- findPeers(array = a,
                                    network = input,
                                    direction = direction,
                                    disregard.parallels = disregard.parallels)
      recipient <- findDirectChains(array = a,
                                    network = input,
                                    direction = direction)
      input[[a]][[paste0("all.", direction)]] <<- recipient[[1]]
      input[[a]][[paste0("all.", direction, ".and.par")]] <<- recipient[[2]]
    })
  }

  return(input)
}

#' Find efficiency peers for a specific array
#'
#' @param array The array for which to find peers
#' @param network An array list
#' @param direction The direction of peers to be
#'   found ("before" or "after")
#'
#' @return The array list with efficiency peers.
#'
#' @keywords internal
#'
findPeers <- function(array, network,
                      direction = c("before", "after"),
                      disregard.parallels) {
  event(type = "debug", "Running findPeers.")
  direction <- match.arg(direction)
  opp_dir <- ifelse(direction == "before", "after", "before")

  if (length(array) > 1) {
    event(type = "stop",
          "'array' must be of length 1. This error should never",
          " happen. Contact developer.") # nocov
  }

  if (!(array %in% names(network))) {
    event(type = "stop",
          "Requested array does not exist in the array list",
          " (findPeers). This error should never happen.",
          " Contact developer.") # nocov
  }

  # start with nothing
  usable_peers <- c()

  # placeholder just to trigger the start of the while loop
  check_results <- c(TRUE, FALSE)

  event(type = "debug",
        "Finding ", direction, " peers for array ", array)

  while (any(check_results) & !all(check_results)) {
      # Find every array in the network that has
      # not been deemed a peer yet.
      checked <- names(network) %in% c(array, usable_peers)
      to_check <- names(network)[!checked]
      
      event(type = "debug",
            "Round candidates: ",
            paste0(to_check, collapse = ", "))

      check_results <- sapply(network[to_check], function(candidate) {
        # If the candidate does not connect to anything in the opposite
        # direction, then it cannot be a valid peer.
        # e.g. if I have A -- B -- C, A cannot be the "after" peer of anyone,
        # because nothing comes "before" anything.
        if (length(candidate[[opp_dir]]) == 0){
          return(FALSE) # not worth continuing
        }

        # There are two types of parallels that can cause trouble:
        # 1) parallels in the array (for which we are determining peers)
        # 2) parallels in the candidate (which we're trying to determine
        #    as a valid peer)
        #
        # Type 1 (pars in the array) is only an issue if we want to ignore
        # parallel arrays (i.e. disregard.parallels = TRUE) and the array
        # is right next to the candidate.
        if (disregard.parallels & array %in% candidate[[opp_dir]]) {
          # For the candidate to be a valid peer of the array, the max number
          # of connections to the candidate can only be the sum of the peers
          # we already know about + the array + and any parallels of the array
          n_to_cand <- length(candidate[[opp_dir]])
          n_peers <- length(usable_peers)
          n_par <- length(network[[array]]$parallel)
          too_many <- n_to_cand > sum(n_peers, n_par, 1)

          # Additionally, all the connections to the candidate must be either
          # the array, a parallel of the array that shares all connections
          # with array, or an array that has already been determined as a
          # valid peer.
          valid_par <- sapply(network[[array]]$parallel,
                              function(parallel) {
                                par_con <- network[[parallel]][[opp_dir]]
                                array_con <- network[[array]][[opp_dir]]
                                all(par_con %in% array_con)
                              }
          )
          valid_par <- names(valid_par)[valid_par]
          valid_conns <- c(array, valid_par, usable_peers)
          all_valid <- all(candidate[[opp_dir]] %in% valid_conns)
        } else {
          # In a situation where either disregard.parallels = FALSE, or the
          # array is not directly next to the candidate, then the max number
          # of connections to the candidate can only be the sum of the peers
          # we already know about, plus the array.
          n_to_cand <- length(candidate[[opp_dir]])
          n_peers <- length(usable_peers)
          too_many <- n_to_cand > sum(n_peers, 1)
          # Additionally, all the connections to the candidate must be either
          # the array, or an array that has already been determined as a
          # valid peer. Note that parallels are not allowed here, even if
          # disregard.parallels = TRUE. If this ever becomes a point of
          # confusion, find the drawings in issue #72.
          valid_conns <- c(array, usable_peers)
          all_valid <- all(candidate[[opp_dir]] %in% valid_conns)
        }

        if (too_many | !all_valid){
          return(FALSE) # not worth continuing
        }

        # Type 2 (pars in the candidate) is only relevant if 
        # disregard.parallels = FALSE. Here, we have to confirm if the arrays
        # that are parallel to the candidate do not have any third-party
        # connections
        # that are not, in themselves, a valid peer of array. E.g. if we have:      
        # A -- B -- C -- D
        # B -- E -- D
        # C -- E -- C
        # F -- E
        # Which would look something like:
        # A -- B -- C -- D
        #        \  |  /
        #         \ | /
        #      F -- E   (note: C and E are parallel)
        #
        # when finding peers for B, in the two checks above, array C will emerge
        # as a potentially valid peer. If we disregard parallels, than that is
        # indeed the case. However, if we do not disregard parallels, then array
        # E (a parallel of C) will cause array C to be invalidated, due to the
        # connection coming from array F. However, this wouldn't be a problem if
        # F were a valid peer of B (e.g. if B -- F).
        if (disregard.parallels) {
          pars_are_ok <- TRUE
        } else {
          # So, if disregard.parallels = FALSE, and the candidate has parallels,
          # we need to go find which arrays lead to said parallels.
          aux <- sapply(candidate$parallel, 
                        function(parallel) {
                          network[[parallel]][[opp_dir]]
                        }
                 )
          leading_to_pars <- unique(unlist(aux))
          # Finally, we verify that only valid peers of array lead to the
          # parallels listed above.
          pars_are_ok <- all(leading_to_pars %in% c(array, usable_peers))
        }

        if (pars_are_ok) {
          # then the candidate is a valid peer of array
          return(TRUE)
        } else {
          # then the candidate is _not_ a valid peer of array (yet!)
          return(FALSE)
        }
      })

      # store the new peers together with the rest, and restart the loop.
      # loop will stop once no new peers are found during an iteration.
      if (any(check_results)) {
        usable_peers <- c(usable_peers, to_check[check_results])
      }

      event(type = "debug", 
            "Usable peers at end of round:",
            paste0(usable_peers, collapse = ", "))
  }
  return(usable_peers)
}

#' Find all arrays linked to an array in a given direction
#'
#' @inheritParams findPeers
#' @param direction The direction in which to expand the chain
#'   ("before" or "after")
#'
#' @return The array list with all linked arrays.
#'
#' @keywords internal
#'
findDirectChains <- function(array, network, direction = c("before", "after")) {
  event(type = "debug", "Running findDirectChains.")
  direction <- match.arg(direction)
  chain <- NULL
  pars <- network[[array]]$parallel
  to_check <- network[[array]][[direction]]
  while (!is.null(to_check)) {
    new_check <- NULL
    for (b in to_check) {
        if (is.null(chain) || all(!grepl(paste0("^", b, "$"), chain))) {
          chain <- c(chain, b)
          pars <- c(pars, network[[b]]$parallel)
          new_check <- c(new_check, network[[b]][[direction]])
        }
      to_check <- unique(new_check)
    }
  }
  output <- list(chain = unique(chain), unique(c(chain, pars)))
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
  event(type = "debug", "Running findShortestChains.")
  # List to store the paths
  the_paths <- list()
  for (A in names(input)) {
    # make vector of arrays to look for
    look_for <- rep(NA, length(input))
    names(look_for) <- names(input)
    # Don't look for the own array
    look_for[A] <- 0
    # Set neighbours with distance 1, to start
    look_for[as.character(input[[A]]$neighbours)] <- 1
    # begin. 
    # The loop below looks for the shortest way to all the other
    # nodes simultaneously.
    this_step <- 0
    more_to_find <- TRUE
    while (more_to_find) {
      # increment steps
      this_step <- this_step + 1
      # starting points are those that match the steps
      # e.g. the first iteration starts from the neighbours (1 step to reach)
      to_check <- names(look_for)[na.as.false(look_for == this_step)]
      # nodes to look for are those that have not been seen yet
      to_look <- names(look_for)[is.na(look_for)]
      # for each of the starting points...
      for (i in to_check) {
        # see if the ones we're looking for are their neighbours
        aux <- match(to_look, input[[i]]$neighbours)
        # if any neighbours are who we're looking for...
        if (any(!is.na(aux))) {
          # grab their names
          aux <- input[[i]]$neighbours[na.as.false(aux)]
          # and then, for each of them
          for (found in aux) {
            # if the path is just starting, then just paste
            # the name of the starting point
            if (is.null(the_paths[[paste0(A, "_to_", i)]])) {
              to.add <- i
            # else concatenate the stepping stones with " -> "
            } else {
              to.add <- paste(the_paths[[paste0(A, "_to_", i)]],
                              i, sep = " -> ")
            }
            A_to_found <- paste0(A,"_to_",found)
            # if this is the first short path found, simply allocate.
            # otherwise, store the multiple possible shortest paths together
            if (is.null(the_paths[[A_to_found]])) {
              the_paths[[A_to_found]] <- to.add
            } else {
              the_paths[[A_to_found]] <- c(the_paths[[A_to_found]], to.add)
            }
            # allocate the shortest distance to the look_for vector
            look_for[found] <- this_step + 1
          }
        }
      }
      # now, to decide if we go again:
      # 1) Is there anything else to look for?
      check1 <- any(is.na(look_for))
      # 2) Are there new paths still available?
      check2 <- any(na.as.false(look_for == (this_step + 1))) 
      # If yes, then go again.
      more_to_find <- check1 & check2
    }
  }
  return(the_paths)
}

#' Create Standard Names for spatial elements
#'
#' Includes standard names and also reprints 'spatial.csv'
#'
#' @param input A data frame with spatial information.
#'
#' @return A data frame with the same information as the input plus
#'         a Standard.name column.
#'
#' @keywords internal
#'
setSpatialStandards <- function(input){
  event(type = "debug","Running setSpatialStandards.")
  std_names <- paste0("St.", seq_len(sum(input$Type == "Hydrophone")))

  input$Standard.name <- as.character(input$Station.name)
  input$Standard.name <- gsub(" ", "_", input$Standard.name)
  link <- input$Type == "Hydrophone"
  input$Standard.name[link] <- std_names

  return(input)
}

#' Load distances matrix
#'
#' @param input Either a path to a csv file containing a distances matrix,
#'  or an R object containing a distances matrix.
#' @param spatial A list of spatial objects in the study area.
#'
#' @return A matrix of the distances (in metres) between stations
#'  (if a 'distances.csv' is present)
#'
#' @keywords internal
#'
loadDistances <- function(input = "distances.csv", spatial) {
  event(type = "debug", "Running loadDistances.")
  # Check for distances
  invalid.dist <- TRUE
  if (is.character(input)) {
    if (file.exists(input)) {
      event(type = c("screen", "report"),
            "M: File '", input, "' found, activating speed calculations.")
      dist.mat <- as.matrix(read.csv(input, row.names = 1, check.names = FALSE))
      if (ncol(dist.mat) == 1) {
        event(type = c("warning", "screen"),
              "Only one column was identified in '",
              input, "'. If this seems wrong,",
              " please make sure that the values are separated using commas.")
      }
    } else {
      dist.mat <- NA
    }
  } else {
    # Ensure the input is a matrix,
    # in case the user provided the data in a non-base format
    dist.mat <- as.matrix(input)
  }

  if (is.matrix(dist.mat)) {
    rownames(dist.mat) <- gsub(" ", "_", rownames(dist.mat))
    colnames(dist.mat) <- gsub(" ", "_", colnames(dist.mat))
    invalid.dist <- FALSE
    if (nrow(dist.mat) != ncol(dist.mat)) {
      event(type = c("warning", "screen", "report"),
            "The distances matrix appears to be missing data (ncol != nrow).",
            " Deactivating speed calculations to avoid function failure.")
      invalid.dist <- TRUE
    }
    if (!invalid.dist) {
      row_link <- is.na(match(rownames(dist.mat), colnames(dist.mat)))
      col_link <- is.na(match(colnames(dist.mat), rownames(dist.mat)))
      if (any(row_link) | any(col_link)) {
        event(type = c("warning", "screen", "report"),
              "The column and row names in the distances matrix do not",
              " match each other. Deactivating speed calculations to",
              " avoid function failure.")
        if (any(row_link)) {
          event(type = "screen",
                " Row names missing in the columns: '",
                paste(rownames(dist.mat)[row_link], collapse = "', '"), "'.")
        }
        if (any(col_link)) {
          event(type = "screen",
                " Column names missing in the rows: '",
                paste(colnames(dist.mat)[col_link], collapse = "', '"), "'.")
        }
        invalid.dist <- TRUE
      }
    }

    # Failsafe for the case of unspecified release site
    if (any(grepl("^unspecified$", spatial$release.sites$Standard.name))) {
      dist.mat <- rbind(dist.mat, NA)
      dist.mat <- cbind(dist.mat, NA)
      colnames(dist.mat)[ncol(dist.mat)] <- "unspecified"
      rownames(dist.mat)[nrow(dist.mat)] <- "unspecified"
      event(type = c("warning", "screen", "report"),
            "A distances matrix is being used but release site is unspecified.",
            " Adding a row and column of NAs to the distances matrix to avoid",
            " function failure.")
    }
    if (!invalid.dist && sum(nrow(spatial$stations),
                             nrow(spatial$release.sites)) != nrow(dist.mat)) {
      event(type = c("warning", "screen", "report"),
            "The number of spatial points does not match the number",
            " of rows in the distances matrix. Deactivating speed",
            " calculations to avoid function failure.")
      event(type = "screen",
            " Number of stations and release sites listed: ",
            sum(nrow(spatial$stations), nrow(spatial$release.sites)), "\n",
            " Number of rows/columns in the distance matrix: ",
            nrow(dist.mat))
      invalid.dist <- TRUE
    }
    if (!invalid.dist) {
      spatial_names <- c(spatial$stations$Standard.name,
                         spatial$release.sites$Standard.name)
      if (any(!matchl(spatial_names, colnames(dist.mat)))) {
        event(type = c("warning", "screen", "report"),
              "Some stations and/or release sites are not",
              " present in the distances matrix. Deactivating speed",
              " calculations to avoid function failure.")
        link <- !matchl(spatial$release.sites$Standard.name, colnames(dist.mat))
        missing.releases <- spatial$release.sites$Standard.name[link]
        link <- !matchl(spatial$stations$Standard.name, colnames(dist.mat))
        missing.stations <- spatial$stations$Standard.name[link]
        if (length(missing.releases) > 0) {
          event(type = "screen",
                " Release sites missing: '",
                paste(missing.releases, collapse = "', '"))
        }
        if (length(missing.stations) > 0) {
          event(type = "screen",
                " Stations missing: '",
                paste(missing.stations, collapse = "', '"))
        }
        invalid.dist <- TRUE
      }
    }
    if (!invalid.dist) {
      if (any(is.na(dist.mat))) {
        event(type = c("warning", "screen", "report"),
              "NAs detected in the distances matrix!",
              " Distance and speed calculations might fail!")
      }
    }
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
  event(type = "debug","Running loadDeployments.")
  weird_chars <-"\\\\|/|\\||:|\\*|\\?|\\\"|<|>|\\\'"
  timestamp_formats <- c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%dT%H:%M:%OS",
                         "%Y-%m-%d %H:%M", "%Y-%m-%dT%H:%M",
                         "%Y-%m-%d")
  yyyymmddthhmm <- paste0("^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9]",
                          "[ |T|t][0-2][0-9]:[0-5][0-9]")

  # compatibility with preload()
  if (is.character(input))
    preloaded <- FALSE
  else
    preloaded <- TRUE

  if (preloaded) {
    input <- as.data.frame(input)
    to.convert <- which(sapply(input, class) == "factor")
    if (length(to.convert) > 0) {
      for(i in to.convert) {
        input[, i] <- as.character(input[, i])
      }
    }
  } else {
    if (file.exists(input)) {
      input <- suppressWarnings(
                 data.table::fread(input, 
                                   colClasses = c("Start" = "character",
                                                  "Stop" = "character")))
      input <- as.data.frame(input, stringsAsFactors = FALSE)
    } else {
      event(type = "stop",
            "Could not find a '", input, "' file in the working directory.")
    }
  }

  if (!is.na(link <- match("Station.Name", colnames(input))))
    colnames(input)[link] <- "Station.name"

  if (any(link <- duplicated(colnames(input)))) {
    event(type = "stop",
          "The following columns are duplicated in the deployments: '",
          paste(unique(colnames(input)[link]), sep = "', '"), "'.")
  }

  default.cols <- c("Receiver", "Station.name", "Start", "Stop")
  link <- match(default.cols, colnames(input))
  if (any(is.na(link))) {
    event(type = "stop",
          "Column", ifelse(sum(is.na(link)) > 1, "(s) '", " '"),
          paste(default.cols[is.na(link)], collapse = "', '"),
          ifelse(sum(is.na(link)) > 1, "' are", "' is"),
          " missing in the deployments.")
  }

  # replace any weird characters in station names
  if (any(grepl(weird_chars, input$Station.name))) {
    input$Station.name <- gsub(weird_chars, "_", input$Station.name)
  }

  start_is_posix <- inherits(input$Start, "POSIXct")
  stop_is_posix <- inherits(input$Stop, "POSIXct")
  if (preloaded & start_is_posix & stop_is_posix) {
    event(type = c("screen", "report"),
          "M: Preloaded deployment times are already in POSIX format.",
          " Skipping timestamp format checks.")

    start_tz <- attributes(input$Start)$tz
    stop_tz <- attributes(input$Stop)$tz
    if (start_tz != stop_tz) {
      event(type = "stop",
            "Deployment Start and Stop times are not in the same time zone",
            " (", start_tz, " != ", stop_tz, ")!",
            " Please double-check the deployments.")
    }

    if (start_tz != tz) {
      event(type = c("warning", "screen", "report"),
            "Potential mismatch between deployments time zone",
            " (", start_tz, ") and 'tz' argument (", tz, ")!",
            " This could cause unwanted timelapses!")
    }
  } else {
    if (!start_is_posix) {
      if (any(!grepl(yyyymmddthhmm, input$Start))) {
        event(type = "stop",
              "Not all values in the 'Start' column appear to be in a",
              " 'yyyy-mm-dd hh:mm' format (seconds are optional).",
              " Please double-check the deployments.")
      }
      try_result <- try(
        input$Start <- as.POSIXct(input$Start, tz = tz, 
                                  tryFormats = timestamp_formats),
        silent = TRUE)
      if (inherits(try_result, "try-error")) {
        event(type = "stop",
              "Could not recognise the data in the 'Start' column as",
              " POSIX-compatible timestamps.",
              " Please double-check the deployments.")
      }
    }

    if (!stop_is_posix) {
      if (any(!grepl(yyyymmddthhmm, input$Stop))) {
        event(type = "stop",
              "Not all values in the 'Stop' column appear to be in a",
              " 'yyyy-mm-dd hh:mm' format (seconds are optional).",
              " Please double-check the deployments.")
      }
      try_result <- try(
        input$Stop <- as.POSIXct(input$Stop, tz = tz, 
                                 tryFormats = timestamp_formats),
        silent = TRUE)
      if (inherits(try_result, "try-error")) {
        event(type = "stop",
              "Could not recognise the data in the 'Stop' column as",
              " POSIX-compatible timestamps.",
              " Please double-check the deployments.")
      }
    }
  }

  check <- input$Start > input$Stop
  if (any(check)) {
    event(type = "stop",
          "Some deployment periods end before they have started!",
          " Please fix this before continuing.\n",
          "       Troublesome rows: ", paste(which(check), collapse = ", "))
  }

  input$Receiver <- as.character(input$Receiver)
  input$Receiver <- sapply(input$Receiver, 
                           function(x) tail(unlist(strsplit(x, "-")), 1))
  input <- input[order(input$Start), ]
  return(input)
}

#' Load Spatial File
#'
#' Loads a spatial file prepared for actel and appends the Standard.name column.
#' Additionally, performs a series of quality checks on the contents of the
#' target file.
#'
#' @param input Either a data frame or the name of an input file with spatial
#'        data in the actel format.
#' @param section.order A vector containing the order by which sections should
#'        be aligned in the results.
#' 
#' @examples
#' # This function requires the presence of a file with spatial information
#'
#' # Fetch location of actel's example files
#' aux <- system.file(package = "actel")[1]
#'
#' # run loadSpatial on the temporary spatial.csv file
#' loadSpatial(input = paste0(aux, '/example_spatial.csv'))
#'
#' @return A data frame with the spatial information present in 'spatial.csv'
#'         and the Standard.name column.
#'
#' @export
#'
loadSpatial <- function(input = "spatial.csv", section.order = NULL){
  event(type = "debug", "Running loadSpatial.")
  # note: the character | is allowed here.
  weird_chars <-"\\\\|/|:|\\*|\\?|\\\"|<|>|\\\'"

  if (is.character(input)) {
    if (file.exists(input))
      input <- as.data.frame(data.table::fread(input), stringsAsFactors = FALSE)
    else {
      event(type = "stop",
            "Could not find a '", input, "' file in the working directory.")
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
  dups <- duplicated(colnames(input))
  if (any(dups)) {
    event(type = "stop",
          "The following columns are duplicated in the spatial input: '",
          paste(unique(colnames(input)[dups]), sep = "', '"), "'.")
  }
  # Check wrong capitals in Station.name
  link <- match("Station.Name", colnames(input))
  if (!is.na(link)) {
    colnames(input)[link] <- "Station.name"
  }
  # Check missing Station.name
  if (!any(grepl("^Station\\.name$", colnames(input)))) {
    event(type = "stop",
          "The spatial input must contain a 'Station.name' column.")
  } else {
    # Check all station names are unique
    link <- table(input$Station.name) > 1
    if (any(link)) {
      event(type = "stop",
          "The 'Station.name' column in the spatial input must not",
          " have duplicated values.\n",
          "Stations appearing more than once: ",
          paste(names(table(input$Station.name))[link], collapse = ", "))
    }
    # Check that stations do not contain weird characters
    if (any(grepl(weird_chars, input$Station.name))) {
      event(type = c("warning", "screen", "report"),
            "Troublesome characters found in the station names",
            " (\\/|:*?\"<>\').",
            " Replacing these with '_' to prevent function failure.")
      input$Station.name <- gsub(weird_chars, "_", input$Station.name)
    }
  }
  # Check missing Array column
  if (!any(grepl("^Array$", colnames(input)))) {
    event(type = "stop",
          "The spatial input must contain an 'Array' column.")
  }
  # check missing data in the arrays
  if (any(is.na(input$Array)) | any(input$Array == "")) {
    event(type = "stop",
          "Some rows do not contain 'Array' information in the spatial input.",
          " Please double-check the input files.")
  }
  # check spaces in the array names
  if (any(grepl(" ", input$Array))) {
    event(type = "screen",
          "M: Replacing spaces with '_' in array names",
          " to prevent function failure.")
    input$Array <- gsub(" ", "_", input$Array)
  }
  if (any(grepl(weird_chars, input$Array))) {
    event(type = c("warning", "screen", "report"),
          "Troublesome characters found in the array names (\\/:*?\"<>\').",
          " Replacing these with '_' to prevent function failure.")
    input$Array <- gsub(weird_chars, "_", input$Array)
  }

  # check reserved array names
  if (any(grepl("^Release$", input$Array))) {
    event(type = "stop",
         "The term 'Release' is reserved for internal calculations.",
         " Do not name any sections or arrays as 'Release'.")
  }
  if (any(grepl("^Total$", input$Array))) {
    event(type = "stop",
          "The term 'Total' is reserved for internal calculations.",
          " Do not name any sections or arrays as 'Total'.")
  }
  if (any(grepl("^Invalid$", input$Array))) {
    event(type = "stop",
          "The term 'Invalid' is reserved for internal calculations.",
          " Do not name any sections or arrays as 'Invalid'.")
  }
  if (any(grepl("^Unknown$", input$Array))) {
    event(type = "stop",
          "The term 'Unknown' is reserved for internal calculations.",
          " Do not name any sections or arrays as 'Unknown'.")
  }
  # check array name length
  aux <- unlist(strsplit(input$Array, "|", fixed = TRUE))
  aux <- nchar(as.character(aux))
  if (any(aux > 6)) {
    event(type = c("warning", "screen", "report"),
          "Long array names detected. To improve graphic rendering,",
          " consider keeping array names under six characters.")
  }
  # check missing Type column
  if (!any(grepl("^Type$", colnames(input)))) {
    event(type = c("screen", "report"),
          "M: No 'Type' column found in the spatial input.",
          " Assigning all rows as hydrophones.")
    input$Type <- "Hydrophone"
  } else {
    # check strange data in the Type column
    if (any(is.na(match(unique(input$Type), c("Hydrophone", "Release"))))) {
      event(type = "stop",
            "Could not recognise the data in the 'Type' column as only one",
            " of 'Hydrophone' or 'Release'.",
            " Please double-check the spatial input.")
    }
  }
  # Check Section column
  if (!any(grepl("^Section$", colnames(input)))) {
    event(type = c("warning", "screen", "report"),
          "spatial does not contain a 'Section' column. Assigning all",
          " arrays to section 'All'")
    input$Section <- "All"
  }

  # check missing data in the arrays
  no_section <- any(is.na(input$Section[input$Type == "Hydrophone"]))
  empty_section <- any(input$Section[input$Type == "Hydrophone"] == "")
  if (no_section | empty_section) {
    event(type = "stop",
          "Some rows do not contain 'Section' information in the spatial",
          " input. Please double-check the input files.")
  }
  # check spaces in the array names
  if (any(grepl(" ", input$Section))) {
    event(type = "screen",
          "M: Replacing spaces with '_' in section names to",
          " prevent function failure.")
    input$Section <- gsub(" ", "_", input$Section)
  }
  if (any(grepl(weird_chars, input$Section))) {
    event(type = c("warning", "screen", "report"),
          "Troublesome characters found in the section names (\\/|:*?\"<>|).",
          " Replacing these with '_' to prevent function failure.")
    input$Section <- gsub(weird_chars, "_", input$Section)
  }
  sections <- unique(input$Section[input$Type == "Hydrophone"])
  # check reserved section names
  if (any(grepl("^Release$", sections))) {
    event(type = "stop",
          "The term 'Release' is reserved for internal calculations.",
          " Do not name any sections or arrays as 'Release'.")
  }
  if (any(grepl("^Total$", sections))) {
    event(type = "stop",
          "The term 'Total' is reserved for internal calculations.",
          " Do not name any sections or arrays as 'Total'.")
  }
  if (any(grepl("^Unknown$", sections))) {
    event(type = "stop",
          "The term 'Unknown' is reserved for internal calculations.",
          " Do not name any sections or arrays as 'Unknown'.")
  }
  # check that section names are independent
  link <- sapply(sections, function(i) length(grep(i, sections))) > 1
  if (any(link)) {
    event(type = "stop",
          ifelse(sum(link) == 1, "Section '", "Sections '"),
          paste(sections[link], collapse = "', '"),
          ifelse(sum(link) == 1, "' is", "' are"),
          " contained within other section names.",
          " Sections must be unique and independent.\n",
          "       Please rename your sections so that section",
          " names are not contained within each other.")
  }

  if (is.null(section.order)) {
    input$Section <- factor(input$Section, levels = sections)
  } else {
    section.order <- gsub(" ", "_", section.order)

    if (any(link <- is.na(match(sections, section.order)))) {
      event(type = "stop",
            "Not all sections are listed in 'section.order'.",
            " Sections missing: ", paste(sections[link], collapse = ", "))
    }

    if (any(link <- is.na(match(section.order, sections)))) {
      event(type = c("warning", "screen", "report"),
            "Not all values listed in 'section.order' correspond",
            " to sections. Discarding the following values: ",
            paste(section.order[link], collapse = ", "))
      section.order <- section.order[!link]
    }
    input$Section <- factor(input$Section, levels = section.order)
  }
  if (any(nchar(as.character(sections)) > 6)) {
    event(type = c("warning", "screen", "report"),
          "Long section names detected. To improve graphic rendering,",
          " consider keeping section names under six characters.")
  }

  # find if arrays are assigned to more than one section
  aux <- input[input$Type == "Hydrophone", ]
  aux <- with(aux, as.data.frame.matrix(table(Array, Section)))
  sections.per.array <- apply(aux, 1, function(i) sum(i > 0))
  if (any(sections.per.array > 1)) {
    event(type = "stop",
          ifelse(sum(sections.per.array > 1) == 1, "Array '", "Arrays '"),
          paste0(names(sections.per.array)[sections.per.array > 1],
                 collapse = "', '"),
          ifelse(sum(sections.per.array > 1) == 1,
                 "' has been", "' have been"),
          " assigned to more than one section! Each array can only belong",
          " to one section. Please correct the spatial input before",
          " continuing.")
  }

  # check release arrays exist
  receivers <- unique(input$Array[input$Type == "Hydrophone"])
  releases <- unique(unlist(sapply(input$Array[input$Type == "Release"],
                                   function(x) unlist(strsplit(x, "|",
                                                               fixed = TRUE)))))
  link <- is.na(match(releases, receivers))
  if (any(link)) {
    event(type = "stop",
          "Not all the expected first arrays of the release sites exist.\n",
          "Unknown expected first arrays: '",
          paste0(releases[link], collapse = "', '"), "'.\n",
          "In the spatial input, the expected first arrays of the release",
          " sites should match the arrays where hydrophone stations where",
          " deployed.")
  }
  # Create Standard.name for each station
  input <- setSpatialStandards(input = input)
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
  event(type = "debug", "Running loadBio.")
  timestamp_formats <- c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%dT%H:%M:%OS",
                         "%Y-%m-%d %H:%M", "%Y-%m-%dT%H:%M",
                         "%Y-%m-%d")
  yyyymmddthhmm <- paste0("^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9]",
                          "[ |T|t][0-2][0-9]:[0-5][0-9]")

  # these never happen during an analysis, no need to use event()
  if (missing(input)) {
    stop("'input' is missing.")
  }
  if (missing(tz)) {
    stop("'tz' is missing.")
  }

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
    if (file.exists(input)) {
      bio <- suppressWarnings(
               data.table::fread(input,
                                 colClasses = c("Release.date" = "character")))
      bio <- as.data.frame(bio, stringsAsFactors = FALSE)
    } else {
      event(type = "stop",
            "Could not find a '", input, "' file in the working directory.")
    }
  }

  dups <- duplicated(colnames(bio))
  if (any(dups)) {
    event(type = "stop",
          "The following columns are duplicated in the biometrics: '",
          paste(unique(colnames(bio)[dups]), sep = "', '"), "'.")
  }

  if (!any(grepl("^Release\\.date$", colnames(bio)))) {
    event(type = "stop",
          "The biometrics must contain an 'Release.date' column.")
  }

  if (preloaded & inherits(bio$Release.date, "POSIXct")) {
    event(type = c("screen", "report"),
          "M: Preloaded Release dates are already in POSIX format.",
          " Skipping timestamp format checks.")
    if (attributes(bio$Release.date)$tz != tz) {
      event(type = c("warning", "screen", "report"),
            "Potential mismatch between release dates time zone (",
            attributes(bio$Release.date)$tz, ") and 'tz' argument (", tz,
            ")! This could cause unwanted timelapses!")
    }
  } else {
    if (any(!grepl(yyyymmddthhmm, bio$Release.date))) {
      event(type = "stop",
            "Not all values in the 'Release.date' column appear to be in a",
            " 'yyyy-mm-dd hh:mm' format (seconds are optional).",
            " Please double-check the biometrics.")
    }

    try_result <- try(
      bio$Release.date <- as.POSIXct(bio$Release.date, tz = tz, 
                                     tryFormats = timestamp_formats),
      silent = TRUE)
    if (inherits(try_result, "try-error")) {
      event(type = "stop",
            "Could not recognise the data in the 'Release.date' column as",
            " POSIX-compatible timestamps. Please double-check the biometrics.")
    }
  }

  if (!any(grepl("^Signal$", colnames(bio)))) {
    event(type = "stop",
          "The biometrics must contain an 'Signal' column.")
  }

  if (any(is.na(bio$Signal))) {
    event(type = "stop",
          "Some animals have no 'Signal' information.",
          " Please double-check the biometrics.")
  }

  if (!any(grepl("^Code\\.space$", colnames(bio)))) {
    event(type = c("screen", "report"),
          "M: No Code.space column was found in the biometrics.",
          " Assigning code spaces based on detections.")
  } else {
    if (any(is.na(bio$Code.space)) | any(bio$Code.space == "")) {
      event(type = "stop",
            'Not all tags have an associated code space.",
            " Please specify the code space of every tag.')
    }
  }

  # activate multi-sensor versatility.
  if (any(grepl("|", bio$Signal, fixed = TRUE))) {
    event(type = c("screen", "report"),
          "M: Multi-sensor tags detected. These tags will be referred",
          " to by their lowest signal value.")
    expect_integer <- FALSE
  } else {
    expect_integer <- TRUE
  }

  # examine signal quality
  if (expect_integer & !inherits(bio$Signal, "integer")) {
    event(type = "stop",
          "Could not recognise the data in the 'Signal' column as integers.",
          " Please double-check the biometrics.")
  } else {
    aux <- unlist(strsplit(as.character(bio$Signal), "|", fixed = TRUE))
    signal_check <- suppressWarnings(as.numeric(aux))
    if (any(is.na(signal_check))) {
      event(type = "stop",
            "Could not recognise the data in the 'Signal' column as integers.",
            " Please double-check the biometrics.")
    }
  }

  # check that tags are not duplicated
  if (expect_integer) {
    if (any(colnames(bio) == "Code.space")) {
      aux <- paste(bio$Code.space, "-", bio$Signal)
      check <- table(aux) > 1
      prefix <- "Tag"
    } else {
      check <- table(bio$Signal) > 1
      prefix <- "Signal"
    }
  }
  else {
    if (any(colnames(bio) == "Code.space")) {
      aux <- apply(bio, 1,
                   function(x) paste0(x['Code.space'], 
                                      '-', splitSignals(x['Signal'])))
      check <- table(unlist(aux)) > 1
      prefix <- "Tag"
    }
    else {
      aux <- unlist(sapply(bio$Signal, splitSignals))
      check <- table(aux) > 1
      prefix <- "Signal"
    }
  }
  if (any(check)) {
    event(type = "stop",
          prefix, ifelse(sum(check) > 1, "s ", " "),
          paste(names(check)[check], collapse = ", "),
          ifelse(sum(check) > 1," are ", " is "),
          "duplicated in the biometrics.")
  }

  # check sensor names
  if (!expect_integer) {
    if (!any(grepl("^Sensor\\.unit$", colnames(bio)))) {
      event(type = c("warning", "screen"),
            "Tags with multiple sensors are listed in the biometrics,",
            " but a 'Sensor.unit' column could not be found.",
            " Skipping sensor unit assignment.")
    }
    else {
      # failsafe in case all values are numeric, or NA.
      bio$Sensor.unit <- as.character(bio$Sensor.unit)
      bio$Sensor.unit[bio$Sensor.unit == ''] <- NA_character_

      link <- na.as.false(startsWith(bio$Sensor.unit, '|'))
      if (any(link)) {
        event(type = c("warning", "screen"),
              "The Sensor.unit information in ",
              ifelse(sum(link) <= 10,
                     paste0("row(s) ", paste0(which(link), collapse = ", ")),
                     paste0(sum(link), " row(s)")),
              " of the biometrics starts with a '|' character.",
              " Could you have forgotten to include a sensor unit?")
      }
      link <- na.as.false(endsWith(bio$Sensor.unit, '|'))
      if (any(link)) {
        event(type = c("warning", "screen"),
              "The Sensor.unit information in ",
              ifelse(sum(link) <= 10,
                     paste0("row(s) ", paste0(which(link), collapse = ", ")),
                     paste0(sum(link), " row(s)")),
              " of the biometrics ends with a '|' character.",
              " Could you have forgotten to include a sensor unit?")
      }

      signals_per_tag <- sapply(strsplit(bio$Signal, "|", fixed = TRUE), length)
      aux <- strsplit(bio$Sensor.unit, "|", fixed = TRUE)
      sensors_per_tag <- sapply(aux, length)

      link <- signals_per_tag != sensors_per_tag
      if (any(link)) {
        event(type = "stop",
              "The number of provided sensor units does not",
              " match the number of signals for ",
              ifelse(sum(link) <= 10,
                     paste0("row(s) ", paste0(which(link), collapse = ", ")),
                     paste0(sum(link), " row(s)")),
              " of the biometrics.")
      }
    }
  }

  # Release site quality checking/creation
  if (!any(grepl("^Release\\.site$", colnames(bio)))) {
    event(type = "screen",
          "M: No Release site has been indicated in the biometrics.",
          " Creating a 'Release.site' column to avoid function failure.",
          " Filling with 'unspecified'.")
    bio$Release.site <- "unspecified"
  } else {
    bio$Release.site <- gsub(" ", "_", bio$Release.site)

    # replace any weird characters in station names
    # note: | is allowed here
    weird_chars <-"\\\\|/|:|\\*|\\?|\\\"|<|>|\\\'"
    if (any(grepl(weird_chars, bio$Release.site))) {
      bio$Release.site <- gsub(weird_chars, "_", bio$Release.site)
    }

    bio$Release.site <- factor(bio$Release.site)
    link <- is.na(bio$Release.site) | bio$Release.site == ""
    if (any(link)) {
      event(type = c("warning", "screen", "report"),
            "Some animals contain no release site information.",
            " You may want to double-check the data.\n",
            "   Filling the blanks with 'unspecified'.")
      levels(bio$Release.site) <- c(levels(bio$Release.site), "unspecified")
      bio$Release.site[link] <- "unspecified"
      bio$Release.site <- droplevels(bio$Release.site)
    }
  }

  # Group quality checking/creation
  if (!any(grepl("^Group$", colnames(bio)))) {
    event(type = c("screen", "report"),
          "M: No 'Group' column found in the biometrics.",
          " Assigning all animals to group 'All'.")
    bio$Group <- "All"
    bio$Group <- as.factor(bio$Group)
  } else {
    bio$Group <- factor(bio$Group)
    if (any(link <- is.na(bio$Group) | bio$Group == "")) {
      event(type = c("warning", "screen", "report"),
            "Some animals contain no group information.",
            " You may want to double-check the data.\n",
            "   Filling the blanks with 'unspecified'.")
      levels(bio$Group) <- c(levels(bio$Group), "unspecified")
      bio$Group[link] <- "unspecified"
      bio$Group <- droplevels(bio$Group)
    }
    link <- sapply(levels(bio$Group),
                   function(i) length(grep(i, levels(bio$Group))))
    link <- link > 1
    if (any(link)) {
      event(type = c("warning", "screen", "report"),
            ifelse(sum(link) == 1, "Group '", "Groups '"),
            paste(levels(bio$Group)[link], collapse = "', '"),
            ifelse(sum(link) == 1, "' is", "' are"),
            " contained within other groups.",
            " To avoid function failure, a number will be appended to ",
            ifelse(sum(link) == 1, "this group.", "these groups."))
      levels(bio$Group)[link] <- paste(levels(bio$Group)[link],
                                       1:sum(link), sep = "_")
    }
    if (any(grepl("\\.", levels(bio$Group)))) {
      event(type = c("screen", "report"),
            "M: Some groups contain one or more '.' characters.",
            " To avoid function failure, these will be replaced with '_'.")
      levels(bio$Group) <- gsub("\\.", "_", levels(bio$Group))
    }
  }
  if (any(nchar(as.character(bio$Group)) > 6)) {
    event(type = c("warning", "screen", "report"),
          "Long group names detected. To improve graphic rendering,",
          " consider keeping group names under six characters.")
  }

  # order table by signal before handing it over
  bio <- bio[order(bio$Signal),]
  return(bio)
}

#' Load ALS detections
#'
#' If there are previously compiled detections present, offers the chance
#' to reuse. Otherwise triggers combineDetections.
#'
#' @inheritParams explore
#'
#' @return A data frame with all the detections
#'
#' @keywords internal
#'
loadDetections <- function(start.time = NULL, stop.time = NULL, tz,
                           force = FALSE, save.detections = FALSE,
                           record.source = FALSE) {
  event(type = "debug", "Running loadDetections.")
  # NOTE: The variable actel.detections is loaded from an RData file.
  # To avoid package check notes, the variable name is created before any use.
  actel.detections <- NULL
  recompile <- TRUE
  detection.paths <- c(file.exists("actel.detections.RData"),
                       file.exists("detections/actel.detections.RData"))

  if (any(detection.paths)) {
    if (all(detection.paths)) {
      event(type = c("warning", "screen", "report"),
            "Previously compiled detections were found both in the current",
            " directory and in a 'detections' folder.\n",
            "   Loading ONLY the compiled detections present",
            " in the 'detections' folder.")
    }
    if(detection.paths[2]) {
      load("detections/actel.detections.RData")
    } else {
      load("actel.detections.RData")
    }
    if (force) { # nocov start
      decision <- "Y"
    } else {
      event(type = "screen",
            "M: The detections have been processed on ",
            actel.detections$timestamp, ".\n",
            "   If the input detection files were not changed,",
            " it is safe to use these again.")
      decision <- userInput("   Reuse processed detections?(y/n) ",
                            choices = c("y", "n"),
                            hash = "# reuse detections?")
    }
    if (decision == "y"){
      event(type = c("screen", "report"),
            "M: Using detections previously compiled on ",
            actel.detections$timestamp, "...")
      detections <- actel.detections$detections
      attributes(detections$Timestamp)$tzone <- "UTC"
      detections <- convertTimes(input = detections, 
                                 start.time = start.time, 
                                 stop.time = stop.time, 
                                 tz = tz)
      recompile <- FALSE
    } else {
      event(type = "Screen",
            "M: Reprocessing the detections.")
    } # nocov end
    rm(actel.detections)
  }

  if (recompile) {
    detections <- compileDetections(path = "detections",
                                    start.time = start.time,
                                    stop.time = stop.time, 
                                    tz = tz, 
                                    save.detections = save.detections, 
                                    record.source = record.source)
  }

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
compileDetections <- function(path = "detections", start.time = NULL,
    stop.time = NULL, tz, save.detections = FALSE, record.source = FALSE) {
  event(type = "debug", "Running compileDetections")
  event(type = "Screen",
        "M: Compiling detections...")
  # Find the detection files
  if (file_test("-d", path)) {
    file.list <- list.files(path = path,
      pattern = "*(\\.[cC][sS][vV]|\\.[vV][rR][lL]|\\.[vV][dD][aA][tT])",
      full.names = TRUE)
    if (length(file.list) == 0) {
      event(type = "stop",
            "A 'detections' folder is present but appears to be empty.")
    }
  } else {
    if (file.exists("detections.csv")) {
      file.list <- "detections.csv"
    } else {
      event(type = "stop",
            "Could not find a 'detections' folder ",
            "nor a 'detections.csv' file.")
    }
  }
  if (file_test("-d", path) & file.exists("detections.csv")) {
    event(type = c("warning", "screen", "report"),
          "Both a 'detections' folder and a 'detections.csv' file are ",
          "present in the current directory.\n",
          "   Loading ONLY the files present in the 'detections' folder.")
  }

  # known header formats
  header_formats <- list(
    std = c("Timestamp", "CodeSpace", "Receiver", "Signal"),
    thelma_old = c("CodeType", "TBR Serial Number", "Id"),
    thelma_new = c("Protocol", "Receiver", "ID"),
    vemco = c("Transmitter", "Receiver", "Date.and.Time"),
    innovasea = c("Device Time (UTC)", "Full ID", "Serial Number"),
    vdat = c("VEMCO DATA LOG")
  )

  # Prepare the detection files
  data.files <- lapply(file.list, function(i) {
    event(type = "debug", paste0("Importing file '", i, "'."))

    file_extension <- tools::file_ext(i)

    if (grepl("[vV][rR][lL]", file_extension)) {
      event(type = c("warning", "screen", "report"),
            "File '", i, "' is in VRL (Vemco Receiver Log) format, which ",
            "actel can't currently process. To include this file in your ",
            "analyses, you must convert it to CSV format. That can be done ",
            "using Innovasea's software, or the vrl2csv() function of the ",
            "glatos package.")
      return(NULL)
    }
    if (grepl("[vV][dD][aA][tT]", file_extension)) {
      event(type = c("warning", "screen", "report"),
            "File '", i, "' is in VDAT (Vemco Data) format, which ",
            "actel can't currently process. To include this file in your ",
            "analyses, you must convert it to CSV format. That can be done ",
            "using Innovasea's Fathom software, the write_vdat_csv() function ",
            "of the glatos package, or the vdat_to_csv() function of the ",
            "rvdat package.")
      return(NULL)
    }

    file_header <- readLines(i, 1)

    # find the header format that matches the current file
    file_match <- sapply(header_formats,
      function(fingerprint) {
        all(sapply(fingerprint,
          function(i) {grepl(i, file_header)}))
      })

    # if no matches were found, then warn the
    # user that the file won't be processed.
    if (all(!file_match)) {
      event(type = c("warning", "screen", "report"),
            "File '", i, "' does not match to any of the supported ",
            "hydrophone file formats!\n         If your file corresponds ",
            "to a hydrophone log and actel did not recognize it, please get ",
            "in contact through www.github.com/hugomflavio/actel/issues/new")
      return(NULL) # stops this lapply iteration - nothing to do here.
    }

    # if more than one match is found, let the user know,
    # and pick the earliest match in the list
    if (sum(file_match) > 1) {
      event(type = c("screen", "report"),
            "File '", i, "' matches more than one file format (matches: ",
            paste0(names(file_match)[file_match], collapse = ", "),
            "). Picking the first matching type (",
            names(file_match)[file_match][1],
            "). If this causes issues, get in contact through ",
            "www.github.com/hugomflavio/actel/issues/new")
    }

    # this code works both for the cases where there is only one match
    # and where we want to pick the first of many.
    file_type <- names(file_match)[file_match][1]

    # preliminary load and check of CSV logs
    if (file_type %in%
        c("std", "thelma_new", "thelma_old", "vemco", "innovasea", "vdat")) {
      if (file_type == "vdat") {
        aux <- {
          search_fun <- ifelse(.Platform$OS.type == "windows", "FINDSTR", "grep")
          
          data.table::fread(
            cmd = paste(
              search_fun, " DET", shQuote(i)
            ),
            header = F,
            col.names = {
              as.character(
                data.table::fread(cmd = paste(
                  search_fun, " DET_DESC", shQuote(i)
                ),
                header = F)
              )
            },
            showProgress = FALSE
          )
        }
      } else {
        aux <- data.table::fread(i, fill = TRUE, sep = ",", showProgress = FALSE)
      }
      if(nrow(aux) == 0){
        event(type = c("screen", "report"),
              "File '", i, "' is empty, skipping processing.")
        return(NULL) # File is empty, skip to next file
      }
      if (ncol(aux) < 3) {
        event(type = c("screen", "warning", "report"),
              "File '", i, "' could not be recognized as a valid detections",
              " table (ncol < 3), skipping processing. Are you sure it is a",
              " comma separated file?")
        return(NULL)
      }
    }
    
    if (file_type == "std") {
      event(type = "debug", "File '", i, "' matches a Standard log.")
      output <- tryCatch(
        processStandardFile(input = aux),
        error = function(e) {
          event(type = "stop",
                "Something went wrong when processing file '", i,
                "'. If you are absolutely sure this file is ok, contact the ",
                "developer.\nOriginal error:", sub("^Error:", "", e))
        })
      unknown.file <- FALSE
    }
    if (file_type == "thelma_old") {
      event(type = "debug", "File '", i, "' matches a Thelma log.")
      output <- tryCatch(
        processThelmaOldFile(input = aux), error = function(e) {
          event(type = "stop",
                "Something went wrong when processing file '", i,
                "'. If you are absolutely sure this file is ok, contact the ",
                "developer.\nOriginal error:", sub("^Error:", "", e))
        })
      unknown.file <- FALSE
    }
    if (file_type == "thelma_new") {
      event(type = "debug",
            "File '", i, "' matches a Thelma log (old format).")
      output <- tryCatch(
        processThelmaNewFile(input = aux), error = function(e) {
          event(type = "stop",
                "Something went wrong when processing file '", i,
                "'. If you are absolutely sure this file is ok, contact the ",
                "developer.\nOriginal error:", sub("^Error:", "", e))
        })
      unknown.file <- FALSE
    }
    if (file_type == "vemco") {
      event(type = "debug", "File '", i, "' matches a Vemco log.")
      output <- tryCatch(
        processVemcoFile(input = aux), error = function(e) {
          event(type = "stop",
                "Something went wrong when processing file '", i,
                "'. If you are absolutely sure this file is ok, contact the ",
                "developer.\nOriginal error:", sub("^Error:", "", e))
        })
      unknown.file <- FALSE
    }
    if (file_type == "innovasea") {
      event(type = "debug", "File '", i, "' matches an Innovasea log.")
      output <- tryCatch(
        processInnovaseaFile(input = aux, file_type = file_type),
        error = function(e) {
          event(type = "stop",
                "Something went wrong when processing file '", i,
                "'. If you are absolutely sure this file is ok, contact the ",
                "developer.\nOriginal error:", sub("^Error:", "", e))
        })
      unknown.file <- FALSE
    }
    if (file_type == "vdat") {
      event(type = "debug", "File '", i, "' matches a VDAT log.")
      output <- tryCatch(
        processInnovaseaFile(input = aux, file_type = file_type), 
        error = function(e) {
          event(type = "stop",
                "Something went wrong when processing file '", i,
                "'. If you are absolutely sure this file is ok, contact the ",
                "developer.\nOriginal error:", sub("^Error:", "", e))
        })
      unknown.file <- FALSE
    }

    if (record.source) {
      output$Source.file <- i
    }
    return(output)
  })

  names(data.files) <- file.list
  if (any(sapply(data.files, is.null))) {
    if (sum(sapply(data.files, is.null)) > 1) {
      event(type = "screen",
            "M: ", sum(sapply(data.files, is.null)),
            " files were excluded from further analyses.")
    } else {
      event(type = "Screen",
            "M: One file was excluded from further analyses.")
    }
  }

  if (all(sapply(data.files, is.null))) {
    event(type = "stop",
          "No valid detection files were found.")
  }

  # Bind the detection files
  event(type = "debug", "Binding detections.")
  output <- data.table::rbindlist(data.files)
  output$Receiver <- as.factor(output$Receiver)
  output$CodeSpace <- as.factor(output$CodeSpace)
  # Convert codespaces
  if (getOption("actel.auto.convert.codespaces", default = TRUE)) {
    output <- convertCodes(input = output)
  }

  # Compile transmitters
  output$Transmitter <- as.factor(paste(output$CodeSpace,
                                        output$Signal, sep = "-"))

  # save detections in UTC
  actel.detections <- list(detections = output, timestamp = Sys.time())
  if (save.detections) {
    save(actel.detections, 
         file = ifelse(file_test("-d", path),
                       paste0(path, "/actel.detections.RData"),
                       "actel.detections.RData"))
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
  event(type = "debug", "Running processStandardFile.")
  # NOTE: This function uses stop() instead of event(),
  # because these stop calls are caught by a trycatch.

  if (!is.numeric(input$Receiver)) {
   stop("The 'Receiver' column is not numeric.\n",
        "Please include only the receiver serial numbers in the ",
        "'Receiver' column.", call. = FALSE)
  }
  if (!is.numeric(input$Signal)) {
    stop("The 'Signal' column is not numeric.\n",
         "Please include only the tag signals in the 'Signal' column.",
         call. = FALSE)
  }

  input <- as.data.frame(input, stringsAsFactors = FALSE)

  time_vec <- fasttime::fastPOSIXct(
    sapply(as.character(input$Timestamp),
           function(x) gsub("Z", "", gsub("T", " ", x))),
    tz = "UTC")

  output <- data.table::data.table(
    Timestamp = time_vec,
    Receiver = input$Receiver,
    CodeSpace = input$CodeSpace,
    Signal = input$Signal)

  # include sensor data, if present
  if ("Sensor.Value" %in% colnames(input)) {
    output$Sensor.Value <- input$Sensor.Value
  } else {
    output$Sensor.Value <- NA_real_
  }

  if ("Sensor.Unit" %in% colnames(input)) {
    output$Sensor.Unit <- input$Sensor.Unit
  } else {
    output$Sensor.Unit <- NA_character_
  }

  # final checks
  # NOTE: This function uses stop() instead of event(),
  # because these stop calls are caught by a trycatch.
  if (any(is.na(output$Timestamp))) {
    stop("Importing timestamps failed", call. = FALSE)
  }
  if (any(is.na(output$Receiver))) {
    stop("Importing receivers failed", call. = FALSE)
  }
  if (any(is.na(output$CodeSpace))) {
    stop("Importing code space failed", call. = FALSE)
  }
  if (any(is.na(output$Signal))) {
    stop("Importing signals failed", call. = FALSE)
  }
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
  event(type = "debug", "Running processThelmaOldFile.")
  # NOTE: This function uses stop() instead of event(),
  # because these stop calls are caught by a trycatch.

  input <- as.data.frame(input, stringsAsFactors = FALSE)

  # leave the dots as wildcards as data table does not replace spaces with dots
  time_col <- grep("^Date.and.Time", colnames(input))
  time_vec <- fasttime::fastPOSIXct(sapply(as.character(input[, time_col]),
                                           function(x) {
                                             gsub("Z", "", gsub("T", " ", x))
                                           }),
                                    tz = "UTC")

  output <- data.table::data.table(Timestamp = time_vec,
                                   Receiver = input$`TBR Serial Number`,
                                   CodeSpace = input$CodeType,
                                   Signal = input$Id,
                                   Sensor.Value = input$Data,
                                   Sensor.Unit = rep(NA_character_,
                                                     nrow(input)))

  # Some thelma output files come with "-" rather than NA...
  output$Sensor.Value[output$Sensor.Value == "-"] <- NA
  output$Sensor.Value <- as.numeric(output$Sensor.Value)

  # NOTE: This function uses stop() instead of event(),
  # because these stop calls are caught by a trycatch.
  if (any(is.na(output$Timestamp))) {
    stop("Importing timestamps failed", call. = FALSE)
  }
  if (any(is.na(output$Receiver))) {
    stop("Importing receivers failed", call. = FALSE)
  }
  if (any(is.na(output$CodeSpace))) {
    stop("Importing code space failed", call. = FALSE)
  }
  if (any(is.na(output$Signal))) {
    stop("Importing signals failed", call. = FALSE)
  }
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
  event(type = "debug", "Running processThelmaNewFile.")
  # NOTE: This function uses stop() instead of event(),
  # because these stop calls are caught by a trycatch.

  input <- as.data.frame(input, stringsAsFactors = FALSE)

  # leave the dots as wildcards as data table does not replace spaces with dots
  time_col <- grep("^Date.and.Time", colnames(input))
  time_vec <- fasttime::fastPOSIXct(sapply(as.character(input[, time_col]),
                                           function(x) {
                                             gsub("Z", "", gsub("T", " ", x))
                                           }),
                                    tz = "UTC")

  codespace_vec <- sapply(input$Protocol,
                          function(x) {
                            unlist(strsplit(x, "-", fixed = TRUE))[1]
                          })

  output <- data.table::data.table(Timestamp = time_vec,
                                   Receiver = input$Receiver,
                                   CodeSpace = codespace_vec,
                                   Signal = input$ID,
                                   Sensor.Value = input$Data,
                                   Sensor.Unit = rep(NA_character_,
                                                     nrow(input)))

  # Some thelma output files come with "-" rather than NA...
  output$Sensor.Value[output$Sensor.Value == "-"] <- NA
  output$Sensor.Value <- as.numeric(output$Sensor.Value)

  # NOTE: This function uses stop() instead of event(),
  # because these stop calls are caught by a trycatch.
  if (any(is.na(output$Timestamp))) {
    stop("Importing timestamps failed", call. = FALSE)
  }
  if (any(is.na(output$Receiver))) {
    stop("Importing receivers failed", call. = FALSE)
  }
  if (any(is.na(output$CodeSpace))) {
    stop("Importing code space failed", call. = FALSE)
  }
  if (any(is.na(output$Signal))) {
    stop("Importing signals failed", call. = FALSE)
  }
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
  event(type = "debug", "Running processVemcoFile.")
  # NOTE: This function uses stop() instead of event(),
  # because these stop calls are caught by a trycatch.

  transmitter_aux <- strsplit(input$Transmitter, "-", fixed = TRUE)
  input$CodeSpace <- extractCodeSpaces(input$Transmitter)
  input$Signal <- extractSignals(input$Transmitter)
  input$Receiver <- extractSignals(input$Receiver)

  colnames(input)[grep("^Date.and.Time", colnames(input))] <- c("Timestamp")
  colnames(input) <- gsub(" ", ".", colnames(input))

  if (!any(grepl("^Sensor\\.Value$", colnames(input)))) {
    input$Sensor.Value <- rep(NA_real_, nrow(input))
    input$Sensor.Unit <- rep(NA_character_, nrow(input))
  }

  std_cols <- c("Timestamp", "Receiver", "CodeSpace",
                "Signal", "Sensor.Value", "Sensor.Unit")
  input <- input[, std_cols, with = FALSE]
  input$Timestamp <- fasttime::fastPOSIXct(as.character(input$Timestamp),
                                           tz = "UTC")

  # NOTE: This function uses stop() instead of event(),
  # because these stop calls are caught by a trycatch.
  if (any(is.na(input$Timestamp))) {
    stop("Importing timestamps failed", call. = FALSE)
  }
  if (any(is.na(input$Receiver))) {
    stop("Importing receivers failed", call. = FALSE)
  }
  if (any(is.na(input$CodeSpace))) {
    stop("Importing code space failed", call. = FALSE)
  }
  if (any(is.na(input$Signal))) {
    stop("Importing signals failed", call. = FALSE)
  }
  return(input)
}

#' Innovasea files
#'
#' Processes Innovasea ALS files
#'
#' @param input the detections data frame.
#' @param file_type the detected file type.
#'
#' @return A data frame of standardized detections from the input file.
#'
#' @keywords internal
#'
processInnovaseaFile <- function(input, file_type) {
  event(type = "debug", "Running processInnovaseaFile.")
  # NOTE: This function uses stop() instead of event(),
  # because these stop calls are caught by a trycatch.

  colnames(input) <- gsub(" ", ".", colnames(input))

  transmitter_aux <- strsplit(input$Full.ID, "-", fixed = TRUE)
  input$CodeSpace <- extractCodeSpaces(input$Full.ID)
  input$Signal <- ifelse(file_type == "vdat", input$Full.ID,
                         extractSignals(input$Full.ID))

  data.table::setnames(input,
                       c("Serial.Number", "Device.Time.(UTC)", "Raw.Data"),
                       c("Receiver", "Timestamp", "Sensor.Value"))
  

  input$Sensor.Unit <- rep(NA_character_, nrow(input))

  std_cols <- c("Timestamp", "Receiver", "CodeSpace",
                "Signal", "Sensor.Value", "Sensor.Unit")
  input <- input[, std_cols, with = FALSE]
  input$Timestamp <- fasttime::fastPOSIXct(as.character(input$Timestamp),
                                           tz = "UTC")

  # NOTE: This function uses stop() instead of event(),
  # because these stop calls are caught by a trycatch.
  if (any(is.na(input$Timestamp))) {
    stop("Importing timestamps failed", call. = FALSE)
  }
  if (any(is.na(input$Receiver))) {
    stop("Importing receivers failed", call. = FALSE)
  }
  if (any(is.na(input$CodeSpace))) {
    stop("Importing code space failed", call. = FALSE)
  }
  if (any(is.na(input$Signal))) {
    stop("Importing signals failed", call. = FALSE)
  }
  return(input)
}

#' Convert code spaces
#'
#' Unifies CodeSpace names, to avoid having different names depending on
#' ALS vendor.
#'
#' @param input A data frame of standardized detections.
#'
#' @return A data frame with standardized code spaces.
#'
#' @keywords internal
#'
convertCodes <- function(input) {
  event(type = "debug", "Running convertCodes.")
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
#' Converts the ALS timestamps (UTC) to the designated study area time zone.
#' Can also trim the data by time.
#'
#' @inheritParams convertCodes
#' @inheritParams explore
#'
#' @return A data frame with corrected timestamps.
#'
#' @keywords internal
#'
convertTimes <- function(input, start.time, stop.time, tz) {
  event(type = "debug", "Running convertTimes.")
  # NOTE: The NULL variables below are actually column names used by data.table.
  # This definition is just to prevent the package check from issuing a note
  # due unknown variables.
  Timestamp <- NULL

  attributes(input$Timestamp)$tzone <- tz
  input <- input[order(input$Timestamp), ]
  if (!is.null(start.time)){
    onr <- nrow(input)
    input <- input[Timestamp >= as.POSIXct(start.time, tz = tz)]
    event(type = c("screen", "report"),
          "M: Discarding detection data previous to ", start.time,
          " per user command (", onr - nrow(input), " detections discarded).")
  }
  if (!is.null(stop.time)){
    onr <- nrow(input)
    input <- input[Timestamp <= as.POSIXct(stop.time, tz = tz), ]
    event(type = c("screen", "report"),
          "M: Discarding detection data posterior to ", stop.time,
          " per user command (", onr - nrow(input), " detections discarded).")
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
  event(type = "debug", "Running createUniqueSerials.")
  output <- split(input, input$Receiver)
  for (i in 1:length(output)) {
    if (nrow(output[[i]]) > 1) {
      output[[i]]$Receiver <- paste0(output[[i]]$Receiver, 
                                     ".dpl.", 1:nrow(output[[i]]))
    }
  }
  return(output)
}

#' Split detections by tag
#'
#' Splits the detections' table by tags and selects only detections
#' from target tags
#'
#' @inheritParams explore
#' @inheritParams loadDetections
#' @param bio A table with the tags and biometrics of the studied animals.
#' @param detections A data frame with all the detections.
#'        Supplied by loadDetections.
#'
#' @return A list of detections for each tag.
#'
#' @keywords internal
#'
splitDetections <- function(detections, bio, exclude.tags = NULL) {
  event(type = "debug", "Running splitDetections.")

  if (file.exists(paste0(tempdir(), "/temp_strays.csv"))) {
    file.remove(paste0(tempdir(), "/temp_strays.csv"))
  }

  # failsafe in case all detections for a transmitter were previously excluded
  detections$Transmitter <- droplevels(detections$Transmitter)

  my.list <- split(detections, detections$Transmitter)
  my.list <- excludeTags(input = my.list, exclude.tags = exclude.tags)

  checkNoDetections(input = my.list, bio = bio)
  checkDupSignals(input = my.list, bio = bio)

  event(type = "debug", "Creating 'trimmed.list'.")
  # this dataframe serves as an index to the tags detected
  detected <- data.frame(Code.space = extractCodeSpaces(names(my.list)),
                         Signal = extractSignals(names(my.list)))

  # and this one as an index for the target tags
  if (any(grepl("^Code\\.space$", colnames(bio)))) {
    bio_aux <- bio[, c("Code.space", "Signal")]
  } else {
    bio_aux <- data.frame(Code.space = NA,
                          Signal = bio$Signal)
  }

  # break down the signals for multi-signal tags
  aux <- strsplit(as.character(bio$Signal),
                  "|", fixed = TRUE)
  bio_aux$Signal_expanded <- lapply(aux, as.numeric)

  # include sensor units, if relevant
  if (any(grepl("^Sensor\\.unit$", colnames(bio)))) {
    bio_aux$Sensor.unit_expanded <- strsplit(as.character(bio$Sensor.unit),
                                             "|", fixed = TRUE)
  } else {
    bio_aux$Sensor.unit_expanded <- NA
  }

  trimmed_list_names <- c() # to store the names as the lapply goes

  event(type = "screen",
        "M: Extracting relevant detections...")

  trimmed_list <- lapply(1:nrow(bio_aux), function(i) {
    # cat(i, "\r")

    # create/reset variable to store the codespace
    the_codespace <- c()

    # This sapply grabs all entries that match the
    # target signal(s) and code space (if relevant)
    list_matches <- sapply(bio_aux$Signal_expanded[[i]], function(j) {
      link_s <- detected$Signal == j

      if (sum(link_s) == 0) {
        return(NA)
      }

      if (is.na(bio_aux$Code.space[i])) {
        if (sum(link_s) > 1) {
          # this should never happen because duplicated signals
          # with no codespaces are handled by checkDupSignals
          event(type = "stop",
                "Something went wrong when splitting the detections.
                This should not have happened. Contact the developer. (1)")
        }
        the_codespace <<- unique(detected$Code.space[which(link_s)])
        return(which(link_s))
      } else {
        link_c <- detected$Code.space[which(link_s)] == bio_aux$Code.space[i]
        if (sum(link_c) > 1) {
          # Even if there are multiple codespaces,
          # only one should fit the requested
          event(type = "stop",
                "Something went wrong when splitting the detections.",
                " This should not have happened. Contact the developer. (2)")
        }

        if (sum(link_c) == 0) {
          event(type = c("warning", "screen", "report"),
                "Signal ", j, " was found in the detections, but its",
                " code space does not match the required ('",
                bio_aux$Code.space[i], "' != '",
                paste0(unique(detected$Code.space[which(link_s)]),
                       collapse = "', '"),
                "'). Are you sure the code space was written correctly?",
                " Discarding detections from alien code space(s).")
          return(NA)
        } else {
          the_codespace <<- detected$Code.space[which(link_s)][link_c]
          return(which(link_s)[which(link_c)])
        }
      }
    })

    # compile the detections list
    if (all(is.na(list_matches))) { 
      # if the tag was not found, return empty
      return(NULL)
    } else { 
      # otherwise, prepare tag name and include sensor units if present
      trimmed_list_names <<- c(trimmed_list_names, 
                               paste0(the_codespace, "-", 
                                      min(bio_aux$Signal_expanded[[i]])))
      output <- my.list[list_matches]

      # Find Sensor.unit column in the biometrics
      if (any(grepl("^Sensor\\.unit$", colnames(bio)))) {
        # Replace sensor units...
        for (j in 1:length(output)) {
          sensor_index <- match(extractSignals(names(output)[j]),
                                bio_aux$Signal_expanded[[i]])
          # ...but only if the the sensor unit provided is not NA
          provided <- bio_aux$Sensor.unit_expanded[[i]][sensor_index]
          if (!is.na(provided)) {
            output[[j]]$Sensor.Unit <- rep(provided, nrow(output[[j]]))
          }
        }
      }
      # merge is required for multiple-signal tags
      output <- data.table::rbindlist(output)
      # order by time before delivering
      output <- output[order(output$Timestamp), ]
      return(output)
    }
  })

  # remove empty entries and then name the list components
  trimmed_list <- trimmed_list[!sapply(trimmed_list, is.null)]
  names(trimmed_list) <- trimmed_list_names

  event(type = "debug", "Creating transmitter codes.")

  # store output of extractSignals before running sapply loop
  # to massively save on processing time.
  trimmed_list_signals <- extractSignals(names(trimmed_list))

  # Extract transmitter names (to store in bio)
  transmitter_names <- sapply(1:nrow(bio), function(i) {
    # cat(i, "\r")

    the_signal <- bio$Signal[i]
    the_codespace <- bio$Code.space[i] # returns NULL if column is missing

    aux <- unlist(strsplit(as.character(the_signal), "|", fixed = TRUE))
    lowest_signal <- min(as.numeric(aux))

    if (is.null(the_codespace)) {
      # locations of the lowest_signal in the detected signals
      link <- match(lowest_signal, trimmed_list_signals)
      if (is.na(link)) {
        output <- paste0('Unknown-', lowest_signal)
      } else {
        output <- names(trimmed_list)[link]
      }
    } else {
      # like above but using full tag codes.
      link <- match(paste0(the_codespace, '-', the_signal), names(trimmed_list))
      if (is.na(link)) {
        output <- paste0(the_codespace, '-', lowest_signal)
      } else {
        output <- names(trimmed_list)[link]
      }
    }

    return(output)
  })

  # Transfer transmitter names to bio
  bio$Transmitter <- transmitter_names

  event(type = "debug", "Collecting stray information.")

  # Collect stray summary
  valid_tags <- unlist(lapply(trimmed_list, function(x) unique(x$Transmitter)))
  stray_tags <- !names(my.list) %in% as.character(valid_tags)
  if (any(stray_tags)) {
    collectStrays(input = my.list[stray_tags])
  }
  storeStrays()

  return(list(detections.list = trimmed_list, bio = bio))
}

#' Collect summary information on the tags detected
#' but that are not part of the study.
#'
#' @param input list of detections for the tags to be excluded.
#'
#' @return No return value, called for side effects.
#'
#' @keywords internal
#'
collectStrays <- function(input) {
  event(type = "debug", "Running collectStrays.")
  if (length(input) > 0) {
    first_d <- lapply(input,
                      function(x) as.character(head(x$Timestamp,1)))
    last_d <- lapply(input,
                     function(x) as.character(tail(x$Timestamp,1)))
    receivers <- lapply(input, 
                        function(x) paste(unique(x$Receiver),
                                          collapse = ", "))
    
    recipient <- data.frame(Transmitter = names(input),
                            N.detections = unlist(lapply(input, nrow)),
                            First.detection = unlist(first_d),
                            Last.detection = unlist(last_d),
                            Receivers = unlist(receivers))

    write.table(recipient, 
                file = paste0(tempdir(), "/temp_strays.csv"),
                sep = ",",
                append = file.exists(paste0(tempdir(), "/temp_strays.csv")),
                row.names = FALSE,
                col.names = !file.exists(paste0(tempdir(), "/temp_strays.csv")))
  }
}

#' Store summary information on the stray tags detected in a permanent file.
#'
#' @return No return value, called for side effects.
#'
#' @keywords internal
#'
storeStrays <- function() {
  event(type = "debug", "Running storeStrays.")
  if (file.exists(paste0(tempdir(), "/temp_strays.csv"))) {
    if (file.exists(newname <- "stray_tags.csv")) {
      continue <- TRUE
      index <- 1
      while (continue) {
        newname <- paste("stray_tags", index, "csv", sep = ".")
        if (file.exists(newname)) {
          index <- index + 1
        } else {
          continue <- FALSE
        }
      }
    }
    decision <- userInput(paste0("Stray tags were detected in your study area.",
                                 " Would you like to save a summary to ",
                                 newname, "?(y/n) "),
                          choices = c("y", "n"), hash = "# save strays?")
    if (!interactive()) {
      decision <- "y"
    }
    if (decision == "y") {
      file.copy(paste0(tempdir(), "/temp_strays.csv"), newname)
    }
  }
}

#' Standardize serial numbers, stations and arrays in the detections
#'
#' Matches the ALS serial number to the deployments to rename the serial number.
#' The corresponding deployment is then used to update the Standard Station name
#' and the array based in the spatial object.
#'
#' @param detections a data frame of detections
#' @param spatial A list of spatial objects in the study area
#' @param deployments a list of deployments
#'
#' @return A data frame with standardized station names.
#'
#' @keywords internal
#'
createStandards <- function(detections, spatial, deployments,
                            discard.orphans = FALSE) {
  event(type = "debug", "Running createStandards.")
  detections$Receiver <- as.character(detections$Receiver)
  detections$Standard.name <- NA_character_
  detections$Array <- NA_character_
  detections$Section <- NA_character_
  empty.receivers <- NULL
  event(type = c("screen", "report"),
        "M: Matching detections with deployment periods.")

  if (interactive()) {
    pb <- txtProgressBar(min = 0, max = nrow(detections),
                         style = 3, width = 60) # nocov
  }
  counter <- 0
  st <- spatial$stations
  for (i in 1:length(deployments)) {
    link_r <- detections$Receiver == names(deployments)[i]
    counter <- counter + sum(link_r)
    if (all(!link_r)) {
      empty.receivers <- c(empty.receivers, names(deployments)[i])
    } else {
      for (j in 1:nrow(deployments[[i]])) {
        # find target rows in detections
        A <- detections$Timestamp[link_r] >= deployments[[i]]$Start[j]
        B <- detections$Timestamp[link_r] < deployments[[i]]$Stop[j]
        link_d <- A & B
        # rename receiver
        detections$Receiver[link_r][link_d] <- deployments[[i]]$Receiver[j]
        # find corresponding standard station name
        link_s <- match(deployments[[i]]$Station.name[j], st$Station.name)
        # include Standard.name
        detections$Standard.name[link_r][link_d] <- st$Standard.name[link_s]
        # include Array
        detections$Array[link_r][link_d] <- as.character(st$Array[link_s])
        # include Section
        if (any(grepl("^Section$", colnames(st)))) {
          detections$Section[link_r][link_d] <- as.character(st$Section[link_s])
        }
      }
      orphans <- is.na(detections$Standard.name[link_r])
      if (any(orphans)) {
        rows.to.remove <- detections[link_r, which = TRUE][orphans]
        if (interactive()) {
          event(type = "screen", "") # nocov
        }
        if (!discard.orphans) {
          event(type = c("screen", "report"),
                "Error: ", sum(orphans), " detections for receiver ",
                names(deployments)[i],
                " do not fall within deployment periods.")
          event(type = "screen", "")

          cols_to_hide <- c("Transmitter", "Valid",
                            "Standard.name",
                            "Array", "Section")
          if ("Source.file" %in% colnames(detections)) {
            cols_to_hide <- c(cols_to_hide, "Source.file")
          }
          to_show <- detections[link_r, ][orphans, ]
          to_show <- to_show[, !(colnames(to_show) %in% cols_to_hide)]

          event(type = "screen",
                paste0(capture.output(print(to_show)), collapse = "\n"),
                 "\nPossible options:\n",
                "   a) Stop and double-check the data (recommended)\n",
                "   b) Discard orphan detections in this instance.\n",
                "   c) Discard orphan detections for all instances.\n",
                "   d) Save orphan detections to a file and re-open dialogue.")

          restart <- TRUE
          while (restart) {
            if (interactive()) { # nocov start
              decision <- userInput(paste0("Which option should be",
                                           " followed?(a/b/c/d) "),
                                    choices = letters[1:4],
                                    hash = paste0("# orphan detections for",
                                                 " receiver ",
                                                 names(deployments)[i]))
            } else { # nocov end
              decision <- "b"
            }

            if (decision == "a") {
              event(type = "stop",
                    "Stopping analysis per user command.") # nocov
            }

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
              file.name <- userInput(paste0("Please specify a file name",
                                            " (leave empty to abort saving): "),
                                     hash = paste0("# save receiver orphans",
                                                   " to this file"))
              # break if empty
              if (file.name == "") {
                next()
              }
              # confirm extension
              if (!grepl("\\.csv$", file.name)) {
                file.name <- paste0(file.name, ".csv")
              }
              # prevent auto-overwrite
              if (file.exists(file.name)) {
                aux <- userInput(paste0("File '", file.name,
                                        "' already exists.",
                                        " Overwrite contents?(y/n) "),
                                 choices = c("y", "n"),
                                 hash = "# overwrite file with same name?")
                if (aux == "y") {
                  overwrite <- TRUE
                } else {
                  overwrite <- FALSE
                }
              } else {
                overwrite <- TRUE
              }
              # save
              if (overwrite) {
                success <- TRUE
                # recover if saving fails
                tryCatch(data.table::fwrite(detections[rows.to.remove], 
                                            file.name, 
                                            dateTimeAs = "write.csv"),
                         error = function(e) {
                           event(type = c("screen", "report"),
                                 "Error: Could not save file (reason: '",
                                 sub("\n$", "", e), "').\n",
                                 "       Reopening previous interaction.")
                         success <<- FALSE
                })
                if (success) {
                  event(type = c("screen", "report"),
                        "M: A copy of the orphan detections has been",
                        " saved to '", file.name, "'.\n",
                        "   Reopening previous interaction.")
                }
              }
            } # nocov end
          }
        } else {
          event(type = c("warning", "screen", "report"),
                sum(orphans), " detections for receiver ",
                names(deployments)[i], "
                do not fall within deployment periods.",
                " Discarding orphan detections.")
          detections <- detections[-rows.to.remove]
        }
      }
    }
    if (interactive()) { # nocov start
      setTxtProgressBar(pb, counter)
      flush.console()
    } # nocov end
  }

  if (interactive()) { # nocov start
    setTxtProgressBar(pb, nrow(detections))
    close(pb)
  } # nocov end

  event(type = c("screen", "report"),
        "M: Number of ALS: ", length(deployments),
        " (of which ", length(empty.receivers), " had no detections)")

  if (!is.null(empty.receivers)) {
    event(type = c("warning", "screen", "report"),
          "No detections were found for receiver(s) ",
          paste0(empty.receivers, collapse = ", "), ".")
  }

  detections$Receiver <- as.factor(detections$Receiver)
  detections$Array <- factor(detections$Array,
                             levels = unlist(spatial$array.order))

  if (any(grepl("^Section$", colnames(spatial$stations))))
    detections$Section <- factor(detections$Section,
                                 levels = names(spatial$array.order))

  detections$Standard.name <- factor(detections$Standard.name,
                                     levels = spatial$stations$Standard.name)
  return(detections)
}

#' Process spatial elements
#'
#' Creates a list containing multiple spatial elements required
#' throughout the analyses
#'
#' @param first.array Either NULL or the top level array in the study area.
#' @inheritParams splitDetections
#' @inheritParams explore
#'
#' @return The stations, release sites and array order.
#'
#' @keywords internal
#'
transformSpatial <- function(spatial, bio, dot_list, first.array = NULL) {
  event(type = "debug", "Running transformSpatial.")
  # Break the stations away
  event(type = "debug", "Creating 'stations'.")
  rows <- spatial$Type == "Hydrophone"
  cols <- -match("Type", colnames(spatial))
  stations <- spatial[rows, cols]
  stations$Array <- factor(stations$Array, levels = unique(stations$Array))

  event(type = "debug", "Creating 'release.sites'.")
  # If there is any release site in the spatial file
  if (sum(spatial$Type == "Release") > 0) {
    # If no release sites were specified in the biometrics
    one_release <- length(unique(bio$Release.site)) == 1
    unspecified <- unique(bio$Release.site) == "unspecified"
    if (one_release && unspecified) {
      event(type = c("warning", "screen", "report"),
            "At least one release site has been indicated in the",
            " spatial.csv file, but no release sites were specified",
            " in the biometrics file.\n",
            "         Discarding release site information and assuming",
            " all animals were released at the top level array to avoid",
            " function failure.\n",
            "         Please double-check your data.")
      # Try to recover by assigning a first array, if possible
      if (is.null(first.array)) {
        event(type = "stop",
              "There is more than one top level array in the study area.",
              " Please specify release site(s) in the 'spatial.csv' file",
              " and in the 'biometrics.csv' file.")
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
      link <- is.na(match(B, A))
      # If any release sites in the biometrics are missing in the spatial
      if (any(link)) {
        event(type = "stop",
              "There is a mismatch between the release sites reported and",
              " the release locations for the animals.\n",
              "The following release sites were listed in the biometrics.csv",
              " file but are not part of the release sites listed in the",
              " spatial.csv file: ",
              paste(sort(B[link]), collapse = ", "), "\n",
              "Please include the missing release sites in",
              " the spatial.csv file.")
      } else {
        from.row <- spatial$Type == "Release"
        link <- !(colnames(spatial) %in% c("Receiver", "Section"))
        from.col <- colnames(spatial)[link]
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
      link <- grepl("|", release.sites$Array, fixed = TRUE)
      if (any(link)) {
        if (sum(link) >= 6) {
          event(type = c("screen", "report"),
                "M: Multiple possible first arrays detected for more",
                " than five release sites.")
        }
        for (i in which(link)) {
          if (sum(link) < 6) {
            event(type = c("screen", "report"),
                  "M: Multiple possible first arrays detected for",
                  " release site '", release.sites$Standard.name[i], "'.")
          }
          aux <- unlist(strsplit(release.sites$Array[i],
                                 "|", fixed = TRUE))
          these <- dot_list$array_info$dotmat[aux, aux]
          if (any(is.na(these)) ||  any(these > 1)) {
            event(type = c("warning", "screen", "report"),
                  "Release site ", release.sites$Standard.name[i],
                  " has multiple possible first arrays (",
                  paste(aux, collapse = ", "),
                  "), but not all of these arrays appear to be directly",
                  " connected with each other.",
                  " Could there be a mistake in the input?")
          }
        }
      }
    }
  } else {
    event(type = c("warning", "screen", "report"),
          "Release sites were not specified in the spatial.csv file.",
          " Attempting to assume all released animals start at the",
          " top level array.")
    if (is.null(first.array)) {
      event(type = "stop",
            "There is more than one top level array in the study area.",
            " Please specify release site(s) in the spatial.csv file and",
            " in the biometrics.csv file.")
    }
    release.sites <- data.frame(Station.name = unique(bio$Release.site),
                                Longitude = NA_real_,
                                Latitude = NA_real_,
                                Array = rep(first.array,
                                            length(unique(bio$Release.site))),
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
    # array.order is used to determine if the tag's last detection
    # was in the last array of a given section
    array.order <- list()
    for (j in sections) {
      link <- spatial$Type == "Hydrophone" & spatial$Section == j
      array.order[[j]] <- unique(spatial$Array[link])
    }
  } else {
    array.order <- list(all = names(dot_list$array_info$arrays))
  }
  # Order release sites by entry point.
  first.releases <- sapply(as.character(release.sites$Array),
                           function(x) {
                             unlist(strsplit(x, "|", fixed = TRUE))[1]
                           })
  the_order <- match(first.releases, unlist(array.order))
  release.sites <- release.sites[order(the_order), ]

  # join everything
  output <- list(stations = stations,
                 release.sites = release.sites,
                 array.order = array.order)
  return(output)
}

#' Collect summary information on the tags detected
#' but that are not part of the study.
#'
#' @param input list of detections
#' @inheritParams explore
#' @inheritParams splitDetections
#'
#' @return A list of detections for each tag that
#'         does not contain the excluded tags.
#'
#' @keywords internal
#'
excludeTags <- function(input, exclude.tags){
  event(type = "debug", "Running excludeTags.")
  if (length(exclude.tags) != 0) {
    link <- match(exclude.tags, names(input))
    logical_link <- !is.na(link)
    if (any(!logical_link)) {
      event(type = c("warning", "screen", "report"),
            "The user asked for ",
            ifelse(sum(!logical_link) > 1, "tags '", "tag '"),
            paste(exclude.tags[!logical_link], collapse = "', '"),
            "' to be excluded from the analysis, but ",
            ifelse(sum(!logical_link) > 1, "these tags are", "this tag is"),
            " not present in the detections.")
    }
    if (!all(!logical_link)) {
      link <- link[!is.na(link)]
      event(type = c("screen", "report"),
            "M: Excluding tag(s) ",
            paste(exclude.tags[logical_link], collapse = ", "),
            " from the analysis per used command (detections removed: ",
            paste(unlist(lapply(input[link], nrow)), collapse = ", "),
            ifelse(length(input[link]) > 1, ", respectively).", ")."))
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
  event(type = "debug", "Running discardFirst.")
  # convert trim to seconds
  trim <- trim * 3600

  # count is used for the message at the end
  det_discarded <- 0
  output <- lapply(seq_along(input), function(i) {
    too_early <- input[[i]]$Timestamp <= bio$Release.date[i] + trim
    det_discarded <<- det_discarded + sum(too_early)
   
    input[[i]]$Valid[too_early] <- FALSE

    event(type = "debug",
          sum(too_early), " early detection(s) invalidated for tag ",
          names(input)[i], ".")
  
    return(input[[i]])
  })
  names(output) <- names(input)

  event(type = "Screen",
        "M: ", det_discarded, " detection(s) were invalidated because they",
        " were recorded before the time set in 'discard.first' had passed.")
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
  event(type = "debug", "Running liveArrayTimes.")
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
          link <- which(breaks$values)
          to.combine <- data.frame(from = cumsum(breaks$lengths)[link - 1],
                                   to = cumsum(breaks$lengths)[link])

          aux$isolated <- !aux$overlaps & !c(aux$overlaps[-1], FALSE)

          output <- aux[aux$isolated, c("Start", "Stop")]

          for (i in 1:nrow(to.combine)) {
            stop_range <- to.combine$from[i]:to.combine$to[i]
            tmp <- data.frame(Start = aux$Start[to.combine$from[i]],
                              Stop = max(aux$Stop[stop_range]))
            output <- rbind(output, tmp)
          }

          output <- output[order(output$Start, output$Stop), ]

          output$overlaps <- c(FALSE, 
                               output$Stop[-nrow(output)] >= output$Start[-1])

          if (any(output$overlaps)) {
            aux <- output
          } else {
            restart <- FALSE
          }
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
