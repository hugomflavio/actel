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
    lowest_signals <- sapply(bio$Signal, function(i) min(as.numeric(unlist(strsplit(as.character(i), "|", fixed = TRUE)))))
    if (any(link <- is.na(match(override, lowest_signals))))
      stopAndReport("Some tag signals listed in 'override' (", paste0(override[link], collapse = ", "), ") are not listed in the biometrics file.")
  }
  deployments <- loadDeployments(input = "deployments.csv", tz = tz)
  checkDeploymentTimes(input = deployments) # check that receivers are not deployed before being retrieved
  
  spatial <- loadSpatial(input = "spatial.csv", section.order = section.order)

  deployments <- checkDeploymentStations(input = deployments, spatial = spatial) # match Station.name in the deployments to Station.name in spatial, and vice-versa
  deployments <- createUniqueSerials(input = deployments) # Prepare serial numbers to overwrite the serials in detections

  detections <- loadDetections(start.time = start.time, stop.time = stop.time, tz = tz,
    save.detections = save.detections, record.source = getOption("actel.debug", default = FALSE))
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
loadDot <- function(string = NULL, input = NULL, spatial, disregard.parallels) {
  appendTo("debug", "Running loadDot.")
  if (is.null(string) & is.null(input))
    stopAndReport("No dot file or dot string were specified.")
  if (is.null(string)) {
    tryCatch(dot <- readDot(input = input),
      error = function(e) stopAndReport("The contents of the '", input, "' file could not be recognised by the readDot function."))
  } else {
    dot <- readDot(string = string)
  }
  mat <- dotMatrix(input = dot)
  unique.arrays <- unique(unlist(sapply(spatial$Array, function(x) unlist(strsplit(x, "|", fixed = TRUE)))))
  if (any(link <- is.na(match(unique.arrays, colnames(mat))))) {
    if (file.exists("spatial.txt"))
      stopAndReport(paste0("Not all the arrays listed in the spatial.csv file are present in the spatial.txt.\nMissing arrays: ", paste(unique.arrays[link], collapse = ", "), "\n"), call. = FALSE)
    
    if (file.exists("spatial.dot")) {
      stopAndReport(paste0("Not all the arrays listed in the spatial.csv file are present in the spatial.dot.\nMissing arrays: ", paste(unique.arrays[link], collapse = ", "), "\n"), call. = FALSE)
    } else {
      stopAndReport(
"Something went wrong when compiling the dot file:
 - If you are using preload(), double-check that the array names in the dot string match the array names in the spatial input.
 - If you are using input files, try restarting R and trying again. If the problem persists, contact the developer.")
    }
  }
  arrays <- dotList(input = dot, spatial = spatial)
  arrays <- dotPaths(input = arrays, dotmat = mat, disregard.parallels = disregard.parallels)
  shortest.paths <- findShortestChains(input = arrays)
  return(list(dot = dot, arrays = arrays, dotmat = mat, paths = shortest.paths))
}

#' Read dot file or string
#'
#' @inheritParams loadDot
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
readDot <- function (input = NULL, string = NULL) {
  if (is.null(string) & is.null(input))
    stop("No dot file or data were specified.")
  if (is.null(string)) {
    if (!file.exists(input))
      stop("Could not find a '", input, "' file in the working directory.")
    lines <- readLines(input)
  } else {
    lines <- unlist(strsplit(string, "\n|\t"))
  }
  paths <- lines[grepl("[<-][->]", lines)]
  if (length(paths) == 0)
    stop("Could not recognise the input contents as DOT formatted connections.")
  paths <- gsub("[ ;]", "", paths)
  paths <- gsub("\\[label=[^\\]]","", paths)
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
  for (a in unique(c(t(input[, c(1, 3)])))) {
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
#' @param dotmat A dot distance matrix
#' @inheritParams migration
#'
#' @return A list of the all array paths between each pair of arrays.
#'
#' @keywords internal
#'
dotPaths <- function(input, dotmat, disregard.parallels) {
  appendTo("debug", "Running dotPaths.")
  recipient <- findPeers(input = input, dotmat = dotmat, type = "before", disregard.parallels = disregard.parallels)
  recipient <- findDirectChains(input = recipient, dotmat = dotmat,  type = "before")
  recipient <- findPeers(input = recipient, dotmat = dotmat,  type = "after", disregard.parallels = disregard.parallels)
  recipient <- findDirectChains(input = recipient, dotmat = dotmat,  type = "after")
  return(recipient)
}

#' Find efficiency peers for each array
#'
#' @param input An array list
#' @param type The type of peers to be found ("before" or "after")
#'
#' @return The array list with efficiency peers.
#'
#' @keywords internal
#'
findPeers <- function(input, dotmat, type = c("before", "after"), disregard.parallels) {
  type <- match.arg(type)
  opposite <- ifelse(type == "before", "after", "before")
  for (a in names(input)) {
    peers <- NULL
    to.check <- input[[a]][[type]]
    while (!is.null(to.check)) {
      new.check <- NULL
      for (b in to.check) {
        # IF A and B are adjacent
        if (dotmat[a, b] == 1) {
          # IF there are no third-party paths leading to B
          if (length(input[[b]][[opposite]]) == 1) {
            # IF B has no parallels or parallels are being discarded
            if (is.null(input[[b]]$parallel) || disregard.parallels) {
              if (is.null(peers) || all(!grepl(b, peers))) {
                peers <- c(peers, b)
                new.check <- c(new.check, input[[b]][[type]])
              }
            # IF B has parallels
            } else {
              # Find out which arrays lead to the parallels
              leading.to.parallels <- unique(unlist(sapply(input[[b]]$parallel, function(x) input[[x]][[opposite]])))
              # as this is a distance 1 case, verify that only array A leads to the parallels.
              if (all(!is.na(match(leading.to.parallels, a)))) {
                peers <- c(peers, b)
                new.check <- c(new.check, input[[b]][[type]])
              }
            }
          }
        }
        # If B is far away, check that the paths leading to B are in the valid peers list
        if (dotmat[a, b] > 1 && all(!is.na(match(input[[b]][[opposite]], peers)))) {
          # IF B has no parallel arrays, or disregard parallels is set to TRUE
          if (is.null(input[[b]]$parallel) || disregard.parallels) {
            if (is.null(peers) || all(!grepl(b, peers))) {
              peers <- c(peers, b)
              new.check <- c(new.check, input[[b]][[type]])
            }
            # IF B has paralles
          } else {
            # Find out which arrays lead to the parallels
            leading.to.parallels <- unique(unlist(sapply(input[[b]]$parallel, function(x) input[[x]][[opposite]])))
            # as this is a distance >1 case, verify that all arrays leading to the parallels are contained in the peers
            if (all(!is.na(match(leading.to.parallels, peers)))) {
              peers <- c(peers, b)
              new.check <- c(new.check, input[[b]][[type]])
            }
          }
        }
      }
      to.check <- unique(new.check)
    }
    input[[a]][[paste0(type, ".peers")]] <- unique(peers)
  }
  return(input)
}

#' Find all arrays linked to an array in a given direction
#'
#' @param input An array list
#' @param type The direction in which to expand the chain ("before" or "after")
#'
#' @return The array list with all linked arrays.
#'
#' @keywords internal
#'
findDirectChains <- function(input, dotmat, type = c("before", "after")) {
  type <- match.arg(type)
  for(a in names(input)) {
    chain <- NULL
    parallel.aux <- input[[a]]$parallel
    to.check <- input[[a]][[type]]
    while (!is.null(to.check)) {
      new.check <- NULL
      for (b in to.check) {
          if (is.null(chain) || all(!grepl(b, chain))) {
            chain <- c(chain, b)
            parallel.aux <- c(parallel.aux, input[[b]]$parallel)
            new.check <- c(new.check, input[[b]][[type]])
          }
        to.check <- unique(new.check)
      }
    }
    input[[a]][[paste0("all.", type)]] <- unique(chain)
    input[[a]][[paste0("all.", type, ".and.par")]] <- unique(c(chain, parallel.aux))
  }
  return(input)
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
  input$Standard.name <- gsub(" ", "", input$Standard.name)
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
      dist.mat <- read.csv(input, row.names = 1)
      if (ncol(dist.mat) == 1)
        warning("Only one column was identified in '", input, "'. If this seems wrong, please make sure that the values are separated using commas.", immediate. = TRUE, call. = FALSE)
    } else {
      dist.mat <- NULL
    }
  } else {
    dist.mat <- as.matrix(input)
  }

  if (!is.null(dist.mat)) {
    rownames(dist.mat) <- gsub(" ", "", rownames(dist.mat))
    colnames(dist.mat) <- gsub(" ", "", colnames(dist.mat))
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
    stopAndReport("The following columns are duplicated in the deployments: '", paste(unique(colnames(input)[link]), sep = "', '"), "'.", call. = FALSE)

  default.cols <- c("Receiver", "Station.name", "Start", "Stop")
  link <- match(default.cols, colnames(input))
  if (any(is.na(link))) {
    stopAndReport(paste0("Column",
      ifelse(sum(is.na(link)) > 1, "(s) '", " '"),
      paste(default.cols[is.na(link)], collapse = "', '"),
      ifelse(sum(is.na(link)) > 1, "' are", "' is"),
      " missing in the deployments."), call. = FALSE)
  }
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
  if (!any(grepl("Station.name", colnames(input)))) {
    stopAndReport("The spatial input must contain a 'Station.name' column.")
  } else {
    # Check all station names are unique
    if (any(link <- table(input$Station.name) > 1)) {
      stopAndReport("The 'Station.name' column in the spatial input must not have duplicated values.\nStations appearing more than once: ", paste(names(table(input$Station.name))[link], collapse = ", "), "")
    }
  }
  # Check missing Array column
  if (!any(grepl("Array", colnames(input)))) {
    stopAndReport("The spatial input must contain an 'Array' column.")
  }
  # check missing data in the arrays
  if (any(is.na(input$Array)))
    stopAndReport("Some rows do not contain 'Array' information in the spatial input. Please double-check the input files.")
  # check spaces in the array names
  if (any(grepl(" ", input$Array))) {
    appendTo("Screen", "M: Replacing spaces in array names to prevent function failure.")
    input$Array <- gsub(" ", "_", input$Array)
  }
  # check arrays called "Release"
  if (any(grepl("^Release$", input$Array)))
    stopAndReport("The term 'Release' is reserved for internal calculations. Do not name any sections or arrays as 'Release'.")
  # check arrays called "Total"
  if (any(grepl("^Total$", input$Array)))
    stopAndReport("The term 'Total' is reserved for internal calculations. Do not name any sections or arrays as 'Total'.")
  # check arrays called "Invalid"
  if (any(grepl("^Invalid$", input$Array)))
    stopAndReport("The term 'Invalid' is reserved for internal calculations. Do not name any arrays as 'Invalid'.")
  # check array name length
  aux <- unlist(strsplit(input$Array, "|", fixed = TRUE))
  if (any(nchar(as.character(aux)) > 6))
    appendTo(c("Screen", "Report", "Warning"), "Long array names detected. To improve graphic rendering, consider keeping array names under six characters.")
  rm(aux)
  # check missing Type column
  if (!any(grepl("Type", colnames(input)))) {
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
      appendTo("Screen", "M: Replacing spaces in section names to prevent function failure.")
      input$Section <- gsub(" ", "_", input$Section)
    }
    sections <- unique(input$Section[input$Type == "Hydrophone"])
    # check sections called "Release"
    if (any(grepl("^Release$", sections)))
      stopAndReport("The term 'Release' is reserved for internal calculations. Do not name any sections or arrays as 'Release'.")
    if (any(grepl("^Total$", sections)))
      stopAndReport("The term 'Total' is reserved for internal calculations. Do not name any sections or arrays as 'Total'.")
    # check that section names are independent
    if (any(link <- sapply(sections, function(i) length(grep(i, sections))) > 1))
      stopAndReport(
        ifelse(sum(link) == 1, "Section '", "Sections '"),
        paste(sections[link], collapse = "', '"),
        ifelse(sum(link) == 1, "' is", "' are"),
        " contained within other section names. Sections must be unique and independent.\n       Please rename your sections so that section names are not contained within each other.")

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

  if (is.character(input))
    preloaded <- FALSE
  else
    preloaded <- TRUE

  if (!preloaded) {
    if (file.exists(input))
      bio <- as.data.frame(suppressWarnings(data.table::fread(input, colClasses = c("Release.date" = "character"))),
                           stringsAsFactors = FALSE)
    else
      stopAndReport("Could not find a '", input, "' file in the working directory.")
  } else {
    bio <- as.data.frame(input, stringsAsFactors = FALSE)
    to.convert <- which(sapply(bio, class) == "factor")
    if (length(to.convert) > 0) {
      for(i in to.convert) {
        bio[, i] <- as.character(bio[, i])
      }
    }
  }

  if (any(link <- duplicated(colnames(bio))))
    stopAndReport("The following columns are duplicated in the biometrics: '",
      paste(unique(colnames(bio)[link]), sep = "', '"), "'.")

  if (!any(grepl("Release.date", colnames(bio)))) {
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

  if (!any(grepl("Signal", colnames(bio)))){
    stopAndReport("The biometrics must contain an 'Signal' column.")
  }

  if (any(is.na(bio$Signal))) {
    stopAndReport("Some animals have no 'Signal' information. Please double-check the biometrics.")
  }

  if (any(grepl("|", bio$Signal, fixed = TRUE))) {
    appendTo(c("Screen", "Report"), "M: Multi-sensor tags detected. These tags will be referred to by their lowest signal value.")
    expect_integer <- FALSE
  } else {
    expect_integer <- TRUE
  }

  if (expect_integer & !inherits(bio$Signal, "integer")) {
    stopAndReport("Could not recognise the data in the 'Signal' column as integers. Please double-check the biometrics.")
  } else {
    signal_check <- suppressWarnings(as.numeric(unlist(strsplit(as.character(bio$Signal), "|", fixed = TRUE))))
    if (any(is.na(signal_check))) {
      stopAndReport("Could not recognise the data in the 'Signal' column as integers. Please double-check the biometrics.")
    }
  }

  if (expect_integer & any(link <- table(bio$Signal) > 1)) {
    stopAndReport(ifelse(sum(link) > 1, "Signals ", "Signal "), paste(names(table(bio$Signal))[link], collapse = ", "), ifelse(sum(link) > 1," are ", " is "), "duplicated in the biometrics.")
  } else {
    if (any(link <- table(signal_check) > 1)) {
      stopAndReport(ifelse(sum(link) > 1, "Signals ", "Signal "), paste(names(table(signal_check))[link], collapse = ", "), ifelse(sum(link) > 1," are ", " is "), "duplicated in the biometrics.")
    }
  }

  if (!expect_integer & !any(grepl("Sensor.unit", colnames(bio))))
    appendTo(c("Screen", "Warning"), "Tags with multiple sensors are listed in the biometrics, but a 'Sensor.unit' column could not be found. Skipping sensor unit assignment.")

  if (!any(grepl("Release.site", colnames(bio)))) {
    appendTo("Screen", "M: No Release site has been indicated in the biometrics. Creating a 'Release.site' column to avoid function failure. Filling with 'unspecified'.")
    bio$Release.site <- "unspecified"
  } else {
    bio$Release.site <-  gsub(" ", "", bio$Release.site)
    bio$Release.site <- factor(bio$Release.site)
    if (any(link <- is.na(bio$Release.site) | bio$Release.site == "")) {
      appendTo(c("Screen","Report","Warning"),"Some animals contain no release site information. You may want to double-check the data.\n   Filling the blanks with 'unspecified'.")
      levels(bio$Release.site) <- c(levels(bio$Release.site), "unspecified")
      bio$Release.site[link] <- "unspecified"
      bio$Release.site <- droplevels(bio$Release.site)
    }
  }
  if (!any(grepl("Group", colnames(bio)))) {
    appendTo("Screen", paste0("M: No 'Group' column found in the biometrics. Assigning all animals to group 'All'."))
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
    if (force) {
      decision <- "Y"
    } else { # nocov start
      appendTo("Screen", paste0("M: The detections have been processed on ", actel.detections$timestamp, ".\n   If the input detection files were not changed, it is safe to use these again."))
      decision <- userInput("   Reuse processed detections?(y/n) ", choices = c("y", "n"), hash = "# reuse detections?")
    } # nocov end
    if (decision == "y"){
      appendTo(c("Screen","Report"), paste0("M: Using detections previously compiled on ", actel.detections$timestamp, "..."))
      detections <- actel.detections$detections
      attributes(detections$Timestamp)$tzone <- "UTC"
      detections <- convertTimes(input = detections, start.time = start.time, stop.time = stop.time, tz = tz)
      recompile <- FALSE
    } else {  # nocov start
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
      return(NULL)
    } else {
      if(nrow(aux) == 0){
        appendTo(c("Screen", "Report"), paste0("File '", i, "' is empty, skipping processing."))
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

  # Find signal matches, including for double-signal tags
  detected_signals <- extractSignals(names(my.list))
  target_signals_list <- lapply(strsplit(as.character(bio$Signal), "|", fixed = TRUE), as.numeric)
  aux <- lapply(1:length(target_signals_list), function(i) {
    output <- rep(NA_integer_, length(detected_signals))
    output[detected_signals %in% target_signals_list[[i]]] <- i
    return(output)
  })
  signal_match <- combine(aux) # detected_signals that are part of the target_signals_list

  # Combine tables for multi-signal tags and transfer sensor units
  if (any(table(signal_match) > 1)) {
    to.combine <- names(which(table(signal_match) > 1))
    for (i in to.combine) {
      unordered_indexes <- which(signal_match %in% i)
      if (any(grepl("Sensor.unit", colnames(bio)))) {
        sensor_units <- unlist(strsplit(bio$Sensor.unit[as.numeric(i)], "|", fixed = TRUE))
        if (length(sensor_units) != length(unordered_indexes)) {
          appendTo(c("Screen", "Warning", "Report"),
            paste0("The number of sensor units provided does not match the number of signals emitted ('",
              bio$Sensor.unit[as.numeric(i)], "' ",
              ifelse(length(sensor_units) > length(unordered_indexes), ">", "<"), " '",
              bio$Signal[as.numeric(i)], "').\n         Aborting sensor unit attribution."))
        } else {
          for (j in 1:length(sensor_units)) {
            my.list[[unordered_indexes[j]]]$Sensor.Unit <- rep(sensor_units[j], nrow(my.list[[unordered_indexes[j]]]))
          }
        }
      }
      indexes <- unordered_indexes[order(unordered_indexes)]
      aux <- data.table::rbindlist(my.list[indexes])
      aux <- aux[order(aux$Timestamp), ]
      my.list[[indexes[1]]] <- aux
    }
  }

  # Transfer transmitter names to bio
  lowest_target_signals <- sapply(bio$Signal, function(i) {
    min(as.numeric(unlist(strsplit(as.character(i), "|", fixed = TRUE))))
  })
  link <- match(lowest_target_signals, detected_signals) # locations of the lowest_target_signals in the detected_signals
  bio$Transmitter <- names(my.list)[link]

  # extract target detections (keep only lowest signals per tag, reuse link from above)
  trimmed.list <- my.list[na.exclude(link)]

  # include sensor units for single signal tags, if relevant
  if (any(grepl("Sensor.unit", colnames(bio)))) {
    link <- match(extractSignals(names(trimmed.list)), bio$Signal)
    aux <- names(trimmed.list)
    trimmed.list <- lapply(1:length(link), function(i) {
      output <- trimmed.list[[i]]
      if (!is.na(link[i])) {
        if (grepl("|", bio$Sensor.unit[link[i]], fixed = TRUE)) {
          appendTo(c("Screen", "Warning", "Report"),
            paste0("The tag with signal ", bio$Signal[link[i]],
              " appears to have more than one sensor unit ('", bio$Sensor.unit[link[i]],
              "'). Could there be an error in the input data?"))
        }
        if (!is.na(bio$Sensor.unit[link[i]]) & bio$Sensor.unit[link[i]] != "")
          output$Sensor.Unit <- rep(bio$Sensor.unit[link[i]], nrow(output))
      }
      return(output)
    })
    names(trimmed.list) <- aux
  }

  # Collect stray summary
  if (any(is.na(signal_match))) {
    collectStrays(input = my.list[is.na(signal_match)])
  }
  storeStrays()

  return(list(detections.list = trimmed.list, bio = bio))
}

#' Collect summary information on the tags detected but that are not part of the study.
#'
#' @param input list of detections for the tags to be excluded.
#' @param restart logical: if TRUE, remove file 'temp_strays.csv' from the working directory.
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
  for (i in 1:length(deployments)) {
    receiver.link <- detections$Receiver == names(deployments)[i]
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
        if (any(grepl("Section", colnames(spatial$stations))))
          detections$Section[receiver.link][deployment.link] <- as.character(spatial$stations$Section[the.station])
      }
      if (any(the.error <- is.na(detections$Standard.name[receiver.link]))) {
        if (!discard.orphans) {
          appendTo(c("Screen", "Report"), paste0("Error: ", sum(the.error), " detections for receiver ", names(deployments)[i], " do not fall within deployment periods."))
          message("")
          message(paste0(capture.output(print(detections[receiver.link, ][the.error, !c("Transmitter", "Valid", "Standard.name", "Array", "Section")])), collapse = "\n"))
          message("")
          message("Possible options:\n   a) Stop and double-check the data (recommended)\n   b) Discard orphan detections in this instance.\n   c) Discard orphan detections for all instances.")
          
          if (interactive()) { # nocov start
            decision <- userInput("Which option should be followed?(a/b/c) ", 
                                  choices = letters[1:3], 
                                  hash = paste("# orphan detections for receiver", names(deployments)[i]))
          } else { # nocov end
            decision <- "b"
          }

          if (decision == "a") { # nocov start
            stopAndReport("Stopping analysis per user command.")
          } else { # nocov end
            rows.to.remove <- detections[receiver.link, which = TRUE][the.error]
            detections <- detections[-rows.to.remove]
          }

          if (decision == "c")
            discard.orphans <- TRUE

        } else {
          appendTo(c("Screen", "Report"), paste0("Error: ", sum(the.error), " detections for receiver ", names(deployments)[i], " do not fall within deployment periods. Discarding orphan detections."))
          rows.to.remove <- detections[receiver.link, which = TRUE][the.error]
          detections <- detections[-rows.to.remove]
        }
      }
    }
  }
  appendTo(c("Screen", "Report"), paste0("M: Number of ALS: ", length(deployments), " (of which ", length(empty.receivers), " had no detections)"))
  if (!is.null(empty.receivers))
    appendTo(c("Screen", "Report", "Warning"), paste0("No detections were found for receiver(s) ", paste0(empty.receivers, collapse = ", "), "."))
  detections$Receiver <- as.factor(detections$Receiver)
  detections$Array <- factor(detections$Array, levels = unlist(spatial$array.order))
  if (any(grepl("Section", colnames(spatial$stations))))
    detections$Section <- factor(detections$Section, levels = names(spatial$array.order))
  detections$Standard.name <- factor(detections$Standard.name, levels = spatial$stations$Standard.name)
  return(detections)
}

#' Process spatial elements
#'
#' Creates a list containing multiple spatial elements required throughout the analyses
#'
#' @param file an input file with spatial data.
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
          paste(sort(B[link]), collapse = ", "), "\nPlease include the missing release sites in the spatial.csv file.", call. = FALSE)
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
      appendTo(c("Screen", "Report"), paste0("M: Excluding tag(s) ", paste(exclude.tags[logical_link], collapse = ", "), " from the analysis per used command (detections removed: ", paste(unlist(lapply(input[link], nrow)), collapse = ", "), ", respectively)."))
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