#' Load, structure and check the inputs
#' 
#' @inheritParams explore
#' @inheritParams migration
#' 
#' @return a list of objects to be used for data analysis
#' 
#' @keywords internal
#' 
loadStudyData <- function(tz, override = NULL, start.time, stop.time, 
  sections = NULL, exclude.tags, disregard.parallels = TRUE) {
  appendTo(c("Screen", "Report"), "M: Importing data. This process may take a while.")
  bio <- loadBio(file = "biometrics.csv", tz = tz)
  appendTo(c("Screen", "Report"), paste0("M: Number of target tags: ", nrow(bio), "."))
  
  # Check that all the overriden fish are part of the study
  if (!is.null(override) && any(link <- is.na(match(unlist(lapply(strsplit(override, "-"), function(x) tail(x, 1))), bio$Signal))))
    stop("Some tag signals listed in 'override' ('", paste0(override[link], collapse = "', '"), "') are not listed in the biometrics file.\n")
  deployments <- loadDeployments(file = "deployments.csv", tz = tz)
  checkDeploymentTimes(input = deployments) # check that receivers are not deployed before being retrieved
  spatial <- loadSpatial(file = "spatial.csv", report = TRUE)
  deployments <- checkDeploymentStations(input = deployments, spatial = spatial) # match Station.name in the deployments to Station.name in spatial, and vice-versa
  deployments <- createUniqueSerials(input = deployments) # Prepare serial numbers to overwrite the serials in detections

  detections <- loadDetections(start.time = start.time, stop.time = stop.time, tz = tz)
  detections <- checkDupDetections(input = detections)
  detections <- createStandards(detections = detections, spatial = spatial, deployments = deployments) # get standardized station and receiver names, check for receivers with no detections
  appendTo(c("Screen","Report"), paste0("M: Data time range: ", as.character(head(detections$Timestamp, 1)), " to ", as.character(tail(detections$Timestamp, 1)), " (", tz, ")."))
  checkUnknownReceivers(input = detections) # Check if there are detections from unknown detections

  use.fakedot <- TRUE
  if (file.exists("spatial.dot")) {
    appendTo(c("Screen", "Report"), "M: A 'spatial.dot' file was detected, activating multi-branch analysis.")
    recipient <- loadDot(input = "spatial.dot", spatial = spatial, sections = sections, disregard.parallels = disregard.parallels)
    use.fakedot <- FALSE
  } 
  if (use.fakedot & file.exists("spatial.txt")) {
    appendTo(c("Screen", "Report"), "M: A 'spatial.txt' file was detected, activating multi-branch analysis.")
    recipient <- loadDot(input = "spatial.txt", spatial = spatial, sections = sections, disregard.parallels = disregard.parallels)
    use.fakedot <- FALSE
  }
  if (use.fakedot) {
    fakedot <- paste(unique(spatial$Array), collapse = "--")
    recipient <- loadDot(string = fakedot, spatial = spatial, sections = sections, disregard.parallels = disregard.parallels)
  }
  dot <- recipient$dot
  arrays <- recipient$arrays
  dotmat <- recipient$dotmat
  paths <- recipient$paths
  if (is.null(dot) | is.null(arrays) | is.null(dotmat) | is.null(paths))
    stop("Something went wrong when assigning recipient objects (loadDot). If this error persists, contact the developer.")

  rm(use.fakedot, recipient)

  # Check if there is a logical first array in the study area, should a replacement release site need to be created.
  if (sum(unlist(lapply(arrays, function(a) is.null(a$before)))) == 1)
    first.array <- names(arrays)[unlist(lapply(arrays, function(a) is.null(a$before)))]
  else
    first.array <- NULL
  spatial <- transformSpatial(spatial = spatial, bio = bio, sections = sections, arrays = arrays, first.array = first.array) # Finish structuring the spatial file
  detections$Array <- factor(detections$Array, levels = unlist(spatial$array.order)) # Fix array levels
  link <- match(names(arrays), unlist(spatial$array.order))
  arrays <- arrays[link] # Reorder arrays by spatial order
  rm(link)
  
  recipient <- loadDistances(spatial = spatial) # Load distances and check if they are valid
  dist.mat <- recipient$dist.mat
  invalid.dist <- recipient$invalid.dist
  if (is.null(dist.mat) | is.null(invalid.dist))
    stop("Something went wrong when assigning recipient objects (loadDistances). If this error persists, contact the developer.\n", call. = FALSE)
  rm(recipient)

  recipient <- splitDetections(detections = detections, bio = bio, exclude.tags = exclude.tags) # Split the detections by tag, store full transmitter names in bio
  detections.list <- recipient$detections.list
  bio <- recipient$bio
  if (is.null(detections.list) | is.null(bio))
    stop("Something went wrong when assigning recipient objects (splitDetections). If this error persists, contact the developer.\n", call. = FALSE)
  rm(recipient)

  recipient <- checkTagsInUnknownReceivers(detections.list = detections.list, deployments = deployments, spatial = spatial) # Check if there is any data loss due to unknown receivers
  spatial <- recipient$spatial
  deployments <- recipient$deployments
  detections.list <- recipient$detections.list
  if (is.null(spatial) | is.null(deployments) | is.null(detections.list))
    stop("Something went wrong when assigning recipient objects (unknownReceivers). If this error persists, contact the developer.\n", call. = FALSE)
  rm(recipient)

  detections.list <- checkDetectionsBeforeRelease(input = detections.list, bio = bio)
  appendTo(c("Screen", "Report"), "M: Data successfully imported!")
  return(list(bio = bio, deployments = deployments, spatial = spatial, dot = dot,
   arrays = arrays, dotmat = dotmat, detections = detections, dist.mat = dist.mat,
    invalid.dist = invalid.dist, detections.list = detections.list, paths = paths))
}

#' Load spatial.dot
#' 
#' @param input The name of the dot file
#' @param spatial A spatial data frame
#' @param string A dot string
#' @inheritParams migration
#' 
#' @return The dot list
#' 
#' @keywords internal
#' 
loadDot <- function(string = NULL, input = NULL, spatial, sections = NULL, disregard.parallels) {
  if (is.null(string) & is.null(input))
    stop("No dot file or dot string were specified.")
  if (is.null(string)) {
    tryCatch(dot <- readDot(input = input), 
      error = function(e) {
        emergencyBreak()
        stop("The contents of the '", input, "' file could not be recognised by the readDot function.\n", call. = FALSE)
        })
  } else {
    dot <- readDot(string = string)
  }
  mat <- dotMatrix(input = dot)
  if (any(is.na(match(unique(spatial$Array), colnames(mat))))) {
    emergencyBreak()
    if (file.exists("spatial.txt"))
      stop("Not all the arrays listed in the spatial.csv file are present in the spatial.txt.\n", call. = FALSE)
    if (file.exists("spatial.dot"))
      stop("Not all the arrays listed in the spatial.csv file are present in the spatial.dot.\n", call. = FALSE)
    else
      stop("Something went wrong when compiling the dot file. Try restarting R and trying again. If the problem persists, contact the developer.\n", call. = FALSE)
  }
  arrays <- dotList(input = dot, sections = sections)
  arrays <- dotPaths(input = arrays, dotmat = mat, disregard.parallels = disregard.parallels)
  shortest.paths <- findShortestChains(input = arrays)
  return(list(dot = dot, arrays = arrays, dotmat = mat, paths = shortest.paths))
}

#' Read dot file
#' 
#' @inheritParams loadDot
#' 
#' @return A table with A to B rows
#' 
#' @export
#' 
readDot <- function (input = NULL, string = NULL) {
  if (is.null(string) & is.null(input))
    stop("No dot file or data were specified.")
  if (is.null(string)) {
    if (!file.exists(input))
      stop("Could not find a '", input, "' file in the working directory.\n", call. = FALSE)
    lines <- readLines(input)
  } else {
    lines <- unlist(strsplit(string, "\n|\t"))
  }
  paths <- lines[grepl("[<-][->]", lines)]
  if (length(paths) == 0)
    stop("Could not recognise the input contents as DOT formatted connections.\n", call. = FALSE)
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
    type <- gsub(paste0(nodes[[i]], collapse = "|"), "", paths[[i]])
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
#' @return a matrix of distances between arrays
#' 
#' @keywords internal
#' 
dotMatrix <- function(input) {
  nodes <- unique(unlist(input[, c(1, 3)]))
  graph <- matrix(0, length(nodes), length(nodes), dimnames = list(nodes, nodes))
  if (any(input$to != "--" & input$to != "<-" & input$to != "->"))
    stop("Unrecognized connectors. Only use '--', '->' or '<-' to connect nodes.\n")
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
#' @inheritParams migration
#' 
#' @return A list containing, for each array, the arrays that connect to it and to which it connects.
#' 
#' @keywords internal
#' 
dotList <- function(input, sections = NULL) {
  if (!is.null(sections)) {
    input$SectionA <- rep(NA_character_, nrow(input))
    input$SectionB <- rep(NA_character_, nrow(input))
    for (section in sections) {
       input$SectionA[grepl(section, input$A)] <- section
       input$SectionB[grepl(section, input$B)] <- section
    }
    input$Edge <- with(input, SectionA != SectionB)
  }

  arrays <- list()
  for (a in unique(unlist(input[,c(1, 3)]))) {
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
#' @return The input list, with an extra element for each array with valid efficiency peers
#' 
#' @keywords internal
#' 
dotPaths <- function(input, dotmat, disregard.parallels) {
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
#' @return The array list with efficiency peers
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
          # prepare to check paths leading to B
          if (disregard.parallels)
            check.paths.to.b <- all(!is.na(match(input[[b]][[opposite]], c(a, input[[a]]$parallel))))
          else
            check.paths.to.b <- length(input[[b]][[opposite]]) == 1
          # IF there are no third-party paths leading to B
          if (check.paths.to.b) {
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
#' @return The array list with all linked arrays
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
#' @return The array list with all paths between arrays with distance > 1
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
#' @return A data frame with the same information as the input plus the Standard names.
#' 
#' @keywords internal
#' 
setSpatialStandards <- function(input){
  appendTo("debug","Running setSpatialStandards")
  input$Standard.name <- as.character(input$Station.name)
  input$Standard.name <- gsub(" ", "", input$Standard.name)
  link <- input$Type == "Hydrophone"
  input$Standard.name[link] <- paste0("St.", seq_len(sum(input$Type == "Hydrophone")))
  write.csv(input, "spatial.csv", row.names = FALSE)
  return(input)
}

#' Load distances matrix
#' 
#' @param spatial A list of spatial objects in the study area
#' 
#' @keywords internal 
#' 
#' @return a list containing the distances matrix and a TRUE/FALSE value indicating whether or not that distances matrix is valid for the target study area.
#' 
loadDistances <- function(spatial) {
  # Check for distances
  invalid.dist <- TRUE
  appendTo("Debug", "Creating 'dist.mat' if a distances.csv file is present.")
  if (file.exists("distances.csv")) {
    appendTo(c("Screen", "Report"), "M: A distances.csv file is present, activating speed calculations.")
    dist.mat <- read.csv("distances.csv", row.names = 1)
    if (ncol(dist.mat) == 1)
      warning("Only one column was identified in 'distances.csv'. If this seems wrong, please make sure that the values are separated using commas.", immediate. = TRUE, call. = FALSE)
    rownames(dist.mat) <- gsub(" ", "", rownames(dist.mat))
    colnames(dist.mat) <- gsub(" ", "", colnames(dist.mat))
    invalid.dist <- FALSE
    if (nrow(dist.mat) != ncol(dist.mat)){
      appendTo(c("Screen", "Report", "Warning"), "The distance matrix appears to be missing data (ncol != nrow). Deactivating speed calculation to avoid function failure.")
      invalid.dist <- TRUE
    }
    if (!invalid.dist && any(link <- is.na(match(rownames(dist.mat), colnames(dist.mat))))) {
      appendTo(c("Screen", "Report", "Warning"), "The column and row names in the distance matrix do not match each other. Deactivating speed calculation to avoid function failure.")
      message(paste0("   Row names missing in the columns: '", paste(rownames(dist.mat)[link], collapse = "', '"), "'."))
      if (any(link <- is.na(match(colnames(dist.mat), rownames(dist.mat))))) 
        message(paste0("   Column names missing in the rows: '", paste(colnames(dist.mat)[link], collapse = "', '"), "'."))
      invalid.dist <- TRUE
    }
    if (!invalid.dist && sum(nrow(spatial$stations), nrow(spatial$release.sites)) != nrow(dist.mat)) {
      appendTo(c("Screen", "Report", "Warning"), "The number of spatial points does not match the number of rows in the distance matrix. Deactivating speed calculation to avoid function failure.")
      message("   Number of stations and release sites listed: ", sum(nrow(spatial$stations), nrow(spatial$release.sites)))
      message("   Number of rows/columns in the distance matrix: ", nrow(dist.mat))
      invalid.dist <- TRUE
    }
    if (!invalid.dist && (any(!matchl(spatial$stations$Standard.name, colnames(dist.mat))) | any(!matchl(spatial$release.sites$Standard.name, colnames(dist.mat))))) {
      appendTo(c("Screen", "Report", "Warning"), "Some stations and/or release sites are not present in the distance matrix. Deactivating speed calculation to avoid function failure.")
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
  return(list(dist.mat = dist.mat, invalid.dist = invalid.dist))  
}


#' Load deployments file and Check the structure
#' 
#' @param file an input file with spatial data.
#' 
#' @return The deployments dataframe
#' 
#' @keywords internal
#' 
loadDeployments <- function(file, tz){
  appendTo("debug","Running loadDeployments.")
  if (file.exists(file))
    input <- as.data.frame(data.table::fread(file))
  else {
    emergencyBreak()
    stop("Could not find a '", file, "' file in the working directory.\n", call. = FALSE)
  }
  if (!is.na(link <- match("Station.Name", colnames(input))))
    colnames(input)[link] <- "Station.name"
  if (any(link <- duplicated(colnames(input))))
    stop("The following columns are duplicated in the file '", file, "': '", paste(unique(colnames(input)[link]), sep = "', '"), "'.", call. = FALSE)
  default.cols <- c("Receiver", "Station.name", "Start", "Stop")
  link <- match(default.cols, colnames(input))
  if (any(is.na(link))) {
    appendTo("Report", paste0("Error: Column", 
      ifelse(sum(is.na(link)) > 1, "(s) '", " '"),
      paste(default.cols[is.na(link)], collapse = "', '"), 
      ifelse(sum(is.na(link)) > 1, "' are", "' is"),
      " missing in the ", file, " file."))
    emergencyBreak()
    stop(paste0("Column", 
      ifelse(sum(is.na(link)) > 1, "(s) '", " '"),
      paste(default.cols[is.na(link)], collapse = "', '"), 
      ifelse(sum(is.na(link)) > 1, "' are", "' is"),
      " missing in the ", file, " file."), call. = FALSE)
  }
  if (any(!grepl("^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9]", input$Start))) {
    emergencyBreak()
    stop("Not all values in the 'Start' column appear to be in a 'yyyy-mm-dd hh:mm' format (seconds are optional). Please double-check the ", file, " file.\n", call. = FALSE)
  }
  if (any(!grepl("^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9]", input$Stop))) {
    emergencyBreak()
    stop("Not all values in the 'Stop' column appear to be in a 'yyyy-mm-dd hh:mm' format (seconds are optional). Please double-check the ", file, " file.\n", call. = FALSE)
  }
  if (inherits(try(as.POSIXct(input$Start), silent = TRUE), "try-error")){
    emergencyBreak()
    stop("Could not recognise the data in the 'Start' column as POSIX-compatible timestamps. Please double-check the ", file," file.\n", call. = FALSE)
  }
  if (inherits(try(as.POSIXct(input$Stop), silent = TRUE),"try-error")){
    emergencyBreak()
    stop("Could not recognise the data in the 'Stop' column as POSIX-compatible timestamps. Please double-check the ", file, " file.\n", call. = FALSE)
  }
  input$Receiver <- as.character(input$Receiver)
  input$Receiver <- sapply(input$Receiver, function(x) tail(unlist(strsplit(x, "-")), 1))
  input$Start <- as.POSIXct(input$Start, tz = tz)
  input$Stop <- as.POSIXct(input$Stop, tz = tz)
  return(input)
}

#' Load Spatial File
#' 
#' @param file an input file with spatial data.
#' @param report Logical: If TRUE, the appendTo function is enabled.
#' 
#' @return The spatial dataframe
#' 
#' @export
#' 
loadSpatial <- function(file = "spatial.csv", report = FALSE){
  if (report)
    appendTo("debug", "Running loadSpatial.")
  if (file.exists(file))
    input <- as.data.frame(data.table::fread(file))
  else {
    emergencyBreak()
    stop("Could not find a '", file, "' file in the working directory.\n", call. = FALSE)
  }
  if (any(link <- duplicated(colnames(input))))
    stop("The following columns are duplicated in the file 'spatial.csv': '", paste(unique(colnames(input)[link]), sep = "', '"), "'.", call. = FALSE)
  if (!is.na(link <- match("Station.Name", colnames(input))))
    colnames(input)[link] <- "Station.name"
  if (!any(grepl("Station.name", colnames(input)))) {
    emergencyBreak()
    stop("The ", file, " file must contain a 'Station.name' column.\n", call. = FALSE)
  } else {
    if (any(link <- table(input$Station.name) > 1)) {
      if (report)
        emergencyBreak()
      stop("The 'Station.name' column in the ", file, " file must not have duplicated values.\nStations appearing more than once: ", paste(names(table(input$Station.name))[link], collapse = ", "), "\n", call. = FALSE)
    }
  }
  if (!any(grepl("Array", colnames(input)))) {
    if (report)
      emergencyBreak()
    stop("The ", file, " file must contain an 'Array' column.\n", call. = FALSE)
  }
  if (any(is.na(input$Array)))
    stop("Some rows do not contain 'Array' information in the ",file, " file. Please double-check the input files.")
  if (any(grepl(" ", input$Array))) {
    appendTo("Screen", "M: Replacing spaces in array names to prevent function failure.")
    input$Array <- gsub(" ", "_", input$Array)
  }
  if (!any(grepl("Type", colnames(input)))) {
    if (report)
      appendTo(c("Screen", "Report"), paste0("M: No 'Type' column found in the ", file, " file. Assigning all rows as hydrophones."))
    else
      message("M: No 'Type' column found in the ", file, " file. Assigning all rows as hydrophones.")
    input$Type <- "Hydrophone"
  } else {
    if (any(is.na(match(unique(input$Type), c("Hydrophone","Release"))))){
      if (report)
        emergencyBreak()
      stop("Could not recognise the data in the 'Type' column as only one of 'Hydrophone' or 'Release'. Please double-check the ", file," file.\n", call. = FALSE)
    }
  }
  input <- setSpatialStandards(input = input) # Create Standard.name for each station  
  return(input)
}



#' Load Biometrics file
#' 
#' @param file an input file with biometric data.
#' @inheritParams explore
#' 
#' @keywords internal
#' 
#' @return The biometrics table
#' 
#' @export
#' 
loadBio <- function(file, tz){
  appendTo("debug", "Running loadBio.")
  if (missing(file))
    stop("'file' is missing.")
  if (missing(tz))
    stop("'tz' is missing.")
  if (file.exists(file))
    bio <- as.data.frame(data.table::fread(file), stringsAsFactors = FALSE)
  else {
    emergencyBreak()
    stop("Could not find a '", file, "' file in the working directory.\n", call. = FALSE)
  }  
  if (any(link <- duplicated(colnames(bio))))
    stop("The following columns are duplicated in the file 'biometrics.csv': '", paste(unique(colnames(bio)[link]), sep = "', '"), "'.\n", call. = FALSE)

  if (!any(grepl("Release.date", colnames(bio)))) {
    emergencyBreak()
    stop("The biometrics.csv file must contain an 'Release.date' column.\n", call. = FALSE)
  }

  if (any(!grepl("^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9]", bio$Release.date))) {
    emergencyBreak()
    stop("Not all values in the 'Release.date' column appear to be in a 'yyyy-mm-dd hh:mm' format (seconds are optional). Please double-check the biometrics.csv file.\n", call. = FALSE)
  }
  
  if (inherits(try(bio$Release.date <- as.POSIXct(bio$Release.date, tz = tz), silent = TRUE),"try-error")){
    emergencyBreak()
    stop("Could not recognise the data in the 'Release.date' column as POSIX-compatible timestamps. Please double-check the biometrics.csv file.\n", call. = FALSE)
  }

  if (!any(grepl("Signal", colnames(bio)))){
    emergencyBreak()
    stop("The biometrics.csv file must contain an 'Signal' column.\n", call. = FALSE)
  }

  if (!inherits(bio$Signal,"integer")) {
    emergencyBreak()
    stop("Could not recognise the data in the 'Signal' column as integers. Please double-check the biometrics.csv file.\n", call. = FALSE)
  }

  if (any(is.na(bio$Signal))) {
    emergencyBreak()
    stop("Some fish have no 'Signal' information. Please double-check the biometrics.csv file.\n", call. = FALSE)
  }

  if (any(link <- table(bio$Signal) > 1)) {
    emergencyBreak()
    stop(ifelse(sum(link) > 1, "Signals ", "Signal "), paste(names(table(bio$Signal))[link], collapse = ", "), ifelse(sum(link) > 1," are ", " is "), "duplicated in the biometrics.csv file.\n", call. = FALSE)
  }

  if (!any(grepl("Release.site", colnames(bio)))) {
    appendTo("Screen", "M: No Release site has been indicated in the biometrics.csv file. Creating a 'Release.site' column to avoid function failure. Filling with 'unspecified'.")
    bio$Release.site <- "unspecified"
  } else {
    bio$Release.site <-  gsub(" ", "", bio$Release.site)
    bio$Release.site <- factor(bio$Release.site)
    if (any(is.na(bio$Release.site) | bio$Release.site == "")) {
      appendTo(c("Screen","Report","Warning"),"Some fish contain no release site information. You may want to double-check the data.\n   Filling the blanks with 'unspecified'.")
      levels(bio$Release.site) <- c(levels(bio$Release.site), "unspecified")
      bio$Release.site[is.na(bio$Release.site)] <- "unspecified"
    }
  }
  if (!any(grepl("Group", colnames(bio)))) {
    appendTo("Screen", "M: No 'Group' column found in the biometrics.csv file. Assigning all fish to group 'All'.")
    bio$Group <- "All"
  } else {
    bio$Group <- factor(bio$Group)
    if (any(is.na(bio$Group) | bio$Group == "")) {
      appendTo(c("Screen", "Report", "Warning"),"Some fish contain no group information. You may want to double-check the data.\n   Filling the blanks with 'unspecified'.")
      levels(bio$Group) <- c(levels(bio$Group), "unspecified")
      bio$Group[is.na(bio$Group) | bio$Group == ""] <- "unspecified"
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
      appendTo(c("Screen", "Report"), "M: Some fish groups contain one or more '.' characters. To avoid function failure, these will be replaced with '_'.")
      levels(bio$Group) <- gsub("\\.", "_", levels(bio$Group))
    }
  }
  bio <- bio[order(bio$Signal),]
  return(bio)
}

#' Load ALS detections
#'
#' If there are previously compiled detections present, offers the chance to reuse. Otherwise triggers combineDetections.
#' 
#' @inheritParams explore
#' 
#' @return A dataframe with all the detections
#' 
#' @keywords internal
#' 
loadDetections <- function(start.time = NULL, stop.time = NULL, tz, force = FALSE) {
  # NOTE: The variable actel.detections is loaded from a RData file, if present. To avoid package check
  #   notes, the variable name is created before any use.
  actel.detections <- NULL
    
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
    } else {
      appendTo("Screen", paste0("M: The detections have been processed on ", actel.detections$timestamp, ".\n   If the input detection files were not changed, it is safe to use these again."))
      decision <- readline("   Reuse processed detections?(Y/n) ")
    }
    appendTo("UD", decision)
    if (decision != "N" & decision != "n"){
      appendTo(c("Screen","Report"), paste0("M: Using detections previously compiled on ", actel.detections$timestamp, "..."))
      detections <- actel.detections$detections
      attributes(detections$Timestamp)$tzone <- "UTC"
      detections <- convertTimes(input = detections, start.time = start.time, stop.time = stop.time, tz = tz)
      recompile <- FALSE
    } else {
      appendTo("Screen", "M: Reprocessing the detections.")
    }
    rm(actel.detections)
  }

  if (recompile)
    detections <- compileDetections(path = "detections", start.time = start.time, 
      stop.time = stop.time, tz = tz)
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
#' @return A dataframe with all the detections
#' 
#' @keywords internal
#' 
compileDetections <- function(path = "detections", start.time = NULL, stop.time = NULL, tz) {
  appendTo("Screen", "M: Compiling detections...")
  # Find the detection files
  if (file_test("-d", path)) {
    file.list <- findFiles(path = path, pattern = "*.csv")
  } else {
    if (file.exists("detections.csv"))
      file.list <- "detections.csv"
    else
      stop("Could not find a 'detections' folder nor a 'detections.csv' file.\n", call. = FALSE)
  }
  if (file_test("-d", path) & file.exists("detections.csv"))
    appendTo(c("Screen", "Warning", "Report"), "Both a 'detections' folder and a 'detections.csv' file are present in the current directory.\n   Loading ONLY the files present in the 'detections' folder.")
  # Prepare the detection files
  data.files <- lapply(file.list, function(i) {
    appendTo("debug", paste0("Importing file '", i, "'."))
    aux <- data.table::fread(i, fill = TRUE, sep = ",", showProgress = FALSE)
    if(nrow(aux) > 0){
      unknown.file <- TRUE
      if (unknown.file && any(grepl("CodeType", colnames(aux)))) {
        appendTo("debug", paste0("File '", i, "' matches a Thelma log."))
        output <- processThelmaFile(input = aux)
        unknown.file <- FALSE
      }
      if (unknown.file && any(grepl("Receiver", colnames(aux)))) {
        appendTo("debug", paste0("File '", i, "' matches a Vemco log."))
        output <- processVemcoFile(input = aux)
        unknown.file <- FALSE
      }
      if (unknown.file) {
        appendTo(c("Screen", "Report", "Warning"), 
          paste0("File '", i, "' does not match to any of the supported hydrophone file formats!\n   If your file corresponds to a hydrophone log and actel did not recognize it, please get in contact through www.github.com/hugomflavio/actel/issues/new"))
        return(NULL)
      }
      return(output)
    } else {
      appendTo(c("Screen", "Report"), paste0("File '", i, "' is empty, skipping processing."))
      return(NULL)
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
    stop("No valid detection files were found.\n", call. = FALSE)

  # Bind the detection files
  appendTo("debug", "Binding detections.")
  output <- data.table::rbindlist(data.files)
  output$Receiver <- as.factor(output$Receiver)
  output$CodeSpace <- as.factor(output$CodeSpace)
  # Convert codespaces
  output <- convertCodes(input = output)
  # Convert time-zones
  output <- convertTimes(input = output, start.time = start.time, 
    stop.time = stop.time, tz = tz)

  actel.detections <- list(detections = output, timestamp = Sys.time())
  save(actel.detections, file = ifelse(file_test("-d", path), paste0(path, "/actel.detections.RData"), "actel.detections.RData"))
  
  return(output)
}

#' Find file names
#'
#' @inheritParams loadDetections
#' 
#' @return A vector of the file names.
#'
#' @keywords internal
#' 
findFiles <- function(path = NULL, pattern = NULL) {
  appendTo("debug", "Running findFiles.")
  if (is.null(path)) {
      file.list <- list.files(pattern = pattern)
  } else {
    file.list <- NULL
    for (folder in path) {
      if (file_test("-d", folder)) {
        file.list <- c(file.list, paste0(folder, "/", list.files(folder, pattern = pattern)))
      } else {
        stop("Could not find a '", folder, "' directory in the current working directory.\n", call. = FALSE)
      }
    }
  }
  return(file.list)
}

#' Thelma files
#' 
#' Processes Thelma ALS files.
#' 
#' @param input the file name, supplied by findFiles.
#'
#' @return A data frame of standardized detections from the input file.
#'
#' @keywords internal
#' 
processThelmaFile <- function(input) {
  appendTo("debug", "Running processThelmaFile.")
  input <- as.data.frame(input)
  output <- data.table(
    Timestamp = fasttime::fastPOSIXct(sapply(input[, 1], function(x) gsub("Z", "", gsub("T", " ", x))), tz = "UTC"),
    Receiver = input[, 8],
    CodeSpace = input[, 3],
    Signal = input[, 4])
  return(output)
}

#' Vemco files
#' 
#' Processes Vemco ALS files
#' 
#' @inheritParams processThelmaFile
#'
#' @return A data frame of standardized detections from the input file.
#'
#' @keywords internal
#' 
processVemcoFile <- function(input) {
  appendTo("Debug", "Running processVemcoFile.")
  appendTo("Debug", "Processing data inside the file...")
  transmitter_aux <- strsplit(input$Transmitter, "-", fixed = TRUE)
  receiver_aux <- strsplit(input$Receiver, "-", fixed = TRUE) # split model and serial
  input[, "CodeSpace"] <- unlist(lapply(transmitter_aux, function(x) paste(x[1:2], collapse = "-"))) # Rejoin code space
  input[, "Signal"] <- unlist(lapply(transmitter_aux, function(x) x[3])) # extract only signal
  input[, "Receiver"] <- unlist(lapply(receiver_aux, function(x) x[2])) # extract only the serial
  appendTo("Debug", "Done!")
  colnames(input)[1] <- c("Timestamp")
  input <- input[, c("Timestamp", "Receiver", "CodeSpace", "Signal")]
  input$Timestamp <- fasttime::fastPOSIXct(input$Timestamp, tz = "UTC")
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
  input$Transmitter <- as.factor(paste(input$CodeSpace, input$Signal, sep = "-"))
  return(input)
}

#' Include the deployment in the serial number of the receive
#' 
#' @param input A data frame with the deployments
#' 
#' @return A list of deployments, with unique serial numbers per deployment
#' 
#' @keywords internal
#' 
createUniqueSerials <- function(input) {
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
#' @param bio A table with the tags and biometrics of the studied fish.
#' @param detections A data frame with all the detections. Supplied by loadDetections.
#' @param silent logical: if TRUE, suppress messages.
#' 
#' @return A list of detections for each tag.
#' 
#' @keywords internal
#' 
splitDetections <- function(detections, bio, exclude.tags = NULL, silent = FALSE) {
  appendTo("debug", "Running splitDetections.")
  my.list <- split(detections, detections$Transmitter)
  my.list <- excludeTags(input = my.list, exclude.tags = exclude.tags, silent = silent)
  
  tags <- checkNoDetections(input = my.list, bio = bio)
  checkDupSignals(input = my.list, bio = bio, tag.list = tags$list)
  
  appendTo("debug", "Debug: Creating 'trimmed.list'.")
  bio$Transmitter <- names(my.list)[tags[["link"]]]
  trimmed.list <- my.list[tags[["link"]]]
  non.empty <- unlist(lapply(trimmed.list, function(x) length(x) != 0))
  trimmed.list <- trimmed.list[non.empty]
  if (!silent){
    collectStrays(input = my.list[-na.exclude(tags[["link"]])])
    storeStrays()
  }

  return(list(detections.list = trimmed.list, bio = bio))
}

#' Collect summary information on the tags detected but that are not part of the study.
#'
#' @param input list of detections for the tags to be excluded.
#' @param restart logical: if TRUE, remove file 'temp_strays.csv' from the working directory.
#' 
#' @keywords internal
#' 
collectStrays <- function(input, restart = FALSE){
  appendTo("debug", "Running collectStrays.")
  if (restart && file.exists("temp_strays.csv")) {
    file.remove("temp_strays.csv")
  }
  if (length(input) > 0) {
    recipient <- data.frame(Transmitter = names(input), 
      N.detections = unlist(lapply(input,nrow)), 
      First.detection = unlist(lapply(input, function(x) as.character(head(x$Timestamp,1)))),
      Last.detection = unlist(lapply(input, function(x) as.character(tail(x$Timestamp,1)))),
      Receivers = unlist(lapply(input, function(x) paste(unique(x$Receiver), collapse = ", ")))
      )
    write.table(recipient, file = "temp_strays.csv", sep = ",", append = file.exists("temp_strays.csv"), row.names = FALSE, col.names = !file.exists("temp_strays.csv"))
  }
}

#' Store summary information on the stray tags detected in a permanent file.
#'
#' @keywords internal
#'
storeStrays <- function(){
  appendTo("debug", "Running storeStrays.")
  if (file.exists("temp_strays.csv")) {
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
    file.rename("temp_strays.csv", newname)
  }
}

#' Temporarily include unknown receivers with target detections in the study area
#' 
#' @param detections.list a list of the detections for each target tag
#' 
#' @return The detections.list with the unknown receivers labelled
#' 
#' @keywords internal
#' 
labelUnknowns <- function(detections.list) {
  if (any(unlist(lapply(detections.list, function(x) any(is.na(x$Array)))))) {
    tags <- NULL
    receivers <- NULL
    lapply(detections.list, function(x) {
      if (any(is.na(x$Array))) {
        tags <- c(tags, unique(x$Transmitter))
        receivers <- c(receivers, unique(x$Transmitter[is.na(x$Array)]))
      }})
    appendTo(c("Screen", "Report", "Warning"), paste0("Fish ", paste(tags, collapse = ", ") , ifelse(length(tags) > 1, " were", " was"), " detected in one or more receivers that are not listed in the study area (receiver(s): ", paste(unique(receivers), collapse = ", "), ")!"))
    message("Possible options:\n   a) Stop and double-check the data (recommended)\n   b) Temporary include the unknown hydrophone(s) in the analysis")
    check <- TRUE
    while (check) {
      decision <- readline("Which option should be followed?(a/b) ")
      if (decision == "a" | decision == "A" | decision == "b" | decision == "B") 
        check <- FALSE 
      else 
        message("Option not recognized, please try again."); flush.console()
      appendTo("UD", decision)
    }
    if (decision == "a" | decision == "A") {
      emergencyBreak()
      stop("Stopping analysis per user command.\n", call. = FALSE)
    }
    detections.list <- lapply(detections.list, function(x) {
      levels(x$Standard.name) <- c(levels(x$Standard.name), "Unknown")
      x$Standard.name[is.na(x$Standard.name)] <- "Unknown"
      levels(x$Array) <- c(levels(x$Array), "Unknown")
      x$Array[is.na(x$Array)] <- "Unknown"
      return(x)
    })
  }
  return(detections.list)
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
createStandards <- function(detections, spatial, deployments) {
  detections$Receiver <- as.character(detections$Receiver)
  detections$Standard.name <- NA_character_
  detections$Array <- NA_character_
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
        the.station <- match(deployments[[i]]$Station.name[j], spatial$Station.name)
        # include Standard.name
        detections$Standard.name[receiver.link][deployment.link] <- spatial$Standard.name[the.station]
        # include Array
        detections$Array[receiver.link][deployment.link] <- as.character(spatial$Array[the.station])
      }
      if (any(the.error <- is.na(detections$Standard.name[receiver.link]))) {
        appendTo(c("Screen", "Report"), paste0("Error: ", sum(the.error), " detections for receiver ", names(deployments)[i], " do not fall within deployment periods."))
        message("")
        print(detections[receiver.link][the.error, -c(6, 7)])
        message("")
        message("Possible options:\n   a) Stop and double-check the data (recommended)\n   b) Discard orphan detections.")
        check <- TRUE
        while (check) {
          decision <- readline("Which option should be followed?(a/b) ")
          if (decision == "a" | decision == "A" | decision == "b" | decision == "B") 
            check <- FALSE 
          else 
            message("Option not recognized, please try again."); flush.console()
          appendTo("UD", decision)
        }
        if (decision == "a" | decision == "A") {
          emergencyBreak()
          stop("Stopping analysis per user command.\n", call. = FALSE)
        } else {
          rows.to.remove <- detections[receiver.link, which = TRUE][the.error]
          detections <- detections[-rows.to.remove]
        }
       }
    }
  }
  appendTo(c("Screen", "Report"), paste0("M: Number of ALS: ", length(deployments), " (of which ", length(empty.receivers), " had no detections)"))
  if (!is.null(empty.receivers))
    appendTo(c("Screen", "Report", "Warning"), paste0("No detections were found for receiver(s) ", paste0(empty.receivers, collapse = ", "), "."))
  aux <- spatial[spatial$Type == "Hydrophone", ]
  detections$Receiver <- as.factor(detections$Receiver)
  detections$Array <- factor(detections$Array, levels = unique(aux$Array))
  detections$Standard.name <- factor(detections$Standard.name, levels = aux$Standard.name)
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
#' @return A list of 1) stations, 2) release sites, 3) ALS columns in the spatial file, 4) the Number of ASL, 5) The ALS serial numbers and 6) the array order.
#' 
#' @keywords internal
#' 
transformSpatial <- function(spatial, bio, arrays, sections = NULL, first.array = NULL) {
  appendTo("debug", "Running transformSpatial.")
  # Break the stations away
  appendTo("debug", "Creating 'stations'.")
  stations <- spatial[spatial$Type == "Hydrophone", -match("Type", colnames(spatial))]
  stations$Array <- factor(stations$Array, levels = unique(stations$Array))
  appendTo("debug", "Creating 'release.sites'.")
  # If there is any release site in the spatial file
  if (sum(spatial$Type == "Release") > 0) {
    if (length(unique(bio$Release.site)) == 1 && unique(bio$Release.site) == "unspecified") {
      appendTo(c("Screen", "Report", "Warning"), "At least one release site has been indicated in the spatial.csv file, but no release sites were specified in the biometrics file.\n   Discarding release site information and assuming all fish were released at the top level array to avoid function failure.\n   Please double-check your data.")
      if (is.null(first.array)) {
        emergencyBreak()
        stop("There is more than one top level array in the study area. Please specify release site(s) in the 'spatial.csv' file and in the 'biometrics.csv' file.\n", call. = FALSE)
      }
      release.sites <- data.frame(Station.name = "unspecified", 
                                  Longitude = NA_real_, 
                                  Latitude = NA_real_, 
                                  Array = first.array,
                                  Standard.name = "unspecified",
                                  n = nrow(bio))
    } else {
      A <- spatial$Standard.name[spatial$Type == "Release"]
      B <- unique(bio$Release.site)
      if (any(is.na(match(B, A)))) {
        appendTo(c("Screen", "Report", "Warning"), "There is a mismatch between the release sites reported and the release locations for the fish.")
        message("   Release sites listed in the spatial.csv file:", paste(A, collapse = ", "))
        message("   Sites listed in the biometrics.csv file 'Release.site' column:", paste(B, collapse = ", "))
        emergencyBreak()
        stop("The release names should be identical in the spatial.csv file and in the biometrics.csv file.\n", call. = FALSE)
      } else {
        from.row <- spatial$Type == "Release"
        from.col <- colnames(spatial)[!grepl("Receiver", colnames(spatial))]
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
      A <- unique(stations$Array)
      B <- unique(release.sites$Array)
      if (any(is.na(match(B, A)))) {
        appendTo(c("Screen", "Report", "Warning"), "There is a mismatch between the expected first array of a release site and the list of arrays.")
        message("   Arrays listed in the spatial.csv file:", paste(A, collapse = ", "))
        message("   Expected first arrays of the release sites:", paste(B, collapse = ", "))
        emergencyBreak()
        stop("The expected first arrays should match the arrays where stations where deployed in the spatial.csv file.\n", call. = FALSE)
      }
    }
  } else {
    if (length(unique(bio$Release.site)) > 1){
      appendTo(c("Screen", "Report", "Warning"), "Release sites were not specified in the spatial.csv file but more than one release site is reported in the biometrics.csv file.\n   Assuming all released fish start at the top level array.")
      if (is.null(first.array)) {
        emergencyBreak()
        stop("There is more than one top level array in the study area. Please specify release site(s) in the spatial.csv file and in the biometrics.csv file.\n", call. = FALSE)
      }
    }
    release.sites <- data.frame(Station.name = unique(bio$Release.site), 
                                Longitude = NA_real_,
                                Latitude = NA_real_, 
                                Array = rep(first.array, length(unique(bio$Release.site))),
                                Standard.name = unique(bio$Release.site))
    for (i in unique(bio$Group)) {
      aux <- bio[bio$Group == i, ]
      release.sites[, paste0("n.", i)] <- 0
      n <- table(aux$Release.site)
      link <- match(names(n), release.sites$Standard.name)
      release.sites[link, paste0("n.", i)] <- n
    }
  }
  # Wrap up
  if (!is.null(sections)) {
    array.order <- list()  # Used to determine if the fish's last detection was in the last array of a given section
    for (j in sections) {
      array.order[[j]] <- names(arrays)[grepl(j, names(arrays))]
    }
    if (any(trigger <- unlist(lapply(array.order, length)) == 0)) {
      appendTo(c("Screen", "Report", "Warning"), paste0("No arrays were found that match section(s) ", paste(names(array.order)[trigger], collapse = ", "), ". There could be a typing mistake!"))
      decision <- readline("All arrays must be assigned to a section. Attempt to continue the analysis?(y/N) ")
      if (decision != "y" & decision != "Y" ){
        emergencyBreak()
        stop("Stopping analysis per user command.\n", call. = FALSE)
      }
    }
    if (any(link <- is.na(match(stations$Array, unlist(array.order))))) {
      emergencyBreak()
      stop(paste0("Array", 
          ifelse(sum(link) == 1, " '", "(s) '"), 
          paste(unique(stations$Array[link]), collapse = "', '"),
          ifelse(sum(link) == 1, "' was", "' were"), 
          " not assigned to any section. Stopping to prevent function failure.\nYou can either:\n1) Rename these arrays to match a section, \n2) Rename a section to match these arrays, or \n3) Include a new section in the analysis.\n"),
      call. = FALSE)
    }
  } else {
    array.order <- list(all = names(arrays))
  }
  # Order release sites by entry point.
  if (!is.ordered(match(release.sites$Array, unlist(array.order))))
    release.sites <- release.sites[order(match(release.sites$Array, unlist(array.order))),]
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
#' @keywords internal
#' 
excludeTags <- function(input, exclude.tags, silent){
  appendTo("debug", "Running excludeTags.")  
  if (length(exclude.tags) != 0) {
    link <- match(exclude.tags, names(input))
    if (!silent){
      appendTo(c("Screen", "Report"), paste0("M: Excluding tag(s) ", paste(exclude.tags, collapse = ", "), " from the analysis per used command (detections removed: ", paste(unlist(lapply(input[link], nrow)), collapse = ", "), ", respectively)."))
      collectStrays(input = input[link], restart = TRUE)
    }
    return(input[-link])
  }
  return(input)
}
