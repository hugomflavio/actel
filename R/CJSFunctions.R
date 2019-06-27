#' Assemble detection matrices
#' 
#' @inheritParams loadDetections
#' @inheritParams simplifyMovements
#' @inheritParams actel
#'
#' @return A list of detection matrices split by groups and release sites
#' 
#' @keywords internal 
#'
assembleMatrices <- function(spatial, movements, minimum.detections, status.df) {
  temp <- efficiencyMatrix(spatial = spatial, movements = movements, minimum.detections = minimum.detections)
  mymatrix <- includeMissing(x = temp, status.df = status.df)
  link <- sapply(status.df$Signal, grep, rownames(mymatrix))
  mymatrix <- mymatrix[link, ]  

  recipient <- split(mymatrix, paste0(status.df$Group, ".", status.df$Release.site))
  the.order <- c()
  for (i in unique(status.df$Group)) {
    the.order <- c(the.order, paste0(i, ".", unique(spatial$release.sites$Standard.Name)))
  }
  recipient <- recipient[order(match(names(recipient), the.order))]
  if (length(unique(spatial$release.sites$Array)) > 1) {
    for(i in 1:length(recipient)){
      r <- sapply(spatial$release.sites$Standard.Name, function(x) grepl(x, names(recipient)[i]))
      if(sum(r) > 1)
        stop("Multiple release sites match the matrix name. Make sure that the release sites are not contained within the fish groups or within themselves.\n")
      the.col <- which(grepl(spatial$release.sites$Array[r], colnames(recipient[[i]])))
      recipient[[i]] <- recipient[[i]][,c(1, the.col:ncol(recipient[[i]]))]
    }
  }
  return(recipient)
}

#' Get efficiency estimate for last array
#' 
#' @inheritParams loadDetections
#' @inheritParams groupMovements
#' @inheritParams actel
#' 
#' @return the modified CJS model for the last array
#' 
#' @keywords internal
#' 
getEstimate <- function(spatial, detections.list, replicate){
  if (!is.null(replicate)) {
    get.estimate = TRUE
    last.array <- tryCatch(lastMatrix(spatial = spatial, detections.list = detections.list, replicate = replicate),
      error = function(e) {cat("Error in lastMatrix(): "); message(e); cat("\nRunning CJS without last array efficiency estimate.\n"); get.estimate <<- FALSE})
    if (get.estimate) {
      last.array.efficiency <- dualArrayCJS(input = last.array, silent = FALSE)
      last.array.results <- list(results = last.array.efficiency, matrix = last.array)
    } else {
      last.array.results <- "Replicates could not be used for last array estimation."
    }
  } else {
    last.array.results <- "No last array replicates were indicated."
  }
  return(last.array.results)
}

#' Compute simple CJS model
#'
#' The calculations are based on 'Using mark-recapture models to estimate survival from telemetry data' by Perry et al. 2012
#'
#' @param input a detection matrix
#' @param estimate an estimate of the last array's efficiency
#' @param fixed.efficiency A vector of fixed efficiency estimates from a more complete CJS model.
#' @inheritParams splitDetections
#' 
#' @return A summary of the CJS results
#' 
#' @export
#' 
simpleCJS <- function(input, estimate = NULL, fixed.efficiency = NULL, silent = TRUE){
  # stop if there is weird data in the input
  if (any(input != 0 & input != 1))
    stop("input must be a matrix containing only 0's and 1's\n")
  
  # stop if both estimate and fixed efficiency are present
  if (!is.null(estimate) & !is.null(fixed.efficiency))
    stop("Please choose only one of 'estimate' or 'fixed.efficiency'.\n")
  
  # Only check below if estimate is set
  if (!is.null(estimate)) {
    # stop if multiple estimate values are provided.
    if (!is.null(estimate) && length(estimate) != 1)
      stop("Please use only one value for estimate.")
    # stop if estimate exceeds 1
    if (!is.null(estimate) && (estimate < 0 | estimate > 1))
      stop("estimate must be between 0 and 1.\n")
    # all good
  }

  # Only check below if fixed.efficiency was set
  if (!is.null(fixed.efficiency)) {
    # stop if there are not enough efficiency values
    if (length(fixed.efficiency) != ncol(input))
     stop("Fixed efficiency was set but its length is not the same as the maximum number of columns in the input.\n")
    # stop if any efficiency exceeds 1
    if (any(fixed.efficiency > 1, na.rm = TRUE))
      stop("Fixed efficiency estimates must be between 0 and 1.")
    # Fake estimate to avoid further changes below.
    if (!is.na(tail(fixed.efficiency, 1)))
     estimate <- tail(fixed.efficiency, 1)
    # all good
    if(!silent)
      cat("M: Running CJS with fixed efficiency estimates.\n"); flush.console()
  }

  # Start the calculations
  S <- rep(NA, ncol(input)-1)
  r <- z <- p <- m <- M <- rep(NA, ncol(input))
  for(i in 1:(ncol(input)-1)){
    # number of tags detected at i and downstream (r)
    tr <- input[input[, i] == 1, (i + 1) : ncol(input)]
    if (is.data.frame(tr))
      r[i] = sum(apply(tr, 1, function(f) any(f == 1)))
    if (is.vector(tr))
      r[i] = sum(tr)
    # number of tags NOT detected at i but detected downstream (z)
    tz <- input[input[, i] == 0, (i + 1) : ncol(input)]
    if (is.data.frame(tz))
      z[i] = sum(apply(tz, 1, function(f) any(f == 1)))
    if (is.vector(tz))
      z[i] = sum(tz)
    # probability of detection (p)
    if (is.null(fixed.efficiency)) {
      if (r[i] == 0 & z[i] == 0)
        p[i] <- 0
      else
        p[i] <- r[i] / (r[i] + z[i])
    } else {
      p[i] <- fixed.efficiency[i]
    }
    # number of detected tags at i (m)
    m[i] = sum(input[, i])
    # number of fish estimated alive at i (M)
    # Failsafe for array with 0 efficiency. Issues warning.
    if (p[i] == 0) {
      if(!silent)
        warning("Array '", colnames(input)[i],"' had 0% efficiency. Skipping survival estimation.")
      M[i] = M[i - 1]
      S[i - 1] = -999
      if (i == (ncol(input)-1))
        l <- 0
    } else {
      M[i] = round(m[i] / p[i], 0)
      # Fix M if the fixed efficiency value draws it too low.
      if (!is.null(fixed.efficiency) && M[i] < sum(m[i], z[i])){
        # Prevent fairly common warning from showing up while inside Actel.
        if(!silent) 
          warning("The fixed efficiency caused a too low estimate at iteration ", i,". Forcing higher estimate.")
        M[i] = sum(m[i], z[i])
      }
      if (i == 1 && M[i] > nrow(input)){
        M[i] = nrow(input)
      }
      # Correct M if the efficiency in the previous array was 0
      if (i > 1 && is.na(M[i - 1]))
          M[i - 1] = M[i]
      # Correct M if the estimate at i is bigger than the estimate at i - 1
      if (i > 1 && M[i] > M[i - 1])
        M[i] = M[i - 1]
      # survival probability from i - 1 to i (S)
      if (i > 1)
        S[i - 1] <- M[i] / M[i - 1]
      # lambda (last S * last P) 
      if (i == (ncol(input)-1))
        l <- r[i] / m[i]
    }
  }
  m[ncol(input)] <- sum(input[, ncol(input)])

  # Complete final values if estimate is present
  if (!is.null(estimate)) {
    if (estimate == 0) {
      S[ncol(input) -1 ] <- -999
    } else {
      S[ncol(input) - 1] <- l / estimate
      if (S[ncol(input) - 1] > 1)
        S[ncol(input) - 1] = 1
    }
    M[ncol(input)] <- round(m[ncol(input)] / estimate, 0)
    if (M[ncol(input)] > M[ncol(input) - 1])
      M[ncol(input)] = M[ncol(input) - 1]
    p[ncol(input)] = estimate
  }

  # prepare printings
  absolutes <- matrix(c(m, r, z, M), ncol = ncol(input), byrow = TRUE)
  rownames(absolutes) <- c("detected", "here plus downstream", "not here but downstream", "estimated")
  colnames(absolutes) <- colnames(input)
  names(p) <- colnames(input)
  link <- is.na(S) | S >= 0
  survival <- matrix(S[link], nrow = sum(link))
  if (any(!link))
    the.names <- colnames(input)[-(which(!link) + 1)]
  else
    the.names <- colnames(input)
  maxcharA <- max(nchar(the.names[-length(the.names)]))
  maxcharB <- max(nchar(the.names[-1]))
  the.rows <- c()
  for (i in 1:(length(the.names) - 1)) {
    if(nchar(the.names[i] != maxcharA))
      centeringA <- paste(rep(" ", maxcharA - nchar(the.names[i])), collapse = "")
    else
      centeringA <- ""
    if(nchar(the.names[i] != maxcharB))
      centeringB <- paste(rep(" ", maxcharB - nchar(the.names[i + 1])), collapse = "")
    else
      centeringB <- ""
    the.rows[length(the.rows) + 1] <- paste(centeringA, the.names[i], " -> ", the.names[i + 1], centeringB, " =", sep = "")
  }
  rownames(survival) <- the.rows
  colnames(survival) <- ""

  return(list(absolutes = absolutes, efficiency = p, survival = survival, lambda = l))
}

#' Calculate CJS for each group.release combination
#' 
#' @param the.matrices A list of detection matrices
#' @inheritParams simpleCJS
#' 
#' @return A list of CJS results
#' 
#' @keywords internal
#' 
getSplitCJS <- function(the.matrices, fixed.efficiency = NULL){
  output <- list()
  for(i in 1:length(the.matrices)){
    if(is.null(fixed.efficiency))
      efficiency.subset <- NULL
    else
      efficiency.subset <- fixed.efficiency[match(colnames(the.matrices[[i]]), names(fixed.efficiency))]
    output[[i]] <- simpleCJS(the.matrices[[i]], fixed.efficiency = efficiency.subset, silent = TRUE)
  }
  names(output) <- names(the.matrices)
  return(output)
}

#' Calculate CJS for each group
#' 
#' @inheritParams getSplitCJS
#' @inheritParams simplifyMovements
#' @inheritParams simpleCJS
#' 
#' @return A list of CJS results
#' 
#' @keywords internal
#' 
getGroupCJS <- function(the.matrices, status.df, fixed.efficiency = NULL) {
  output <- list()
  for (i in 1:length(unique(status.df$Group))) {
    link <- grepl(paste0("^", unique(status.df$Group)[i]), names(the.matrices))
    if(sum(link) == 1)
      output[[i]] <- simpleCJS(the.matrices[[which(link)]], fixed.efficiency = fixed.efficiency, silent = TRUE)
    else
      output[[i]] <- combineCJS(the.matrices[link], fixed.efficiency = fixed.efficiency, silent = TRUE)
  } 
  names(output) <- unique(status.df$Group)
  return(output)
}


#' Calculate CJS per release site
#' 
#' @inheritParams getSplitCJS
#' @inheritParams simplifyMovements
#' 
#' @return A list of CJS results
#' 
#' @keywords internal
#' 
getReleaseCJS <- function(the.matrices, status.df) {
  appendTo("debug", "Starting getReleaseCJS.")

  recipient <- list()
  for(i in unique(status.df$Release.site)) {
    recipient[[i]] <- the.matrices[grepl(paste0(".",i), names(the.matrices))]
  }


  output <- list()
  for (i in names(recipient)) {
    if (exists("last.array.efficiency")){
      inter.array <- simpleCJS(recipient[[i]], estimate = last.array.efficiency$combined.efficiency, silent = FALSE)
      output[[length(output) + 1]] <- list(results = inter.array, matrix = recipient[[i]])
    } else {
      inter.array <- simpleCJS(recipient[[i]], silent = FALSE)
      output[[length(output) + 1]] <- list(results = inter.array, matrix = recipient[[i]])
    }
  }
  names(output) <- names(recipient)
  appendTo("debug", "Terminating getReleaseCJS.")
  return(list(inter.array = output, last.array = last.array.results))
}

#' Compile inter-array detection matrix
#'
#' @inheritParams actel
#' @inheritParams loadDetections
#' @inheritParams simplifyMovements
#' 
#' @return a matrix of detection histories per fish.
#' 
#' @keywords internal
#' 
efficiencyMatrix <- function(spatial, movements, minimum.detections) {
  appendTo("debug", "Starting efficiencyMatrix.")
  efficiency <- as.data.frame(matrix(ncol = length(unlist(spatial$array.order)), nrow = length(movements)))
  colnames(efficiency) <- unlist(spatial$array.order)
  rownames(efficiency) <- stripCodeSpaces(names(movements))
  efficiency[is.na(efficiency)] = 0
  for (i in 1:length(movements)) {
    submoves <- movements[[i]][!grepl("Unknown", movements[[i]][, "Array"]), ]
    if (nrow(submoves) >= 1) {
      A <- match(submoves[,"Array"],unlist(spatial$array.order))
      B1 <- unique(A)
      B2 <- order(A)[!duplicated(sort(A))] 
      if (is.unsorted(B2)) {
        temp <- matrix(ncol = length(B1), nrow = length(B1))
        for (j in 1:length(B1)) {
            temp[j,] <- c(rep(FALSE, j - 1), B1[j] > B1[j:length(B1)])
        }
        B1 <- B1[!apply(temp,2,any)]
      }
      efficiency[i, B1] <- 1
    }
  }
  efficiency$Release <- 1
  appendTo("debug", "Terminating efficiencyMatrix.")
  return(efficiency[,c(ncol(efficiency), 1:(ncol(efficiency)-1))])
}

#' Include fish that were never detected
#' 
#' @param x an efficiency matrix
#' @inheritParams simplifyMovements
#' 
#' @return a matrix of detection histories per fish, including fish that were never detected.
#' 
#' @keywords internal
#' 
includeMissing <- function(x, status.df){
  appendTo("debug", "Starting includeMissing")
  include <- as.character(status.df[-match(rownames(x), status.df$Signal),"Signal"])
  x[include, ] = 0
  x[include, 1] = 1
  appendTo("debug", "Terminating includeMissing")
  return(x)
}

#' Compile detection matrix for last array
#'
#' @inheritParams actel
#' @inheritParams loadDetections
#' @inheritParams groupMovements
#' 
#' @return a matrix of detection histories per fish for the last array.
#' 
#' @keywords internal
#' 
lastMatrix <- function(spatial, detections.list, replicate){
  appendTo("debug", "Starting lastMatrix.")
  all.stations <- spatial$stations$Standard.Name[spatial$stations$Array == tail(unlist(spatial$array.order), 1)]
  if (any(link <- !replicate %in% all.stations)) {
    if (sum(link) > 1)
      stop(paste("Stations ", paste(replicate[link], collapse = ", "), " are not part of ", tail(unlist(spatial$array.order), 1), " (available stations: ", paste(all.stations, collapse = ", "), ").", sep = ""))
    else
      stop(paste("Station ", paste(replicate[link], collapse = ", "), " is not part of ", tail(unlist(spatial$array.order), 1), " (available stations: ", paste(all.stations, collapse = ", "), ").", sep = ""))      
  }
  original <- all.stations[!all.stations %in% replicate]
  efficiency <- as.data.frame(matrix(ncol = 2, nrow = length(detections.list)))
  colnames(efficiency) <- c("original","replicate")
  rownames(efficiency) <- names(detections.list)
  for (i in 1:length(detections.list)) {
    efficiency[i, "original"] <- any(!is.na(match(original,detections.list[[i]][,"Standard.Name"])))
    efficiency[i, "replicate"] <- any(!is.na(match(replicate,detections.list[[i]][,"Standard.Name"])))
  }
  appendTo("debug", "Terminating lastMatrix.")
  return(efficiency)
}


#' Calculate estimated last-array efficiency
#'
#' The calculations are based on 'Using mark-recapture models to estimate survival from telemetry data' by Perry et al. 2012
#' 
#' @inheritParams simpleCJS
#' @inheritParams splitDetections
#' 
#' @return A summary of the CJS results
#' 
#' @export
#' 
dualArrayCJS <- function(input, silent = TRUE){
  if(!silent) appendTo("debug", "Starting dualArray.")
  r <- z <- p <- m <- M <- rep(NA, ncol(input))
  for(i in 1:(ncol(input))){
    # number of tags detected at i and elsewhere (r)
    tr <- input[input[, i] == 1, -i]
    if (is.data.frame(tr))
      r[i] = sum(apply(tr, 1, function(f) any(f == 1)))
    if (is.vector(tr))
      r[i] = sum(tr)
    # number of tags NOT detected at i but detected elsewhere (z)
    tz <- input[input[, i] == 0, -i]
    if (is.data.frame(tz))
      z[i] = sum(apply(tz, 1, function(f) any(f == 1)))
    if (is.vector(tz))
      z[i] = sum(tz)
    # probability of detection (p)
    p[i] <- r[i] / (r[i] + z[i])
    # number of detected tags at i (m)
    m[i] = sum(input[, i])
    # number of fish estimated alive at i (M)
    M[i] = m[i] / p[i]
    if (M[i] > nrow(input))
      M[i] = nrow(input)
  }
  combined.p <- 1 - prod(1 - p)
  absolutes <- matrix(c(apply(input, 2, sum), r[1]), nrow = 3)
  colnames(absolutes) <- ""
  rownames(absolutes) <- c("detected at original:", "detected at replicate: ", "detected at both:")
  names(p) <- c("original", "replicate")
  if(!silent) appendTo("debug", "Terminating dualArray.")
  return(list(absolutes = absolutes, single.efficiency = p, combined.efficiency = combined.p))
}

#' Split CJS matrix and calculate separate CJS
#' 
#' @param input The CJS matrix to be split
#' @param by a grouping vector with the same length has the number of rows in input
#' @param fixed.efficiency logical: If TRUE, a CJS is run with the entire dataset and the efficiency estimates of that CJS are used in the split CJS's.
#' @inheritParams simpleCJS
#' 
#' @return The split CJS
#' 
#' @export
#' 
splitCJS <- function(input, by, estimate = NULL, fixed.efficiency = TRUE){
  if (!is.matrix(input))
    stop("'input' must be a matrix object\n")
  if (!is.vector(by))
    stop("'by' must be a vector\n")
  if (nrow(input) != length(by))
    stop("Length of 'by' is not equal to the number of rows in 'input'.\n")

  recipient <- split(input, by = by)

  if (fixed.efficiency)
    fixed.efficiency <- simpleCJS(input, estimate = estimate)$efficiency
  output <- list()
  for (i in names(recipient)) {
    results <- simpleCJS(recipient[[i]], estimate = estimate, fixed.efficiency = fixed.efficiency)
    output[[length(output) + 1]] <- list(results = results, matrix = recipient[[i]])
  }
  names(output) <- names(recipient)
  return(output)
}

#' Combine multiple CJS models
#' 
#' @param ... The detection matrices to be joined, or a list containing detection matrices
#' @inheritParams simpleCJS
#' @inheritParams splitDetections
#' 
#' @return The combined CJS
#' 
#' @export
#' 
combineCJS <- function(..., estimate = NULL, fixed.efficiency = NULL, silent = FALSE){
  # stop if both estimate and fixed efficiency are present
  if (!is.null(estimate) & !is.null(fixed.efficiency))
    stop("Please choose only one of 'estimate' or 'fixed.efficiency'.\n")
  # stop if multiple estimate values are provided.
  if (!is.null(estimate) && length(estimate) != 1)
    stop("Please use only one value for estimate.")
  # stop if any efficiency exceeds 1
  if (any(fixed.efficiency > 1, na.rm = TRUE))
    stop("Fixed efficiency estimates must be between 0 and 1.")

  # hack to figure the number of arguments in the dots, because I could not find a better way around it.
  arg.names <- names(match.call())
  discount <- 1
  if (!is.null(arg.names)) {
    for (i in c("estimate", "fixed.efficiency", "silent")) {
      if(any(arg.names == i))
        discount = discount + 1
    }
  }
  rm(arg.names)

  # Prepare input
  if ((length(match.call()) - discount) == 1) {
    if(is.list(...)) {
      input <- (...)
      if (length(input) == 1)
        stop("Input appears to contain a list with only one element.\n")
    } else {
      stop("Only one object provided but it is not a list.\n")
    }
  } else {
    input <- list(...)
    if(any(unlist(lapply(input, function(x) !is.matrix(x) & !is.data.frame(x)))))
      stop("Not all objects provided are matrices or dataframes. Please use either one list of matrices/dataframes or multiple matrices/dataframes.\n")
  }

  # stop if not all matrices finish in the same array
  the.last <- unlist(lapply(input, function(x) tail(colnames(x),1)))
  if (length(unique(the.last)) > 1)
    stop("The last array is not the same in all input matrices\n")

  ncols <- unlist(lapply(input, ncol))
  groups <- rev(sort(unique(ncols)))

  if (!is.null(fixed.efficiency)){
    # stop if there are not enough efficiency values
    if (length(fixed.efficiency) != max(ncols)) {
      stop("Fixed efficiency was set but its length is not the same as the maximum number of columns in the input.\n")
    } else {
      if (!silent)
        cat("M: Running CJS with fixed efficiency values.\n"); flush.console()
    }
  }

  combinedmatrices <- list()
  for (i in 1:length(groups)) {
    to.join <- which(ncols == groups[i])
    if (length(unique(table(unlist(lapply(input[to.join], colnames))))) != 1)
      stop(paste("matrices", paste(to.join, collapse = ", "), "both contain", groups[i], "columns but the column names differ.\n"))
    output <- input[[to.join[1]]]
    if (length(to.join) > 1) {
      for (j in 2:length(to.join))
        output <- rbind(output, input[[to.join[j]]])
    }
    combinedmatrices[[i]] <- output
  }

  if(length(combinedmatrices) == 1)
    return(simpleCJS(combinedmatrices[[1]]))

  the.CJSs <- list()
  for (i in combinedmatrices) {
    the.CJSs[[length(the.CJSs) + 1]] <- simpleCJS(i, estimate = estimate)
  }

  return(recalculateCJS(input = the.CJSs, estimate = estimate, fixed.efficiency = fixed.efficiency))
}

#' Combine CJS model results
#' 
#' @param input A list of CJS results to be combined
#' @inheritParams simpleCJS
#' 
#' @return The combined CJS
#' 
#' @keywords internal
#' 
recalculateCJS <- function(input, estimate = NULL, fixed.efficiency = NULL){
  # Fake estimate to avoid further changes below.
  if (!is.null(fixed.efficiency) && !is.na(tail(fixed.efficiency, 1)))
     estimate <- tail(fixed.efficiency, 1)

  # The first CJS starts the process
  absolutes <- input[[1]]$absolutes
  # Determine how many fish enter and where on the remaining CJSs
  exN <- c()
  for (i in input[-1]){
    the.cols <- match(colnames(i$absolutes), colnames(absolutes))[-1]
    exN[length(exN) + 1] <- i$absolutes[1, 1]
    names(exN)[length(exN)] <- colnames(absolutes)[the.cols[1]]
  }
  # Extract variables from input, for easier handling below
  m <- absolutes[1,]
  r <- absolutes[2,]
  z <- absolutes[3,]
  p <- M <- rep(NA, ncol(absolutes))
  S <- c()
  counter <- 1
  for (i in 1:(ncol(absolutes) - 1)) {
    # probability of detection (p)
    if (is.null(fixed.efficiency)) {
      if (r[i] == 0 & z[i] == 0)
        p[i] <- 0
      else
        p[i] <- r[i] / (r[i] + z[i])
    } else {
      p[i] <- fixed.efficiency[i]
    }
    # number of fish estimated alive at i before inclusion of new (M)
    if (p[i] == 0) {
      if (counter == 1) {
        warning("Array'", colnames(input[[1]])[i],"' detected 0 fish. Skipping survival estimation.")
        M[i] = input[[1]][4, i]
      } else {
        stop("You reached a safety stop. The code for this exception does not exist yet. Contact the development team.")
      }
      S[length(S) + 1] = -999
    } else {
      M[i] = round(m[i] / p[i], 0)
      if (!is.null(fixed.efficiency) && M[i] < sum(m[i], z[i])){
        warning("The fixed efficiency caused a too low estimate at iteration ", i,". Forcing higher estimate. [1]")
        M[i] = sum(m[i], z[i])
      }
      # Initial estimated number is the number of released fish
      if (i == 1 && M[i] > absolutes[1, 1])
        M[i] = absolutes[1, 1]

      if (i > 1 && M[i] > M[i - 1]){
        # If new fish entered the CJS in the last round, account for that
        if (grepl(colnames(absolutes)[i - 1], names(exN))){
          if (M[i] > M[i - 1] + input[[counter]]$absolutes[4, 2])
            M[i] = M[i - 1] + input[[counter]]$absolutes[4, 2]
        } else {
          M[i] = M[i - 1]
        }
      }
      # survival probability from i-1 to i (S)
      if (i > 1) {
        # If new fish entered the CJS in the last round, account for that
        if(grepl(colnames(absolutes)[i - 1], names(exN)))
          S[length(S) + 1] <- M[i] / (M[i - 1] + exN[colnames(absolutes)[i - 1]])
        else
          S[length(S) + 1] <- M[i] / M[i - 1]
      } 
    }
    # NEW CJS ENTRY POINT
    if (grepl(colnames(absolutes)[i], names(exN))) {
      counter <- counter + 1
      # Calculate survival from above release to release
      if (input[[counter]]$efficiency[2] == 0) {
        S[length(S) + 1] <- -999
      } else {
        tempS <- input[[counter]]$survival[1]
        if (S[length(S)] != -999) {
          S[length(S)] <- tail(S,1)/tempS
          if (S[length(S)] > 1)
            S[length(S)] <- 1
        }
        S[length(S) + 1] <- tempS
      }
      # update absolutes with new CJS numbers
      to.add <- input[[counter]]$absolutes
      the.cols <- match(colnames(to.add), colnames(absolutes))[-1]
      absolutes[, the.cols] <- absolutes[, the.cols] + to.add[, -1]
      m <- absolutes[1,]
      r <- absolutes[2,]
      z <- absolutes[3,]
      # update p for the array of inclusion
      if (is.null(fixed.efficiency)) {
        if (r[i] == 0 & z[i] == 0)
          p[i] <- 0
        else
          p[i] <- r[i] / (r[i] + z[i])
      }
      # update M based on new p
      M[i] = round(m[i] / p[i], 0)
      if(M[i] < sum(absolutes[c(1, 3), i])) {
        warning("The combined efficiency caused a too low estimate at iteration ", i,". Forcing higher estimate. [2]")
        M[i] <- sum(absolutes[c(1, 3), i])
      }
    }
    # lambda (last S * last P) 
    if (i == (ncol(absolutes) - 1)){
      if(m[i] > 0)
        l <- r[i] / m[i]
      else 
        l <- 0
    }
  }

  # Calculate last S and last M if estimate is present
  if (!is.null(estimate)) {
    if (estimate == 0) {
      S[length(S) + 1] <- -999
    } else {
      S[length(S) + 1] <- l / estimate
      if (tail(S, 1) > 1)
        S[length(S)] = 1
    }
    M[ncol(absolutes)] <- round(m[ncol(absolutes)] / estimate,0)
    if (M[ncol(absolutes)] > M[ncol(absolutes) - 1])
      M[ncol(absolutes)] = M[ncol(absolutes) - 1]
  } else {
    S[length(S) + 1] <- NA
  }

  # Exchange initial absolutes by final absolutes
  absolutes[1,] <- m
  absolutes[2,] <- r
  absolutes[3,] <- z
  absolutes[4,] <- M

  # Give names to the efficiency estimates
  names(p) <- colnames(absolutes)

  # Compile final survival matrix
  link <- is.na(S) | S >= 0
  survival <- matrix(S[link], nrow = sum(link))

  the.names <- colnames(absolutes)
  the.names[1] <- "RS1"
  for (i in 1:length(exN)) {
    pos <- match(names(exN)[i], the.names)
    the.names <- c(the.names[1:(pos - 1)], paste0("RS", i + 1), the.names[pos:length(the.names)])
  }
  if (any(!link)) {
    to.check <- which(!link)
    to.remove <- c()
    for (i in to.check) {
      if (grepl("^RS[0-9]*", the.names[i]))
        to.remove <- c(to.remove, i)
      else
        to.remove <- c(to.remove, i + 1)
    }
    the.names <- the.names[-to.remove]
  }
  maxcharA <- max(nchar(the.names[-length(the.names)]))
  maxcharB <- max(nchar(the.names[-1]))
  the.rows <- c()
  for (i in 1:(length(the.names) - 1)) {
    if(nchar(the.names[i] != maxcharA))
      centeringA <- paste(rep(" ", maxcharA - nchar(the.names[i])), collapse = "")
    else
      centeringA <- ""
    if(nchar(the.names[i] != maxcharB))
      centeringB <- paste(rep(" ", maxcharB - nchar(the.names[i + 1])), collapse = "")
    else
      centeringB <- ""
    the.rows[length(the.rows) + 1] <- paste(centeringA, the.names[i], " -> ", the.names[i + 1], centeringB, " =", sep = "")
  }
  rownames(survival) <- the.rows
  colnames(survival) <- ""
  if (is.null(fixed.efficiency)){
    if (is.null(estimate))
      return(list(absolutes = absolutes, additions = exN, efficiency = p, survival = survival, lambda = l))
    else
      return(list(absolutes = absolutes, additions = exN, efficiency = p, survival = survival, lambda = l, estimate = estimate))  
  } else {
      return(list(absolutes = absolutes, additions = exN, fixed.efficiency = p, survival = survival, lambda = l))    
  }
}
