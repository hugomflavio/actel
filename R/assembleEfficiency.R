#' Efficiency calculation wrapper
#'
#' Collects the input from interSection, inter-array and intra-array into a list.
#' 
#' @inheritParams actel
#' @param detections.list A list of the detections split by each target tag, created by splitDetections.
#' @param spatial A list of the spatial objects, created by assembleSpatial.
#' @param movements A list of movements for each target tag, created by groupMovements.
#' 
#' @return A list containing 1) the intersection efficiency, 2) the inter-array efficiency and 3) the the intra-array efficiency.
#' 
#' @keywords internal
#' 
assembleEfficiency <- function(sections, spatial, detections.list, movements, status.df, minimum.detections, replicate) {
  appendTo("debug", "Starting assembleEfficiency.")
  temp <- efficiencyMatrix(spatial = spatial, movements = movements, minimum.detections = minimum.detections)
  mymatrix <- includeMissing(x = temp, status.df = status.df)
  link <- sapply(status.df$Signal, grep, rownames(mymatrix))
  mymatrix <- mymatrix[link, ]  

  recipient <- split(mymatrix, status.df$Release.site)
  if (length(unique(spatial$release.sites$Array)) > 1) {
    for(i in names(recipient)){
      the.col <- which(grepl(spatial$release.sites$Array[spatial$release.sites$Station.Name == i], colnames(recipient[[i]])))
      recipient[[i]] <- recipient[[i]][,c(1, the.col:ncol(recipient[[i]]))]
    }
  }

# I was here, about to get the CJS to run for every element of recipient
  if (!is.null(replicate)) {
    get.estimate = TRUE
    last.array <- tryCatch(lastMatrix(spatial = spatial, detections.list = detections.list, replicate = replicate),
      error=function(e) {cat("Error in lastMatrix(): "); message(e); cat("\nRunning CJS without last array efficiency estimate.\n"); get.estimate <<- FALSE})
    if (get.estimate) {
      last.array.efficiency <- dualArray(input = last.array, silent = FALSE)
      last.array.results <- list(results = last.array.efficiency, matrix = last.array)
    } else {
      last.array.results <- list(results = "No last array replicates were indicated.")
    }
  } else {
    last.array.results <- list(results = "No last array replicates were indicated.")
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
  appendTo("debug", "Terminating assembleEfficiency.")
  return(list(inter.array = output, last.array = last.array.results))
}

#' Compile inter-array detection matrix
#'
#' @inheritParams actel
#' @inheritParams assembleEfficiency
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
#' @inheritParams assembleOverview
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
#' @inheritParams assembleEfficiency
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
      stop(paste("Stations ", paste(replicate[link], collapse = ", "), " are not part of ", tail(unlist(spatial$array.order), 1), " (available stations: ", paste(all.stations, collapse = ", "), ").\n", sep = ""))
    else
      stop(paste("Station ", paste(replicate[link], collapse = ", "), " is not part of ", tail(unlist(spatial$array.order), 1), " (available stations: ", paste(all.stations, collapse = ", "), ").\n", sep = ""))      
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

#' Compute simple CJS model
#'
#' The calculations are based on 'Using mark-recapture models to estimate survival from telemetry data' by Perry et al. 2012
#'
#' @param input a detection matrix
#' @param estimate an estimate of the last array's efficiency
#' 
#' @return A summary of the CJS results
#' 
#' @export
#' 
simpleCJS <- function(input, estimate = NULL, silent = TRUE){
  if (!silent) appendTo("debug", "Starting simpleCJS.")
  if (!is.null(estimate) && (estimate < 0 | estimate > 1))
    stop("estimate must be between 0 and 1.\n")
  if (any(input != 0 & input != 1))
    stop("input must be a matrix containing only 0's and 1's\n")
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
    p[i] <- r[i] / (r[i] + z[i])
    # number of detected tags at i (m)
    m[i] = sum(input[, i])
    # number of fish estimated alive at i (M)
    M[i] = round(m[i] / p[i], 0)
    if (i == 1 && M[i] > nrow(input))
      M[i] = nrow(input)
    if (i > 1 && M[i] > M[i - 1])
      M[i] = M[i - 1]
    # survival probability from i-1 to i (S)
    if (i > 1)
      S[i - 1] <- M[i] / M[i - 1]
    # lambda (last S * last P) 
    if (i == (ncol(input)-1))
      l <- r[i] / m[i]
  }
  m[ncol(input)] <- sum(input[, ncol(input)])
  if (!is.null(estimate)) {
    S[ncol(input)-1] <- l / estimate
    if (S[ncol(input)-1] > 1)
      S[ncol(input)-1] = 1
    M[ncol(input)] <- round(m[ncol(input)] / estimate,0)
    if (M[ncol(input)] > M[ncol(input) - 1])
      M[ncol(input)] = M[ncol(input) - 1]
  }

  absolutes <- matrix(c(m, r, z, M), ncol = ncol(input), byrow = TRUE)
  rownames(absolutes) <- c("detected", "here plus downstream", "not here but downstream", "estimated")
  colnames(absolutes) <- colnames(input)
  names(p) <- colnames(input)
  survival <- matrix(S, nrow = length(S))
  
  the.rows <- c()
  for(i in 2:ncol(input)){
    maxcharA <- max(nchar(colnames(input)[-ncol(input)]))
    maxcharB <- max(nchar(colnames(input)[-1]))
    if(nchar(colnames(input)[i - 1] != maxcharA))
      centeringA <- paste(rep(" ", maxcharA - nchar(colnames(input)[i - 1])), collapse = "")
    else
      centeringA <- ""
    if(nchar(colnames(input)[i] != maxcharB))
      centeringB <- paste(rep(" ", maxcharB - nchar(colnames(input)[i])), collapse = "")
    else
      centeringB <- ""
    the.rows[i - 1] <- paste(centeringA, colnames(input)[i - 1], " -> ", colnames(input)[i], centeringB, " =", sep = "")
  }
  rownames(survival) <- the.rows
  colnames(survival) <- ""
  if (!silent) appendTo("debug", "Terminating simpleCJS.")
  if (is.null(estimate))
    return(list(absolutes = absolutes, efficiency = p, survival = survival, lambda = l))
  else
    return(list(absolutes = absolutes, efficiency = p, survival = survival, lambda = l, estimate = estimate))
}

#' Calculate estimated last-array efficiency
#'
#' The calculations are based on 'Using mark-recapture models to estimate survival from telemetry data' by Perry et al. 2012
#' 
#' @inheritParams simpleCJS
#' 
#' @return A summary of the CJS results
#' 
#' @export
#' 
dualArray <- function(input, silent = TRUE){
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
  rownames(absolutes) <- c("detected at original:", "detected at replicate:", "detected at both:")
  names(p) <- c("original", "replicate")
  if(!silent) appendTo("debug", "Terminating dualArray.")
  return(list(absolutes = absolutes, single.efficiency = p, combined.efficiency = combined.p))
}

#' Split CJS matrix and calculate separate CJS
#' 
#' @param input The CJS matrix to be split
#' @param by a grouping vector with the same length has the number of rows in input
#' @inheritParams simpleCJS
#' 
#' @return the split CJS
#' 
#' @export
#' 
splitCJS <- function(input, by, estimate = NULL){
  if (!is.matrix(input))
    stop("'input' must be a matrix object\n")
  if (!is.vector(by))
    stop("'by' must be a vector\n")
  if (nrow(input) != length(by))
    stop("Length of 'by' is not equal to the number of rows in 'input'.\n")

  recipient <- split(input, by = by)

  output <- list()
  for (i in names(recipient)) {
    results <- simpleCJS(recipient[[i]], estimate = estimate)
    output[[length(output) + 1]] <- list(results = results, matrix = recipient[[i]])
  }
  names(output) <- names(recipient)
  return(output)
}

#' Split CJS matrix and calculate separate CJS
#' 
#' @param ... The CJS matrixes to be joined
#' @inheritParams simpleCJS
#' 
#' @return the combined CJS
#' 
#' @export
#' 
combineCJS <- function(..., estimate = NULL){
  input <- list(...)

  the.last <- unlist(lapply(input, function(x) tail(colnames(x),1)))
  if (length(unique(the.last)) > 1)
    stop("The last array is not the same in all input matrixes\n")

  ncols <- unlist(lapply(input, ncol))
  groups <- rev(sort(unique(ncols)))

  combinedmatrixes <- list()
  for (i in 1:length(groups)) {
    to.join <- which(ncols == groups[i])
    if (length(unique(table(unlist(lapply(input[to.join], colnames))))) != 1)
      stop(paste("Matrixes", paste(to.join, collapse = ", "), "both contain", groups[i], "columns but the column names differ.\n"))
    output <- input[[to.join[1]]]
    if (length(to.join) > 1) {
      for (j in 2:length(to.join))
        output <- rbind(output, input[[to.join[i]]])
    }
    combinedmatrixes[[i]] <- output
  }

  if(length(combinedmatrixes) == 1)
    return(simpleCJS(combinedmatrixes[[1]]))

  the.CJSs <- list()
  for (i in combinedmatrixes) {
    the.CJSs[[length(the.CJSs) + 1]] <- simpleCJS(i, estimate = estimate)
  }

  return(recalculateCJS(input = the.CJSs, estimate = estimate))
}

#' Combine CJS model results
#' 
#' @param input A list of CJS results to be combined
#' @inheritParams simpleCJS
#' 
#' @return the combined CJS
#' 
#' @keywords internal
#' 
recalculateCJS <- function(input, estimate = NULL){
  # The first CJS starts the party
  absolutes <- input[[1]]$absolutes
  # Determine how many fish enter and where on the remaining CJSs
  exN <- c()
  for (i in input[-1]){
    exN[length(exN) + 1] <- to.add[1, 1]
    names(exN)[length(exN)] <- colnames(absolutes)[the.cols[1]]
  }
  # Extract variables from input, for easier handling below
  m <- absolutes[1,]
  r <- absolutes[2,]
  z <- absolutes[3,]
  p <- M <- rep(NA, ncol(absolutes))
  S <- c()
  counter <- 1
  for(i in 1:(ncol(absolutes)-1)){
    # probability of detection (p)
    p[i] <- r[i] / (r[i] + z[i])
    # number of fish estimated alive at i (M)
    M[i] = round(m[i] / p[i], 0)

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
    # NEW CJS ENTRY POINT
    if (grepl(colnames(absolutes)[i], names(exN))) {
      counter <- counter + 1
      # Calculate survival from above release to release
      tempS <- input[[counter]]$survival[1]
      S[length(S)] <- tail(S,1)/tempS
      if (S[length(S)] > 1)
        S[length(S)] <- 1
      S[length(S) + 1] <- tempS
      # update absolutes with new CJS numbers
      to.add <- input[[counter]]$absolutes
      the.cols <- match(colnames(to.add), colnames(absolutes))[-1]
      absolutes[, the.cols] <- absolutes[, the.cols] + to.add[, -1]
      m <- absolutes[1,]
      r <- absolutes[2,]
      z <- absolutes[3,]
      # update M to include the estimated from the new CJS
      M[i] = M[i] + input[[counter]]$absolutes[4, 2]
    }
  }

  # Calculate last S and last M if estimate is present
  if (!is.null(estimate)) {
    S[length(S) + 1] <- l / estimate
    if (tail(S, 1) > 1)
      S[length(S)] = 1
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
  survival <- matrix(S, nrow = length(S))

  the.strings <- colnames(absolutes)
  the.strings[1] <- "RS1"
  for (i in 1:length(exN)) {
    pos <- match(names(exN)[i], the.strings)
    the.strings <- c(the.strings[1:3], paste0("RS", j + 1), the.strings[4:length(the.strings)])
  }
  maxcharA <- max(nchar(the.strings[-length(the.strings)]))
  maxcharB <- max(nchar(the.strings[-1]))

  the.rows <- c()
  for (i in 2:length(the.strings)) {
    if(nchar(the.strings[i - 1] != maxcharA))
      centeringA <- paste(rep(" ", maxcharA - nchar(the.strings[i - 1])), collapse = "")
    else
      centeringA <- ""
    if(nchar(the.strings[i] != maxcharB))
      centeringB <- paste(rep(" ", maxcharB - nchar(the.strings[i])), collapse = "")
    else
      centeringB <- ""
    the.rows[i - 1] <- paste(centeringA, the.strings[i - 1], " -> ", the.strings[i], centeringB, " =", sep = "")
  }
  rownames(survival) <- the.rows
  colnames(survival) <- ""  

  if (is.null(estimate))
    return(list(absolutes = absolutes, efficiency = p, survival = survival, lambda = l))
  else
    return(list(absolutes = absolutes, efficiency = p, survival = survival, lambda = l, estimate = estimate))  
}
