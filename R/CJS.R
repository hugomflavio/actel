#' CJS.R arguments
#' @param replicates A list of arrays containing the respective replicate stations.
#' @param CJS The overall CJS results.
#' @param detections.list A list of the detections split by each target tag.
#' @param spatial The spatial data frame.
#' @param efficiency The efficiency results.
#' @param arrays A list containing information for each array.
#' @param mat,m The presence/absence matrices.
#' @param input A presence/absence matrix.
#' @param movements,moves The movements table.
#' @param status.df The main results table.
#' @param dotmat The matrix of distances between arrays.
#' @param paths A list containing the shortest paths between arrays with distance > 1.
#' @param estimate An estimate of the last array's efficiency, between 0 and 1.
#' @param fixed.efficiency A vector of fixed efficiency estimates \[0, 1\]. `length(fixed.efficiency)` must match `ncol(input)`.
#' @param silent Logical: Should messages be printed? This argument is mainly intended for function calls running within actel's analyses.
#' @name cjs_args
#' @keywords internal
NULL

#' Prepare intra-array matrices for selected arrays
#'
#' @inheritParams cjs_args
#'
#' @return A list of dual matrices for each relevant array.
#'
#' @keywords internal
#'
getDualMatrices <- function(replicates, CJS = NULL, spatial, detections.list) {
  appendTo("debug", "Running getDualMatrices.")
  output <- list()
  for(i in 1:length(replicates)) {
    continue <- TRUE
    if (!is.null(CJS)) {
      if (any(grepl("^max.efficiency$", names(CJS))))
        already.calculated <- !is.na(CJS$max.efficiency[names(replicates)[i]])
      else
        already.calculated <- !is.na(CJS$efficiency[names(replicates)[i]])
      if (already.calculated) {
        appendTo(c("Screen", "Warning", "Report"), paste0("An inter-array efficiency has already been calculated for array ", names(replicates)[i],"."))
        decision <- userInput("Do you want to replace this with an intra-array efficiency estimate?(y/n) ",
                                choices = c("y", "n"), hash = "# replace efficiency?")
        if (!interactive() | decision == "y") {
          appendTo("Report", paste0("Replacing efficiency estimation."))
          continue <- TRUE
        } else { # nocov start
          appendTo("Report", paste0("Keeping inter-array efficiency estimation."))
          continue <- FALSE
        } # nocov end
      }
    }
    if (continue) {
      output[[length(output) + 1]] <- dualMatrix(array = names(replicates)[i],
        replicates = replicates[[i]], spatial = spatial, detections.list = detections.list)
      names(output)[[length(output)]] <- names(replicates)[i]
    }
  }
  return(output)
}

#' Incorporate intra-array estimates in the overall CJS object
#'
#' @param m A list of dual matrices
#' @inheritParams cjs_args
#'
#' @return A list containing the updated overall CJS and the intra-array CJS results
#'
#' @keywords internal
#'
includeIntraArrayEstimates <- function(m, efficiency = NULL, CJS = NULL) {
  appendTo("debug", "Running includeIntraArrayEstimates.")
  if (!is.null(efficiency) & !is.null(CJS))
    stop("Use only one of 'efficiency' or 'CJS' at a time.\n")
  if (length(m) > 0) {
    intra.CJS <- lapply(m, dualArrayCJS)
    if (!is.null(CJS)) {
      for (i in names(intra.CJS)) {
        CJS$absolutes["estimated", i] <- round(CJS$absolutes["detected", i] / intra.CJS[[i]]$combined.efficiency, 0)
        CJS$efficiency[i] <- intra.CJS[[i]]$combined.efficiency
      }
    }
    if (!is.null(efficiency)) {
      for (i in names(intra.CJS)) {
        efficiency$max.efficiency[i] <- intra.CJS[[i]]$combined.efficiency
        efficiency$min.efficiency[i] <- intra.CJS[[i]]$combined.efficiency
      }
    }
  } else {
    intra.CJS <- NULL
  }
  if (!is.null(CJS))
    return(list(CJS = CJS, intra.CJS = intra.CJS))
  if (!is.null(efficiency))
    return(list(efficiency = efficiency, intra.CJS = intra.CJS))
  return(list(intra.CJS = intra.CJS))
}


#' Assembles CJS tables for all group x release site combinations
#'
#' @param CJS A list of CJS calculated for each group x release site x array combinations
#' @param arrays a list of arrays
#'
#' @return A list containing the CJS results for each group x release site combination.
#'
#' @keywords internal
#'
assembleSplitCJS <- function(mat, CJS, arrays, releases, intra.CJS = NULL) {
  appendTo("debug", "Running assembleSplitCJS.")
  recipient <- lapply(names(CJS), function(i) {
    aux <- releases[releases$Combined == i, ]
    output <- assembleArrayCJS(mat = mat[i], CJS = CJS[[i]], arrays = arrays, releases = aux)[[1]]
    if (!is.null(intra.CJS)) {
      for (i in names(intra.CJS)) {
        output["estimated", i] <- round(output["detected", i] / intra.CJS[[i]]$combined.efficiency, 0)
      }
    }
    output["difference", ] <- output["estimated", ] - output["known", ]
    return(output)
  })
  names(recipient) <- names(CJS)
  return(recipient)
}

#' Assembles CJS tables for all groups
#'
#' @param CJS A list of CJS calculated for each group x array combinations
#' @inheritParams cjs_args
#'
#' @return A data frame containing the CJS results for each group
#'
#' @keywords internal
#'
assembleGroupCJS <- function(mat, CJS, arrays, releases, intra.CJS = NULL) {
  appendTo("debug", "Running assembleGroupCJS.")
  recipient <- lapply(names(CJS), function(i) {
    link <- grepl(paste0("^", i), names(mat))
    aux <- releases[releases$Group == i, ]
    output <- assembleArrayCJS(mat = mat[link], CJS = CJS[[i]], arrays = arrays, releases = aux)[[1]]
    if (!is.null(intra.CJS)) {
      for (i in names(intra.CJS)) {
        output["estimated", i] <- round(output["detected", i] / intra.CJS[[i]]$combined.efficiency, 0)
      }
    }
    output["difference", ] <- output["estimated", ] - output["known", ]
    return(output)
  })
  names(recipient) <- names(CJS)
  return(recipient)
}

#' Break the detection matrices per array
#'
#' @param type The type of arrays to be matched
#' @param verbose Logical: Should appendTo be used?
#' @inheritParams cjs_args
#'
#' @return A list containing the split matrices for each array
#'
#' @keywords internal
#'
breakMatricesByArray <- function(m, arrays, type = c("peers", "all"), verbose = TRUE) {
  appendTo("debug", "Running breakMatricesByArray.")
  type <- match.arg(type)
  recipient <- list()
  for (i in 1:length(arrays)) {
    if ((type == "peers" & !is.null(arrays[[i]]$after.peers)) | (type == "all" & !is.null(arrays[[i]]$all.after))) {
      
      # find out relevant arrays
      if (type == "peers")
        a.regex <- paste0("^", c(names(arrays)[i], arrays[[i]]$after.peers), "$", collapse = "|")
      else
        a.regex <- paste0("^", c(names(arrays)[i], arrays[[i]]$all.after), "$", collapse = "|")
      
      # grab only relevant arrays
      aux  <- lapply(m, function(m_i) m_i[, which(grepl(a.regex, colnames(m_i))), drop = FALSE])
      
      # Failsafe in case some tags are released at one of the peers
      keep <- unlist(lapply(m, function(m_i) any(grepl(paste0("^", names(arrays)[i], "$"), colnames(m_i)))))
      aux  <- aux[keep]
      
      # Failsafe in case there is only one column left
      keep <- unlist(lapply(aux, ncol)) > 1
      aux  <- aux[keep]

      # reorder columns if necessary
      aux <- lapply(aux, function(x) {
        peer.cols <- colnames(x)[!grepl(paste0("^", names(arrays)[i], "$"), colnames(x))]
        return(x[, c(names(arrays)[i], peer.cols)])
      })

      # Convert peers to single column and add fake start
      aux <- lapply(aux, function(m) {
        if (ncol(m) > 2) {
          m[, 2:ncol(m)] <- apply(m[, 2:ncol(m)], 2, as.logical)
          m$AnyPeer <- as.numeric(apply(m[-1], 1, any))
        } else {
          colnames(m)[2] <- "AnyPeer"
        }

        # The fake start prevents the CJS functions from breaking if the efficiency of the array is 0
        m$FakeStart <- rep(1, nrow(m))
        return(m[, c(ncol(m), 1, (ncol(m) - 1))])
      })

      # If all peers are 0, the CJS functions will crash. The same happens if the array is all 0's
      own.zero.check <- unlist(lapply(aux, function(x) sum(x[, 2]) == 0))
      peer.zero.check <- unlist(lapply(aux, function(x) sum(x$AnyPeer) == 0))
      zero.check <- all(own.zero.check) | all(peer.zero.check)
      
      if (zero.check) {
        if (all(own.zero.check) & verbose) {
          appendTo(c("Screen", "Warning", "Report"), paste0("No tags passed through array ", names(arrays)[i], ". Skipping efficiency estimations for this array."))
        } else {
          if (all(peer.zero.check) & verbose)
            appendTo(c("Screen", "Warning", "Report"), paste0("No tags passed through any of the efficiency peers of array ", names(arrays)[i], ". Skipping efficiency estimations for this array."))
          }
      } else {
        recipient[[length(recipient) + 1]] <- aux
        names(recipient)[length(recipient)] <- names(arrays)[i]
      }
    }
  }
  if (length(recipient) == 0 & verbose) {
    appendTo(c("Screen", "Warning", "Report"), "None of the arrays has valid efficiency peers.")
    return(NULL)
  } else {
    return(recipient)
  }
}

#' Combine the individual CJS's of each array into a single table
#'
#' @param CJS A list of CJS calculated for each array
#' @inheritParams cjs_args
#'
#' @return A list containing the CJS absolute numbers and efficiency estimates
#'
#' @keywords internal
#'
assembleArrayCJS <- function(mat, CJS, arrays, releases, silent = TRUE) {
  appendTo("debug", "Running assembleArrayCJS.")
  # Compile final objects
  absolutes <- matrix(nrow = 5, ncol = length(arrays))
  colnames(absolutes) <- names(arrays)
  rownames(absolutes) <- c("detected", "here plus on peers", "not here but on peers", "known", "estimated")
  absolutes <- as.data.frame(absolutes)
  efficiency <- rep(NA, length(arrays))
  names(efficiency) <- names(arrays)
  for (i in 1:length(CJS)) {
    absolutes[, names(CJS)[i]] <- CJS[[i]]$absolutes[, 2]
    efficiency[names(CJS)[i]] <- CJS[[i]]$efficiency[2]
  }
  # Fix max estimated based on arrays before, if needed
  for (i in names(arrays)) {
    # if an estimation is made and previous arrays exist
    if (!is.na(absolutes["estimated", i]) && !is.null(arrays[[i]]$before)) {
      # if estimations were made for all the previous arrays
      if (all(!is.na(absolutes["estimated", arrays[[i]]$before]))) {
        # if tags were included in the system in the target array
        if (any(releases$Array == i))
          # the max is the sum of the estimated for the previous arrays plus the released tags.
          the.max <- sum(absolutes["estimated", arrays[[i]]$before]) + sum(releases$n[releases$Array == i])
        else
          # the max is the sum of the estimated for the previous arrays.
          the.max <- sum(absolutes["estimated", arrays[[i]]$before])
        #if the estimated is too high, correct it.
        if (absolutes["estimated", i] > the.max)
          absolutes["estimated", i] <- the.max
      }
    }
  }
  # Final fixes
  fix.zero <- na.as.false(efficiency == 0)
  fix.peers <- is.na(efficiency)
  for (i in 1:length(arrays)) {
    if (fix.zero[i]) {
      # fix estimated for arrays with 0 efficiency. # if any of the before is NA, this will be NA too.
      if (!silent)
        appendTo(c("Screen", "Report", "Warning"), paste0("Array '", names(arrays)[i], "' has 0 efficiency. Attempting to round estimated based on peers, if possible."))
      if (!is.null(arrays[[i]]$before))
        absolutes["estimated", i] <- sum(absolutes["estimated", arrays[[i]]$before])
    }
    if (fix.peers[i]) {
      # fix absolutes for arrays with no peers
      absolutes["detected", i] <- sum(unlist(lapply(mat, function(m) {
        if (any(colnames(m) == names(arrays)[i]))
          return(sum(m[, names(arrays)[i]]))
      })))
      # Fix knowns for arrays with no peers
      absolutes["known", i] <- absolutes["detected", i]
    }
  }
  return(list(absolutes = absolutes, efficiency = efficiency))
}

#' Assemble detection matrices
#'
#' @inheritParams cjs_args
#'
#' @return A list containing the detection matrices split by groups and release sites
#'
#' @keywords internal
#'
assembleMatrices <- function(spatial, movements, status.df, arrays, paths, dotmat) {
  temp <- efficiencyMatrix(movements = movements, arrays = arrays, paths = paths, dotmat = dotmat)
  appendTo("debug", "Running assembleMatrices.")
  output <- lapply(temp, function(x) {
    # include transmitters that were never detected
    x <- includeMissing(x = x, status.df = status.df)
    
    # sort the rows by the same order as status.df (I think these two lines are not needed, but leaving them in just in case)
    link <- sapply(status.df$Transmitter, function(i) grep(paste0("^", i, "$"), rownames(x)))
    x <- x[link, ]

    # split by group*release site combinations
    aux <- split(x, paste0(status.df$Group, ".", status.df$Release.site))

    # Re-order
    the.order <- c()
    for (i in unique(status.df$Group)) {
      the.order <- c(the.order, paste0(i, ".", unique(spatial$release.sites$Standard.name)))
    }
    aux <- aux[order(match(names(aux), the.order))]

    unique.release.arrays <- unique( # only keep each name once
                              unlist( # turn output into a string
                                sapply(spatial$release.sites$Array, function(x) {
                                    unlist(strsplit(x, "|", fixed = TRUE)) # break arrays by '|'
                                  })
                                )
                              )

    # If the release sites start in different arrays, trim the matrices as needed
    if (length(unique.release.arrays) > 1) {
      for(i in 1:length(aux)){ # for each matrix, find the corresponding release site.
        the_release_site <- sapply(spatial$release.sites$Standard.name, function(x) grepl(paste0(".", x, "$"), names(aux)[i])) 
        if(sum(the_release_site) > 1) # if there is more than one matching release site, stop.
          stop("Multiple release sites match the matrix name. Make sure that the release sites' names are not contained within the animal groups or within themselves.\n")
        # else, find which is the first column to keep. This is tricky for multi-branch sites...
        the.col <- min(which(grepl(spatial$release.sites$Array[the_release_site], colnames(aux[[i]]))))
        # then keep only the relevant columns
        aux[[i]] <- aux[[i]][, c(1, the.col:ncol(aux[[i]]))]
      }
    }
    return(aux)
  })
  return(output)
}

#' Analytical CJS model
#'
#' Computes an analytical CJS model for a presence/absence matrix.
#'
#' @references Perry et al (2012), 'Using mark-recapture models to estimate survival from telemetry data'. url: <https://www.researchgate.net/publication/256443823_Using_mark-recapture_models_to_estimate_survival_from_telemetry_data>
#'
#' @inheritParams cjs_args
#'
#' @examples
#' # prepare a dummy presence/absence matrix
#' x <- matrix(c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE), ncol = 3)
#' colnames(x) <- c("Release", "Array1", "Array2")
#'
#' # run CJS
#' simpleCJS(x)
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{absolutes} A data frame with the absolute number of tags detected and missed,
#'   \item \code{efficiency} A vector of calculated array detection efficiencies,
#'   \item \code{survival} A matrix of calculated survivals,
#'   \item \code{lambda} A combined detection efficiency * survival estimate for the last array.
#' }
#'
#' @export
#'
simpleCJS <- function(input, estimate = NULL, fixed.efficiency = NULL, silent = TRUE){
  if (!is.matrix(input) & !is.data.frame(input))
    stop("input must be a matrix or data frame containing only 0's and 1's.\n")

  # stop if there is weird data in the input
  if (any(input != 0 & input != 1))
    stop("input must be a matrix or data frame containing only 0's and 1's.\n")

  # stop if both estimate and fixed efficiency are present
  if (!is.null(estimate) & !is.null(fixed.efficiency))
    stop("Please choose only one of 'estimate' or 'fixed.efficiency'.\n")

  if (any(input[, 1] == 0))
    stop("The first column of the input should only contain 1's (i.e. release point).\n")

  # Only check below if estimate is set
  if (!is.null(estimate)) {
    # stop if multiple estimate values are provided.
    if (!is.null(estimate) && length(estimate) != 1)
      stop("Please use only one value for estimate.\n")
    # stop if estimate exceeds 1
    if (!is.null(estimate) && (estimate < 0 | estimate > 1))
      stop("'estimate' must be between 0 and 1.\n")
    # all good
  }

  # Only check below if fixed.efficiency was set
  if (!is.null(fixed.efficiency)) {
    # stop if there are not enough efficiency values
    if (length(fixed.efficiency) != ncol(input))
     stop("Fixed efficiency was set but its length is not the same as the number of columns in the input.\n")
    # stop if any efficiency exceeds 1
    if (any(fixed.efficiency > 1, na.rm = TRUE))
      stop("Fixed efficiency estimates must be between 0 and 1.\n")
    # Fake estimate to avoid further changes below.
    if (!is.na(tail(fixed.efficiency, 1)))
     estimate <- tail(fixed.efficiency, 1)
    # all good
    if(!silent)
      message("M: Running CJS with fixed efficiency estimates."); flush.console()
  }

  # Start the calculations

  # S: Survival probability between arrays
  # m: tags detected at i
  # r: tags detected at i and on peers
  # z: tags NOT detected at i but detected on peers
  # p: probability of detection (efficiency)
  # M: animals estimated to be alive at i

  S <- rep(NA, ncol(input) - 1)
  r <- z <- p <- m <- M <- rep(NA, ncol(input))
  for(i in 1:(ncol(input) - 1)){
    # number of tags detected at i and on peers (r)
    tr <- input[input[, i] == 1, (i + 1):ncol(input)]
    if (is.data.frame(tr) | is.matrix(tr))
      r[i] = sum(apply(tr, 1, function(f) any(f == 1)))
    if (is.vector(tr))
      r[i] = sum(tr)
    # number of tags NOT detected at i but detected on peers (z)
    tz <- input[input[, i] == 0, (i + 1):ncol(input)]
    if (is.data.frame(tz) | is.matrix(tz))
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
    # number of animals estimated alive at i (M)
    # Failsafe for array with 0 efficiency. Issues warning.
    if (p[i] == 0 | m[i] == 0) {
      if(p[i] == 0 & !silent)
        warning("Array '", colnames(input)[i],"' had 0% efficiency. Skipping survival estimation.")
      if(m[i] == 0 & !silent)
        warning("No tags were detected at array '", colnames(input)[i],"'. Skipping survival estimation.")
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
      S[ncol(input) - 1] <- -999
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
  k <- sapply(1:length(m), function(i) sum(c(m[i], z[i]), na.rm = TRUE))
  absolutes <- matrix(c(m, r, z, k, M), ncol = ncol(input), byrow = TRUE)
  rownames(absolutes) <- c("detected", "here plus on peers", "not here but on peers", "known", "estimated")
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
    the.rows[length(the.rows) + 1] <- paste0(centeringA, the.names[i], " -> ", the.names[i + 1], centeringB, " =")
  }
  rownames(survival) <- the.rows
  colnames(survival) <- ""

  return(list(absolutes = absolutes, efficiency = p, survival = survival, lambda = l))
}

#' Calculate CJS for each group.release combination
#'
#' @inheritParams cjs_args
#'
#' @return A list containing CJS results split by group and release site
#'
#' @keywords internal
#'
mbSplitCJS <- function(mat, fixed.efficiency = NULL) {
  appendTo("debug", "Running mbSplitCJS.")
  recipient <- lapply(mat, function(m) {
    if (is.na(fixed.efficiency[grepl(paste0("^", colnames(m[[1]])[2], "$"), names(fixed.efficiency))]))
      array.efficiency <- NULL
    else
      array.efficiency <- c(1, fixed.efficiency[grepl(paste0("^", colnames(m[[1]])[2], "$"), names(fixed.efficiency))], 1)
    output <- lapply(m, function(mm) {
      simpleCJS(mm, fixed.efficiency = array.efficiency, silent = TRUE)
    })
    return(output)
  })
  groups <- unique(unlist(lapply(recipient, names)))
  output <- list()
  for (group in groups) {
    output[[length(output) + 1]] <- lapply(recipient, function(r) {
      if (any(grepl(group, names(r))))
        return(r[[which(grepl(group, names(r)))]])
      else
        return(NULL)
    })
  }
  output <- lapply(output, function(x) {
    keep <- !unlist(lapply(x, is.null))
    return(x[keep])
  })
  names(output) <- groups
  return(output)
}

#' Calculate CJS for each group for each array
#'
#' @inheritParams cjs_args
#'
#' @return A list containing CJS results split by group.
#'
#' @keywords internal
#'
mbGroupCJS <- function(mat, status.df, fixed.efficiency = NULL) {
  appendTo("debug", "Running mbGroupCJS.")
  output <- list()
  for (i in 1:length(unique(status.df$Group))) {
    output[[i]] <- lapply(mat, function(m_i) {
      if (is.na(fixed.efficiency[grepl(paste0("^", colnames(m_i[[1]])[2], "$"), names(fixed.efficiency))]))
        array.efficiency <- NULL
      else
        array.efficiency <- c(1, fixed.efficiency[grepl(paste0("^", colnames(m_i[[1]])[2], "$"), names(fixed.efficiency))], 1)
      link <- grepl(paste0("^", unique(status.df$Group)[i]), names(m_i))
      if (any(link)) {
        if(sum(link) == 1)
          return(simpleCJS(m_i[[which(link)]], fixed.efficiency = array.efficiency, silent = TRUE))
        else
          return(combineCJS(m_i[link], fixed.efficiency = array.efficiency, silent = TRUE))
      } else {
        return(NULL)
      }
    })
    names(output[[i]]) <- names(mat)
  }
  output <- lapply(output, function(x) x[!unlist(lapply(x, is.null))])
  names(output) <- unique(status.df$Group)
  return(output)
}

#' Compile inter-array detection matrix
#'
#' @inheritParams cjs_args
#'
#' @return A matrix of detection histories per tag.
#'
#' @keywords internal
#'
efficiencyMatrix <- function(movements, arrays, paths, dotmat) {
  appendTo("debug", "Starting efficiencyMatrix.")
  max.ef <- as.data.frame(matrix(ncol = length(arrays) + 1, nrow = length(movements)))
  colnames(max.ef) <- c("Release", names(arrays))
  rownames(max.ef) <- names(movements)
  max.ef[is.na(max.ef)] = 0
  max.ef$Release = 1
  min.ef <- max.ef

  capture <- lapply(names(movements), function(tag) {
    max.aux <- c(1, rep(0, length(arrays)))
    names(max.aux) <- c("Release", names(arrays))
    one.way <- oneWayMoves(movements = movements[[tag]], arrays = arrays)
    if (!is.null(one.way)) {
      max.aux[match(one.way$Array, names(max.aux))] <- 1
      min.aux <- max.aux
      if (nrow(one.way) > 1) {
        aux <- countArrayFailures(moves = one.way, paths = paths, dotmat = dotmat)
        aux <- unique(aux[grepl("unsure", names(aux))])
        if (!is.null(aux))
          max.aux[match(aux, names(max.aux))] <- 1
      }
      max.ef[tag, ] <<- max.aux
      min.ef[tag, ] <<- min.aux
    }
  })
  return(list(maxmat = max.ef, minmat = min.ef))
}

#' Trim movements table to contain only uni-directional movements
#'
#' @inheritParams cjs_args
#'
#' @return A data frame with the uni-directional movements for the target tags.
#'
#' @keywords internal
#'
oneWayMoves <- function(movements, arrays) {
  appendTo("debug", "Running oneWayMoves.")
  if (nrow(movements) > 1) {
    while (TRUE) {
      aux <- data.frame(from = movements$Array[-nrow(movements)], to = movements$Array[-1])
      link <- apply(aux, 1, function(x) x[2] %in% arrays[[x[1]]]$all.after)
      if (any(link)) {
        if (all(link))
          return(movements)
        else
          movements <- movements[c(TRUE, link), ]
      } else {
        return(movements[1, , drop = FALSE])
      }
    }
  } else {
    return(movements)
  }
}

#' Find and list arrays which failed during the movements of the tags
#'
#' @param moves the valid array movements
#' @inheritParams res_efficiency
#' @inheritParams dotPaths
#'
#' @return NULL if no arrays failed, or a list containing information on the arrays that failed
#'
#' @keywords internal
#'
countArrayFailures <- function(moves, paths, dotmat) {
  appendTo("debug", "Running countArrayFailures.")
  x <- lapply(1:(nrow(moves) - 1), function(i) {
    A <- moves$Array[i]
    B <- moves$Array[i + 1]
    if (A != B & dotmat[A, B] != 1)
      blameArrays(from = A, to = B, paths = paths)
    else
      NULL
  })
  return(unlist(x))
}

#' Find which arrays to blame for a jump in movement events
#'
#' @param from The array where the tag started
#' @param to The array where the tag was next detected
#' @inheritParams res_efficiency
#'
#' @return A list containing information on the arrays that failed
#'
#' @keywords internal
#'
blameArrays <- function(from, to, paths) {
  appendTo("debug", "Running blameArrays.")
  the.paths <- paths[[paste0(from, "_to_", to)]]
  if (is.null(the.paths))
    stop("Either 'from' is not connected to 'to', or both are neighbours.\n")
  if (length(the.paths) == 1) {
    return(list(known = unique(unlist(strsplit(the.paths, " -> ")))))
  } else {
    aux <- strsplit(the.paths, " -> ")
    aux <- lapply(aux, unique)
    combined.unsure <- unique(unlist(aux))
    ocurrences <- table(unlist(aux))
    if (any(ocurrences == length(aux))) {
      combined.knowns <- names(ocurrences)[ocurrences == length(aux)]
      combined.unsure <- combined.unsure[!combined.unsure %in% combined.knowns]
    } else {
      combined.knowns <- NULL
    }
    return(list(known = combined.knowns, unsure = combined.unsure))
  }
}

#' Include tags that were never detected
#'
#' @param x an efficiency matrix
#' @inheritParams cjs_args
#'
#' @return A matrix of detection histories per tag, including tags that were never detected.
#'
#' @keywords internal
#'
includeMissing <- function(x, status.df){
  appendTo("debug", "Running includeMissing.")
  missing_transmitters <- status.df$Transmitter[!status.df$Transmitter %in% rownames(x)]
  x[missing_transmitters, ] <- 0
  x[missing_transmitters, 1] <- 1
  return(x)
}

#' Compile detection matrix for last array
#'
#' @inheritParams cjs_args
#'
#' @return A matrix of detection histories per tag for the last array.
#'
#' @keywords internal
#'
dualMatrix <- function(array, replicates, spatial, detections.list){
  appendTo("debug", "Running dualMatrix.")
  all.stations <- spatial$stations$Standard.name[spatial$stations$Array == array]
  original <- all.stations[!all.stations %in% replicates]
  efficiency <- as.data.frame(matrix(ncol = 2, nrow = length(detections.list)))
  colnames(efficiency) <- c("original","replicates")
  rownames(efficiency) <- names(detections.list)
  for (i in 1:length(detections.list)) {
    efficiency[i, "original"] <- any(!is.na(match(original, detections.list[[i]]$Standard.name)))
    efficiency[i, "replicates"] <- any(!is.na(match(replicates, detections.list[[i]]$Standard.name)))
  }
  colnames(efficiency) <- c("R1", "R2")
  return(efficiency)
}

#' Calculate estimated last-array efficiency
#'
#' @references Perry et al (2012), 'Using mark-recapture models to estimate survival from telemetry data'. url: <https://www.researchgate.net/publication/256443823_Using_mark-recapture_models_to_estimate_survival_from_telemetry_data>
#'
#' @inheritParams cjs_args
#'
#' @examples
#' # prepare a dummy presence/absence matrix
#' x <- matrix(c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE), ncol = 2)
#' colnames(x) <- c("R1", "R2")
#'
#' # run CJS
#' dualArrayCJS(x)
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{absolutes} A matrix with the absolute number of tags detected at each replicate and at both,
#'   \item \code{single.efficiency} A vector of calculated array detection efficiencies for each of the replicates,
#'   \item \code{combined.efficiency} The value of the combined detection efficiency for the array.
#' }
#'
#' @export
#'
dualArrayCJS <- function(input){
  appendTo("debug", "Running dualArrayCJS.")
  if ((!inherits(input, "matrix") & !inherits(input, "data.frame")) || ncol(input) != 2 | any(!apply(input, 2, is.logical)))
    stop("Please provide a data.frame or matrix of TRUE/FALSE's with two columns", call. = FALSE)

  if (is.null(colnames(input)))
    colnames(input) <- c("R1", "R2")
  # r: tags detected at i and on other pair
  # z: tags NOT detected at i but detected on other pair
  # p: probability of detection (efficiency)
  r <- z <- p <- rep(NA, 2)
  for(i in 1:2){
    # number of tags detected at i and elsewhere (r)
    tr <- input[input[, i] == 1, -i]
    if (is.vector(tr))
      r[i] = sum(tr)
    # number of tags NOT detected at i but detected elsewhere (z)
    tz <- input[input[, i] == 0, -i]
    if (is.vector(tz))
      z[i] = sum(tz)
    # probability of detection (p)
    p[i] <- r[i] / (r[i] + z[i])
  }
  combined.p <- 1 - prod(1 - p)
  absolutes <- matrix(c(apply(input, 2, sum), r[1]), nrow = 3)
  colnames(absolutes) <- ""
  rownames(absolutes) <- c(paste0("detected at ", colnames(input)[1], ": "), paste0("detected at ", colnames(input)[2], ": "), "detected at both: ")
  names(p) <- colnames(input)
  return(list(absolutes = absolutes, single.efficiency = p, combined.efficiency = combined.p))
}

#' Combine multiple CJS models
#'
#' @param ... The detection matrices to be joined, or a list containing detection matrices
#' @inheritParams cjs_args
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{absolutes} A data frame with the absolute number of tags detected and missed,
#'   \item \code{efficiency} A vector of calculated array detection efficiencies,
#'   \item \code{survival} A matrix of calculated survivals,
#'   \item \code{lambda} A combined detection efficiency * survival estimate for the last array.
#' }
#'
#' @keywords internal
#'
combineCJS <- function(..., estimate = NULL, fixed.efficiency = NULL, silent = FALSE){
  appendTo("debug", "Running combineCJS.")
  # stop if both estimate and fixed efficiency are present
  if (!is.null(estimate) & !is.null(fixed.efficiency))
    stop("Please choose only one of 'estimate' or 'fixed.efficiency'.\n")

  # stop if multiple estimate values are provided.
  if (!is.null(estimate) && length(estimate) != 1)
    stop("Please use only one value for estimate.\n")

  # stop if any efficiency exceeds 1
  if (!is.null(fixed.efficiency) && any(fixed.efficiency > 1 | fixed.efficiency < 0, na.rm = TRUE))
    stop("Fixed efficiency estimates must be between 0 and 1.\n")

  if (!is.null(estimate) && (estimate > 1 | estimate < 0))
    stop("'estimate' must be between 0 and 1.\n")

  # hack to figure the number of arguments in the dots, because I could not find a better way around it.
  arg.names <- names(match.call())
  if (is.null(arg.names))
    discount <- 1
  else
    discount <- 1 + sum(!is.na(match(c("estimate", "fixed.efficiency", "silent"), arg.names)))

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
  }
  if(any(unlist(lapply(input, function(x) !is.matrix(x) & !is.data.frame(x)))))
    stop("Not all objects provided are matrices or data frames. Please use either one list of matrices/data frames or multiple matrices/data frames.\n")

  # stop if not all matrices finish in the same array
  the.last <- unlist(lapply(input, function(x) tail(colnames(x),1)))
  if (length(unique(the.last)) > 1)
    stop("The last array is not the same in all input matrices.\n")

  ncols <- sapply(input, ncol)
  groups <- rev(sort(unique(ncols)))

  if (!is.null(fixed.efficiency)){
    # stop if there are not enough efficiency values
    if (length(fixed.efficiency) != max(sapply(input, ncol))) {
      stop("Fixed efficiency was set but its length is not the same as the maximum number of columns in the input.\n")
    } else {
      if (!silent)
        message("M: Running CJS with fixed efficiency values."); flush.console()
    }
  }

  combinedmatrices <- as.data.frame(data.table::rbindlist(input))
  return(simpleCJS(combinedmatrices, estimate = estimate, fixed.efficiency = fixed.efficiency))
}
