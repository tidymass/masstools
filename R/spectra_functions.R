### getSpectraMatchScore is used to get
### two MS2 spectra match score, see MS-DIAL
#' @title getSpectraMatchScore
#' @param exp.spectrum exp.spectrum
#' @param lib.spectrum lib.spectrum
#' @param ppm.tol ppm.tol
#' @param mz.ppm.thr mz.ppm.thr
#' @param fraction.weight fraction.weight
#' @param dp.forward.weight dp.forward.weight
#' @param dp.reverse.weight dp.reverse.weight
#' @return spectrum match score
#' @export
#' @examples
#' exp.spectrum <- data.frame(mz = 1:10, intensity = 1:10)
#' lib.spectrum <- data.frame(mz = 1:10, intensity = 1:10)
#' getSpectraMatchScore(exp.spectrum, lib.spectrum)
getSpectraMatchScore <- function(exp.spectrum,
                                 lib.spectrum,
                                 ppm.tol = 30,
                                 mz.ppm.thr = 400,
                                 fraction.weight = 0.2,
                                 dp.forward.weight = 0.7,
                                 dp.reverse.weight = 0.1) {
  # .Deprecated(new = "get_spectra_match_score")
  lifecycle::deprecate_soft(when = "0.99.9",
                            what = "getSpectraMatchScore()",
                            with = "get_spectra_match_score()")
  exp.spectrum <- as.data.frame(exp.spectrum)
  lib.spectrum <- as.data.frame(lib.spectrum)
  
  exp.spectrum$intensity <-
    exp.spectrum$intensity / max(exp.spectrum$intensity)
  lib.spectrum$intensity <-
    lib.spectrum$intensity / max(lib.spectrum$intensity)
  
  match.matrix <- ms2_match(
    exp.spectrum = exp.spectrum,
    lib.spectrum = lib.spectrum,
    ppm.tol = ppm.tol,
    mz.ppm.thr = mz.ppm.thr
  )
  
  fraction <-
    sum(!is.na(match.matrix$Lib.index) &
          !is.na(match.matrix$Exp.index)) / nrow(match.matrix)
  
  dp.forward <- get_dp(exp.int = match.matrix$Exp.intensity,
                       lib.int = match.matrix$Lib.intensity)
  dp.reverse <-
    get_dp(exp.int =
             match.matrix$Exp.intensity[which(match.matrix$Lib.intensity > 0)],
           lib.int =
             match.matrix$Lib.intensity[which(match.matrix$Lib.intensity > 0)])
  dp.forward[is.na(dp.forward)] <- 0
  dp.reverse[is.na(dp.reverse)] <- 0
  score <-
    dp.forward * dp.forward.weight + dp.reverse * dp.reverse.weight +
    fraction * fraction.weight
  return(score)
}


#' @title get_spectra_match_score
#' @description get_spectra_match_score is used to get two
#' MS2 spectra match score, see MS-DIAL
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param exp.spectrum exp.spectrum
#' @param lib.spectrum lib.spectrum
#' @param ppm.tol ppm.tol
#' @param mz.ppm.thr mz.ppm.thr
#' @param fraction.weight fraction.weight
#' @param dp.forward.weight dp.forward.weight
#' @param dp.reverse.weight dp.reverse.weight
#' @param remove_fragment_intensity_cutoff default is 0 (100 percentage).
#' @return spectrum match score
#' @export
#' @examples
#' exp.spectrum <- data.frame(mz = 1:10, intensity = 1:10)
#' lib.spectrum <- data.frame(mz = 1:10, intensity = 1:10)
#' get_spectra_match_score(exp.spectrum, lib.spectrum)

get_spectra_match_score <-
  function(exp.spectrum,
           lib.spectrum,
           ppm.tol = 30,
           mz.ppm.thr = 400,
           fraction.weight = 0.2,
           dp.forward.weight = 0.7,
           dp.reverse.weight = 0.1,
           remove_fragment_intensity_cutoff = 0) {
    exp.spectrum <- as.data.frame(exp.spectrum)
    lib.spectrum <- as.data.frame(lib.spectrum)
    
    exp.spectrum$intensity <-
      exp.spectrum$intensity / max(exp.spectrum$intensity)
    lib.spectrum$intensity <-
      lib.spectrum$intensity / max(lib.spectrum$intensity)
    
    exp.spectrum <-
      exp.spectrum %>%
      dplyr::filter(intensity > remove_fragment_intensity_cutoff)
    
    lib.spectrum <-
      lib.spectrum %>%
      dplyr::filter(intensity > remove_fragment_intensity_cutoff)
    
    match.matrix <- ms2_match(
      exp.spectrum = exp.spectrum,
      lib.spectrum = lib.spectrum,
      ppm.tol = ppm.tol,
      mz.ppm.thr = mz.ppm.thr
    )
    
    fraction <-
      sum(!is.na(match.matrix$Lib.index) &
            !is.na(match.matrix$Exp.index)) / nrow(match.matrix)
    
    dp.forward <- get_dp(exp.int = match.matrix$Exp.intensity,
                         lib.int = match.matrix$Lib.intensity)
    dp.reverse <-
      get_dp(exp.int =
               match.matrix$Exp.intensity[which(match.matrix$Lib.intensity > 0)],
             lib.int =
               match.matrix$Lib.intensity[which(match.matrix$Lib.intensity > 0)])
    dp.forward[is.na(dp.forward)] <- 0
    dp.reverse[is.na(dp.reverse)] <- 0
    score <-
      dp.forward * dp.forward.weight + dp.reverse * dp.reverse.weight +
      fraction * fraction.weight
    return(score)
  }

##### getDP is used to calculate dot product
#' @title getDP
#' @param exp.int exp.int
#' @param lib.int lib.int
#' @return dot product
#' @export
#' @examples
#' getDP(exp.int = 1:10, lib.int = 1:10)
getDP <- function(exp.int, lib.int) {
  # .Deprecated("get_dp")
  lifecycle::deprecate_soft(when = "0.99.9",
                            what = "getDP()",
                            with = "get_dp()")
  exp.weight <- lapply(exp.int, function(x) {
    1 / (1 + x / (sum(exp.int) - 0.5))
  }) %>%
    unlist()
  
  lib.weight <- lapply(lib.int, function(x) {
    1 / (1 + x / (sum(lib.int) - 0.5))
  }) %>%
    unlist()
  
  x <- exp.weight * exp.int
  y <- lib.weight * lib.int
  return(sum(x * y) ^ 2 / (sum(x ^ 2) * sum(y ^ 2)))
}





##### get_dp is used to calculate dot product
#' @title get_dp
#' @param exp.int exp.int
#' @param lib.int lib.int
#' @return dot product
#' @export
#' @examples
#' get_dp(exp.int = 1:10, lib.int = 1:10)
get_dp <- function(exp.int, lib.int) {
  exp.weight <- lapply(exp.int, function(x) {
    1 / (1 + x / (sum(exp.int) - 0.5))
  }) %>%
    unlist()
  
  lib.weight <- lapply(lib.int, function(x) {
    1 / (1 + x / (sum(lib.int) - 0.5))
  }) %>%
    unlist()
  
  x <- exp.weight * exp.int
  y <- lib.weight * lib.int
  return(sum(x * y) ^ 2 / (sum(x ^ 2) * sum(y ^ 2)))
}




### ms2Match is used to match two MS2 spectra
#' @title ms2Match
#' @param exp.spectrum exp.spectrum
#' @param lib.spectrum lib.spectrum
#' @param ppm.tol ppm.tol
#' @param mz.ppm.thr mz.ppm.thr
#' @export
#' @return ms2 match data frame
#' @examples
#' exp.spectrum <- data.frame(mz = 1:10, intensity = 1:10)
#' lib.spectrum <- data.frame(mz = 1:10, intensity = 1:10)
#' ms2Match(exp.spectrum, lib.spectrum)
ms2Match <- function(exp.spectrum,
                     lib.spectrum,
                     ppm.tol = 30,
                     mz.ppm.thr = 400) {
  # .Deprecated(new = "ms2_match")
  lifecycle::deprecate_soft(when = "0.99.9",
                            what = "ms2Match()",
                            with = "ms2_match()")
  ## remove noisy fragments
  exp.spectrum <- remove_noise(spec = exp.spectrum,
                               ppm.ms2match = ppm.tol,
                               mz.ppm.thr = mz.ppm.thr)
  lib.spectrum <- remove_noise(spec = lib.spectrum,
                               ppm.ms2match = ppm.tol,
                               mz.ppm.thr = mz.ppm.thr)
  
  ## for each fragment in lib.spectrum,
  ## its matched fragments index in exp.spectrum
  match.idx <- lapply(lib.spectrum$mz, function(x) {
    diff.mz <- abs(x - exp.spectrum$mz)
    x[x < mz.ppm.thr] <- mz.ppm.thr
    mz.error <- diff.mz * 10 ^ 6 / x
    temp.idx <- which(mz.error < ppm.tol)
    if (length(temp.idx) == 0) {
      return(NA)
    }
    if (length(temp.idx) > 1) {
      return(temp.idx[which.max(exp.spectrum$intensity[temp.idx])])
    }
    return(temp.idx)
  })
  
  match.idx <- do.call(rbind, match.idx)
  match.idx <- cbind(seq_len(nrow(match.idx)), match.idx)
  colnames(match.idx) <- c("Lib", "Exp")
  
  non.idx2 <-
    setdiff(c(seq_len(nrow(exp.spectrum))), match.idx[, 2][!is.na(match.idx[, 2])])
  
  if (length(non.idx2) != 0) {
    match.idx2 <- data.frame(NA, non.idx2, stringsAsFactors = FALSE)
    colnames(match.idx2) <- c("Lib", "Exp")
  } else {
    match.idx2 <- NULL
  }
  
  match.matrix <-
    as.data.frame(rbind(match.idx, match.idx2), stringsAsFactors = FALSE)
  
  
  match.matrix <- data.frame(match.matrix,
                             lib.spectrum[match.matrix$Lib, c(1, 2)],
                             exp.spectrum[match.matrix$Exp, c(1, 2)])
  colnames(match.matrix) <-
    c("Lib.index",
      "Exp.index",
      "Lib.mz",
      "Lib.intensity",
      "Exp.mz",
      "Exp.intensity")
  
  match.matrix$Lib.intensity[is.na(match.matrix$Lib.intensity)] <-
    0
  match.matrix$Exp.intensity[is.na(match.matrix$Exp.intensity)] <-
    0
  rownames(match.matrix) <- NULL
  match.matrix
}





### ms2_match is used to match two MS2 spectra
#' @title ms2_match
#' @param exp.spectrum exp.spectrum
#' @param lib.spectrum lib.spectrum
#' @param ppm.tol ppm.tol
#' @param mz.ppm.thr mz.ppm.thr
#' @export
#' @return ms2 match data frame
#' @examples
#' exp.spectrum <- data.frame(mz = 1:10, intensity = 1:10)
#' lib.spectrum <- data.frame(mz = 1:10, intensity = 1:10)
#' ms2_match(exp.spectrum, lib.spectrum)
ms2_match <- function(exp.spectrum,
                      lib.spectrum,
                      ppm.tol = 30,
                      mz.ppm.thr = 400) {
  ## remove noisy fragments
  exp.spectrum <- remove_noise(spec = exp.spectrum,
                               ppm.ms2match = ppm.tol,
                               mz.ppm.thr = mz.ppm.thr)
  lib.spectrum <- remove_noise(spec = lib.spectrum,
                               ppm.ms2match = ppm.tol,
                               mz.ppm.thr = mz.ppm.thr)
  
  ## for each fragment in lib.spectrum,
  ## its matched fragments index in exp.spectrum
  match.idx <- lapply(lib.spectrum$mz, function(x) {
    diff.mz <- abs(x - exp.spectrum$mz)
    x[x < mz.ppm.thr] <- mz.ppm.thr
    mz.error <- diff.mz * 10 ^ 6 / x
    temp.idx <- which(mz.error < ppm.tol)
    if (length(temp.idx) == 0) {
      return(NA)
    }
    if (length(temp.idx) > 1) {
      return(temp.idx[which.max(exp.spectrum$intensity[temp.idx])])
    }
    return(temp.idx)
  })
  
  match.idx <- do.call(rbind, match.idx)
  match.idx <- cbind(seq_len(nrow(match.idx)), match.idx)
  colnames(match.idx) <- c("Lib", "Exp")
  
  non.idx2 <-
    setdiff(c(seq_len(nrow(exp.spectrum))), match.idx[, 2][!is.na(match.idx[, 2])])
  
  if (length(non.idx2) != 0) {
    match.idx2 <- data.frame(NA, non.idx2, stringsAsFactors = FALSE)
    colnames(match.idx2) <- c("Lib", "Exp")
  } else {
    match.idx2 <- NULL
  }
  
  match.matrix <-
    as.data.frame(rbind(match.idx, match.idx2), stringsAsFactors = FALSE)
  
  
  match.matrix <- data.frame(match.matrix,
                             lib.spectrum[match.matrix$Lib, c(1, 2)],
                             exp.spectrum[match.matrix$Exp, c(1, 2)])
  colnames(match.matrix) <-
    c("Lib.index",
      "Exp.index",
      "Lib.mz",
      "Lib.intensity",
      "Exp.mz",
      "Exp.intensity")
  
  match.matrix$Lib.intensity[is.na(match.matrix$Lib.intensity)] <- 0
  match.matrix$Exp.intensity[is.na(match.matrix$Exp.intensity)] <- 0
  rownames(match.matrix) <- NULL
  match.matrix
}



### for a spectrum, if two fragments with the similar m/z,
### the fragment with the low fragment should be removed from
## the spectrum
#' @title removeNoise
#' @param spec spec
#' @param ppm.ms2match ppm.ms2match
#' @param mz.ppm.thr mz.ppm.thr
#' @return clear spec
#' @export
#' @examples
#' exp.spectrum <- data.frame(mz = 1:10, intensity = 1:10)
#' removeNoise(exp.spectrum)
removeNoise <- function(spec,
                        ppm.ms2match = 30,
                        mz.ppm.thr = 400) {
  # .Deprecated("remove_noise")
  lifecycle::deprecate_soft(when = "0.99.9",
                            what = "removeNoise()",
                            with = "remove_noise()")
  if (nrow(spec) == 1) {
    return(spec)
  }
  spec <- spec[order(spec[, 1]),]
  mz <- spec[, 1]
  mz <- mz[-1]
  diff.mz <- diff(spec[, 1])
  mz[which(mz < mz.ppm.thr)] <- mz.ppm.thr
  mz.error <- diff.mz * 10 ^ 6 / mz
  temp.idx <- which(mz.error < ppm.ms2match)
  if (length(temp.idx) > 0) {
    remove.idx <- lapply(temp.idx, function(idx) {
      c(idx, idx + 1)[which.min(spec[c(idx, idx + 1), 2])]
    })
    
    remove.idx <- unique(unlist(remove.idx))
    spec <- spec[-remove.idx, , drop = FALSE]
  } else {
    return(spec)
  }
}





### for a spectrum, if two fragments with the similar m/z,
### the fragment with the low fragment should be removed from
## the spectrum
#' @title remove_noise
#' @param spec spec
#' @param ppm.ms2match ppm.ms2match
#' @param mz.ppm.thr mz.ppm.thr
#' @return clear spec
#' @export
#' @examples
#' exp.spectrum <- data.frame(mz = 1:10, intensity = 1:10)
#' remove_noise(exp.spectrum)
remove_noise <- function(spec,
                         ppm.ms2match = 30,
                         mz.ppm.thr = 400) {
  if (nrow(spec) == 1) {
    return(spec)
  }
  spec <- spec[order(spec[, 1]),]
  mz <- spec[, 1]
  mz <- mz[-1]
  diff.mz <- diff(spec[, 1])
  mz[which(mz < mz.ppm.thr)] <- mz.ppm.thr
  mz.error <- diff.mz * 10 ^ 6 / mz
  temp.idx <- which(mz.error < ppm.ms2match)
  if (length(temp.idx) > 0) {
    remove.idx <- lapply(temp.idx, function(idx) {
      c(idx, idx + 1)[which.min(spec[c(idx, idx + 1), 2])]
    })
    
    remove.idx <- unique(unlist(remove.idx))
    spec <- spec[-remove.idx, , drop = FALSE]
  } else {
    return(spec)
  }
}
