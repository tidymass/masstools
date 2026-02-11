#' Compute Similarity Score Between Two Mass Spectra
#'
#' This function computes the similarity score between two mass spectra.
#' The similarity is calculated based on a weighted sum of the fraction of
#' matching peaks and dot products (forward and reverse).
#'
#' @param exp.spectrum A data frame representing the experimental spectrum.
#'   This data frame should contain columns for 'mz' and 'intensity'.
#' @param lib.spectrum A data frame representing the library spectrum.
#'   This data frame should contain columns for 'mz' and 'intensity'.
#' @param ppm.tol A numeric value indicating the ppm tolerance.
#' @param mz.ppm.thr A numeric value for m/z threshold for ppm calculation.
#' @param fraction.weight A numeric value for weight of fraction component in score.
#' @param dp.forward.weight A numeric value for weight of forward dot product component in score.
#' @param dp.reverse.weight A numeric value for weight of reverse dot product component in score.
#'
#' @return A numeric value representing the similarity score between
#'   \code{exp.spectrum} and \code{lib.spectrum}.
#'
#' @details
#' The function scales the intensities of both spectra to a range of 0 to 1,
#' identifies matching peaks, and computes a weighted similarity score using
#' fraction of matching peaks and dot products. The function is deprecated,
#' and users should transition to using the new function.
#'
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


#' Compute Similarity Score Between Two Mass Spectra
#'
#' This function computes the similarity score between two mass spectra
#' considering an option to filter out lower fragment intensities.
#' Supports both data.frame and Spectra object inputs.
#'
#' @param exp.spectrum The experimental spectrum. Can be:
#'   \itemize{
#'     \item A data frame with 'mz' and 'intensity' columns
#'     \item A Spectra object from the Spectra package
#'   }
#' @param lib.spectrum The library/reference spectrum. Can be:
#'   \itemize{
#'     \item A data frame with 'mz' and 'intensity' columns
#'     \item A Spectra object from the Spectra package
#'   }
#' @param ppm.tol A numeric value indicating the ppm tolerance.
#' @param mz.ppm.thr A numeric value for m/z threshold for ppm calculation.
#' @param fraction.weight A numeric value for weight of fraction component in score.
#' @param dp.forward.weight A numeric value for weight of forward dot product component in score.
#' @param dp.reverse.weight A numeric value for weight of reverse dot product component in score.
#' @param remove_fragment_intensity_cutoff A numeric value specifying the intensity cutoff for filtering out fragments in the spectra. Default is 0, which means no fragments are filtered out based on intensity.
#'
#' @return A numeric value representing the similarity score between
#'   \code{exp.spectrum} and \code{lib.spectrum}.
#'
#' @details
#' The function scales the intensities of both spectra to a range of 0 to 1,
#' identifies matching peaks, and computes a weighted similarity score using
#' fraction of matching peaks and dot products. Additionally, fragments below
#' the specified intensity cutoff can be removed from the spectra before matching.
#' @export
#' @examples
#' exp.spectrum <- data.frame(mz = 1:10, intensity = 1:10)
#' lib.spectrum <- data.frame(mz = 1:10, intensity = 1:10)
#' get_spectra_match_score(exp.spectrum, lib.spectrum)
#' 
#' # Works with Spectra objects too
#' \dontrun{
#' library(Spectra)
#' exp_spec <- df_to_spectra(exp.spectrum)
#' lib_spec <- df_to_spectra(lib.spectrum)
#' score <- get_spectra_match_score(exp_spec, lib_spec)
#' }

get_spectra_match_score <-
  function(exp.spectrum,
           lib.spectrum,
           ppm.tol = 30,
           mz.ppm.thr = 400,
           fraction.weight = 0.2,
           dp.forward.weight = 0.7,
           dp.reverse.weight = 0.1,
           remove_fragment_intensity_cutoff = 0) {
    
    # Convert inputs to data.frame if they are Spectra objects
    exp.spectrum <- as_spectrum_df(exp.spectrum)
    lib.spectrum <- as_spectrum_df(lib.spectrum)
    
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




#' Compute Dot Product Between Two Intensity Vectors
#'
#' This function computes the weighted dot product between two intensity vectors
#' from experimental and library spectra, considering weights for each intensity.
#'
#' @param exp.int A numeric vector representing the intensity values of the experimental spectrum.
#' @param lib.int A numeric vector representing the intensity values of the library spectrum.
#'
#' @return A numeric value representing the dot product between
#'   \code{exp.int} and \code{lib.int}.
#'
#' @details
#' The function computes weights for each intensity value in both vectors based
#' on their relative contribution to the total intensity. The dot product is then
#' computed using these weighted intensity values.
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





#' Compute Weighted Dot Product Between Two Intensity Vectors
#'
#' This function computes the weighted dot product between two intensity vectors
#' from experimental and library spectra, taking into account weights for each intensity.
#'
#' @param exp.int A numeric vector representing the intensity values of the experimental spectrum.
#' @param lib.int A numeric vector representing the intensity values of the library spectrum.
#'
#' @return A numeric value representing the weighted dot product between 
#'   \code{exp.int} and \code{lib.int}.
#'
#' @details
#' For each intensity in both the experimental and library spectra, weights are computed
#' based on their relative contribution to the total intensity. The dot product is then 
#' calculated using these weighted intensity values.
#'
#' @export
#' @examples
#' get_dp(exp.int = 1:10, lib.int = 1:10)
get_dp <-
  function(exp.int, lib.int) {
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




#' Match Fragments in Experimental and Library Spectra
#'
#' Matches fragments in experimental and library spectra based on their m/z values
#' within a given ppm tolerance. Noisy fragments are removed before matching.
#'
#' @param exp.spectrum A data frame representing the experimental spectrum with columns for m/z and intensity.
#' @param lib.spectrum A data frame representing the library spectrum with columns for m/z and intensity.
#' @param ppm.tol A numeric value specifying the ppm tolerance for matching fragments. Default is 30.
#' @param mz.ppm.thr A numeric value for the m/z threshold. Default is 400.
#'
#' @return A data frame with columns for library index, experimental index, library m/z, library intensity, experimental m/z, and experimental intensity.
#'
#' @details
#' For each fragment in the library spectrum, the function searches for a matching fragment in the experimental spectrum based on the specified ppm tolerance. 
#' Prior to matching, noisy fragments are removed from both spectra using the `remove_noise` function.
#' The resulting data frame provides matched fragments as well as unmatched fragments from both spectra.
#'
#' @export
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





#' Match Fragments in Experimental and Library Spectra
#'
#' This function matches fragments in experimental and library spectra based on their m/z values
#' within a given ppm tolerance. Noisy fragments are removed before matching.
#' Supports both data.frame and Spectra object inputs.
#'
#' @param exp.spectrum The experimental spectrum. Can be:
#'   \itemize{
#'     \item A data frame with columns for m/z and intensity
#'     \item A Spectra object from the Spectra package
#'   }
#' @param lib.spectrum The library/reference spectrum. Can be:
#'   \itemize{
#'     \item A data frame with columns for m/z and intensity
#'     \item A Spectra object from the Spectra package
#'   }
#' @param ppm.tol A numeric value specifying the ppm tolerance for matching fragments. Default is 30.
#' @param mz.ppm.thr A numeric value for the m/z threshold. Default is 400.
#'
#' @return A data frame with columns for library index, experimental index, library m/z, library intensity, experimental m/z, and experimental intensity.
#'
#' @details
#' For each fragment in the library spectrum, the function searches for a matching fragment in the experimental spectrum based on the specified ppm tolerance. 
#' Prior to matching, noisy fragments are removed from both spectra using the `remove_noise` function.
#' The resulting data frame provides matched fragments as well as unmatched fragments from both spectra.
#'
#' @export
#' @examples
#' exp.spectrum <- data.frame(mz = 1:10, intensity = 1:10)
#' lib.spectrum <- data.frame(mz = 1:10, intensity = 1:10)
#' ms2_match(exp.spectrum, lib.spectrum)
#' 
#' # Works with Spectra objects too
#' \dontrun{
#' library(Spectra)
#' exp_spec <- df_to_spectra(exp.spectrum)
#' lib_spec <- df_to_spectra(lib.spectrum)
#' matches <- ms2_match(exp_spec, lib_spec)
#' }
ms2_match <- function(exp.spectrum,
                      lib.spectrum,
                      ppm.tol = 30,
                      mz.ppm.thr = 400) {
  
  # Convert inputs to data.frame if they are Spectra objects
  exp.spectrum <- as_spectrum_df(exp.spectrum)
  lib.spectrum <- as_spectrum_df(lib.spectrum)
  
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



#' Remove Noisy Fragments from a Spectrum
#'
#' This function removes noisy fragments from a given spectrum. Fragments with m/z values too close to each other, 
#' determined by a ppm threshold, are considered noisy. Among such close fragments, the one with the lower intensity is removed.
#'
#' @param spec A data frame representing the spectrum with columns for m/z and intensity.
#' @param ppm.ms2match A numeric value specifying the ppm threshold for identifying close fragments. Default is 30.
#' @param mz.ppm.thr A numeric value for the m/z threshold below which the m/z is set to this threshold. Default is 400.
#'
#' @return A data frame representing the cleaned spectrum after removing noisy fragments.
#'
#' @details
#' For a given spectrum, the function identifies fragments whose m/z values are too close to each other, based on the specified ppm threshold.
#' Among such close fragments, the one with the lower intensity is removed from the spectrum.
#' 
#' @note This function is deprecated. Use `remove_noise()` instead.
#'
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
  spec <- spec[order(spec[, 1]), ]
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





#' Remove Noisy Fragments from a Spectrum
#'
#' This function removes noisy fragments from a given spectrum based on m/z values and intensities.
#' Fragments with m/z values that are too close to each other, determined by a ppm threshold, are considered noisy.
#' Among these close fragments, the fragment with the lowest intensity is removed.
#' Supports both data.frame and Spectra object inputs.
#'
#' @param spec A mass spectrum. Can be:
#'   \itemize{
#'     \item A data frame with columns for m/z and intensity
#'     \item A Spectra object from the Spectra package
#'   }
#' @param ppm.ms2match A numeric value specifying the ppm threshold for identifying close fragments. Default is 30.
#' @param mz.ppm.thr A numeric value for the m/z threshold below which the m/z value will be set to this threshold. Default is 400.
#'
#' @return A spectrum in the same format as input:
#'   \itemize{
#'     \item If input is data.frame: returns data.frame
#'     \item If input is Spectra: returns Spectra object
#'   }
#'
#' @details
#' The function first sorts the spectrum based on m/z values and calculates the differences between consecutive m/z values.
#' Fragments whose m/z differences are below the specified ppm threshold are identified.
#' Among these fragments, the one with the lowest intensity is removed from the spectrum.
#' 
#' @export
#' @examples
#' exp.spectrum <- data.frame(mz = 1:10, intensity = 1:10)
#' remove_noise(exp.spectrum)
#' 
#' # Works with Spectra objects too
#' \dontrun{
#' library(Spectra)
#' spec_obj <- df_to_spectra(exp.spectrum)
#' cleaned <- remove_noise(spec_obj)
#' }
remove_noise <- function(spec,
                         ppm.ms2match = 30,
                         mz.ppm.thr = 400) {
  
  # Check input type and convert if needed
  input_type <- check_spectrum_type(spec)
  
  if (input_type == "Spectra") {
    # Convert to data.frame for processing
    spec_df <- spectra_to_df(spec, index = 1)
    return_spectra <- TRUE
    # Store metadata for reconstruction
    spd <- Spectra::spectraData(spec)[1, ]
  } else {
    spec_df <- as_spectrum_df(spec)
    return_spectra <- FALSE
  }
  
  spec <- spec_df
  
  # Original noise removal logic
  if (nrow(spec) == 1) {
    if (return_spectra) {
      return(df_to_spectra(spec,
                          precursor_mz = spd$precursorMz,
                          precursor_rt = spd$rtime))
    }
    return(spec)
  }
  
  spec <- spec[order(spec[, 1]), ]
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
  }
  
  # Return in appropriate format
  if (return_spectra) {
    return(df_to_spectra(spec,
                        precursor_mz = spd$precursorMz,
                        precursor_rt = spd$rtime))
  }
  
  return(spec)
}
