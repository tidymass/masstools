#' Match Fragments Between Two Spectra
#'
#' Match fragments between two spectra based on m/z values within a ppm
#' tolerance.
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
#' Noisy fragments are removed before matching. For each fragment in the
#' library spectrum, the function looks for a matching fragment in the
#' experimental spectrum and returns matched as well as unmatched peaks.
#'
#' @export
#' @examples
#' exp.spectrum <- data.frame(
#'   mz = c(100.0000, 124.0400, 150.0600, 180.0800, 205.1000),
#'   intensity = c(10, 35, 100, 45, 20)
#' )
#' lib.spectrum <- data.frame(
#'   mz = c(100.0015, 124.0395, 150.0610, 180.0820, 220.1200),
#'   intensity = c(12, 30, 95, 50, 10)
#' )
#'
#' ms2_match(exp.spectrum, lib.spectrum, ppm.tol = 50)
#'
#' # The function also accepts Spectra objects.
#' \dontrun{
#' exp_spec <- df_to_spectra(exp.spectrum)
#' lib_spec <- df_to_spectra(lib.spectrum)
#'
#' ms2_match(exp_spec, lib_spec, ppm.tol = 50)
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
#' Remove fragments with nearly identical m/z values and keep the more intense
#' peak.
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
#' Peaks are ordered by m/z, adjacent peaks are compared using the ppm
#' threshold, and the less intense peak is removed when two peaks are too
#' close. The function accepts both data frames and `Spectra` objects.
#' 
#' @export
#' @examples
#' spectrum <- data.frame(
#'   mz = c(100.0000, 100.0010, 124.0400, 150.0600, 150.0610),
#'   intensity = c(10, 5, 35, 100, 20)
#' )
#'
#' remove_noise(spectrum, ppm.ms2match = 30)
#'
#' # The function also accepts Spectra objects.
#' \dontrun{
#' spec_obj <- df_to_spectra(spectrum)
#'
#' remove_noise(spec_obj, ppm.ms2match = 30)
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
