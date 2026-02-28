#' Convert a Data Frame to a Spectra Object
#'
#' Convert a peak table with `mz` and `intensity` columns to a `Spectra`
#' object.
#'
#' @param spectrum_df A data frame with `mz` and `intensity` columns.
#' @param precursor_mz Numeric. Optional precursor m/z value for the spectrum.
#' @param precursor_rt Numeric. Optional retention time for the spectrum.
#' @param spectrum_id Character. Optional spectrum identifier.
#'
#' @return A `Spectra` object containing one MS2 spectrum.
#' @importFrom S4Vectors DataFrame
#' @export
#' @examples
#' spectrum_df <- data.frame(
#'   mz = c(100.00, 124.04, 150.06),
#'   intensity = c(10, 35, 100)
#' )
#'
#' spec <- df_to_spectra(
#'   spectrum_df,
#'   precursor_mz = 301.12,
#'   precursor_rt = 120.5,
#'   spectrum_id = "example_1"
#' )
df_to_spectra <- function(spectrum_df, 
                          precursor_mz = NA_real_,
                          precursor_rt = NA_real_,
                          spectrum_id = NA_character_) {
  
  if (!is.data.frame(spectrum_df)) {
    stop("spectrum_df must be a data.frame")
  }
  
  if (!all(c("mz", "intensity") %in% colnames(spectrum_df))) {
    stop("spectrum_df must contain 'mz' and 'intensity' columns")
  }
  
  spd <- data.frame(
    msLevel = 2L,
    precursorMz = precursor_mz,
    rtime = precursor_rt,
    stringsAsFactors = FALSE
  )
  spd$mz <- I(list(spectrum_df$mz))
  spd$intensity <- I(list(spectrum_df$intensity))
  
  if (!is.na(spectrum_id)) {
    spd$spectrum_id <- spectrum_id
  }
  
  backend <-
    Spectra::backendInitialize(
      Spectra::MsBackendMemory(),
      data = spd
    )
  sps <- Spectra::Spectra(backend = backend)
  
  return(sps)
}


#' Convert a Spectra Object to a Data Frame
#'
#' Extract one spectrum from a `Spectra` object and return it as a data frame.
#'
#' @param spectra A `Spectra` object.
#' @param index Integer. Which spectrum to extract.
#'
#' @return A data frame with `mz` and `intensity` columns.
#' @export
#' @examples
#' spectrum_df <- data.frame(
#'   mz = c(100.00, 124.04, 150.06),
#'   intensity = c(10, 35, 100)
#' )
#' spec <- df_to_spectra(spectrum_df)
#'
#' spectra_to_df(spec)
spectra_to_df <- function(spectra, index = 1) {
  
  if (!inherits(spectra, "Spectra")) {
    stop("spectra must be a Spectra object")
  }
  
  if (length(spectra) < index) {
    stop("index exceeds number of spectra")
  }
  
  # Extract peak data
  peaks <- Spectra::peaksData(spectra)[[index]]
  
  # Convert to data frame
  df <- data.frame(
    mz = peaks[, "mz"],
    intensity = peaks[, "intensity"],
    stringsAsFactors = FALSE
  )
  
  return(df)
}


#' Check the Type of a Spectrum-Like Object
#'
#' Internal helper that classifies supported spectrum input types.
#'
#' @param x An object to check
#'
#' @return Character string: "Spectra", "data.frame", "matrix", or "unknown"
#' @keywords internal
check_spectrum_type <- function(x) {
  if (inherits(x, "Spectra")) {
    return("Spectra")
  } else if (is.data.frame(x)) {
    return("data.frame")
  } else if (is.matrix(x)) {
    return("matrix")
  } else {
    return("unknown")
  }
}


#' Convert Spectrum Input to a Data Frame
#'
#' Internal helper that converts supported spectrum inputs to a standard data
#' frame representation.
#'
#' @param spectrum A spectrum in any supported format
#' @param index Integer, which spectrum to extract if Spectra object (default: 1)
#'
#' @return A data frame with 'mz' and 'intensity' columns
#' @keywords internal
as_spectrum_df <- function(spectrum, index = 1) {
  
  type <- check_spectrum_type(spectrum)
  
  if (type == "Spectra") {
    return(spectra_to_df(spectrum, index = index))
  } else if (type == "data.frame") {
    if (!all(c("mz", "intensity") %in% colnames(spectrum))) {
      stop("data.frame must contain 'mz' and 'intensity' columns")
    }
    return(as.data.frame(spectrum))
  } else if (type == "matrix") {
    if (ncol(spectrum) != 2) {
      stop("matrix must have 2 columns (mz, intensity)")
    }
    return(data.frame(
      mz = spectrum[, 1],
      intensity = spectrum[, 2],
      stringsAsFactors = FALSE
    ))
  } else {
    stop("spectrum must be a Spectra object, data.frame, or matrix")
  }
}


#' Convert Multiple Spectra to a List Representation
#'
#' Converts a Spectra object with multiple spectra to the legacy list format
#' used by read_mgf and read_mzxml functions.
#'
#' @param spectra A Spectra object
#'
#' @return A list where each element contains 'info' and 'spec'
#' @keywords internal
spectra_to_list <- function(spectra) {
  
  if (!inherits(spectra, "Spectra")) {
    stop("spectra must be a Spectra object")
  }
  
  n_spectra <- length(spectra)
  
  result <- lapply(seq_len(n_spectra), function(i) {
    # Extract spectrum data
    peaks <- Spectra::peaksData(spectra)[[i]]
    
    # Extract metadata
    spd <- Spectra::spectraData(spectra)[i, ]
    
    # Create info vector
    info <- c(
      mz = if ("precursorMz" %in% colnames(spd)) spd$precursorMz else NA_real_,
      rt = if ("rtime" %in% colnames(spd)) spd$rtime else NA_real_
    )
    
    # Add name if available
    if ("spectrum_id" %in% colnames(spd)) {
      names(info)[1] <- spd$spectrum_id
    }
    
    # Return in legacy format
    list(
      info = info,
      spec = peaks
    )
  })
  
  return(result)
}


#' Convert a Legacy Spectrum List to a Spectra Object
#'
#' Converts the legacy list format (from read_mgf/read_mzxml) to a Spectra object.
#'
#' @param spectrum_list A list where each element contains 'info' and 'spec'
#'
#' @return A Spectra object
#' @keywords internal
list_to_spectra <- function(spectrum_list) {
  
  if (!is.list(spectrum_list)) {
    stop("spectrum_list must be a list")
  }
  
  spd_list <- lapply(spectrum_list, function(x) {
    info <- x$info
    spec <- x$spec
    if (is.matrix(spec)) {
      spec <- as.data.frame(spec, stringsAsFactors = FALSE)
    }
    list(
      msLevel = 2L,
      precursorMz = if ("mz" %in% names(info)) info["mz"] else NA_real_,
      rtime = if ("rt" %in% names(info)) info["rt"] else NA_real_,
      mz = I(list(as.numeric(spec$mz))),
      intensity = I(list(as.numeric(spec$intensity)))
    )
  })
  
  spd <- do.call(rbind, lapply(spd_list, as.data.frame))
  backend <-
    Spectra::backendInitialize(
      Spectra::MsBackendMemory(),
      data = spd
    )
  sps <- Spectra::Spectra(backend = backend)
  
  return(sps)
}
