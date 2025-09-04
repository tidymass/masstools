#' Read and Process mzXML Files
#'
#' Reads mzXML files and extracts relevant MS2 spectra information.
#'
#' @param file A character vector specifying the path(s) to the mzXML file(s).
#' @param threads An integer, indicating the number of cores to use (default: 3). 
#' @param mode A character string, either \code{"inMemory"} or \code{"onDisk"} 
#'   specifying the mode of reading data. Default is \code{"inMemory"}.
#'
#' @return A list containing processed MS2 spectra information for each provided mzXML file. 
#' Each element of the list contains two components: 
#' \itemize{
#'   \item \code{info}: A data frame with columns for the name (composed of m/z and retention time), 
#'   m/z, retention time, and file name.
#'   \item \code{spec}: A data frame where each row represents a fragment ion peak, 
#'   with columns for m/z and intensity values.
#' }
#'
#' @examples
#' # Locate the example mzXML file in the installed package
#' file_path <- system.file("extdata", "example.mzXML", package = "masstools")
#' # Then use it in your function
#' result <- read_mzxml(file_path)
#' @export

read_mzxml <-
  function(file,
           threads = 3,
           mode = c("inMemory", "onDisk")) {
    # pbapply::pboptions(style = 1)
    message(crayon::green("Reading MS2 data..."))
    # mzxml.data.list <- pbapply::pblapply(file, ListMGF)
    ms2 <-
      MSnbase::readMSData(files = file,
                          msLevel. = 2,
                          mode = mode)
    message(crayon::green("Processing..."))
    
    new.ms2 <- ProtGenerics::spectra(object = ms2)
    rm(list = c("ms2"))
    #
    # temp.fun <- function(idx, ms2) {
    #   temp.ms2 <- ms2[[idx]]
    #   rm(list = c("ms2"))
    #   info <-
    #     data.frame(
    #       name = paste("mz", temp.ms2@precursorMz,
    #                    "rt", temp.ms2@rt, sep = ""),
    #       "mz" = temp.ms2@precursorMz,
    #       "rt" = temp.ms2@rt,
    #       "file" = file[temp.ms2@fromFile],
    #       stringsAsFactors = FALSE
    #     )
    #   duplicated.name <-
    #     unique(info$name[duplicated(info$name)])
    #   if (length(duplicated.name) > 0) {
    #     lapply(duplicated.name, function(x) {
    #       info$name[which(info$name == x)] <-
    #         paste(x, seq_len(sum(info$name == x)), sep = "_")
    #     })
    #   }
    #
    #   rownames(info) <- NULL
    #   spec <- data.frame(
    #     "mz" = temp.ms2@mz,
    #     "intensity" = temp.ms2@intensity,
    #     stringsAsFactors = FALSE
    #   )
    #   list(info = info, spec = spec)
    # }
    
    new.ms2 <-
      seq_along(new.ms2) %>%
      purrr::map(function(idx) {
        temp.ms2 <- new.ms2[[idx]]
        info <-
          data.frame(
            name = paste("mz", temp.ms2@precursorMz,
                         "rt", temp.ms2@rt, sep = ""),
            "mz" = temp.ms2@precursorMz,
            "rt" = temp.ms2@rt,
            "file" = file[temp.ms2@fromFile],
            stringsAsFactors = FALSE
          )
        duplicated.name <-
          unique(info$name[duplicated(info$name)])
        if (length(duplicated.name) > 0) {
          lapply(duplicated.name, function(x) {
            info$name[which(info$name == x)] <-
              paste(x, seq_len(sum(info$name == x)), sep = "_")
          })
        }
        
        rownames(info) <- NULL
        spec <- data.frame(
          "mz" = temp.ms2@mz,
          "intensity" = temp.ms2@intensity,
          stringsAsFactors = FALSE
        )
        list(info = info, spec = spec)
      })
    
    
    # new.ms2 <-
    #   BiocParallel::bplapply(
    #     X = seq_along(new.ms2),
    #     FUN = temp.fun,
    #     BPPARAM = BiocParallel::MulticoreParam(workers = threads,
    #                                            progressbar = TRUE),
    #     ms2 = new.ms2
    #   )
    #
    new.ms2 <- new.ms2
  }



