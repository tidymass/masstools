#' Read mzXML Files
#'
#' Read mzXML files and extract MS2 spectra in the legacy list format used by
#' the package.
#'
#' @param file A character vector of mzXML file paths.
#' @param threads Integer. Reserved for compatibility with older workflows.
#' @param mode Character string specifying whether files are read with
#'   `\"inMemory\"` or `\"onDisk\"`.
#'
#' @return A list of spectra. Each element contains:
#' \itemize{
#'   \item \code{info}: a data frame containing spectrum name, precursor `mz`,
#'     `rt`, and source file.
#'   \item \code{spec}: a data frame with `mz` and `intensity` columns.
#' }
#'
#' @examples
#' if (requireNamespace("MSnbase", quietly = TRUE)) {
#' file_path <- system.file("extdata", "example.mzXML", package = "masstools")
#' result <- read_mzxml(file_path)
#' length(result)
#' result[[1]]$info
#' }
#' @export

read_mzxml <-
  function(file,
           threads = 3,
           mode = c("inMemory", "onDisk")) {
    mode <- match.arg(mode)
    if (!requireNamespace("MSnbase", quietly = TRUE)) {
      stop(
        "read_mzxml() requires the optional package 'MSnbase'. ",
        "Install it with BiocManager::install('MSnbase').",
        call. = FALSE
      )
    }
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
    new.ms2
  }
