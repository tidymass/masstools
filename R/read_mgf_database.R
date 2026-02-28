#' Read MGF Files for Database Workflows
#'
#' Read MGF files and return spectra in the list structure used by the database
#' workflow.
#'
#' @param file A character vector of MGF file paths.
#'
#' @return A list of spectra. Each element contains:
#' \itemize{
#'   \item \code{info}: a data frame containing spectrum name, precursor `mz`,
#'     `rt`, and source file.
#'   \item \code{spec}: a peak table with `mz` and `intensity` columns.
#' }
#' Empty spectra are removed from the output.
#'
#' @examples
#' file_path <- system.file("extdata", "example.mgf", package = "masstools")
#' result <- read_mgf_database(file_path)
#' length(result)
#' result[[1]]$info
#' @export
read_mgf_database <- function(file) {
  pbapply::pboptions(style = 1)
  message(crayon::green("Reading mgf data..."))
  ms2 <- purrr::map(
    .x = file,
    .f = function(mgf_data_name) {
      message("Reading ", mgf_data_name, "...")
      mgf.data <- ListMGF(mgf_data_name)
      nl.spec <-
        lapply(mgf.data, function(x) {
          grep("^\\d", x)
        })

      remove_idx <-
        which(unlist(lapply(nl.spec, length)) == 0)

      if (length(remove_idx) > 0) {
        mgf.data <- mgf.data[-remove_idx]
        nl.spec <- nl.spec[-remove_idx]
      }

      info.mz <-
        lapply(mgf.data, function(x) {
          grep("^PEPMASS|PRECURSORMZ", x, value = TRUE)
        })
      info.rt <-
        lapply(mgf.data, function(x) {
          grep("^RTINSECONDS|RETENTIONTIME", x, value = TRUE)
        })

      info.mz <- unlist(info.mz) %>%
        stringr::str_replace("[a-zA-Z|\\:|\\=]{1,20}", "") %>%
        stringr::str_trim()

      info.mz <-
        unlist(lapply(strsplit(x = info.mz, split = " "), function(x) {
          x[1]
        })) %>%
        as.numeric()
      info.mz <-
        as.numeric(gsub(pattern = "\\w+=", "", info.mz))

      info.rt <- unlist(info.rt) %>%
        stringr::str_replace("[a-zA-Z|\\:|\\=]{1,20}", "") %>%
        stringr::str_trim() %>%
        as.numeric()

      info.rt <-
        as.numeric(gsub(pattern = "\\w+=", "", info.rt))

      if (length(mgf.data) == 1) {
        spec <- mapply(function(x, y) {
          temp <- do.call(rbind, strsplit(x[y], split = " "))
          list(temp)
        },
        x = mgf.data,
        y = nl.spec)
      } else {
        spec <- mapply(function(x, y) {
          do.call(rbind, strsplit(x[y], split = " "))
        },
        x = mgf.data,
        y = nl.spec)
      }

      spec <- lapply(spec, function(x) {
        if (ncol(x) == 1) {
          if (length(grep("\\\t", x[1, 1])) > 0) {
            x <-
              as.data.frame(x) %>%
              tidyr::separate(
                col = 1,
                sep = "\\\t",
                into = c("mz", "intensity")
              )
          }
        }
        temp <- cbind(as.numeric(x[, 1]), as.numeric(x[, 2]))
        temp <- matrix(temp, ncol = 2)
        temp <- matrix(temp, ncol = 2)
        colnames(temp) <- c("mz", "intensity")
        temp
      })

      ms2 <-
        seq_len(length(info.mz)) %>%
        purrr::map(function(i) {
          list(info = data.frame(
            name = paste(info.mz[i],
                         info.rt[i],
                         sep = ""),
            mz = info.mz[i],
            rt = info.rt[i],
            file = mgf_data_name,
            stringsAsFactors = FALSE
          ),
          spec = as.data.frame(spec[[i]], stringsAsFactors = FALSE))
        })
      ms2
    }
  )
  spec.info <- unlist(ms2, recursive = FALSE)
  spec.info
}
