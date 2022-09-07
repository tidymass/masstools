#' @title read_mgf4database
#' @description Read MGF data.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param file The vector of names of ms2 files. MS2 file must be mgf.
#' @importFrom tidyr separate
#' @return Return ms2 data. This is a list.
#' @export

read_mgf4database <- function(file) {
  pbapply::pboptions(style = 1)
  message(crayon::green("Reading mgf data..."))
  # mgf.data.list <- pbapply::pblapply(file, ListMGF)
  ms2 <- purrr::map(
    .x = file,
    .f = function(mgf_data_name) {
      message("Reading ", mgf_data_name, "...")
      mgf.data <- ListMGF(mgf_data_name)
      # nl.spec <- grep('^\\d', mgf.data)
      nl.spec <-
        lapply(mgf.data, function(x) {
          grep("^\\d", x)
        })
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
      
      # for orbitrap data, the intensity of
      # precursor ion should be removed
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
            file = mgf_data_name
          ),
          spec = as.data.frame(spec[[i]]))
          
        })
      ms2
    }
  )
  
  spec.info <- ms2[[1]]
  if (length(ms2) > 1) {
    for (i in seq_along(ms2)[-1]) {
      spec.info <- c(spec.info, ms2[[i]])
    }
  }
  
  remove.idx <-
    which(unlist(lapply(spec.info, function(x) {
      nrow(x[[2]])
    })) == 0)
  if (length(remove.idx) != 0) {
    spec.info <- spec.info[-remove.idx]
  }
  # ##remove noise
  # message("\n")
  # message("Remove noise of MS/MS spectra...\n")
  # spec.info <- pbapply::pblapply(spec.info, function(x){
  #   temp.spec <- x[[2]]
  #   temp.spec <- remove_noise(temp.spec)
  #   x[[2]] <- temp.spec
  #   x
  # })
  
  spec.info <- spec.info
}
