#' @title read_mgf
#' @description Read MGF data.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param file The vector of names of ms2 files. MS2 file must be mgf.
#' @return Return ms2 data. This is a list.
#' @export

read_mgf <- function(file) {
    pbapply::pboptions(style = 1)
    cat(crayon::green("Reading mgf data...\n"))
    # mgf.data.list <- pbapply::pblapply(file, ListMGF)
    ms2 <- purrr::map(
        .x = file,
        .f = function(mgf.data) {
            mgf.data <- ListMGF(mgf.data)
            # nl.spec <- grep('^\\d', mgf.data)
            nl.spec <-
                lapply(mgf.data, function(x) {
                      grep("^\\d", x)
                  })
            info.mz <-
                lapply(mgf.data, function(x) {
                      grep("^PEPMASS", x, value = TRUE)
                  })
            info.rt <-
                lapply(mgf.data, function(x) {
                      grep("^RTINSECONDS", x, value = TRUE)
                  })

            info.mz <- unlist(info.mz)
            # for orbitrap data, the intensity of precursor ion should be removed
            info.mz <-
                unlist(lapply(strsplit(x = info.mz, split = " "), function(x) {
                      x[1]
                  }))
            info.mz <-
                as.numeric(gsub(pattern = "\\w+=", "", info.mz))
            info.rt <- unlist(info.rt)
            info.rt <-
                as.numeric(gsub(pattern = "\\w+=", "", info.rt))

            if (length(mgf.data) == 1) {
                spec <- mapply(function(x, y) {
                    temp <- do.call(rbind, strsplit(x[y], split = " "))
                    list(temp)
                },
                x = mgf.data,
                y = nl.spec
                )
            } else {
                spec <- mapply(function(x, y) {
                    do.call(rbind, strsplit(x[y], split = " "))
                },
                x = mgf.data,
                y = nl.spec
                )
            }

            spec <- lapply(spec, function(x) {
                temp <- cbind(as.numeric(x[, 1]), as.numeric(x[, 2]))
                temp <- matrix(temp, ncol = 2)
                # if(nrow(temp) > 0) temp <- temp[temp[,2] >= max(temp[,2])*0.01,]
                temp <- matrix(temp, ncol = 2)
                colnames(temp) <- c("mz", "intensity")
                temp
            })

            ms2 <- mapply(function(x, y, z) {
                info <- c(y, z)
                names(info) <- c("mz", "rt")
                spectrum <- as.matrix(x)
                temp <- list(info, spectrum)
                names(temp) <- c("info", "spec")
                list(temp)
            },
            x = spec,
            y = info.mz,
            z = info.rt
            )

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
    # cat("\n")
    # cat("Remove noise of MS/MS spectra...\n")
    # spec.info <- pbapply::pblapply(spec.info, function(x){
    #   temp.spec <- x[[2]]
    #   temp.spec <- removeNoise(temp.spec)
    #   x[[2]] <- temp.spec
    #   x
    # })

    spec.info <- spec.info
}

ListMGF <- function(file) {
    mgf.data <- readLines(file)
    nl.rec.new <- 1
    idx.rec <- 1
    rec.list <- list()
    for (nl in seq_along(mgf.data))
    {
        if (mgf.data[nl] == "END IONS") {
            rec.list[idx.rec] <- list(Compound = mgf.data[nl.rec.new:nl])
            nl.rec.new <- nl + 1
            idx.rec <- idx.rec + 1
        }
    }
    rec.list
}
