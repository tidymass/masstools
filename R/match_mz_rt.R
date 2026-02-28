#' @title Match Peaks by m/z and Retention Time
#' @description Match rows between two peak tables using m/z and retention time
#'   tolerances.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param data1 First peak table. The first two columns must be `mz` and `rt`.
#' @param data2 Second peak table. The first two columns must be `mz` and `rt`.
#' @param mz.tol Numeric. m/z tolerance in ppm.
#' @param rt.tol Numeric. Retention time tolerance.
#' @param rt.error.type Character. Use relative or absolute retention time
#'   error.
#' @return A data frame of candidate matches, or `NULL` if no matches are found.
#' @export
#' @examples
#' data1 <- data.frame(
#'   mz = c(100.0000, 150.0600, 300.1234),
#'   rt = c(120, 240, 360)
#' )
#' data2 <- data.frame(
#'   mz = c(100.0008, 150.0610, 310.0000),
#'   rt = c(121, 238, 400)
#' )
#'
#' match_mz_rt(data1, data2, mz.tol = 10, rt.tol = 2, rt.error.type = "abs")
#' @export
match_mz_rt <-
  function(data1,
           data2,
           mz.tol,
           rt.tol = 30,
           rt.error.type = c("relative", "abs")) {
    UseMethod("match_mz_rt")
  }

#' @export
match_mz_rt.data.frame <-
  function(data1,
           data2,
           mz.tol,
           rt.tol = 30,
           rt.error.type = c("relative", "abs")) {
    rt.error.type <- match.arg(rt.error.type)
    match_mz_rt_default(data1 = data1,
                        data2 = data2,
                        mz.tol = mz.tol,
                        rt.tol = rt.tol,
                        rt.error.type = rt.error.type)
  }

#' @export
match_mz_rt.matrix <-
  function(data1,
           data2,
           mz.tol,
           rt.tol = 30,
           rt.error.type = c("relative", "abs")) {
    rt.error.type <- match.arg(rt.error.type)
    match_mz_rt_default(data1 = data1,
                        data2 = data2,
                        mz.tol = mz.tol,
                        rt.tol = rt.tol,
                        rt.error.type = rt.error.type)
  }


match_mz_rt_default <-
  function(data1,
           data2,
           mz.tol,
           rt.tol = 30,
           rt.error.type = c("relative", "abs")) {
    rt.error.type <- match.arg(rt.error.type)
    if (nrow(data1) == 0 || nrow(data2) == 0) {
      result <- NULL
      return(result)
    }
    info1 <- data1[, c(1, 2)]
    info1 <- apply(info1, 1, list)

    mz2 <- as.numeric(data2[, 1])
    rt2 <- as.numeric(data2[, 2])

    result <- pbapply::pblapply(info1, function(x) {
      temp.mz1 <- x[[1]][[1]]
      temp.rt1 <- x[[1]][[2]]
      mz.error <- abs(temp.mz1 - mz2) * 10 ^ 6 / temp.mz1
      if (rt.error.type == "relative") {
        rt.error <- abs(temp.rt1 - rt2) * 100 / temp.rt1
      } else {
        rt.error <- abs(temp.rt1 - rt2)
      }

      j <- which(mz.error <= mz.tol & rt.error <= rt.tol)
      if (length(j) == 0) {
        matrix(NA, ncol = 7)
      } else {
        cbind(j, temp.mz1, mz2[j],
              mz.error[j], temp.rt1, rt2[j], rt.error[j])
      }
    })

    if (length(result) == 1) {
      result <- cbind(1, result[[1]])
    } else {
      result <- mapply(function(x, y) {
        list(cbind(x, y))
      },
      x <- seq_along(info1),
      y = result)
      result <- do.call(rbind, result)
    }

    result <-
      matrix(result[which(!apply(result, 1, function(x) {
        any(is.na(x))
      })), ], ncol = 8)
    if (nrow(result) == 0) {
      return(NULL)
    }
    colnames(result) <-
      c("Index1",
        "Index2",
        "mz1",
        "mz2",
        "mz error",
        "rt1",
        "rt2",
        "rt error")
    result <- as.data.frame(result)
    result
  }

