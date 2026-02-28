#' @title Match Peaks by m/z and Retention Time
#' @description `mz_rt_match()` is deprecated. Please use
#'   `match_mz_rt()` instead.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @inheritParams match_mz_rt
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
#' mz_rt_match(data1, data2, mz.tol = 10, rt.tol = 2, rt.error.type = "abs")
mz_rt_match <-
  function(data1,
           data2,
           mz.tol,
           rt.tol = 30,
           rt.error.type = c("relative", "abs")) {
    UseMethod("mz_rt_match")
  }


#' @export
mz_rt_match.data.frame <-
  function(data1,
           data2,
           mz.tol,
           rt.tol = 30,
           rt.error.type = c("relative", "abs")) {
    lifecycle::deprecate_soft(
      when = "0.99.1",
      what = "mz_rt_match()",
      with = "match_mz_rt()"
    )
    match_mz_rt.data.frame(
      data1 = data1,
      data2 = data2,
      mz.tol = mz.tol,
      rt.tol = rt.tol,
      rt.error.type = rt.error.type
    )
  }


#' @export
mz_rt_match.matrix <-
  function(data1,
           data2,
           mz.tol,
           rt.tol = 30,
           rt.error.type = c("relative", "abs")) {
    lifecycle::deprecate_soft(
      when = "0.99.1",
      what = "mz_rt_match()",
      with = "match_mz_rt()"
    )
    match_mz_rt.matrix(
      data1 = data1,
      data2 = data2,
      mz.tol = mz.tol,
      rt.tol = rt.tol,
      rt.error.type = rt.error.type
    )
  }


mz_rt_match_default <-
  function(data1,
           data2,
           mz.tol,
           rt.tol = 30,
           rt.error.type = c("relative", "abs")) {
    lifecycle::deprecate_soft(
      when = "0.99.1",
      what = "mz_rt_match_default()",
      with = "match_mz_rt_default()"
    )
    match_mz_rt_default(
      data1 = data1,
      data2 = data2,
      mz.tol = mz.tol,
      rt.tol = rt.tol,
      rt.error.type = rt.error.type
    )
  }
