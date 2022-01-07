#' @title setwd_win
#' @description Set work directory in windows.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @export
#' @return noting

setwd_win <- function() {
    x <- readline("Please paste the directory:")
    setwd(x)
}
