#' @title openwd
#' @description Open current work directory.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @return inder.
#' @export
openwd <- function() {
  system(sprintf("open %s", shQuote(getwd())))
}
