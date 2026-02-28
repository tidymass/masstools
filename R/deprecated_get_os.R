#' Determine the Operating System of the Current R Session
#'
#' `get_os()` is deprecated. Please use `detect_os()` instead.
#'
#' @return A character scalar such as `"windows"`, `"osx"`, or `"linux"`.
#' @examples
#' get_os()
#' @export
get_os <- function() {
  lifecycle::deprecate_soft(
    when = "0.99.1",
    what = "get_os()",
    with = "detect_os()"
  )
  detect_os()
}
