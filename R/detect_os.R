#' Determine the Operating System of the Current R Session
#'
#' This function identifies the operating system for the current R session and
#' returns it in a standardized format.
#'
#' @return A character scalar such as `"windows"`, `"osx"`, or `"linux"`.
#' @examples
#' detect_os()
#' @export
detect_os <- function() {
  sysinf <- Sys.info()
  if (!is.null(sysinf)) {
    os <- sysinf["sysname"]
    if (os == "Darwin") {
      os <- "osx"
    }
  } else {
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os)) {
      os <- "osx"
    }
    if (grepl("linux-gnu", R.version$os)) {
      os <- "linux"
    }
  }
  tolower(os)
}
