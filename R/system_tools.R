#' Determine the Operating System of the Current R Session
#'
#' This function identifies the operating system on which the R session is running and returns it in a standardized format.
#'
#' @return A character string indicating the operating system. Possible values are "windows", "osx", "linux", or others based on the system.
#' @examples
#' get_os()
#' @export
get_os <- function() {
    sysinf <- Sys.info()
    if (!is.null(sysinf)) {
        os <- sysinf["sysname"]
        if (os == "Darwin") {
              os <- "osx"
          }
    } else {
        ## mystery machine
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
