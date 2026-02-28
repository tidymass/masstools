#' Read MGF Files for Database Workflows
#'
#' `read_mgf4database()` is deprecated. Please use `read_mgf_database()`
#' instead.
#'
#' @param file A character vector of MGF file paths.
#'
#' @return A list of spectra in the database workflow format.
#'
#' @examples
#' file_path <- system.file("extdata", "example.mgf", package = "masstools")
#' result <- read_mgf4database(file_path)
#' length(result)
#' @export
read_mgf4database <- function(file) {
  lifecycle::deprecate_soft(
    when = "0.99.1",
    what = "read_mgf4database()",
    with = "read_mgf_database()"
  )
  read_mgf_database(file)
}
