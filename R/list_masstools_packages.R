#' List Packages Imported by masstools
#'
#' @param include_self Logical. If `TRUE`, include `"masstools"` in the
#'   returned character vector.
#' @return A character vector of package names listed in the package Imports.
#' @export
#' @examples
#' list_masstools_packages()
list_masstools_packages <- function(include_self = TRUE) {
  raw <- utils::packageDescription("masstools")$Imports
  imports <- strsplit(raw, ",")[[1]]
  parsed <- gsub("^\\s+|\\s+$", "", imports)
  names <-
    vapply(strsplit(parsed, "\\s+"), "[[", 1, FUN.VALUE = character(1))

  if (include_self) {
    names <- c(names, "masstools")
  }

  names
}
