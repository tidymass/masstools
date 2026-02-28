#' List Packages Imported by masstools
#'
#' `masstools_packages()` is deprecated. Please use
#' `list_masstools_packages()` instead.
#'
#' @param include_self Logical. If `TRUE`, include `"masstools"` in the
#'   returned character vector.
#' @return A character vector of package names listed in the package Imports.
#' @export
#' @examples
#' masstools_packages()
masstools_packages <- function(include_self = TRUE) {
  lifecycle::deprecate_soft(
    when = "0.99.1",
    what = "masstools_packages()",
    with = "list_masstools_packages()"
  )
  list_masstools_packages(include_self = include_self)
}
