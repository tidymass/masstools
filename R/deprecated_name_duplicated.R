#' Make Duplicated Names Unique
#'
#' `name_duplicated()` is deprecated. Please use `deduplicate_names()`
#' instead.
#'
#' @param x A character vector.
#'
#' @return A character vector where duplicated names are made unique by
#'   appending a sequence number.
#'
#' @examples
#' vec <- c("apple", "orange", "apple", "banana", "orange")
#' name_duplicated(vec)
#'
#' @export
name_duplicated <- function(x) {
  lifecycle::deprecate_soft(
    when = "0.99.1",
    what = "name_duplicated()",
    with = "deduplicate_names()"
  )
  deduplicate_names(x)
}
