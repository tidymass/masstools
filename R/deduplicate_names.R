#' Make Duplicated Names Unique
#'
#' Append sequence numbers to duplicated character values.
#'
#' @param x A character vector.
#'
#' @return A character vector where duplicated names are made unique by
#'   appending a sequence number.
#'
#' @examples
#' vec <- c("apple", "orange", "apple", "banana", "orange")
#' deduplicate_names(vec)
#'
#' @export
deduplicate_names <- function(x) {
  if (sum(duplicated(x)) == 0) {
    return(x)
  }

  duplicated_x <-
    unique(x[duplicated(x)])

  for (i in seq_along(duplicated_x)) {
    y <- duplicated_x[i]
    x[which(x == y)] <- paste(y, seq_along(x[which(x == y)]), sep = "_")
  }
  x
}
