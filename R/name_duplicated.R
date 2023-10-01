#' Handle Duplicated Names by Appending Sequence Number
#'
#' This function checks for duplicated elements in a vector. If duplicated elements are found,
#' it appends an underscore and a sequence number to each occurrence of the duplicated element.
#'
#' @param x A character vector.
#'
#' @return A character vector where duplicated names are made unique by appending a sequence number.
#'
#' @examples
#' vec <- c("apple", "orange", "apple", "banana", "orange")
#' name_duplicated(vec)
#'
#' @export
name_duplicated <- function(x) {
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
