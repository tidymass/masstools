#' @title name_duplicated
#' @description name_duplicated
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param x A character vector.
#' @return A character vector without duplicated.
#' @export
#' @examples
#' name_duplicated(c("a", "a", "b", "c", "a", "b", "c", "a"))
#' name_duplicated(c(rep(1, 5), 2))
#' name_duplicated(1:5)
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
