#' @title Print the masstools Startup Logo
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @return This function is called for its side effects and returns `NULL`
#'   invisibly.
#' @export
#' @examples
#' masstools_logo()
masstools_logo <- function() {
  message("Thank you for using masstools!")
  message("Version ", masstools_version())
  message("More information: masstools.tidymass.org")
  ascii_art <- c(
    "                       _______          _     ",
    "                      |__   __|        | |    ",
    "  _ __ ___   __ _ ___ ___| | ___   ___ | |___ ",
    " | '_ ` _ \\\\ / _` / __/ __| |/ _ \\\\ / _ \\\\| / __|",
    " | | | | | | (_| \\\\__ \\\\__ \\\\ | (_) | (_) | \\\\__ \\\\",
    " |_| |_| |_|\\\\__,_|___/___/_|\\\\___/ \\\\___/|_|___/",
    "                                              ",
    "                                              "
  )
  cli::cat_line(ascii_art)
  invisible(NULL)
}

masstools_version <- function() {
  as.character(utils::packageVersion("masstools"))
}
