#' @title Masstools Logo Printer
#' @description `masstools_logo` is a function designed to display the masstools logo, 
#' thank the user, show the version information, and provide a URL for 
#' additional details about the package. This can be useful for branding
#' and user interaction when they load or utilize the package.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @importFrom stringr str_replace str_split str_replace_all str_trim str_detect str_extract
#' @importFrom dplyr filter mutate select everything
#' @importFrom utils packageDescription write.csv
#' @importFrom cli rule symbol
#' @importFrom crayon green blue col_align
#' @importFrom crayon red black white style make_style num_colors
#' @importFrom pbapply pblapply
#' @importFrom methods .hasSlot new is
#' @importFrom stats p.adjust rgamma
#' @importFrom utils data str head tail
#' @importFrom magrittr %>%
#' @importFrom purrr map map2 walk
#' @import tidyr
#' @import ggplot2
#' @importFrom MSnbase readMSData
#' @importFrom ProtGenerics spectra
#' @importFrom lifecycle deprecate_soft
#' @importFrom remotes install_git install_github install_gitlab
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom xml2 read_html read_xml xml_text
#' @importFrom rvest html_nodes html_text
#' @export
#' @return Prints messages to the console and does not return any value.
#' @examples
#' masstools_logo()
masstools_logo <- function() {
  message("Thank you for using masstools!")
  message("Version ", masstools_version, " (", update_date, ')')
  message("More information: masstools.tidymass.org")
  cat(
    c(
      "                       _______          _     ",
      "                      |__   __|        | |    ",
      "  _ __ ___   __ _ ___ ___| | ___   ___ | |___ ",
      " | '_ ` _ \\ / _` / __/ __| |/ _ \\ / _ \\| / __|",
      " | | | | | | (_| \\__ \\__ \\ | (_) | (_) | \\__ \\",
      " |_| |_| |_|\\__,_|___/___/_|\\___/ \\___/|_|___/",
      "                                              ",
      "                                              "
    ),
    sep = "\n"
  )
}

masstools_version <-
  as.character(utils::packageVersion(pkg = "masstools"))
update_date <- as.character(Sys.time())

# library(cowsay)
# # https://onlineasciitools.com/convert-text-to-ascii-art
# # writeLines(capture.output(say("Hello"), type = "message"), con = "ascii_art.txt")
# art <- readLines("logo.txt")
# dput(art)
# metid_logo <-
#   c("                _    _____  ___ ", " _ __ ___   ___| |_  \\_   \\/   \\",
#     "| '_ ` _ \\ / _ \\ __|  / /\\/ /\\ /", "| | | | | |  __/ |_/\\/ /_/ /_// ",
#     "|_| |_| |_|\\___|\\__\\____/___,'  ", "                                "
#   )
# cat(metid_logo, sep = "\n")
