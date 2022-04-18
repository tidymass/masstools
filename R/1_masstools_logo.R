#' @title masstools_logo
#' @description Get the detailed of metPath package.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @importFrom stringr str_replace str_split str_replace_all str_trim
#' @importFrom dplyr filter mutate select everything
#' @importFrom rstudioapi isAvailable hasFun getThemeInfo
#' @importFrom utils packageDescription write.csv
#' @importFrom cli rule symbol
#' @importFrom crayon green blue col_align
#' @importFrom crayon red black white style make_style num_colors
#' @importFrom plotly ggplotly
#' @importFrom pbapply pblapply
#' @import ggplot2
#' @importFrom plyr dlply .
#' @import readxl
#' @importFrom methods .hasSlot new is
#' @importFrom stats p.adjust rgamma
#' @importFrom utils data str
#' @importFrom magrittr %>%
#' @importFrom purrr map map2 walk
#' @import htmltools
#' @import tidyr
#' @importFrom BiocParallel bplapply
#' @importFrom MSnbase readMSData
#' @importFrom ProtGenerics spectra
#' @export
#' @return masstools logo
#' @examples
#' masstools_logo()
masstools_logo <- function() {
  message(crayon::green("Thank you for using masstools!\n"))
  message(crayon::green("Version", masstools_version, "(", update_date, ')\n'))
  message(crayon::green(
        "More information: search 'tidymass masstools'\n"
    ))
  cat(crayon::green(
        c(
            "  _   _          _______          _     ",
            " | | (_)        |__   __|        | |    ",
            " | |_ _ _ __  _   _| | ___   ___ | |___ ",
            " | __| | '_ \\| | | | |/ _ \\ / _ \\| / __|",
            " | |_| | | | | |_| | | (_) | (_) | \\__ \\",
            "  \\__|_|_| |_|\\__, |_|\\___/ \\___/|_|___/",
            "               __/ |                    ",
            "              |___/                     "
        )
    ), sep = "\n")
}

masstools_version <- 
  utils::packageVersion(pkg = "masstools")
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
