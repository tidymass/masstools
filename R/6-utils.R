# msg <- function(..., startup = FALSE) {
#   if (startup) {
#     if (!isTRUE(getOption("masstools.quiet"))) {
#       packageStartupMessage(text_col(...))
#     }
#   } else {
#     message(text_col(...))
#   }
# }

# text_col <- function(x) {
#   # If RStudio not available, messages already printed in black
#   if (!rstudioapi::isAvailable()) {
#     return(x)
#   }
#
#   if (!rstudioapi::hasFun("getThemeInfo")) {
#     return(x)
#   }
#
#   theme <- rstudioapi::getThemeInfo()
#
#   if (isTRUE(theme$dark)) {
#     crayon::white(x)
#   } else {
#     crayon::black(x)
#   }
# }

#' List all packages in the masstools
#'
#' @param include_self Include masstools in the list?
#' @export
#' @return masstools packages
#' @examples
#' masstools_packages()
masstools_packages <- function(include_self = TRUE) {
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

invert <- function(x) {
  if (length(x) == 0) {
    return()
  }
  stacked <- utils::stack(x)
  tapply(as.character(stacked$ind), stacked$values, list)
}


style_grey <- function(level, ...) {
  crayon::style(paste0(...),
                crayon::make_style(grDevices::grey(level), grey = TRUE))
}


























# # NOTE: keep in sync with install-patRoon version
# findPWizPath <- function() {
#   # try to find ProteoWizard
#   # order: options --> win registry --> PATH
#   # the PATH is searched last because
#    OpenMS might have added its own old version.
#
#   path <- getOption("patRoon.path.pwiz")
#   if (!is.null(path) & nzchar(path))
#     return(path)
#
#   if (Sys.info()[["sysname"]] == "Windows")
#   {
#     # Inspired by scan_registry_for_rtools() from pkgload
#     key <- "Directory\\shell\\Open with SeeMS\\command"
#     reg <-
#       tryCatch(
#         utils::readRegistry(key, "HCR"),
#         error = function(e)
#           NULL
#       )
#
#     # not sure if this might occur
#     if (is.null(reg))
#       reg <-
#       tryCatch(
#         utils::readRegistry(key, "HLM"),
#         error = function(e)
#           NULL
#       )
#
#     if (!is.null(reg))
#     {
#       path <-
#         tryCatch(
#           dirname(sub("\"([^\"]*)\".*", "\\1", reg[[1]])),
#           error = function(e)
#             NULL
#         )
#       if (!is.null(path) &
#           file.exists(file.path(path, "msconvert.exe")))
#         # extra check: see if msconvert is there
#         return(path)
#     }
#   }
#
#   # check PATH
#   msc <-
#     if (Sys.info()[["sysname"]] == "Windows")
#       "msconvert.exe"
#   else
#     "msconvert"
#   path <- dirname(Sys.which(msc))
#   if (nzchar(path))
#     return(path)
#
#   return(NULL)
# }



#' @title show_progresser
#' @description show_progresser
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param index index for loop
#' @param progresser progresser
#' @return A data.frame
#' @importFrom magrittr %>%
#' @importFrom dplyr case_when everything select filter
#' @importFrom purrr map map2 walk
#' @importFrom crayon green
#' @export

show_progresser <-
  function(index = 1:1000,
           progresser = c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) {
    idx <-
      seq(
        from = 1,
        to = max(index),
        length.out = length(progresser)
      ) %>%
      round()
    
    data.frame(idx = idx,
               progresser = paste0(progresser, "%"))
  }





#' @title install_fastgit
#' @description install packages from fastgit. Credit to Shixiang Wang
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param pkg pkg name from github, gitlab or gitee, "name/repo" format
#' @param from gitlab, github or gitee.
#' @param ... Other parameters for install_git
#' @return NULL
#' @export

install_fastgit <-
  function(pkg,
           from = c("gitee", "gitlab", "github"),
           ...) {
    from <- match.arg(from)
    
    if (from == "gitee") {
      if (!grepl("/", pkg)) {
        stop("Invalid package name, should in 'name/repo' format.")
      }
      remotes::install_git(paste0("https://gitee.com/", pkg), ...)
    } else {
      if (any(grepl(":", pkg))) {
        remotes::install_git(pkg, ...)
      } else {
        if (any(grepl("/", pkg))) {
          tryCatch(
            remotes::install_git(paste0("https://hub.fastgit.org/", pkg)),
            error = function(e) {
              message("Install error when use GitHub mirror, roll back to official GitHub.")
              remotes::install_github(pkg)
            }
          )
        }
      }
    }
  }
