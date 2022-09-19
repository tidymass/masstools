#' #' @title install_fastgit
#' #' @description install packages from fastgit. Credit to Shixiang Wang
#' #' @author Xiaotao Shen
#' #' \email{shenxt1990@@outlook.com}
#' #' @param pkg pkg name from github, gitlab or gitee, "name/repo" format
#' #' @param from gitlab, github or gitee.
#' #' @param ... Other parameters for install_git
#' #' @export
#' 
#' install_fastgit <-
#'   function(pkg,
#'            from = c("gitee", "gitlab", "github"),
#'            ...) {
#'     from <- match.arg(from)
#'     
#'     if (from == "gitee") {
#'       if (!grepl("/", pkg)) {
#'         stop("Invalid package name, should in 'name/repo' format.")
#'       }
#'       remotes::install_git(paste0("https://gitee.com/", pkg), ...)
#'     } else {
#'       if (any(grepl(":", pkg))) {
#'         remotes::install_git(pkg, ...)
#'       } else {
#'         if (any(grepl("/", pkg))) {
#'           tryCatch(
#'             remotes::install_git(paste0("https://hub.fastgit.org/", pkg)),
#'             error = function(e) {
#'               message("Install error when use GitHub mirror, roll back to official GitHub.")
#'               remotes::install_github(pkg)
#'             }
#'           )
#'         } else {
#'           BiocManager::install(pkg)
#'         }
#'       }
#'     }
#'   }
#' 
