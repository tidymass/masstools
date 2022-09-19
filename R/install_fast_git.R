#' ####install_fastgit, credit to Shixiang Wang
#' #' @title install_fastgit
#' #' @param pkg pkg name from github, gitlab or gitee
#' #' @param from gitlab, github or gitee
#' #' @param spectrum1_name Name of spectrum1
#' #' @param spectrum2_name Name of spectrum2
#' #' @param range.mz range.mz
#' #' @param ppm.tol ppm.tol
#' #' @param mz.ppm.thr mz.ppm.thr
#' #' @param xlab xlab.
#' #' @param ylab ylab.
#' #' @param col1 Color 1.
#' #' @param col2 Color 2.
#' #' @param title.size title.size
#' #' @param lab.size lab.size
#' #' @param axis.text.size axis.text.size.
#' #' @param legend.title.size legend.title.size
#' #' @param legend.text.size legend.text.size
#' #' @param interactive_plot Interactive plot or not.
#' #' @return Return a MS2 spectrum.
#' #' @export
#' install_fastgit <- function(pkg, gitee = FALSE, ...) {
#'   stopifnot(is.logical(gitee))
#'   if (gitee) {
#'     if (!grepl("/", pkg))
#'       stop("Invalid package name, should in name/repo format.")
#'     verbose_git()
#'     remotes::install_git(paste0("https://gitee.com/", pkg), ...)
#'   } else {
#'     if (any(grepl(":", pkg))) {
#'       verbose_git()
#'       remotes::install_git(pkg, ...)
#'     } else {
#'       if (any(grepl("/", pkg))) {
#'         verbose_git()
#'         tryCatch(
#'           remotes::install_git(paste0("https://hub.fastgit.org/", pkg)),
#'           error = function(e) {
#'             message("Install error when use GitHub mirror, roll back to official GitHub.")
#'             verbose_github()
#'             remotes::install_github(pkg)
#'           }
#'         )
#'       } else {
#'         verbose_bioc()
#'         BiocManager::install(pkg)
#'       }
#'     }
#'   }
#' }