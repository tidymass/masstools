#' Report Function Name Conflicts
#'
#' List conflicts between functions exported by masstools and functions from
#' other attached packages.
#'
#' The report ignores `dplyr::intersect()`, `dplyr::union()`,
#' `dplyr::setequal()`, and `dplyr::setdiff()`, because these make the base
#' equivalents generic and are usually harmless in practice.
#'
#' @export
#' @return An object of class `"masstools_conflicts"` describing detected
#'   conflicts.
#' @examples
#' report_conflicts()
report_conflicts <- function() {
  envs <- grep("^package:", search(), value = TRUE)
  envs <- purrr::set_names(envs)
  objs <- invert(lapply(envs, ls_env))

  conflicts <- purrr::keep(objs, ~ length(.x) > 1)

  tidy_names <- paste0("package:", list_masstools_packages())
  conflicts <- purrr::keep(conflicts, ~ any(.x %in% tidy_names))

  conflict_funs <- purrr::imap(conflicts, confirm_conflict)
  conflict_funs <- purrr::compact(conflict_funs)

  structure(conflict_funs, class = "masstools_conflicts")
}
