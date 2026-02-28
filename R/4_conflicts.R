masstools_conflict_message <- function(x) {
    if (length(x) == 0) {
          return("")
      }

    header <- cli::rule(
        left = crayon::bold("Conflicts"),
        right = "report_conflicts()"
    )

    pkgs <- x %>% purrr::map(~ gsub("^package:", "", .))
    others <- pkgs %>% purrr::map(`[`, -1)
    other_calls <- purrr::map2_chr(
        others,
        names(others),
        ~ paste0(crayon::blue(.x),
            "::", .y, "()",
            collapse = ", "
        )
    )

    winner <- pkgs %>% purrr::map_chr(1)
    funs <-
        format(paste0(crayon::blue(winner), "::", crayon::green(paste0(names(
            x
        ), "()"))))
    bullets <- paste0(crayon::red(cli::symbol$cross),
        " ",
        funs,
        " masks ",
        other_calls,
        collapse = "\n"
    )

    paste0(header, "\n", bullets)
}

#' @export
print.masstools_conflicts <- function(x, ..., startup = FALSE) {
    cli::cat_line(masstools_conflict_message(x))
}

#' @importFrom magrittr %>%
confirm_conflict <- function(packages, name) {
    # Only look at functions
    objs <- packages %>%
        purrr::map(~ get(name, pos = .)) %>%
        purrr::keep(is.function)

    if (length(objs) <= 1) {
          return()
      }

    # Remove identical functions
    objs <- objs[!duplicated(objs)]
    packages <- packages[!duplicated(packages)]
    if (length(objs) == 1) {
          return()
      }

    packages
}

ls_env <- function(env) {
    x <- ls(pos = env)
    if (identical(env, "package:dplyr")) {
        x <- setdiff(x, c("intersect", "setdiff", "setequal", "union"))
    }
    x
}
