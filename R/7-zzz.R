.onAttach <- function(...) {
    # needed <- core[!is_attached(core)]
    # if (length(needed) == 0) {
    #       return()
    #   }
    # 
    crayon::num_colors(TRUE)
    masstools_attach()

    # if (!"package:conflicted" %in% search()) {
    #     x <- masstools_conflicts()
    #     msg(masstools_conflict_message(x), startup = TRUE)
    # }
  packageStartupMessage(paste0("masstools ", masstools_version, " (", update_date, ')'))
}

is_attached <- function(x) {
    paste0("package:", x) %in% search()
}
