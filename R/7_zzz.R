.onAttach <- function(...) {
  packageStartupMessage(paste0("masstools ", masstools_version()))
}
