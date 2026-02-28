#' Retrieve Supported Identifier Conversion Systems
#'
#' `request_metabolite_id_systems()` is deprecated. Please use
#' `list_metabolite_id_systems()` instead.
#'
#' @inheritParams list_metabolite_id_systems
#' @return A data frame or vector containing the supported identifier
#'   conversions for the selected backend.
#' @export
#' @examples
#' request_metabolite_id_systems(server = "cts.fiehnlab", source_format = "local")
request_metabolite_id_systems <-
  function(server = c("cts.fiehnlab", "chemspider", "openai"),
           source_format = c("local", "online")) {
    lifecycle::deprecate_soft(
      when = "0.99.1",
      what = "request_metabolite_id_systems()",
      with = "list_metabolite_id_systems()"
    )
    list_metabolite_id_systems(
      server = server,
      source_format = source_format
    )
  }
