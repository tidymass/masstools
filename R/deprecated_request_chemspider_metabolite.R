#' Retrieve Metabolite Information from ChemSpider
#'
#' `request_chemspider_metabolite()` is deprecated. Please use
#' `retrieve_chemspider_metabolite()` instead.
#'
#' @inheritParams retrieve_chemspider_metabolite
#' @return A data frame of ChemSpider properties, or `NA` on failure.
#' @export
#' @examples
#' \dontrun{
#' request_chemspider_metabolite(chemspider_id = 12345, chemspider_apikey = "your_key")
#' }
request_chemspider_metabolite <-
  function(chemspider_id, chemspider_apikey = "") {
    lifecycle::deprecate_soft(
      when = "0.99.1",
      what = "request_chemspider_metabolite()",
      with = "retrieve_chemspider_metabolite()"
    )
    retrieve_chemspider_metabolite(
      chemspider_id = chemspider_id,
      chemspider_apikey = chemspider_apikey
    )
  }
