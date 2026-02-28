#' Retrieve Compound Classification Information
#'
#' `get_compound_class()` is deprecated. Please use
#' `retrieve_compound_class()` instead.
#'
#' @inheritParams retrieve_compound_class
#' @return An S4 object of class `"classyfire"`, or `NA` on failure.
#' @examples
#' \dontrun{
#' get_compound_class(
#'   inchikey = "QZDWODWEESGPLC-UHFFFAOYSA-N",
#'   server = "http://classyfire.wishartlab.com/entities/",
#'   sleep = 5
#' )
#' }
#' @export
get_compound_class <-
  function(inchikey = "QZDWODWEESGPLC-UHFFFAOYSA-N",
           server = "http://classyfire.wishartlab.com/entities/",
           sleep = 5) {
    lifecycle::deprecate_soft(
      when = "0.99.1",
      what = "get_compound_class()",
      with = "retrieve_compound_class()"
    )
    retrieve_compound_class(
      inchikey = inchikey,
      server = server,
      sleep = sleep
    )
  }
