#' Convert Chemical Identifier Between Different Databases
#'
#' `trans_ID()` is deprecated. Please use `convert_metabolite_id()` instead.
#'
#' @inheritParams convert_metabolite_id
#' @return A data frame with the input identifier in the first column and the
#'   converted identifier(s) in the second column.
#' @author Xiaotao Shen <xiaotao.shen@outlook.com>
#' @export
#' @examples
#' \dontrun{
#' trans_ID(
#'   query = "C00001",
#'   from = "KEGG",
#'   to = "PubChem SID",
#'   top = 1,
#'   server = "cts.fiehnlab"
#' )
#' }
trans_ID <- function(query = "C00001",
                     from = "KEGG",
                     to = "PubChem SID",
                     top = 1,
                     server = c("cts.fiehnlab", "chemspider", "openai"),
                     chemspider_apikey = "",
                     openai_apikey = "") {
  lifecycle::deprecate_soft(
    when = "0.99.9",
    what = "trans_ID()",
    with = "convert_metabolite_id()"
  )

  convert_metabolite_id(
    query = query,
    from = from,
    to = to,
    top = top,
    server = server,
    chemspider_apikey = chemspider_apikey,
    openai_apikey = openai_apikey
  )
}
