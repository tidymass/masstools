#' Retrieve Metabolite Information from ChemSpider
#'
#' Query the ChemSpider API and return detailed metabolite information for one
#' or more ChemSpider IDs.
#'
#' @param chemspider_id Numeric or character. The ChemSpider compound ID(s) to query.
#' @param chemspider_apikey Character. The API key required to access the ChemSpider database.
#'   **How to Obtain a ChemSpider API Key:**
#'   1. Visit the **Royal Society of Chemistry (RSC) Developer Portal**:
#'      [https://developer.rsc.org/](https://developer.rsc.org/)
#'   2. Sign in or create an account.
#'   3. Navigate to **My Account** > **API Keys**.
#'   4. Apply for access to the **ChemSpider API**.
#'   5. Once approved, generate your API key and copy it for use.
#'   6. Use this key as the `chemspider_apikey` argument in this function.
#'
#' @return A data frame containing metabolite properties retrieved from ChemSpider.
#'   If the request fails, the function returns `NA`.
#' @export
#' @examples
#' \dontrun{
#' retrieve_chemspider_metabolite(chemspider_id = 12345, chemspider_apikey = "your_key")
#' }
retrieve_chemspider_metabolite <-
  function(chemspider_id, chemspider_apikey = "") {
    if (chemspider_apikey == "") {
      stop("Please provide the ChemSpider API key.")
    }

    fields <- c(
      "SMILES",
      "Formula",
      "InChI",
      "InChIKey",
      "StdInChI",
      "StdInChIKey",
      "AverageMass",
      "MolecularWeight",
      "MonoisotopicMass",
      "NominalMass",
      "CommonName",
      "ReferenceCount",
      "DataSourceCount",
      "PubMedCount",
      "RSCCount"
    )
    chemspider_id <- as.numeric(chemspider_id)
    headers <- c("Content-Type" = "", "apikey" = chemspider_apikey)
    body <- list("recordIds" = chemspider_id[!is.na(chemspider_id)], "fields" = fields)
    body <- jsonlite::toJSON(body)

    qurl <- "https://api.rsc.org/compounds/v1/records/batch"

    postres <- try(httr::RETRY(
      "POST",
      url = qurl,
      httr::add_headers(.headers = headers),
      body = body,
      terminate_on = 404,
      quiet = TRUE
    ),
    silent = TRUE)

    if (inherits(postres, "try-error")) {
      return(NA)
    }

    if (postres$status_code == 200) {
      res <- jsonlite::fromJSON(rawToChar(postres$content))$records
      if (length(res) == 0) {
        return(NA)
      }
      out <- data.frame(id = chemspider_id)
      out <- dplyr::left_join(out, res, by = "id")
      out$id <- as.character(out$id)
      return(out)
    } else {
      return(NA)
    }
  }
