#' Retrieve Supported Identifier Conversion Systems
#'
#' Return the identifier systems supported by the selected conversion backend.
#'
#' @param server Character. The server to retrieve supported identifier formats from.
#' @param source_format Character. Specifies whether to use a **local** or **online** source.
#' @return A data frame or vector containing the supported identifier conversions
#'   for the selected `server`.
#' @details
#' - **CTS FiehnLab**: Queries the CTS endpoint or returns the local cached list.
#' - **ChemSpider**: Returns the supported ChemSpider conversion table.
#' - **OpenAI**: Returns the supported source formats available in the package.
#' @export
#' @examples
#' list_metabolite_id_systems(server = "cts.fiehnlab", source_format = "local")
list_metabolite_id_systems <-
  function(server = c("cts.fiehnlab", "chemspider", "openai"),
           source_format = c("local", "online")) {
    server <- match.arg(server)
    source_format <- match.arg(source_format)
    if (server == "cts.fiehnlab") {
      server <- "https://cts.fiehnlab.ucdavis.edu/service/convert"
    }

    if (server == "chemspider") {
      server <- "https://api.rsc.org/compounds/v1/tools/convert"
    }

    if (server == "https://cts.fiehnlab.ucdavis.edu/service/convert") {
      if (source_format == "local") {
        return(cts_fiehnlab_chemical_sources)
      }
      chemical_database_from <-
        xml2::read_html("http://cts.fiehnlab.ucdavis.edu/rest/fromValues")
      chemical_database_to <-
        xml2::read_html("http://cts.fiehnlab.ucdavis.edu/rest/fromValues")

      chemical_database_from <-
        chemical_database_from %>%
        rvest::html_nodes("p") %>%
        rvest::html_text(TRUE) %>%
        stringr::str_split(pattern = ",") %>%
        `[[`(1) %>%
        lapply(function(x) {
          stringr::str_trim(x, "both") %>%
            stringr::str_replace_all(pattern = '\"', replacement = "") %>%
            stringr::str_replace_all(pattern = ",|\\[", replacement = "")
        }) %>%
        unlist() %>%
        unname()

      chemical_database_to <-
        chemical_database_to %>%
        rvest::html_nodes("p") %>%
        rvest::html_text(TRUE) %>%
        stringr::str_split(pattern = ",") %>%
        `[[`(1) %>%
        lapply(function(x) {
          stringr::str_trim(x, "both") %>%
            stringr::str_replace_all(pattern = '\"', replacement = "") %>%
            stringr::str_replace_all(pattern = ",|\\[", replacement = "")
        }) %>%
        unlist() %>%
        unname()

      chemical_database_from <- sort(chemical_database_from)
      chemical_database_to <- sort(chemical_database_to)

      chemical_database_from
    } else if (server == "https://api.rsc.org/compounds/v1/tools/convert") {
      result <- c(
        "inchikey_csid",
        "inchikey_inchi",
        "inchi_csid",
        "inchi_inchikey",
        "inchi_smiles",
        "smiles_inchi"
      )
      result <-
        stringr::str_split(result, "_") %>%
        dplyr::bind_cols() %>%
        t() %>%
        tibble::as_tibble(.name_repair = "minimal")
      colnames(result) <- c("From", "To")
      message(
        crayon::green(
          nrow(result),
          "are supported in server https://api.rsc.org/compounds/v1/tools/convert."
        )
      )

      result <-
        result %>%
        dplyr::left_join(chemspider_code_name_table(), by = c("From" = "code")) %>%
        dplyr::left_join(chemspider_code_name_table(), by = c("To" = "code")) %>%
        dplyr::select(-c("From", "To")) %>%
        dplyr::rename("From" = "name.x", "To" = "name.y")

      return(result)
    } else if (server %in% c("openai")) {
      openai_chemical_sources
    }
  }
