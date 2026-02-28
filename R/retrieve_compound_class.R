#' Retrieve Compound Classification Information
#'
#' Query the ClassyFire service and return the taxonomic classification for a
#' compound identified by InChIKey.
#'
#' @param inchikey A character string representing the InChIKey of the compound.
#' Default is "QZDWODWEESGPLC-UHFFFAOYSA-N".
#' @param server A character string representing the base URL of the ClassyFire server.
#' Default is "http://classyfire.wishartlab.com/entities/".
#' @param sleep A numeric value indicating the time (in seconds) to pause between the
#' request and fetching the result to avoid overloading the server. Default is 5 seconds.
#'
#' @return An S4 object of class "classyfire" containing details about the compound,
#' including its basic information, taxonomic classification, description, and external
#' descriptors. If the InChIKey is not found in the database or there's an error, it
#' returns relevant NA-filled placeholders and warnings.
#'
#' @author Xiaotao Shen <xiaotao.shen@outlook.com>
#' @export
#' @examples
#' \dontrun{
#' retrieve_compound_class(
#' inchikey = "QZDWODWEESGPLC-UHFFFAOYSA-N",
#' server = "http://classyfire.wishartlab.com/entities/",
#' sleep = 5
#' )
#' }
retrieve_compound_class <-
  function(inchikey = "QZDWODWEESGPLC-UHFFFAOYSA-N",
           server = "http://classyfire.wishartlab.com/entities/",
           sleep = 5) {
    url <- paste(server, inchikey, sep = "")
    Sys.sleep(time = sleep)
    result <- try(expr = xml2::read_html(url), silent = TRUE)
    if (inherits(result, "try-error")) {
      message(
        crayon::red(
          inchikey,
          "is not available in website.\nPlease check this link:\n",
          url
        )
      )
      return(NA)
    }
    result <-
      try(result %>% rvest::html_nodes(css = ".main_card"), silent = TRUE)

    if (inherits(result, "try-error")) {
      message(
        crayon::red(
          inchikey,
          "is not available in website.\nPlease check this link:\n",
          url
        )
      )
      return(NA)
    }
    result <- try(rvest::html_text(x = result, trim = TRUE))
    if (inherits(result, "try-error")) {
      message(
        crayon::red(
          inchikey,
          "is not available in website.\nPlease check this link:\n",
          url
        )
      )
      return(NA)
    }

    compound_info <-
      try(result[[1]] %>%
            stringr::str_replace_all("\n", "{}") %>%
            stringr::str_split("\\{\\}") %>% `[[`(1) %>%
            stringr::str_trim(side = "both") %>%
            tibble::enframe(name = NULL) %>%
            dplyr::filter(value != "") %>% dplyr::pull(value) %>%
            lapply(function(x) {
              if (x %in% c("SMILES", "InChIKey", "Formula", "Mass")) {
                tibble::tibble(name = x, value = .[which(x == .) + 1])
              }
            }) %>% do.call(rbind, .) %>% tibble::as_tibble() %>%
            dplyr::distinct(name, value),
          silent = TRUE)
    classification_info <-
      try(result[[2]] %>%
            stringr::str_replace_all("\n", "{}") %>%
            stringr::str_split("\\{\\}") %>% `[[`(1) %>%
            stringr::str_trim(side = "both") %>%
            tibble::enframe(name = NULL) %>% dplyr::filter(value != "") %>%
            dplyr::filter(!value %in% c("Taxonomic Classification",
                                        "Taxonomy Tree")) %>%
            dplyr::pull(value),
          silent = TRUE)
    idx <-
      which(classification_info == "Kingdom")
    taxonomy_tree <-
      try(classification_info[idx[1]:(idx[2] - 1)] %>%
            matrix(ncol = 2, byrow = TRUE) %>%
            tibble::as_tibble(.name_repair = "minimal"))
    try(colnames(taxonomy_tree) <- c("name", "value"))
    if (inherits(taxonomy_tree, "try-error")) {
      taxonomy_tree <- tibble::tibble(
        name = c("Kingdom", "Superclass",
                 "Class", "Subclass"),
        value = rep(NA, 4)
      )
      message(
        crayon::red(
          inchikey,
          "is not available in website.\n"
        )
      )
    } else {
      message(crayon::green(
        inchikey,
        "is available in website."
      ))
    }
    classification_info <-
      try(result[[2]] %>%
            stringr::str_replace_all("\n", "{}") %>%
            stringr::str_split("\\{\\}") %>% `[[`(1) %>%
            stringr::str_trim(side = "both") %>%
            tibble::enframe(name = NULL) %>%
            dplyr::filter(value != "") %>% dplyr::pull(value) %>%
            lapply(function(x) {
              if (x %in% c(
                "Kingdom",
                "Superclass",
                "Class",
                "Subclass",
                "Intermediate Tree Nodes",
                "Direct Parent",
                "Alternative Parents",
                "Molecular Framework",
                "Substituents"
              )) {
                tibble::tibble(name = x, value = .[which(x == .) + 1])
              }
            }) %>% do.call(rbind, .) %>% tibble::as_tibble() %>%
            dplyr::distinct(name, value),
          silent = TRUE)
    description <-
      try(result[[3]] %>% stringr::str_replace_all("\n", "{}") %>%
            stringr::str_split("\\{\\}") %>% `[[`(1) %>%
            stringr::str_trim(side = "both") %>%
            tibble::enframe(name = NULL) %>%
            dplyr::filter(value != "") %>%
            dplyr::pull(value) %>%
            lapply(function(x) {
              if (x %in% c("Description")) {
                tibble::tibble(name = x, value = .[which(x == .) + 1])
              }
            }) %>%
            do.call(rbind, .) %>% tibble::as_tibble() %>%
            dplyr::distinct(name, value),
          silent = TRUE)
    external_descriptors <-
      try(result[[4]] %>%
            stringr::str_replace_all("\n", "{}") %>%
            stringr::str_split("\\{\\}") %>% `[[`(1) %>%
            stringr::str_trim(side = "both") %>%
            tibble::enframe(name = NULL) %>%
            dplyr::filter(value != "") %>% dplyr::pull(value) %>%
            lapply(function(x) {
              if (x %in% c("External Descriptors")) {
                tibble::tibble(name = x, value = .[which(x == .) + 1])
              }
            }) %>%
            do.call(rbind, .) %>% tibble::as_tibble() %>%
            dplyr::distinct(name, value),
          silent = TRUE)
    if (inherits(compound_info, "try-error")) {
      compound_info <-
        tibble::tibble(
          name = c("SMILES", "InChIKey", "Formula", "Mass"),
          value = rep(NA, 4)
        )
    }
    if (inherits(classification_info, "try-error")) {
      classification_info <-
        tibble::tibble(
          name = c(
            "Kingdom",
            "Superclass",
            "Class",
            "Subclass",
            "Intermediate Tree Nodes",
            "Direct Parent",
            "Alternative Parents",
            "Molecular Framework",
            "Substituents"
          ),
          value = rep(NA, 9)
        )
    }
    if (inherits(description, "try-error")) {
      description <- tibble::tibble(name = "Description",
                                    value = NA)
    }
    if (inherits(external_descriptors, "try-error")) {
      external_descriptors <- tibble::tibble(name = "External Descriptors",
                                             value = NA)
    }
    result <- methods::new(Class = "classyfire")
    result@compound_info <- compound_info
    result@taxonomy_tree <- taxonomy_tree
    result@classification_info <- classification_info
    result@description <- description
    result@external_descriptors <- external_descriptors
    return(result)
  }
