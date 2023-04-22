#' @title get_compound_class
#' @description Get the class information of a compound using classyfire.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param inchikey The inchikey ID of a metabolite.
#' @param server server.
#' @param sleep Sleep time for system. Second.
#' @return A classyfire class object.
#' @export
#' @examples
#' \dontrun{
#' get_compound_class(
#' inchikey = "QZDWODWEESGPLC-UHFFFAOYSA-N",
#' server = "http://classyfire.wishartlab.com/entities/",
#' sleep = 5
#' )
#' }
get_compound_class <-
  function(inchikey = "QZDWODWEESGPLC-UHFFFAOYSA-N",
           server = "http://classyfire.wishartlab.com/entities/",
           sleep = 5) {
    url <- paste(server, inchikey, sep = "")
    Sys.sleep(time = sleep)
    result <- try(expr = xml2::read_html(url), silent = TRUE)
    if (is(result, class2 = "try-error")) {
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
    
    if (is(result, class2 = "try-error")) {
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
    if (is(result, class2 = "try-error")) {
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
      # try(classification_info %>% `==`("Kingdom") %>% which())
    taxonomy_tree <-
      try(classification_info[idx[1]:(idx[2] - 1)] %>%
            matrix(ncol = 2, byrow = TRUE) %>%
            tibble::as_tibble(.name_repair = "minimal"))
    try(colnames(taxonomy_tree) <- c("name", "value"))
    if (is(taxonomy_tree, class2 = "try-error")) {
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
    if (is(compound_info, class2 = "try-error")) {
      compound_info <-
        tibble::tibble(
          name = c("SMILES", "InChIKey", "Formula", "Mass"),
          value = rep(NA, 4)
        )
    }
    if (is(classification_info, class2 = "try-error")) {
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
    if (is(description, class2 = "try-error")) {
      description <- tibble::tibble(name = "Description",
                                    value = NA)
    }
      if (is(external_descriptors, class2 = "try-error")) {
      external_descriptors <- tibble::tibble(name = "External Descriptors",
                                             value = NA)
    }
    result <- new(Class = "classyfire")
    result@compound_info <- compound_info
    result@taxonomy_tree <- taxonomy_tree
    result@classification_info <- classification_info
    result@description <- description
    result@external_descriptors <- external_descriptors
    return(result)
  }


setClass(
  Class = "classyfire",
  representation = representation(
    compound_info = "tbl_df",
    taxonomy_tree = "tbl_df",
    classification_info = "tbl_df",
    description = "tbl_df",
    external_descriptors = "tbl_df"
  )
)


setMethod("show",
          signature = "classyfire",
          function(object) {
            message(cli::rule(
              left = crayon::bold("classyfire Object"),
              right = paste0("masstools v", utils::packageVersion("masstools"))
            ))
            
            message(crayon::red(
              "Object Size:",
              format(utils::object.size(object), units = "Kb"),
              "\n",
              "\n"
            ))
            
            message(crayon::green("Information:"))
            
            message("SMILES: ",
                    dplyr::pull(object@compound_info, "value")[1])
            message("InChIKey: ",
                    dplyr::pull(object@compound_info, "value")[2])
            message("Formula: ",
                    dplyr::pull(object@compound_info, "value")[3])
            message("Mass: ", dplyr::pull(object@compound_info, "value")[4])
            
            
            tree_list <-
              object@taxonomy_tree
            
            tree_df <- data.frame(
              stringsAsFactors = FALSE,
              id = tree_list$name,
              connections = I(c(
                as.list(tree_list$name)[-1], list(character(0))
              ))
            )
            
            tree_df$label <-
              paste0(crayon::bold(tree_df$id),
                     " : ",
                     cli::col_cyan(tree_list$value))
            
            print(cli::tree(tree_df))
          })
