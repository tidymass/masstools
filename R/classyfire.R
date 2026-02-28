#' S4 Class for ClassyFire Results
#'
#' Store the result of a ClassyFire query, including compound information,
#' taxonomy, free-text description, and external descriptors.
#'
#' @slot compound_info A tibble containing basic compound information such as
#' SMILES, InChIKey, Formula, and Mass.
#' @slot taxonomy_tree A tibble representing the taxonomic tree of the compound
#' that includes classifications like Kingdom, Superclass, Class, and Subclass.
#' @slot classification_info A tibble containing a detailed taxonomic
#' classification of the compound.
#' @slot description A tibble holding the compound's description from ClassyFire.
#' @slot external_descriptors A tibble with external descriptors related to the
#' compound.
#'
#' @name classyfire
#' @docType class
#' @seealso \code{\link[methods:show]{show}}
#' @importFrom methods setClass setMethod setOldClass
#' @importMethodsFrom methods show
#'
#' @author Xiaotao Shen <xiaotao.shen@outlook.com>
methods::setOldClass(c("tbl_df", "tbl", "data.frame"))

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


#' Print a `classyfire` Object
#'
#' Print the main fields of a `classyfire` object, including compound
#' information and taxonomy.
#'
#' @param object An object of class 'classyfire'.
#'
#' @return `NULL`. This method is called for its side effect of printing a summary of the 'classyfire' object.
#' @examples
#' \donttest{
#' library(tibble)
#'
#' cf <- methods::new("classyfire",
#'   compound_info = tibble::tibble(
#'     name = c("SMILES", "InChIKey", "Formula", "Mass"),
#'     value = c("C(C(=O)O)N", "BQJCRHHNABKAKU-KBQPJGBKSA-N", "C2H5NO2", "75.07")
#'   ),
#'   taxonomy_tree = tibble::tibble(
#'     name = c("Kingdom", "Superclass", "Class"),
#'     value = c("Organic compounds", "Organonitrogen compounds", "Amino acids")
#'   ),
#'   classification_info = tibble::tibble(), 
#'   description = tibble::tibble(),
#'   external_descriptors = tibble::tibble()
#' )
#'
#' # call show method
#' show(cf)
#' }
#'
#' @author Xiaotao Shen <xiaotao.shen@outlook.com>
#' @export

setMethod("show",
          signature = "classyfire",
          function(object) {
            pkg_version <- utils::packageVersion("masstools")
            message_to_right <- paste0("masstools v", pkg_version)
            message(cli::rule(
              left = crayon::bold("classyfire Object"),
              right = message_to_right
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
