#' Validate if a given chemical element symbol is valid
#'
#' This function checks whether a given symbol is a valid chemical element.
#' The function uses a list of known chemical elements up to 2022.
#' It returns TRUE if the symbol is valid, otherwise returns FALSE.
#'
#' @param element A character string representing the chemical element symbol to be validated.
#'
#' @return A logical value: TRUE if the element symbol is valid, FALSE otherwise.
#'
#' @examples
#' check_chemical_element("Fe") # TRUE
#' check_chemical_element("Fz") # FALSE
#'
#' @export

check_chemical_element <-
  function(element) {
    # List of all chemical element symbols up to 2022.
    all_elements <- chemical_elements_information$element
    
    # Use a regex to match chemical element symbols, ensuring proper capitalization.
    matches <- gregexpr("[A-Z][a-z]?", element)
    symbols <- unlist(regmatches(element, matches))
    
    # Validate each symbol against the known elements list.
    is_valid <- all(symbols %in% all_elements)
    
    return(is_valid)
  }


#' Validate if a given chemical formula is a standard valid formula
#'
#' This function checks whether a given chemical formula is valid based on standard conventions.
#' It ensures that element symbols are followed by valid numeric subscripts (if present).
#' The function uses a list of known chemical elements up to 2022.
#' It returns TRUE if the formula is valid, otherwise returns FALSE.
#'
#' @param formula A character string representing the chemical formula to be validated.
#'
#' @return A logical value: TRUE if the formula is valid, FALSE otherwise.
#'
#' @examples
#' check_chemical_formula("H2O") # TRUE
#' check_chemical_formula("2H") # FALSE
#'
#' @export
check_chemical_formula <-
  function(formula) {
    elements <- chemical_elements_information$element
    
    # Ensure the formula contains only valid characters (letters and numbers)
    if (grepl("[^A-Za-z0-9]", formula)) {
      return(FALSE)
    }
    
    # Use regex to match chemical elements followed by optional numeric subscripts.
    # matches <- gregexpr("[A-Z][a-z]?\\d*", formula)
    matches <- gregexpr("\\d*[A-Z][a-z]?\\d*", formula)
    symbols <- unlist(regmatches(formula, matches))
    # Ensure no unmatched parts remain in the formula
    if (paste(symbols, collapse = "") != formula) {
      return(FALSE)
    }
    # Ensure the matched symbols reconstruct the formula
    if (paste(symbols, collapse = "") != formula) {
      return(FALSE)
    }
    
    # Check if each symbol and subscript combination is valid.
    is_valid <- TRUE
    for (symbol in symbols) {
      element_match <- regmatches(symbol, regexpr("[A-Z][a-z]?", symbol))
      subscript_match <- regmatches(symbol, regexpr("\\d+$", symbol))
      
      if (!(element_match %in% elements)) {
        is_valid <- FALSE
        break
      }
      
      if (length(subscript_match) > 0 &&
          as.numeric(subscript_match) <= 0) {
        is_valid <- FALSE
        break
      }
    }
    
    return(is_valid)
  }


#' Validate if a given adduct formula is correct
#'
#' This function checks whether a given adduct formula is valid by comparing it to known valid adducts.
#' It returns TRUE if the adduct formula is valid, otherwise returns FALSE.
#'
#' @param adduct A character string representing the adduct formula to be validated.
#'
#' @return A logical value: TRUE if the adduct formula is valid, FALSE otherwise.
#'
#' @examples
#' check_adduct_formula("(M+H)+") # TRUE
#' check_adduct_formula("(M+X)+") # FALSE
#'
#' @export
check_adduct_formula <-
  function(adduct) {
    # Define the regex pattern to match valid adduct formulas
    pattern <- "^\\([0-9]{0,9}M([+-][A-Za-z0-9]+)*\\)[+-]$"
    
    # Check if the adduct matches the valid pattern
    if (grepl(pattern, adduct)) {
      return(TRUE)
    }
    
    return(FALSE)
  }
