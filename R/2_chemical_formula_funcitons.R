#' Parse a Molecular Formula
#'
#' Break a molecular formula into element symbols and counts.
#'
#' @param formula Character string containing a molecular formula such as
#'   `"H2O"` or `"C6H12O6"`.
#'
#' @return A data frame with columns `Element` and `Count`.
#'
#' @examples
#' parse_chemical_formula("H2O")       # Returns a data frame with H:2 and O:1
#' parse_chemical_formula("C6H12O6")   # Returns a data frame with C:6, H:12, and O:6
#' parse_chemical_formula("Fe2O3")     # Returns a data frame with Fe:2 and O:3
#'
#' @author Xiaotao Shen \email{xiaotao.shen@outlook.com}
#' @title Parse a Molecular Formula
#' @description Parse a molecular formula into element symbols and counts.
#' @export

parse_chemical_formula <-
  function(formula) {
    if (!check_chemical_formula(formula)) {
      stop("Invalid chemical formula")
    }
    
    # This regex matches an uppercase letter followed by lowercase letters (for the element)
    # and then possibly followed by a number (for the count).
    matches <-
      regmatches(formula, gregexpr("[A-Z][a-z]*\\d*", formula))[[1]]
    
    elements <- c()
    counts <- c()
    
    for (match in matches) {
      # Split the match into element and count.
      parts <-
        unlist(strsplit(match, "(?<=\\D)(?=\\d)|(?<=\\d)(?=\\D)", perl = TRUE))
      
      elements <- c(elements, parts[1])
      
      # If there's a count, use it, otherwise assume 1.
      if (length(parts) > 1) {
        counts <- c(counts, as.integer(parts[2]))
      } else {
        counts <- c(counts, 1)
      }
    }
    
    result <-
      data.frame(Element = elements, Count = counts)
    return(result)
  }






#' Calculate the Mass of a Molecular Formula
#'
#' Calculate the exact or average mass of a molecular formula from its element
#' composition.
#'
#' @param formula Character string containing a molecular formula.
#' @param which Character string specifying whether to calculate `"exact_mass"`
#'   or `"average_mass"`.
#'
#' @return A named numeric value giving the calculated mass.
#'
#' @examples
#' calculate_mass("H2O")                      # Using default exact_mass
#' calculate_mass("H2O", which = "average_mass")
#' calculate_mass("C6H12O6", which = "exact_mass")
#'
#' @author Xiaotao Shen \email{xiaotao.shen@outlook.com}
#' @title Calculate the Mass of a Molecular Formula
#' @description Calculate the exact or average mass of a molecular formula.
#' @export
calculate_mass <-
  function(formula,
           which = c("exact_mass", "average_mass")) {
    ##check formula
    if (!check_chemical_formula(formula = formula)) {
      stop("Invalid chemical formula")
    }
    which <-
      match.arg(which)
    elements <-
      parse_chemical_formula(formula = formula)
    element_info <- chemical_elements_information()
    
    # Calculate accurate mass
    if (which == "exact_mass") {
      mass <-
        seq_len(nrow(elements)) %>%
        vapply(function(idx) {
          element_info$accurate_mass[elements$Element[idx] == element_info$element] * elements$Count[idx]
        }, FUN.VALUE = numeric(1)) %>%
        sum()
    } else {
      mass <-
        seq_len(nrow(elements)) %>%
        vapply(function(idx) {
          element_info$average_mass[elements$Element[idx] == element_info$element] * elements$Count[idx]
        }, FUN.VALUE = numeric(1)) %>%
        sum()
    }
    names(mass) <-
      formula
    return(mass)
  }


#' @title Convert Precursor m/z to Accurate Mass
#' @description Calculate neutral mass from a precursor m/z value and adduct.
#' @param precursor_mz Numeric precursor m/z value.
#' @param adduct Character string describing the adduct, for example
#'   `"(M-H)-"` or `"(2M-2H+Na)-"`.
#' @return A numeric value giving the calculated neutral mass.
#' @author Xiaotao Shen <xiaotao.shen@outlook.com>
#' @export
#' @examples
#' convert_precursor_mz2accurate_mass(805.559, "(2M-2H+Na)-")
#' convert_precursor_mz2accurate_mass(805.559, "(M-H)-")
convert_precursor_mz2accurate_mass <-
  function(precursor_mz = 805.559,
           adduct = "(2M-2H+Na)-") {
    precursor_mz <- as.numeric(precursor_mz)
    if (!check_adduct_formula(adduct)) {
      stop("Invalid adduct formula, it should be like (2M-2H+Na)-")
    }
    if (is.na(precursor_mz)) {
      return(NA)
    }
    
    if (adduct == "(M)+" || adduct == "(M)-") {
      return(precursor_mz)
    }
    
    element <-
      adduct %>%
      stringr::str_replace("\\(", "") %>%
      stringr::str_replace("\\)[+-]$", "") %>%
      stringr::str_split("[\\-\\+]{1}")
    
    element <- element[[1]][-1]
    
    element_number <-
      stringr::str_extract(element, "[0-9]{1,3}") %>%
      as.numeric()
    
    element_number[is.na(element_number)] <- 1
    
    element <-
      element %>%
      stringr::str_replace("[0-9]{1,3}", "") %>%
      stringr::str_replace_all("\\(\\)", "")
    
    plus_minus <-
      stringr::str_extract_all(adduct, "[\\-\\+]{1}")[[1]]
    
    plus_minus <- plus_minus[-length(plus_minus)]
    
    df <-
      data.frame(plus_minus, element_number, element)
    
    if (nrow(df) == 0) {
      return(NA)
    }
    
    df$element[df$element == "ACN"] <- "C2H3N"
    df$element[df$element == "MeOH"] <- "CH4O"
    
    df$element_mass <-
      df$element %>%
      purrr::map(function(x) {
        tryCatch(
          calculate_mass(x, "exact_mass"),
          error = function(e)
            NA
        )
      }) %>%
      unlist()
    
    df <-
      df %>%
      dplyr::mutate(pos_neg =
                      dplyr::case_when(
                        plus_minus == "+" ~ 1,
                        plus_minus == "-" ~ -1
                      ))
    
    additional_mass <-
      (df$element_mass * df$element_number) * df$pos_neg
    
    precursor_number <-
      stringr::str_extract(adduct, "^[0-9]{1,2}") %>%
      as.numeric()
    
    if (is.na(precursor_number)) {
      precursor_number <- 1
    }
    
    (precursor_mz + sum(additional_mass)) / precursor_number
  }



