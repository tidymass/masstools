#' Parse a Chemical Formula into its Constituent Elements and Counts
#'
#' This function breaks down a chemical formula into its individual elements and their respective counts.
#' If no count is specified for an element, it defaults to 1.
#' The result is presented as a data frame with columns for elements and counts.
#'
#' @param formula Character string representing the chemical formula to be parsed.
#'
#' @return A data frame with two columns: 'Element' and 'Count', representing each element and its count in the given formula.
#'
#' @examples
#' parse_chemical_formula("H2O")       # Returns a data frame with H:2 and O:1
#' parse_chemical_formula("C6H12O6")   # Returns a data frame with C:6, H:12, and O:6
#' parse_chemical_formula("Fe2O3")     # Returns a data frame with Fe:2 and O:3
#'
#' @author Xiaotao Shen \email{shenxt1990@outlook.com}
#' @title Parse a Chemical Formula into its Constituent Elements and Counts
#' @description Breaks down a chemical formula into its individual elements and counts,
#' returning the results as a data frame.
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






#' Calculate Mass of a Chemical Formula
#'
#' This function computes the mass of a given chemical formula based on either exact or average mass of its constituent elements.
#'
#' @param formula Character string representing the chemical formula for which the mass is to be computed.
#' @param which Character string indicating the type of mass to be computed. Possible values are "exact_mass" or "average_mass". Default is "exact_mass".
#'
#' @return A numeric value representing the computed mass of the given chemical formula based on the specified type (exact or average).
#'
#' @examples
#' calculate_mass("H2O")                      # Using default exact_mass
#' calculate_mass("H2O", which = "average_mass")
#' calculate_mass("C6H12O6", which = "exact_mass")
#'
#' @author Xiaotao Shen \email{shenxt1990@outlook.com}
#' @title Calculate Mass of a Chemical Formula
#' @description Computes the mass of a given chemical formula based either on the exact mass or the average mass of its constituent elements.
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
    
    # Calculate accurate mass
    if (which == "exact_mass") {
      mass <-
        seq_len(nrow(elements)) %>%
        sapply(function(idx) {
          chemical_elements_information$accurate_mass[elements$Element[idx] == chemical_elements_information$element] * elements$Count[idx]
        }) %>%
        sum()
    } else{
      mass <-
        seq_len(nrow(elements)) %>%
        sapply(function(idx) {
          chemical_elements_information$average_mass[elements$Element[idx] == chemical_elements_information$element] * elements$Count[idx]
        }) %>%
        sum()
    }
    names(mass) <-
      formula
    return(mass)
  }


#' @title Convert Precursor m/z to Accurate Mass
#' @description This function computes the accurate mass from the given precursor m/z and adduct type.
#' It uses specific rules to adjust the provided m/z based on the adduct type,
#' and then calculates the accurate mass of the molecule.
#' @param precursor_mz A numeric value representing the precursor m/z value.
#' Default is 805.559.
#' @param adduct A character string representing the type of adduct.
#' Possible values include but are not limited to "M+", "M-", and "2M-2H+Na".
#' Default is "2M-2H+Na".
#' @return A numeric value representing the accurate mass of the molecule
#' based on the precursor m/z and adduct type.
#' @author Xiaotao Shen <shenxt1990@outlook.com>
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
    
    if (adduct == "(M)+" | adduct == "(M)-") {
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
                      case_when(plus_minus == "+" ~ 1, plus_minus == "-" ~ -1))
    
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





## ----------------------------------------------------------------------------
#' Summation of Chemical Formulas with Adducts
#'
#' Combines a chemical formula with a specified adduct, and returns the resultant
#' summed formula. This function can handle addition or subtraction of elements
#' from the main formula based on the adduct.
#'
#' @param formula A character string representing the base chemical formula.
#' Default is "C9H11NO2".
#' @param adduct A character string representing the adduct to be added or
#' subtracted from the base formula. Examples include "M-H2O+H", "M+", and "M-".
#' Default is "M-H2O+H".
#'
#' @return A character string of the resulting chemical formula after combining
#' the base formula with the adduct. If an error occurs or the summation results
#' in negative counts for any element, it returns `NA`.
#' @author Xiaotao Shen <shenxt1990@outlook.com>
#' @export
#' @examples
#' sum_formula(formula = "C9H11NO2", adduct = "M+H")
#' sum_formula(formula = "C9H11NO2", adduct = "M+")
#' sum_formula(formula = "C9H11NO2", adduct = "M+CH3COOH")
#' sum_formula(formula = "C9H11", adduct = "M-H20")
sum_formula <-
  function(formula = "C9H11NO2", adduct = "M-H2O+H") {
    if (is.na(formula)) {
      return(NA)
    }
    
    if (is.na(adduct)) {
      return(formula)
    }
    
    if (adduct == "M+" | adduct == "M-") {
      return(formula)
    }
    
    formula1 <- split_formula(formula)
    adduct1 <-
      strsplit(x = adduct, split = "\\-|\\+")[[1]][-1]
    
    polymer <-
      as.numeric(gsub(
        pattern = "M",
        replacement = "",
        strsplit(x = adduct, split = "\\-|\\+")[[1]][1]
      ))
    
    if (is.na(polymer)) {
      polymer <- 1
    }
    
    plusorminus <- strsplit(x = adduct, split = "")[[1]]
    plusorminus <-
      grep("\\+|\\-", plusorminus, value = TRUE)
    
    formula1$number <- formula1$number * polymer
    
    adduct1 <- mapply(function(x, y) {
      temp <- split_formula(x)
      temp$number <- temp$number * ifelse(y == "+", 1, -1)
      list(temp)
    }, x = adduct1, y = plusorminus)
    
    adduct1 <- do.call(rbind, adduct1)
    
    formula <- rbind(formula1, adduct1)
    rownames(formula) <- NULL
    
    unique.element <- unique(formula$element)
    if (length(unique.element) == nrow(formula)) {
      if (any(formula$number < 0)) {
        return(NA)
      } else {
        formula$number[formula$number == 1] <- "W"
        formula <-
          paste(paste(formula$element, formula$number, sep = ""),
                collapse = "")
        formula <- strsplit(formula, split = "")[[1]]
        formula[formula == "W"] <- ""
        formula <- paste(formula, collapse = "")
        return(formula)
      }
    } else {
      formula <- lapply(unique.element, function(x) {
        formula[formula$element == x, , drop = FALSE]
      })
      
      formula <- lapply(formula, function(x) {
        data.frame(unique(x$element), sum(x$number), stringsAsFactors = FALSE)
      })
      
      formula <- do.call(rbind, formula)
      formula <- formula[formula[, 2] != 0, ]
      colnames(formula) <- c("element", "number")
      if (any(formula$number < 0)) {
        return(NA)
      } else {
        formula$number[formula$number == 1] <- "W"
        formula <-
          paste(paste(formula$element, formula$number, sep = ""),
                collapse = "")
        formula <- strsplit(formula, split = "")[[1]]
        formula[formula == "W"] <- ""
        formula <- paste(formula, collapse = "")
        return(formula)
      }
    }
  }
