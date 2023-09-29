## ---------------------------------------------------------------------------
#' @title split_formula
#' @description Split a formula into element and number.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param formula Chemical formula.
#' @return A spited formula.
#' @export
#' @examples
#' split_formula(formula = "C9H11NO2")
#' split_formula(formula = "CH3")
split_formula <-
  function(formula = "C9H11NO2") {
    temp.formula <- strsplit(formula, split = "")[[1]]
    
    number <- NULL
    for (i in seq_along(temp.formula)) {
      if (length(grep("[0-9]{1}", temp.formula[i])) == 0) {
        break
      }
      number[i] <- temp.formula[i]
    }
    
    if (!is.null(number)) {
      number <- as.numeric(paste(number, collapse = ""))
    } else {
      number <- 1
    }
    
    ## first select the Na, Cl and so on element
    idx1 <- gregexpr("[A-Z][a-z][0-9]*", formula)[[1]]
    len1 <- attributes(idx1)$match.length
    
    ## no double element
    if (idx1[1] == -1) {
      double.formula <- matrix(NA, ncol = 2)
      formula1 <- formula
    } else {
      double.letter.element <- NULL
      double.number <- NULL
      remove.idx <- NULL
      for (i in seq_along(idx1)) {
        double.letter.element[i] <-
          substr(formula, idx1[i], idx1[i] + len1[i] - 1)
        if (nchar(double.letter.element[i]) == 2) {
          double.number[i] <- 1
        } else {
          double.number[i] <-
            as.numeric(substr(
              double.letter.element[i],
              3,
              nchar(double.letter.element[i])
            ))
        }
        double.letter.element[i] <-
          substr(double.letter.element[i], 1, 2)
        remove.idx <-
          c(remove.idx, idx1[i]:(idx1[i] + len1[i] - 1))
      }
      
      double.formula <- data.frame(double.letter.element,
                                   double.number,
                                   stringsAsFactors = FALSE)
      formula1 <- strsplit(formula, split = "")[[1]]
      formula1 <- formula1[-remove.idx]
      formula1 <- paste(formula1, collapse = "")
    }
    
    ## no one element
    if (formula1 == "") {
      one.formula <- matrix(NA, ncol = 2)
    } else {
      idx2 <- gregexpr("[A-Z][0-9]*", formula1)[[1]]
      len2 <- attributes(idx2)$match.length
      one.letter.element <- NULL
      one.number <- NULL
      for (i in seq_along(idx2)) {
        one.letter.element[i] <-
          substr(formula1, idx2[i], idx2[i] + len2[i] - 1)
        if (nchar(one.letter.element[i]) == 1) {
          one.number[i] <- 1
        } else {
          one.number[i] <-
            as.numeric(substr(one.letter.element[i],
                              2,
                              nchar(one.letter.element[i])))
        }
        one.letter.element[i] <-
          substr(one.letter.element[i], 1, 1)
      }
      one.formula <-
        data.frame(one.letter.element, one.number,
                   stringsAsFactors = FALSE)
    }
    
    colnames(double.formula) <-
      colnames(one.formula) <- c("element.name", "number")
    formula <- rbind(double.formula, one.formula)
    formula <-
      formula[!apply(formula, 1, function(x) {
        any(is.na(x))
      }),]
    
    formula <- formula[order(formula$element.name),]
    formula$number <- formula$number * number
    unique.element <- unique(formula$element.name)
    if (length(unique.element) == nrow(formula)) {
      return(formula)
    } else {
      formula <- lapply(unique.element, function(x) {
        formula[formula$element.name == x, , drop = FALSE]
      })
      
      formula <- lapply(formula, function(x) {
        data.frame(unique(x$element.name),
                   sum(x$number),
                   stringsAsFactors = FALSE)
      })
      
      formula <- do.call(rbind, formula)
      colnames(formula) <- c("element.name", "number")
      return(formula)
    }
  }



#' @title convert_precursor_mz2accurate_mass
#' @description convert_precursor_mz2accurate_mass
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param precursor_mz Chemical formula.
#' @param adduct Adduct.
#' @export
#' @return formula
#' @examples
# convert_precursor_mz2accurate_mass(
# precursor_mz = 805.559,
# adduct = "2M-2H+Na")
convert_precursor_mz2accurate_mass <-
  function(precursor_mz = 805.559,
           adduct = "2M-2H+Na") {
    precursor_mz <- as.numeric(precursor_mz)
    if (is.na(precursor_mz)) {
      return(NA)
    }
    
    if (is.na(adduct)) {
      return(NA)
    }
    
    if (adduct == "M+" | adduct == "M-") {
      return(precursor_mz)
    }
    
    element <-
      stringr::str_split(adduct, "[\\-\\+]{1}")[[1]][-1]
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
          get_mass(x, "exact_mass"),
          error = function(e)
            NA
        )
      }) %>%
      unlist()
    
    df <-
      df %>%
      dplyr::mutate(pos_neg =
                      case_when(plus_minus == "+" ~ -1,
                                plus_minus == "-" ~ 1))
    
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
#' @title sum_formula
#' @description Get the total formula after add adduct.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param formula Chemical formula.
#' @param adduct Adduct.
#' @export
#' @return formula
#' @examples
#' sum_formula(formula = "C9H11NO2", adduct = "M+H")
#' sum_formula(formula = "C9H11NO2", adduct = "M+")
#' sum_formula(formula = "C9H11NO2", adduct = "M+CH3COOH")
#' sum_formula(formula = "C9H11", adduct = "M-H20")
sum_formula <-
  function(formula = "C9H11NO2",
           adduct = "M-H2O+H") {
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
    },
    x = adduct1,
    y = plusorminus)
    
    adduct1 <- do.call(rbind, adduct1)
    
    formula <- rbind(formula1, adduct1)
    rownames(formula) <- NULL
    
    unique.element <- unique(formula$element.name)
    if (length(unique.element) == nrow(formula)) {
      if (any(formula$number < 0)) {
        return(NA)
      } else {
        formula$number[formula$number == 1] <- "W"
        formula <-
          paste(paste(formula$element.name, formula$number, sep = ""),
                collapse = "")
        formula <- strsplit(formula, split = "")[[1]]
        formula[formula == "W"] <- ""
        formula <- paste(formula, collapse = "")
        return(formula)
      }
    } else {
      formula <- lapply(unique.element, function(x) {
        formula[formula$element.name == x, , drop = FALSE]
      })
      
      formula <- lapply(formula, function(x) {
        data.frame(unique(x$element.name),
                   sum(x$number),
                   stringsAsFactors = FALSE)
      })
      
      formula <- do.call(rbind, formula)
      formula <- formula[formula[, 2] != 0,]
      colnames(formula) <- c("element.name", "number")
      if (any(formula$number < 0)) {
        return(NA)
      } else {
        formula$number[formula$number == 1] <- "W"
        formula <-
          paste(paste(formula$element.name, formula$number, sep = ""),
                collapse = "")
        formula <- strsplit(formula, split = "")[[1]]
        formula[formula == "W"] <- ""
        formula <- paste(formula, collapse = "")
        return(formula)
      }
    }
  }




# Associative vector of element symbols and their exact masses
# based on their most abundant isotope
element_accurate_masses <- c(
  'H' = 1.007825,
  'He' = 4.002603,
  'Li' = 7.016004,
  'Be' = 9.012182,
  'B' = 11.009305,
  'C' = 12.000000,
  'N' = 14.003074,
  'O' = 15.994915,
  'F' = 18.998403,
  'Ne' = 19.992440,
  'Na' = 22.989769,
  'Mg' = 23.985042,
  'Al' = 26.981538,
  'Si' = 27.976927,
  'P' = 30.973762,
  'S' = 31.972071,
  'Cl' = 34.968853,
  'Ar' = 39.962383,
  'K' = 38.963707,
  'Ca' = 39.962591,
  'Sc' = 44.955912,
  'Ti' = 47.947947,
  'V' = 50.943962,
  'Cr' = 51.940510,
  'Mn' = 54.938045,
  'Fe' = 55.934942,
  'Co' = 58.933198,
  'Ni' = 57.935348,
  'Cu' = 62.929599,
  'Zn' = 63.929147,
  'Ga' = 68.925581,
  'Ge' = 73.921178,
  'As' = 74.921596,
  'Se' = 79.916521,
  'Br' = 78.918338,
  'Kr' = 83.911507,
  'Rb' = 84.911800,
  'Sr' = 87.905614,
  'Y' = 88.905848,
  'Zr' = 89.904703,
  'Nb' = 92.906377,
  'Mo' = 97.905407,
  'Tc' = 98.000000,
  # Technetium has no stable isotopes; value is a placeholder
  'Ru' = 101.904349,
  'Rh' = 102.905504,
  'Pd' = 105.903483,
  'Ag' = 106.905093,
  'Cd' = 113.903358,
  'In' = 114.903878,
  'Sn' = 119.902197,
  'Sb' = 120.903818,
  'Te' = 129.906223,
  'I' = 126.904468,
  'Xe' = 131.904154,
  'Cs' = 132.905451,
  'Ba' = 137.905247,
  'La' = 138.906348,
  'Ce' = 139.905443,
  'Pr' = 140.907648,
  'Nd' = 141.907719,
  'Pm' = 145.000000,
  # Promethium has no stable isotopes; value is a placeholder
  'Sm' = 151.919729,
  'Eu' = 152.921227,
  'Gd' = 157.924101,
  'Tb' = 158.925343,
  'Dy' = 163.929174,
  'Ho' = 164.930319,
  'Er' = 165.930290,
  'Tm' = 168.934211,
  'Yb' = 173.938859,
  'Lu' = 174.940770,
  'Hf' = 179.946549,
  'Ta' = 180.947994,
  'W' = 183.950930,
  'Re' = 186.955750,
  'Os' = 191.961479,
  'Ir' = 192.962921,
  'Pt' = 194.964774,
  'Au' = 196.966552,
  'Hg' = 201.970626,
  'Tl' = 204.974412,
  'Pb' = 207.976636,
  'Bi' = 208.980383,
  'Th' = 232.038050,
  'Pa' = 231.035880,
  'U' = 238.050783,
  'Np' = 237.000000,
  # Neptunium has no long-lived stable isotopes; value is a placeholder
  'Pu' = 244.000000,
  # Plutonium has no long-lived stable isotopes; value is a placeholder
  'Am' = 243.000000,
  # Americium has no long-lived stable isotopes; value is a placeholder
  'Cm' = 247.000000,
  # Curium has no long-lived stable isotopes; value is a placeholder
  'Bk' = 247.000000,
  # Berkelium has no long-lived stable isotopes; value is a placeholder
  'Cf' = 251.000000,
  # Californium has no long-lived stable isotopes; value is a placeholder
  'Es' = 252.000000,
  # Einsteinium has no long-lived stable isotopes; value is a placeholder
  'Fm' = 257.000000,
  # Fermium has no long-lived stable isotopes; value is a placeholder
  'Md' = 258.000000,
  # Mendelevium has no long-lived stable isotopes; value is a placeholder
  'No' = 259.000000,
  # Nobelium has no long-lived stable isotopes; value is a placeholder
  'Lr' = 262.000000,
  # Lawrencium has no long-lived stable isotopes; value is a placeholder
  'Rf' = 267.000000,
  # Rutherfordium has no long-lived stable isotopes; value is a placeholder
  'Db' = 270.000000,
  # Dubnium has no long-lived stable isotopes; value is a placeholder
  'Sg' = 271.000000,
  # Seaborgium has no long-lived stable isotopes; value is a placeholder
  'Bh' = 270.000000,
  # Bohrium has no long-lived stable isotopes; value is a placeholder
  'Hs' = 277.000000,
  # Hassium has no long-lived stable isotopes; value is a placeholder
  'Mt' = 276.000000,
  # Meitnerium has no long-lived stable isotopes; value is a placeholder
  'Ds' = 281.000000,
  # Darmstadtium has no long-lived stable isotopes; value is a placeholder
  'Rg' = 280.000000,
  # Roentgenium has no long-lived stable isotopes; value is a placeholder
  'Cn' = 285.000000,
  # Copernicium has no long-lived stable isotopes; value is a placeholder
  'Nh' = 284.000000,
  # Nihonium has no long-lived stable isotopes; value is a placeholder
  'Fl' = 289.000000,
  # Flerovium has no long-lived stable isotopes; value is a placeholder
  'Mc' = 288.000000,
  # Moscovium has no long-lived stable isotopes; value is a placeholder
  'Lv' = 293.000000,
  # Livermorium has no long-lived stable isotopes; value is a placeholder
  'Ts' = 294.000000,
  # Tennessine has no long-lived stable isotopes; value is a placeholder
  'Og' = 294.000000   # Oganesson has no long-lived stable isotopes; value is a placeholder
)


# Associative vector of element symbols and their average atomic masses
# Values from IUPAC as of my last training data in 2022
element_average_masses <- c(
  'H' = 1.008,
  'He' = 4.0026,
  'Li' = 6.94,
  'Be' = 9.0122,
  'B' = 10.81,
  'C' = 12.011,
  'N' = 14.007,
  'O' = 15.999,
  'F' = 19.000,
  'Ne' = 20.180,
  'Na' = 22.990,
  'Mg' = 24.305,
  'Al' = 26.982,
  'Si' = 28.086,
  'P' = 30.974,
  'S' = 32.06,
  'Cl' = 35.45,
  'Ar' = 39.948,
  'K' = 39.098,
  'Ca' = 40.078,
  'Sc' = 44.956,
  'Ti' = 47.867,
  'V' = 50.942,
  'Cr' = 52.00,
  'Mn' = 54.938,
  'Fe' = 55.845,
  'Co' = 58.933,
  'Ni' = 58.693,
  'Cu' = 63.546,
  'Zn' = 65.38,
  'Ga' = 69.723,
  'Ge' = 72.630,
  'As' = 74.922,
  'Se' = 78.971,
  'Br' = 79.904,
  'Kr' = 83.798,
  'Rb' = 85.468,
  'Sr' = 87.62,
  'Y' = 88.906,
  'Zr' = 91.224,
  'Nb' = 92.906,
  'Mo' = 95.95,
  'Tc' = 98,
  # Technetium has no stable isotopes; value is an average
  'Ru' = 101.07,
  'Rh' = 102.91,
  'Pd' = 106.42,
  'Ag' = 107.87,
  'Cd' = 112.41,
  'In' = 114.82,
  'Sn' = 118.71,
  'Sb' = 121.76,
  'Te' = 127.60,
  'I' = 126.90,
  'Xe' = 131.29,
  'Cs' = 132.91,
  'Ba' = 137.33,
  'La' = 138.91,
  'Ce' = 140.12,
  'Pr' = 140.91,
  'Nd' = 144.24,
  'Pm' = 145,
  # Promethium has no stable isotopes; value is an average
  'Sm' = 150.36,
  'Eu' = 151.98,
  'Gd' = 157.25,
  'Tb' = 158.93,
  'Dy' = 162.50,
  'Ho' = 164.93,
  'Er' = 167.26,
  'Tm' = 168.93,
  'Yb' = 173.05,
  'Lu' = 174.97,
  'Hf' = 178.49,
  'Ta' = 180.95,
  'W' = 183.84,
  'Re' = 186.21,
  'Os' = 190.23,
  'Ir' = 192.22,
  'Pt' = 195.08,
  'Au' = 196.97,
  'Hg' = 200.59,
  'Tl' = 204.38,
  'Pb' = 207.2,
  'Bi' = 208.98,
  'Th' = 232.04,
  'Pa' = 231.04,
  'U' = 238.03,
  # The following are transuranic elements, and their average masses can vary depending on the isotope and source of information.
  'Np' = 237,
  'Pu' = 244,
  'Am' = 243,
  'Cm' = 247,
  'Bk' = 247,
  'Cf' = 251,
  'Es' = 252,
  'Fm' = 257,
  'Md' = 258,
  'No' = 259,
  'Lr' = 262,
  'Rf' = 267,
  'Db' = 270,
  'Sg' = 271,
  'Bh' = 270,
  'Hs' = 277,
  'Mt' = 276,
  'Ds' = 281,
  'Rg' = 280,
  'Cn' = 285,
  'Nh' = 284,
  'Fl' = 289,
  'Mc' = 288,
  'Lv' = 293,
  'Ts' = 294,
  'Og' = 294
)

#' Check Validity of a Chemical Formula
#'
#' Determines the validity of a chemical formula based on the known chemical elements up to 2022.
#' The function checks whether all the symbols in the formula belong to the list of known chemical elements.
#' It doesn't consider charges or other non-element symbols in the formula.
#'
#' @param formula Character string representing the chemical formula to be checked.
#'
#' @return Logical. Returns `TRUE` if the formula is valid and `FALSE` otherwise.
#'
#' @examples
#' is_valid_chemical_formula("H2O")      # TRUE
#' is_valid_chemical_formula("C6H12O6")  # TRUE
#' is_valid_chemical_formula("H2ZO4")    # FALSE
#'
#' @author Xiaotao Shen \email{shenxt1990@outlook.com}
#' @title Check Validity of a Chemical Formula
#' @description A function to validate chemical formulas based on the list of recognized chemical elements as of 2022.
#' @export

is_valid_chemical_formula <- function(formula) {
  # List of all chemical element symbols up to my last update in 2022.
  elements <-
    c(
      "H",
      "He",
      "Li",
      "Be",
      "B",
      "C",
      "N",
      "O",
      "F",
      "Ne",
      "Na",
      "Mg",
      "Al",
      "Si",
      "P",
      "S",
      "Cl",
      "Ar",
      "K",
      "Ca",
      "Sc",
      "Ti",
      "V",
      "Cr",
      "Mn",
      "Fe",
      "Co",
      "Ni",
      "Cu",
      "Zn",
      "Ga",
      "Ge",
      "As",
      "Se",
      "Br",
      "Kr",
      "Rb",
      "Sr",
      "Y",
      "Zr",
      "Nb",
      "Mo",
      "Tc",
      "Ru",
      "Rh",
      "Pd",
      "Ag",
      "Cd",
      "In",
      "Sn",
      "Sb",
      "Te",
      "I",
      "Xe",
      "Cs",
      "Ba",
      "La",
      "Ce",
      "Pr",
      "Nd",
      "Pm",
      "Sm",
      "Eu",
      "Gd",
      "Tb",
      "Dy",
      "Ho",
      "Er",
      "Tm",
      "Yb",
      "Lu",
      "Hf",
      "Ta",
      "W",
      "Re",
      "Os",
      "Ir",
      "Pt",
      "Au",
      "Hg",
      "Tl",
      "Pb",
      "Bi",
      "Th",
      "Pa",
      "U",
      "Np",
      "Pu",
      "Am",
      "Cm",
      "Bk",
      "Cf",
      "Es",
      "Fm",
      "Md",
      "No",
      "Lr",
      "Rf",
      "Db",
      "Sg",
      "Bh",
      "Hs",
      "Mt",
      "Ds",
      "Rg",
      "Cn",
      "Nh",
      "Fl",
      "Mc",
      "Lv",
      "Ts",
      "Og"
    )
  
  # Use a regex to match chemical element symbols.
  # Note: This assumes that the formula doesn't have any charge specification (+, -) or other non-element symbols.
  matches <- gregexpr("[A-Z][a-z]*", formula)
  symbols <- unlist(regmatches(formula, matches))
  
  # Check if all symbols are valid elements.
  for (symbol in symbols) {
    if (!(symbol %in% elements)) {
      return(FALSE)
    }
  }
  return(TRUE)
}



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
#' get_mass("H2O")                      # Using default exact_mass
#' get_mass("H2O", which = "average_mass")
#' get_mass("C6H12O6", which = "exact_mass")
#'
#' @author Xiaotao Shen \email{shenxt1990@outlook.com}
#' @title Calculate Mass of a Chemical Formula
#' @description Computes the mass of a given chemical formula based either on the exact mass or the average mass of its constituent elements.
#' @export
get_mass <-
  function(formula,
           which = c("exact_mass",
                     "average_mass")) {
    which <-
      match.arg(which)
    elements <-
      parse_chemical_formula(formula = formula)
    
    # Calculate accurate mass
    if (which == "exact_mass") {
      mass <-
        seq_len(nrow(elements)) %>%
        sapply(function(idx) {
          element_accurate_masses[elements$Element[idx]] * elements$Count[idx]
        }) %>%
        sum()
    } else{
      mass <-
        seq_len(nrow(elements)) %>%
        sapply(function(idx) {
          element_average_masses[elements$Element[idx]] * elements$Count[idx]
        }) %>%
        sum()
    }
    names(mass) <-
      formula
    return(mass)
  }


