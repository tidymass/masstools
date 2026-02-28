#' Sum a Molecular Formula with an Adduct
#'
#' Combine a molecular formula with an adduct expression and return the
#' resulting summed formula.
#'
#' @param formula A character string containing the base molecular formula.
#' @param adduct A character string containing the adduct expression, such as
#'   `"M-H2O+H"` or `"M+CH3COOH"`.
#'
#' @return A character string containing the summed formula, or `NA` if the
#'   result is invalid.
#' @author Xiaotao Shen <xiaotao.shen@outlook.com>
#' @export
#' @examples
#' combine_formula_adduct(formula = "C9H11NO2", adduct = "M+H")
#' combine_formula_adduct(formula = "C9H11NO2", adduct = "M+")
#' combine_formula_adduct(formula = "C9H11NO2", adduct = "M+CH3COOH")
#' combine_formula_adduct(formula = "C9H11", adduct = "M-H20")
combine_formula_adduct <-
  function(formula = "C9H11NO2", adduct = "M-H2O+H") {
    if (is.na(formula)) {
      return(NA)
    }

    if (is.na(adduct)) {
      return(formula)
    }

    if (adduct == "M+" || adduct == "M-") {
      return(formula)
    }

    formula1 <- parse_chemical_formula(formula)
    colnames(formula1) <- c("element", "number")
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
      temp <- parse_chemical_formula(x)
      colnames(temp) <- c("element", "number")
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
