## ---------------------------------------------------------------------------
#' @title Split Chemical Formula
#' @description Takes a chemical formula string and breaks it down into its
#' individual elemental components and their respective counts.
#' @param formula A character string representing a chemical formula (e.g., "C9H11NO2").
#' Default is "C9H11NO2".
#' @return A data frame with two columns: `element` contains the elemental symbols
#' and `number` contains the respective counts for each element in the formula.
#' @export
#' @examples
#' split_formula(formula = "C9H11NO2")
#' split_formula(formula = "CH3")
#' split_formula(formula = "CH3C")
split_formula <-
  function(formula = "C9H11NO2") {
    if (!check_chemical_formula(formula)) {
      stop("Invalid chemical formula")
    }
    temp_formula <- strsplit(formula, split = "")[[1]]
    number <- NULL
    for (i in seq_along(temp_formula)) {
      if (length(grep("[0-9]{1}", temp_formula[i])) == 0) {
        break
      }
      number[i] <- temp_formula[i]
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
      
      double.formula <- data.frame(double.letter.element, double.number, stringsAsFactors = FALSE)
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
            as.numeric(substr(one.letter.element[i], 2, nchar(one.letter.element[i])))
        }
        one.letter.element[i] <-
          substr(one.letter.element[i], 1, 1)
      }
      one.formula <-
        data.frame(one.letter.element, one.number, stringsAsFactors = FALSE)
    }
    
    colnames(double.formula) <-
      colnames(one.formula) <- c("element", "number")
    formula <- rbind(double.formula, one.formula)
    formula <-
      formula[!apply(formula, 1, function(x) {
        any(is.na(x))
      }), ]
    
    formula <- formula[order(formula$element), ]
    formula$number <- formula$number * number
    unique.element <- unique(formula$element)
    if (length(unique.element) == nrow(formula)) {
      return(formula)
    } else {
      formula <- lapply(unique.element, function(x) {
        formula[formula$element == x, , drop = FALSE]
      })
      
      formula <- lapply(formula, function(x) {
        data.frame(unique(x$element), sum(x$number), stringsAsFactors = FALSE)
      })
      
      formula <- do.call(rbind, formula)
      colnames(formula) <- c("element", "number")
      return(formula)
    }
  }