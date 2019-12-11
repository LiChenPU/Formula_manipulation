

#' isotopic_abundance
#'
#' @param formula e.g. "C2H4O1"
#' @param elem_table a table records unsaturation
#' @param isotope e.g. [13]C, or [13]C2 for M+2
#'
#' @return the ratio of given formula and its isotopic peak
#' @export
#'
#' @examples isotopic_abundance(formula = "C2H4O1", elem_table = lc8::elem_table)
isotopic_abundance = function(formula = "C2H4O1", isotope = "[13]C1", elem_table = lc8::elem_table){
  formula <- gsub("D", "[2]H", formula)
  ende2 <- nchar(formula)
  element2 <- c()
  number2 <- c()
  j <- c(1)
  while (j <= ende2) {

    if (substr(formula, j, j) == c("[")) {
      b <- j
      while (any(substr(formula, j, j) == c("]")) !=
             TRUE) {
        j <- c(j + 1)
      }
      k <- j
      while (any(substr(formula, j, j) == c("-", "0", "1",
                                                 "2", "3", "4", "5", "6", "7", "8", "9")) !=
             TRUE) {
        j <- c(j + 1)
      }
      m <- c(j - 1)
      element2 <- c(element2, substr(formula, b, m))
    }
    if (any(substr(formula, j, j) == c("-", "0", "1", "2", "3",
                                            "4", "5", "6", "7", "8", "9")) != TRUE) {
      k <- j
      while (any(substr(formula, j, j) == c("-", "0", "1",
                                                 "2", "3", "4", "5", "6", "7", "8", "9")) !=
             TRUE) {
        j <- c(j + 1)
      }
      m <- c(j - 1)
      j <- c(j - 1)
      element2 <- c(element2, substr(formula, k, m))
    }
    if (any(substr(formula, j, j) == c("-", "0", "1", "2", "3",
                                            "4", "5", "6", "7", "8", "9")) == TRUE) {
      k <- j
      while (any(substr(formula, j, j) == c("-", "0", "1",
                                                 "2", "3", "4", "5", "6", "7", "8", "9")) ==
             TRUE) {
        j <- c(j + 1)
      }
      m <- c(j - 1)
      j <- c(j - 1)
      number2 <- c(number2, as.numeric(substr(formula,
                                              k, m)))
    }
    j <- j + 1

  }


  number1 = number2
  element1 = element2


  formula <- gsub("D", "[2]H", isotope)
  ende2 <- nchar(formula)
  element2 <- c()
  number2 <- c()
  j <- c(1)
  while (j <= ende2) {

    if (substr(formula, j, j) == c("[")) {
      b <- j
      while (any(substr(formula, j, j) == c("]")) !=
             TRUE) {
        j <- c(j + 1)
      }
      k <- j
      while (any(substr(formula, j, j) == c("-", "0", "1",
                                            "2", "3", "4", "5", "6", "7", "8", "9")) !=
             TRUE) {
        j <- c(j + 1)
        if(j>ende2){   break      }
      }
      m <- c(j - 1)
      element2 <- c(element2, substr(formula, b, m))
    }
    if(j>ende2){
      number2 = c(number2, 1)
      break
    }
    if (any(substr(formula, j, j) == c("-", "0", "1", "2", "3",
                                       "4", "5", "6", "7", "8", "9")) != TRUE) {
      k <- j
      while (any(substr(formula, j, j) == c("-", "0", "1",
                                            "2", "3", "4", "5", "6", "7", "8", "9")) !=
             TRUE) {
        j <- c(j + 1)
        if(j>ende2){   break      }
      }
      m <- c(j - 1)
      j <- c(j - 1)
      element2 <- c(element2, substr(formula, k, m))
    }
    if(j>ende2){
      number2 = c(number2, 1)
      break
    }
    if (any(substr(formula, j, j) == c("-", "0", "1", "2", "3",
                                       "4", "5", "6", "7", "8", "9")) == TRUE) {
      k <- j
      while (any(substr(formula, j, j) == c("-", "0", "1",
                                            "2", "3", "4", "5", "6", "7", "8", "9")) ==
             TRUE) {
        j <- c(j + 1)
      }
      m <- c(j - 1)
      j <- c(j - 1)
      number2 <- c(number2, as.numeric(substr(formula,
                                              k, m)))
    }
    j <- j + 1

  }

  selected_isotopes = grepl("\\[\\d+\\]",element2)
  number_isotopes = number2[selected_isotopes]
  element_isotopes = element2[selected_isotopes]

  ratio = 1
  for(i in 1:length(element_isotopes)){
    element_isotope = element_isotopes[i]
    element_parent = gsub("\\[\\d+\\]", "", element_isotope)
    num_isotope_formula2 = number_isotopes[i]


    num_parent_formula1 = 0
    num_isotpe_formula1 = 0
    if(any(element1 == element_parent)){
      num_parent_formula1 = number1[element1 == element_parent]
    }
    if(any(element1 == element_isotope)){
      num_isotpe_formula1 = number1[element1 == element_isotope]
    }

    natural_abundance = elem_table$abundance[elem_table$element == element_isotope]

    inten_formula1 = choose((num_parent_formula1+num_isotpe_formula1),num_isotpe_formula1)*natural_abundance^(num_isotpe_formula1)
    inten_formula2 = choose((num_parent_formula1+num_isotpe_formula1),(num_isotpe_formula1+num_isotope_formula2))*natural_abundance^(num_isotpe_formula1+num_isotope_formula2)

    ratio = ratio * (inten_formula2/inten_formula1)

  }

  return(ratio)
}

# test
# formula = "[13]C1"
# isotope = "[13]C-1"
# isotopic_abundance("[13]C1", "[13]C-1")
