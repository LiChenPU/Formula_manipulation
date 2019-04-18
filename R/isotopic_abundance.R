

#' isotopic_abundance
#'
#' @param formula e.g. "C2H4O1"
#' @param elem_table a table records unsaturation
#' @param isotope ..
#'
#' @return the ratio of given formula and its isotopic peak
#' @export
#'
#' @examples formula_rdbe(formula = "C2H4O1", elem_table = lc8::elem_table)
isotopic_abundance = function(formula = "C2H4O1", isotope = "[13]C1C-1", elem_table = lc8::elem_table){

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


  if(sum(number2<0)!=1 | sum(number2>0)!=1 ){
    print("error, incorrect isotope input")
    return(0)
  }




  elem_parent = element2[number2<0]
  elem_iso = element2[number2>0]

  n1_parent = number1[element1 == elem_parent]
  n2_iso = number2[element2 == elem_iso]
  if(!any(element1 == elem_iso)){

    n1_iso=0
    iso_parent = 1
  }else{
    n1_iso = number1[element1 == elem_iso]

    iso_parent = choose((n1_iso+n1_parent),n1_iso)*elem_table$abundance[elem_table$element == elem_iso]^(n1_iso)
  }

  iso_target = choose(n1_parent+n1_iso, (n1_iso+n2_iso))*elem_table$abundance[elem_table$element == elem_iso]^(n1_iso+n2_iso)
  iso_ratio = iso_target/iso_parent

  return(iso_ratio)
}


