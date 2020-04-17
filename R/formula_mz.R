


#' formula_mz
#'
#' @param formula e.g. "C2H4O1"
#' @param charge 0: neutral, 1: positive charge, -1: negative charge
#' @param elem_table a table can be imported
#'
#' @return the exact mass of the input formula
#' @export
#'
#' @examples formula_mz(formula = "C2H4O1", charge = 0)
#'
#'
formula_mz = function(formula = "C2H4O1", charge = 0,elem_table = lc8::elem_table){

  mz = numeric()
  for(i in 1:length(formula)){

    if(is.na(formula) | is.null(formula)){
      mz[i] = NA
      next
    }
    if(formula == ""){
      mz[i] = 0
      next
    }


  temp_formula = formula[i]
  temp_formula <- gsub("D", "[2]H", temp_formula)
  ende2 <- nchar(temp_formula)
  element2 <- c()
  number2 <- c()
  j <- c(1)
  while (j <= ende2) {
    if (substr(temp_formula, j, j) == c("[")) {
      b <- j
      while (any(substr(temp_formula, j, j) == c("]")) !=
             TRUE) {
        j <- c(j + 1)
      }
      k <- j
      while (any(substr(temp_formula, j, j) == c("-", ".","0", "1",
                                            "2", "3", "4", "5", "6", "7", "8", "9")) !=
             TRUE) {
        j <- c(j + 1)
      }
      m <- c(j - 1)
      element2 <- c(element2, substr(temp_formula, b, m))
    }
    if (any(substr(temp_formula, j, j) == c("-", ".","0", "1", "2", "3",
                                       "4", "5", "6", "7", "8", "9")) != TRUE) {
      k <- j
      while (any(substr(temp_formula, j, j) == c("-", ".","0", "1",
                                            "2", "3", "4", "5", "6", "7", "8", "9")) !=
             TRUE) {
        j <- c(j + 1)
      }
      m <- c(j - 1)
      j <- c(j - 1)
      element2 <- c(element2, substr(temp_formula, k, m))
    }
    if (any(substr(temp_formula, j, j) == c("-", ".","0", "1", "2", "3",
                                       "4", "5", "6", "7", "8", "9")) == TRUE) {
      k <- j
      while (any(substr(temp_formula, j, j) == c("-", ".","0", "1",
                                            "2", "3", "4", "5", "6", "7", "8", "9")) ==
             TRUE) {
        j <- c(j + 1)
      }
      m <- c(j - 1)
      j <- c(j - 1)
      number2 <- c(number2, as.numeric(substr(temp_formula,
                                              k, m)))
    }
    j <- j + 1

  }

  mz[i]=0

  for(j in 1:length(element2)){
    mz[i] = mz[i] + elem_table$mass[element2[j] == elem_table$element] * number2[j]
  }


  e_mass = 0.00054857990943
  mz[i] = mz[i] - e_mass*charge

  }
  return(mz)


}

