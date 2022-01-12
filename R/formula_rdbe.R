

#' formula_rdbe
#'
#' @param formula e.g. "C2H4O1"
#' @param elem_table a table records unsaturation
#'
#' @return the ring and double bond number
#' @export
#'
#' @examples formula_rdbe(formula = "C2H4O1", elem_table = lc8::elem_table)
formula_rdbe = function(formula = "C2H4O1", elem_table = lc8::elem_table){

  rdbe = numeric()
  for(i in 1:length(formula)){

    temp_formula = formula[i]
    # temp_formula <- gsub("D", "[2]H", temp_formula)
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
        while (any(substr(temp_formula, j, j) == c("-", ".", "0", "1",
                                                   "2", "3", "4", "5", "6", "7", "8", "9")) !=
               TRUE) {
          j <- c(j + 1)
        }
        m <- c(j - 1)
        element2 <- c(element2, substr(temp_formula, b, m))
      }
      if (any(substr(temp_formula, j, j) == c("-", ".", "0", "1", "2", "3",
                                              "4", "5", "6", "7", "8", "9")) != TRUE) {
        k <- j
        while (any(substr(temp_formula, j, j) == c("-", ".", "0", "1",
                                                   "2", "3", "4", "5", "6", "7", "8", "9")) !=
               TRUE) {
          j <- c(j + 1)
        }
        m <- c(j - 1)
        j <- c(j - 1)
        element2 <- c(element2, substr(temp_formula, k, m))
      }
      if (any(substr(temp_formula, j, j) == c("-", ".", "0", "1", "2", "3",
                                              "4", "5", "6", "7", "8", "9")) == TRUE) {
        k <- j
        while (any(substr(temp_formula, j, j) == c("-", ".", "0", "1",
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

    rdbe[i]=1
  for (j in 1:length(element2)) {
    rdbe[i] = rdbe[i] + elem_table$unsaturation[element2[j] ==
                                                  elem_table$element] * number2[j]
  }
  }
  return(rdbe)
}


