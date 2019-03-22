

#' my_break_formula
#'
#' @param formula any chemical formula
#'
#' @return a list containing elem and count
#' @export
#'
#' @examples my_break_formula("[13]C1C-1H4N2")
#'
#'
my_break_formula = function(formula = "[13]C1C-1H4N2"){
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

return(list(elem=element2, count=number2))

}




