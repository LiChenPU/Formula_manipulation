

#' elem_num_query
#'
#' @param formula for example "C2H4O2"
#' @param elem_query it refers to element in query
#'
#' @return It returns the number of element in formula
#' @export
#'
#' @examples elem_num_query("C2H4O2","C")
elem_num_query = function(formula,elem_query){

  if(!is.character(formula)|is.na(formula)){return(NA)}
  formula <- gsub("D", "[2]H", formula)
  ende2 <- nchar(formula)
  element2 <- c()
  number2 <- c()
  j <- c(1)
  while (j <= ende2) {
    #browser()
    if (substr(formula, j, j) == c("[")) {
      b <- j
      while (any(substr(formula, j, j) == c("]")) !=
             TRUE) {
        j <- c(j + 1)
      }
      k <- j
      while (any(substr(formula, j, j) == c("0", "1",
                                             "2", "3", "4", "5", "6", "7", "8", "9")) !=
             TRUE) {
        j <- c(j + 1)
      }
      m <- c(j - 1)
      element2 <- c(element2, substr(formula, b, m))
    }
    if (any(substr(formula, j, j) == c("0", "1", "2", "3",
                                        "4", "5", "6", "7", "8", "9")) != TRUE) {
      k <- j
      while (any(substr(formula, j, j) == c("0", "1",
                                             "2", "3", "4", "5", "6", "7", "8", "9")) !=
             TRUE) {
        j <- c(j + 1)
      }
      m <- c(j - 1)
      j <- c(j - 1)
      element2 <- c(element2, substr(formula, k, m))
    }
    if (any(substr(formula, j, j) == c("0", "1", "2", "3",
                                        "4", "5", "6", "7", "8", "9")) == TRUE) {
      k <- j
      while (any(substr(formula, j, j) == c("0", "1",
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

  if(any(element2 == elem_query )){
    num_query = number2[elem_query == element2]

  }
  else{
    num_query=0

  }
  return(num_query)
}
