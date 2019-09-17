


#' my_calculate_formula
#'
#' @param formula1 formula1
#' @param formula2 formula1
#' @param sign 1 for merge two formula, -1 for subtract second from first formula
#' @param Is_valid if set True, only return formula if the element count is > 0
#'
#' @return the merge or subtract formula
#' @export
#'
#' @examples my_calculate_formula(formula1 = "C2H4O1S2P1",formula2 = "N1H1O-1",sign = 1,Is_valid = TRUE)
my_calculate_formula = function(formula1,formula2,sign = 1,Is_valid = TRUE){

  {
    formula2 <- gsub("D", "[2]H", formula2)
    ende2 <- nchar(formula2)
    element2 <- c()
    number2 <- c()
    j <- c(1)
    while (j <= ende2) {
      if (substr(formula2, j, j) == c("[")) {
        b <- j
        while (any(substr(formula2, j, j) == c("]")) !=
               TRUE) {
          j <- c(j + 1)
        }
        k <- j
        while (any(substr(formula2, j, j) == c("-", "0", "1",
                                               "2", "3", "4", "5", "6", "7", "8", "9")) !=
               TRUE) {
          j <- c(j + 1)
        }
        m <- c(j - 1)
        element2 <- c(element2, substr(formula2, b, m))
      }
      if (any(substr(formula2, j, j) == c("-", "0", "1", "2", "3",
                                          "4", "5", "6", "7", "8", "9")) != TRUE) {
        k <- j
        while (any(substr(formula2, j, j) == c("-", "0", "1",
                                               "2", "3", "4", "5", "6", "7", "8", "9")) !=
               TRUE) {
          j <- c(j + 1)
        }
        m <- c(j - 1)
        j <- c(j - 1)
        element2 <- c(element2, substr(formula2, k, m))
      }
      if (any(substr(formula2, j, j) == c("-", "0", "1", "2", "3",
                                          "4", "5", "6", "7", "8", "9")) == TRUE) {
        k <- j
        while (any(substr(formula2, j, j) == c("-", "0", "1",
                                               "2", "3", "4", "5", "6", "7", "8", "9")) ==
               TRUE) {
          j <- c(j + 1)
        }
        m <- c(j - 1)
        j <- c(j - 1)
        number2 <- c(number2, as.numeric(substr(formula2,
                                                k, m)))
      }
      j <- j + 1

    }

  }

  {
    formula1 <- gsub("D", "[2]H", formula1)
    ende2 <- nchar(formula1)
    elem_all <- c()
    count_all <- c()
    j <- c(1)
    while (j <= ende2) {
      if (substr(formula1, j, j) == c("[")) {
        b <- j
        while (any(substr(formula1, j, j) == c("]")) !=
               TRUE) {
          j <- c(j + 1)
        }
        k <- j
        while (any(substr(formula1, j, j) == c("-", "0", "1",
                                               "2", "3", "4", "5", "6", "7", "8", "9")) !=
               TRUE) {
          j <- c(j + 1)
        }
        m <- c(j - 1)
        elem_all <- c(elem_all, substr(formula1, b, m))
      }
      if (any(substr(formula1, j, j) == c("-", "0", "1", "2", "3",
                                          "4", "5", "6", "7", "8", "9")) != TRUE) {
        k <- j
        while (any(substr(formula1, j, j) == c("-", "0", "1",
                                               "2", "3", "4", "5", "6", "7", "8", "9")) !=
               TRUE) {
          j <- c(j + 1)
        }
        m <- c(j - 1)
        j <- c(j - 1)
        elem_all <- c(elem_all, substr(formula1, k, m))
      }
      if (any(substr(formula1, j, j) == c("-", "0", "1", "2", "3",
                                          "4", "5", "6", "7", "8", "9")) == TRUE) {
        k <- j
        while (any(substr(formula1, j, j) == c("-", "0", "1",
                                               "2", "3", "4", "5", "6", "7", "8", "9")) ==
               TRUE) {
          j <- c(j + 1)
        }
        m <- c(j - 1)
        j <- c(j - 1)
        count_all <- c(count_all, as.numeric(substr(formula1,
                                                    k, m)))
      }
      j <- j + 1

    }

  }

  number2 = number2 * sign
  for(i in 1:length(element2)){
    if(any(element2[i]==elem_all) == TRUE){
      count_all[elem_all==element2[i]] = count_all[elem_all==element2[i]]+number2[i]
    }else{
      count_all = c(count_all,number2[i])
      elem_all = c(elem_all,element2[i])
    }
  }
  if(any(count_all<0)==TRUE & Is_valid){
    return(F)
  } else{
    elem_order = order(elem_all)
    count_all = count_all[elem_order]
    elem_all = elem_all[elem_order]

    elem_all=elem_all[count_all!=0]
    count_all=count_all[count_all!=0]
    formula_all=c()
    for (i in 1:length(count_all)) {
      formula_all <- paste(formula_all, elem_all[i],
                           count_all[i], sep = "")
    }
    return(formula_all)
  }
}

my_calculate_formula("C1H2N3O4", "C3H2B2")
