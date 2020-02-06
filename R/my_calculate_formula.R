


#' my_calculate_formula
#'
#' @param Formula1 formula1 (could be a vector)
#' @param Formula2 formula2 (could be a vector)
#' @param sign 1 for merge two formula, -1 for subtract second from first formula
#' @param valid_mat if set True, only return formula if the element count is > 0
#'
#' @return the merge or subtract formula in vector or matrix; and if valid_mati is true, a validation of whether negative element presents
#' @export
#'
#' @examples my_calculate_formula(Formula1 = "C2H4O1S2P1",Formula2 = "N1H1O-1",sign = 1,Valid_formula = FALSE)
my_calculate_formula = function(Formula1,Formula2,sign = 1, Valid_formula = FALSE){
  formula2_ls = list()
  for(i in 1:length(Formula2)){
    formula2 = Formula2[i]
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
          while (any(substr(formula2, j, j) == c("-", ".",  "0", "1",
                                                 "2", "3", "4", "5", "6", "7", "8", "9")) !=
                 TRUE) {
            j <- c(j + 1)
          }
          m <- c(j - 1)
          element2 <- c(element2, substr(formula2, b, m))
        }
        if (any(substr(formula2, j, j) == c("-", ".",  "0", "1", "2", "3",
                                            "4", "5", "6", "7", "8", "9")) != TRUE) {
          k <- j
          while (any(substr(formula2, j, j) == c("-", ".",  "0", "1",
                                                 "2", "3", "4", "5", "6", "7", "8", "9")) !=
                 TRUE) {
            j <- c(j + 1)
          }
          m <- c(j - 1)
          j <- c(j - 1)
          element2 <- c(element2, substr(formula2, k, m))
        }
        if (any(substr(formula2, j, j) == c("-", ".",  "0", "1", "2", "3",
                                            "4", "5", "6", "7", "8", "9")) == TRUE) {
          k <- j
          while (any(substr(formula2, j, j) == c("-", ".",  "0", "1",
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
    number2 = number2 * sign
    formula2_ls[[length(formula2_ls)+1]] = list(element2, number2)
  }

  formula1_ls = list()
  for(i in 1:length(Formula1)){
    formula1 = Formula1[i]
    {
      formula1 <- gsub("D", "[2]H", formula1)
      ende2 <- nchar(formula1)
      element1 <- c()
      number1 <- c()
      j <- c(1)
      while (j <= ende2) {
        if (substr(formula1, j, j) == c("[")) {
          b <- j
          while (any(substr(formula1, j, j) == c("]")) !=
                 TRUE) {
            j <- c(j + 1)
          }
          k <- j
          while (any(substr(formula1, j, j) == c("-", ".",  "0", "1",
                                                 "2", "3", "4", "5", "6", "7", "8", "9")) !=
                 TRUE) {
            j <- c(j + 1)
          }
          m <- c(j - 1)
          element1 <- c(element1, substr(formula1, b, m))
        }
        if (any(substr(formula1, j, j) == c("-", ".",  "0", "1", "2", "3",
                                            "4", "5", "6", "7", "8", "9")) != TRUE) {
          k <- j
          while (any(substr(formula1, j, j) == c("-", ".",  "0", "1",
                                                 "2", "3", "4", "5", "6", "7", "8", "9")) !=
                 TRUE) {
            j <- c(j + 1)
          }
          m <- c(j - 1)
          j <- c(j - 1)
          element1 <- c(element1, substr(formula1, k, m))
        }
        if (any(substr(formula1, j, j) == c("-", ".",  "0", "1", "2", "3",
                                            "4", "5", "6", "7", "8", "9")) == TRUE) {
          k <- j
          while (any(substr(formula1, j, j) == c("-", ".",  "0", "1",
                                                 "2", "3", "4", "5", "6", "7", "8", "9")) ==
                 TRUE) {
            j <- c(j + 1)
          }
          m <- c(j - 1)
          j <- c(j - 1)
          number1 <- c(number1, as.numeric(substr(formula1,
                                                      k, m)))
        }
        j <- j + 1
      }
    }
    formula1_ls[[length(formula1_ls)+1]] = list(element1, number1)
  }

  formula_mat = matrix("", nrow = length(formula1_ls), ncol = length(formula2_ls))
  valid_mat = matrix(F, nrow = length(formula1_ls), ncol = length(formula2_ls))
  for(row_i in 1:length(Formula1)){
    element1 = formula1_ls[[row_i]][[1]]
    number1 = formula1_ls[[row_i]][[2]]
    for(col_j in 1:length(Formula2)){
      element2 = formula2_ls[[col_j]][[1]]
      number2 = formula2_ls[[col_j]][[2]]
      count_all = number1
      elem_all = element1
      for(i in 1:length(element2)){
        if(any(element2[i]==elem_all) == TRUE){
          count_all[elem_all==element2[i]] = count_all[elem_all==element2[i]]+number2[i]
        }else{
          count_all = c(count_all,number2[i])
          elem_all = c(elem_all,element2[i])
        }
      }

      if(any(count_all<0)){
        valid_mat[row_i,col_j]=F
      }

      elem_order = order(elem_all)
      count_all = count_all[elem_order]
      elem_all = elem_all[elem_order]

      elem_all=elem_all[count_all!=0]
      count_all=count_all[count_all!=0]
      formula_all=c()
      for (i in 1:length(count_all)) {
        formula_all <- c(formula_all, elem_all[i],
                              count_all[i])
      }
      formula_mat[row_i,col_j]=paste0(formula_all, collapse = "")
    }
  }

  if(length(Formula1)==1 & length(Formula2)==1){
    formula_mat = as.vector(formula_mat)
    valid_mat = as.vector(valid_mat)
  }

  if(Valid_formula){
    return(list(formula_mat, valid_mat))
  } else {
    return(formula_mat)
  }
}

# Formula1 = c("C1H2","C3H4O2")
# Formula2 = c("C1H2","N1H3","B1O2")
# my_calculate_formula(Formula1, Formula2)
# for(i in 1:1000000){
#   identical(formula1_ls[[1]], formula2_ls[[3]])
# }



