# setwd("./data")
# elem_table = read.csv("elem_table.csv")
# save(elem_table, file="elem_table.rda")
# sinew::makeOxygen(elem_table)

#' @title abundant_isotopes
#' @description This table include the abundant isotope used to calculate exact mass for given formula
#' @format A data frame with 97 rows and 5 variables:
#' \describe{
#'   \item{\code{element}}{character ##}
#'   \item{\code{isotope}}{character ##}
#'   \item{\code{mass}}{double ##}
#'   \item{\code{abundance}}{double ##}
#'   \item{\code{ratioC}}{integer ##}
#'}
#' @details DETAILS
"abundant_isotopes"

#' @title full_isotopes
#' @description This table include the abundant isotope used to calculate exact mass for given formula
#' @format A data frame with 308 rows and 5 variables:
#' \describe{
#'   \item{\code{element}}{character COLUMN_DESCRIPTION}
#'   \item{\code{isotope}}{character COLUMN_DESCRIPTION}
#'   \item{\code{mass}}{double COLUMN_DESCRIPTION}
#'   \item{\code{abundance}}{double COLUMN_DESCRIPTION}
#'   \item{\code{ratioC}}{integer COLUMN_DESCRIPTION}
#'}
#' @details DETAILS
"full_isotopes"

#' @title elem_table
#' @description elem_table
#' @format A data frame with 31 rows and 5 variables:
#' \describe{
#'   \item{\code{element}}{character COLUMN_DESCRIPTION}
#'   \item{\code{isotope}}{character COLUMN_DESCRIPTION}
#'   \item{\code{mass}}{double COLUMN_DESCRIPTION}
#'   \item{\code{abundance}}{double COLUMN_DESCRIPTION}
#'   \item{\code{unsaturation}}{double COLUMN_DESCRIPTION}
#'}
#' @details DETAILS
"elem_table"
