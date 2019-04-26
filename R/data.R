# library(readr)
# setwd("./data")
# full_isotopes = read_csv("full_isotopes.csv")
# save(full_isotopes, file="full_isotopes.rda")
# sinew::makeOxygen(full_isotopes)
#' @title full_isotopes
#' @description This table include the abundant isotope used to calculate exact mass for given formula
#' @format A data frame with 296 rows and 6 variables:
#' \describe{
#'   \item{\code{element}}{character COLUMN_DESCRIPTION}
#'   \item{\code{isotope}}{character COLUMN_DESCRIPTION}
#'   \item{\code{mass}}{double COLUMN_DESCRIPTION}
#'   \item{\code{abundance}}{double COLUMN_DESCRIPTION}
#'   \item{\code{ratioC}}{double COLUMN_DESCRIPTION}
#'   \item{\code{Mass_Dif}}{double COLUMN_DESCRIPTION}
#'}
#' @details DETAILS
"full_isotopes"

# elem_table = read_csv("elem_table.csv")
# save(elem_table, file="elem_table.rda")
# sinew::makeOxygen(elem_table)
#' @title elem_table
#' @description elem_table
#' @format A data frame with 288 rows and 7 variables:
#' \describe{
#'   \item{\code{element}}{character COLUMN_DESCRIPTION}
#'   \item{\code{isotope}}{character COLUMN_DESCRIPTION}
#'   \item{\code{mass}}{double COLUMN_DESCRIPTION}
#'   \item{\code{abundance}}{double COLUMN_DESCRIPTION}
#'   \item{\code{ratioC}}{double COLUMN_DESCRIPTION}
#'   \item{\code{unsaturation}}{double COLUMN_DESCRIPTION}
#'}
#' @details DETAILS
"elem_table"
