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

# elem_table = read_csv("./data/elem_table.csv")
# save(elem_table, file="./data/elem_table.rda")
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
#'   \item{\code{Mass_Dif}}{double COLUMN_DESCRIPTION}
#'   \item{\code{unsaturation}}{double COLUMN_DESCRIPTION}
#'}
#' @details DETAILS
"elem_table"


# periodic_table = read_csv("periodic_table.csv")
# save(periodic_table, file="periodic_table.rda")
# sinew::makeOxygen(periodic_table)
#' @title periodic_table
#' @description DATASET_DESCRIPTION
#' @format A data frame with 118 rows and 3 variables:
#' \describe{
#'   \item{\code{Element Name}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Symbol}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Atomic Number}}{double COLUMN_DESCRIPTION}
#'}
#' @details DETAILS
"periodic_table"
