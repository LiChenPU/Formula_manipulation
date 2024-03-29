% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formula_mz.R
\name{formula_mz}
\alias{formula_mz}
\title{formula_mz}
\usage{
formula_mz(formula = "C2H4O1", charge = 0, elem_table = lc8::elem_table)
}
\arguments{
\item{formula}{e.g. "C2H4O1"}

\item{charge}{0: neutral, 1: positive charge, -1: negative charge}

\item{elem_table}{a table can be imported}
}
\value{
the exact mass of the input formula
}
\description{
formula_mz
}
\examples{
formula_mz(formula = "C2H4O1", charge = 0)


}
