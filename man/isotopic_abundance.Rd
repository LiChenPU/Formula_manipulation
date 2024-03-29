% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/isotopic_abundance.R
\name{isotopic_abundance}
\alias{isotopic_abundance}
\title{isotopic_abundance}
\usage{
isotopic_abundance(
  formula = "C2H4O1",
  isotope = "[13]C1",
  elem_table = lc8::elem_table
)
}
\arguments{
\item{formula}{e.g. "C2H4O1"}

\item{isotope}{e.g. [13]C, or [13]C2 for M+2}

\item{elem_table}{a table records unsaturation}
}
\value{
the ratio of given formula and its isotopic peak
}
\description{
isotopic_abundance
}
\examples{
isotopic_abundance(formula = "C2H4O1", elem_table = lc8::elem_table)
}
