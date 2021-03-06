% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/frailp_poolEst.R
\name{frailp_poolEst}
\alias{frailp_poolEst}
\title{Format multiple imputed frailtypack `frailtyPenal` models into Hazard Ratio
table format with pooled estimates across imputed models}
\usage{
frailp_poolEst(modList, digits = 2)
}
\arguments{
\item{modList}{list of frailtypack objects}

\item{digits}{number of digits top format to}
}
\value{
tibble with Hazard Ratio, 95-CI (pvalue WIP)
}
\description{
Function accepts a list of frailtypack models and generates a
tibble with HR and CI
}
\examples{
TBD
}
