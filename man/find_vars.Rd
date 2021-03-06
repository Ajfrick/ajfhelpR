% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_vars.R
\name{find_vars}
\alias{find_vars}
\title{Find columns containing string or regular expression}
\usage{
find_vars(dat, pattern, ignore.case = TRUE)
}
\arguments{
\item{dat}{data frame, tibble, or named vector}

\item{pattern}{character vector or regular expression to search for}

\item{ignore.case}{logical for case sensitivity}
}
\value{
named vector with column or index locations with variable names
}
\description{
Find columns containing string or regular expression
}
\examples{
find_vars(mtcars, "cyl")
find_vars(1:100, "cyl")
mtcars[,find_vars(mtcars,"cyl")]
mtcars[,find_vars(mtcars,"c")]
mtcars[,find_vars(mtcars,"C",ignore.case = F)]
}
