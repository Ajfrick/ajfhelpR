% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/na_2dash.R
\name{na_2dash}
\alias{na_2dash}
\title{Replace NAs with '-'}
\usage{
na_2dash(x)
}
\arguments{
\item{x}{Vector}
}
\value{
`x` coerced to character vector with all NAs replaced with `-`
}
\description{
Replace NAs with '-'
}
\examples{
x = 1:10
x[c(2,4,7)] = NA
na_2dash(x)

#install.packages(tidyverse)
library(tidyverse)

set.seed(271828)
dat = iris[1,30]
na_ind = sample(1:nrow(dat), size = 10)
dat$Species[na_ind] = NA

dat \%>\% mutate_at(vars(Species), na_2dash) \%>\% View
}
