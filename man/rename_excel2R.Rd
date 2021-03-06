% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rename_excel2R.R
\name{rename_excel2R}
\alias{rename_excel2R}
\title{Pipeable column header rename function}
\usage{
rename_excel2R(dat)
}
\arguments{
\item{dat}{Dataset to rename}
}
\value{
dataframe or tibble containing the updated names
}
\description{
This function takes a data frame or tibble object and replaces
any 'bad' column names (containing whitespace, dashes, slashes, etc) and replaces
them with more R friendly column names. Most often when imported manual Excel
data into R for further analysis
}
\examples{
library(tibble)
library(stringr)
set.seed(1234)

dat = tibble(
"Column 1" = 1:10,
"Column#2" = sample(1:10, size = 10, replace = T),
"Col1/Col2" = `Column 1`/`Column#2`,
"| New~Column |" = "test"
)

newdat = rename_excel2R(dat)
newdat
newdat2 = dat \%>\% rename_excel2R
all.equal(newdat, newdat2)

pipe_test = dat \%>\%
  rename_excel2R \%>\%
  select_if(is.double)

}
