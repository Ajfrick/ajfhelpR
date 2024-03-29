% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cat_freqs.R
\name{cat_freqs}
\alias{cat_freqs}
\title{Create column for frequency distribution of data}
\usage{
cat_freqs(
  x,
  omit_na = T,
  NAname = NA,
  digits = 1,
  incl_denom = F,
  remove_na = F,
  out = c("percentage", "percent"),
  perc_disp = F,
  escape = F,
  zero2dash = T
)
}
\arguments{
\item{x}{vector of characters or factors}

\item{omit_na}{logical for ommision of NAs in computation of counts and
proportions of non missing data}

\item{digits}{number of digits to round}

\item{incl_denom}{logical for inclusion of denominator when displaying frequencies}

\item{remove_na}{logical for removal of frequency of missing data}

\item{out}{single character representing output for proportion}

\item{perc_disp}{logical for inclustion of \% sign}

\item{escape}{logical for inclusion of escape character for LaTeX tables}

\item{zero2dash}{logical for returning "-" instead of "0 (0)" for tables}
}
\value{
dataframe/tibble with two columns, one for category name, and one with
N (\\%) output from summ_prop function
}
\description{
Create column for frequency distribution of data
}
\examples{
dat = iris
specs = dat$Species
cat_freqs(specs)
cat_freqs(iris$Species)

#Playing around with Missing data

set.seed(314159)
index_na = sample(1:nrow(dat), size = 25, replace = F)
dat[index_na,]$Species = NA

# Different output based on arguments to `omit_na`
cat_freqs(dat$Species) #omit_na = F default
cat_freqs(dat$Species, omit_na = T)
cat_freqs(dat$Species, incl_denom = T)
}
