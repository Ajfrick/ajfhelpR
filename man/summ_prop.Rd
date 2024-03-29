% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summ_prop.R
\name{summ_prop}
\alias{summ_prop}
\title{Create count with proportion as string}
\usage{
summ_prop(
  x,
  category = NULL,
  omit_na = T,
  digits = 1,
  incl_denom = F,
  out = c("percentage", "percent"),
  perc_disp = F,
  escape = F,
  zero2dash = T
)
}
\arguments{
\item{x}{vector}

\item{category}{Category to count x matches (not required if x logical)}

\item{omit_na}{logical for removal of missing values when reporting porportion}

\item{digits}{number of digits to round}

\item{incl_denom}{logical for inclusion of denominator in frequency}

\item{out}{single character representing output for proportion}

\item{perc_disp}{logical for inclusion of \% sign}

\item{escape}{logical for inclusion of escape character for LaTeX tables}

\item{zero2dash}{logical for returning "-" instead of "0 (0)" for tables}
}
\value{
Character String of the form "N ( \% )"
}
\description{
Create count with proportion as string
}
\examples{
x = sample(1:3, size = 20, replace = T)
summ_prop(x, category = 2,digits = 3)
summ_prop(x, category = 1, out = "percent")
summ_prop(x == 1)
summ_prop(x \%in\% 1:3)
}
