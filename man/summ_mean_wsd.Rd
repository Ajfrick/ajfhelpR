% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summ_mean_wsd.R
\name{summ_mean_wsd}
\alias{summ_mean_wsd}
\title{Create center with spread data summary for table}
\usage{
summ_mean_wsd(x, digits = 1, na.rm = T)
}
\arguments{
\item{x}{Numeric Vector}

\item{na.rm}{logical value for exclusion of NA values from calculation of mean and sd}

\item{limits}{numeric vector of length 2 for lower and upper limit, defaults to IQR}
}
\value{
Character String of the form \code{mean(x) (sd(x))}
}
\description{
Create center with spread data summary for table
}
\examples{
set.seed(1234)
x = sample(1:100, size = 100, replace = T)
summ_mean_wsd(x,digits = 3)
summ_mean_wsd(c(x, NA), digits = 1)
summ_mean_wsd(c(x, NA), digits = 1, na.rm = T)
data("iris")
bind_rows(
iris \%>\%
   group_by(Species) \%>\%
   summarise_all(summ_mean_wsd),
iris \%>\%
   select(-Species)
   summarise_all(summ_mean_wsd) \%>\%
   mutate(Species = "All")
   )

bind_rows(
iris \%>\%
   group_by(Species) \%>\%
   summarise_all(summ_mean_wsd, digits = 2),
iris \%>\%
   select(-Species)
   summarise_all(summ_mean_wsd, digits = 2) \%>\%
   mutate(Species = "All")
   )
}
