#' Replace NAs with '-'
#'
#' @param x Vector
#' @return `x` coerced to character vector with all NAs replaced with `-`
#' @examples
#' x = 1:10
#' x[c(2,4,7)] = NA
#' na_2dash(x)
#'
#' #install.packages(tidyverse)
#' library(tidyverse)
#'
#' set.seed(271828)
#' dat = iris[1,30]
#' na_ind = sample(1:nrow(dat), size = 10)
#' dat$Species[na_ind] = NA
#'
#' dat %>% mutate_at(vars(Species), na_2dash) %>% View

na_2dash = function(x){
  x = as.character(x)
  x[which(is.na(x))] = "-"
  x
}
