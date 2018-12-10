#' Count Missing
#'
#' @param x Vector
#' @return integer for number of missing
#' @examples
#' x = 1:100
#' ind = sample(1:100, 10, replace = F)
#' x[ind] = NA
#'
#' x = 1:200
#' na_count(x)
#'
#' x[ind] = NA
#' na_count(x)
#'

na_count = function(x){
  return(sum(is.na(x)))
}
