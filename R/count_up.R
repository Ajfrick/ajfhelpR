#' Count number of unique non-missing observations in a vector
#' @param x Vector of values to tally up
#' @description This function tallies up the number of unique observations
#' within a vector. Input vector can be comprised of any data type
#' @return integer representing number of non-missing values in a vector
#' @examples
#' x = 1:10
#' count_up(x)
#'
#' x[c(1,5,7)] = NA
#' count_up(x)
#'
#' x = sample(1:5, size = 100, replace = T)
#' count_up(x)
#'
#' y = x[x != 5]
#' count_up(y)

count_up = function(x){
  length(unique(na.omit(x)))
}



