#' Squeezing numeric values within specified range
#' @param x vector of numeric values
#' @param low  lower limit of numeric range
#' @param high upper limit of numeric range
#' @description Coerces numbers outside the specified range to the min or max
#' value. low and high arguments default to `-Inf` and `Inf`
#' Function will return NULL if lower
#' @return numeric vector
#' @examples
#' x = -100:100
#'
#' num_squeeze(x)
#' num_squeeze(x, low = 0)
#' num_squeeze(x, low = -1e3, high = 1e3)
#' num_squeeze(x, low = -2, high = -5) #returns NULL with warning
#' num_squeeze(x, low = 0, high = 0)

num_squeeze = function(x, low = -Inf, high = Inf){
  if(low > high ){
    warning("lower bound is greater than upper bound")
    return(NULL)
  }
  x[x<low] = low
  x[x>high]= high
  x
}
