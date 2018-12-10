#' Function to force print to desired number of decimal points
#'
#' @param x Numeric Value
#' @param digits NUmber of digits to force printed
#' @return character representation of numeric value with desired number of floating points
#' @examples
#' TBD
print_dec = function(x,digits = 2){
  sprintf(paste0('%.',digits,'f'),x)
}
