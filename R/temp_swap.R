#' Temperature Transformation
#' @param faren Temperature in Farenheit
#' @param cels  Temperature in Celsius
#' @description Accepts different combinations of temperatures in farenheit and Celsius
#'
#' @return vector of logicals
#' @examples
#' coming soon
#'

temp_swap = function(faren = NA, cels = NA){
  if(is.na(faren) & is.na(cels)){
    warning("Please enter at least one temperature")
  }
  if(!is.na(faren)){
    temp = (5/9*(faren - 32))
    names(temp) = "Celsius"
  }
  if(!is.na(cels)){
    temp = (9/5*cels + 32)
    names(temp) = "Farenheit"
  }
  return(temp)
}
