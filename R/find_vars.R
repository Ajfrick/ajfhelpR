#' Create middle IQR string from numeric vector
#'
#' @param dat data frame, tibble, or named vector
#' @param pattern character vector to search for
#' @param ignore.case logical for case sensitivity
#' @return named vector with column or index locations with variable names
#' @examples
#' find_vars(mtcars, "cyl")
#' find_vars(1:100, "cyl")
#' mtcars[,find_vars(mtcars,"cyl")]
#' mtcars[,find_vars(mtcars,"c")]
#' mtcars[,find_vars(mtcars,"C",ignore.case = F)]

find_vars = function(dat,pattern,ignore.case = TRUE){
  if(tibble::is.tibble(dat) | is.data.frame(dat) ) cols = colnames(dat)
  else {
    cols = names(dat)
  }

  if(ignore.case){
    cols = tolower(cols)
    pattern = tolower(pattern)
  }

  index = which(grepl(pattern,cols))
  names(index) = cols[index]
  index
}
