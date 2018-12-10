
#' Replace PI excel column names with R friendly names
#'
#' @param colname character vector for column names in dataset
#' @return character vector with R friendly column names
#' @examples
#' Coming soon

str_excel2R = function(colname){
  colname = stringr::str_replace_all(colname," |-","_")
  colname = stringr::str_replace_all(colname,"'|\\(|\\)|/","")
  return(colname)
}
