#' Create middle IQR string from numeric vector
#'
#' @param x character string
#' @param format Date string format
#' @param numeric Return Year as numeric
#' @return Year as a character string
#' @examples
#' extract_yr(Sys.Date())
#' extract_yr("1989-03-12")
#' extract_yr("031289", format = "%m%d%y")
#' extract_yr("031289", format = "%m%d%y", numeric = T)
#' extract_yr("12_1989_03", format = "%d_%Y_%m")
#' extract_yr("12_1989_03", format = "%d_%Y_%m", numeric = T)

extract_yr = function(x,format = "%Y-%m-%d", numeric = F){
  d = format(as.Date(x, format= format),"%Y")
  if(numeric){return(as.numeric(d))}
  d
}
