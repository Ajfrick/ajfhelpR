#' Create middle IQR string from numeric vector
#'
#' @param x character string
#' @param format Date string format
#' @param numeric Return Year as numeric
#' @return Year as a character string
#' @examples
#' extract_Yr(Sys.Date())
#' extract_Yr("1989-03-12")
#' extract_Yr("031289", format = "%m%d%y")
#' extract_Yr("031289", format = "%m%d%y", numeric = T)
#' extract_Yr("12_1989_03", format = "%d_%Y_%m")
#' extract_Yr("12_1989_03", format = "%d_%Y_%m", numeric = T)

extract_Yr = function(x,format = "%Y-%m-%d", numeric = F){
  d = format(as.Date(x, format= format),"%Y")
  if(numeric){return(as.numeric(d))}
  d
}
