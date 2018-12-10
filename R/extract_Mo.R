#' Create middle IQR string from numeric vector
#'
#' @param x character string
#' @param format Date string format
#' @return Month as a character string
#' @examples
#' extract_Mo(Sys.Date())
#' extract_Mo("1989-03-12")
#' extract_Mo("1989-03-12",numeric = T)
#' extract_Mo("031289", format = "%m%d%y")
#' extract_Mo("031289", format = "%m%d%y", numeric = T)
#' extract_Mo("12_1989_03", format = "%d_%Y_%m")

extract_Mo = function(x,format = "%Y-%m-%d", numeric = F){
  d = format(as.Date(x, format= format),"%m")
  if(numeric){return(as.numeric(d))}
  d
}
