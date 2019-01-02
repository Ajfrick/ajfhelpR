#' Extract month from Date object
#'
#' @param x character string
#' @param format Date string format
#' @return month as a character string
#' @examples
#' extract_mo(Sys.Date())
#' extract_mo("1989-03-12")
#' extract_mo("1989-03-12",numeric = T)
#' extract_mo("031289", format = "%m%d%y")
#' extract_mo("031289", format = "%m%d%y", numeric = T)
#' extract_mo("12_1989_03", format = "%d_%Y_%m")

extract_mo = function(x,format = "%Y-%m-%d", numeric = F){
  d = format(as.Date(x, format= format),"%m")
  if(numeric){return(as.numeric(d))}
  d
}
