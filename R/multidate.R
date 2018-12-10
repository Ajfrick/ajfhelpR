#' Read Dates from varying forms
#'
#' @param data Data Set (for piping)
#' @param formats Character vector containing the various date formats
#' @return single Date object
#' @description Useful for combining with apply when columns
#' have various date formats
#' @examples
#' multidate("03121989")
#' multidate("03-12-89")
#' multidate("03/12/1989")
#'
#' ##Not Run
#' library(tidyverse)
#' date_list = list(c("03121989","03-12-89","03/12/1989"))
#' dates = lapply(date_list, multidate)[[1]]
#'
#' ##Can also adpat to different formats for apply
#' date_list = list(c("128903","12198903","03/-12/-1989"))
#' my_multidate = function(x){
#' multidate(data = x, formats = c("%d%y%m","%d%Y%m","%m/-%d/-%Y"))
#' }
#' dates = lapply(date_list, my_multidate)[[1]]

multidate = function(data, formats = c("%m%d%Y", "%m-%d-%y", "%m/%d/%Y")){
  a = list()
  for(i in 1:length(formats)){
    a[[i]] = as.Date(data, format = formats[i])
    a[[1]][!is.na(a[[i]])] = a[[i]][!is.na(a[[i]])]
  }
  a[[1]]
}
