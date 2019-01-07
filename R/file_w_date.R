#' Create File path with Today's Date appended
#'
#' @param dir path to directory
#' @param file filename
#' @param ext extension type
#' @param date logical for Y/N appended date
#' @return Character String with the path to filename
#' @description used to paste strings together for file location.
#' Primary usage is for long directory names that you can store in a string, as
#' well as for automatically appending the date to a file name for intermittenly
#' run reports/etc.
#' @examples
#' file_w_date("C:/Documents", "Test", "csv")
#' data(mtcars)
#' DataEx = head(mtcars)
#' write.csv(DataEx, file_w_date("C:/Documents", "Test", "csv"))
#' write.csv(DataEx, file_w_date("C:/Documents", "Test", "csv", date = F))

file_w_date = function(dir,file,ext, date = T){
  ifelse(date==T,
    stringr::str_c(dir,file,"_",
          stringr::str_replace_all(Sys.Date(),"-",""),
          ".",ext),
    stringr::str_c(dir,file,
          ".",ext))
}
