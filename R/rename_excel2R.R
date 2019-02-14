#' Pipeable column header rename function
#'
#' @description This function takes a data frame or tibble object and replaces
#' any 'bad' column names (containing whitespace, dashes, slashes, etc) and replaces
#' them with more R friendly column names. Most often when imported manual Excel
#' data into R for further analysis
#' @param dat Dataset to rename
#' @return dataframe or tibble containing the updated names
#' @examples
#' library(tibble)
#' library(stringr)
#' set.seed(1234)
#'
#' dat = tibble(
#' "Column 1" = 1:10,
#' "Column#2" = sample(1:10, size = 10, replace = T),
#' "Col1/Col2" = `Column 1`/`Column#2`,
#' "| New~Column |" = "test"
#' )
#'
#' newdat = rename_excel2R(dat)
#' newdat
#' newdat2 = dat %>% rename_excel2R
#' all.equal(newdat, newdat2)
#'
#' pipe_test = dat %>%
#'   rename_excel2R %>%
#'   select_if(is.double)
#'

rename_excel2R = function(dat){
  colname = colnames(dat)
  colname = stringr::str_replace_all(colname,"#","num")
  colname = stringr::str_replace_all(colname," |-","_")
  colname = stringr::str_replace_all(colname,"~|'|\\(|\\)|/|\\|","")
  colname = stringr::str_replace_all(colname,"^_","")
  colnames(dat) = colname
  return(dat)
}
