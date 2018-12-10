#' 5-Number summary as character string
#'
#' @param x Numeric Vector
#' @param fun Function for Central Measure (Defaults to median)
#' @param digits number of digits to round
#' @param delim  delimiter of choice
#' @return Character String of the form \code{fun(x) <delim> (Q1,Q3) [min,max]}
#' @examples
#' x = seq(1,100)
#' str_comb_full(x,digits = 3)
#' str_comb_full(x, lower = 0.1, upper = 0.85)
#' str_comb_full(x^2,fun = median, delim = " - ")
#' str_comb_full(x^2,fun = mean, delim = "_")

str_comb_full = function(x, fun = median, digits = 2, delim = ","){

  full = round(quantile(as.numeric(x),probs = seq(0,1,by=0.25),na.rm=T),
               digits = digits)
  full[3] = round(fun(x), digits = digits)

  funName = deparse(substitute(fun))
  funName = paste0(toupper(substring(funName, 1, 1)),
                   substring(funName,2),
                   sep = "", collapse = " ")

  return(stringr::str_c(funName, ": ",full[3],
                        " IQR (",full[2],delim,full[4],") ",
                        "Range [",full[1],delim,full[5],"]"))
}
