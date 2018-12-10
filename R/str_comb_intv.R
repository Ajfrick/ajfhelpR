#' Create center with spread data summary for table
#'
#' @param x Numeric Vector
#' @param fun Function for Central Measure (Defaults to median)
#' @param limits numeric vector of length 2 for lower and upper limit, defaults to IQR
#' @param digits number of digits to round
#' @param delim  delimiter of choice
#' @return Character String of the form \code{fun(x) <delim> (Q1,Q3)}
#' @examples
#' x = seq(1,100)
#' str_comb_Intv(x,digits = 3)
#' str_comb_Intv(x, lower = 0.1, upper = 0.85)
#' str_comb_Intv(x^2,fun = median, delim = " - ")
#' str_comb_Intv(x^2,fun = mean, delim = "_")

str_comb_intv = function(x, fun = median, digits = 2, delim = ",",
                        limits = c(0.25,0.75)){

  Cent = round(fun(as.numeric(x), na.rm = T), digits = digits)

  if(length(limits) == 1){
    warning("Please provide length two vector for limits")
    return()
  }

  limits = sort(limits)

  Intv = round(quantile(as.numeric(x), probs = c(limits[1],limits[2]), na.rm = T), digits = digits)

  return(stringr::str_c(print_dec(Cent,digits)," (",
                        print_dec(Intv[1],digits), delim,
                        print_dec(Intv[2],digits), ")"))
}
