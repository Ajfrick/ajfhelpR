#' Count up Frequency between two dates
#' @param x vector of Date objects
#' @param start Start of time interval in YYYY-MM-DD format
#' @param end End of time interval in YYYY-MM-DD format
#' @param rel logical for output as relative frequency
#' @param incl logical for inclusion of endpoints
#' @description Counts up number of elements in x between the start and end
#' point
#'
#' lower and upper default to Jan 1AD and Jan 4000AD, so one sided intervals only need one
#' @description Counts up number of elements in x between the start and end
#' point
#'
#' @return integer for total number of observations between time period or
#' double for relative frequency
#' @examples
#' soon.tm

date_count = function(x, lower = "0001-01-01", upper = "4000-01-01", rel = F,
                      incl = F){
  lower = as.Date(lower, format = "%Y-%m-%d")
  upper = as.Date(upper, format = "%Y-%m-%d")

  sum = ifelse(incl,length(x[x >= lower & x <= upper]),
                    length(x[x > lower & x < upper]))

  out = ifelse(rel,sum/length(x),sum)

  out
}
