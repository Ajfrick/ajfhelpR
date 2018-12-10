#' Count Missing and \% of Total
#'
#' @param x Vector
#' @param digits Number of decimal points to round to
#' @param out return percent or percentage
#' @param zero2dash automatically change '0 (0)' to '-'
#' @return N (\%) of Number missing
#' @examples
#' x = 1:100
#' ind = sample(1:100, 10, replace = F)
#' x[ind] = NA
#' str_comb_NA(x, out = "percent")
#' str_comb_NA(x, out = "percentage")
#'
#' x = 1:200
#' str_comb_NA(x)
#'
#' x[ind] = NA
#' str_comb_NA(x)

str_comb_NA = function(x, digits = 1,
                       out = c("percentage","percent"),
                       zero2dash = T){
  if(missing(out)) out = "percent"

  N = sum(is.na(x))

  Tot = length(x)

  p = ifelse(tolower(out) == "percentage",round(N/Tot, digits = digits+2),
             ifelse(tolower(out) == "percent",round(100*N/Tot, digits = digits),
                    "err"))

  if(zero2dash == T & N == "0"){ #coerce 0 (0) -> '-'
    return("-")
  }

  return(stringr::str_c(N, " (",p,")"))
}
