#' Count Missing and \% of Total
#'
#' @param x Vector
#' @param digits Number of decimal points to round to
#' @param incl_denom logical for inclusion of denominator in frequency
#' @param out return percent or percentage
#' @param zero2dash automatically change '0 (0)' to '-'
#' @return N (\%) of Number missing
#' @examples
#' x = 1:100
#' ind = sample(1:100, 10, replace = F)
#' x[ind] = NA
#' summ_na(x, out = "percent")
#' summ_na(x, out = "percentage")
#'
#' x = 1:200
#' summ_na(x)
#'
#' x[ind] = NA
#' summ_na(x)

summ_na = function(x, digits = 1, incl_denom = F,
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
  if(incl_denom){
    N = stringr::str_c(N, "/", Tot)
  }

  return(stringr::str_c(N, " (",p,")"))
}
