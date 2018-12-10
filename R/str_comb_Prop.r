#' Create count with proportion as string
#' @param x vector
#' @param category Category to count x matches (not required if x logical)
#' @param digits number of digits to round
#' @param out single character representing output for proportion
#' @param perc.disp logical for inclusion of \% sign
#' @param escape logical for inclusion of escape character for LaTeX tables
#' @param zero2dash logical for returning "-" instead of "0 (0)" for tables
#' @return Character String of the form "N ( \% )"
#' @examples
#' x = sample(1:3, size = 20, replace = T)
#' str_comb_prop(x, category = 2,digits = 3)
#' str_comb_prop(x, category = 1, out = "percent")
#' str_comb_prop(x == 1)
#' str_comb_prop(x %in% 1:3)

str_comb_prop = function(x, category = NULL, na.rm = T, digits = 1,
                         out = c("percentage","percent"),
                         perc.disp = F, escape = F,
                         zero2dash = T){
  if(missing(out)) out = "percent"
  if(typeof(category) == "double") category = as.integer(category)
  if(typeof(x) == "double") x = as.integer(x)
  if(typeof(x) != typeof(category) & !is.null(category) |
     !is.logical(x) & is.null(category)) {
    stop("Non Matching Data types")
  }
  if(is.logical(x) & is.null(category)) category = T

  N = sum(x == category, na.rm = na.rm)

  Tot = length(na.omit(x))

  p = print_dec(ifelse(tolower(out) == "percentage",round(N/Tot, digits = digits+2),
             ifelse(tolower(out) == "percent",round(100*N/Tot, digits = digits),
                    "err")), digits = digits)

  if(zero2dash == T & N == "0"){ #coerce 0 (0) -> '-'
    return("-")
  }
  if(perc.disp == T & tolower(out) == "percent"){ #add \% if requested
    p = stringr::str_c(p,ifelse(escape,"\\\\%","%"))
  }
  return(stringr::str_c(N, " (",p,")"))
}
