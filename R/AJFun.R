#' na.rm functions for funs
#' @param x numeric vector used in summarise
#' @return single numeric value based on function name
#' @examples
#' set.seed(1234)
#' Tib = tibble(x = 1:100,
#'              y = (1:100)^2)
#' NA.indx = sample(1:100, size = 10, replace = FALSE)
#' NA.indy = sample(1:100, size = 10, replace = FALSE)
#' Tib[NA.indx,1] = NA
#' Tib[NA.indy,2] = NA
#' Tib %>% summarise(Q1x = q1.na(X),
#'                   meany= mean.na(y))


sum.na = function(x){
  sum(x, na.rm = T)
}

mean.na = function(x){
  mean(x, na.rm = T)
}

med.na= function(x){
  quantile(x, probs = .5, na.rm = T)
}

q1.na =  function(x){
  quantile(x, probs = 0.25, na.rm = T)
}

q3.na =  function(x){
  quantile(x, probs = 0.75, na.rm = T)
}


