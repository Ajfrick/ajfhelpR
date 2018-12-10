#' Obtain pooled estimate for multiple imputation analyses
#'
#' @param q Vector of estimates obtained from MI
#' @param u Vector of SEs corresponding to each estimate
#' @return list with pooled estimate and se, along with t statistic and pvalue
#' @examples
#' TBD
#'
mimp_pool_uv = function(q,u){

  m = length(q)
  qbar = mean(q)

  ubar = mean(u^2)
  b = sum((q - qbar)^2)/(m - 1)
  t    = ubar + (1 + 1/m)* b

  tstat    = qbar/sqrt(t)

  df = (m - 1) * (1 + ubar/((1 + 1/m)*t))^2

  p = (1 - pt(abs(tstat),df = df))*2

  out = list("PoolEst" = qbar,
             "PoolSE"  = sqrt(t),
             "t"       = tstat,
             "df"      = df,
             "p"       = p)
  return(out)

}



