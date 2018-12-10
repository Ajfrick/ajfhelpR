#' Format single predictor frailtypack `frailtyPenal` models into Hazard Ratio
#' table format from multiple imputed data sets
#'
#' @param modList list of frailtypack objects
#' @param digits number of digits top format to
#' @return tibble with Hazard Ratio, 95-CI, and p value
#' @description Function accepts a list of frailtypack models and generates a
#' one row table for binding together with other models in univariate table
#' @examples
#' TBD
frailp_uniHR_mimp = function(modList, digits = 2){require(frailtypack)

  q = numeric()
  u = numeric()

  for(i in 1:length(modList)){
    q[i] = modList[[i]]$coef
    u[i] = sqrt(modList[[i]]$varH)
  }
  pool = mimp_pool_uv(q,u)

  tib = tibble(
    Pred = names(modList[[1]]$coef),
    Est  = pool$PoolEst,
    HR   = exp(Est),
    SE   = pool$PoolSE,
    low  = exp(log(HR) - qt(0.975, pool$df) * SE),
    upp  = exp(log(HR) + qt(0.975, pool$df) * SE),
    p    = pool$p
  ) %>%
    dplyr::mutate_if(is.numeric, round, digits) %>%
    dplyr::mutate_at(dplyr::vars(p), to_lessthan) %>%
    dplyr::mutate_at(dplyr::vars(Est,SE,HR), print_dec, digits) %>%
    dplyr::mutate(CI   = paste0("(",print_dec(low, digits),", ",
                                print_dec(upp, digits),")")) %>%
    dplyr::select(Pred,Est,SE,HR,CI,p)

  tib
}

