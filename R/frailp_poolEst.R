#' Format multiple imputed frailtypack `frailtyPenal` models into Hazard Ratio
#' table format with pooled estimates across imputed models
#'
#' @param modList list of frailtypack objects
#' @param digits number of digits top format to
#' @return tibble with Hazard Ratio, 95-CI (pvalue WIP)
#' @description Function accepts a list of frailtypack models and generates a
#' tibble with HR and CI
#' @examples
#' TBD
frailp_poolEst = function(modList, digits = 2){

  p = length(modList[[1]]$coef)

  m = length(modList)
  Q = matrix(0,m,p)
  U = array(0,c(p,p,m))
  B = numeric(1)
  Tv = numeric()

  for(i in 1:m){
    Q[i,] = modList[[i]]$coef
    U[,,i] = modList[[i]]$varH
  }

  Qm = apply(Q,FUN = mean, MARGIN = 2)
  Um = matrix(0,p,p)

  for(i in 1:p){
    for(j in 1:p){
      Um[i,j] = mean(U[i,j,])
    }
  }

  Qfac = 0

  for(i in 1:m){
    val = (Qm - Q[i,]) %*% t(Qm - Q[i,])
    Qfac = Qfac + val
  }

  B = 1/(m-1) * Qfac
  r = (1 + 1/m) * sum(diag(B %*% solve(Um)))/p
  Tv = (1 + r) * Um

  tib = tibble(
    Pred = names(modList[[1]]$coef),
    Est = Qm,
    HR  = exp(Est),
    SE  = sqrt(diag(Tv)),
    low  = exp(Qm - 2 * SE),
    upp  = exp(Qm + 2 * SE)
  ) %>%
    dplyr::mutate_if(is.numeric, round, digits) %>%
    dplyr::mutate_at(dplyr::vars(Est,SE,HR), print_dec, digits) %>%
    dplyr::mutate(CI   = paste0("(",print_dec(low, digits),", ",
                                print_dec(upp, digits),")")) %>%
    dplyr::select(Pred,Est,SE,HR,CI)

  return(tib)
}
