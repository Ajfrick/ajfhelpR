#' Obtain pooled estimate for multiple imputation analyses with multiple parameters
#'
#' @param mods list of models corresponding to MI datasets
#' @param mod0 single model for same data without imputed predictor
#' @return Pvalue for f test for inclusion of MI predictor
#' @description Serious WIP missing full functionality. Will be replaced
#' @examples
#' TBD
#'
mimp_pool_mv = function(mods,mod0){

  p = length(mods[[1]]$coef)
  p0= length(mod0$coef)
  m = length(mods)
  Q = matrix(0,m,p)
  U = array(0,c(p,p,m))
  B = numeric(1)
  Tv = numeric()

  for(i in 1:10){
    Q[i,] = mods[[i]]$coef
    U[,,i] = mods[[i]]$varH
  }

  Qm = apply(Q,FUN = mean, MARGIN = 2)
  Um = matrix(0,p,p)

  for(i in 1:p){
    for(j in 1:p){
      Um[i,j] = mean(U[i,j,])
    }
  }

  Qfac = 0
  Q0   = c(mod0$coef,rep(0,times = p - p0))

  for(i in 1:m){
    val = (Qm - Q[i,]) %*% t(Qm - Q[i,])
    Qfac = Qfac + val
  }

  B = 1/(m-1) * Qfac
  r = (1 + 1/m) * sum(diag(B %*% solve(Um)))/p
  Tv = (1 + r) * Um
  f = t(Qm - Q0) %*% solve(Tv) %*% (Qm- Q0) / p
  q = p * (m - 1)
  v = 4 + (q - 4) * (1 + (1 + 2/q)/r)^2

  1-pf(f,p,v)
}


