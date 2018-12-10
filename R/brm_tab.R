
#' Generate Coefficient Significance tables from brm model
#'
#' @param fit model fit by brm function
#' @param digits number of digits to round
#' @param predlabs  optional vetor of labels for each predictor
#' @return Tibble with 4 columns: Name, Prob, 95% CI (as string), pvalue
#' @examples
#' require(brms)
#' x = 1:100
#' y = x^2 + rnorm(length(x))
#' z = runif(length(x),0,length(x))
#' g = rep(1:4,each = 25)
#' df = tibble(x = x,y = y,z = z, g = g)
#' brmfit = brm(y ~ x^2 + z + (1|g), data = df)
#' brm_tab(brmfit)

brm_tab = function(fit, digits = 2, predlabs,...){

  # form = fit$formula
  coefs = coef(fit)
  pars = dim(coefs[[1]])[3]
  betacos = t(coefs[[1]][1,c(1,3:4),2:pars])

  if(!missing(predlabs)){
    if(length(predlabs) != (pars-1)){
      warning("Improper Number of Predictor Labels")
    }
  }

  if(pars == 2){
    Tabs = tibble::tibble(Predictor = rownames(fixef(fit))[-1],
                          RegCoef = round(betacos[,1],digits = digits),
                          Lower95 = round(betacos[,2],digits = digits),
                          Upper95 = round(betacos[,3],digits = digits),
                          pValue  = round(brm_pval(fit)$pvals,digits = max(3, digits)),
                          #  'p<0.05'   = ifelse((Lower95>=1 & Upper95>=1) | (Lower95 <= 1 & Upper95 <= 1),
                          #                 "*","")
                          CI = stringr::str_c("(",Lower95,",",Upper95,")"),
                          sig = ifelse(pValue<=0.05,"*",""))%>%
      dplyr::select(Predictor, RegCoef, CI, pValue, sig)
    return(Tabs)
  }
  if(missing(predlabs)){
    Tabs = tibble::tibble(Predictor = rownames(betacos),
                  RegCoef = round(betacos[,1],digits = digits),
                  Lower95 = round(betacos[,2],digits = digits),
                  Upper95 = round(betacos[,3],digits = digits),
                  pValue  = round(brm_pval(fit)$pvals,digits = max(3, digits)),
                  #  'p<0.05'   = ifelse((Lower95>=1 & Upper95>=1) | (Lower95 <= 1 & Upper95 <= 1),
                  #                 "*","")
                  CI = stringr::str_c("(",Lower95,",",Upper95,")"),
                  sig = ifelse(pValue<=0.05,"*",""))%>%
      dplyr::select(Predictor, RegCoef, CI, pValue, sig)
    return(Tabs)
  }else if(!missing(predlabs)){
    Tabs = tibble::tibble(Predictor = predlabs,
                  RegCoef = round(betacos[,1],digits = digits),
                  Lower95 = round(betacos[,2],digits = digits),
                  Upper95 = round(betacos[,3],digits = digits),
                  pValue  = round(brm_pval(fit)$pvals,digits = max(3, digits)),
                  #  'p<0.05'   = ifelse((Lower95>=1 & Upper95>=1) | (Lower95 <= 1 & Upper95 <= 1),
                  #                 "*","")
                  CI = stringr::str_c("(",Lower95,",",Upper95,")"),
                  sig = ifelse(pValue<=0.05,"*",""))%>%
      dplyr::select(Predictor, RegCoef, CI, pValue, sig)
    return(Tabs)
  }

}
