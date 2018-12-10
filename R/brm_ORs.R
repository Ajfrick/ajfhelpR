
#' Generate Odds Ratios from logit link brm model
#'
#' @param fit model fit by brm function
#' @param digits number of digits to round
#' @param predlabs  optional vetor of labels for each predictor
#' @return Tibble with 3 columns: Name, Prob, 95% CI (as string)
#' @examples
#'  require(brms)
#' x = runif(100,0,10)
#' y = sample(c(0,1), size = 100, replace = T)
#' z = runif(100,0,10)
#' df = tibble(x = x,y = y)
#' brmfit = brm(y ~ x + z, data = df, family = bernoulli(link = "logit"))
#' brm_pval(brmfit)

brm_ors = function(fit, digits = 2, predlabs,...){

  # form = fit$formula
  coefs = coef(fit)
  pars = dim(coefs[[1]])[3]
  betacos = t(coefs[[1]][1,c(1,3:4),2:pars])

  if(!missing(predlabs)){
    if(length(predlabs) != (pars-1)){
      warning("Improper Number of Predictor Labels")
    }
  }

  if(missing(predlabs)){
    ORs = tibble::tibble(Predictor = rownames(betacos),
                 OR = round(exp(betacos[,1]),digits = digits),
                 Lower95 = round(exp(betacos[,2]),digits = digits),
                 Upper95 = round(exp(betacos[,3]),digits = digits),
                 pValue  = round(brm_pval(fit)$pvals,digits = max(3, digits)),
                 #  'p<0.05'   = ifelse((Lower95>=1 & Upper95>=1) | (Lower95 <= 1 & Upper95 <= 1),
                 #                 "*","")
                 CI = stringr::str_c("(",Lower95,",",Upper95,")"),
                 sig = ifelse(pValue<=0.05,"*",""))%>%
      dplyr::select(Predictor, OR, CI, pValue, sig)
    return(ORs)
  }else if(!missing(predlabs)){
    ORs = tibble::tibble(Predictor = predlabs,
                 OR = round(exp(betacos[,1]),digits = digits),
                 Lower95 = round(exp(betacos[,2]),digits = digits),
                 Upper95 = round(exp(betacos[,3]),digits = digits),
                 pValue  = round(brm_pval(fit)$pvals,digits = max(3, digits)),
                 #  'p<0.05'   = ifelse((Lower95>=1 & Upper95>=1) | (Lower95 <= 1 & Upper95 <= 1),
                 #                 "*","")
                 CI = stringr::str_c("(",Lower95,",",Upper95,")"),
                 sig = ifelse(pValue<=0.05,"*",""))%>%
      dplyr::select(Predictor, OR, CI, pValue, sig)
    return(ORs)
  }
}
