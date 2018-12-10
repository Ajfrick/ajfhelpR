
#' Generate Probabilities from logit link model
#'
#' @param fit model fit by brm function
#' @param digits number of digits to round
#' @param predlabs  optional vetor of labels for each predictor
#' @return Tibble with 3 columns: Name, Prob, 95% CI (as string)
#' @examples
#' x = runif(100,0,10)
#' y = sample(c(0,1), size = 100, replace = T)
#' z = runif(100,0,10)
#' g = rep(1:4,each = 25)
#' df = tibble(x = x,y = y, z = z, g = g)
#' brmfit = brm(y ~ x + z, data = df, family = bernoulli(link = "logit"))
#' brm_pval(brmfit)

brm_probs = function(fit, digits = 2, predlabs,...){
  #require(brms);require(tidyverse);require(sjmisc)

  coefs = coef(fit)
  pars = dim(coefs[[1]])[3]
  betacos = t(coefs[[1]][1,c(1,3:4),2:pars])

  if(missing(predlabs)){predlabs = rownames(betacos)}

  probs = tibble::tibble(Predictor = predlabs,
                 Prob = round(inv_logit_scaled(betacos[,1]), digits = 2),
                 Lower = round(inv_logit_scaled(betacos[,2]), digits = 2),
                 Upper = round(inv_logit_scaled(betacos[,3]), digits = 2)) %>%
    tidyr::mutate(probCI = stringr::str_c("(",Lower,", ",Upper,")")) %>%
    dplyr::select(Predictor, Prob, probCI)
  return(probs)
}
