#' Generate Odds Ratios CI Pval table from GLM
#'
#' @param fit model fit by brm function
#' @return Tibble with 5 columns: Name (char), OR, 2.5%, 97.%, p-val (doubles)
#' @examples
#' x = runif(100,0,10)
#' y = sample(c(0,1), size = 100, replace = T)
#' z = runif(100,0,10)
#' df = tibble(
#' x = x,
#' y = y,
#' z = z
#' )
#' glmfit = glm(y ~ x + z, data = df, family = bernoulli(link = "logit"))
#' glm_ORs(glmfit)

glm_ORs = function(fit){

  CI = suppressMessages(confint(fit))[-1,]
  summ = coef(summary(fit))[-1,c(1,4)]

  if(nrow(coef(summary(fit)))==2){
    Tib = tibble(
      Name = rownames(coef(summary(fit)))[-1],
      OR =  exp(summ[1]),
      Lower = exp(CI[1]),
      Upper = exp(CI[2]),
      p   = summ[2]
    )
    Tib
  }else{
    Tib = tibble(
      Name = rownames(summ),
      OR =  exp(summ[,1]),
      Lower = exp(CI[,1]),
      Upper = exp(CI[,2]),
      p   = summ[,2]
    )
    Tib
  }
}

