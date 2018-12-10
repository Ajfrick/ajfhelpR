#' Generate Hazard ratio, CI,  Pval table from parfm
#'
#' @param fit model fit by parfm function
#' @param pred Optional Argument for models with single predictor
#' @description Generates table with Hazard rate, 95 CI and pval for parfm models fixed effects
#' WIP name extraction via other methods, until then names for models with single predictor
#' will have blank in Name column
#' @return Tibble with 5 columns: Name (char), HR, 95 CI, p-val (doubles)
#' @examples
#' data(kidney)
#'
#' fit = parfm(Surv(time,status) ~ age + sex + disease,
#'              cluster = "id", data = kidney,
#'              dist = "weibull", frailty = "gamma")
#'
#' parfm_HRs(fit)

parfm_HRs = function(fit, pred = NA){
  CI  = parfm::ci.parfm(fit)
  tab = dplyr::tibble(
    Name= ifelse(is.null(names(coef(fit))),pred,names(coef(fit))),
    HR = round(exp(coef(fit)),digits = 3),
    low = round(CI[,1],digits = 3),
    upp = round(CI[,2],digits = 3),
    CI  = stringr::str_c("(",low,", ",upp,")"),
    p = round(na.omit(fit[,3]), digits = 2)
  ) %>% dplyr::select(Name,HR,CI,p)
  tab
}

