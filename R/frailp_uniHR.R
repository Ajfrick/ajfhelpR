#' Format single predictor frailtypack `frailtyPenal` models into Hazard Ratio
#' table format
#'
#' @param mod frailtyPenal object
#' @param digits number of digits top format to
#' @return tibble with Hazard Ratio, 95-CI, and p value
#' @description Function accepts a frailtypack model and generates a one row table
#' for binding together with other models in univariate table
#' @examples
#' TBD
frailp_uniHR = function(mod, digits = 2){require(frailtypack)

  # Generate standard table values
  tib = tibble(
    Pred = names(mod$coef),
    Est  = mod$coef,
    HR   = exp(Est),
    SE   = sqrt(mod$varH),
    low  = exp(log(HR) - 1.96 * SE),
    upp  = exp(log(HR) + 1.96 * SE),
    p    = mod$beta_p.value
  ) %>%
    # Convert numerics to string with desired number of decimals
    dplyr::mutate_if(is.numeric, round, digits) %>%
    dplyr::mutate_at(dplyr::vars(p), to_lessthan) %>%
    dplyr::mutate_at(dplyr::vars(Est,SE,HR), print_dec, digits) %>%
    dplyr::mutate(CI   = paste0("(",print_dec(low, digits),", ",
                                print_dec(upp, digits),")")) %>%
    dplyr::select(Pred,Est,SE,HR,CI,p)

  tib

}
