#' Sample Size and Power for McNemar's Test
#'
#' Computes sample size or power for McNemar's test for paired nominal data.
#'
#' @param p10 Probability of pre-test success and post-test failure. See
#' details.
#'
#' @param p01 Probability of pre-test failure and post-test success. See
#' details.
#'
#' @param alpha Specified significance level. Defaults to 0.05.
#'
#' @param n Sample size. Cannot be left blank if \code{power} is missing.
#'
#' @param power Statistical power. Cannot be left blank if \code{n} is missing.
#'
#' @details
#' H0: Both groups have the same success probability.
#' H1: The success probability is not equal between the Groups.
#'
#'                    Post
#'             Success    Failure
#'     Success   p11        p10
#' Pre
#'     Failure   p01        p00
#'
#' @references
#' github/bgreenwell For Code
#'
#' Connor R. J. 1987. Sample size for testing differences in proportions for the
#' paired-sample design. Biometrics 43(1):207-211. page 209.
#'
#' @examples
#' pwr.mcnemar(0.05, 0.45, power = 0.8)
#' pwr.mcnemar(0.05, 0.45, n = 22)
pwr_mcnemar <- function(p10, p01, alpha = 0.05, n, power) {
  pdisc <- p10 + p01
  pdiff <- p10 - p01
  if (missing(power) && !missing(n)) {
    x1 <- (pdiff * sqrt(n) - qnorm(1 - alpha / 2) * sqrt(pdisc)) /
      sqrt(pdisc - pdiff ^ 2)
    x2 <- (-pdiff * sqrt(n) - qnorm(1 - alpha / 2) * sqrt(pdisc)) /
      sqrt(pdisc - pdiff ^ 2)
    power <- pnorm(x1) + pnorm(x2)
  } else if (missing(n) && !missing(power)) {
    n <- ((qnorm(1 - alpha / 2) * sqrt(pdisc) + qnorm(power) *
             sqrt(pdisc - pdiff ^ 2)) / pdiff) ^ 2
  } else {
    stop("Must supply one of `n` or `power`, but not both.")
  }
  c("n" = n, "power" = power)
}
