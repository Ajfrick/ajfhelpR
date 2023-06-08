#' Create center with spread data summary for table
#'
#' @param x Numeric Vector
#' @param limits numeric vector of length 2 for lower and upper limit, defaults to IQR
#' @param na.rm logical value for exclusion of NA values from calculation of mean and sd
#' @return Character String of the form \code{mean(x) (sd(x))}
#' @examples
#' set.seed(1234)
#' x = sample(1:100, size = 100, replace = T)
#' summ_mean_wsd(x,digits = 3)
#' summ_mean_wsd(c(x, NA), digits = 1)
#' summ_mean_wsd(c(x, NA), digits = 1, na.rm = T)
#' data("iris")
#' bind_rows(
#' iris %>%
#'    group_by(Species) %>%
#'    summarise_all(summ_mean_wsd),
#' iris %>%
#'    select(-Species)
#'    summarise_all(summ_mean_wsd) %>%
#'    mutate(Species = "All")
#'    )
#'
#' bind_rows(
#' iris %>%
#'    group_by(Species) %>%
#'    summarise_all(summ_mean_wsd, digits = 2),
#' iris %>%
#'    select(-Species)
#'    summarise_all(summ_mean_wsd, digits = 2) %>%
#'    mutate(Species = "All")
#'    )

summ_mean_wsd = function(x, digits = 1, na.rm = T){
  paste0(print_dec(mean(x, na.rm = na.rm), digits = digits),
        " (",
        print_dec(sd(x, na.rm = na.rm), digits = digits),
        ")")
}
