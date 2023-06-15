#' Create column for frequency distribution of data
#' @param x vector of characters or factors
#' @param omit_na logical for ommision of NAs in computation of counts and
#' proportions of non missing data
#' @param digits number of digits to round
#' @param incl_denom logical for inclusion of denominator when displaying frequencies
#' @param remove_na logical for removal of frequency of missing data
#' @param out single character representing output for proportion
#' @param perc_disp logical for inclustion of \% sign
#' @param escape logical for inclusion of escape character for LaTeX tables
#' @param zero2dash logical for returning "-" instead of "0 (0)" for tables
#' @return dataframe/tibble with two columns, one for category name, and one with
#' N (\\%) output from summ_prop function
#' @examples
#' dat = iris
#' specs = dat$Species
#' cat_freqs(specs)
#' cat_freqs(iris$Species)
#'
#' #Playing around with Missing data
#'
#' set.seed(314159)
#' index_na = sample(1:nrow(dat), size = 25, replace = F)
#' dat[index_na,]$Species = NA
#'
#' # Different output based on arguments to `omit_na`
#' cat_freqs(dat$Species) #omit_na = F default
#' cat_freqs(dat$Species, omit_na = T)
#' cat_freqs(dat$Species, incl_denom = T)

cat_freqs = function(x, omit_na = T, NAname = NA, digits = 1,
                     incl_denom = F, remove_na = F,
                     out = c("percentage","percent"),
                     perc_disp = F, escape = F,
                     zero2dash = T){

  if(remove_na == T){
    x = na.omit(x)
  }
    x2 = x
  if(missing(out)) out = "percent"
  if(omit_na){
    x2 = na.omit(x)
  }

  n = length(x)
  M = length(table(x))

  # if(M < 2){
  #   warning("Less than 2 categories, function has no purpose")
  #   return()
  # }

  cats = as.character(names(table(x, useNA = 'ifany')))

  tib = dplyr::tibble(
    N = summ_prop(x2 %==% cats[1], digits = digits, out = out,
                  perc_disp = perc_disp, zero2dash = zero2dash,
                  incl_denom = incl_denom,escape = escape)
  )
  if( M >= 2){
    for(i in 2:M){
      tib[i,1] = summ_prop(x2 %==% cats[i], digits = digits, out = out,
                           perc_disp = perc_disp, zero2dash = zero2dash,
                           incl_denom = incl_denom,escape = escape)
    }
  }
  if( (sum(is.na(x)) == 0)){
    tib = tib %>%
      dplyr::mutate(Cat = cats) %>%
      dplyr::select(Cat,N)

    return(tib)
  }else{
    tib[(M+1),1] = summ_prop(is.na(x), digits = digits, out = out,
                             perc_disp = perc_disp, zero2dash = zero2dash,
                             incl_denom = incl_denom,escape = escape)
   tib = tib %>%
      dplyr::mutate(Cat = cats) %>%
      dplyr::select(Cat,N)

   tib[is.na(tib$Cat),1] = NAname

    return(tib)
  }
}
