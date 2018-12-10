#' Create column for frequency distribution of data
#' @param x vector of characters or factors
#' @param omit.na logical for ommision of NAs in computation of counts and
#' proportions
#' @param digits number of digits to round
#' @param out single character representing output for proportion
#' @param perc.disp logical for inclustion of \% sign
#' @param escape logical for inclusion of escape character for LaTeX tables
#' @param zero2dash logical for returning "-" instead of "0 (0)" for tables
#' @return dataframe/tibble with two columns, one for category name, and one with
#' N (\\%) output from str_comb_prop function
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
#' # Different output based on arguments to `omit.na`
#' cat_freqs(dat$Species) #omit.na = F default
#' cat_freqs(dat$Species, omit.na = T)

cat_freqs = function(x, omit.na = F, NAname = NA, digits = 1,
                     out = c("percentage","percent"),
                     perc.disp = F, escape = F,
                     zero2dash = T){

  if(missing(out)) out = "percent"
  if(omit.na){
    x = na.omit(x)
  }

  n = length(x)
  M = length(table(x))

  # if(M < 2){
  #   warning("Less than 2 categories, function has no purpose")
  #   return()
  # }

  cats = as.character(names(table(x, useNA = 'ifany')))

  tib = dplyr::tibble(
    N = str_comb_prop(x %==% cats[1], digits = digits, out = out,
                      perc.disp = perc.disp, zero2dash = zero2dash,
                      escape = escape)
  )
  for(i in 2:M){
    tib[i,1] = str_comb_prop(x %==% cats[i], digits = digits, out = out,
                             perc.disp = perc.disp, zero2dash = zero2dash,
                             escape = escape)
  }
  if(omit.na | (sum(is.na(x)) == 0)){
    tib = tib %>%
      dplyr::mutate(Cat = cats) %>%
      dplyr::select(Cat,N)

    return(tib)
  }else{
    tib[(M+1),1] = str_comb_prop(is.na(x), digits = digits, out = out,
                                 perc.disp = perc.disp, zero2dash = zero2dash)

   tib = tib %>%
      dplyr::mutate(Cat = cats) %>%
      dplyr::select(Cat,N)

   tib[is.na(tib$Cat),1] = NAname

    return(tib)
  }
}
