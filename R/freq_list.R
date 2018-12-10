#' freq_list
#'
#' @param tab table to format
#' @description Reformats table/3D Array into frequency list
#' Currently usage requires table generated with raw variable names after attach(<data>)
#' @return tibble formulated with all combos and freq
#' @examples
#' attach(mtcars)
#' View(freq_list(table(cyl,vs,am,gear,carb)))
#' View(freq_list(table(cyl,vs,am)))
#' detach(mtcars)


freq_list = function(Tab){

  dims = dim(Tab)
  dimNams = dimnames(Tab)
  vars = names(dimNams)

  names = list()

  for(i in 1:length(dims)){
    names[[i]] = dimnames(Tab)[[i]]
  }

  Tib = expand.grid(names)
  colnames(Tib) = vars

  Tib$N = as.vector(Tab)
  Tot = sum(Tab)

  Tib = Tib %>%
    arrange(get(vars[1]),get(vars[2])) %>%
    dplyr::filter(N != 0) %>%
    dplyr::mutate(Pct = round(N / Tot, digits = 4)) %>%
    dplyr::arrange(-N) %>%
    dplyr::mutate(Cpct = cumsum(Pct))

  return(Tib)
}
