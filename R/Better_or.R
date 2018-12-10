#' F or NA -> F Operator
#' @param left Left side of operator
#' @param right right side of operator
#' @description Replicates `|` behavior, but returns F instead of NA when making comparisons
#' of F or NA. NA & NA still returns missing when comparing equal length
#' vectors.
#'
#' Left and Right sides must be same length, or one of them length 1.
#'
#' Equal length vectors will return vector of point-wise comparisons between the two
#'
#' If left or right is length 1, it compares it to the entire other vector, returning
#' a vector of logicals comparing to each place.
#' @return vector of logicals
#' @examples
#' coming soon

`%or%` = function(left,right){

  if(xor(length(left) == 1,length(right) == 1)){
    inputs = list(left,right)
    lengths = unlist(lapply(inputs,length))

    lil = inputs[[which(lengths == 1)]]
    right = inputs[[which(lengths != 1)]]

    left = rep(lil, times = length(right))
  }
  results = logical(length(left))
  for(i in 1:length(left)){
    if(is.na(left[i]) & is.na(right[i])) {  ###  NA | NA -> NA
      results[i] = NA
    }else{
      results[i] = left[i] | right[i]
      if(right[i] == F & is.na(left[i]) |
         left[i] == F & is.na(right[i])){ ## (F | NA) | (NA | F) -> F
        results[i] = F
      }
    }
  }
  results
}



