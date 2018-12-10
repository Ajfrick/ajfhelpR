#' Find Nearest Date within Threshold
#' @param dates vector of Date objects to search through
#' @param target Date object to find nearest match
#' @param thresh numeric value for threshold for maximum number of days between match
#' @param onlypre logical for right censor of dates
#' @param sidepref character for decision in case left and right side both have minimum distance
#' @description Searches through vector of Dates for nearest match using given criteria according to
#' function arguments.
#'
#' sidepref must be filled in as 'l' or 'r' to receieve consistent results
#' If sidepref is missing date_near will return one at random
#'
#' @return Date object representing closest match to target Date object
#' @examples
#' x = seq.Date(from = as.Date("2010-01-01"), to = as.Date("2012-01-01"), length.out = 30)
#' y = as.Date("2011-01-01")
#' date_near(x,y, sidepref = 'l')
#' date_near(x,y, sidepref = 'r')
#' date_near(x,y, thresh = 1)
#' date_near(x,"2011-01-12", onlypre = T)
#'

date_near = function(dates, target, thresh = Inf, onlypre = F,
                     sidepref, quiet = T){
  ##Basic options
  target = unique(target)
  if(is.character(target)){target = as.Date(target)}
  if(length(target) != 1){
    warning("Target argument should only contain one Date")
    return(NA)
  }

  ## Remove points after if only before points desired
  if(onlypre == T){
    dates = dates[dates < target]
  }
  dates = unique(sort(dates))
  dates = dates[abs(dates-target)<thresh]
  delts = as.numeric(abs(dates-target))
  if(length(delts) == 0){
    if(!quiet){
      warning("No Match found within threshold")
    }
    return(NA)
    }
  ind = delts == min(delts)
  date  = dates[ind]

  if(length(date) == 2){
    if(missing(sidepref)){
      date = date[sample(1:2,1)]
    }else if(sidepref == "l"){
      date = date[1]
    }else{
      date = date[2]
    }
  }
  return(date)
}
