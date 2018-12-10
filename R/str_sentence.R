#' Modify sentence to Capitalize only first word
#'
#' @param string string to put in sentence form
#' @param noCaps Additional Words to not capitalize
#' @description Coerces everything to lower case, then capitalizes the first
#' word and every word after that isn't 2-3 letter commonly lowercase word.
#'
#' "in", "is","on", "at", "to", "from", "by","and", "but",
#'  "or", "for", "nor", "a", "an" all kept lowercase by default, additional
#'  words can be supplied with noCaps argument
#' @return Character String with proper capitalization
#' @examples
#' string = "HelLO, thIS IS A SEnTance oF SpONgEBoB mOcKInG yOu"
#' str_sentense(string)

str_sentence = function(string, noCaps = NULL){
  strList = stringr::str_split(string, pattern = " ")
  obs     = length(strList)
  buff    = character(obs)

  nocaps = c("in", "is","on", "at", "to", "from", "by",
             "and", "but", "or", "for", "nor", "a", "an", noCaps)

  for(j in 1:obs){
    len = length(strList[[j]])


    ##### Try to vectorize this eventually
    for(i in 1:len){
      strList[[j]][i] = tolower(strList[[j]][i])
      if(i != 1 & i != len & strList[[j]][i] %in% nocaps){
        next
      }else{
        strList[[j]][i] = stringr::str_replace(strList[[j]][i],
                                               "^[:alnum:]{1}", toupper)
      }
    }

    out = strList[[j]][1]

    if(len == 1){
      out
    }else{
      for(i in 2:len){
        out = stringr::str_c(out,strList[[j]][i], sep = " ")
      }
    }
  buff[j] = out
  }
  buff
}
