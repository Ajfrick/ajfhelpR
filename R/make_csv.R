#' write.csv with more useful Defaults
#' @param data data frame to output to csv
#' @param file name of the file to write to
#' @description Replicates functionality of write.csv but with default options
#' na = ", row.names = F, and quote = F
#'
#' Currently the function just calls write.csv with desired default options set
#' @return file
#' @examples
#' coming soon

make_csv = function(data, file ){
  write.csv(x = data, file = file, na = "",
            row.names = F,quote = F)
}

