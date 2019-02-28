
#' helper function to list things.
#' @param x a vector of elements
#' @param end_string the character that precedes the last item in x
#' @param sep_string the string that links names to values
#' @param col_string the string that precedes items other than the first and last in x
#' @param use_names if names are used, the names of x are listed along with values in x
#' @param lower if true, then all characters are printed in lower case.
#' @return a string that lists the elements of x
#' @export

list_elements = function(
  x,
  end_string = 'and ',
  sep_string = '=',
  col_string = ', ',
  use_names=FALSE,
  lower=TRUE
){

  if(is.list(x)) x = purrr::reduce(x, c)

  length_x = length(x)

  if(use_names){
    if(is.numeric(x)){
      xx = paste(
        names(x),
        format(round(x,2), nsmall = 2),
        sep = sep_string
        )
    }
    if (is.character(x)){
      xx = if(lower) tolower(names(x)) else names(x)
    }
  } else {
    xx = x
  }

  if (length_x == 1){
    paste(xx)
  } else if (length_x == 2){
    paste0(xx[1], ' and ', xx[2])
  } else if (length_x >= 3){
    start = paste(xx[1:(length_x-1)], collapse = col_string)
    stop =  paste0(end_string, xx[length_x])
    paste(start, stop)
  }
}
