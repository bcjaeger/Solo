
#' print a solo object
#' @param x an object
#' @param ... arguments passed to other functions
#' @export

print.solo_object <- function(x, ...){

  obj <- x$analysis

  obj <- obj[,-which(base::substr(names(obj), start=0, stop=1)=='.')]

  print(obj)

}
