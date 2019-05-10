
#' set the units for a continuous variable
#' @param data a data frame containing the columns that will be labeled
#' @param ... a named vector. Names are variables, values are labels. For example, age = '10 years' indicates that the age variable is in 10 year units.
#' @export

set_variable_units <- function(data, ...){

  dots <- list(...)

  if (length(dots) > 0) {
    if (!all(names(dots) %in% names(data)))
      stop("some variables not found in .data")
    for (v in names(dots)){
      attr(data[[v]], 'units') <- dots[[v]]
    }
  }
  data

}
