
#' parse a formula and set its class to be solo compatible
#' @param formula a model formula
#' @param outcome the outcome variable
#' @export

parse_formula <- function(formula, outcome){

  .=NULL
  output = as.formula(formula)

  new_class=gsub("outcome", "formula", class(outcome), fixed=TRUE)

  class(output) %<>% c(new_class, .)
  output

}
