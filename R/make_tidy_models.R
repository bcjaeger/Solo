
#' Generate a nice table with prevalence ratios
#' @param model_formulas a list of model formulas
#' @param data a data frame or mids object
#' @param exposure a character indicating the column name of the exposure.
#'
#' @return a list of tidy model summaries
#' @export

make_tidy_models <- function(
  data,
  model_formulas,
  exposure
){

  UseMethod("make_tidy_models")

}

