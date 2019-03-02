
#' Generate a nice table with prevalence ratios
#' @param model_formulas a list of model formulas
#' @param data a data frame or mids object
#' @param exposure a character indicating the column name of the exposure.
#'
#' @return a list of gee models
#' @export

gee_work <- function(
  data,
  model_formulas,
  exposure
){

  UseMethod("gee_work")

}

