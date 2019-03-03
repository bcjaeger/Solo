
#' modeling function for hazard ratios
#' @param formula a modeling formula
#' @param data a data frame for model fitting.
#' @param tidy logical. Should the model be tidied before it is returned?
#' @return a fitted proportional hazards model
#' @export

fit_model <- function(formula, data, tidy){

  # Initialize for CRAN

  ids = estimate = std.error = NULL

  formula_parts <- paste(formula)

  if(grepl('Surv(',formula_parts[2],fixed=T)){

    model = survival::coxph(
      formula = formula,
      data = data
    )

    if(tidy){
      model = model %>%
      broom::tidy()
    }

  } else {

    data$ids <- 1:nrow(data)

    model = geepack::geeglm(
      formula = formula,
      family = poisson,
      data = data,
      id = ids
    )

    if(tidy){
      model = model %>%
        broom::tidy() %>%
        mutate(
          conf.low=estimate+qnorm(0.025)*std.error,
          conf.high=estimate+qnorm(0.975)*std.error
        )
    }

  }

  model

}


