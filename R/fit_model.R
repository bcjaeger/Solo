
#' fit a model with the given formula
#' @param formula a modeling formula
#' @param exposure the exposure variable
#' @param data a data frame for model fitting.
#' @param variables_to_test character vector of variables to test.
#' @param keep.model should the model object be kept?
#' @return a fitted model
#' @importFrom stats 'poisson' 'na.omit'
#' @export

# formula = analysis$.formula[[1]]
# exposure = analysis$.exposure[[1]]
# data = analysis$.dat[[1]]
# variables_to_test = 'Sex'
# keep.model = TRUE

fit_model <- function(
  formula,
  exposure,
  data,
  variables_to_test = NULL,
  keep.model=TRUE
){

  UseMethod("fit_model")

}

#' fit a model with the given formula
#' @inheritParams fit_model
#' @return a fitted model
#' @export


fit_model.list <- function(
  formula,
  exposure,
  data,
  variables_to_test = NULL,
  keep.model=TRUE
){

  pmap(
    list(formula, exposure, data),
    .f = fit_model,
    variables_to_test = variables_to_test,
    keep.model = keep.model
  )

}

#' fit a model with the given formula
#' @inheritParams fit_model
#' @return a fitted model
#' @export

fit_model.rel_risk_formula <- function(
  formula,
  exposure,
  data,
  variables_to_test = NULL,
  keep.model=TRUE
){

  variable = Estimate = solo_id = . = NULL

  nimpute=length(data$mult)

  if(nimpute > 0){

    model_fits = seq_along(1:nimpute) %>%
      map(.f=function(mi){

        model = geepack::geeglm(
          formula = formula,
          family = stats::poisson(link='log'),
          data = data$mult[[mi]],
          id = solo_id
        )

        covb=summary(model)$cov.scaled

        list(model=model,covb=covb)

      })

    model = model_fits %>%
      map(~.x$model)

    tests = test_hypotheses(
      model = model,
      exposure = exposure,
      data = data,
      variables_to_test = variables_to_test
    )

    betas <- model %>%
      mitml::testEstimates() %>%
      magrittr::use_series('estimate') %>%
      .[,'Estimate']

    covbs <- model_fits %>%
      purrr::map(~.x$covb) %>%
      purrr::reduce(`+`) %>%
      magrittr::divide_by(nimpute)

    structure(
      list(
        model=if(keep.model) model else NULL,
        main_exp=exposure$term,
        tests=tests,
        betas=betas,
        covbs=covbs
      ),
      class="rel_risk_fit"
    )

  } else {

    model = geepack::geeglm(
      formula = formula,
      family = stats::poisson(link='log'),
      data = data$orig,
      id = solo_id
    )

    tests = test_hypotheses(
      model = model,
      exposure = exposure,
      data = data,
      variables_to_test = variables_to_test
    )

    structure(
      list(
        model=if(keep.model) model else NULL,
        main_exp=exposure$term,
        tests=tests,
        betas=coef(model),
        covbs=summary(model)$cov.scaled
      ),
      class="rel_risk_fit"
    )

  }

}

#' fit a model with the given formula
#' @inheritParams fit_model
#' @return a fitted model
#' @export

fit_model.prop_haz_formula <- function(
  formula,
  exposure,
  data,
  variables_to_test = NULL,
  keep.model=TRUE
){

  variable = Estimate = solo_id = . = NULL

  nimpute=length(data$mult)

  if(nimpute > 0){

    #print(formula)

    model_fits = seq_along(1:nimpute) %>%
      map(.f=function(mi){

        model = survival::coxph(
          formula = formula,
          data = data$mult[[mi]],
        )

        covb=vcov(model)

        list(model=model,covb=covb)

      })

    model = model_fits %>%
      map(~.x$model)

    tests = test_hypotheses(
      model = model,
      exposure = exposure,
      data = data,
      variables_to_test = variables_to_test
    )

    betas <- model %>%
      mitml::testEstimates() %>%
      magrittr::use_series('estimate') %>%
      as_tibble(rownames = 'variable') %>%
      dplyr::select(variable, Estimate) %>%
      deframe()

    covbs <- model_fits %>%
      purrr::map(~.x$covb) %>%
      purrr::reduce(`+`) %>%
      magrittr::divide_by(nimpute)

    structure(
      list(
        model=if(keep.model) model else NULL,
        main_exp=exposure$term,
        tests=tests,
        betas=betas,
        covbs=covbs
      ),
      class="prop_haz_fit"
    )

  } else {

    model = survival::coxph(
      formula = formula,
      data = data$orig,
    )

    tests = test_hypotheses(
      model = model,
      exposure = exposure,
      data = data,
      variables_to_test = variables_to_test
    )

    structure(
      list(
        model=if(keep.model) model else NULL,
        main_exp=exposure$term,
        tests=tests,
        betas=coef(model),
        covbs=vcov(model)
      ),
      class="prop_haz_fit"
    )

  }

}



# fit_model <- function(formula, data, tidy, exposure, test.effects, is.mi){
#
#   # Initialize for CRAN
#
#   ids = estimate = std.error = NULL
#
#   formula_parts <- paste(formula)
#
#   if(grepl('Surv(',formula_parts[2],fixed=T)){
#
#     model = survival::coxph(
#       formula = formula,
#       data = data
#     )
#
#     if(tidy){
#       model = model %>%
#       broom::tidy()
#     }
#
#   } else {
#
#     data$ids <- 1:nrow(data)
#
#     model = geepack::geeglm(
#       formula = formula,
#       family = stats::poisson(link='log'),
#       data = data,
#       id = ids
#     )
#
#
#     variables_to_test <- c(exposure, test.effects)
#
#     if(!is.mi){
#
#       wald_tests <- variables_to_test %>%
#         map(
#           .f=function(.variable){
#
#             # .variable = variables_to_test[[1]]
#             model_terms <- paste(formula)[3] %>%
#               strsplit(
#                 split = ' + ',
#                 fixed=TRUE
#               ) %>%
#               .[[1]]
#
#             plus_or_minus <- if(.variable %in% model_terms) "-" else "+"
#
#             # Search model terms for any other instances of .variable
#
#             int1_index <- grepl(
#               pattern = paste0(":",.variable),
#               x = model_terms
#             )
#             int2_index <- grepl(
#               pattern = paste0(.variable,":"),
#               x = model_terms
#             )
#
#             # if any are found, include them in the test term
#
#             if(any(int1_index)){
#               .variable = c(.variable, model_terms[int1_index])
#             }
#
#             if(any(int2_index)){
#               .variable = c(.variable, model_terms[int2_index])
#             }
#
#             reduced_model <- model %>%
#               update(
#                 as.formula(
#                   paste(
#                     ". ~ .", plus_or_minus,
#                     paste(.variable, collapse=plus_or_minus)
#                   )
#                 )
#               )
#
#             anova(
#               model,
#               reduced_model
#             ) %>%
#               as_tibble() %>%
#               dplyr::rename(
#                 df=Df,
#                 statistic=X2,
#                 p.value=`P(>|Chi|)`
#               ) %>%
#               mutate(
#                 p.value = ifelse(
#                   p.value >= 0.001,
#                   format(round(p.value,3),nsmall=3),
#                   "< 0.001"
#                 )
#               )
#           }
#         ) %>%
#         dplyr::bind_rows(.id='term') %>%
#         dplyr::select(term, p.value)
#
#     }
#
#     if(tidy){
#       model = model %>%
#         broom::tidy() %>%
#         mutate(
#           conf.low=estimate+qnorm(0.025)*std.error,
#           conf.high=estimate+qnorm(0.975)*std.error
#         )
#     }
#
#   }
#
#   if(!is.mi){
#     list(
#       model = model,
#       wald_tests = wald_tests
#     )
#   } else {
#     model
#   }
#
# }


