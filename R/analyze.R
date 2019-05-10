
# analyze

trim_list <- function(x, rmv_inner_space=TRUE){
  x <- trimws(unlist(x))
  if(rmv_inner_space){
    x <- gsub(pattern = " ", replacement = "", x = x, fixed = TRUE)
  }
  x
}

split_surv <- function(surv_string, grab_at=2){

  .=NULL

  regmatches(
    surv_string,
    gregexpr("(?<=\\().*?(?=\\))", surv_string, perl=T)
  ) %>%
    unlist() %>%
    strsplit(x = ., split = ',', fixed=TRUE) %>%
    map_chr(~.x[grab_at])
}

prep_data <- function(x){
  as_tibble(x) %>%
    mutate(solo_id=1:nrow(x))
}

grab_at <- function(string, split, grab){

  strsplit(string, split=split, fixed=TRUE) %>%
    map_chr(~.x[grab])

}

grab_last <- function(string, split){

  .=NULL

  strsplit(string, split=split, fixed=TRUE) %>%
    unlist() %>%
    .[length(.)]

}

#' conduct an analysis of binary or survival outcomes
#' @param data_orig the unimputed data. This is all you need if your data don't have missing values.
#' @param data_mult a list of imputed data. This is needed if the original data have missing values.
#' @param formula a formula object with outcomes on the left hand side and main exposures on the right hand side.
#' @param control a list of formulas with each item in the list representing a new control model and containing a function with control variables on the right hand side (see examples).
#' @param nested_models should the models specified in control be nested?
#' @param analysis_type one of 'rel_risk' or 'prop_haz'.
#' @param include.unadjusted should an unadjusted model be included?
#' @param variables_to_test a character vector of variables that should be tested in addition to the main exposure.
#' @param person_years only relevant if the outcome is time-to-event. The denominator for incidence rates. Default is incidence rate per 1000 person-years.
#' @importFrom stats 'complete.cases'
#' @importFrom purrr 'map' 'pmap' 'reduce'
#' @importFrom magrittr '%>%' '%<>%'
#' @importFrom dplyr mutate
#' @export

analyze <- function(
  data_orig,
  data_mult=NULL,
  formula,
  control,
  nested_models=TRUE,
  analysis_type = NULL,
  include.unadjusted = TRUE,
  variables_to_test = NULL,
  person_years = 1000
){
  # Checking inputs ---------------------------------------------------------

  # for CRAN
  name = role = type = label = unit = model_fits = NULL
  outcome = exposure = .formula = .exposure = .x = NULL

  analysis_variables <- c(
    all.vars(formula),
    purrr::reduce(purrr::map(control, all.vars),base::c)
  )

  analysis_char_vars <- analysis_variables %>%
    purrr::set_names() %>%
    purrr::map_lgl(
    ~is.character(data_orig[[.x]])
  ) %>%
    which() %>%
    names()

  if(length(analysis_char_vars) > 0){

    warn_msg <- paste(
      "The following character variables are being converted to factors:",
      paste(analysis_char_vars, collapse = ' -- '),
      "Please convert these variables to factors in the original data",
      sep = '\n'
    )

    warning(warn_msg)

    data_orig %<>%
      mutate_at(analysis_char_vars, ~as.factor(.x))

    if(!is.null(data_mult)){
      data_mult %<>%
        map(
          .f=function(mi_data){
            mi_data %<>%
              mutate_at(analysis_char_vars, ~as.factor(.x))
          }
        )
    }



  }


  if(is.null(data_mult)){

    # Check the inputs

    n_rows_complete <- sum(complete.cases(data_orig[, analysis_variables]))

    n_rows_missing <- nrow(data_orig) - n_rows_complete

    if(n_rows_missing > 0){
      stop(
        paste("data_orig contains missing values. \n Either \n",
              "  1. Supply imputed data using data_mult \n OR \n",
              "  2. Remove missing rows from data_orig.")
      )
    }

  }

  if(analysis_type=='log_regr'){
    stop(paste(analysis_type, "is not available yet"))
  }

  analysis_type = match.arg(
    analysis_type,
    choices = c("rel_risk","log_regr","prop_haz")
  )

  # Set up data object
  .dat <- list(
    orig = prep_data(data_orig),
    mult = map(data_mult, prep_data)
  )

  # Formula parsing ---------------------------------------------------------

  formula_parts <-
    base::paste(formula[-1]) %>%
    purrr::map(~trim_list(strsplit(.x, '+', fixed=TRUE))) %>%
    purrr::set_names('outcome', 'exposure')

  formula_variables <-
    purrr::map(formula[-1], all.vars) %>%
    purrr::set_names(names(formula_parts)) %>%
    purrr::list_modify(control=trim_list(map(control,all.vars)))

  model_predictors <-
    gather_predictors(
      control = control,
      data = .dat$orig,
      nested_models = nested_models
    )


  # Check the format of outcomes --------------------------------------------

  if(analysis_type=='rel_risk'){

    for(.outcome in formula_variables$outcome){

      right_format <- !is.factor(.dat$orig[[.outcome]])

      if(!right_format){
        stop(
          paste(
            "Cannot proceed with relative risk analysis: \n",
            "Outcome variables should have a numeric class (i.e., integer) \n",
            "Check the following outcome variable in your data:", .outcome
          )
        )
      }

      right_unis <- all(unique(.dat$orig[[.outcome]]) %in% c(0,1))

      if(!right_unis){
        stop(
          paste(
            "Outcome variables should take values of 0 or 1 \n",
            "Check the following outcome variable in your data:", .outcome
          )
        )
      }

    }
  }

  # Variable labels ---------------------------------------------------------

  variables <-
    base::names(data_orig) %>%
    purrr::set_names() %>%
    purrr::map_chr(
      ~{
        lab=labelled::var_label(data_orig[[.x]])
        if(is.null(lab)) .x else lab
      }
    ) %>%
    tibble::enframe(value='label') %>%
    dplyr::mutate(
      type=purrr::map_chr(name,~base::class(.dat$orig[[.x]])),
      unit=purrr::map_chr(
        name,
        ~ {
          if(!is.null(attr(.dat$orig[[.x]],'units'))){
            attr(.dat$orig[[.x]],'units')
          } else {
            NA_character_
          }
        }
      ),
      role = dplyr::case_when(
        name %in% formula_variables$outcome~'outcome',
        name %in% formula_variables$exposure~'exposure',
        name %in% formula_variables$control~'control',
        TRUE ~ "none specified"
      ),
      role = base::factor(
        role, levels = c(
          'outcome','exposure','control','none specified'
        )
      )
    ) %>%
    dplyr::select(name, role, type, label, unit) %>%
    dplyr::arrange(role)

  # Model footnotes ---------------------------------------------------------

  footnote <- vector(
    mode='list',
    length=length(model_predictors)
  )

  mp_labels <- map(model_predictors, names)

  for(i in 1:length(footnote)){

    if(i==1 | !nested_models){
      footnote[[i]] = paste0(
        names(mp_labels)[i],
        ' includes adjustment for ',
        list_elements(mp_labels[[i]])
      )

    } else {

      footnote[[i]]= paste0(
        names(mp_labels)[i],
        ' includes ',
        names(mp_labels)[i-1],
        ' with additional adjustment for ',
        list_elements(
          setdiff(mp_labels[[i]], mp_labels[[i-1]])
        )
      )

    }
  }

  # Analysis ----------------------------------------------------------------

  if(include.unadjusted){
    # save the current names of model predictors
    current_names<-names(model_predictors)
    # include an unadjusted item in model predictors
    model_predictors$Unadjusted <- "1"
    # reorder the model predictors so that unadjusted estimates come first
    model_predictors <-
      model_predictors[c("Unadjusted",current_names)]
    # remove the old names, we just needed them for ordering
    rm(current_names)
  }

  # .control contains
  ## term: representation in model formulas
  ## variables: columns in data_orig that describe this element

  ## EXPOSURE CLASSES:

  ### Variable types
  ### continous  : ctns
  ### categorical: catg

  #### Effect types
  #### main effect: meff
  #### interaction: ieff
  #### spline     : seff (continuous only)

  ##### special cases:
  ##### interaction between ctns/catg: ctcg

  # this object only stores class/label info
  # it does not organize model variables
  # (that is what .formula will do)

  .control <- map(
    control, ~ trim_list(
      strsplit(paste(.x)[-1], split = '+', fixed=TRUE)
    )
  ) %>%
    # cant use map_chr b/c some control have length > 1
    reduce(base::c) %>%
    map(set_exposure_class, data=.dat$orig)

  analysis <-
    purrr::set_names(formula_parts$outcome) %>%
    purrr::map(
      .f=function(.outcome){
        purrr::set_names(formula_parts$exposure) %>%
          purrr::map(
            .f=function(.exposure){
              purrr::map_chr(
                model_predictors,
                .f=function(.predictors){
                  base::paste(
                    .outcome, '~',
                    base::paste(.exposure, collapse = ' + '), '+',
                    base::paste(.predictors, collapse=' + ')
                  )
                }
              ) %>%
                tibble::enframe(name='formula',value='.formula')
            }) %>%
          dplyr::bind_rows(.id='exposure')
      }) %>%
    dplyr::bind_rows(.id='outcome') %>%
    dplyr::mutate(
      .outcome = map(
        outcome,
        set_outcome_class,
        new_class = analysis_type,
        data = .dat$orig
      ),
      .exposure = map(
        exposure,
        set_exposure_class,
        data = .dat$orig
      ),
      .formula = pmap(
        list(.formula, .outcome),
        parse_formula
      )
    ) %>%
    dplyr::select(
      outcome, .outcome,
      exposure, .exposure,
      formula, .formula
    ) %>%
    dplyr::mutate_at(
      c('outcome','exposure','formula'),
      ~ fct_inorder(.x)
    )

  analysis$.dat=list(.dat)
  analysis$.control = list(.control)

  # these are for debugging
  # formula = analysis$.formula[[1]]
  # outcome = analysis$.outcome[[1]]
  # exposure = analysis$.exposure[[1]]
  # data = analysis$.dat[[1]]
  # fit = analysis$model_fits[[1]]
  # lst = analysis$.control[[1]]
  # exposure=lst[[2]]

  analysis %<>%
    mutate(
      desc_stats = cmp_descr(.outcome, .exposure, .dat, N=person_years),
      model_fits = fit_model(.formula, .exposure, .dat, variables_to_test),
      main_estms = cmp_estms(model_fits, .exposure, .dat),
      supp_estms = pmap(
        list(model_fits, .control, .dat),
        .f = function(fit, lst, data){
          map(
            lst, .f=function(.exp){
              cmp_estms(fit=fit, exposure=.exp, data=data)
            }
          )
        }
      )
    )

  output <- list(
    analysis = analysis,
    labels = list(variables=variables,footnote=footnote),
    type = analysis_type
  )

  class(output) <- c("solo_object", class(output))

  output


}
