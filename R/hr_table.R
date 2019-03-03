
#' Generate a publication-ready table with hazard ratios
#' @param data a data frame
#' @param exposure a named character vector. Names are variable labels. Values are column names in data.
#' @param time a named character vector. Names are labels for time variables. Values are column names of the time columns in data.
#' @param status a named character vector. Names are labels for status variables. Values are column names of the status columns in data.
#' @param control a list of character vectors. The ith item in the list should contain column names of variables that will be added as control variables for the ith model. Naming the character vectors will result in the names being used as labels for control variables in the footnote of the table.
#' @param include.unadjusted logical. Should unadjusted prevalence ratios be presented in the table?
#' @param include.descriptive logical. Should event rates be presented in the table?
#' @param person_years_denominator integer. Incidence rates will be computed per this number.
#' @param return_data logical. Should the table data be returned instead of the table?
#' @export

hr_table <- function(
  data,
  exposure,
  time,
  status,
  control,
  include.unadjusted=TRUE,
  include.descriptive=TRUE,
  person_years_denominator=1000,
  return_data=FALSE
){

  # Initialize for CRAN -----------------------------------------------------

  estimate = conf.low = conf.high = grp = term = . = NULL
  model = prev_ratio = p.value = table_value = tmp = NULL
  n_total = n_subs = n_cases = prevalence = outcome = NULL
  n = nsubs = table_values = table_part = table_row = NULL

  # Check inputs ------------------------------------------------------------

  if(!is.logical(include.unadjusted)){
    stop("include.unadjusted should equal TRUE or FALSE")
  }
  if(!is.logical(include.descriptive)){
    stop("include.descriptive should equal TRUE or FALSE")
  }
  if(!is.list(control)){
    stop("control should be a list, e.g. control=list(m1=c('x1'), m2=c('x2'))")
  }
  if(is.null(names(time))){
    warning("No names on time vector. Labels are set using names")
  }
  if(is.null(names(time))){
    warning("No names on status vector. Labels are set using names")
  }
  if(is.null(names(exposure))){
    warning("No names on exposure vector. Labels are set using names")
  }

  # Scrub inputs ------------------------------------------------------------

  # Identify all of the variables that are used for modeling
  all_vars <- scrub_variables(
    purrr::reduce(control, c),
    exposure,
    time,
    status
  )

  # check to make sure the data are complete for these variables,
  # and save the original (og) data for descriptive statistics
  og_data <- scrub_data(
    data = data,
    all_vars = all_vars,
    exposure = exposure,
    control = control
  )

  # the top rows of the table will contain descriptive statistics
  # if include.descriptive is set equal to TRUE
  if(include.descriptive){

    # descriptives are computed on original (not imputed) data
    descriptives <- compute_survival_descriptives(
      data = og_data,
      exposure = exposure,
      status = status,
      time = time,
      N = person_years_denominator
    )

  } else {

    # descriptives aren't computed
    descriptives = NULL

  }

  # Develop models ----------------------------------------------------------

  # Create a model predictors object
  ## translates control into a list of model predictors with simular names
  model_predictors <- gather_predictors(control = control)

  # A footnote goes under the table to indicate model structure
  footnote <- write_footer(
    model_predictors = model_predictors,
    control = control
  )

  if(include.unadjusted){
    # save the current names of model predictors
    current_names<-names(model_predictors)
    # include an unadjusted item in model predictors
    model_predictors$Unadjusted <- "1"
    # reorder the model predictors so that unadjusted estimates come first
    model_predictors <- model_predictors[c("Unadjusted",current_names)]
    # remove the old names, we just needed them for ordering
    rm(current_names)
  }

  # check the format of the exposure variable.
  if(is.character(og_data[[exposure]])){

    stop("Please convert exposure from character to factor")

  } else if(is.factor(og_data[[exposure]])){

    # if format is correct, set up model formulas for next step

    outcome = paste0("survival::Surv(",time,', ',status,')')

    model_formulas <- model_predictors %>%
      map(
        ~ paste(
          outcome, '~', exposure, '+', paste(., collapse=' + ')
        )
      )

  } else if(is.numeric(og_data[[exposure]])){

    if(length(unique(og_data[[exposure]]))<5){
      stop(paste("Please convert exposure to factor:",exposure))
    } else {
      stop("Please use the pr_figure() function for continuous exposures")
    }

  }

  model_table <- data %>%
    make_tidy_models(
      model_formulas = model_formulas,
      exposure = exposure
    ) %>%
    bind_models(
      model_formulas = model_formulas,
      descriptives = descriptives
    )

  if(return_data){

    return(model_table)

  } else {

    time_label <- if(!is.null(names(time))){
      names(time)
    } else {
      time
    }

    status_label <- if(!is.null(names(status))){
      names(status)
    } else {
      status
    }

    exposure_label <- if(!is.null(names(exposure))){
      names(exposure)
    } else {
      exposure
    }

    model_table %>%
      magrittr::set_names(
        gsub("table_row",status_label,names(.))
      ) %>%
      gt::gt() %>%
      gt::tab_spanner(
        label=exposure_label,
        columns=levels(og_data[[exposure]])
      ) %>%
      gt::cols_align(
        align='center',
        columns=levels(og_data[[exposure]])
      ) %>%
      gt::cols_align(
        align='left',
        columns=status_label
      ) %>%
      gt::tab_footnote(
        footnote=footnote,
        locations = gt::cells_data(
          columns=1,
          rows=if(include.descriptive) 4 else 1
        )
      )
  }

  # gt::tab_options(
  #   footnote.glyph=c("*, †, ‡, §, ||, ¶, #, **")
  # )
}
