
#' Generate a nice table with prevalence ratios
#' @param data a data frame
#' @param exposure a named character vector. Names are variable labels. Values are column names in the data.
#' @param outcome a named character vector. Names are outcome labels. Values are column names of the outcomes in the data.
#' @param control a list of character vectors. The ith item in the list should contain column names of variables that will be added as control variables for the ith model. Naming the character vectors will result in the names being used as labels for control variables in the footnote of the table.
#' @param include.unadjusted logical. Should unadjusted prevalence ratios be presented in the table?
#' @param include.descriptive logical. Should the prevalence of the outcome be presented in the table?
#' @param return_data logical. Should the table data be returned instead of the table?
#' @param collapse_footer logical. Should the model footnote be collapsed?
#' @export
#' @importFrom stats "as.formula" "complete.cases" "poisson" "qnorm"
#' @importFrom magrittr "set_names" "%>%" "%<>%"
#' @importFrom dplyr 'mutate' 'select' 'filter' 'bind_rows' 'everything' 'group_by' 'as_tibble'
#' @importFrom purrr 'map' 'map_lgl' 'map2' 'reduce'
#' @importFrom tidyr 'spread' 'gather'
#' @importFrom broom 'tidy'
#' @importFrom survival "Surv"

#' @examples
#' library(titanic)
#' library(magrittr)
#' library(dplyr)
#' library(gt)
#'
#' data <- titanic_train %>%
#'   dplyr::select(Survived, Pclass, Sex, Age, SibSp, Fare) %>%
#'   as_tibble() %>%
#'   dplyr::mutate(
#'     Pclass=factor(
#'       Pclass,
#'       levels=c(1,2,3),
#'       labels=c("1st Class","2nd Class","3rd Class")
#'     )
#'   ) %>%
#'   na.omit()
#'
#' tst_output <- pr_table(
#'   data=data,
#'   exposure=c("Passenger class"='Pclass'),
#'   outcome=c("Survival"='Survived'),
#'   control=list(
#'     c("Passenger gender"="Sex"),
#'     c("Passenger age"="Age",
#'       "Ticket price"="Fare"),
#'     c("Gender by age interaction"="Sex:Age")
#'   ),
#'   include.unadjusted=TRUE,
#'   include.descriptive=TRUE
#' )

pr_table <- function(
  data,
  exposure,
  outcome,
  control,
  include.unadjusted=TRUE,
  include.descriptive=TRUE,
  return_data=FALSE,
  collapse_footer = FALSE
){

  # Initialize for CRAN -----------------------------------------------------

  estimate = conf.low = conf.high = grp = term = . = NULL
  model = prev_ratio = p.value = table_value = tmp = NULL
  n = n_total = n_subs = n_cases = prevalence = NULL
  nsubs = table_values = table_part = table_row = NULL

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
  if(is.null(names(outcome))){
    warning("No names on outcome vector. Labels are set using names")
  }
  if(is.null(names(exposure))){
    warning("No names on exposure vector. Labels are set using names")
  }


  # Scrub inputs ------------------------------------------------------------

  # Identify all of the variables that are used for modeling
  all_vars <- scrub_variables(
      purrr::reduce(control, c),
      exposure,
      outcome
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
    descriptives <- compute_binary_descriptives(
      data = og_data,
      exposure = exposure,
      outcome = outcome
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
    control = control,
    collapse = collapse_footer
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

    outcome_label <- if(!is.null(names(outcome))){
      names(outcome)
    } else {
      outcome
    }

    exposure_label <- if(!is.null(names(exposure))){
      names(exposure)
    } else {
      exposure
    }

    model_table %<>%
      magrittr::set_names(
        gsub("table_row",outcome_label,names(.))
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
        columns=outcome_label
      )

    if(collapse_footer){

      model_table %<>%
        gt::tab_footnote(
          footnote=footnote,
          locations = gt::cells_data(
            columns=1,
            rows=which(.[[1]]==setdiff(names(model_formulas),"Unadjusted")[1])
          )
        )

    } else {

      for(i in 1:length(control)){
        model_table %<>%
          gt::tab_footnote(
            footnote = footnote[[i]][1],
            locations = gt::cells_data(
              columns = 1,
              rows = which(.[[1]]==setdiff(names(model_formulas),"Unadjusted")[i])
            )
          )
      }

    }

    model_table

  }

  # gt::tab_options(
  #   footnote.glyph=c("*, †, ‡, §, ||, ¶, #, **")
  # )
}
