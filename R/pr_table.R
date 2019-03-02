
#' Generate a nice table with prevalence ratios
#' @param data a data frame
#' @param exposure a named character vector. Names are variable labels. Values are column names in the data.
#' @param outcome a named character vector. Names are outcome labels. Values are column names of the outcomes in the data.
#' @param control a list of character vectors. The ith item in the list should contain column names of variables that will be added as control variables for the ith model. Naming the character vectors will result in the names being used as labels for control variables in the footnote of the table.
#' @param include.unadjusted logical. Should unadjusted prevalence ratios be presented in the table?
#' @param include.descriptive logical. Should the prevalence of the outcome be presented in the table?
#' @export
#' @importFrom stats "as.formula" "complete.cases"
#' @importFrom magrittr "set_names" "%>%" "%<>%"
#' @importFrom dplyr 'mutate' 'select' 'filter' 'bind_rows' 'everything' 'group_by
#' @importFrom purrr 'map' 'map_lgl' 'map2' 'reduce'
#' @importFrom tidyr 'spread' 'gather'

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
  include.descriptive=TRUE
){

  # Initialize for CRAN -----------------------------------------------------

  Effect = conf_lower = conf_upper = grp = . = NULL
  model = prev_ratio = pval = table_value = tmp = NULL
  n = n_total = n_subs = n_cases = prevalence = NULL
  nsubs = table_values = table_part = NULL

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

  all_vars <-
    c(
      purrr::reduce(control, c),
      exposure,
      outcome
    ) %>%
    set_names(NULL) %>%
    paste(collapse=" + ") %>%
    paste("~", .) %>%
    as.formula() %>%
    all.vars()

  if(inherits(data,'data.frame')){

    nrows_with_complete_data <- data[, all_vars] %>%
      complete.cases() %>%
      sum()

    if(nrows_with_complete_data < nrow(data)) {
      stop("Missing values in control variables.")
    }

  }

  exposure_error <- control %>%
    map_lgl(
      ~ any(
        grepl(exposure, ., fixed=TRUE)
      )
    )

  if(any(exposure_error)){
    error_message <- paste0(
      "Exposure variable should not be in the control list.",
      "See control list item number",
      if(sum(exposure_error)>1) "s " else " ",
      list_elements(which(exposure_error))
    )
    stop(error_message)
  }

  og_data <- if (inherits(data, 'data.frame')){
    data
  } else if(inherits(data, 'mids')) {
    data$data
  }

  # Develop models ----------------------------------------------------------

  # Creat a model predictors object
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

  if(is.character(og_data[[exposure]])){

    stop("Please convert exposure from character to factor")

  } else if(is.factor(og_data[[exposure]])){

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

  models <- data %>%
    gee_work(
      model_formulas = model_formulas,
      exposure = exposure
    )

  exposure_quo <- rlang::enquo(exposure)
  outcome_quo <- rlang::enquo(outcome)

  descriptives <- og_data %>%
    dplyr::rename(
      tmp=!!outcome_quo,
      grp=!!exposure_quo
    ) %>%
    dplyr::select(tmp, grp) %>%
    dplyr::filter(
      !is.na(tmp),
      !is.na(grp)
    ) %>%
    dplyr::mutate(n_total=n()) %>%
    dplyr::group_by(grp) %>%
    dplyr::summarise(
      prevalence=paste0(adapt_round(100*mean(tmp)),"%"),
      n_cases = sum(tmp==1),
      n_subs = n(),
      n_total = n_total[1]
    ) %>%
    mutate(
      nsubs = paste0(
        n_subs, ' (', adapt_round(100 * n_subs / n_total), '%)'
      ),
      table_value = paste0(
        n_cases,' (',prevalence,')'
      )
    ) %>%
    dplyr::select(grp, nsubs, table_value) %>%
    tidyr::nest(nsubs, table_value, .key='table_values') %>%
    tidyr::spread(key=grp, value=table_values) %>%
    purrr::map(.f = unlist) %>%
    dplyr::bind_cols() %>%
    dplyr::mutate(
      model=c(
        'No. of participants (%)',
        'No. of cases (Prevalence)'
        )
    ) %>%
    dplyr::select(model, everything())

  model_tibble <- models %>%
    bind_rows(.id='model') %>%
    mutate(
      model=factor(
        model,
        levels=names(model_formulas)
      ),
      table_value = paste0(
        adapt_round(prev_ratio), ' (',
        adapt_round(conf_lower), ', ',
        adapt_round(conf_upper), ')'
      ),
      table_value = gsub(
        "1.00, 1.00",
        "ref",
        table_value
      )
    )

  model_table <- model_tibble %>%
    dplyr::select(-prev_ratio,-conf_lower,-conf_upper,-pval) %>%
    tidyr::spread(Effect, table_value)

  if(include.descriptive){
    model_table <- bind_rows(
      descriptives,
      dplyr::mutate_if(model_table, is.factor, as.character),
      .id='table_part'
    ) %>%
      mutate(
        model=factor(model),
        model=forcats::fct_inorder(model),
        table_part=factor(
          table_part,
          labels=c(
            "Descriptive statistics",
            "Prevalence ratios (95% CI)"
          )
        )
      ) %>%
      dplyr::group_by(table_part)
  }

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

  model_gtable <- model_table %>%
    magrittr::set_names(
      gsub("model",outcome_label,names(.))
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
    ) %>%
    gt::tab_footnote(
      footnote=footnote,
      locations = gt::cells_data(
        columns=1,
        rows=1+include.descriptive+include.unadjusted
      )
    )

  # gt::tab_options(
  #   footnote.glyph=c("*, †, ‡, §, ||, ¶, #, **")
  # )
}
