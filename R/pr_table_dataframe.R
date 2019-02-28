
#' Generate a nice table with prevalence ratios
#' @inherit pr_table
#' @export

pr_table.data.frame <- function(
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

  nrows_with_complete_data <- data[, all_vars] %>%
    complete.cases() %>%
    sum()

  if(nrows_with_complete_data < nrow(data)) {
    stop("Missing values in control variables.")
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

  model_formulas <- model_predictors %>%
    map(
      ~ paste(
        outcome, '~', exposure, '+', paste(., collapse=' + ')
      )
    )

  models <- model_formulas %>%
    gee_work(
      data=data,
      exposure=exposure
    )

  exposure_quo <- rlang::enquo(exposure)
  outcome_quo <- rlang::enquo(outcome)

  prevalences <- data %>%
    dplyr::rename(
      tmp=!!outcome_quo,
      grp=!!exposure_quo
    ) %>%
    dplyr::group_by(grp) %>%
    dplyr::summarise(
      prev_ratio=adapt_round(mean(tmp))
    ) %>%
    tidyr::spread(grp, prev_ratio) %>%
    dplyr::mutate(model='Prevalence') %>%
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
      prevalences,
      dplyr:: mutate_if(model_table, is.factor, as.character)
    ) %>%
      mutate(
        model=factor(model),
        model=forcats::fct_inorder(model)
      )
  }

  model_gtable <- model_table %>%
    magrittr::set_names(
      gsub("model"," ",names(.))
    ) %>%
    gt::gt() %>%
    gt::tab_spanner(
      label=exposure,
      columns=levels(data[[exposure]])
    ) %>%
    gt::cols_align(
      align='center',
      columns=levels(data[[exposure]])
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
