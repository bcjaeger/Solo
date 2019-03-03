
#' turn a list of tidy models into a data frame of tidy models.
#' @param models the models
#' @param model_formulas formulas for the models
#' @param descriptives a tibble of descriptive statistics
#' @export
#'

bind_models <- function(models, model_formulas, descriptives=NULL){

  # For CRAN

  table_row = estimate = conf.low = conf.high = NULL
  p.value = term = table_part = table_value = NULL

  output <- models %>%
    bind_rows(.id='table_row') %>%
    mutate(
      table_row=factor(
        table_row,
        levels=names(model_formulas)
      ),
      table_value = paste0(
        adapt_round(estimate), ' (',
        adapt_round(conf.low), ', ',
        adapt_round(conf.high), ')'
      ),
      table_value = gsub(
        "1.00, 1.00",
        "ref",
        table_value
      ),
      table_value = gsub(
        "1.00 (ref)", "1 (ref)", table_value, fixed=TRUE
      )
    ) %>%
    dplyr::select(
      -c(
        estimate,
        conf.low,
        conf.high,
        p.value
      )
    ) %>%
    tidyr::spread(term, table_value)

  if(!is.null(descriptives)){

    output <- bind_rows(
      descriptives,
      dplyr::mutate_if(output, is.factor, as.character),
      .id='table_part'
    ) %>%
      mutate(
        table_row=factor(table_row),
        table_row=forcats::fct_inorder(table_row),
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

  output

}
