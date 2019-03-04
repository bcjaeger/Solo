
#' compute size and prevalence of a binary outcome for exposure groups
#' @param data a data set
#' @param exposure the exposure
#' @param outcome the outcome
#' @export

compute_binary_descriptives <- function(data, exposure, outcome){

  # For CRAN
  tmp = grp = n = n_total = n_subs = n_cases = prevalence = NULL
  nsubs = table_value = table_values = table_row = NULL

  data %>%
    dplyr::rename(
      tmp=!!rlang::enquo(outcome),
      grp=!!rlang::enquo(exposure)
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
      table_row = c(
        'No. of participants (%)',
        'No. of cases (Prevalence)'
      )
    ) %>%
    dplyr::select(table_row, everything())
}


