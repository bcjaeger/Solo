#' compute descriptive statistics for survival outcomes
#' @param data a data frame
#' @param exposure a character vector
#' @param status a character vector
#' @param time a character
#' @param N the denominator for rates, e.g., rate per N=1000 person-years
#' @export

compute_survival_descriptives <- function(data, exposure, status, time, N=1000)
{

  # For CRAN
  n = events = person_years = rate = conf.low = conf.high = NULL
  incidence_rate = table_values = table_row = NULL

  data %<>%
    dplyr::rename(
      exposure=!!rlang::enquo(exposure),
      time=!!rlang::enquo(time),
      status=!!rlang::enquo(status)
    )

  data %>% group_by(exposure) %>%
    dplyr::summarise(
      events = sum(status==1),
      person_years = sum(time),
      n = n(),
      N = N
    ) %>%
    mutate(
      rate = N * events/person_years,
      conf.low = (N/person_years) * (events+stats::qnorm(0.025)*sqrt(events)),
      conf.high = (N/person_years) * (events+stats::qnorm(0.975)*sqrt(events)),
      person_years = format(person_years, big.mark = ','),
      incidence_rate =
        paste0(
          adapt_round(100 * rate), ' (',
          adapt_round(100 * conf.low), ', ',
          adapt_round(100 * conf.high), ')'
        )
    ) %>%
    dplyr::select(
      exposure,
      events,
      person_years,
      incidence_rate
    ) %>%
    tidyr::nest(-exposure, .key='table_values') %>%
    tidyr::spread(key=exposure, value=table_values) %>%
    purrr::map(.f = unlist) %>%
    dplyr::bind_cols()%>%
    dplyr::mutate(
      table_row = c(
        'No. of events',
        "Person-years",
        paste('Rate (95% CI) per', N, 'person-years')
      )
    ) %>%
    dplyr::select(table_row, everything())

}
