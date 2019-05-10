
#' compute descriptive statistics for a solo table
#' @param outcome  dependent variable
#' @param exposure exposure variable
#' @param data data frame
#' @param N denominator for incidence rate calculation
#' @importFrom purrr 'pluck' 'set_names' 'map2'
#' @importFrom dplyr 'group_by' 'everything'
#' @importFrom forcats 'fct_inorder'
#' @importFrom stats 'update' 'as.formula' 'qnorm' 'vcov' 'coef'
#' @importFrom tibble 'as_tibble' 'enframe' 'deframe'
#' @importFrom magrittr 'use_series'
#' @export

cmp_descr <- function(outcome, exposure, data, N){
  UseMethod('cmp_descr')
}

#' @describeIn cmp_descr compute descriptive statistics over a list
#' @inheritParams cmp_descr
#' @importFrom purrr 'pluck' 'set_names' 'map2'
#' @importFrom forcats 'fct_inorder'
#' @importFrom dplyr 'group_by' 'everything'
#' @importFrom stats 'update' 'as.formula' 'qnorm' 'vcov' 'coef'
#' @importFrom tibble 'as_tibble' 'enframe' 'deframe'
#' @importFrom magrittr 'use_series'
#' @export

cmp_descr.list <- function(outcome, exposure, data, N){

  pmap(
    list(outcome, exposure, data), cmp_descr, N=N
  )

}

#' @describeIn cmp_descr compute descriptive statistics for a relative risk analysis
#' @inheritParams cmp_descr
#' @importFrom purrr 'pluck' 'set_names' 'map2'
#' @importFrom forcats 'fct_inorder'
#' @importFrom dplyr 'group_by' 'everything'
#' @importFrom stats 'update' 'as.formula' 'qnorm' 'vcov' 'coef'
#' @importFrom tibble 'as_tibble' 'enframe' 'deframe'
#' @importFrom magrittr 'use_series'
#' @export

cmp_descr.rel_risk_outcome <-
  function(outcome, exposure, data, N)
  {

    # For CRAN
    table_value = variable = upr = lwr = est = Std.err = NULL
    Std.Error = Estimate = term = nsubs = n_subs = NULL
    n_total = time = status = . = n = events = NULL
    person_years = rate = conf.low = conf.high = NULL
    incidence_rate = table_values = table_row = NULL
    table_col = tmp = grp = n_cases = prevalence = term = NULL

    if(all(c("ieff","catg") %in% class(exposure))){
      data$orig$.solo_exposure = interaction(
        data$orig[, exposure$variables[1:2]],
        lex.order = TRUE,
        sep = '._solo_.'
      )
      grp_name = '.solo_exposure'
    } else{
      grp_name = exposure$term
    }

    output <- data$orig %>%
      dplyr::rename(
        tmp=!!outcome,
        grp=!!grp_name
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
      dplyr::filter(
        !is.na(grp)
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
          'No. of cases (prevalence)'
        )
      ) %>%
      dplyr::select(table_row, everything())

    class(output) <- c("descriptives", class(output))

    output

  }

#' @describeIn cmp_descr compute descriptive statistics for a binary outcome
#' @inheritParams cmp_descr
#' @importFrom purrr 'pluck' 'set_names' 'map2'
#' @importFrom forcats 'fct_inorder'
#' @importFrom dplyr 'group_by' 'everything'
#' @importFrom stats 'update' 'as.formula' 'qnorm' 'vcov' 'coef'
#' @importFrom tibble 'as_tibble' 'enframe' 'deframe'
#' @importFrom magrittr 'use_series'
#' @export

cmp_descr.prop_haz_outcome <-
  function(outcome, exposure, data, N)
  {
    # For CRAN
    table_value = variable = upr = lwr = est = Std.err = NULL
    Std.Error = Estimate = term = nsubs = n_subs = NULL
    n_total = time = status = . = n = events = NULL
    person_years = rate = conf.low = conf.high = NULL
    incidence_rate = table_values = table_row = NULL
    table_col = tmp = grp = n_cases = prevalence = term = NULL

    if(all(c("ieff","catg") %in% class(exposure))){
      data$orig$.solo_exposure = interaction(
        data$orig[, exposure$variables[1:2]],
        lex.order = TRUE,
        sep = '._solo_.'
      )
      grp_name = '.solo_exposure'
    } else{
      grp_name = exposure$term
    }

    outcome_parts <-
      regmatches(
        outcome,
        gregexpr("(?<=\\().*?(?=\\))", outcome, perl=T)
      ) %>%
      pluck(outcome) %>%
      strsplit(x = ., split = ',', fixed=TRUE) %>%
      unlist()

    output <- data$orig %>%
      dplyr::rename(
        exposure=!!grp_name,
        time=!!outcome_parts[1],
        status=!!outcome_parts[2]
      ) %>%
      dplyr::mutate(n_total=n()) %>%
      group_by(exposure) %>%
      dplyr::summarise(
        events = sum(status==1),
        person_years = sum(time),
        n_subs = n(),
        n_total = n_total[1],
        N = N
      ) %>%
      dplyr::filter(
        !is.na(exposure)
      ) %>%
      mutate(
        nsubs = paste0(
          n_subs, ' (', adapt_round(100 * n_subs / n_total), '%)'
        ),
        rate = N * events/person_years,
        conf.low = (N/person_years) * (events+stats::qnorm(0.025)*sqrt(events)),
        conf.high = (N/person_years) * (events+stats::qnorm(0.975)*sqrt(events)),
        person_years = format(round(person_years,0), big.mark = ','),
        incidence_rate =
          paste0(
            adapt_round(rate), ' (',
            adapt_round(conf.low), ', ',
            adapt_round(conf.high), ')'
          )
      ) %>%
      dplyr::select(
        exposure,
        nsubs,
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
          'No. of participants (%)',
          'No. of events',
          "Person-years",
          paste('Rate (95% CI) per', N, 'person-years')
        )
      ) %>%
      dplyr::select(table_row, everything())

    class(output) <- c("descriptives", class(output))

    output

  }



