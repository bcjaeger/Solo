
#' Generate a nice table with prevalence ratios
#' @inherit make_tidy_models
#' @export
#' @importFrom mitml 'testEstimates'
#' @importFrom mice 'complete' 'pool'
#' @importFrom magrittr 'use_series'
#' @importFrom dplyr 'rename' 'tibble' 'as_tibble'

make_tidy_models.mids <- function(
  data,
  model_formulas,
  exposure
){

  `P(>|t|)` = Std.Error = Estimate = term = estimate = NULL
  Std.err = std.error = p.value = conf.low = conf.high = NULL

  model_formulas %>%
    purrr::map(
      .f = function(formula){

        reference_rows <- dplyr::tibble(
          term = levels(data$data[[exposure]])[1],
          estimate=1, conf.low=1, conf.high=1, p.value='--'
        )

        exposure_rows <- map(1:data$m, .f=function(action){

          as.formula(formula) %>%
            fit_model(
              data = mice::complete(data, action=action),
              tidy = FALSE
            )

        })

        if(inherits(exposure_rows[[1]], 'geeglm')){

          exposure_rows %<>%
            mitml::testEstimates() %>%
            magrittr::use_series("estimates") %>%
            dplyr::as_tibble(
              rownames = "term"
            ) %>%
            dplyr::rename(
              p.value=`P(>|t|)`,
              Std.err = Std.Error,
              estimate = Estimate
            ) %>%
            dplyr::select(term, estimate, Std.err, p.value)

        } else {

          # mice pool function gives spam warnings
          suppressWarnings(
            exposure_rows %<>%
              mice::pool() %>%
              summary() %>%
              as_tibble(
                rownames = 'term'
              ) %>%
              dplyr::select(
                term,
                estimate,
                Std.err=std.error,
                p.value
              )
          )
        }
        exposure_rows %<>%
          mutate(
            p.value=ifelse(
              p.value > 0.001,
              format(round(p.value, digits = 3)),
              "< 0.001"
            ),
            conf.low=exp(estimate+qnorm(0.025)*Std.err),
            conf.high=exp(estimate+qnorm(0.975)*Std.err),
            estimate=exp(estimate)
          ) %>%
          dplyr::select(
            term,
            estimate,
            conf.low,
            conf.high,
            p.value
          ) %>%
          dplyr::filter(
            grepl(exposure,term)
          ) %>%
          dplyr::mutate(
            term=gsub(exposure, "", term)
          )

        dplyr::bind_rows(
          reference_rows,
          exposure_rows
        ) %>%
          mutate(
            term = forcats::fct_inorder(f=term)
          )
      }
    )

}
