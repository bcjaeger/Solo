
#' Generate a nice table with ratios
#' @inherit make_tidy_models
#' @export

make_tidy_models.data.frame <- function(
  data,
  model_formulas,
  exposure
){

  # Initialize for CRAN

  term = estimate = conf.low = conf.high = p.value = . = NULL

  model_formulas %>%
    purrr::map(
      .f = function(formula){

        reference_row <- dplyr::tibble(
          term = levels(data[[exposure]])[1],
          estimate=1, conf.low=1, conf.high=1, p.value='--'
        )

        exposure_rows <- as.formula(formula) %>%
          fit_model(data=data, tidy=TRUE) %>%
          dplyr::select(
            term,
            estimate,
            conf.low,
            conf.high,
            p.value
          ) %>%
          mutate(
            p.value=ifelse(
              p.value > 0.001,
              format(
                round(
                  p.value, digits = 3
                )
              ),
              "< 0.001"
            )
          ) %>%
          dplyr::mutate_at(
            .vars=c('estimate','conf.low','conf.high'),
            ~ exp(.x)
          ) %>%
          dplyr::filter(
            grepl(exposure,term)
          ) %>%
          dplyr::mutate(
            term=gsub(exposure, "", term)
          )

        reference_row %>%
          bind_rows(exposure_rows) %>%
          mutate(
            term = forcats::fct_inorder(f=term)
          )
      }
    )

}
