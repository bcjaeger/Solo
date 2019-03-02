
#' Generate a nice table with prevalence ratios
#' @inherit gee_work
#' @export
#' @importFrom mitml 'testEstimates'
#' @importFrom mice 'complete'

gee_work.mids <- function(
  data,
  model_formulas,
  exposure
){

  reference_level <- levels(data$data[[exposure]])[1]

  model_formulas %>% purrr::map(
    ~{

      ref <- dplyr::tibble(
        Effect = reference_level,
        prev_ratio=1, conf_lower=1, conf_upper=1, pval='--'
      )

      exp <- map(1:data$m, .f=function(action){
        mice::complete(data, action=action) %>%
          geepack::geeglm(
            formula = as.formula(.x),
            family = poisson,
            data = .,
            id = 1:nrow(.)
          )
      }) %>%
        mitml::testEstimates() %>%
        magrittr::use_series("estimates") %>%
        as_tibble(
          rownames = "Effect"
        ) %>%
        dplyr::rename(
          pval=`P(>|t|)`,
          Std.err = Std.Error
        ) %>%
        mutate(
          pval=ifelse(
            pval > 0.001,
            format(
              round(
                pval, digits = 3
              )
            ),
            "< 0.001"
          ),
          prev_ratio=exp(Estimate),
          conf_lower=exp(Estimate+qnorm(0.025)*Std.err),
          conf_upper=exp(Estimate+qnorm(0.975)*Std.err)
        ) %>%
        dplyr::select(
          Effect, prev_ratio, conf_lower, conf_upper, pval
        ) %>%
        dplyr::filter(
          grepl(exposure,Effect)
        ) %>%
        dplyr::mutate(
          Effect=gsub(exposure, "", Effect)
        )

      dplyr::bind_rows(ref,exp) %>%
        mutate(
          Effect = forcats::fct_inorder(f=Effect)
        )
    }
  )

}
