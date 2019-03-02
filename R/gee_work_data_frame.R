
#' Generate a nice table with prevalence ratios
#' @inherit gee_work
#' @export

gee_work.data.frame <- function(
  data,
  model_formulas,
  exposure
){

  reference_level <- levels(data[[exposure]])[1]

  model_formulas %>% purrr::map(
    ~{

      ref <- dplyr::tibble(
        Effect = reference_level,
        prev_ratio=1, conf_lower=1, conf_upper=1, pval='--'
      )

      exp <- as.formula(.x) %>%
        geepack::geeglm(
          family = poisson,
          data = data,
          id = 1:nrow(data)
        ) %>%
        summary() %>%
        magrittr::use_series("coefficients") %>%
        as_tibble(
          rownames = "Effect"
        ) %>%
        dplyr::rename(
          pval=`Pr(>|W|)`
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
