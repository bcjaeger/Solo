#' modeling function for hazard ratios
#' @param fit a fitted model
#' @param exposure the exposure variable
#' @param data a data frame for model fitting.
#' @return model estimates
#' @export

cmp_estms <- function(fit, exposure, data){
  UseMethod('cmp_estms')
}

#' modeling function for hazard ratios
#' @inheritParams cmp_estms
#' @return model estimates
#' @export

cmp_estms.list <- function(fit, exposure, data){

  pmap(
    list(fit,exposure,data), cmp_estms
  )

}

#' modeling function for hazard ratios
#' @inheritParams cmp_estms
#' @return model estimates
#' @export


cmp_estms.rel_risk_fit <- function(fit, exposure, data){

  # For CRAN
  term = Estimate = . = Std.Error = Std.err = est = lwr = upr = NULL
  variable = table_value = std.error = table_col = NULL

  betas=fit$betas
  covbs=fit$covbs

  terms = strsplit(exposure$term, "*", fixed=TRUE) %>%
    unlist() %>%
    trimws() %>%
    set_names(
      paste(.)
    )

  # continuous exposure, no interaction term
  if(all(c("meff","ctns") %in% class(exposure))){

    if(!(terms%in%names(betas))){

      # The control variable was not in this model
      # so we will re-fit the model without the main
      # exposure in order to get an estimate of this
      # control variable, adjusting for whatever is
      # part of the current model.

      # If multiple imputation was used,

      if(length(data$mult) > 1){
        numeric <- map2(
          fit$model, data$mult,
          .f = function(.model, .data){
            update(
              .model,
              formula. = as.formula(
                paste(
                  ".~.-",fit$main_exp,'+',terms
                )
              ),
              data = .data
            )
          }
        ) %>%
          mitml::testEstimates() %>%
          magrittr::use_series('estimate') %>%
          .[,c('Estimate','Std.Error')] %>%
          as_tibble(rownames = 'term') %>%
          dplyr::filter(term==exposure$term) %>%
          dplyr::rename(est=Estimate, std.err=Std.Error)

      } else {

        numeric <- update(
          fit$model,
          formula. = as.formula(
            paste(
              ".~.-",fit$main_exp,'+',terms
            )
          ),
          data = data$orig
        ) %>%
          summary() %>%
          use_series('coef') %>%
          .[,c("Estimate", "Std.err")] %>%
          as_tibble(rownames = 'term') %>%
          dplyr::filter(term==exposure$term) %>%
          dplyr::rename(est=Estimate, std.err=Std.err)
      }

       numeric %<>%
        mutate(
          lwr = exp(est + qnorm(0.025)*std.err),
          upr = exp(est + qnorm(0.975)*std.err),
          est = exp(est),
          variable = names(exposure$variables),
          table_value = paste0(
            adapt_round(est), ' (',
            adapt_round(lwr), ', ',
            adapt_round(upr), ')'
          )
        ) %>%
        dplyr::select(
          term, variable, est, lwr, upr, table_value
        )

      return(
        list(
          numeric = numeric,
          tabular = NULL
        )
      )

    }

    term_indx <- which(names(betas)==terms)
    std.err <- sqrt(covbs[term_indx, term_indx])

    numeric <- betas[term_indx] %>%
      enframe(name='term', value = 'est') %>%
      mutate(
        lwr = exp(est + qnorm(0.025)*std.err),
        upr = exp(est + qnorm(0.975)*std.err),
        est = exp(est),
        variable = names(exposure$variables),
        table_value = paste0(
          adapt_round(est), ' (',
          adapt_round(lwr), ', ',
          adapt_round(upr), ')'
        )
      ) %>%
      dplyr::select(
        term, variable, dplyr::everything()
      )

    # for consistency with other types of inputs
    output <- list(
      numeric=numeric,
      tabular=NULL
    )

  } else if(inherits(exposure, "catg")){

    term_names <- terms %>%
      map(
        ~paste0(.x,levels(data$orig[[.x]]))
      ) %>%
      reduce(base::c)

    if(!any(term_names%in%names(betas))){

      # The control variable was not in this model
      # so we will re-fit the model without the main
      # exposure in order to get an estimate of this
      # control variable, adjusting for whatever is
      # part of the current model.

      # If multiple imputation was used,

      if(length(data$mult) > 1){

        new_fits <- map2(
          fit$model, data$mult,
          .f = function(.model, .data){
            model = update(
              .model,
              formula. = as.formula(
                paste(
                  ".~.-",fit$main_exp,'+',exposure$term
                )
              ),
              data = .data
            )
            covb = summary(model)$cov.scaled
            list(model=model,covb=covb)
          }
        )

        model = map(new_fits, ~.x$model)

        covbs <- new_fits %>%
          purrr::map(~.x$covb) %>%
          purrr::reduce(`+`) %>%
          magrittr::divide_by(length(data$mult))

        betas <- model %>%
          mitml::testEstimates() %>%
          magrittr::use_series('estimate') %>%
          .[,'Estimate']

      } else {

        model <- update(
          fit$model,
          formula. = as.formula(
            paste(
              ".~.-",fit$main_exp,'+',exposure$term
            )
          ),
          data = data$orig
        )

        betas=coef(model)
        covbs=summary(model)$cov.scaled

      }

    }

    estimates <- map(
      terms, ~levels(data$orig[[.]])
    ) %>%
      expand.grid(stringsAsFactors = FALSE)

    estimates$est = 0
    estimates$std.error = 0

    for(i in 1:nrow(estimates)){

      variables <- c(
        paste0(terms, estimates[i,terms])
      ) %>%
        c(paste(., collapse=':')) %>%
        unique()

      indx <- which(names(betas) %in% variables)
      nrep <- length(indx)

      estimates$std.error[i] <- rep(1,nrep) %>%
        magrittr::multiply_by_matrix(covbs[indx,indx]) %>%
        magrittr::multiply_by_matrix(rep(1,nrep)) %>%
        magrittr::raise_to_power(1/2)

      estimates$est[i] <- betas %>%
        magrittr::extract(indx) %>%
        sum()

    }

    numeric_output <- as_tibble(estimates) %>%
      mutate(
        lwr=exp(est+stats::qnorm(0.025)*std.error),
        upr=exp(est+stats::qnorm(0.975)*std.error),
        est=exp(est),
        table_value = paste0(
          adapt_round(est), ' (',
          adapt_round(lwr), ', ',
          adapt_round(upr), ')'
        ),
        table_value = gsub(
          "1.00, 1.00",
          "ref",
          table_value
        ),
        table_value = gsub(
          "1.00 (ref)", "1 (ref)", table_value, fixed=TRUE
        )
      )

    tabular_output <- numeric_output %>%
      dplyr::select(
        !!terms, table_value
      ) %>%
      dplyr::mutate_at(.vars=terms, ~fct_inorder(.x)) %>%
      dplyr::arrange_at(.vars=terms) %>%
      tidyr::unite(-table_value, col='table_col', sep='._solo_.') %>%
      dplyr::mutate(table_col = fct_inorder(table_col)) %>%
      tidyr::spread(table_col, table_value)

    if(all(c("catg","meff") %in% class(exposure))){

      numeric_output %<>%
        dplyr::mutate(term = terms) %>%
        dplyr::rename(variable = !!terms) %>%
        dplyr::select(term, variable, everything(), -std.error)

      if(nrow(numeric_output)==2){
        # When categorical variables have only 2 levels,
        # only the exposure group needs to be specified
        numeric_output=numeric_output[-1,]
      }

    } else {

      numeric_output %<>%
        dplyr::mutate(term = exposure$term) %>%
        tidyr::unite(col = "variable", !!terms, sep = '._solo_.') %>%
        dplyr::select(term, variable, everything(), -std.error)

    }

    output <- list(
      numeric = numeric_output,
      tabular = tabular_output
    )

  }



  class(output) <- c("model_estimates", class(output))

  output

}

#' modeling function for hazard ratios
#' @inheritParams cmp_estms
#' @return model estimates
#' @export


cmp_estms.prop_haz_fit <- function(fit, exposure, data){

  #print(exposure$term)

  # For CRAN
  term = Estimate = . = Std.Error = Std.err = est = lwr = upr = NULL
  variable = table_value = std.error = table_col = `se(coef)` = NULL

  betas=fit$betas
  covbs=fit$covbs

  terms = strsplit(exposure$term, "*", fixed=TRUE) %>%
    unlist() %>%
    trimws() %>%
    set_names(
      paste(.)
    )

  # continuous exposure, no interaction term
  if(all(c("meff","ctns") %in% class(exposure))){

    if(!(terms%in%names(betas))){

      # The control variable was not in this model
      # so we will re-fit the model without the main
      # exposure in order to get an estimate of this
      # control variable, adjusting for whatever is
      # part of the current model.

      # If multiple imputation was used,

      if(length(data$mult) > 1){
        numeric <- map2(
          fit$model, data$mult,
          .f = function(.model, .data){
            update(
              .model,
              formula. = as.formula(
                paste(
                  ".~.-",fit$main_exp,'+',terms
                )
              ),
              data = .data
            )
          }
        ) %>%
          mitml::testEstimates() %>%
          magrittr::use_series('estimate') %>%
          as_tibble(rownames='term') %>%
          dplyr::select(term, Estimate, Std.Error) %>%
          dplyr::filter(term==exposure$term) %>%
          dplyr::rename(est=Estimate, std.err=Std.Error)

      } else {

        numeric <- update(
          fit$model,
          formula. = as.formula(
            paste(
              ".~.-",fit$main_exp,'+',terms
            )
          ),
          data = data$orig
        ) %>%
          summary() %>%
          use_series('coef') %>%
          as_tibble(rownames = 'term') %>%
          dplyr::select(term, est = coef, std.err = `se(coef)`) %>%
          dplyr::filter(term==exposure$term)

      }

      numeric %<>%
        mutate(
          lwr = exp(est + qnorm(0.025)*std.err),
          upr = exp(est + qnorm(0.975)*std.err),
          est = exp(est),
          variable = names(exposure$variables),
          table_value = paste0(
            adapt_round(est), ' (',
            adapt_round(lwr), ', ',
            adapt_round(upr), ')'
          )
        ) %>%
        dplyr::select(
          term, variable, est, lwr, upr, table_value
        )

      return(
        list(
          numeric = numeric,
          tabular = NULL
        )
      )

    }

    term_indx <- which(names(betas)==terms)
    std.err <- sqrt(covbs[term_indx, term_indx])

    numeric <- betas[term_indx] %>%
      enframe(name='term', value = 'est') %>%
      mutate(
        lwr = exp(est + qnorm(0.025)*std.err),
        upr = exp(est + qnorm(0.975)*std.err),
        est = exp(est),
        variable = names(exposure$variables),
        table_value = paste0(
          adapt_round(est), ' (',
          adapt_round(lwr), ', ',
          adapt_round(upr), ')'
        )
      ) %>%
      dplyr::select(
        term, variable, dplyr::everything()
      )

    # for consistency with other types of inputs
    output <- list(
      numeric=numeric,
      tabular=NULL
    )

  } else if(inherits(exposure, "catg")){

    term_names <- terms %>%
      map(
        ~paste0(.x,levels(data$orig[[.x]]))
      ) %>%
      reduce(base::c)

    if(!any(term_names%in%names(betas))){

      # The control variable was not in this model
      # so we will re-fit the model without the main
      # exposure in order to get an estimate of this
      # control variable, adjusting for whatever is
      # part of the current model.

      # If multiple imputation was used,

      if(length(data$mult) > 1){

        new_fits <- map2(
          fit$model, data$mult,
          .f = function(.model, .data){
            model = update(
              .model,
              formula. = as.formula(
                paste(
                  ".~.-",fit$main_exp,'+',exposure$term
                )
              ),
              data = .data
            )
            covb = vcov(model)
            list(model=model,covb=covb)
          }
        )

        model = map(new_fits, ~.x$model)

        covbs <- new_fits %>%
          purrr::map(~.x$covb) %>%
          purrr::reduce(`+`) %>%
          magrittr::divide_by(length(data$mult))

        betas <- model %>%
          mitml::testEstimates() %>%
          magrittr::use_series('estimate') %>%
          as_tibble(rownames = 'variable') %>%
          dplyr::select(variable, Estimate) %>%
          deframe()

      } else {

        model <- update(
          fit$model,
          formula. = as.formula(
            paste(
              ".~.-",fit$main_exp,'+',exposure$term
            )
          ),
          data = data$orig
        )

        betas=coef(model)
        covbs=vcov(model)

      }

    }

    estimates <- map(
      terms, ~levels(data$orig[[.]])
    ) %>%
      expand.grid(stringsAsFactors = FALSE)

    estimates$est = 0
    estimates$std.error = 0

    for(i in 1:nrow(estimates)){

      variables <- c(
        paste0(terms, estimates[i,terms])
      ) %>%
        c(paste(., collapse=':')) %>%
        unique()

      indx <- which(names(betas) %in% variables)
      nrep <- length(indx)

      estimates$std.error[i] <- rep(1,nrep) %>%
        magrittr::multiply_by_matrix(covbs[indx,indx]) %>%
        magrittr::multiply_by_matrix(rep(1,nrep)) %>%
        magrittr::raise_to_power(1/2)

      estimates$est[i] <- betas %>%
        magrittr::extract(indx) %>%
        sum()

    }

    numeric_output <- as_tibble(estimates) %>%
      mutate(
        lwr=exp(est+stats::qnorm(0.025)*std.error),
        upr=exp(est+stats::qnorm(0.975)*std.error),
        est=exp(est),
        table_value = paste0(
          adapt_round(est), ' (',
          adapt_round(lwr), ', ',
          adapt_round(upr), ')'
        ),
        table_value = gsub(
          "1.00, 1.00",
          "ref",
          table_value
        ),
        table_value = gsub(
          "1.00 (ref)", "1 (ref)", table_value, fixed=TRUE
        )
      )

    tabular_output <- numeric_output %>%
      dplyr::select(
        !!terms, table_value
      ) %>%
      dplyr::mutate_at(.vars=terms, ~fct_inorder(.x)) %>%
      dplyr::arrange_at(.vars=terms) %>%
      tidyr::unite(-table_value, col='table_col', sep='._solo_.') %>%
      dplyr::mutate(table_col = fct_inorder(table_col)) %>%
      tidyr::spread(table_col, table_value)

    if(all(c("catg","meff") %in% class(exposure))){

      numeric_output %<>%
        dplyr::mutate(term = terms) %>%
        dplyr::rename(variable = !!terms) %>%
        dplyr::select(term, variable, everything(), -std.error)

      if(nrow(numeric_output)==2){
        # When categorical variables have only 2 levels,
        # only the exposure group needs to be specified
        numeric_output=numeric_output[-1,]
      }

    } else {

      numeric_output %<>%
        dplyr::mutate(term = exposure$term) %>%
        tidyr::unite(col = "variable", !!terms, sep = '._solo_.') %>%
        dplyr::select(term, variable, everything(), -std.error)

    }

    output <- list(
      numeric = numeric_output,
      tabular = tabular_output
    )

  }

  class(output) <- c("model_estimates", class(output))

  output

}




