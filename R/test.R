

#' test statistical hypotheses from a solo model
#' @param model  a fitted model object
#' @param exposure exposure variable
#' @param data data frame
#' @param variables_to_test character vector of variables to test
#' @importFrom stats 'na.omit' 'anova'
#' @export

test_hypotheses <- function(
  model,
  exposure,
  data,
  variables_to_test=NULL
){

  .=NULL

  variables_to_test %<>% c(exposure$variables, .) %>%
    unique() %>%
    purrr::set_names()

  nimpute = length(data$mult)

  if(nimpute > 0){

    class(model)<- class(model[[1]])

    tst = mimpute_test(
      model=model,
      exposure=exposure,
      data=data,
      variables_to_test=variables_to_test
    )

  } else {

    tst = simpute_test(
      model=model,
      exposure=exposure,
      data=data,
      variables_to_test=variables_to_test
    )

  }

  if(ncol(tst)==1){
    names(tst) <- 'P Value'
  } else {
    names(tst) <- paste0("P Value._solo_.", names(tst))
  }

  tst

}

simpute_test <-  function(
  model,
  exposure,
  data,
  variables_to_test
){

  UseMethod("simpute_test")

}

mimpute_test <-  function(
  model,
  exposure,
  data,
  variables_to_test
){

  UseMethod("mimpute_test")

}

simpute_test.geeglm <- function(
  model,
  exposure,
  data,
  variables_to_test
){

  effect = df = t.value = `P(>|t|)` = RIV = . = NULL
  df1 = F.value = `P(>F)` = p.value = term = NULL
  Df = X2 = `P(>|Chi|)` = NULL

  variables_to_test %>%
    map(
      .f=function(.variable){

        # .variable = variables_to_test[[1]]
        model_terms <- paste(model$formula)[3] %>%
          strsplit(split = ' + ', fixed=TRUE) %>%
          unlist() %>%
          map(
            .f=function(string){
              if(grepl("*", string, fixed=TRUE)){
                terms = strsplit(string, "*", fixed=TRUE) %>%
                  unlist() %>%
                  trimws()
                c(terms, paste(terms,collapse=':'))
              } else {
                string
              }
            }
          ) %>%
          unlist()

        plus_or_minus <- if(.variable %in% model_terms) "-" else "+"

        # Search model terms for any other instances of .variable

        int1_index <- grepl(
          pattern = paste0(":",.variable),
          x = model_terms
        )
        int2_index <- grepl(
          pattern = paste0(.variable,":"),
          x = model_terms
        )

        # if any are found, include them in the test term

        if(any(int1_index)){
          .variable = c(.variable, model_terms[int1_index])
        }

        if(any(int2_index)){
          .variable = c(.variable, model_terms[int2_index])
        }

        # There are scoping issues with the data
        # and the update function. Defining the data
        # outside of the update function is necessary.
        m0 <- update(
          model, as.formula(
            paste(
              ". ~ .", plus_or_minus,
              paste(.variable, collapse=plus_or_minus)
            )
          ),
          data=data$orig
        )

        anova(
          model,
          m0
        ) %>%
          as_tibble() %>%
          dplyr::rename(
            df=Df,
            statistic=X2,
            p.value=`P(>|Chi|)`
          ) %>%
          mutate(
            p.value = ifelse(
              p.value >= 0.001,
              format(round(p.value,3),nsmall=3),
              "<0.001"
            )
          )
      }
    ) %>%
    dplyr::bind_rows(.id='term') %>%
    dplyr::select(term, p.value) %>%
    tidyr::spread(term, p.value) %>%
    dplyr::select(!!variables_to_test)

}

mimpute_test.geeglm <- function(
  model,
  exposure,
  data,
  variables_to_test
){

  effect = df = t.value = `P(>|t|)` = RIV = . = NULL
  df1 = F.value = `P(>F)` = p.value = term = NULL
  Df = X2 = `P(>|Chi|)` = NULL

  variables_to_test %>%
    map(
      .f=function(.variable){

        # .variable = variables_to_test[[1]]
        model_terms <- paste(model[[1]]$formula)[3] %>%
          strsplit(split = ' + ', fixed=TRUE) %>%
          unlist() %>%
          map(
            .f=function(string){
              if(grepl("*", string, fixed=TRUE)){
                terms = strsplit(string, "*", fixed=TRUE) %>%
                  unlist() %>%
                  trimws()
                c(terms, paste(terms,collapse=':'))
              } else {
                string
              }
            }
          ) %>%
          unlist()

        plus_or_minus <- if(.variable %in% model_terms) "-" else "+"

        # Search model terms for any other instances of .variable

        int1_index <- grepl(
          pattern = paste0(":",.variable),
          x = model_terms
        )
        int2_index <- grepl(
          pattern = paste0(.variable,":"),
          x = model_terms
        )

        # if any are found, include them in the test term

        if(any(int1_index)){
          .variable = c(.variable, model_terms[int1_index])
        }

        if(any(int2_index)){
          .variable = c(.variable, model_terms[int2_index])
        }

        m0 <- map2(
          model, 1:length(model), function(.model,.iter){
            update(
              .model,
              as.formula(
                paste(
                  ". ~ .", plus_or_minus,
                  paste(.variable, collapse=plus_or_minus)
                )
              ),
              data = data$mult[[.iter]]
            )
          }
        )

        df_complete <- data$orig %>%
          dplyr::select(all.vars(model[[1]]$formula)) %>%
          na.omit() %>%
          nrow()

        if(df_complete==0) df_complete=NULL

        if(plus_or_minus=='+'){

          full.model = m0
          null.model = model

        } else {

          full.model = model
          null.model = m0

        }

        rank_diff <- abs(
          full.model[[1]]$rank - null.model[[1]]$rank
        )

        if(rank_diff >= 2){

          pooled_test <- mitml::testModels(
            model = full.model,
            null.model = null.model,
            df.com = df_complete
          )

        } else {

          .variable_label = if(is.factor(data$orig[[.variable]])){
            paste0(.variable, levels(data$orig[[.variable]])[2])
          } else {
            .variable
          }

          univariate_test = mitml::testEstimates(
            model = full.model, df.com = df_complete
          ) %>%
            magrittr::use_series('estimates') %>%
            tibble::as_tibble(rownames = 'effect') %>%
            .[-1,] %>%
            dplyr::filter(effect==!!.variable_label) %>%
            dplyr::select(
              df1 = df,
              F.value = t.value,
              `P(>F)`=`P(>|t|)`,
              RIV
            )
          pooled_test <- list(
            test = univariate_test
          )

        }

        pooled_test %>%
          magrittr::use_series('test') %>%
          as_tibble() %>%
          dplyr::rename(
            df=df1,
            statistic=F.value,
            p.value=`P(>F)`,
            rel_variance_incr=RIV
          ) %>%
          mutate(
            p.value = ifelse(
              p.value >= 0.001,
              format(round(p.value,3),nsmall=3),
              "<0.001"
            )
          )
      }
    ) %>%
    dplyr::bind_rows(.id='term') %>%
    dplyr::select(term, p.value) %>%
    tidyr::spread(term, p.value) %>%
    dplyr::select(!!variables_to_test)

}

simpute_test.coxph <- function(
  model,
  exposure,
  data,
  variables_to_test
){

  effect = df = t.value = `P(>|t|)` = RIV = . = Chisq = z = NULL
  df1 = F.value = `P(>F)` = p.value = term = Df = NULL
  X2 = `P(>|Chi|)` = `Pr(>|z|)` = NULL

  variables_to_test %>%
    map(
      .f=function(.variable){

        # .variable = variables_to_test[[1]]
        model_terms <- paste(model$formula)[3] %>%
          strsplit(split = ' + ', fixed=TRUE) %>%
          unlist() %>%
          map(
            .f=function(string){
              if(grepl("*", string, fixed=TRUE)){
                terms = strsplit(string, "*", fixed=TRUE) %>%
                  unlist() %>%
                  trimws()
                c(terms, paste(terms,collapse=':'))
              } else {
                string
              }
            }
          ) %>%
          unlist()

        plus_or_minus <- if(.variable %in% model_terms) "-" else "+"

        # Search model terms for any other instances of .variable

        int1_index <- grepl(
          pattern = paste0(":",.variable),
          x = model_terms
        )

        int2_index <- grepl(
          pattern = paste0(.variable,":"),
          x = model_terms
        )

        # if any are found, include them in the test term

        if(any(int1_index)){
          .variable = c(.variable, model_terms[int1_index])
        }

        if(any(int2_index)){
          .variable = c(.variable, model_terms[int2_index])
        }

        m0 <- update(
          model,
          as.formula(
            paste(
              ". ~ .", plus_or_minus,
              paste(.variable, collapse=plus_or_minus)
            )
          ),
          data = data$orig
        )

        if(plus_or_minus=='+'){

          full.model = m0
          null.model = model

        } else {

          full.model = model
          null.model = m0

        }

        rank_diff <- abs(
          length(coef(full.model)) - length(coef(null.model))
        )

        if(rank_diff >= 2){

          pooled_test <- anova(full.model, null.model) %>%
            as.data.frame() %>%
            as_tibble() %>%
            dplyr::select(
              df = Df,
              statistic = Chisq,
              p.value=`P(>|Chi|)`
            ) %>%
            mutate(RIV=0) %>%
            dplyr::filter(!is.na(p.value))

          pooled_test <- list(
            test = pooled_test
          )

        } else {

          .variable_label = if(is.factor(data$orig[[.variable]])){
            paste0(.variable, levels(data$orig[[.variable]])[2])
          } else {
            .variable
          }

          univariate_test = summary(model) %>%
            magrittr::use_series("coefficients") %>%
            tibble::as_tibble(rownames = 'effect') %>%
            dplyr::filter(effect==!!.variable_label) %>%
            dplyr::mutate(df=1, RIV=0) %>%
            dplyr::select(
              df,
              statistic = z,
              p.value = `Pr(>|z|)`,
              RIV
            )

          pooled_test <- list(
            test = univariate_test
          )

        }
        pooled_test %>%
          magrittr::use_series('test') %>%
          as_tibble() %>%
          dplyr::rename(
            rel_variance_incr=RIV
          ) %>%
          mutate(
            p.value = ifelse(
              p.value >= 0.001,
              format(round(p.value,3),nsmall=3),
              "<0.001"
            )
          )
      }
    ) %>%
    dplyr::bind_rows(.id='term') %>%
    dplyr::select(term, p.value) %>%
    tidyr::spread(term, p.value) %>%
    dplyr::select(!!variables_to_test)

}


mimpute_test.coxph <- function(
  model,
  exposure,
  data,
  variables_to_test
){

  effect = df = t.value = `P(>|t|)` = RIV = . = NULL
  df1 = F.value = `P(>F)` = p.value = term =
  Df = X2 = `P(>|Chi|)` = NULL

  variables_to_test %>%
    map(
      .f=function(.variable){

        # .variable = variables_to_test[[1]]
        model_terms <- paste(model[[1]]$formula)[3] %>%
          strsplit(split = ' + ', fixed=TRUE) %>%
          unlist() %>%
          map(
            .f=function(string){
              if(grepl("*", string, fixed=TRUE)){
                terms = strsplit(string, "*", fixed=TRUE) %>%
                  unlist() %>%
                  trimws()
                c(terms, paste(terms,collapse=':'))
              } else {
                string
              }
            }
          ) %>%
          unlist()

        plus_or_minus <- if(.variable %in% model_terms) "-" else "+"

        # Search model terms for any other instances of .variable

        int1_index <- grepl(
          pattern = paste0(":",.variable),
          x = model_terms
        )

        int2_index <- grepl(
          pattern = paste0(.variable,":"),
          x = model_terms
        )

        # if any are found, include them in the test term

        if(any(int1_index)){
          .variable = c(.variable, model_terms[int1_index])
        }

        if(any(int2_index)){
          .variable = c(.variable, model_terms[int2_index])
        }

        m0 <- map2(
          model, 1:length(model), function(.model,.iter){
            update(
              .model,
              as.formula(
                paste(
                  ". ~ .", plus_or_minus,
                  paste(.variable, collapse=plus_or_minus)
                )
              ),
              data = data$mult[[.iter]]
            )
          }
        )

        df_complete <- data$orig %>%
          dplyr::select(all.vars(model[[1]]$formula)) %>%
          na.omit() %>%
          nrow()

        if(df_complete==0) df_complete=NULL

        if(plus_or_minus=='+'){

          full.model = m0
          null.model = model

        } else {

          full.model = model
          null.model = m0

        }

        rank_diff <- abs(
          length(coef(full.model[[1]])) - length(coef(null.model[[1]]))
        )

        if(rank_diff >= 2){

          pooled_test <- mitml::testModels(
            model = full.model,
            null.model = null.model,
            df.com = df_complete
          )

        } else {

          .variable_label = if(is.factor(data$orig[[.variable]])){
            paste0(.variable, levels(data$orig[[.variable]])[2])
          } else {
            .variable
          }

          univariate_test = mitml::testEstimates(
            model = full.model, df.com = df_complete
          ) %>%
            magrittr::use_series('estimates') %>%
            tibble::as_tibble(rownames = 'effect') %>%
            dplyr::filter(effect==!!.variable_label) %>%
            dplyr::select(
              df1 = df,
              F.value = t.value,
              `P(>F)`=`P(>|t|)`,
              RIV
            )
          pooled_test <- list(
            test = univariate_test
          )

        }

        pooled_test %>%
          magrittr::use_series('test') %>%
          as_tibble() %>%
          dplyr::rename(
            df=df1,
            statistic=F.value,
            p.value=`P(>F)`,
            rel_variance_incr=RIV
          ) %>%
          mutate(
            p.value = ifelse(
              p.value >= 0.001,
              format(round(p.value,3),nsmall=3),
              "<0.001"
            )
          )
      }
    ) %>%
    dplyr::bind_rows(.id='term') %>%
    dplyr::select(term, p.value) %>%
    tidyr::spread(term, p.value) %>%
    dplyr::select(!!variables_to_test)

}


