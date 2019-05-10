
#' helper function to translate control objects into lists of model formulas
#' @param control a list of character vectors.
#' @param data a data frame
#' @param nested_models are the models nested?
#' @return a set of parameters for regression models
#' @export

gather_predictors <- function(
  control,
  data,
  nested_models
){

  var_label = NULL

  output <- control

  output[[1]] <- paste(control[[1]])[2] %>%
    strsplit("+", fixed=TRUE) %>%
    trim_list()

  nmodel <- length(output)

  if(nmodel >= 2){

    # if the models are nested
    if(nested_models){
      for(i in 2:length(output)){
        output[[i]] <-
          c(paste(control[[i]])[2] %>%
              strsplit("+", fixed=TRUE) %>%
              trim_list(),
            paste(output[[i-1]])
          )
      }
    } else {
      for(i in 2:length(output)){
        output[[i]] <-
          paste(control[[i]])[2] %>%
          strsplit("+", fixed=TRUE) %>%
          trim_list()
      }
    }

    names(output) <-
      if(is.null(names(control))){
        paste("Model",1:nmodel)
      } else {
        names(control)
      }

  } else {

    names(output)<-
      if(is.null(names(control))){
        "Adjusted"
      } else {
        names(control)
      }

  }

  for(i in 1:length(output)){
    for(j in 1:length(output[[i]])){

      lab <- var_label(data[[output[[i]][j]]])
      if(is.null(lab)) lab <- output[[i]][j]

      names(output[[i]])[j] <- lab

    }
  }

  output

}


