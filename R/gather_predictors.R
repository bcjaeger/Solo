
#' helper function to translate control objects into lists of model formulas
#' @param control a list of character vectors.
#' @return a set of parameters for regression models
#' @export

gather_predictors <- function(
  control
){

  output <- control
  nmodel <- length(output)

  if(nmodel >= 2){

    # if the models are nested
    for(i in 2:length(output)){
      output[[i]] <-
        c(output[[i]],
          output[[i-1]]
        )
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

  output

}


