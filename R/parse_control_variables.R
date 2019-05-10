
#' parse control variables
#' @param control_variables the control variables
#' @param data a data frame containing the control variables.
#' @export

parse_control_variables <- function(control_variables, data){

  var_label = . = NULL

  if(is.null(names(control_variables))){
    names(control_variables) <- paste(
      "Model", 1:length(control_variables)
    )
  }

  .control <- control_variables %>%
    map(~paste(.x)[2])

  for(i in 1:length(.control)){
    for(j in 1:length(.control[[i]])){
      if(!is.null(var_label(data[[.control[[i]][j]]]))){
        names(.control[[i]])[j] <- var_label(data[[.control[[i]][j]]])
      } else {
        names(.control[[i]])[j] <- .control[[i]][j]
      }
    }
  }

  .control

}
