
#' check inputs and return the original (og) data.
#' @param data the data
#' @param all_vars all of the variables specified for modeling
#' @param exposure the designated exposure variable for model fitting
#' @param control  list of model parameters
#' @export

scrub_data <- function(data, all_vars, exposure, control){

  if(inherits(data,'data.frame')){

    nrows_with_complete_data <- data[, all_vars] %>%
      complete.cases() %>%
      sum()

    if(nrows_with_complete_data < nrow(data)) {
      stop("Missing values in control variables.")
    }

  }

  exposure_error <- control %>%
    map_lgl(
      ~ any(
        grepl(exposure, ., fixed=TRUE)
      )
    )

  if(any(exposure_error)){
    error_message <- paste0(
      "Exposure variable should not be in the control list.",
      "See control list item number",
      if(sum(exposure_error)>1) "s " else " ",
      list_elements(which(exposure_error))
    )
    stop(error_message)
  }

  if (inherits(data, 'data.frame')){
    dplyr::as_tibble(data)
  } else if(inherits(data, 'mids')) {
    dplyr::as_tibble(data$data)
  }

}

