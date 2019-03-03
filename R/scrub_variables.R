
#' identify unique variables in a character vector of model terms
#' @param ... character strings or vectors of model terms
#' @export

scrub_variables <- function(...){

  args = list(...) %>% reduce(c)

  # For CRAN
  . = NULL

  args %>%
    set_names(NULL) %>%
    paste(collapse=" + ") %>%
    paste("~", .) %>%
    as.formula() %>%
    all.vars()
}
