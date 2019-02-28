
#' writes a footnote describing the models in a prevalence table
#' @param model_predictors a list describing model predictors.
#' @param control a list describing control parameters.
#' @return a character value of a table footnote
#' @export

write_footer <- function(
  model_predictors,
  control
){
  footnote <- vector(
    mode='list',
    length=length(model_predictors)
  )

  for(i in 1:length(footnote)){
    if(i==1){
      footnote[[i]] = paste0(
        names(model_predictors)[i],
        ' includes adjustment for'
      )
    } else {
      footnote[[i]]= paste0(
        names(model_predictors)[i],
        ' includes ',
        tolower(names(model_predictors)[i-1]),
        ' with additional adjustment for'
      )
    }
  }

  map2(
    footnote, control,
    ~{
      paste(
        .x, list_elements(.y,use_names=!is.null(names(.y)))
      )
    }
  ) %>%
    reduce(
      function(x,y){
        paste0(x,'; ',y)
      }
    )
}
