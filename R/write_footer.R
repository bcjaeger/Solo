
#' writes a footnote describing the models in a prevalence table
#' @param model_predictors a list describing model predictors.
#' @param control a list describing control parameters.
#' @param collapse logical. Should it be one footnote or multiple (one per model)?
#' @return a character value of a table footnote
#' @export

write_footer <- function(
  model_predictors,
  control,
  collapse
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

  output <- map2(
    footnote, control,
    ~{
      paste(
        .x, list_elements(.y,use_names=!is.null(names(.y)))
      )
    }
  )

  if(collapse){
    output %>%
      reduce(
        function(x,y){
          paste0(x,'; ',y)
        }
      )
  } else {
    output
  }

}
