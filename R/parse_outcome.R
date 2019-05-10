
#' grab the outcome variables from a formula
#' @param outcome outcome variable
#' @param new_class new class for the outcome
#' @param data a dataset with labelled columns
#' @export

set_outcome_class <- function(outcome, new_class, data){

  var_label = . = NULL

  if(!is.null(var_label(data[[outcome]]))){
    label<-var_label(data[[outcome]])
  } else {
    label<-outcome
  }

  new_class = paste(new_class,"outcome",sep="_")

  structure(
    .Data = outcome,
    class = new_class,
    names = label
  )

}

# parse_outcome <- function(outcome, data){
#
#   terms = trimws(outcome)
#
#   labels = vector(mode='character', length = length(terms))
#
#   for(i in 1:length(terms)){
#     if(!is.null(var_label(data[[terms[i]]]))){
#       labels[i] <- var_label(data[[terms[i]]])
#     } else {
#       labels[i] <- terms[i]
#     }
#   }
#
#   map2(
#     terms, labels, .f = function(.term, .label){
#
#       new_class = if(grepl('Surv(',.term,fixed=T)){
#         'survival_outcome'
#       } else {
#         'binary_outcome'
#       }
#
#       structure(
#         .Data = c(.term) %>% set_names(.label),
#         class = new_class
#       )
#     }
#   ) %>%
#     set_names(terms)
#
# }
