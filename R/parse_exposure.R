
#' grab the exposure variables from a formula
#' @param exposure the exposure variable
#' @param data a dataset with labelled columns
#' @export

set_exposure_class <- function(exposure, data){

  var_label = . = NULL

  term <- gsub(" ", "", trimws(exposure), fixed=TRUE)

  new_class <- if(grepl("*", term, fixed=TRUE)){

    parts <- unlist(strsplit(term, "*", fixed=TRUE))

    if(any(grepl("s(", parts, fixed=T))){
      parts <- gsub("s(","", parts, fixed=T)
      parts <- gsub(")", "", parts, fixed=T)
      ctns_label <- 'spline'
    } else {
      ctns_label <- 'linear'
    }

    classes <- map_chr(data[,parts], ~class(.x)[1])

    fct <- 'factor' %in% classes
    nmr <- any(c('numeric','integer') %in% classes)

    if(fct & nmr){
      c('ieff','catg_ctns', ctns_label)
    } else if (fct & !nmr){
      c('ieff','catg')
    } else if (!fct & nmr){
      c('ieff','ctns', ctns_label)
    } else {
      stop("All exposures should be numeric or factor classes")
    }

  } else {

    if(any(grepl("s(", exposure, fixed=T))){
      exposure <- gsub("s(","", exposure, fixed=T)
      exposure <- gsub(")", "", exposure, fixed=T)
      ctns_label <- 'spline'
    } else {
      ctns_label <- 'linear'
    }

    variable_class <- class(data[[exposure]])[1]

    if(variable_class=='factor'){
      c('meff','catg')
    } else if(variable_class %in% c('numeric','integer')){
      c('meff','ctns', ctns_label)
    }

  }

  # Remove the smoother indication
  # spline instructions are stored in the new_class variable now
  term <- gsub("s(","", term, fixed=T)
  term <- gsub(")", "", term, fixed=T)

  variables = term %>%
    map(.f=function(.term){
      if(grepl("*", .term, fixed=TRUE)){
        strsplit(split = '*', x=.term, fixed=TRUE) %>%
          unlist() %>%
          trimws() %>%
          c(paste(., collapse=':'))
      } else {
        .term
      }
    }) %>%
    unlist()

  labels = vector(mode='character', length = length(variables))

  for(i in 1:length(variables)){

    # check to see if this is an interaction variable
    if(grepl(":", variables[i], fixed=TRUE)){
      # if so, get labels one by one
      var_parts <- strsplit(
        variables[i], split=':', fixed=TRUE
      ) %>%
        unlist()

      for(j in 1:length(var_parts)){
        # If this column has a label in the data
        if(!is.null(var_label(data[[var_parts[j]]]))){
          # use it
          var_parts[j] <- var_label(data[[var_parts[j]]])
        }
        # otherwise, let the column name be the label
      }

      labels[i] <- paste(var_parts, collapse = ' x ')

    } else {

      if(!is.null(var_label(data[[variables[i]]]))){
        labels[i] <- var_label(data[[variables[i]]])
      } else {
        labels[i] <- variables[i]
      }

    }

  }

  names(variables) <- labels

  structure(
    .Data=list(
      term=term,
      variables=variables
    ),
    class = new_class
  )

}


# parse_exposure <- function(exposure, data){
#
#   terms_init = exposure %>%
#     trimws() %>%
#     gsub(" ", "", ., fixed=TRUE)
#
#   terms = terms_init %>%
#     map(.f=function(.term){
#       if(grepl("*", .term, fixed=TRUE)){
#         strsplit(split = '*', x=.term, fixed=TRUE) %>%
#           unlist() %>%
#           trimws() %>%
#           c(paste(., collapse=':'))
#       } else {
#         .term
#       }
#     })
#
#   labels = map(terms, .f=function(.terms){
#     map_chr(.terms, .f=function(.term){
#       if(!is.null(var_label(data[[.term]]))){
#         var_label(data[[.term]])
#       } else if (grepl(":", .term)){
#         "Interaction"
#       } else {
#         .term
#       }
#     })
#   })
#
#   map2(
#     terms, labels, .f = function(.term, .label){
#
#       new_class = if(any(grepl("Interaction",.label,fixed=TRUE))){
#         "catg_exposure_ieff"
#       } else {
#         "catg_exposure_meff"
#       }
#
#       structure(
#         .Data = c(.term) %>% set_names(.label),
#         class = new_class
#       )
#     }
#   ) %>%
#     set_names(gsub("*","_x_",terms_init,fixed=TRUE))
#
# }
