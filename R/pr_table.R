
#' Generate a nice table with prevalence ratios
#' @param data a data frame
#' @param exposure a named character vector. Names are variable labels. Values are column names in the data.
#' @param outcome a named character vector. Names are outcome labels. Values are column names of the outcomes in the data.
#' @param control a list of character vectors. The ith item in the list should contain column names of variables that will be added as control variables for the ith model. Naming the character vectors will result in the names being used as labels for control variables in the footnote of the table.
#' @param include.unadjusted logical. Should unadjusted prevalence ratios be presented in the table?
#' @param include.descriptive logical. Should the prevalence of the outcome be presented in the table?
#' @export
#' @importFrom stats "as.formula" "complete.cases"
#' @importFrom magrittr "set_names" "%>%"
#' @importFrom dplyr 'mutate' 'select' 'filter' 'bind_rows' 'everything'
#' @importFrom purrr 'map' 'map_lgl' 'map2' 'reduce'
#' @importFrom tidyr 'spread' 'gather'

#' @examples
#' library(titanic)
#' library(magrittr)
#' library(dplyr)
#' library(gt)
#'
#' data <- titanic_train %>%
#'   dplyr::select(Survived, Pclass, Sex, Age, SibSp, Fare) %>%
#'   as_tibble() %>%
#'   dplyr::mutate(
#'     Pclass=factor(
#'       Pclass,
#'       levels=c(1,2,3),
#'       labels=c("1st Class","2nd Class","3rd Class")
#'     )
#'   ) %>%
#'   na.omit()
#'
#' tst_output <- pr_table(
#'   data=data,
#'   exposure=c("Passenger class"='Pclass'),
#'   outcome=c("Survival"='Survived'),
#'   control=list(
#'     c("Passenger gender"="Sex"),
#'     c("Passenger age"="Age",
#'       "Ticket price"="Fare"),
#'     c("Gender by age interaction"="Sex:Age")
#'   ),
#'   include.unadjusted=TRUE,
#'   include.descriptive=TRUE
#' )

pr_table <- function(
  data,
  exposure,
  outcome,
  control,
  include.unadjusted=TRUE,
  include.descriptive=TRUE
){

 UseMethod('pr_table')

}
