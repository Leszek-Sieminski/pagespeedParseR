#' Checking existence in LH nested list
#'
#' @param report searched report
#' @param elem searched element
#' @param object_name name of list of all LH reports
#'
#' @return TRUE if exists, FALSE if not
fun_lhex <- function(report, elem = NULL, object_name = "results"){
  # example of checked object, do not delete ----------------------------------
  # results <- list(
  #   "report_1" = list(
  #     "items" = list(
  #       "element_1.1" = "1.1",
  #       "element_1.2" = "1.2")),
  #   "report_2" = list(
  #     "items" = list(
  #       "element_2.1" = "2.1",
  #       "element_2.2" = "2.2")))

  # x <- "results"
  # report <- "report_1"
  # elem <- "element_1.2"

  # custom function for checking if object is NA ------------------------------
  # Traditional is.na have some vec problems
  isNA <- function(x) {
    is.atomic(x) && length(x) == 1 && is.na(x)
  }

  # downloading report object from parent enviroment
  object <- get(object_name, parent.frame())
  if(isNA(object)) { # stop if NA
    return(FALSE)
  }

  content <- object[[report]]
  if(isNA(content)) { # stop if NA
    return(FALSE)
    }

  if (length(content) > 0 & !is.null(content) & !isNA(content)) {
    if (is.null(elem)) {
      # if only 2nd level list to check
      logic_test <- "items" %in% names(object[[report]])
    } else {
      # if also 3rd level list to check
      logic_test <- "items" %in% names(object[[report]]) & elem %in% names(object[[report]][["items"]])
    }
  } else {
    return(FALSE)
  }
}
