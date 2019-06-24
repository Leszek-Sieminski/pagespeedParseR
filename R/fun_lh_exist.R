#' Checking existence in LH nested list
#'
#' @param report searched report
#' @param elem searched element
#' @param x list of all LH reports
#'
#' @return TRUE if exists, FALSE if not
fun_lhex <- function(report, elem = NULL, x = "results"){

  content <- eval(parse(text = paste0(x, '}[["', report, '"]]'))) # glue::glue('{x}[["{report}"]]')))

  isNA <- function(x) {
    is.atomic(x) && length(x) == 1 && is.na(x)
  }

  if (length(content) > 0 & !is.null(content) & !isNA(content)) {
    t <- paste0('"items" %in% names(', x, '[["', report, '"]])')
    if (!is.null(elem)) {
      t <- paste0(t, ' & "', elem, '" %in% names(', x, '[["', report, '"]][["items"]])')
    }
    z <- eval(parse(text = t))
    return(z)
  } else {
    return(FALSE)
  }
}
