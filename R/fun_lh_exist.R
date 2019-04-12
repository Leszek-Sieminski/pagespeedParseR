#' Checking existence in LH nested list
#'
#' @param report searched report
#' @param elem searched element
#' @param x list of all LH reports
#'
#' @importFrom glue glue
#'
#' @return TRUE if exists, FALSE if not
fun_lhex <- function(report, elem = NULL, x = "results"){
  # x      = "results"
  # report = "font_size"
  # elem   = "overallSavingsBytes"

  content <- eval(parse(text = glue::glue('{x}[["{report}"]]')))
  # length(content) > 0
  # is.null(content)
  # is.na(content)

  isNA <- function(x) {
    is.atomic(x) && length(x) == 1 && is.na(x)
  }

  # isNA(content)
  # if (length(content) > 0 && (!is.null(content) & !is.na(content))) {
  if (length(content) > 0 & !is.null(content) & !isNA(content)) {
    t <- glue::glue('"items" %in% names({x}[["{report}"]])')
    if (!is.null(elem)) {
      t <- glue::glue(t, ' & "{elem}" %in% names({x}[["{report}"]][["items"]])')
    }
    z <- eval(parse(text = t))
    return(z)
  } else {
    return(FALSE)
  }
}

# fun_lh_exist("results", "bootup_time_details", "url")
# fun_lh_exist("results", "html_has_lang_details", "url")
# fun_lh_exist("results", "html_has_lang_details")


