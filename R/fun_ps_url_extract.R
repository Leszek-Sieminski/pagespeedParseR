#' Extract nested list as a dataframe
#'
#' @param object nested list object that contains 1-line data frames as lists
#'
#' @return data frame
#' @importFrom dplyr bind_rows
#'
#' @examples
#' \dontrun{
#' ps_url_extract(parsed$minify_css$url_blocks[1]$args)
#' }
ps_url_extract <- function(object) {
  x <- if (!is.null(object)) {
    # dplyr::bind_rows(object, .id = "column_label")$value
    withCallingHandlers(suppressWarnings(
      bind_rows(object, .id = "column_label")$value), warning = function(w) {print(w)})

  } else {
    NULL
  }

  x <- if (!is.null(object)) {
    y <- x[grepl("\\.", x) & grepl("\\/", x)]
    paste0(unique(y), collapse = ", ")
  } else {
    NULL
  }
  return(x)
}
