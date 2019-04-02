#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
ps_url_extract <- function(object)
{
  x <- if (!is.null(object)) {
    dplyr::bind_rows(object, .id = "column_label")$value
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
