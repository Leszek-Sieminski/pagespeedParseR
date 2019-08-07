#' Extract general information for all URLs in Lighthouse report object.
#'
#' @param x downloaded LH report object
#'
#' @return data frames with the scores
#' @import dplyr
#' @import assertthat
#' @export
#'
#' @examples
#' \dontrun{
#' # tbd
#' }
extract_lighthouse_general_info <- function(x) {
  # safety net ----------------------------------------------------------------
  # assert_that((is.list(x) || grepl("largeList", class(x))), !is.data.frame(x), length(x) >= 1)

  # extraction ----------------------------------------------------------------
  results <- vector(mode = "list", length = length(x))
  for(i in seq_along(x)) {
    results[[i]] <- fun_lh_extract_general_info(x[[i]])
  }
  results_2 <- Reduce(function(x, y) suppressMessages(full_join(x, y)), results)
  return(results_2)
}
