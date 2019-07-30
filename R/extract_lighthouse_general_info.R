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
  for(i in 1:length(x)) {
    res <- fun_lh_extract_general_info(x[[i]])

    if (i == 1){
      results <- res
    } else {
      results <- suppressMessages(full_join(results, res))
    }
  }
  return(results)
}
