#' Extract category scores for all URLs in Lighthouse report object.
#'
#' @param x nested list object returned by \code{download_lighthouse()} function
#'
#' @return data frames with the scores
#' @export
#' @importFrom purrr map_dfr
#'
#' @examples
#' \dontrun{
#' # download Lighthouse report as a global enviroment object
#' lh_object <- download_lighthouse(
#' url        = c("https://www.w3.org/",
#'                "https://www.google.com"),
#' categories = c("performance",
#'                "accessibility",
#'                "best-practices",
#'                "pwa",
#'                "seo"))
#'
#' # download Lighthouse report as a reference to file
#' lh_ref <- download_lighthouse(
#'   url            = c("https://www.w3.org/",
#'                      "https://www.google.com"),
#'   categories     = c("performance",
#'                      "accessibility",
#'                      "best-practices",
#'                      "pwa",
#'                      "seo"),
#'   as_reference   = TRUE,
#'   reference_path = "ref_path.llo")
#'
#' # extraction
#' scores_obj  <- lh_score_extraction4(lh_object)
#' scores_ref <- lh_score_extraction4(lh_ref)
#'
#' # let's see the objects' structures!
#'
#' str(scores_obj)
#' 'data.frame':	2 obs. of  7 variables:
#' $ url           : chr  "https://www.w3.org/" "https://www.google.com/"
#' $ device        : chr  "desktop" "desktop"
#' $ performance   : num  1 1
#' $ accessibility : num  0.78 0.87
#' $ best.practices: num  0.77 0.85
#' $ seo           : num  1 0.8
#' $ pwa           : num  0.54 0.46
#'
#' > str(scores_ref)
#' 'data.frame':	2 obs. of  7 variables:
#' $ url           : chr  "https://www.w3.org/" "https://www.google.com/"
#' $ device        : chr  "desktop" "desktop"
#' $ performance   : num  1 1
#' $ accessibility : num  0.78 0.89
#' $ best.practices: num  0.77 0.92
#' $ seo           : num  1 0.8
#' $ pwa           : num  0.54 0.46#'
#' }
extract_lighthouse_scores <- function(x) {
  # preallocation
  results <- vector(mode = "list", length = length(x))
  # lapply/map doesn't work for references... but for does
  for(i in 1:length(x)) {
    res <- fun_lh_extract_scores(x[[i]])
    results[i] <- list(res)
  }
  # resulting list of data frames must be converted to a single data frame
  return(map_dfr(results, `[`))
}
