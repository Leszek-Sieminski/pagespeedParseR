#' Extract the category scores from single URL Lighthouse report
#'
#' @param x LH report object for a single URL
#'
#' @return data frame
#'
#' @examples
#' \dontrun{
#' # download Lighthouse report as a global enviroment object
#' lh_object <- download_lighthouse(
#' url        = c("https://www.w3.org/"),
#' categories = c("performance",
#'                "accessibility",
#'                "best-practices",
#'                "pwa",
#'                "seo"))
#'
#' # download Lighthouse report as a reference to file
#' lh_ref <- download_lighthouse(
#'   url            = c("https://www.w3.org/"),
#'   categories     = c("performance",
#'                      "accessibility",
#'                      "best-practices",
#'                      "pwa",
#'                      "seo"),
#'   as_reference   = TRUE,
#'   reference_path = "ref_path.llo")
#'
#' # extraction
#' scores_obj <- fun_lh_extract_scores(lh_object)
#' scores_ref <- fun_lh_extract_scores(lh_ref)
#' }
fun_lh_extract_scores <- function(x) {
  cbind(
    list(url = as.character(x$id)),
    list(device = x$lighthouseResult$configSettings$emulatedFormFactor),
    as.data.frame(lapply(x$lighthouseResult$categories, function(y) y$score)),
    stringsAsFactors = F
  )
}
