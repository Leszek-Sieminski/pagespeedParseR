#' Title
#'
#' @param url
#' @param strategy
#' @param interval
#' @param keep_tmp
#' @param key
#' @param filter_third_party
#' @param locale
#' @param rule
#' @param screenshot
#' @param snapshots
#' @param utm_campaign
#' @param utm_source
#' @param fields
#'
#' @return
#' @export
#'
#' @examples
pagespeed_raw_v5 <- function(
  url, strategy = NULL, interval = 0.5, keep_tmp = FALSE,
  key = Sys.getenv("PAGESPEED_API_KEY"), filter_third_party = NULL, locale = NULL,
  rule = NULL, screenshot = NULL, snapshots = NULL, utm_campaign = NULL,
  utm_source = NULL, fields = NULL)
{
  message("please wait, v5 is under development")
}
