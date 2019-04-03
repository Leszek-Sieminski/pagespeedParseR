#' Title
#'
#' @param url
#' @param strategy
#' @param output_type
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
#'
#' @return
#' @export
#'
#' @examples
download_pagespeed <- function(url, key = Sys.getenv("PAGESPEED_API_KEY"),
                               output_type = "simple", api_version = 4,
                               strategy = "desktop", interval = 0.5, keep_tmp = FALSE,
                               filter_third_party = NULL, locale = NULL, rule = NULL,
                               screenshot = NULL, snapshots = NULL,
                               utm_campaign = NULL, utm_source = NULL)
{
  # safety net ----------------------------------------------------------------
  if (is.null(key) | nchar(key) == 0){
    stop("API key is a NULL or has length = 0. Please check it and provide a proper API key.", call. = FALSE)}

  assert_that(not_empty(url), is.string(url), grepl(".", url, fixed = T),
              is.string(key), is.number(api_version), api_version %in% c(4, 5),
              is.character(strategy) | is.null(strategy),
              is.number(interval) & interval >= 0 & interval <= 120,
              is.logical(keep_tmp), is.string(filter_third_party),
              is.string(locale), is.string(rule), is.logical(screenshot),
              is.logical(snapshots), is.string(utm_campaign),
              is.string(utm_source))

  # creating report -----------------------------------------------------------
  if (grepl("raw", output_type) & api_version == 4) {
    pagespeed_raw_list_v4(
      url = url, strategy = strategy, interval = interval, keep_tmp = keep_tmp, key = key,
      filter_third_party = filter_third_party, locale = locale, rule = rule, screenshot = screenshot,
      snapshots = snapshots, utm_campaign = utm_campaign, utm_source = utm_source)
  } else if (grepl("simple", output_type) & api_version == 4) {
    pagespeed_simple_list_v4(
      url = url, strategy = strategy, interval = interval, keep_tmp = keep_tmp, key = key,
      filter_third_party = filter_third_party, locale = locale, rule = rule, screenshot = screenshot,
      snapshots = snapshots, utm_campaign = utm_campaign, utm_source = utm_source)
  } else if (grepl("raw", output_type) & api_version == 5) {
    #pagespeed_raw_list_v5()
  } else if (grepl("simple", output_type) & api_version == 5) {
    #pagespeed_simple_list_v5()
  }
}
