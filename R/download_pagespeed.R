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
#' @param fields
#'
#' @return
#' @export
#'
#' @examples
download_pagespeed <- function(
  url, strategy = NULL, output_type = "simple", interval = 0.5,
  keep_tmp = FALSE, key = Sys.getenv("PAGESPEED_API_KEY"),
  filter_third_party = NULL,  locale = NULL, rule = NULL, screenshot = NULL,
  snapshots = NULL, utm_campaign = NULL, utm_source = NULL, fields = NULL)
{
  if (grepl("raw", output_type)) {
    pagespeed_raw_list(url = url, strategy = strategy, interval = interval,
                       keep_tmp = keep_tmp, key = key,
                       filter_third_party = filter_third_party,
                       locale = locale, rule = rule, screenshot = screenshot,
                       snapshots = snapshots, utm_campaign = utm_campaign,
                       utm_source = utm_source, fields = fields)
  } else {
    pagespeed_simple_list(url = url, strategy = strategy, interval = interval,
                          keep_tmp = keep_tmp, key = key,
                          filter_third_party = filter_third_party,
                          locale = locale, rule = rule, screenshot = screenshot,
                          snapshots = snapshots, utm_campaign = utm_campaign,
                          utm_source = utm_source, fields = fields)
  }
}
