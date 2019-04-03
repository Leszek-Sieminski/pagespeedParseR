#' Download Pagespeed report
#'
#' @description
#'
#' @param url vector of character strings. The URLs to fetch and analyze
#' @param key string. Pagespeed API key to authenticate. Defaults to
#'     "PAGESPEED_API_KEY" enviroment variable.
#' @param output_type string. Choose how to parse the output. Options:
#'     "simple" or "raw". See more in Details section
#' @param api_version numeric. Choose which API version to generate report
#'     from. See more in Details section
#' @param strategy string/character vector. The analysis strategy to use.
#'     Options: \code{"desktop"}, \code{"mobile"},
#'     and \code{"c("desktop", "mobile")"} to return both results in one
#'     function call.
#' @param interval numeric. Number of seconds to wait between multiple queries.
#'     Defaults to 0.5 second.
#' @param keep_tmp logical. Set to TRUE if you need to keep temporary Rdata file
#'     with parsed response. Defaults to FALSE
#' @param filter_third_party logical. Indicates if third party resources should
#'     be filtered out before PageSpeed analysis. Defaults to NULL (= FALSE)
#' @param locale string. The locale used to localize formatted results
#' @param rule string. A PageSpeed rule to run; if none are given, all rules
#'     are run
#' @param screenshot logical. Indicates if binary data containing a screenshot
#'     should be included. Defaults to NULL (= FALSE)
#' @param snapshots logical. Indicates if binary data containing snapshot images
#'     should be included. Defaults to NULL (= FALSE)
#' @param utm_campaign string. Campaign name for analytics. Defaults to NULL
#' @param utm_source string. Campaign source for analytics. Defaults to NULL
#'
#' @details The \code{output_type} parameter regulates how the output will be
#'     parsed and stored.
#'     For "simple" - formatted data frame that contains most of the data
#'     (scores, recommendations and error occurences).
#'     For "raw" - unformatted nested list that contains all the data that
#'     was returned by the API.
#'
#'
#'     The \code{api_version} parameter regulates which API version is to
#'     create the report. Legacy version 4 is now default, but it will
#'     change in the future as version 5 supports additional Lighthouse
#'     reports.
#'
#' @return two options: data frame (if "simple"), nested list (if "raw")
#' @export
#'
#' @examples
#' #' \dontrun{
#' checked_url <- download_pagespeed("https://www.google.com/", )
#'
#' checked_url_raw_list <- download_pagespeed(
#'   url = c("https://www.google.com/", "https://www.bing.com/"),
#'   strategy = c("desktop", "mobile"),
#'   output_type = "raw",
#'   api_version = 4)
#'
#' checked_url_simple_df <- download_pagespeed(
#'   url = c("https://www.google.com/", "https://www.bing.com/"),
#'   strategy = c("desktop", "mobile"),
#'   api_version = 4)
#'
#' }
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
