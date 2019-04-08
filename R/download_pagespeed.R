#' Download classic Pagespeed report (Pagespeed API ver. 4)
#'
#' @description Download single or multiple URLs in variety of options.
#'
#' @param url vector of character strings. The URLs to fetch and analyze
#' @param key string. Pagespeed API key to authenticate. Defaults to
#'     "PAGESPEED_API_KEY" enviroment variable.
#' @param output_type string. Choose how to parse the output. Options:
#'     "simple" or "raw". See more in Details section
#' @param strategy string/character vector. The analysis strategy to use.
#'     Options: \code{"desktop"}, \code{"mobile"},
#'     and \code{"c("desktop", "mobile")"} to return both results in one
#'     function call.
#' @param interval numeric. Number of seconds to wait between multiple queries.
#'     Defaults to 0.5 second.
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
#'     The \code{categories} parameter works only for API version 5.
#'     It regulates which of the tests' categories from Lighthouse
#'     are to be run. You can select more than one in a vector.
#'     Options: "accessibility", "best-practices", "performance", "pwa",
#'     "seo".
#'
#' @return two options: data frame (if "simple"), nested list (if "raw")
#' @export
#'
#' @examples
#' \dontrun{
#' # download simple data frame with Pagespeed report for Google.com
#' ps_df_1 <- download_pagespeed(url = "https://www.google.com", output_type = "simple")
#'
#' # run Pagespeed reports for Google.com & Bing.com for mobile and
#' # return in a data frame with most important columns
#' ps_df_2 <- download_pagespeed(url = c("https://www.google.com",
#'                                       "https://www.bing.com/"),
#'                               output_type = "simple",  # return the results in a wide data frame
#'                               strategy = "mobile",     # run tests for mobile
#'                               interval = 1)            # wait 1 second between the calls to API
#'
#' # run Pagespeed reports for Google.com & Bing.com for both desktop & mobile and
#' # return in a data frame with most important columns
#' ps_df_3 <- download_pagespeed(url = c("https://www.google.com",
#'                                       "https://www.bing.com/"),
#'                               output_type = "simple",  # return the results in a wide data frame
#'                               strategy = c("desktop",  # check both desktop and mobile, bind
#'                                            "mobile"),
#'                               interval = 2)            # wait 2 seconds between the calls to API
#'
#' # download nested list with Pagespeed report for Google.com
#' ps_nl_1 <- download_pagespeed(url = "https://www.google.com",
#'                               output_type = "raw")
#'
#' # run Pagespeed for Google.com & Bing.com for desktop and
#' # return in a nested list with all possible data
#' ps_nl_2 <- download_pagespeed(url = c("https://www.google.com",
#'                                       "https://www.bing.com/"),
#'                               output_type = "raw",
#'                               strategy = "desktop",
#'                               interval = 1)
#'
#' # check "Performance" for Google.com & Bing.com for both desktop & mobile and
#' # return in a nested list with all possible data
#' ps_nl_3 <- download_pagespeed(url = c("https://www.google.com",
#'                                       "https://www.bing.com/"),
#'                               output_type = "raw",
#'                               strategy = c("desktop",
#'                                            "mobile"),
#'                               interval = 2)
#'
#' }
download_pagespeed <- function(url, key = Sys.getenv("PAGESPEED_API_KEY"),
                               output_type = "simple", strategy = "desktop",
                               interval = 0.5,
                               filter_third_party = NULL, locale = NULL,
                               rule = NULL, screenshot = NULL,
                               snapshots = NULL, utm_campaign = NULL,
                               utm_source = NULL)
{
  # safety net ----------------------------------------------------------------
  if (is.null(key) | nchar(key) == 0){
    stop("API key is a NULL or has length = 0. Please check it and provide a proper API key.", call. = FALSE)}

  assert_that(not_empty(url), is.character(url), all(grepl(".", url, fixed = T)),
              is.string(key), # is.number(api_version), api_version %in% c(4, 5),
              is.character(strategy) | is.null(strategy),
              is.number(interval) & interval >= 0 & interval <= 120,
              is.string(filter_third_party) | is.null(filter_third_party),
              is.string(locale)             | is.null(locale),
              is.string(rule)               | is.null(rule),
              is.logical(screenshot)        | is.null(screenshot),
              is.logical(snapshots)         | is.null(snapshots),
              is.string(utm_campaign)       | is.null(utm_campaign),
              is.string(utm_source)         | is.null(utm_source))

  # creating report -----------------------------------------------------------
  if (grepl("raw", output_type)) {
    pagespeed_raw_list_v4(
      url = url, strategy = strategy, interval = interval, key = key,
      filter_third_party = filter_third_party, locale = locale, rule = rule, screenshot = screenshot,
      snapshots = snapshots, utm_campaign = utm_campaign, utm_source = utm_source)
  } else if (grepl("simple", output_type)) {
    pagespeed_simple_list_v4(
      url = url, strategy = strategy, interval = interval, key = key,
      filter_third_party = filter_third_party, locale = locale, rule = rule, screenshot = screenshot,
      snapshots = snapshots, utm_campaign = utm_campaign, utm_source = utm_source)}
}
