#' Download new Pagespeed report (Lighthouse) (Pagespeed API ver. 5)
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
#' @param categories string. A Lighthouse categories to run.
#'     Defaults to "performance". See more in Details section
#' @param interval numeric. Number of seconds to wait between multiple queries.
#'     Defaults to 0.5 second.
#' @param enhanced_lighthouse logical. Set to TRUE if you want to obtain even
#'     more data from Lighthouse. However, it can create a VERY WIDE data
#'     frame (hundreds of columns) depending on your
#' @param locale string. The locale used to localize formatted results
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
#' checked_url <- download_pagespeed("https://www.google.com/")
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
download_lighthouse <- function(url, key = Sys.getenv("PAGESPEED_API_KEY"),
                                output_type = "simple",
                                strategy = "desktop", categories = "performance",
                                interval = 0.5, enhanced_lighthouse = FALSE,
                                locale = NULL,
                                utm_campaign = NULL, utm_source = NULL) {
  # safety net ----------------------------------------------------------------
  if (is.null(key) | nchar(key) == 0){
    stop("API key is a NULL or has length = 0. Please check it and provide a proper API key.",
         call. = FALSE)}

  assert_that(not_empty(url), is.character(url), all(grepl(".", url, fixed = T)),
              is.string(key), # is.number(api_version), api_version %in% c(4, 5),
              is.character(strategy) | is.null(strategy),
              is.vector(categories) | is.character(categories),
              is.number(interval) & interval >= 0 & interval <= 120,
              is.logical(enhanced_lighthouse),
              is.string(locale)             | is.null(locale),
              is.string(utm_campaign)       | is.null(utm_campaign),
              is.string(utm_source)         | is.null(utm_source))

  # creating report -----------------------------------------------------------
  if (grepl("raw", output_type)) {
    pagespeed_raw_list_v5(
      url = url, key = key, strategy = strategy, categories = categories, interval = interval,
      locale = locale, utm_campaign = utm_campaign, utm_source = utm_source)
  } else if (grepl("simple", output_type)) {
    # TODO adding finished function call (pagespeed_simple_list_v5)
    pagespeed_simple_list_v5(
      url = url, key = key, strategy = strategy, categories = categories, interval = interval,
      locale = locale, utm_campaign = utm_campaign, utm_source = utm_source)
  }
}
