#' Download new Pagespeed report (Lighthouse) (Pagespeed API ver. 5)
#'
#' @description Download PageSpeed Insights v5 report ("Lighthouse")
#'      for single or multiple URLs in variety of options.
#'      You can choose the the resulting object to be an original nested list
#'      (\code{output_type = "raw"}) with all the data or a parsed data frame
#'      (\code{output_type = "simple"}) with most of the data.
#'
#'      If you choose a data frame, mind that it will have literally
#'      hundreds/thousands of columns. The number
#'      of columns IS NOT STABLE, because it depends to number of error
#'      occurrences and their types.
#'
#'      Because of this you can decide to set parameter (\code{long_result =
#'      TRUE}) to obtain more messy but "long-like" output data frame.
#'
#' @param url vector of character strings. The URLs to fetch and analyze
#'     MUST contain "http://" or "https://".
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
#' @param long_result logical. Should the resulting data frame be a long df?
#' @param interval numeric. Number of seconds to wait between multiple queries.
#'     Defaults to 0.5 second.
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
#'     create the report. Legacy version 4 is a classic Pagespeed, and the
#'     new version 5 returns Lighthouse reports.
#'
#'     The \code{categories} parameter works only for API version 5.
#'     It regulates which of the tests' categories from Lighthouse
#'     are to be run. You can select more than one in a vector.
#'     Options: "accessibility", "best-practices", "performance", "pwa",
#'     "seo".
#'
#' @return two options: data frame (if \code{output_type = "simple"}),
#'     nested list (if \code{output_type = "raw"})
#' @export
#'
#' @examples
#' \dontrun{
#' # download simple data frame with "Performance" Lighthouse report for Google.com:
#' # that's a lot of columns and you will have problems but you can
#' # spread/gather them as you like
#'
#' lh_df_1 <- download_lighthouse(
#'  url = "https://www.google.com",
#'  output_type = "simple") # return the results in a wide data frame
#'
#' class(lh_df_1)
#' # [1] "data.frame"
#' dim(lh_df_1)   # 1 row, 779 columns. The number of columns may wildly differ
#' # [1]   1 779  # because it depends also to number of spotted errors and their types
#'
#'
#'
#' # this time let's download it and parse into messy long-like table:
#' lh_df_1_long <- download_lighthouse(
#'   url = "https://www.google.com",
#'   output_type = "simple", # return the results in a wide data frame
#'   long_result = TRUE) # spread the data into easier-to-digest form
#'
#' class(lh_df_1_long)
#' # [1] "data.frame"
#' dim(lh_df_1_long) # 780 rows in 3 columns
#' # [1] 780   3
#'
#'
#'
#' # check "Performance" for Google.com & Bing.com for both desktop & mobile and
#' # return in a data frame with most important columns
#' lh_df_2 <- download_lighthouse(
#'   url = c("https://www.google.com",
#'           "https://www.bing.com/"),
#'   output_type = "simple", # return the results in a wide data frame
#'   strategy = c("desktop", # check both desktop and mobile, bind
#'                "mobile"),
#'   interval = 1, # wait 1 second between the calls to API
#'   categories = "performance") # which Lighthouse reports
#'                               # are to be run?
#'
#' class(lh_df_2)
#' # [1] "data.frame"
#' dim(lh_df_2)
#' # [1]    4 1231
#'
#'
#'
#' # check "Performance" and "Accessibility" for Google.com & Bing.com for
#' # both desktop & mobile and return in a data frame with most important columns
#' lh_df_3 <- download_lighthouse(
#'   url = c("https://www.google.com",
#'           "https://www.bing.com/"),
#'   output_type = "simple", # return the results in a wide data frame
#'   strategy = c("desktop", # check both desktop and mobile, bind
#'                "mobile"),
#'   interval = 2,           # wait 2 seconds between the calls to API
#'   categories = c("performance", # run performance & accessibility
#'                  "accessibility"))
#'
#' class(lh_df_3)
#' [1] "data.frame"
#' dim(lh_df_3)
#' [1]    4 1637
#'
#'
#'
#' # check "Performance" and "Accessibility" for Google.com & Bing.com for
#' # both desktop & mobile and return in a data frame with even more data,
#' # including error occurences and the importance of each report result
#' lh_df_4 <- download_lighthouse(
#'   url = c("https://www.google.com",
#'           "https://www.bing.com/"),
#'   output_type = "simple", # return the results in a wide data frame
#'   strategy = c("desktop", # check both desktop and mobile, bind
#'                "mobile"),
#'   interval = 2,           # wait 2 seconds between the calls to API
#'   categories = c("performance", # run performance & accessibility
#'                  "accessibility"))
#'
#'
#'
#' # another run for a messy long-like data frame
#' lh_df_4_long <- download_lighthouse(
#'   url = c("https://www.google.com",
#'           "https://www.bing.com/"),
#'   output_type = "simple", # return the results in a wide data frame
#'   strategy = c("desktop", # check both desktop and mobile, bind
#'                "mobile"),
#'   interval = 2,           # wait 2 seconds between the calls to API
#'   categories = c("performance", # run performance & accessibility
#'                  "accessibility"),
#'   long_result = TRUE) # spread into 4 columns
#'
#' class(lh_df_4_long)
#' # [1] "data.frame"
#' dim(lh_df_4_long)
#' # 4 columns ("device" + "parameter" + pages values x2) and 1637 rows
#' # [1]    4 1637
#'
#'
#'
#' # download nested list with "Performance" Lighthouse report for Google.com
#' lh_nl_1 <- download_lighthouse(
#'   url = "https://www.google.com",
#'   output_type = "raw") # return nested list with all possible data
#'
#'
#'
#' # check "Performance" for Google.com & Bing.com for both desktop & mobile and
#' # return in a nested list with all possible data
#' lh_nl_2 <- download_lighthouse(
#'   url = c("https://www.google.com",
#'           "https://www.bing.com/"),
#'   output_type = "raw", # return nested list with all possible data
#'   strategy = c("desktop", # check both desktop and mobile, bind
#'                "mobile"),
#'   interval = 1,           # wait 1 second between the calls to API
#'   categories = "performance") # which Lighthouse reports
#'                               # are to be run?
#' }
download_lighthouse <- function(
  url,
  key          = Sys.getenv("PAGESPEED_API_KEY"),
  output_type  = "simple",
  strategy     = "desktop",
  categories   = "performance",
  long_result  = FALSE,
  interval     = 0.5,
  locale       = NULL,
  utm_campaign = NULL,
  utm_source   = NULL)
{
  # safety net ----------------------------------------------------------------
  if (is.null(key) | nchar(key) == 0){stop("API key is a NULL or has length = 0. Please check it and provide a proper API key.", call. = FALSE)}

  assert_that(
    all(not_empty(url)), all(!is.null(url)),
    all(is.character(url)) & length(url) > 0,
    all(grepl(".", url, fixed = T)), is.string(key),
    all(!is.na(strategy)) &
      (is.null(strategy) || (is.character(strategy) &
                               all(strategy %in% c("desktop", "mobile")))),
    is.null(categories) ||
      (is.character(categories) & categories %in%
         c("accessibility", "best-practices", "performance", "pwa", "seo")),
    is.logical(long_result),
    is.number(interval) & interval >= 0 & interval <= 120,
    (is.string(locale) & nchar(locale) > 0) || is.null(locale),
    is.string(utm_campaign) | is.null(utm_campaign),
    is.string(utm_source)   | is.null(utm_source))

  # creating report -----------------------------------------------------------
  if (grepl("raw", output_type)) {
    lh_raw_2_vec(
      url = url, key = key, strategy = strategy, categories = categories,
      interval = interval,
      locale = locale, utm_campaign = utm_campaign, utm_source = utm_source)
  } else if (grepl("simple", output_type)) {
    lh_simple_2_vec(
      url = url, key = key, strategy = strategy, categories = categories,
      long_result, interval = interval,
      locale = locale, utm_campaign = utm_campaign, utm_source = utm_source)
  }
}
