#' Download Pagespeed v4 raport for multiple URLs as one data frame
#'
#' @description This function can check multiple URLs given in a vector
#'    and parse them into a one data frame. This data frame doesn't
#'    contain all the possible information from Pagespeed as response
#'    cannot be simply parsed into tabular form, but it contains
#'    most of the metrics, recommendations and error occurences.
#'
#' @details This function uses legacy version 4 of the API.
#'    Check function \code{pagespeed_simple_list_v5} for version 5.
#'    If you need all the information but in form of a nested list,
#'    use \code{pagespeed_raw_lists_v4}.
#'
#' @param url vector of character strings. The URLs to fetch and analyze
#' @param key string. Pagespeed API key to authenticate. Defaults to
#'     "PAGESPEED_API_KEY" enviroment variable.
#' @param strategy string. The analysis strategy to use. Options: "desktop" or
#'     "mobile". Defaults to "desktop"
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
#' @return formatted data frame
#'
#'
#' @import assertthat
#'
#' @examples
#' \dontrun{
#' multiple_urls_simple_output <- pagespeed_simple_list_v4("https://www.google.com/")
#' }
pagespeed_simple_list_v4 <- function(url, key = Sys.getenv("PAGESPEED_API_KEY"),
                                     strategy = NULL, interval = 0.5, keep_tmp = FALSE,
                                     filter_third_party = NULL,locale = NULL, rule = NULL,
                                     screenshot = NULL, snapshots = NULL,
                                     utm_campaign = NULL, utm_source = NULL)
{
  # safety net ----------------------------------------------------------------
  if (is.null(key) | nchar(key) == 0){
    stop("API key is a NULL or has length = 0. Please check it and provide a proper API key.", call. = FALSE)}

  assert_that(not_empty(url), is.character(url), all(grepl(".", url, fixed = T)),
              is.string(key), is.character(strategy) | is.null(strategy),
              is.number(interval) & interval >= 0 & interval <= 120,
              is.logical(keep_tmp),
              is.string(filter_third_party) | is.null(filter_third_party),
              is.string(locale)             | is.null(locale),
              is.string(rule)               | is.null(rule),
              is.logical(screenshot)        | is.null(screenshot),
              is.logical(snapshots)         | is.null(snapshots),
              is.string(utm_campaign)       | is.null(utm_campaign),
              is.string(utm_source)         | is.null(utm_source))

  # downloading ---------------------------------------------------------------
  if ("desktop" %in% strategy & "mobile" %in% strategy) {
    # simple df, both devices -------------------------------------------------
    desktop <- purrr::map_dfr(
      .x = url,
      # TODO cleaning the code from comments
      # .f = if (api_version == 4) {pagespeed_simple_v4} else if (api_version == 5) {pagespeed_simple_v5},
      .f = pagespeed_simple_v4,
      strategy = "desktop", key = key, interval = interval,
      filter_third_party = filter_third_party, locale = locale, rule = rule,
      screenshot = screenshot, snapshots = snapshots,
      utm_campaign = utm_campaign, utm_source = utm_source)

    Sys.sleep(1 + interval) # very simple time interval for saving API limits

    mobile <- purrr::map_dfr(
      .x = url,
      # .f = if (api_version == 4) {pagespeed_simple_v4} else if (api_version == 5) {pagespeed_simple_v5},
      .f = pagespeed_simple_v4,
      interval = interval, strategy = "mobile", key = key,
      filter_third_party = filter_third_party, locale = locale, rule = rule,
      screenshot = screenshot, snapshots = snapshots,
      utm_campaign = utm_campaign, utm_source = utm_source)

    results <- rbind(desktop, mobile)
    return(results)
  } else if (is.null(strategy) || grepl("desktop", strategy)) {

    # simple df, only desktop -------------------------------------------------
    results <- purrr::map_dfr(
      .x = url,
      # .f = if (api_version == 4) {pagespeed_simple_v4} else if (api_version == 5) {pagespeed_simple_v5},
      .f = pagespeed_simple_v4,
      interval = interval, strategy = "desktop", key = key,
      filter_third_party = filter_third_party, locale = locale, rule = rule,
      screenshot = screenshot, snapshots = snapshots,
      utm_campaign = utm_campaign, utm_source = utm_source)
    return(results)
  } else if (grepl("mobile", strategy)) {

    # simple df, only mobile --------------------------------------------------
    results <- purrr::map_dfr(
      .x = url,
      # .f = if (api_version == 4) {pagespeed_simple_v4} else if (api_version == 5) {pagespeed_simple_v5},
      .f = pagespeed_simple_v4,
      interval = interval, strategy = "mobile", key = key,
      filter_third_party = filter_third_party, locale = locale, rule = rule,
      screenshot = screenshot, snapshots = snapshots,
      utm_campaign = utm_campaign, utm_source = utm_source)
    return(results)
  }
}
