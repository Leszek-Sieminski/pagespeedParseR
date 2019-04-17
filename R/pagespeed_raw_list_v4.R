#' Download Pagespeed v4 raport for multiple URLs as a one nested list
#'
#' @description This function can check multiple URLs (character vector)
#'    and parse the output into a data frame. This data frame contain all
#'    the possible information from Pagespeed ver 4.
#'
#' @details This function uses legacy version 4 of the API.
#'    Check function \code{pagespeed_raw_list_v5} for version 5.
#'    If you need less information but in form of a data frame,
#'    use \code{pagespeed_simple_lists_v4}.
#'
#' @param url vector of character strings. The URLs to fetch and analyze
#' @param key string. Pagespeed API key to authenticate. Defaults to
#'     "PAGESPEED_API_KEY" enviroment variable.
#' @param strategy string. The analysis strategy to use. Options: "desktop" or
#'     "mobile". Defaults to "desktop"
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
#' @return unformatted nested list
#'
#' @import assertthat
#'
#' @examples
#' \dontrun{
#' multiple_urls_raw_output <- pagespeed_raw_list_v4("https://www.google.com/")
#' }
pagespeed_raw_list_v4 <- function(url, key = Sys.getenv("PAGESPEED_API_KEY"),
                                  strategy = "desktop", interval = 0.5,
                                  filter_third_party = NULL, locale = NULL, rule = NULL,
                                  screenshot = NULL, snapshots = NULL,
                                  utm_campaign = NULL, utm_source = NULL)
{
  # safety net ----------------------------------------------------------------
  if (is.null(key) | nchar(key) == 0){stop("API key is a NULL or has length = 0. Please check it and provide a proper API key.", call. = FALSE)}

  assert_that(all(not_empty(url)), all(!is.null(url)), all(is.character(url)) & length(url) > 0, all(grepl(".", url, fixed = T)),
              is.string(key),
              !is.na(strategy) & (is.null(strategy) || (is.character(strategy) & strategy %in% c("desktop", "mobile"))),
              is.number(interval) & interval >= 0 & interval <= 120,
              is.null(filter_third_party) ||
                (is.logical(filter_third_party) & length(filter_third_party) > 0 & !is.na(filter_third_party)),
              (is.string(locale) & nchar(locale) > 0) || is.null(locale),
              (is.string(rule) & nchar(rule) > 0) || is.null(rule),
              (is.logical(screenshot) & length(screenshot) > 0 & !is.na(screenshot)) || is.null(screenshot),
              (is.logical(snapshots) & length(snapshots) > 0 & !is.na(snapshots)) || is.null(snapshots),
              is.string(utm_campaign) | is.null(utm_campaign),
              is.string(utm_source)   | is.null(utm_source))


  if ("desktop" %in% strategy & "mobile" %in% strategy) {
    # simple df, both devices ------------------------------------------------------------
    desktop <- purrr::map(
      .x = url,
      .f = pagespeed_raw_v4,
      strategy = "desktop", key = key, interval = interval,
      filter_third_party = filter_third_party, locale = locale, rule = rule,
      screenshot = screenshot, snapshots = snapshots,
      utm_campaign = utm_campaign, utm_source = utm_source)

    Sys.sleep(1 + interval) # very simple time interval for saving API limits

    mobile <- purrr::map(
      .x = url,
      .f = pagespeed_raw_v4,
      strategy = "mobile", interval = interval, key = key,
      filter_third_party = filter_third_party, locale = locale, rule = rule,
      screenshot = screenshot, snapshots = snapshots,
      utm_campaign = utm_campaign, utm_source = utm_source)

    results <- list(
      "desktop" = desktop,
      "mobile"  = mobile
    )

    # results <- c(desktop, mobile)
    return(results)
  } else if (is.null(strategy) || grepl("desktop", strategy)) {

    # simple df, only desktop --------------------------------------------------------------
    results <- purrr::map(
      .x = url,
      .f = pagespeed_raw_v4,
      strategy = "desktop", interval = interval, key = key,
      filter_third_party = filter_third_party, locale = locale, rule = rule,
      screenshot = screenshot, snapshots = snapshots,
      utm_campaign = utm_campaign, utm_source = utm_source)
    return(results)
  } else if (grepl("mobile", strategy)) {

    # simple df, only mobile ---------------------------------------------------------------
    results <- purrr::map(
      .x = url,
      .f = pagespeed_raw_v4,
      strategy = "mobile", interval = interval, key = key,
      filter_third_party = filter_third_party, locale = locale, rule = rule,
      screenshot = screenshot, snapshots = snapshots,
      utm_campaign = utm_campaign, utm_source = utm_source)
    return(results)
  }
}
