#' Download Pagespeed v4 raport for an URL as a nested list
#'
#' @description This function can check a single URL (character) and parse
#'    the output into a data frame. This data frame contain all the possible
#'    information from Pagespeed ver 4.
#'
#' @details This function uses legacy version 4 of the API.
#'    Check function \code{lh_raw_1()} for version 5.
#'    If you need less information but in form of a data frame,
#'    use \code{ps_simple_1()}.
#'
#' @param url string. The URL to fetch and analyze
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
#' single_url_raw_output <- ps_raw_1("https://www.google.com/")
#' }
ps_raw_1 <- function(
  url, key = Sys.getenv("PAGESPEED_API_KEY"),
  strategy = "desktop", interval = 0.5,
  filter_third_party = NULL, locale = NULL, rule = NULL,
  screenshot = NULL, snapshots = NULL,
  utm_campaign = NULL, utm_source = NULL)
{
  # safety net ----------------------------------------------------------------
  if (is.null(key) | nchar(key) == 0){stop("API key is a NULL or has length = 0. Please check it and provide a proper API key.", call. = FALSE)}

  assert_that(not_empty(url), is.string(url) & length(url) > 0, grepl(".", url, fixed = T),
              is.string(key), is.null(strategy) ||
                (is.character(strategy) & strategy %in% c("desktop", "mobile")),
              is.number(interval) & interval >= 0 & interval <= 120,
              is.null(filter_third_party) ||
                (is.logical(filter_third_party) & length(filter_third_party) > 0 & !is.na(filter_third_party)),
              (is.string(locale) & nchar(locale) > 0) || is.null(locale),
              (is.string(rule) & nchar(rule) > 0) || is.null(rule),
              (is.logical(screenshot) & length(screenshot) > 0 & !is.na(screenshot)) || is.null(screenshot),
              (is.logical(snapshots) & length(snapshots) > 0 & !is.na(snapshots)) || is.null(snapshots),
              is.string(utm_campaign) | is.null(utm_campaign),
              is.string(utm_source)   | is.null(utm_source))

  if (is.null(key) | nchar(key) == 0){stop("API key is a NULL - please check it and provide a proper API key.", call. = FALSE)}

  # downloading ---------------------------------------------------------------
  req <- httr::GET(
    url = "https://www.googleapis.com/pagespeedonline/v4/runPagespeed",
    query = list(url = url, key = key, filter_third_party = filter_third_party,
                 locale = locale, rule = rule, screenshot = screenshot,
                 snapshots = snapshots, strategy = strategy,
                 utm_campaign = utm_campaign, utm_source = utm_source))

  # parsing -------------------------------------------------------------------
  # httr::stop_for_status(req) # we don't want to stop for error as we want to know which URL's wasn't properly returned
  if (req$status_code == 200){
    if (httr::http_type(req) != "application/json") {
      stop("API did not return json", call. = FALSE)
    }
    con <- httr::content(req, "text")
    parsed <- jsonlite::fromJSON(con)
    full_results <- parsed
    return(full_results)
  } else {
    full_results <- NULL
    Sys.sleep(interval)
    return(full_results)
  }
}
