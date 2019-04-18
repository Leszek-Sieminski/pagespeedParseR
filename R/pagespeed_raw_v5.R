#' Download Pagespeed v4 raport for an URL as a nested list
#'
#' @description This function can check a single URL (character) and parse
#'    the output into a data frame. This data frame contain all the possible
#'    information from Pagespeed ver 5.
#'
#' @details This function uses version 5 of the API.
#'    Check function \code{pagespeed_raw_v4} for version 4.
#'    If you need less information but in form of a data frame,
#'    use \code{pagespeed_simple_v5}.
#'
#' @param url string. The URL to fetch and analyze
#' @param key string. Pagespeed API key to authenticate. Defaults to
#'     "PAGESPEED_API_KEY" enviroment variable.
#' @param strategy string. The analysis strategy to use. Options: "desktop" or
#'     "mobile". Defaults to "desktop"
#' @param categories string. A Lighthouse category to run. Defaults to
#'     "performance". See more in Details section
#' @param interval numeric. Number of seconds to wait between multiple queries.
#'     Defaults to 0.5 second.
#' @param locale string. The locale used to localize formatted results
#' @param utm_campaign string. Campaign name for analytics. Defaults to NULL
#' @param utm_source string. Campaign source for analytics. Defaults to NULL
#'
#' @details The \code{category} parameter regulates which of the tests'
#'     categories from Lighthouse are to be run. You can select more than
#'     one.
#'     Options: "accessibility", "best-practices", "performance", "pwa",
#'     "seo".
#'
#' @return nested list
#'
#' @examples
#' \dontrun{
#' single_url_raw_output_5 <- pagespeed_raw_v5("https://www.google.com/")
#' }
pagespeed_raw_v5 <- function(url, key = Sys.getenv("PAGESPEED_API_KEY"),
                             strategy = NULL, categories = "performance",
                             interval = 0.5, locale = NULL,
                             utm_campaign = NULL, utm_source = NULL)
{
  # safety net ----------------------------------------------------------------
  if (is.null(key) | nchar(key) == 0){stop("API key is a NULL or has length = 0. Please check it and provide a proper API key.", call. = FALSE)}

  assert_that(not_empty(url), is.string(url) & length(url) > 0, grepl(".", url, fixed = T),
              is.string(key), is.null(strategy) ||
                (is.character(strategy) & strategy %in% c("desktop", "mobile")),
              is.null(categories) ||
                (is.character(categories) & categories %in%
                   c("accessibility", "best-practices", "performance", "pwa", "seo")),
              is.number(interval) & interval >= 0 & interval <= 120,
              (is.string(locale) & nchar(locale) > 0) || is.null(locale),
              is.string(utm_campaign) | is.null(utm_campaign),
              is.string(utm_source)   | is.null(utm_source))

  # downloading ---------------------------------------------------------------
  req <- httr::GET(
    url = "https://www.googleapis.com/pagespeedonline/v5/runPagespeed",
    query = list(url = url, strategy = strategy, key = key, locale = locale,
                 category = categories[1],
                 category = if (length(categories) >= 2) {categories[2]} else {NULL},
                 category = if (length(categories) >= 3) {categories[3]} else {NULL},
                 category = if (length(categories) >= 4) {categories[4]} else {NULL},
                 category = if (length(categories) == 5) {categories[5]} else {NULL},
                 utm_campaign = utm_campaign,
                 utm_source = utm_source))

  # parsing -------------------------------------------------------------------
  # httr::stop_for_status(req) # we don't want to stop for error as we want to know which URL's wasn't properly returned
  if (req$status_code == 200) {
    if (httr::http_type(req) != "application/json") {stop("API did not return json", call. = FALSE)}
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
