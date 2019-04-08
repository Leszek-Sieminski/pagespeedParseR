#' Download Pagespeed v5 raport (Lighthouse) for multiple URLs as one data frame
#'
#' @description This function can check multiple URLs given in a vector
#'    and parse them into a one data frame. This data frame doesn't
#'    contain all the possible information from Pagespeed as response
#'    cannot be simply parsed into tabular form, but it contains
#'    most of the metrics, recommendations and error occurences.
#'
#' @details This function uses new version of the API (5th).
#'    Check function \code{pagespeed_simple_list_v4} for version 4
#'    (classic Pagespeed results).
#'    If you need all the information but in form of a nested list,
#'    use \code{pagespeed_raw_lists_v5}.
#'
#'    Setting \code{enhanced_lighthouse} parameter to \code{TRUE}
#'    will add more data about error occurencess and opportunities
#'    for improvement, but mind that resulting data frame can have
#'    literally hundreds of columns (depending on how many \code{categories}
#'    were selected). This is due to large amount of data returned by
#'    Lighthouse reports.
#'
#' @param url vector of character strings. The URLs to fetch and analyze
#' @param key string. Pagespeed API key to authenticate. Defaults to
#'     "PAGESPEED_API_KEY" enviroment variable.
#' @param strategy string. The analysis strategy to use. Options: "desktop" or
#'     "mobile". Defaults to "desktop"
#' @param categories string. A Lighthouse category/categories to run.
#'     Defaults to "performance". See more in Details section
#' @param interval numeric. Number of seconds to wait between multiple queries.
#'     Defaults to 0.5 second.
#' @param enhanced_lighthouse logical. If TRUE, adds even more columns with
#'     Lighthouse data (errors, opportunities). Defaults to FALSE. See more in
#'     Details section
#' @param locale string. The locale used to localize formatted results
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
pagespeed_simple_list_v5 <- function(url, key = Sys.getenv("PAGESPEED_API_KEY"),
                                     strategy = NULL, categories = "performance",
                                     interval = 0.5,
                                     enhanced_lighthouse = FALSE, locale = NULL,
                                     utm_campaign = NULL, utm_source = NULL)
{
  # safety net ----------------------------------------------------------------
  if (is.null(key) | nchar(key) == 0){
    stop("API key is a NULL or has length = 0. Please check it and provide a proper API key.", call. = FALSE)}

  assert_that(not_empty(url), is.character(url), all(grepl(".", url, fixed = T)),
              is.string(key), is.character(strategy) | is.null(strategy),
              is.number(interval) & interval >= 0 & interval <= 120,
              is.logical(enhanced_lighthouse),
              is.vector(categories) | is.string(categories) | is.null(categories),
              is.string(locale)             | is.null(locale),
              is.string(utm_campaign)       | is.null(utm_campaign),
              is.string(utm_source)         | is.null(utm_source))

  # downloading ---------------------------------------------------------------
  if ("desktop" %in% strategy & "mobile" %in% strategy) {
    # simple df, both devices -------------------------------------------------
    desktop <- purrr::map_dfr(
      .x = url,
      .f = pagespeed_simple_v5,
      strategy = "desktop", key = key, interval = interval,
      categories = categories,
      enhanced_lighthouse = enhanced_lighthouse, locale = locale,
      utm_campaign = utm_campaign, utm_source = utm_source)

    Sys.sleep(1 + interval) # very simple time interval for saving API limits

    mobile <- purrr::map_dfr(
      .x = url,
      .f = pagespeed_simple_v5,
      interval = interval, strategy = "mobile", key = key,
      categories = categories,
      enhanced_lighthouse = enhanced_lighthouse, locale = locale,
      utm_campaign = utm_campaign, utm_source = utm_source)

    results <- rbind(desktop, mobile)
    return(results)
  } else if (is.null(strategy) || grepl("desktop", strategy)) {

    # simple df, only desktop -------------------------------------------------
    results <- purrr::map_dfr(
      .x = url,
      .f = pagespeed_simple_v5,
      interval = interval, strategy = "desktop", key = key,
      categories = categories,
      enhanced_lighthouse = enhanced_lighthouse, locale = locale,
      utm_campaign = utm_campaign, utm_source = utm_source)
    return(results)
  } else if (grepl("mobile", strategy)) {

    # simple df, only mobile --------------------------------------------------
    results <- purrr::map_dfr(
      .x = url,
      .f = pagespeed_simple_v5,
      interval = interval, strategy = "mobile", key = key,
      categories = categories,
      enhanced_lighthouse = enhanced_lighthouse, locale = locale,
      utm_campaign = utm_campaign, utm_source = utm_source)
    return(results)
  }
}
