#' Download Pagespeed v5 raport (Lighthouse) for multiple URLs as a one nested list
#'
#' @description This function can check multiple URLs (character vector)
#'    and parse the output into a data frame. This data frame contain all
#'    the possible information from Pagespeed ver 5.
#'
#' @details This function uses legacy version 4 of the API.
#'    Check function \code{lh_raw_2_vec()} for version 5.
#'    If you need less information but in form of a data frame,
#'    use \code{ps_simple_2_vec()}.
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
#' @param locale string. The locale used to localize formatted results
#' @param utm_campaign string. Campaign name for analytics. Defaults to NULL
#' @param utm_source string. Campaign source for analytics. Defaults to NULL
#'
#' @return unformatted nested list
#'
#' @import assertthat
#'
#' @examples
#' \dontrun{
#' multiple_urls_raw_output <- lh_raw_2_vec("https://www.google.com/")
#' }
lh_raw_2_vec <- function(
  url, key = Sys.getenv("PAGESPEED_API_KEY"),
  strategy = NULL, categories = "performance",
  interval = 0.5, locale = NULL,
  utm_campaign = NULL, utm_source = NULL)
{
  # safety net ----------------------------------------------------------------
  if (is.null(key) | nchar(key) == 0){stop("API key is a NULL or has length = 0. Please check it and provide a proper API key.", call. = FALSE)}

  assert_that(all(not_empty(url)), all(!is.null(url)), all(is.character(url)) & length(url) > 0, all(grepl(".", url, fixed = T)),
              is.string(key),
              all(!is.na(strategy)) & (is.null(strategy) || (is.character(strategy) & all(strategy %in% c("desktop", "mobile")))),
              is.null(categories) ||
                (is.character(categories) & categories %in%
                   c("accessibility", "best-practices", "performance", "pwa", "seo")),
              is.number(interval) & interval >= 0 & interval <= 120,
              (is.string(locale) & nchar(locale) > 0) || is.null(locale),
              is.string(utm_campaign) | is.null(utm_campaign),
              is.string(utm_source)   | is.null(utm_source))

  if ("desktop" %in% strategy & "mobile" %in% strategy) {
    # nested list, both devices ------------------------------------------------------------
    desktop <- purrr::map(
      .x = url,
      .f = lh_raw_1,
      strategy = "desktop", key = key, interval = interval,
      categories = categories, locale = locale,
      utm_campaign = utm_campaign, utm_source = utm_source)

    Sys.sleep(1 + interval) # very simple time interval for saving API limits

    mobile <- purrr::map(
      .x = url,
      .f = lh_raw_1,
      strategy = "mobile", interval = interval, key = key,
      categories = categories, locale = locale,
      utm_campaign = utm_campaign, utm_source = utm_source)

    results <- list(
      "desktop" = desktop,
      "mobile"  = mobile
    )

    # results <- c(desktop, mobile)
    return(results)
  } else if (is.null(strategy) || grepl("desktop", strategy)) {

    # nested list, only desktop --------------------------------------------------------------
    results <- purrr::map(
      .x = url,
      .f = lh_raw_1,
      strategy = "desktop", interval = interval, key = key,
      categories = categories, locale = locale,
      utm_campaign = utm_campaign, utm_source = utm_source)
    return(results)
  } else if (grepl("mobile", strategy)) {

    # nested list, only mobile ---------------------------------------------------------------
    results <- purrr::map(
      .x = url,
      .f = lh_raw_1,
      strategy = "mobile", interval = interval, key = key,
      categories = categories, locale = locale,
      utm_campaign = utm_campaign, utm_source = utm_source)
    return(results)
  }
}
