#' Download Pagespeed v5 raport (Lighthouse) for multiple URLs as one data
#'    frame
#'
#' @description This function can check multiple URLs given in a vector
#'    and parse them into a one data frame. This data frame doesn't
#'    contain all the possible information from Pagespeed as response
#'    cannot be simply parsed into tabular form, but it contains
#'    most of the metrics, recommendations and error occurences.
#'
#' @details This function uses new version of the API (5th).
#'    Check function \code{ps_simple_2_vec()} for version 4
#'    (classic Pagespeed results).
#'    If you need all the information but in form of a nested list,
#'    use \code{lh_raw_2_vec()}.
#'
#'    Mind that resulting data frame can have
#'    literally hundreds/thousands of columns (depending on how many
#'    \code{categories}
#'    were selected). This is due to large amount of data returned by
#'    Lighthouse reports.
#'
#' @param url vector of character strings. The URLs to fetch and analyze
#' @param key string. Pagespeed API key to authenticate. Defaults to
#'     "PAGESPEED_API_KEY" enviroment variable
#' @param strategy string. The analysis strategy to use. Options: "desktop" or
#'     "mobile". Defaults to "desktop"
#' @param categories string. A Lighthouse category/categories to run.
#'     Defaults to "performance". See more in Details section
#' @param long_result logical. Should the resulting data frame be a long df?
#' @param interval numeric. Number of seconds to wait between multiple queries.
#'     Defaults to 0.5 second
#' @param locale string. The locale used to localize formatted results
#' @param utm_campaign string. Campaign name for analytics. Defaults to NULL
#' @param utm_source string. Campaign source for analytics. Defaults to NULL
#'
#' @return formatted data frame
#'
#' @import assertthat
#' @importFrom dplyr full_join
#' @importFrom tidyr gather
#' @importFrom stats reshape
#'
#' @examples
#' \dontrun{
#' multiple_urls_simple_output <- lh_simple_2_vec("https://www.google.com/")
#' }
lh_simple_2_vec <- function(
  url,
  key          = Sys.getenv("PAGESPEED_API_KEY"),
  strategy     = NULL,
  categories   = "performance",
  long_result  = TRUE,
  interval     = 0.5,
  locale       = NULL,
  utm_campaign = NULL,
  utm_source   = NULL)
{
  # safety net ----------------------------------------------------------------
  if (is.null(key) | nchar(key) == 0){stop("API key is a NULL or has length = 0. Please check it and provide a proper API key.", call. = FALSE)}

  assert_that(
    all(not_empty(url)), all(!is.null(url)),
    all(is.character(url)) & length(url) > 0, all(grepl(".", url, fixed = T)),
    is.string(key),
    all(!is.na(strategy)) & (is.null(strategy) ||
                               (is.character(strategy) & all(strategy %in% c("desktop", "mobile")))),
    is.null(categories) || (is.character(categories) & categories %in%
         c("accessibility", "best-practices", "performance", "pwa", "seo")),
    is.logical(long_result), is.number(interval) & interval >= 0 & interval <= 120,
    (is.string(locale) & nchar(locale) > 0) || is.null(locale),
    is.string(utm_campaign) | is.null(utm_campaign),
    is.string(utm_source)   | is.null(utm_source))

  # downloading ---------------------------------------------------------------
  if ("desktop" %in% strategy & "mobile" %in% strategy) {
    # simple df, both devices -------------------------------------------------
    desktop <- data.frame()
    for(i in 1:length(url)){
      res <- lh_simple_1(
        url = url[i],
        interval = interval,
        strategy = "desktop",
        key = key,
        categories = categories,
        locale = locale,
        utm_campaign = utm_campaign, utm_source = utm_source)

      if (i == 1){
        desktop <- res
      } else {
        desktop <- suppressMessages(full_join(desktop, res))
      }
    }

    Sys.sleep(1 + interval) # very simple time interval for saving API limits

    mobile <- data.frame()
    for(i in 1:length(url)){
      res <- lh_simple_1(
        url = url[i],
        interval = interval,
        strategy = "mobile",
        key = key,
        categories = categories,
        locale = locale,
        utm_campaign = utm_campaign, utm_source = utm_source)

      if (i == 1){
        mobile <- res
      } else {
        mobile <- suppressMessages(full_join(mobile, res))
      }
    }

    results <- suppressMessages(full_join(mobile, desktop))

    if (long_result) {
      results <- gather(results, "parameter", "value", -"url", -"device")
      results <- reshape(results, idvar = c('parameter', "device"), timevar = 'url', direction = 'wide', sep = "_",)
    }

    return(results)
  } else if (is.null(strategy) || grepl("desktop", strategy)) {

    # simple df, only desktop -------------------------------------------------
    results <- data.frame()
    for(i in 1:length(url)){
      res <- lh_simple_1(url = url[i],
                         interval = interval,
                         strategy = "desktop",
                         key = key,
                         categories = categories,
                         locale = locale,
                         utm_campaign = utm_campaign, utm_source = utm_source)

      if (i == 1){
        results <- res
      } else {
        results <- suppressMessages(full_join(results, res))
      }
    }

    if (long_result) {
      results <- gather(results, "parameter", "value", -"url", -"device")
      results <- reshape(results, idvar = c('parameter', "device"), timevar = 'url', direction = 'wide', sep = "_",)
    }

    return(results)
  } else if (grepl("mobile", strategy)) {

    # simple df, only mobile --------------------------------------------------
    results <- data.frame()
    for(i in 1:length(url)) {
      res <- lh_simple_1(
        url = url[i],
        interval = interval,
        strategy = "mobile",
        key = key,
        categories = categories,
        locale = locale,
        utm_campaign = utm_campaign, utm_source = utm_source)

      if (i == 1){
        results <- res
      } else {
        results <- suppressMessages(full_join(results, res))
      }
    }

    if (long_result) {
      results <- gather(results, "parameter", "value", -"url", -"device")
      results <- reshape(results, idvar = c('parameter', "device"), timevar = 'url', direction = 'wide', sep = "_",)
    }

    return(results)
  }
}
