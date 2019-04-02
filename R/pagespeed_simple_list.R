#' Title
#'
#' @param url
#' @param strategy
#' @param interval
#' @param api_version
#' @param keep_tmp
#' @param key
#' @param filter_third_party
#' @param locale
#' @param rule
#' @param screenshot
#' @param snapshots
#' @param utm_campaign
#' @param utm_source
#' @param fields
#'
#' @return
#' @export
#'
#' @examples
pagespeed_simple_list <- function(
  url, strategy = NULL, interval = 0.5, api_version = 4, keep_tmp = FALSE,
  key = Sys.getenv("PAGESPEED_API_KEY"), filter_third_party = NULL,
  locale = NULL, rule = NULL, screenshot = NULL, snapshots = NULL,
  utm_campaign = NULL, utm_source = NULL, fields = NULL)
{
  url <- "https://www.rennie.pl/" # "https://www.wp.pl" # "https://zVCSDVSDvSDcv.ok"
  # # url_list <- c("https://www.wp.pl", "https://www.onet.pl/")
  key = Sys.getenv("PAGESPEED_API_KEY")
  api_version = 4
  interval = 0.5
  keep_tmp = TRUE # FALSE
  filter_third_party = NULL
  locale = NULL
  rule = NULL
  screenshot = NULL
  snapshots = NULL
  strategy = NULL
  utm_campaign = NULL
  utm_source = NULL
  fields = NULL
  if ("desktop" %in% strategy & "mobile" %in% strategy) {
    # simple df, both devices ------------------------------------------------------------
    desktop <- purrr::map_dfr(
      .x = url,
      .f = if (api_version == 4) {pagespeed_simple_v4} else if (api_version == 5) {pagespeed_simple_v5},
      # .f = pagespeed_simple_v4,
      # .f = switch(api_version,
      #             "4" = {pagespeed_simple_v4},
      #             "5" = {pagespeed_simple_v5}),
      strategy = "desktop", key = key, interval = interval,
      filter_third_party = filter_third_party, locale = locale, rule = rule,
      screenshot = screenshot, snapshots = snapshots,
      utm_campaign = utm_campaign, utm_source = utm_source,
      fields = fields)

    Sys.sleep(1 + interval) # very simple time interval for saving API limits

    mobile <- purrr::map_dfr(
      .x = url,
      .f = if (api_version == 4) {pagespeed_simple_v4} else if (api_version == 5) {pagespeed_simple_v5},
      # .f = pagespeed_simple_v4,
      # .f = switch(api_version,
      #             "4" = {pagespeed_simple_v4},
      #             "5" = {pagespeed_simple_v5}),
      interval = interval, strategy = "mobile", key = key,
      filter_third_party = filter_third_party, locale = locale, rule = rule,
      screenshot = screenshot, snapshots = snapshots,
      utm_campaign = utm_campaign, utm_source = utm_source,
      fields = fields)

    results <- rbind(desktop, mobile)
    return(results)
  } else if (is.null(strategy) || grepl("desktop", strategy)) {

    # simple df, only desktop --------------------------------------------------------------
    results <- purrr::map_dfr(
      .x = url,
      .f = if (api_version == 4) {pagespeed_simple_v4} else if (api_version == 5) {pagespeed_simple_v5},
      # .f = pagespeed_simple_v4,
      # .f = switch(api_version,
      #             "4" = {pagespeed_simple_v4},
      #             "5" = {pagespeed_simple_v5}),
      interval = interval, strategy = "desktop", key = key,
      filter_third_party = filter_third_party, locale = locale, rule = rule,
      screenshot = screenshot, snapshots = snapshots,
      utm_campaign = utm_campaign, utm_source = utm_source,
      fields = fields)
    return(results)
  } else if (grepl("mobile", strategy)) {

    # simple df, only mobile ---------------------------------------------------------------
    results <- purrr::map_dfr(
      .x = url,
      .f = if (api_version == 4) {pagespeed_simple_v4} else if (api_version == 5) {pagespeed_simple_v5},
      # .f = pagespeed_simple_v4,
      # .f = switch(api_version,
      #             "4" = {pagespeed_simple_v4},
      #             "5" = {pagespeed_simple_v5}),
      interval = interval, strategy = "mobile", key = key,
      filter_third_party = filter_third_party, locale = locale, rule = rule,
      screenshot = screenshot, snapshots = snapshots,
      utm_campaign = utm_campaign, utm_source = utm_source,
      fields = fields)
    return(results)
  }
}
