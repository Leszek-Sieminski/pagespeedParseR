#' Download Pagespeed v4 raport for an URL as a data frame
#'
#' @param url string. The URL to fetch and analyze
#' @param key string. Pagespeed API key to authenticate. Defaults to
#'     "PAGESPEED_API_KEY" enviroment variable.
#' @param strategy string. The analysis strategy to use. Options: "desktop" or
#'     "mobile". Defaults to "desktop"
#' @param categories string. A Lighthouse category/categories to run.
#'     Defaults to "performance". See more in Details section
#' @param interval numeric. Number of seconds to wait between multiple queries.
#'     Defaults to 0.5 second.
#' @param enhanced_lighthouse logical. If TRUE, adds even more columns with
#'     Lighthouse data. Defaults to FALSE
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
#' @return data frame
#' @export
#'
#' @examples
#' \dontrun{
#' single_url_simple_output_5 <- pagespeed_simple_v5("https://www.google.com/")
#' }
pagespeed_simple_v5 <- function(url, key = Sys.getenv("PAGESPEED_API_KEY"),
                                strategy = NULL, categories = "performance",
                                interval = 0.5,
                                enhanced_lighthouse = FALSE, locale = NULL,
                                utm_campaign = NULL, utm_source = NULL)
{
  url = "http://www.rennie.pl"
  key = Sys.getenv("PAGESPEED_API_KEY")
  strategy = NULL
  categories = "performance"
  interval = 0.5
  enhanced_lighthouse = FALSE
  locale = NULL
  utm_campaign = NULL
  utm_source = NULL

  # safety net ----------------------------------------------------------------
  if (is.null(key) | nchar(key) == 0){
    stop("API key is a NULL or has length = 0. Please check it and provide a proper API key.", call. = FALSE)}

  assert_that(not_empty(url), is.string(url), all(grepl(".", url, fixed = T)),
              is.string(key), is.character(strategy) | is.null(strategy),
              is.number(interval) & interval >= 0 & interval <= 120,
              is.logical(enhanced_lighthouse),
              is.vector(categories) | is.string(categories) | is.null(categories),
              is.string(locale) | is.null(locale),
              is.string(utm_campaign) | is.null(utm_campaign),
              is.string(utm_source) | is.null(utm_source))

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
  # httr::stop_for_status(req) # we don't want to stop for error
  # as we want to know which URL's wasn't properly returned
  if (req$status_code == 200){
    # 01 json check --------------------------------------------------------------
    if (httr::http_type(req) != "application/json") {stop("API did not return json", call. = FALSE)}

    # 02 extracting from JSON -------------------------------------------------
    parsed <- jsonlite::fromJSON(httr::content(req, "text"))

    # 04 creating baseline data frame -----------------------------------------
    baseline <- data.frame(
      device           = ifelse(is.null(strategy), "desktop", strategy),
      url              = ifelse(is.null(parsed$loadingExperience$initial_url), NA, parsed$loadingExperience$initial_url),
      status_code      = req$status_code,
      stringsAsFactors = FALSE)

    audits <- parsed$lighthouseResult$audits

    # 05 basic lighthouse data extraction -------------------------------------
    basic <- fun_lh_basic_extract(audits)
    basic <- basic[, -1]
    full_results <- cbind(baseline, basic)

    # 06 missing columns in case of mobile ------------------------------------
    if (grepl("desktop", strategy) || is.null(strategy)){
      full_results$first_contentful_paint_3g_description <- NA
      full_results$first_contentful_paint_3g_score <- NA
      full_results$first_contentful_paint_3g_display_value <- NA
    }

    # 07 sorting the columns --------------------------------------------------
    full_results <- fun_lh_basic_sort(full_results)

    # TODO create proper extraction from this nested list for all possible categories
    # 08 extra lighthouse data extraction -------------------------------------
    # if (extra) {
    #   details <- fun_lh_details_extract(audits)
    #   full_results <- cbind(full_results, details)
    # }

    # 09 extra columns sorting
    # if (extra) {
    #   full_results <- fun_lh_details_sort(full_results)
    # }

    # 10 returning ----------------------------------------------------------
    return(full_results)

  } else {
    # else NA df --------------------------------------------------------------
    # if there were no results, create placeholder to keep track which URL failed

    # TODO finish placeholder extended and uncomment these lines:
    if (!enhanced_lighthouse) { # basic export
      full_results <- v5_placeholder_basic()
    } # else {
    # full_results <- v5_placeholder_enhanced()
    # }

    Sys.sleep(interval) # optional waiting to keep API limits happy
    return(full_results)
  }
}

