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
#'
#' @importFrom httr GET
#' @importFrom httr http_type
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#' single_url_simple_output_5 <- lh_simple_1("https://www.google.com/")
#' }
lh_simple_1 <- function(url, key = Sys.getenv("PAGESPEED_API_KEY"),
                        strategy = NULL, categories = "performance",
                        interval = 0.5,
                        locale = NULL,
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
  req <- GET(
    url = "https://www.googleapis.com/pagespeedonline/v5/runPagespeed",
    query = list(
      url = url, strategy = strategy, key = key, locale = locale,
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
  if (req$status_code == 200) {
    # 01 json check --------------------------------------------------------------
    if (http_type(req) != "application/json") {stop("API did not return json", call. = FALSE)}

    # 02 extracting from JSON -------------------------------------------------
    parsed  <- fromJSON(content(req, as = "text", type = "application/json", encoding = "UTF-8"))

    # 03 creating baseline data frame -----------------------------------------
    baseline <- data.frame(
      device           = `if`(is.null(strategy), "desktop", strategy),
      url              = url, #`if`(is.null(parsed$loadingExperience$initial_url), NA, parsed$loadingExperience$initial_url),
      status_code      = req$status_code,
      stringsAsFactors = FALSE)

    # 04 intermediary audit object -----------------------------------------------
    audits <- parsed$lighthouseResult$audits

    # 05 report category object --------------------------------------------------
    if ("performance" %in% categories) {
      pm <- data.frame(
        category = "performance",
        report_name = gsub("-", "_", parsed$lighthouseResult$categories$performance$auditRefs$id,
                           fixed = TRUE),
        stringsAsFactors = FALSE)
    }

    if ("accessibility" %in% categories) {
      acc <- data.frame(
        category = "accessibility",
        report_name = gsub("-", "_", parsed$lighthouseResult$categories$accessibility$auditRefs$id,
                           fixed = TRUE),
        stringsAsFactors = FALSE)
    }

    if ("best-practices" %in% categories) {
      bp <- data.frame(
        category = "best-practices",
        report_name = gsub("-", "_", parsed$lighthouseResult$categories$`best-practices`$auditRefs$id,
                           fixed = TRUE),
        stringsAsFactors = FALSE)
    }

    if ("pwa" %in% categories) {
      pwa <- data.frame(
        category = "pwa",
        report_name = gsub("-", "_", parsed$lighthouseResult$categories$pwa$auditRefs$id,
                           fixed = TRUE),
        stringsAsFactors = FALSE)
    }

    if ("seo" %in% categories) {
      seo <- data.frame(
        category = "seo",
        report_name = gsub("-", "_", parsed$lighthouseResult$categories$seo$auditRefs$id,
                           fixed = TRUE),
        stringsAsFactors = FALSE)
    }

    report_cat_df <- rbind(
      if ("performance" %in% categories) {pm},
      if ("accessibility" %in% categories) {acc},
      if ("best-practices" %in% categories) {bp},
      if ("pwa" %in% categories) {pwa},
      if ("seo" %in% categories) {seo}
    )

    # 06 basic lighthouse data extraction -------------------------------------
    # basic <- fun_lh_basic_extract(audits, report_cat_df)
    # basic <- pagespeedParseR:::fun_lh_basic_extract(audits, report_cat_df)
    results_load_exp <- fun_lh_enhanced_extract(parsed$loadingExperience)
    results_audits   <- fun_lh_enhanced_extract(audits, report_cat_df)
    full_results     <- cbind(baseline, results_load_exp, results_audits)

    # 07 missing columns in case of mobile ------------------------------------
    # if ((grepl("desktop", strategy) || is.null(strategy)) & "performance" %in% categories) {
    #   full_results$performance_first_contentful_paint_3g_description   <- NA
    #   full_results$performance_first_contentful_paint_3g_score         <- NA
    #   full_results$performance_first_contentful_paint_3g_display_value <- NA
    # }

    if ("performance" %in% categories) {full_results$score.performance <- parsed$lighthouseResult$categories$performance$score}
    if ("accessibility" %in% categories) {full_results$score.accessibility <- parsed$lighthouseResult$categories$accessibility$score}
    if ("best-practices" %in% categories) {full_results$score.best_practices <- parsed$lighthouseResult$categories$`best-practices`$score}
    if ("pwa" %in% categories) {full_results$score.pwa <- parsed$lighthouseResult$categories$pwa$score}
    if ("seo" %in% categories) {full_results$score.seo <- parsed$lighthouseResult$categories$seo$score}

    # 08 sorting the columns --------------------------------------------------
    colnames_v <- colnames(full_results) # capturing column names
    colnames_sorted <- sort(colnames_v) # sorting column names alphabetically

    # deleting baseline columns (they need to go first!)
    col_scores <- colnames_sorted[grepl("^score.*", colnames_sorted)] # main scores
    colnames_sorted <- colnames_sorted[!colnames_sorted %in% c('device', 'url', 'status_code', col_scores)]
    # adding baseline columns at the beggining of the df
    colnames_sorted <- c(c('device', 'url', 'status_code', col_scores), colnames_sorted)
    full_results <- full_results[, colnames_sorted] # choosing the df columns in order

    # full_results_tf1 <- tidyr::gather(full_results, parameter, value, -url)
    # full_results_tf2 <- tidyr::gather(full_results, parameter, value)
    # full_results_tf <- tidyr::gather(full_results, parameter, value, -url)
    # colnames(full_results_tf)[3] <- full_results_tf$url[1]
    # full_results_tf$url <- NULL

    # 10 returning ------------------------------------------------------------
    return(full_results)
  } else {
    # else NA df --------------------------------------------------------------
    # if there were no results, create placeholder to keep track which URL failed
    parsed  <- fromJSON(content(req, as = "text", type = "application/json", encoding = "UTF-8"))
    full_results <- data.frame(
      device           = `if`(is.null(strategy), "desktop", strategy),
      url              = url, #`if`(is.null(parsed$loadingExperience$initial_url), NA, parsed$loadingExperience$initial_url),
      status_code      = req$status_code,
      stringsAsFactors = FALSE)

    full_results$url <- as.character(full_results$url)
    Sys.sleep(0.5 + interval) # optional waiting to keep API limits happy
    return(full_results)
  }
}
