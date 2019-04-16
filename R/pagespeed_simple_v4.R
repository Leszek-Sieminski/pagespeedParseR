#' Download Pagespeed v4 raport for an URL as a data frame
#'
#' @description This function can check a single URL (character) and parse
#'    the output into a data frame. This data frame contain all the possible
#'    information from Pagespeed ver 4.
#'
#' @details This function uses legacy version 4 of the API.
#'    Check function \code{pagespeed_raw_v5} for version 5.
#'    If you need all the information but in form of a nested list,
#'    use \code{pagespeed_raw_v4}.
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
#' @return formatted data frame
#'
#' @import assertthat
#'
#' @examples
#' \dontrun{
#' single_url_simple_output <- pagespeed_simple_v4("https://www.google.com/")
#' }
pagespeed_simple_v4 <- function(url, key = Sys.getenv("PAGESPEED_API_KEY"),
                                strategy = NULL, interval = 0.5,
                                filter_third_party = NULL,locale = NULL, rule = NULL,
                                screenshot = NULL, snapshots = NULL,
                                utm_campaign = NULL, utm_source = NULL)
{
  # safety net ----------------------------------------------------------------
  if (is.null(key) | nchar(key) == 0){
    stop("API key is a NULL or has length = 0. Please check it and provide a proper API key.", call. = FALSE)}

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

  # downloading ---------------------------------------------------------------
  req <- httr::GET(
    url = "https://www.googleapis.com/pagespeedonline/v4/runPagespeed",
    query = list(url = url, strategy = strategy, key = key, locale = locale,
                 filter_third_party = filter_third_party, rule = rule,
                 screenshot = screenshot, snapshots = snapshots,
                 utm_campaign = utm_campaign, utm_source = utm_source))

  # parsing -------------------------------------------------------------------
  # httr::stop_for_status(req) # we don't want to stop for error
  # as we want to know which URL's wasn't properly returned
  if (req$status_code == 200){
    # 01 json check --------------------------------------------------------------
    if (httr::http_type(req) != "application/json") {stop("API did not return json", call. = FALSE)}

    # 02 extracting from JSON -------------------------------------------------
    parsed <- jsonlite::fromJSON(httr::content(req, "text"))

    # 03 creating baseline data frame -----------------------------------------
    baseline <- data.frame(
      device           = ifelse(is.null(strategy), "desktop", strategy),
      title            = ifelse(is.null(parsed$title), NA, parsed$title),
      url              = ifelse(is.null(parsed$loadingExperience$initial_url), NA, parsed$loadingExperience$initial_url),
      status_code      = req$status_code,
      speed_score      = ifelse(is.null(parsed$ruleGroups$SPEED$score), NA, parsed$ruleGroups$SPEED$score),
      overall_category = ifelse(is.null(parsed$loadingExperience$overall_category), NA, parsed$loadingExperience$overall_category),
      stringsAsFactors = FALSE)

    # 04 first contentful paint -----------------------------------------------
    tmp_fcp <- parsed$loadingExperience$metrics$FIRST_CONTENTFUL_PAINT_MS
    fcp_main <- data.frame(fcp_median       = ifelse(is.null(tmp_fcp$median),   NA, tmp_fcp$median),
                           fcp_category     = ifelse(is.null(tmp_fcp$category), NA, tmp_fcp$category),
                           stringsAsFactors = FALSE)

    # 05 dom content load -----------------------------------------------------
    tmp_dcl <- parsed$loadingExperience$metrics$DOM_CONTENT_LOADED_EVENT_FIRED_MS
    dcl_main <- data.frame(dcl_median       = ifelse(is.null(tmp_dcl$median),   NA, tmp_dcl$median),
                           dcl_category     = ifelse(is.null(tmp_dcl$category), NA, tmp_dcl$category),
                           stringsAsFactors = FALSE)

    # 06 page stats -----------------------------------------------------------
    page_stats_df <- as.data.frame(parsed$pageStats, stringsAsFactors = FALSE)

    # 07 compression ----------------------------------------------------------
    compression <- parsed$formattedResults$ruleResults$EnableGzipCompression
    compression_impact <- compression$ruleImpact
    compression_summary <- gsub("{{BEGIN_LINK}}", "", compression$summary$format, fixed = TRUE)
    compression_summary <- gsub("{{END_LINK}}.", ": ", compression_summary, fixed = TRUE)
    compression_summary <- paste0(compression_summary, compression$summary$args$value)

    if ("urlBlocks" %in% names(compression)){
      compression_urls <- compression$summary$args
      compression_urls <- ps_url_extract(compression_urls)
    } else {compression_urls <- NA}

    compression_main <- data.frame(compression_summary = compression_summary,
                                   compression_impact  = compression_impact,
                                   compression_urls    = compression_urls,
                                   stringsAsFactors    = FALSE)

    # 08 caching --------------------------------------------------------------
    caching <- parsed$formattedResults$ruleResults$LeverageBrowserCaching
    caching_impact   <- caching$ruleImpact
    caching_summary <- paste0(caching$summary$format, caching$summary$args$value)

    if ("urlBlocks" %in% names(caching)){
      cache_urls <- caching$urlBlocks$urls[[1]]$result$args
      cache_urls <- ps_url_extract(cache_urls)
    } else {cache_urls <- NA}

    caching_main <- data.frame(caching_summary  = caching_summary,
                               caching_impact   = caching_impact,
                               caching_urls     = cache_urls,
                               stringsAsFactors = FALSE)

    # 09 server_resp ----------------------------------------------------------
    server_resp <- parsed$formattedResults$ruleResults$MainResourceServerResponseTime
    server_resp_impact   <- server_resp$ruleImpact
    server_resp_summary  <- server_resp$localizedRuleName

    if ("urlBlocks" %in% names(server_resp)){
      server_resp_recommendation <- server_resp$urlBlocks$header$args
      server_resp_recommendation <- ps_url_extract(server_resp_recommendation)
    } else {server_resp_recommendation <- NA}

    server_resp_main <- data.frame(server_resp_summary        = server_resp_summary,
                                   server_resp_impact         = server_resp_impact,
                                   server_resp_recommendation = server_resp_recommendation,
                                   stringsAsFactors           = FALSE)

    # 10 redirects ------------------------------------------------------------
    redirects <- parsed$formattedResults$ruleResults$AvoidLandingPageRedirects
    redirects_impact   <- redirects$ruleImpact
    redirects_summary  <- redirects$localizedRuleName

    if ("urlBlocks" %in% names(redirects)){
      redirects_recommendation <- redirects$summary$args$value
      # server_resp_recommendation <- ps_url_extract(server_resp_recommendation)
    } else {redirects_recommendation <- NA}

    redirects_main <- data.frame(redirects_summary        = redirects_summary,
                                 redirects_impact         = redirects_impact,
                                 redirects_recommendation = redirects_recommendation,
                                 stringsAsFactors         = FALSE)

    # 11 minify_css -----------------------------------------------------------
    minify_css <- parsed$formattedResults$ruleResults$MinifyCss
    minify_css_impact   <- minify_css$ruleImpact
    minify_css_summary <- gsub("{{BEGIN_LINK}}", "", minify_css$summary$format, fixed = TRUE)
    minify_css_summary <- gsub("{{END_LINK}}", "", minify_css_summary, fixed = TRUE)

    if ("urlBlocks" %in% names(minify_css)) {
      minify_css_url <- minify_css$urlBlocks$urls[[1]]$result$args
      minify_css_url <- ps_url_extract(minify_css_url)
    } else {minify_css_url <- NA}

    minify_css_main <- data.frame(minify_css_summary = minify_css_summary,
                                  minify_css_impact  = minify_css_impact,
                                  minify_css_url     = minify_css_url,
                                  stringsAsFactors   = FALSE)

    # 12 minify_html ----------------------------------------------------------
    minify_html <- parsed$formattedResults$ruleResults$MinifyHTML
    minify_html_impact <- minify_html$ruleImpact
    minify_html_summary <- gsub("{{BEGIN_LINK}}", "", minify_html$summary$format, fixed = TRUE)
    minify_html_summary <- gsub("{{END_LINK}}", "", minify_html_summary, fixed = TRUE)

    if ("urlBlocks" %in% names(minify_html)) {
      minify_html_url <- minify_html$urlBlocks$urls[[1]]$result$args
      minify_html_url <- ps_url_extract(minify_html_url)
    } else {minify_html_url <- NA}

    minify_html_main <- data.frame(minify_html_summary = minify_html_summary,
                                   minify_html_impact  = minify_html_impact,
                                   minify_html_url      = minify_html_url,
                                   stringsAsFactors    = FALSE)

    # 13 minify_js  -----------------------------------------------------------
    minify_js <- parsed$formattedResults$ruleResults$MinifyJavaScript
    minify_js_impact <- minify_js$ruleImpact
    minify_js_summary <- gsub("{{BEGIN_LINK}}", "", minify_js$summary$format, fixed = TRUE)
    minify_js_summary <- gsub("{{END_LINK}}", "", minify_js_summary, fixed = TRUE)

    if ("urlBlocks" %in% names(minify_js)) {
      minify_js_url <- minify_js$urlBlocks$urls[[1]]$result$args
      minify_js_url <- ps_url_extract(minify_js_url)
    } else {minify_js_url <- NA}

    minify_js_main <- data.frame(minify_js_summary = minify_js_summary,
                                 minify_js_impact  = minify_js_impact,
                                 minify_js_url     = minify_js_url,
                                 stringsAsFactors  = FALSE)

    # 14 render_block ---------------------------------------------------------
    render_block <- parsed$formattedResults$ruleResults$MinimizeRenderBlockingResources
    render_block_impact <- render_block$ruleImpact
    render_block_summary <- gsub("{{NUM_CSS}} ", "", render_block$summary$format, fixed = T)
    render_block_summary <- gsub("{{NUM_SCRIPTS}} ", "", render_block_summary, fixed = T)

    if ("urlBlocks" %in% names(render_block)) {
      render_block_url <- render_block$urlBlocks$urls[[2]]$result$args
      render_block_url <- ps_url_extract(render_block_url)
    } else {render_block_url <- NA}

    render_block_main <- data.frame(render_block_summary = render_block_summary,
                                    render_block_impact  = render_block_impact,
                                    render_block_url     = render_block_url,
                                    stringsAsFactors     = FALSE)

    # 15 images ---------------------------------------------------------------
    images <- parsed$formattedResults$ruleResults$OptimizeImages
    images_impact <- images$ruleImpact
    images_summary <- gsub("{{BEGIN_LINK}}", "", images$summary$format, fixed = TRUE)
    images_summary <- gsub("{{END_LINK}}", "", images_summary, fixed = TRUE)

    if ("urlBlocks" %in% names(images)) {
      images_url <- images$urlBlocks$urls[[1]]$result$args
      images_url <- ps_url_extract(images_url)
    } else {images_url <- NA}

    images_main <- data.frame(images_summary   = images_summary,
                              images_impact    = images_impact,
                              images_url       = images_url,
                              stringsAsFactors = FALSE)

    # 16 visible --------------------------------------------------------------
    visible <- parsed$formattedResults$ruleResults$PrioritizeVisibleContent
    visible_impact <- visible$ruleImpact
    visible_summary <- gsub("{{BEGIN_LINK}}", "", visible$summary$format, fixed = TRUE)
    visible_summary <- gsub("{{END_LINK}}.", ": ", visible_summary, fixed = TRUE)
    visible_summary <- paste0(visible_summary, visible$summary$args$value)

    # TODO repair visible_url error when there is an url in the list (empty)
    if ("urlBlocks" %in% names(visible)) {
      visible_url <- visible$urlBlocks$urls[[1]]$result$args
      visible_url <- ps_url_extract(visible_url)
    } else {visible_url <- NA}

    visible_main <- data.frame(visible_summary  = visible_summary,
                               visible_impact   = visible_impact,
                               visible_url      = visible_url,
                               stringsAsFactors = FALSE)

    # 17 dealing with missing in stats when no JS/CSS -------------------------
    if ("totalRequestBytes" %in% names(page_stats_df)) {
      page_stats_df$totalRequestBytes <- as.integer(as.character(page_stats_df$totalRequestBytes))
    } else {
      page_stats_df$totalRequestBytes <- NA
    }

    if ("htmlResponseBytes" %in% names(page_stats_df)) {
      page_stats_df$htmlResponseBytes <- as.integer(as.character(page_stats_df$htmlResponseBytes))
    } else {
      page_stats_df$htmlResponseBytes <- NA
    }

    if ("overTheWireResponseBytes" %in% names(page_stats_df)) {
      page_stats_df$overTheWireResponseBytes <- as.integer(as.character(page_stats_df$overTheWireResponseBytes))
    } else {
      page_stats_df$cssResponseBytes <- NA
    }

    if ("cssResponseBytes" %in% names(page_stats_df)) {
      page_stats_df$cssResponseBytes <- as.integer(as.character(page_stats_df$cssResponseBytes))
    } else {
      page_stats_df$cssResponseBytes <- NA
    }

    if ("imageResponseBytes" %in% names(page_stats_df)) {
      page_stats_df$imageResponseBytes <- as.integer(as.character(page_stats_df$imageResponseBytes))
    } else {
      page_stats_df$imageResponseBytes <- NA
    }

    if ("javascriptResponseBytes" %in% names(page_stats_df)) {
      page_stats_df$javascriptResponseBytes <- as.integer(as.character(page_stats_df$javascriptResponseBytes))
    } else {
      page_stats_df$javascriptResponseBytes <- NA
    }

    if ("otherResponseBytes" %in% names(page_stats_df)) {
      page_stats_df$otherResponseBytes <- as.integer(as.character(page_stats_df$otherResponseBytes))
    } else {
      page_stats_df$page_stats_df$otherResponseBytes <- NA
    }

    if ("numberJsResources" %in% names(page_stats_df)) {
      page_stats_df$numberJsResources <- as.integer(as.character(page_stats_df$numberJsResources))
    } else {
      page_stats_df$numberJsResources <- NA
    }

    if ("numberCssResources" %in% names(page_stats_df)) {
      page_stats_df$numberCssResources <- as.integer(as.character(page_stats_df$numberCssResources))
    } else {
      page_stats_df$numberCssResources <- NA
    }

    # 18 binding results together ---------------------------------------------
    full_results <- cbind(baseline, page_stats_df, fcp_main, dcl_main, compression_main,
                          caching_main, server_resp_main, redirects_main, minify_css_main,
                          minify_html_main, minify_js_main, render_block_main, images_main,
                          visible_main, stringsAsFactors = FALSE)

    # 19 sorting the df -------------------------------------------------------
    full_results <- fun_lh_basic_sort(full_results)

    # 20 returning ------------------------------------------------------------
    return(full_results)
  } else {
    # else NA df --------------------------------------------------------------
    # if there were no results, create placeholder to
    # keep track which URL failed
    full_results <- data.frame(
      device = ifelse(is.null(strategy), "desktop", strategy), title = NA,
      url = url, status_code = req$status_code, speed_score = NA,
      overall_category = NA, numberResources = NA, numberHosts = NA,
      totalRequestBytes = NA, numberStaticResources = NA, htmlResponseBytes = NA,
      overTheWireResponseBytes = NA, cssResponseBytes = NA,
      imageResponseBytes = NA, javascriptResponseBytes = NA,
      otherResponseBytes = NA, numberJsResources = NA, numberCssResources = NA,
      numTotalRoundTrips = NA, numRenderBlockingRoundTrips = NA, fcp_median = NA,
      fcp_category = NA, dcl_median = NA, dcl_category = NA,
      compression_summary = NA, compression_impact = NA, compression_urls = NA,
      caching_summary = NA, caching_impact = NA, caching_urls = NA,
      server_resp_summary = NA, server_resp_impact = NA, server_resp_recommendation = NA,
      redirects_summary = NA, redirects_impact = NA, # redirects_url = NA,
      minify_css_summary = NA, minify_css_impact = NA, minify_css_url = NA,
      minify_html_summary = NA, minify_html_impact = NA, minify_html_url = NA,
      minify_js_summary = NA, minify_js_impact = NA, minify_js_url = NA,
      render_block_summary = NA, render_block_impact = NA, render_block_url = NA,
      images_summary = NA, images_impact = NA, images_url = NA,
      visible_summary = NA, visible_impact = NA, visible_url = NA,
      stringsAsFactors = FALSE)
    Sys.sleep(interval) # optional waiting to keep API limits happy
    return(full_results)
  }
}
