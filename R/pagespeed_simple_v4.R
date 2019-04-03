#' Download Pagespeed v4 raport for an URL as a data frame
#'
#' @param url string. The URL to fetch and analyze
#' @param key string. Pagespeed API key to authenticate. Defaults to
#'     "PAGESPEED_API_KEY" enviroment variable.
#' @param strategy string. The analysis strategy to use. Options: "desktop" or
#'     "mobile". Defaults to "desktop"
#' @param interval numeric. Number of seconds to wait between multiple queries.
#'     Defaults to 0.5 second.
#' @param keep_tmp logical. Set to TRUE if you need to keep temporary Rdata file
#'     with parsed response. Defaults to FALSE
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
                                strategy = NULL, interval = 0.5, keep_tmp = FALSE,
                                filter_third_party = NULL,locale = NULL, rule = NULL,
                                screenshot = NULL, snapshots = NULL,
                                utm_campaign = NULL, utm_source = NULL)
{
  # safety net ----------------------------------------------------------------
  if (is.null(key) | nchar(key) == 0){
    stop("API key is a NULL or has length = 0. Please check it and provide a proper API key.", call. = FALSE)}

  assert_that(not_empty(url), is.string(url), grepl(".", url, fixed = T),
              is.string(key), is.character(strategy) | is.null(strategy),
              is.number(interval) & interval >= 0 & interval <= 120,
              is.logical(keep_tmp), is.string(filter_third_party),
              is.string(locale), is.string(rule), is.logical(screenshot),
              is.logical(snapshots), is.string(utm_campaign),
              is.string(utm_source))

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

    # 03 temporary file -------------------------------------------------------
    if (keep_tmp) { # saving tmp file for debugging in dev
      rnd <- paste0(
        do.call(paste0,
                replicate(n = 3, sample(x = LETTERS, size = 1, replace = TRUE), simplify = FALSE)),
        sprintf("%03d", sample(x = 999,  size = 1, replace = TRUE)),
        sample(x = LETTERS, size = 1, replace = TRUE))

      # save(parsed, file = paste0("tmp_", url, "_", Sys.Date(), "_",  rnd, ".RData"))
      save(parsed, file = paste0("tmp_",  rnd, ".RData"))
    }

    # 04 creating baseline data frame -----------------------------------------
    baseline <- data.frame(
      device           = ifelse(is.null(strategy), "desktop", strategy),
      title            = ifelse(is.null(parsed$title), NA, parsed$title),
      url              = ifelse(is.null(parsed$loadingExperience$initial_url), NA, parsed$loadingExperience$initial_url),
      status_code      = req$status_code,
      speed_score      = ifelse(is.null(parsed$ruleGroups$SPEED$score), NA, parsed$ruleGroups$SPEED$score),
      overall_category = ifelse(is.null(parsed$loadingExperience$overall_category), NA, parsed$loadingExperience$overall_category),
      stringsAsFactors = FALSE)
    # source("additional_data_ext.R", encoding = "UTF-8", local = TRUE)

    # 05 helper function ------------------------------------------------------
    source("fun_ps_url_extract.R", encoding = "UTF-8")

    # 06 first contentful paint -----------------------------------------------
    tmp_fcp <- parsed$loadingExperience$metrics$FIRST_CONTENTFUL_PAINT_MS
    fcp_main <- data.frame(fcp_median       = ifelse(is.null(tmp_fcp$median),   NA, tmp_fcp$median),
                           fcp_category     = ifelse(is.null(tmp_fcp$category), NA, tmp_fcp$category),
                           stringsAsFactors = FALSE)

    # 07 dom content load -----------------------------------------------------
    tmp_dcl <- parsed$loadingExperience$metrics$DOM_CONTENT_LOADED_EVENT_FIRED_MS
    dcl_main <- data.frame(dcl_median       = ifelse(is.null(tmp_dcl$median),   NA, tmp_dcl$median),
                           dcl_category     = ifelse(is.null(tmp_dcl$category), NA, tmp_dcl$category),
                           stringsAsFactors = FALSE)

    # 08 page stats -----------------------------------------------------------
    page_stats_df <- as.data.frame(parsed$pageStats)

    # 09 compression ----------------------------------------------------------
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

    # 10 caching --------------------------------------------------------------
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

    # 11 server_resp ----------------------------------------------------------
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

    # 12 redirects ------------------------------------------------------------
    redirects <- parsed$formattedResults$ruleResults$AvoidLandingPageRedirects
    redirects_impact   <- redirects$ruleImpact
    redirects_summary  <- redirects$localizedRuleName

    if ("urlBlocks" %in% names(redirects)){
      server_resp_recommendation <- redirects$summary$args$value
      # server_resp_recommendation <- ps_url_extract(server_resp_recommendation)
    } else {server_resp_recommendation <- NA}

    redirects_main <- data.frame(redirects_summary          = redirects_summary,
                                 redirects_impact           = redirects_impact,
                                 server_resp_recommendation = server_resp_recommendation,
                                 stringsAsFactors           = FALSE)

    # 13 minify_css -----------------------------------------------------------
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

    # 14 minify_html ----------------------------------------------------------
    minify_html <- parsed$formattedResults$ruleResults$MinifyHTML
    minify_html_impact <- minify_html$ruleImpact
    minify_html_summary <- gsub("{{BEGIN_LINK}}", "", minify_html$summary$format, fixed = TRUE)
    minify_html_summary <- gsub("{{END_LINK}}", "", minify_html_summary, fixed = TRUE)

    if ("urlBlocks" %in% names(minify_html)) {
      minify_html_url <- minify_html$urlBlocks$urls[[1]]$result$args
      minify_html_url <- ps_url_extract(minify_html_url)
    } else {minify_css_url <- NA}

    minify_html_main <- data.frame(minify_html_summary = minify_html_summary,
                                   minify_html_impact  = minify_html_impact,
                                   minify_css_url      = minify_css_url,
                                   stringsAsFactors    = FALSE)

    # 15 minify_js  -----------------------------------------------------------
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

    # 16 render_block ---------------------------------------------------------
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

    # 17 images ---------------------------------------------------------------
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

    # 18 visible --------------------------------------------------------------
    visible <- parsed$formattedResults$ruleResults$PrioritizeVisibleContent
    visible_impact <- visible$ruleImpact
    visible_summary <- gsub("{{BEGIN_LINK}}", "", visible$summary$format, fixed = TRUE)
    visible_summary <- gsub("{{END_LINK}}.", ": ", visible_summary, fixed = TRUE)
    visible_summary <- paste0(visible_summary, visible$summary$args$value)

    if ("urlBlocks" %in% names(visible)) {
      visible_url <- visible$urlBlocks$urls[[1]]$result$args
      visible_url <- ps_url_extract(visible_url)
    } else {visible_url <- NA}

    visible_main <- data.frame(visible_summary  = visible_summary,
                               visible_impact   = visible_impact,
                               visible_url      = visible_url,
                               stringsAsFactors = FALSE)

    # 19 binding results together ---------------------------------------------
    full_results <- cbind(baseline, page_stats_df, fcp_main, dcl_main, compression_main,
                          caching_main, server_resp_main, redirects_main, minify_css_main,
                          minify_html_main, minify_js_main, render_block_main, images_main,
                          visible_main, stringsAsFactors = FALSE)

    full_results$totalRequestBytes        <- as.integer(as.character(full_results$totalRequestBytes))
    full_results$htmlResponseBytes        <- as.integer(as.character(full_results$htmlResponseBytes))
    full_results$overTheWireResponseBytes <- as.integer(as.character(full_results$overTheWireResponseBytes))
    full_results$cssResponseBytes         <- as.integer(as.character(full_results$cssResponseBytes))
    full_results$imageResponseBytes       <- as.integer(as.character(full_results$imageResponseBytes))
    full_results$javascriptResponseBytes  <- as.integer(as.character(full_results$javascriptResponseBytes))
    full_results$otherResponseBytes       <- as.integer(as.character(full_results$otherResponseBytes))

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
      server_resp_summary = NA, server_resp_impact = NA,
      server_resp_recommendation = NA, redirects_summary = NA,
      redirects_impact = NA, server_resp_recommendation = NA,
      minify_css_summary = NA, minify_css_impact = NA, minify_css_url = NA,
      minify_html_summary = NA, minify_html_impact = NA,
      minify_css_url = NA, minify_js_summary = NA, minify_js_impact = NA,
      minify_js_url = NA, render_block_summary = NA,
      render_block_impact = NA, render_block_url = NA, images_summary = NA,
      images_impact = NA, images_url = NA, visible_summary = NA,
      visible_impact = NA, visible_url = NA)
    Sys.sleep(interval) # optional waiting to keep API limits happy
    return(full_results)
  }
}
