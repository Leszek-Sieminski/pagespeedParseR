#' Extraction of enhanced details from Lighthouse reports
#'
#' @param audits audits object
#' @param categories Lighthouse categories to be run
#' @param report_cat_df reports category object
#'
#' @import glue
#'
#' @return data frame with enhanced columns
fun_lh_enhanced_extract <- function(audits, categories, report_cat_df){
  # creating empty list for storing the data ----------------------------------
  results <- list()

  # extraction from the audits nested list ------------------------------------
  for (i in 1:length(audits)) {
    # extraction of raw data
    x <- list(details = if (!is.null(audits[[i]]$details)) {audits[[i]]$details} else {NA})
    # adding proper name
    names(x) <- paste0(gsub("-", "_", audits[[i]]$id))
    results <- c(results, x)
  }

  # empty df ------------------------------------------------------------------
  results_df <- data.frame(1)

  # performance ---------------------------------------------------------------
  # gsub("-", "_", parsed$lighthouseResult$categories$performance$auditRefs$id)
  if ("performance" %in% categories) {

    # network_requests----------------------------------
    results_df$network_requests_url_vs_resource_size <- paste0(results$network_requests$items$url, " : ", results$network_requests$items$resourceSize, collapse = ", ")

    # efficient_animated_content----------------------------------
    results_df$efficient_animated_content_urls <- if (fun_lhex("efficient_animated_content","url")) {paste0(results$efficient_animated_content$items$url, collapse = ', ')} else {NA}
    results_df$efficient_animated_content_overall_savings_bytes <- if (fun_lhex("efficient_animated_content","overallSavingsBytes")) {paste0(results$efficient_animated_content$overallSavingsBytes, collapse = ', ')} else {NA}
    results_df$efficient_animated_content_overall_savings_ms <- if (fun_lhex("efficient_animated_content","overallSavingsMs")) {paste0(results$efficient_animated_content$overallSavingsMs, collapse = ', ')} else {NA}


    # screenshot_thumbnails----------------------------------
    results_df$screenshot_thumbnails_timestamp <- if (fun_lhex("screenshot_thumbnails","timestamp")) {paste0(results$screenshot_thumbnails$items$timestamp, collapse = ', ')} else {NA}
    results_df$screenshot_thumbnails_data <- if (fun_lhex("screenshot_thumbnails","data")) {paste0(results$screenshot_thumbnails$items$data, collapse = ', ')} else {NA}
    results_df$screenshot_thumbnails_timing <- if (fun_lhex("screenshot_thumbnails","timing")) {paste0(results$screenshot_thumbnails$items$timing, collapse = ', ')} else {NA}

    # interactive----------------------------------
    results_df$interactive_urls <- if (fun_lhex("interactive","url")) {paste0(results$interactive$items$url, collapse = ', ')} else {NA}
    # results_df$interactive_overall_savings_bytes <- if (fun_lhex("interactive","overallSavingsBytes")) {paste0(results$interactive$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$interactive_overall_savings_ms <- if (fun_lhex("interactive","overallSavingsMs")) {paste0(results$interactive$overallSavingsMs, collapse = ', ')} else {NA}

    # uses_long_cache_ttl----------------------------------
    results_df$uses_long_cache_ttl_url_vs_wastedBytes <- paste0(results$network_requests$items$url, " : ", results$network_requests$items$resourceSize, collapse = ", ")

    # total_byte_weight----------------------------------
    results_df$total_byte_weight_url_vs_total_bytes <- paste0(results$total_byte_weight$items$url, " : ", results$total_byte_weight$items$totalBytes, collapse = ", ")

    # uses_text_compression----------------------------------
    results_df$uses_text_compression_urls <- if (fun_lhex("uses_text_compression","url")) {paste0(results$uses_text_compression$items$url, collapse = ', ')} else {NA}
    results_df$uses_text_compression_overall_savings_bytes <- if (fun_lhex("uses_text_compression","overallSavingsBytes")) {paste0(results$uses_text_compression$overallSavingsBytes, collapse = ', ')} else {NA}
    results_df$uses_text_compression_overall_savings_ms <- if (fun_lhex("uses_text_compression","overallSavingsMs")) {paste0(results$uses_text_compression$overallSavingsMs, collapse = ', ')} else {NA}


    # first_contentful_paint----------------------------------
    # results_df$first_contentful_paint_urls <- if (fun_lhex("first_contentful_paint","url")) {paste0(results$first_contentful_paint$items$url, collapse = ', ')} else {NA}
    # results_df$first_contentful_paint_overall_savings_bytes <- if (fun_lhex("first_contentful_paint","overallSavingsBytes")) {paste0(results$first_contentful_paint$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$first_contentful_paint_overall_savings_ms <- if (fun_lhex("first_contentful_paint","overallSavingsMs")) {paste0(results$first_contentful_paint$overallSavingsMs, collapse = ', ')} else {NA}

    # first_meaningful_paint----------------------------------
    results_df$first_meaningful_paint_urls <- if (fun_lhex("first_meaningful_paint","url")) {paste0(results$first_meaningful_paint$items$url, collapse = ', ')} else {NA}
    # results_df$first_meaningful_paint_overall_savings_bytes <- if (fun_lhex("first_meaningful_paint","overallSavingsBytes")) {paste0(results$first_meaningful_paint$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$first_meaningful_paint_overall_savings_ms <- if (fun_lhex("first_meaningful_paint","overallSavingsMs")) {paste0(results$first_meaningful_paint$overallSavingsMs, collapse = ', ')} else {NA}

    # speed_index----------------------------------
    results_df$speed_index_urls <- if (fun_lhex("speed_index","url")) {paste0(results$speed_index$items$url, collapse = ', ')} else {NA}
    # results_df$speed_index_overall_savings_bytes <- if (fun_lhex("speed_index","overallSavingsBytes")) {paste0(results$speed_index$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$speed_index_overall_savings_ms <- if (fun_lhex("speed_index","overallSavingsMs")) {paste0(results$speed_index$overallSavingsMs, collapse = ', ')} else {NA}



    # first_cpu_idle----------------------------------
    results_df$first_cpu_idle_urls <- if (fun_lhex("first_cpu_idle","url")) {paste0(results$first_cpu_idle$items$url, collapse = ', ')} else {NA}
    # results_df$first_cpu_idle_overall_savings_bytes <- if (fun_lhex("first_cpu_idle","overallSavingsBytes")) {paste0(results$first_cpu_idle$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$first_cpu_idle_overall_savings_ms <- if (fun_lhex("first_cpu_idle","overallSavingsMs")) {paste0(results$first_cpu_idle$overallSavingsMs, collapse = ', ')} else {NA}

    # estimated_input_latency----------------------------------
    results_df$estimated_input_latency_urls <- if (fun_lhex("estimated_input_latency","url")) {paste0(results$estimated_input_latency$items$url, collapse = ', ')} else {NA}
    # results_df$estimated_input_latency_overall_savings_bytes <- if (fun_lhex("estimated_input_latency","overallSavingsBytes")) {paste0(results$estimated_input_latency$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$estimated_input_latency_overall_savings_ms <- if (fun_lhex("estimated_input_latency","overallSavingsMs")) {paste0(results$estimated_input_latency$overallSavingsMs, collapse = ', ')} else {NA}

    # render_blocking_resources----------------------------------
    results_df$render_blocking_resources_url_vs_wastedMs <- if (fun_lhex("render_blocking_resources","url")) {paste0(results$render_blocking_resources$items$url, " : ", results$render_blocking_resources$items$wastedMs, collapse = ', ')} else {NA}

    # uses_responsive_images----------------------------------
    results_df$uses_responsive_images_urls <- if (fun_lhex("uses_responsive_images","url")) {paste0(results$uses_responsive_images$items$url, collapse = ', ')} else {NA}
    results_df$uses_responsive_images_totalBytes <- if (fun_lhex("uses_responsive_images","totalBytes")) {paste0(results$uses_responsive_images$items$totalBytes, collapse = ', ')} else {NA}
    results_df$uses_responsive_images_wastedBytes <- if (fun_lhex("uses_responsive_images","wastedBytes")) {paste0(results$uses_responsive_images$items$wastedBytes, collapse = ', ')} else {NA}
    results_df$uses_responsive_images_wastedPercent <- if (fun_lhex("uses_responsive_images","wastedPercent")) {paste0(results$uses_responsive_images$items$wastedPercent, collapse = ', ')} else {NA}

    # offscreen_images----------------------------------
    results_df$offscreen_images_urls <- if (fun_lhex("offscreen_images","url")) {paste0(results$offscreen_images$items$url, collapse = ', ')} else {NA}
    results_df$offscreen_images_overall_savings_bytes <- if (fun_lhex("offscreen_images","overallSavingsBytes")) {paste0(results$offscreen_images$overallSavingsBytes, collapse = ', ')} else {NA}
    results_df$offscreen_images_overall_savings_ms <- if (fun_lhex("offscreen_images","overallSavingsMs")) {paste0(results$offscreen_images$overallSavingsMs, collapse = ', ')} else {NA}

    # unminified_css----------------------------------
    results_df$unminified_css_urls <- if (fun_lhex("unminified_css","url")) {paste0(results$unminified_css$items$url, collapse = ', ')} else {NA}
    results_df$unminified_css_overall_savings_bytes <- if (fun_lhex("unminified_css","overallSavingsBytes")) {paste0(results$unminified_css$overallSavingsBytes, collapse = ', ')} else {NA}
    results_df$unminified_css_overall_savings_ms <- if (fun_lhex("unminified_css","overallSavingsMs")) {paste0(results$unminified_css$overallSavingsMs, collapse = ', ')} else {NA}

    # unminified_javascript----------------------------------
    results_df$unminified_javascript_urls <- if (fun_lhex("unminified_javascript","url")) {paste0(results$unminified_javascript$items$url, collapse = ', ')} else {NA}
    results_df$unminified_javascript_overall_savings_bytes <- if (fun_lhex("unminified_javascript","overallSavingsBytes")) {paste0(results$unminified_javascript$overallSavingsBytes, collapse = ', ')} else {NA}
    results_df$unminified_javascript_overall_savings_ms <- if (fun_lhex("unminified_javascript","overallSavingsMs")) {paste0(results$unminified_javascript$overallSavingsMs, collapse = ', ')} else {NA}

    # unused_css_rules----------------------------------
    results_df$unused_css_rules_urls <- if (fun_lhex("unused_css_rules","url")) {paste0(results$unused_css_rules$items$url, collapse = ', ')} else {NA}
    results_df$unused_css_rules_wastedPercent <- if (fun_lhex("unused_css_rules","wastedPercent")) {paste0(results$unused_css_rules$items$wastedPercent, collapse = ', ')} else {NA}
    results_df$unused_css_rules_totalBytes <- if (fun_lhex("unused_css_rules","totalBytes")) {paste0(results$unused_css_rules$items$totalBytes, collapse = ', ')} else {NA}
    results_df$unused_css_rules_wastedBytes <- if (fun_lhex("unused_css_rules","wastedBytes")) {paste0(results$unused_css_rules$items$wastedBytes, collapse = ', ')} else {NA}
    results_df$unused_css_rules_overall_savings_bytes <- if (fun_lhex("unused_css_rules","overallSavingsBytes")) {paste0(results$unused_css_rules$overallSavingsBytes, collapse = ', ')} else {NA}
    results_df$unused_css_rules_overall_savings_ms <- if (fun_lhex("unused_css_rules","overallSavingsMs")) {paste0(results$unused_css_rules$overallSavingsMs, collapse = ', ')} else {NA}

    # uses_optimized_images----------------------------------
    results_df$uses_optimized_images_urls <- if (fun_lhex("uses_optimized_images","url")) {paste0(results$uses_optimized_images$items$url, collapse = ', ')} else {NA}
    results_df$uses_optimized_images_overall_savings_bytes <- if (fun_lhex("uses_optimized_images","overallSavingsBytes")) {paste0(results$uses_optimized_images$overallSavingsBytes, collapse = ', ')} else {NA}
    results_df$uses_optimized_images_overall_savings_ms <- if (fun_lhex("uses_optimized_images","overallSavingsMs")) {paste0(results$uses_optimized_images$overallSavingsMs, collapse = ', ')} else {NA}

    # uses_webp_images----------------------------------
    results_df$uses_webp_images_urls <- if (fun_lhex("uses_webp_images","url")) {paste0(results$uses_webp_images$items$url, collapse = ', ')} else {NA}
    results_df$uses_webp_images_overall_savings_bytes <- if (fun_lhex("uses_webp_images","overallSavingsBytes")) {paste0(results$uses_webp_images$overallSavingsBytes, collapse = ', ')} else {NA}
    results_df$uses_webp_images_overall_savings_ms <- if (fun_lhex("uses_webp_images","overallSavingsMs")) {paste0(results$uses_webp_images$overallSavingsMs, collapse = ', ')} else {NA}

    # uses_text_compression----------------------------------
    results_df$uses_text_compression_urls <- if (fun_lhex("uses_text_compression","url")) {paste0(results$uses_text_compression$items$url, collapse = ', ')} else {NA}
    results_df$uses_text_compression_overall_savings_bytes <- if (fun_lhex("uses_text_compression","overallSavingsBytes")) {paste0(results$uses_text_compression$overallSavingsBytes, collapse = ', ')} else {NA}
    results_df$uses_text_compression_overall_savings_ms <- if (fun_lhex("uses_text_compression","overallSavingsMs")) {paste0(results$uses_text_compression$overallSavingsMs, collapse = ', ')} else {NA}

    # uses_rel_preconnect----------------------------------
    results_df$uses_rel_preconnect_urls <- if (fun_lhex("uses_rel_preconnect","url")) {paste0(results$uses_rel_preconnect$items$url, collapse = ', ')} else {NA}
    results_df$uses_rel_preconnect_overall_savings_ms <- if (fun_lhex("uses_rel_preconnect","overallSavingsMs")) {paste0(results$uses_rel_preconnect$overallSavingsMs, collapse = ', ')} else {NA}

    # time_to_first_byte----------------------------------
    results_df$time_to_first_byte_urls <- if (fun_lhex("time_to_first_byte","url")) {paste0(results$time_to_first_byte$items$url, collapse = ', ')} else {NA}
    results_df$time_to_first_byte_overall_savings_bytes <- if (fun_lhex("time_to_first_byte","overallSavingsBytes")) {paste0(results$time_to_first_byte$overallSavingsBytes, collapse = ', ')} else {NA}
    results_df$time_to_first_byte_overall_savings_ms <- if (fun_lhex("time_to_first_byte","overallSavingsMs")) {paste0(results$time_to_first_byte$overallSavingsMs, collapse = ', ')} else {NA}

    # redirects----------------------------------
    results_df$redirects_urls_vs_wastedMs <- if (fun_lhex("redirects","url")) {paste0(results$redirects$items$url, " : ", results$redirects$items$wastedMs, collapse = ", ")} else {NA}

    # uses_rel_preload----------------------------------
    results_df$uses_rel_preload_urls <- if (fun_lhex("uses_rel_preload","url")) {paste0(results$uses_rel_preload$items$url, collapse = ', ')} else {NA}
    results_df$uses_rel_preload_overall_savings_bytes <- if (fun_lhex("uses_rel_preload","overallSavingsBytes")) {paste0(results$uses_rel_preload$overallSavingsBytes, collapse = ', ')} else {NA}
    results_df$uses_rel_preload_overall_savings_ms <- if (fun_lhex("uses_rel_preload","overallSavingsMs")) {paste0(results$uses_rel_preload$overallSavingsMs, collapse = ', ')} else {NA}

    # efficient_animated_content----------------------------------
    results_df$efficient_animated_content_urls <- if (fun_lhex("efficient_animated_content","url")) {paste0(results$efficient_animated_content$items$url, collapse = ', ')} else {NA}
    results_df$efficient_animated_content_overall_savings_bytes <- if (fun_lhex("efficient_animated_content","overallSavingsBytes")) {paste0(results$efficient_animated_content$overallSavingsBytes, collapse = ', ')} else {NA}
    results_df$efficient_animated_content_overall_savings_ms <- if (fun_lhex("efficient_animated_content","overallSavingsMs")) {paste0(results$efficient_animated_content$overallSavingsMs, collapse = ', ')} else {NA}

    # total_byte_weight----------------------------------
    results_df$total_byte_weight_url_vs_total_bytes <- paste0(results$total_byte_weight$items$url, " : ", results$total_byte_weight$items$totalBytes, collapse = ", ")

    # uses_long_cache_ttl----------------------------------
    results_df$uses_long_cache_ttl_details_url_vs_wastedBytes <- paste0(results$network_requests$items$url, " : ", results$network_requests$items$resourceSize, collapse = ", ")

    # dom_size----------------------------------
    results_df$dom_size_details <- paste0(results$dom_size$items$statistic, " : ", results$dom_size$items$value, collapse = ", ")

    # TODO create a way of parsing critical request chains (multiple nested lists with parameters)
    # critical_request_chains----------------------------------
    # results_df$critical_request_chains_urls <- if (fun_lhex("","")) {paste0(results$critical_request_chains$items$url, collapse = ', ')} else {NA}
    # results_df$critical_request_chains_overall_savings_bytes <- if (fun_lhex("","")) {paste0(results$critical_request_chains$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$critical_request_chains_overall_savings_ms <- if (fun_lhex("","")) {paste0(results$critical_request_chains$overallSavingsMs, collapse = ', ')} else {NA}

    # uses_long_cache_ttl ----------------------------------------------
    results_df$uses_long_cache_ttl_url <- if (fun_lhex("uses_long_cache_ttl", "url")) {paste0(results$network_requests$items$url, collapse = ", ")} else {NA}
    results_df$uses_long_cache_ttl_mimeType <- if (fun_lhex("uses_long_cache_ttl", "mimeType")) {paste0(results$network_requests$items$mimeType, collapse = ", ")} else {NA}
    results_df$uses_long_cache_ttl_endTime <- if (fun_lhex("uses_long_cache_ttl", "endTime")) {paste0(results$network_requests$items$endTime, collapse = ", ")} else {NA}
    results_df$uses_long_cache_ttl_resourceSize <- if (fun_lhex("uses_long_cache_ttl", "resourceSize")) {paste0(results$network_requests$items$resourceSize, collapse = ", ")} else {NA}
    results_df$uses_long_cache_ttl_startTime <- if (fun_lhex("uses_long_cache_ttl", "startTime")) {paste0(results$network_requests$items$startTime, collapse = ", ")} else {NA}
    results_df$uses_long_cache_ttl_transferSize <- if (fun_lhex("uses_long_cache_ttl", "transferSize")) {paste0(results$network_requests$items$transferSize, collapse = ", ")} else {NA}
    results_df$uses_long_cache_ttl_statusCode <- if (fun_lhex("uses_long_cache_ttl", "statusCode")) {paste0(results$network_requests$items$statusCode, collapse = ", ")} else {NA}
    results_df$uses_long_cache_ttl_resourceType <- if (fun_lhex("uses_long_cache_ttl", "resourceType")) {paste0(results$network_requests$items$resourceType, collapse = ", ")} else {NA}

    # metrics----------------------------------
    results_df$metrics_first_contentful_paint       <- if (fun_lhex("metrics","firstContentfulPaint")) {paste0(results$metrics$items$firstContentfulPaint, collapse = ', ')} else {NA}
    results_df$metrics_speedIndex                   <- if (fun_lhex("metrics","speedIndex")) {paste0(results$metrics$items$speedIndex, collapse = ', ')} else {NA}
    results_df$metrics_observedFirstContentfulPaint <- if (fun_lhex("metrics","observedFirstContentfulPaint")) {paste0(results$metrics$items$observedFirstContentfulPaint, collapse = ', ')} else {NA}
    results_df$metrics_observedFirstVisualChange    <- if (fun_lhex("metrics","observedFirstVisualChange")) {paste0(results$metrics$items$observedFirstVisualChange, collapse = ', ')} else {NA}
    results_df$metrics_firstMeaningfulPaint         <- if (fun_lhex("metrics","firstMeaningfulPaint")) {paste0(results$metrics$items$firstMeaningfulPaint, collapse = ', ')} else {NA}
    results_df$metrics_observedFirstMeaningfulPaint <- if (fun_lhex("metrics","observedFirstMeaningfulPaint")) {paste0(results$metrics$items$observedFirstMeaningfulPaint, collapse = ', ')} else {NA}
    results_df$metrics_observedTraceEnd             <- if (fun_lhex("metrics","observedTraceEnd")) {paste0(results$metrics$items$observedTraceEnd, collapse = ', ')} else {NA}
    results_df$metrics_firstCPUIdle                 <- if (fun_lhex("metrics","firstCPUIdle")) {paste0(results$metrics$items$firstCPUIdle, collapse = ', ')} else {NA}
    results_df$metrics_observedDomContentLoaded     <- if (fun_lhex("metrics","observedDomContentLoaded")) {paste0(results$metrics$items$observedDomContentLoaded, collapse = ', ')} else {NA}
    results_df$metrics_observedNavigationStart      <- if (fun_lhex("metrics","observedNavigationStart")) {paste0(results$metrics$items$observedNavigationStart, collapse = ', ')} else {NA}
    results_df$metrics_interactive                  <- if (fun_lhex("metrics","interactive")) {paste0(results$metrics$items$interactive, collapse = ', ')} else {NA}
    results_df$metrics_observedLoad                 <- if (fun_lhex("metrics","observedLoad")) {paste0(results$metrics$items$observedLoad, collapse = ', ')} else {NA}
    results_df$metrics_observedSpeedIndex           <- if (fun_lhex("metrics","observedSpeedIndex")) {paste0(results$metrics$items$observedSpeedIndex, collapse = ', ')} else {NA}
    results_df$metrics_estimatedInputLatency        <- if (fun_lhex("metrics","estimatedInputLatency")) {paste0(results$metrics$items$estimatedInputLatency, collapse = ', ')} else {NA}
    results_df$metrics_observedFirstPaint           <- if (fun_lhex("metrics","observedFirstPaint")) {paste0(results$metrics$items$observedFirstPaint, collapse = ', ')} else {NA}
    results_df$metrics_observedLastVisualChange     <- if (fun_lhex("metrics","observedLastVisualChange")) {paste0(results$metrics$items$observedLastVisualChange, collapse = ', ')} else {NA}

    # user_timings----------------------------------
    results_df$user_timings_urls <- if (fun_lhex("user_timings","url")) {paste0(results$user_timings$items$url, collapse = ', ')} else {NA}
    results_df$user_timings_overall_savings_bytes <- if (fun_lhex("user_timings","overallSavingsBytes")) {paste0(results$user_timings$overallSavingsBytes, collapse = ', ')} else {NA}
    results_df$user_timings_overall_savings_ms <- if (fun_lhex("user_timings","overallSavingsMs")) {paste0(results$user_timings$overallSavingsMs, collapse = ', ')} else {NA}

    # bootup_time----------------------------------
    results_df$bootup_time_urls <- if (fun_lhex("bootup_time","url")) {paste0(results$bootup_time$items$url, collapse = ', ')} else {NA}
    results_df$bootup_time_scripting_ms <- if (fun_lhex("bootup_time","scripting")) {paste0(results$bootup_time$items$scripting, collapse = ', ')} else {NA}
    results_df$bootup_time_scriptParseCompile_ms <- if (fun_lhex("bootup_time","scriptParseCompile")) {paste0(results$bootup_time$items$scriptParseCompile, collapse = ', ')} else {NA}

    # screenshot_thumbnails----------------------------------
    results_df$screenshot_thumbnails_timestamp <- if (fun_lhex("screenshot_thumbnails","timestamp")) {paste0(results$screenshot_thumbnails$items$timestamp, collapse = ', ')} else {NA}
    results_df$screenshot_thumbnails_data <- if (fun_lhex("screenshot_thumbnails","data")) {paste0(results$screenshot_thumbnails$items$data, collapse = ', ')} else {NA}
    results_df$screenshot_thumbnails_timing <- if (fun_lhex("screenshot_thumbnails","timing")) {paste0(results$screenshot_thumbnails$items$timing, collapse = ', ')} else {NA}

    # final_screenshot----------------------------------
    results_df$final_screenshot_timestamp <- if (fun_lhex("final_screenshot")) {paste0(results$final_screenshot$items$timestamp, collapse = ', ')} else {NA}
    results_df$final_screenshot_data <- if (fun_lhex("final_screenshot")) {paste0(results$final_screenshot$items$data, collapse = ', ')} else {NA}
    results_df$final_screenshot_type <- if (fun_lhex("final_screenshot","url")) {paste0(results$final_screenshot$items$type, collapse = ', ')} else {NA}

    # mainthread_work_breakdown----------------------------------
    results_df$total_byte_worktype_vs_duration <- paste0(results$mainthread_work_breakdown$items$groupLabel, " : ", results$mainthread_work_breakdown$items$duration, collapse = ", ")

    # font_display----------------------------------
    results_df$font_display_urls_details_url_vs_wastedMs <- paste0(results$font_display$items$url, " : ", results$font_display$items$wastedMs, collapse = ", ")

  }

  # accessibility -------------------------------------------------------------
  # gsub("-", "_", parsed$lighthouseResult$categories$accessibility$auditRefs$id)
  if ("accessibility" %in% categories) {
    # aria_allowed_attr----------------------------------
    results_df$aria_allowed_attr_urls <- if (fun_lhex("aria_allowed_attr","url")) {paste0(results$aria_allowed_attr$items$url, collapse = ', ')} else {NA}
    # results_df$aria_allowed_attr_overall_savings_bytes <- if (fun_lhex("aria_allowed_attr","overallSavingsBytes")) {paste0(results$aria_allowed_attr$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$aria_allowed_attr_overall_savings_ms <- if (fun_lhex("aria_allowed_attr","overallSavingsMs")) {paste0(results$aria_allowed_attr$overallSavingsMs, collapse = ', ')} else {NA}

    # aria_required_attr----------------------------------
    results_df$aria_required_attr_urls <- if (fun_lhex("aria_required_attr","url")) {paste0(results$aria_required_attr$items$url, collapse = ', ')} else {NA}
    # results_df$aria_required_attr_overall_savings_bytes <- if (fun_lhex("aria_required_attr","overallSavingsBytes")) {paste0(results$aria_required_attr$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$aria_required_attr_overall_savings_ms <- if (fun_lhex("aria_required_attr","overallSavingsMs")) {paste0(results$aria_required_attr$overallSavingsMs, collapse = ', ')} else {NA}

    # aria_required_children----------------------------------
    results_df$aria_required_children_urls <- if (fun_lhex("aria_required_children","url")) {paste0(results$aria_required_children$items$url, collapse = ', ')} else {NA}
    # results_df$aria_required_children_overall_savings_bytes <- if (fun_lhex("aria_required_children","overallSavingsBytes")) {paste0(results$aria_required_children$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$aria_required_children_overall_savings_ms <- if (fun_lhex("aria_required_children","overallSavingsMs")) {paste0(results$aria_required_children$overallSavingsMs, collapse = ', ')} else {NA}

    # aria_required_parent----------------------------------
    results_df$aria_required_parent_urls <- if (fun_lhex("aria_required_parent","url")) {paste0(results$aria_required_parent$items$url, collapse = ', ')} else {NA}
    # results_df$aria_required_parent_overall_savings_bytes <- if (fun_lhex("aria_required_parent","overallSavingsBytes")) {paste0(results$aria_required_parent$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$aria_required_parent_overall_savings_ms <- if (fun_lhex("aria_required_parent","overallSavingsMs")) {paste0(results$aria_required_parent$overallSavingsMs, collapse = ', ')} else {NA}

    # aria_roles----------------------------------
    results_df$aria_roles_urls <- if (fun_lhex("aria_roles","url")) {paste0(results$aria_roles$items$url, collapse = ', ')} else {NA}
    # results_df$aria_roles_overall_savings_bytes <- if (fun_lhex("aria_roles","overallSavingsBytes")) {paste0(results$aria_roles$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$aria_roles_overall_savings_ms <- if (fun_lhex("aria_roles","overallSavingsMs")) {paste0(results$aria_roles$overallSavingsMs, collapse = ', ')} else {NA}

    # aria_valid_attr_value----------------------------------
    results_df$aria_valid_attr_value_urls <- if (fun_lhex("aria_valid_attr_value","url")) {paste0(results$aria_valid_attr_value$items$url, collapse = ', ')} else {NA}
    # results_df$aria_valid_attr_value_overall_savings_bytes <- if (fun_lhex("aria_valid_attr_value","overallSavingsBytes")) {paste0(results$aria_valid_attr_value$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$aria_valid_attr_value_overall_savings_ms <- if (fun_lhex("aria_valid_attr_value","overallSavingsMs")) {paste0(results$aria_valid_attr_value$overallSavingsMs, collapse = ', ')} else {NA}

    # aria_valid_attr----------------------------------
    results_df$aria_valid_attr_urls <- if (fun_lhex("aria_valid_attr","url")) {paste0(results$aria_valid_attr$items$url, collapse = ', ')} else {NA}
    # results_df$aria_valid_attr_overall_savings_bytes <- if (fun_lhex("aria_valid_attr","overallSavingsBytes")) {paste0(results$aria_valid_attr$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$aria_valid_attr_overall_savings_ms <- if (fun_lhex("aria_valid_attr","overallSavingsMs")) {pa  ste0(results$aria_valid_attr$overallSavingsMs, collapse = ', ')} else {NA}

    # audio_caption----------------------------------
    results_df$audio_caption_urls <- if (fun_lhex("audio_caption","url")) {paste0(results$audio_caption$items$url, collapse = ', ')} else {NA}
    # results_df$audio_caption_overall_savings_bytes <- if (fun_lhex("audio_caption","overallSavingsBytes")) {paste0(results$audio_caption$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$audio_caption_overall_savings_ms <- if (fun_lhex("audio_caption","overallSavingsMs")) {paste0(results$audio_caption$overallSavingsMs, collapse = ', ')} else {NA}

    # button_name----------------------------------
    results_df$button_name_urls <- if (fun_lhex("button_name","url")) {paste0(results$button_name$items$url, collapse = ', ')} else {NA}
    # results_df$button_name_overall_savings_bytes <- if (fun_lhex("button_name","overallSavingsBytes")) {paste0(results$button_name$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$button_name_overall_savings_ms <- if (fun_lhex("button_name","overallSavingsMs")) {paste0(results$button_name$overallSavingsMs, collapse = ', ')} else {NA}

    # bypass----------------------------------
    results_df$bypass_urls <- if (fun_lhex("bypass","url")) {paste0(results$bypass$items$url, collapse = ', ')} else {NA}
    results_df$bypass_overall_savings_bytes <- if (fun_lhex("bypass","overallSavingsBytes")) {paste0(results$bypass$overallSavingsBytes, collapse = ', ')} else {NA}
    results_df$bypass_overall_savings_ms <- if (fun_lhex("bypass","overallSavingsMs")) {paste0(results$bypass$overallSavingsMs, collapse = ', ')} else {NA}

    # color_contrast----------------------------------
    results_df$color_contrast_explanation <- if (fun_lhex("color_contrast","node")) {paste0(results$color_contrast$items$node$explanation, collapse = ', ')} else {NA}
    results_df$color_contrast_type <- if (fun_lhex("color_contrast","node")) {paste0(results$color_contrast$items$node$type, collapse = ', ')} else {NA}
    results_df$color_contrast_selector <- if (fun_lhex("color_contrast","node")) {paste0(results$color_contrast$items$node$selector, collapse = ', ')} else {NA}
    results_df$color_contrast_path <- if (fun_lhex("color_contrast","node")) {paste0(results$color_contrast$items$node$path, collapse = ', ')} else {NA}

    # definition_list----------------------------------
    results_df$definition_list_urls <- if (fun_lhex("definition_list","url")) {paste0(results$definition_list$items$url, collapse = ', ')} else {NA}
    # results_df$definition_list_overall_savings_bytes <- if (fun_lhex("definition_list","overallSavingsBytes")) {paste0(results$definition_list$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$definition_list_overall_savings_ms <- if (fun_lhex("definition_list","overallSavingsMs")) {paste0(results$definition_list$overallSavingsMs, collapse = ', ')} else {NA}

    # dlitem----------------------------------
    results_df$dlitem_urls <- if (fun_lhex("dlitem","url")) {paste0(results$dlitem$items$url, collapse = ', ')} else {NA}
    # results_df$dlitem_overall_savings_bytes <- if (fun_lhex("dlitem","overallSavingsBytes")) {paste0(results$dlitem$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$dlitem_overall_savings_ms <- if (fun_lhex("dlitem","overallSavingsMs")) {paste0(results$dlitem$overallSavingsMs, collapse = ', ')} else {NA}

    # duplicate_id----------------------------------
    results_df$duplicate_id_impact <- if (fun_lhex("duplicate_id") & !is.null(results$duplicate_id$impact)) {paste0(results$duplicate_id$impact, collapse = ', ')} else {NA}
    results_df$duplicate_id_selector    <- if (fun_lhex("duplicate_id","node")) {paste0(results$duplicate_id$items$node$selector, collapse = ', ')} else {NA}
    results_df$duplicate_id_path        <- if (fun_lhex("duplicate_id","node")) {paste0(results$duplicate_id$items$node$path, collapse = ', ')} else {NA}
    results_df$duplicate_id_snippet     <- if (fun_lhex("duplicate_id","node")) {paste0(results$duplicate_id$items$node$snippet, collapse = ', ')} else {NA}
    results_df$duplicate_id_explanation <- if (fun_lhex("duplicate_id","node")) {paste0(results$duplicate_id$items$node$explanation, collapse = ', ')} else {NA}
    results_df$duplicate_id_type        <- if (fun_lhex("duplicate_id","node")) {paste0(results$duplicate_id$items$node$type, collapse = ', ')} else {NA}

    # frame_title----------------------------------
    results_df$frame_title_urls <- if (fun_lhex("frame_title","url")) {paste0(results$frame_title$items$url, collapse = ', ')} else {NA}
    # results_df$frame_title_overall_savings_bytes <- if (fun_lhex("frame_title","overallSavingsBytes")) {paste0(results$frame_title$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$frame_title_overall_savings_ms <- if (fun_lhex("frame_title","overallSavingsMs")) {paste0(results$frame_title$overallSavingsMs, collapse = ', ')} else {NA}

    # html_has_lang----------------------------------
    results_df$html_has_lang_urls <- if (fun_lhex("html_has_lang", "url")) {paste0(results$html_has_lang$items$url, collapse = ', ')} else {NA}
    results_df$html_has_lang_overall_savings_bytes <- if (fun_lhex("html_has_lang", "overallSavingsBytes")) {paste0(results$html_has_lang$overallSavingsBytes, collapse = ', ')} else {NA}
    results_df$html_has_lang_overall_savings_ms <- if (fun_lhex("html_has_lang", "overallSavingsMs")) {paste0(results$html_has_lang$overallSavingsMs, collapse = ', ')} else {NA}

    # html_lang_valid----------------------------------
    results_df$html_lang_valid_urls <- if (fun_lhex("html_lang_valid","url")) {paste0(results$html_lang_valid$items$url, collapse = ', ')} else {NA}
    results_df$html_lang_valid_overall_savings_bytes <- if (fun_lhex("html_lang_valid","overallSavingsBytes")) {paste0(results$html_lang_valid$overallSavingsBytes, collapse = ', ')} else {NA}
    results_df$html_lang_valid_overall_savings_ms <- if (fun_lhex("html_lang_valid","overallSavingsMs")) {paste0(results$html_lang_valid$overallSavingsMs, collapse = ', ')} else {NA}

    # image_alt----------------------------------
    results_df$image_alt_snippet <- if (fun_lhex("image_alt","node")) {paste0(results$image_alt$items$node$snippet, collapse = ', ')} else {NA}
    results_df$image_alt_selector <- if (fun_lhex("image_alt","node")) {paste0(results$image_alt$items$node$selector, collapse = ', ')} else {NA}
    results_df$image_alt_path <- if (fun_lhex("image_alt","node")) {paste0(results$image_alt$items$node$path, collapse = ', ')} else {NA}
    results_df$image_alt_explanation <- if (fun_lhex("image_alt","node")) {paste0(results$image_alt$items$node$explanation, collapse = ', ')} else {NA}
    results_df$image_alt_type <- if (fun_lhex("image_alt","node")) {paste0(results$image_alt$items$node$type, collapse = ', ')} else {NA}

    # input_image_alt----------------------------------
    results_df$input_image_alt_urls <- if (fun_lhex("input_image_alt","url")) {paste0(results$input_image_alt$items$url, collapse = ', ')} else {NA}
    # results_df$input_image_alt_overall_savings_bytes <- if (fun_lhex("input_image_alt","overallSavingsBytes")) {paste0(results$input_image_alt$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$input_image_alt_overall_savings_ms <- if (fun_lhex("input_image_alt","overallSavingsMs")) {paste0(results$input_image_alt$overallSavingsMs, collapse = ', ')} else {NA}

    # label----------------------------------
    results_df$label_urls <- if (fun_lhex("label","url")) {paste0(results$label$items$url, collapse = ', ')} else {NA}
    # results_df$label_overall_savings_bytes <- if (fun_lhex("label","overallSavingsBytes")) {paste0(results$label$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$label_overall_savings_ms <- if (fun_lhex("label","overallSavingsMs")) {paste0(results$label$overallSavingsMs, collapse = ', ')} else {NA}

    # layout_table----------------------------------
    results_df$layout_table_urls <- if (fun_lhex("layout_table","url")) {paste0(results$layout_table$items$url, collapse = ', ')} else {NA}
    # results_df$layout_table_overall_savings_bytes <- if (fun_lhex("layout_table","overallSavingsBytes")) {paste0(results$layout_table$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$layout_table_overall_savings_ms <- if (fun_lhex("layout_table","overallSavingsMs")) {paste0(results$layout_table$overallSavingsMs, collapse = ', ')} else {NA}

    # link_name----------------------------------
    results_df$link_name_explanation <- if (fun_lhex("link_name","node")) {paste0(results$link_name$items$explanation, collapse = ', ')} else {NA}
    results_df$link_name_type        <- if (fun_lhex("link_name","node")) {paste0(results$link_name$items$type, collapse = ', ')} else {NA}
    results_df$link_name_selector    <- if (fun_lhex("link_name","node")) {paste0(results$link_name$items$selector, collapse = ', ')} else {NA}
    results_df$link_name_path        <- if (fun_lhex("link_name","node")) {paste0(results$link_name$items$path, collapse = ', ')} else {NA}
    results_df$link_name_snippet     <- if (fun_lhex("link_name","node")) {paste0(results$link_name$items$snippet, collapse = ', ')} else {NA}

    # list----------------------------------
    results_df$list_urls <- if (fun_lhex("list","url")) {paste0(results$list$items$url, collapse = ', ')} else {NA}
    results_df$list_overall_savings_bytes <- if (fun_lhex("list","overallSavingsBytes")) {paste0(results$list$overallSavingsBytes, collapse = ', ')} else {NA}
    results_df$list_overall_savings_ms <- if (fun_lhex("list","overallSavingsMs")) {paste0(results$list$overallSavingsMs, collapse = ', ')} else {NA}

    # listitem----------------------------------
    results_df$listitem_urls <- if (fun_lhex("listitem","url")) {paste0(results$listitem$items$url, collapse = ', ')} else {NA}
    results_df$listitem_overall_savings_bytes <- if (fun_lhex("listitem","overallSavingsBytes")) {paste0(results$listitem$overallSavingsBytes, collapse = ', ')} else {NA}
    results_df$listitem_overall_savings_ms <- if (fun_lhex("listitem","overallSavingsMs")) {paste0(results$listitem$overallSavingsMs, collapse = ', ')} else {NA}

    # meta_refresh----------------------------------
    results_df$meta_refresh_urls <- if (fun_lhex("meta_refresh","url")) {paste0(results$meta_refresh$items$url, collapse = ', ')} else {NA}
    # results_df$meta_refresh_overall_savings_bytes <- if (fun_lhex("meta_refresh","overallSavingsBytes")) {paste0(results$meta_refresh$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$meta_refresh_overall_savings_ms <- if (fun_lhex("meta_refresh","overallSavingsMs")) {paste0(results$meta_refresh$overallSavingsMs, collapse = ', ')} else {NA}

    # meta_viewport----------------------------------
    results_df$meta_viewport_urls <- if (fun_lhex("meta_viewport","url")) {paste0(results$meta_viewport$items$url, collapse = ', ')} else {NA}
    results_df$meta_viewport_overall_savings_bytes <- if (fun_lhex("meta_viewport","overallSavingsBytes")) {paste0(results$meta_viewport$overallSavingsBytes, collapse = ', ')} else {NA}
    results_df$meta_viewport_overall_savings_ms <- if (fun_lhex("meta_viewport","overallSavingsMs")) {paste0(results$meta_viewport$overallSavingsMs, collapse = ', ')} else {NA}

    # object_alt----------------------------------
    results_df$object_alt_urls <- if (fun_lhex("object_alt","url")) {paste0(results$object_alt$items$url, collapse = ', ')} else {NA}
    # results_df$object_alt_overall_savings_bytes <- if (fun_lhex("object_alt","overallSavingsBytes")) {paste0(results$object_alt$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$object_alt_overall_savings_ms <- if (fun_lhex("object_alt","overallSavingsMs")) {paste0(results$object_alt$overallSavingsMs, collapse = ', ')} else {NA}

    # tabindex----------------------------------
    results_df$tabindex_urls <- if (fun_lhex("tabindex","url")) {paste0(results$tabindex$items$url, collapse = ', ')} else {NA}
    results_df$tabindex_overall_savings_bytes <- if (fun_lhex("tabindex","overallSavingsBytes")) {paste0(results$tabindex$overallSavingsBytes, collapse = ', ')} else {NA}
    results_df$tabindex_overall_savings_ms <- if (fun_lhex("tabindex","overallSavingsMs")) {paste0(results$tabindex$overallSavingsMs, collapse = ', ')} else {NA}

    # td_headers_attr----------------------------------
    results_df$td_headers_attr_urls <- if (fun_lhex("td_headers_attr","url")) {paste0(results$td_headers_attr$items$url, collapse = ', ')} else {NA}
    # results_df$td_headers_attr_overall_savings_bytes <- if (fun_lhex("td_headers_attr","overallSavingsBytes")) {paste0(results$td_headers_attr$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$td_headers_attr_overall_savings_ms <- if (fun_lhex("td_headers_attr","overallSavingsMs")) {paste0(results$td_headers_attr$overallSavingsMs, collapse = ', ')} else {NA}

    # th_has_data_cells----------------------------------
    results_df$th_has_data_cells_urls <- if (fun_lhex("th_has_data_cells","url")) {paste0(results$th_has_data_cells$items$url, collapse = ', ')} else {NA}
    # results_df$th_has_data_cells_overall_savings_bytes <- if (fun_lhex("th_has_data_cells","overallSavingsBytes")) {paste0(results$th_has_data_cells$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$th_has_data_cells_overall_savings_ms <- if (fun_lhex("th_has_data_cells","overallSavingsMs")) {paste0(results$th_has_data_cells$overallSavingsMs, collapse = ', ')} else {NA}

    # valid_lang----------------------------------
    results_df$valid_lang_urls <- if (fun_lhex("valid_lang","url")) {paste0(results$valid_lang$items$url, collapse = ', ')} else {NA}
    # results_df$valid_lang_overall_savings_bytes <- if (fun_lhex("valid_lang","overallSavingsBytes")) {paste0(results$valid_lang$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$valid_lang_overall_savings_ms <- if (fun_lhex("valid_lang","overallSavingsMs")) {paste0(results$valid_lang$overallSavingsMs, collapse = ', ')} else {NA}

    # video_caption----------------------------------
    results_df$video_caption_urls <- if (fun_lhex("video_caption","url")) {paste0(results$video_caption$items$url, collapse = ', ')} else {NA}
    # results_df$video_caption_overall_savings_bytes <- if (fun_lhex("video_caption","overallSavingsBytes")) {paste0(results$video_caption$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$video_caption_overall_savings_ms <- if (fun_lhex("video_caption","overallSavingsMs")) {paste0(results$video_caption$overallSavingsMs, collapse = ', ')} else {NA}

    # video_description----------------------------------
    results_df$video_description_urls <- if (fun_lhex("video_description","url")) {paste0(results$video_description$items$url, collapse = ', ')} else {NA}
    # results_df$video_description_overall_savings_bytes <- if (fun_lhex("video_description","overallSavingsBytes")) {paste0(results$video_description$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$video_description_overall_savings_ms <- if (fun_lhex("video_description","overallSavingsMs")) {paste0(results$video_description$overallSavingsMs, collapse = ', ')} else {NA}

    # accesskeys----------------------------------
    results_df$accesskeys_urls <- if (fun_lhex("accesskeys","url")) {paste0(results$accesskeys$items$url, collapse = ', ')} else {NA}
    # results_df$accesskeys_overall_savings_bytes <- if (fun_lhex("accesskeys","overallSavingsBytes")) {paste0(results$accesskeys$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$accesskeys_overall_savings_ms <- if (fun_lhex("accesskeys","overallSavingsMs")) {paste0(results$accesskeys$overallSavingsMs, collapse = ', ')} else {NA}

    # logical_tab_order----------------------------------
    results_df$logical_tab_order_urls <- if (fun_lhex("logical_tab_order","url")) {paste0(results$logical_tab_order$items$url, collapse = ', ')} else {NA}
    # results_df$logical_tab_order_overall_savings_bytes <- if (fun_lhex("logical_tab_order","overallSavingsBytes")) {paste0(results$logical_tab_order$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$logical_tab_order_overall_savings_ms <- if (fun_lhex("logical_tab_order","overallSavingsMs")) {paste0(results$logical_tab_order$overallSavingsMs, collapse = ', ')} else {NA}

    # focusable_controls----------------------------------
    results_df$focusable_controls_urls <- if (fun_lhex("focusable_controls","url")) {paste0(results$focusable_controls$items$url, collapse = ', ')} else {NA}
    # results_df$focusable_controls_overall_savings_bytes <- if (fun_lhex("focusable_controls","overallSavingsBytes")) {paste0(results$focusable_controls$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$focusable_controls_overall_savings_ms <- if (fun_lhex("focusable_controls","overallSavingsMs")) {paste0(results$focusable_controls$overallSavingsMs, collapse = ', ')} else {NA}

    # interactive_element_affordance----------------------------------
    results_df$interactive_element_affordance_urls <- if (fun_lhex("interactive_element_affordance","url")) {paste0(results$interactive_element_affordance$items$url, collapse = ', ')} else {NA}
    # results_df$interactive_element_affordance_overall_savings_bytes <- if (fun_lhex("interactive_element_affordance","overallSavingsBytes")) {paste0(results$interactive_element_affordance$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$interactive_element_affordance_overall_savings_ms <- if (fun_lhex("interactive_element_affordance","overallSavingsMs")) {paste0(results$interactive_element_affordance$overallSavingsMs, collapse = ', ')} else {NA}

    # managed_focus----------------------------------
    results_df$managed_focus_urls <- if (fun_lhex("managed_focus","url")) {paste0(results$managed_focus$items$url, collapse = ', ')} else {NA}
    # results_df$managed_focus_overall_savings_bytes <- if (fun_lhex("managed_focus","overallSavingsBytes")) {paste0(results$managed_focus$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$managed_focus_overall_savings_ms <- if (fun_lhex("managed_focus","overallSavingsMs")) {paste0(results$managed_focus$overallSavingsMs, collapse = ', ')} else {NA}

    # focus_traps----------------------------------
    results_df$focus_traps_urls <- if (fun_lhex("focus_traps","url")) {paste0(results$focus_traps$items$url, collapse = ', ')} else {NA}
    # results_df$focus_traps_overall_savings_bytes <- if (fun_lhex("focus_traps","overallSavingsBytes")) {paste0(results$focus_traps$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$focus_traps_overall_savings_ms <- if (fun_lhex("focus_traps","overallSavingsMs")) {paste0(results$focus_traps$overallSavingsMs, collapse = ', ')} else {NA}

    # custom_controls_labels----------------------------------
    results_df$custom_controls_labels_urls <- if (fun_lhex("custom_controls_labels","url")) {paste0(results$custom_controls_labels$items$url, collapse = ', ')} else {NA}
    # results_df$custom_controls_labels_overall_savings_bytes <- if (fun_lhex("custom_controls_labels","overallSavingsBytes")) {paste0(results$custom_controls_labels$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$custom_controls_labels_overall_savings_ms <- if (fun_lhex("custom_controls_labels","overallSavingsMs")) {paste0(results$custom_controls_labels$overallSavingsMs, collapse = ', ')} else {NA}

    # custom_controls_roles----------------------------------
    results_df$custom_controls_roles_urls <- if (fun_lhex("custom_controls_roles","url")) {paste0(results$custom_controls_roles$items$url, collapse = ', ')} else {NA}

    # visual_order_follows_dom----------------------------------
    results_df$visual_order_follows_dom_urls <- if (fun_lhex("visual_order_follows_dom","url")) {paste0(results$visual_order_follows_dom$items$url, collapse = ', ')} else {NA}
    # results_df$visual_order_follows_dom_overall_savings_bytes <- if (fun_lhex("visual_order_follows_dom","overallSavingsBytes")) {paste0(results$visual_order_follows_dom$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$visual_order_follows_dom_overall_savings_ms <- if (fun_lhex("visual_order_follows_dom","overallSavingsMs")) {paste0(results$visual_order_follows_dom$overallSavingsMs, collapse = ', ')} else {NA}

    # offscreen_content_hidden----------------------------------
    results_df$offscreen_content_hidden_urls <- if (fun_lhex("offscreen_content_hidden","url")) {paste0(results$offscreen_content_hidden$items$url, collapse = ', ')} else {NA}
    # results_df$offscreen_content_hidden_overall_savings_bytes <- if (fun_lhex("offscreen_content_hidden","overallSavingsBytes")) {paste0(results$offscreen_content_hidden$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$offscreen_content_hidden_overall_savings_ms <- if (fun_lhex("offscreen_content_hidden","overallSavingsMs")) {paste0(results$offscreen_content_hidden$overallSavingsMs, collapse = ', ')} else {NA}

    # heading_levels----------------------------------
    results_df$heading_levels_urls <- if (fun_lhex("heading_levels","url")) {paste0(results$heading_levels$items$url, collapse = ', ')} else {NA}
    # results_df$heading_levels_overall_savings_bytes <- if (fun_lhex("heading_levels","overallSavingsBytes")) {paste0(results$heading_levels$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$heading_levels_overall_savings_ms <- if (fun_lhex("heading_levels","overallSavingsMs")) {paste0(results$heading_levels$overallSavingsMs, collapse = ', ')} else {NA}

    # use_landmarks----------------------------------
    results_df$use_landmarks_urls <- if (fun_lhex("use_landmarks","url")) {paste0(results$use_landmarks$items$url, collapse = ', ')} else {NA}
    # results_df$use_landmarks_overall_savings_bytes <- if (fun_lhex("use_landmarks","overallSavingsBytes")) {paste0(results$use_landmarks$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$use_landmarks_overall_savings_ms <- if (fun_lhex("use_landmarks","overallSavingsMs")) {paste0(results$use_landmarks$overallSavingsMs, collapse = ', ')} else {NA}

  }

  # best-practices ------------------------------------------------------------
  # gsub("-", "_", parsed$lighthouseResult$categories$`best-practices`$auditRefs$id)
  if ("best-practices" %in% categories) {
    # appcache_manifest----------------------------------
    results_df$appcache_manifest_urls <- if (fun_lhex("appcache_manifest","url")) {paste0(results$appcache_manifest$items$url, collapse = ', ')} else {NA}
    # results_df$appcache_manifest_overall_savings_bytes <- if (fun_lhex("appcache_manifest","overallSavingsBytes")) {paste0(results$appcache_manifest$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$appcache_manifest_overall_savings_ms <- if (fun_lhex("appcache_manifest","overallSavingsMs")) {paste0(results$appcache_manifest$overallSavingsMs, collapse = ', ')} else {NA}

    # no_document_write----------------------------------
    results_df$no_document_write_urls <- if (fun_lhex("no_document_write","url")) {paste0(results$no_document_write$items$url, collapse = ', ')} else {NA}
    # results_df$no_document_write_overall_savings_bytes <- if (fun_lhex("no_document_write","overallSavingsBytes")) {paste0(results$no_document_write$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$no_document_write_overall_savings_ms <- if (fun_lhex("no_document_write","overallSavingsMs")) {paste0(results$no_document_write$overallSavingsMs, collapse = ', ')} else {NA}

    # uses_passive_event_listeners----------------------------------
    results_df$uses_passive_event_listeners_urls <- if (fun_lhex("uses_passive_event_listeners","url")) {paste0(results$uses_passive_event_listeners$items$url, " : ", results$uses_passive_event_listeners$items$label, collapse = ', ')} else {NA}

    # external_anchors_use_rel_noopener----------------------------------
    results_df$external_anchors_use_rel_noopener_href <- if (fun_lhex("external_anchors_use_rel_noopener","href")) {paste0(results$external_anchors_use_rel_noopener$items$href, collapse = ', ')} else {NA}
    results_df$external_anchors_use_rel_noopener_outerHTML <- if (fun_lhex("external_anchors_use_rel_noopener","outerHTML")) {paste0(results$external_anchors_use_rel_noopener$items$outerHTML, collapse = ', ')} else {NA}
    results_df$external_anchors_use_rel_noopener_target <- if (fun_lhex("external_anchors_use_rel_noopener","target")) {paste0(results$external_anchors_use_rel_noopener$items$target, collapse = ', ')} else {NA}

    # geolocation_on_start----------------------------------
    results_df$geolocation_on_start_urls <- if (fun_lhex("geolocation_on_start","url")) {paste0(results$geolocation_on_start$items$url, collapse = ', ')} else {NA}
    results_df$geolocation_on_start_overall_savings_bytes <- if (fun_lhex("geolocation_on_start","overallSavingsBytes")) {paste0(results$geolocation_on_start$overallSavingsBytes, collapse = ', ')} else {NA}
    results_df$geolocation_on_start_overall_savings_ms <- if (fun_lhex("geolocation_on_start","overallSavingsMs")) {paste0(results$geolocation_on_start$overallSavingsMs, collapse = ', ')} else {NA}

    # doctype----------------------------------
    results_df$doctype_urls <- if (fun_lhex("doctype","url")) {paste0(results$doctype$items$url, collapse = ', ')} else {NA}
    # results_df$doctype_overall_savings_bytes <- if (fun_lhex("doctype","overallSavingsBytes")) {paste0(results$doctype$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$doctype_overall_savings_ms <- if (fun_lhex("doctype","overallSavingsMs")) {paste0(results$doctype$overallSavingsMs, collapse = ', ')} else {NA}

    # no_vulnerable_libraries----------------------------------
    results_df$no_vulnerable_libraries_detectedLib.text <- if (fun_lhex("no_vulnerable_libraries","detectedLib.text")) {paste0(results$no_vulnerable_libraries$items$detectedLib.text, collapse = ', ')} else {NA}
    results_df$no_vulnerable_libraries_detectedLib.type <- if (fun_lhex("no_vulnerable_libraries","detectedLib.type")) {paste0(results$no_vulnerable_libraries$items$detectedLib.type, collapse = ', ')} else {NA}
    results_df$no_vulnerable_libraries_detectedLib.url <- if (fun_lhex("no_vulnerable_libraries","detectedLib.url")) {paste0(results$no_vulnerable_libraries$items$detectedLib.url, collapse = ', ')} else {NA}
    results_df$no_vulnerable_libraries_highestSeverity <- if (fun_lhex("no_vulnerable_libraries","highestSeverity")) {paste0(results$no_vulnerable_libraries$items$highestSeverity, collapse = ', ')} else {NA}
    results_df$no_vulnerable_libraries_vulnCount <- if (fun_lhex("no_vulnerable_libraries","vulnCount")) {paste0(results$no_vulnerable_libraries$items$vulnCount, collapse = ', ')} else {NA}

    # js_libraries----------------------------------
    results_df$js_libraries_name <- if (fun_lhex("js_libraries","name")) {paste0(results$js_libraries$items$name, collapse = ', ')} else {NA}
    results_df$js_libraries_npm <- if (fun_lhex("js_libraries","npm")) {paste0(results$js_libraries$items$npm, collapse = ', ')} else {NA}
    results_df$js_libraries_version <- if (fun_lhex("js_libraries","version")) {paste0(results$js_libraries$items$version, collapse = ', ')} else {NA}



    # deprecations----------------------------------
    results_df$deprecations_urls <- if (fun_lhex("deprecations","url")) {paste0(results$deprecations$items$url, collapse = ', ')} else {NA}
    results_df$deprecations_overall_savings_bytes <- if (fun_lhex("deprecations","overallSavingsBytes")) {paste0(results$deprecations$overallSavingsBytes, collapse = ', ')} else {NA}
    results_df$deprecations_overall_savings_ms <- if (fun_lhex("deprecations","overallSavingsMs")) {paste0(results$deprecations$overallSavingsMs, collapse = ', ')} else {NA}

    # password_inputs_can_be_pasted_into----------------------------------
    results_df$password_inputs_can_be_pasted_into_urls <- if (fun_lhex("password_inputs_can_be_pasted_into","url")) {paste0(results$password_inputs_can_be_pasted_into$items$url, collapse = ', ')} else {NA}
    results_df$password_inputs_can_be_pasted_into_overall_savings_bytes <- if (fun_lhex("password_inputs_can_be_pasted_into","overallSavingsBytes")) {paste0(results$password_inputs_can_be_pasted_into$overallSavingsBytes, collapse = ', ')} else {NA}
    results_df$password_inputs_can_be_pasted_into_overall_savings_ms <- if (fun_lhex("password_inputs_can_be_pasted_into","overallSavingsMs")) {paste0(results$password_inputs_can_be_pasted_into$overallSavingsMs, collapse = ', ')} else {NA}

    # errors_in_console----------------------------------
    results_df$errors_in_console_urls <- if (fun_lhex("errors_in_console","url")) {paste0(results$errors_in_console$items$url, collapse = ', ')} else {NA}
    results_df$errors_in_console_overall_savings_bytes <- if (fun_lhex("errors_in_console","overallSavingsBytes")) {paste0(results$errors_in_console$overallSavingsBytes, collapse = ', ')} else {NA}
    results_df$errors_in_console_overall_savings_ms <- if (fun_lhex("errors_in_console","overallSavingsMs")) {paste0(results$errors_in_console$overallSavingsMs, collapse = ', ')} else {NA}

    # image_aspect_ratio----------------------------------
    results_df$image_aspect_ratio_urls <- if (fun_lhex("image_aspect_ratio","url")) {paste0(results$image_aspect_ratio$items$url, collapse = ', ')} else {NA}
    results_df$image_aspect_ratio_displayedAspectRatio <- if (fun_lhex("image_aspect_ratio","displayedAspectRatio")) {paste0(results$image_aspect_ratio$items$displayedAspectRatio, collapse = ', ')} else {NA}
    results_df$image_aspect_ratio_doRatiosMatch <- if (fun_lhex("image_aspect_ratio","doRatiosMatch")) {paste0(results$image_aspect_ratio$items$doRatiosMatch, collapse = ', ')} else {NA}
    results_df$image_aspect_ratio_actualAspectRatio <- if (fun_lhex("image_aspect_ratio","actualAspectRatio")) {paste0(results$image_aspect_ratio$items$actualAspectRatio, collapse = ', ')} else {NA}

  }

  # pwa -----------------------------------------------------------------------
  # gsub("-", "_", parsed$lighthouseResult$categories$pwa$auditRefs$id)
  if ("pwa" %in% categories) {

    # load_fast_enough_for_pwa----------------------------------
    results_df$load_fast_enough_for_pwa_urls <- if (fun_lhex("load_fast_enough_for_pwa","url")) {paste0(results$load_fast_enough_for_pwa$items$url, collapse = ', ')} else {NA}
    # results_df$load_fast_enough_for_pwa_overall_savings_bytes <- if (fun_lhex("load_fast_enough_for_pwa","overallSavingsBytes")) {paste0(results$load_fast_enough_for_pwa$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$load_fast_enough_for_pwa_overall_savings_ms <- if (fun_lhex("load_fast_enough_for_pwa","overallSavingsMs")) {paste0(results$load_fast_enough_for_pwa$overallSavingsMs, collapse = ', ')} else {NA}


    # notification_on_start----------------------------------
    results_df$notification_on_start_urls <- if (fun_lhex("load_fast_enough_for_pwa","url")) {paste0(results$notification_on_start$items$url, collapse = ', ')} else {NA}
    results_df$notification_on_start_overall_savings_bytes <- if (fun_lhex("load_fast_enough_for_pwa","overallSavingsBytes")) {paste0(results$notification_on_start$overallSavingsBytes, collapse = ', ')} else {NA}
    results_df$notification_on_start_overall_savings_ms <- if (fun_lhex("load_fast_enough_for_pwa","overallSavingsMs")) {paste0(results$notification_on_start$overallSavingsMs, collapse = ', ')} else {NA}

    # works_offline----------------------------------
    results_df$works_offline_urls <- if (fun_lhex("works_offline","url")) {paste0(results$works_offline$items$url, collapse = ', ')} else {NA}
    # results_df$works_offline_overall_savings_bytes <- if (fun_lhex("works_offline","overallSavingsBytes")) {paste0(results$works_offline$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$works_offline_overall_savings_ms <- if (fun_lhex("works_offline","overallSavingsMs")) {paste0(results$works_offline$overallSavingsMs, collapse = ', ')} else {NA}

    # offline_start_url----------------------------------
    results_df$offline_start_url_urls <- if (fun_lhex("offline_start_url","url")) {paste0(results$offline_start_url$items$url, collapse = ', ')} else {NA}
    # results_df$offline_start_url_overall_savings_bytes <- if (fun_lhex("offline_start_url","overallSavingsBytes")) {paste0(results$offline_start_url$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$offline_start_url_overall_savings_ms <- if (fun_lhex("offline_start_url","overallSavingsMs")) {paste0(results$offline_start_url$overallSavingsMs, collapse = ', ')} else {NA}

    # is_on_https----------------------------------
    results_df$is_on_https_urls <- if (fun_lhex("is_on_https","url")) {paste0(results$is_on_https$items$url, collapse = ', ')} else {NA}

    # service_worker----------------------------------
    results_df$service_worker_urls <- if (fun_lhex("service_worker","url")) {paste0(results$service_worker$items$url, collapse = ', ')} else {NA}
    # results_df$service_worker_overall_savings_bytes <- if (fun_lhex("service_worker","overallSavingsBytes")) {paste0(results$service_worker$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$service_worker_overall_savings_ms <- if (fun_lhex("service_worker","overallSavingsMs")) {paste0(results$service_worker$overallSavingsMs, collapse = ', ')} else {NA}

    # installable_manifest----------------------------------
    results_df$installable_manifest_isParseFailure <- if (fun_lhex("installable_manifest","isParseFailure")) {paste0(results$installable_manifest$items$isParseFailure, collapse = ', ')} else {NA}
    results_df$installable_manifest_failures <- if (fun_lhex("installable_manifest","failures")) {paste0(results$installable_manifest$items$failures, collapse = ', ')} else {NA}
    results_df$installable_manifest_parseFailureReason <- if (fun_lhex("installable_manifest","parseFailureReason")) {paste0(results$installable_manifest$items$parseFailureReason, collapse = ', ')} else {NA}

    # redirects_http----------------------------------
    results_df$redirects_http_urls <- if (fun_lhex("redirects_http","url")) {paste0(results$redirects_http$items$url, collapse = ', ')} else {NA}
    # results_df$redirects_http_overall_savings_bytes <- if (fun_lhex("redirects_http","overallSavingsBytes")) {paste0(results$redirects_http$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$redirects_http_overall_savings_ms <- if (fun_lhex("redirects_http","overallSavingsMs")) {paste0(results$redirects_http$overallSavingsMs, collapse = ', ')} else {NA}

    # splash_screen----------------------------------
    results_df$splash_screen_parseFailureReason <- if (fun_lhex("splash_screen","parseFailureReason")) {paste0(results$splash_screen$items$parseFailureReason, collapse = ', ')} else {NA}
    results_df$splash_screen_isParseFailure <- if (fun_lhex("splash_screen","isParseFailure")) {paste0(results$splash_screen$items$isParseFailure, collapse = ', ')} else {NA}
    results_df$splash_screen_failures <- if (fun_lhex("splash_screen","failures")) {paste0(results$splash_screen$items$failures, collapse = ', ')} else {NA}

    # themed_omnibox----------------------------------
    results_df$themed_omnibox_parseFailureReason <- if (fun_lhex("themed_omnibox","parseFailureReason")) {paste0(results$themed_omnibox$items$parseFailureReason, collapse = ', ')} else {NA}
    results_df$themed_omnibox_isParseFailure <- if (fun_lhex("themed_omnibox","isParseFailure")) {paste0(results$themed_omnibox$items$isParseFailure, collapse = ', ')} else {NA}
    results_df$themed_omnibox_themeColor <- if (fun_lhex("themed_omnibox","themeColor")) {paste0(results$themed_omnibox$items$themeColor, collapse = ', ')} else {NA}
    results_df$themed_omnibox_failures <- if (fun_lhex("themed_omnibox","failures")) {paste0(results$themed_omnibox$items$failures, collapse = ', ')} else {NA}

    # content_width----------------------------------
    results_df$content_width_urls <- if (fun_lhex("content_width","url")) {paste0(results$content_width$items$url, collapse = ', ')} else {NA}
    # results_df$content_width_overall_savings_bytes <- if (fun_lhex("content_width","overallSavingsBytes")) {paste0(results$content_width$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$content_width_overall_savings_ms <- if (fun_lhex("content_width","overallSavingsMs")) {paste0(results$content_width$overallSavingsMs, collapse = ', ')} else {NA}



    # without_javascript----------------------------------
    results_df$without_javascript_urls <- if (fun_lhex("without_javascript","url")) {paste0(results$without_javascript$items$url, collapse = ', ')} else {NA}
    # results_df$without_javascript_overall_savings_bytes <- if (fun_lhex("without_javascript","overallSavingsBytes")) {paste0(results$without_javascript$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$without_javascript_overall_savings_ms <- if (fun_lhex("without_javascript","overallSavingsMs")) {paste0(results$without_javascript$overallSavingsMs, collapse = ', ')} else {NA}

    # pwa_cross_browser----------------------------------
    results_df$pwa_cross_browser_urls <- if (fun_lhex("pwa_cross_browser","url")) {paste0(results$pwa_cross_browser$items$url, collapse = ', ')} else {NA}
    # results_df$pwa_cross_browser_overall_savings_bytes <- if (fun_lhex("pwa_cross_browser","overallSavingsBytes")) {paste0(results$pwa_cross_browser$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$pwa_cross_browser_overall_savings_ms <- if (fun_lhex("pwa_cross_browser","overallSavingsMs")) {paste0(results$pwa_cross_browser$overallSavingsMs, collapse = ', ')} else {NA}

    # pwa_page_transitions----------------------------------
    results_df$pwa_page_transitions_urls <- if (fun_lhex("pwa_page_transitions","url")) {paste0(results$pwa_page_transitions$items$url, collapse = ', ')} else {NA}
    # results_df$pwa_page_transitions_overall_savings_bytes <- if (fun_lhex("pwa_page_transitions","overallSavingsBytes")) {paste0(results$pwa_page_transitions$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$pwa_page_transitions_overall_savings_ms <- if (fun_lhex("pwa_page_transitions","overallSavingsMs")) {paste0(results$pwa_page_transitions$overallSavingsMs, collapse = ', ')} else {NA}

    # pwa_each_page_has_url----------------------------------
    results_df$pwa_each_page_has_url_urls <- if (fun_lhex("pwa_each_page_has_url","url")) {paste0(results$pwa_each_page_has_url$items$url, collapse = ', ')} else {NA}
    # results_df$pwa_each_page_has_url_overall_savings_bytes <- if (fun_lhex("pwa_each_page_has_url","overallSavingsBytes")) {paste0(results$pwa_each_page_has_url$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$pwa_each_page_has_url_overall_savings_ms <- if (fun_lhex("pwa_each_page_has_url","overallSavingsMs")) {paste0(results$pwa_each_page_has_url$overallSavingsMs, collapse = ', ')} else {NA}
  }

  # seo -----------------------------------------------------------------------
  # gsub("-", "_", parsed$lighthouseResult$categories$seo$auditRefs$id)
  if ("seo" %in% categories) {
    # viewport----------------------------------
    results_df$viewport_urls <- if (fun_lhex("viewport","url")) {paste0(results$viewport$items$url, collapse = ', ')} else {NA}
    # results_df$viewport_overall_savings_bytes <- if (fun_lhex("viewport","overallSavingsBytes")) {paste0(results$viewport$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$viewport_overall_savings_ms <- if (fun_lhex("viewport","overallSavingsMs")) {paste0(results$viewport$overallSavingsMs, collapse = ', ')} else {NA}

    # document_title----------------------------------
    results_df$document_title_urls <- if (fun_lhex("document_title","url")) {paste0(results$document_title$items$url, collapse = ', ')} else {NA}
    # results_df$document_title_overall_savings_bytes <- if (fun_lhex("document_title","overallSavingsBytes")) {paste0(results$document_title$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$document_title_overall_savings_ms <- if (fun_lhex("document_title","overallSavingsMs")) {paste0(results$document_title$overallSavingsMs, collapse = ', ')} else {NA}

    # meta_description----------------------------------
    results_df$meta_description_urls <- if (fun_lhex("meta_description","url")) {paste0(results$meta_description$items$url, collapse = ', ')} else {NA}
    # results_df$meta_description_overall_savings_bytes <- if (fun_lhex("meta_description","overallSavingsBytes")) {paste0(results$meta_description$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$meta_description_overall_savings_ms <- if (fun_lhex("meta_description","overallSavingsMs")) {paste0(results$meta_description$overallSavingsMs, collapse = ', ')} else {NA}

    # http_status_code----------------------------------
    results_df$http_status_code_urls <- if (fun_lhex("http_status_code","url")) {paste0(results$http_status_code$items$url, collapse = ', ')} else {NA}
    # results_df$http_status_code_overall_savings_bytes <- if (fun_lhex("http_status_code","overallSavingsBytes")) {paste0(results$http_status_code$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$http_status_code_overall_savings_ms <- if (fun_lhex("http_status_code","overallSavingsMs")) {paste0(results$http_status_code$overallSavingsMs, collapse = ', ')} else {NA}

    # link_text----------------------------------
    results_df$link_text_urls <- if (fun_lhex("link_text","url")) {paste0(results$link_text$items$url, collapse = ', ')} else {NA}
    results_df$link_text_overall_savings_bytes <- if (fun_lhex("link_text","overallSavingsBytes")) {paste0(results$link_text$overallSavingsBytes, collapse = ', ')} else {NA}
    results_df$link_text_overall_savings_ms <- if (fun_lhex("link_text","overallSavingsMs")) {paste0(results$link_text$overallSavingsMs, collapse = ', ')} else {NA}

    # is_crawlable----------------------------------
    results_df$is_crawlable_urls <- if (fun_lhex("is_crawlable","url")) {paste0(results$is_crawlable$items$url, collapse = ', ')} else {NA}
    results_df$is_crawlable_overall_savings_bytes <- if (fun_lhex("is_crawlable","overallSavingsBytes")) {paste0(results$is_crawlable$overallSavingsBytes, collapse = ', ')} else {NA}
    results_df$is_crawlable_overall_savings_ms <- if (fun_lhex("is_crawlable","overallSavingsMs")) {paste0(results$is_crawlable$overallSavingsMs, collapse = ', ')} else {NA}

    # robots_txt----------------------------------
    results_df$robots_txt_urls <- if (fun_lhex("robots_txt","url")) {paste0(results$robots_txt$items$url, collapse = ', ')} else {NA}
    results_df$robots_txt_overall_savings_bytes <- if (fun_lhex("robots_txt","overallSavingsBytes")) {paste0(results$robots_txt$overallSavingsBytes, collapse = ', ')} else {NA}
    results_df$robots_txt_overall_savings_ms <- if (fun_lhex("robots_txt","overallSavingsMs")) {paste0(results$robots_txt$overallSavingsMs, collapse = ', ')} else {NA}

    # hreflang----------------------------------
    results_df$hreflang_urls <- if (fun_lhex("hreflang","url")) {paste0(results$hreflang$items$url, collapse = ', ')} else {NA}
    results_df$hreflang_overall_savings_bytes <- if (fun_lhex("hreflang","overallSavingsBytes")) {paste0(results$hreflang$overallSavingsBytes, collapse = ', ')} else {NA}
    results_df$hreflang_overall_savings_ms <- if (fun_lhex("hreflang","overallSavingsMs")) {paste0(results$hreflang$overallSavingsMs, collapse = ', ')} else {NA}

    # canonical----------------------------------
    results_df$canonical_urls <- if (fun_lhex("canonical","url")) {paste0(results$canonical$items$url, collapse = ', ')} else {NA}
    # results_df$canonical_overall_savings_bytes <- if (fun_lhex("canonical","overallSavingsBytes")) {paste0(results$canonical$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$canonical_overall_savings_ms <- if (fun_lhex("canonical","overallSavingsMs")) {paste0(results$canonical$overallSavingsMs, collapse = ', ')} else {NA}

    # font_size----------------------------------
    results_df$font_size_source <- if (fun_lhex("font_size","source")) {paste0(results$font_size$items$source, collapse = ', ')} else {NA}
    results_df$font_size_coverage <- if (fun_lhex("font_size","coverage")) {paste0(results$font_size$items$coverage, collapse = ', ')} else {NA}
    results_df$font_size_fontsize <- if (fun_lhex("font_size","fontSize")) {paste0(results$font_size$items$fontSize, collapse = ', ')} else {NA}

    # plugins----------------------------------
    results_df$plugins_urls <- if (fun_lhex("plugins","url")) {paste0(results$plugins$items$url, collapse = ', ')} else {NA}
    results_df$plugins_overall_savings_bytes <- if (fun_lhex("plugins","overallSavingsBytes")) {paste0(results$plugins$overallSavingsBytes, collapse = ', ')} else {NA}
    results_df$plugins_overall_savings_ms <- if (fun_lhex("plugins","overallSavingsMs")) {paste0(results$plugins$overallSavingsMs, collapse = ', ')} else {NA}

    # tap_targets----------------------------------
    results_df$tap_targets_urls <- if (fun_lhex("tap_targets","url")) {paste0(results$tap_targets$items$url, collapse = ', ')} else {NA}
    results_df$tap_targets_overall_savings_bytes <- if (fun_lhex("tap_targets","overallSavingsBytes")) {paste0(results$tap_targets$overallSavingsBytes, collapse = ', ')} else {NA}
    results_df$tap_targets_overall_savings_ms <- if (fun_lhex("tap_targets","overallSavingsMs")) {paste0(results$tap_targets$overallSavingsMs, collapse = ', ')} else {NA}

    # mobile_friendly----------------------------------
    results_df$mobile_friendly_urls <- if (fun_lhex("mobile_friendly","url")) {paste0(results$mobile_friendly$items$url, collapse = ', ')} else {NA}
    # results_df$mobile_friendly_overall_savings_bytes <- if (fun_lhex("mobile_friendly","overallSavingsBytes")) {paste0(results$mobile_friendly$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$mobile_friendly_overall_savings_ms <- if (fun_lhex("mobile_friendly","overallSavingsMs")) {paste0(results$mobile_friendly$overallSavingsMs, collapse = ', ')} else {NA}

    # structured_data----------------------------------
    results_df$structured_data_urls <- if (fun_lhex("structured_data","url")) {paste0(results$structured_data$items$url, collapse = ', ')} else {NA}
    # results_df$structured_data_overall_savings_bytes <- if (fun_lhex("structured_data","overallSavingsBytes")) {paste0(results$structured_data$overallSavingsBytes, collapse = ', ')} else {NA}
    # results_df$structured_data_overall_savings_ms <- if (fun_lhex("structured_data","overallSavingsMs")) {paste0(results$structured_data$overallSavingsMs, collapse = ', ')} else {NA}

  }

  # returning -----------------------------------------------------------------
  results_df$X1 <- NULL
  return(results_df)
}
