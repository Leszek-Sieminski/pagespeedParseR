fun_lh_details_extract <- function(audits){
  results <- list()

  for (i in 1:length(audits)) {
    x <- list(details = if (!is.null(audits[[i]]$details)) {audits[[i]]$details} else {NA})
    names(x) <- paste0(gsub("-", "_", audits[[i]]$id), "_details")
    results <- c(results, x)
  }

  # user_timings --------------------------------------------------------------
  # results$user_timings_details

  # first_meaningful_paint ----------------------------------------------------
  # results$first_meaningful_paint_details


  # efficient_animated_content ------------------------------------------------
  # results$efficient_animated_content_details

  # metrics -------------------------------------------------------------------
  results$metrics_details$items$firstContentfulPaint
  results$metrics_details$items$speedIndex
  results$metrics_details$items$observedFirstContentfulPaint
  results$metrics_details$items$observedFirstVisualChange
  results$metrics_details$items$firstMeaningfulPaint
  results$metrics_details$items$observedFirstMeaningfulPaint
  results$metrics_details$items$observedTraceEnd
  results$metrics_details$items$firstCPUIdle
  results$metrics_details$items$observedDomContentLoaded
  results$metrics_details$items$observedNavigationStart
  results$metrics_details$items$interactive
  results$metrics_details$items$observedLoad
  results$metrics_details$items$observedSpeedIndex
  results$metrics_details$items$estimatedInputLatency
  results$metrics_details$items$observedFirstPaint
  results$metrics_details$items$observedLastVisualChange

  # time_to_first_byte --------------------------------------------------------
  # results$time_to_first_byte_details

  # render_blocking_resources -------------------------------------------------
  results$render_blocking_resources_details$overallSavingsMs
  results$render_blocking_resources_details$items$url

  # uses_optimized_images -----------------------------------------------------
  results$render_blocking_resources_details$overallSavingsMs
  results$render_blocking_resources_details$items$url

  # uses_text_compression -----------------------------------------------------
  # results$uses_text_compression_details

  # network_requests ----------------------------------------------------------
  results$network_requests_details$items$url
  results$network_requests_details$items$resourceSize

  # uses_long_cache_ttl -------------------------------------------------------
  results$uses_long_cache_ttl_details$summary$wastedBytes
  results$uses_long_cache_ttl_details$items$url

  # interactive_details -------------------------------------------------------
  # results$interactive_details

  # screenshot_thumbnails -----------------------------------------------------
  # results$screenshot_thumbnails_details

  # font_display --------------------------------------------------------------
  results$font_display_details$items$url
  results$font_display_details$items$wastedMs

  # estimated_input_latency ---------------------------------------------------
  # results$estimated_input_latency_details

  # uses_rel_preconnect -------------------------------------------------------
  # results$uses_rel_preconnect_details$overallSavingsMs
  # results$uses_rel_preconnect_details$items$url

  # bootup_time ---------------------------------------------------------------
  results$bootup_time_details$summary$wastedMs
  results$bootup_time_details$items$url

  # unminified_css ------------------------------------------------------------
  results$unminified_css_details$overallSavingsBytes
  results$unminified_css_details$overallSavingsMs
  results$unminified_css_details$items$url

  # offscreen_images ----------------------------------------------------------
  results$offscreen_images_details$overallSavingsBytes
  results$offscreen_images_details$overallSavingsMs
  results$offscreen_images_details$items$url

  # uses_responsive_images ----------------------------------------------------
  results$uses_responsive_images_details

  # unused_css_rules ----------------------------------------------------------
  results$unused_css_rules_details

  # speed_index ---------------------------------------------------------------
  results$speed_index_details

  # first_cpu_idle ------------------------------------------------------------
  results$first_cpu_idle_details

  # total_byte_weight ---------------------------------------------------------
  results$total_byte_weight_details

  # mainthread_work_breakdown -------------------------------------------------
  results$mainthread_work_breakdown_details

  # first_contentful_paint ----------------------------------------------------
  results$first_contentful_paint_details

  # uses_webp_images ----------------------------------------------------------
  results$uses_webp_images_details

  # critical_request_chains ---------------------------------------------------
  results$critical_request_chains_details

  # dom_size ------------------------------------------------------------------
  results$dom_size_details

  # uses_rel_preload ----------------------------------------------------------
  results$uses_rel_preload_details

  # unminified_javascript -----------------------------------------------------
  results$unminified_javascript_details

  # redirects -----------------------------------------------------------------
  results$redirects_details

  }
# str(results)
# det <- fun_lh_details_extract(audits = audits)
# str(det)



# names(det$metrics_details$items)[!grepl(".*Ts$", names(det$metrics_details$items))]