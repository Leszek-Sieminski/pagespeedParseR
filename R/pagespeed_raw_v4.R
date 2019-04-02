#' Title
#'
#' @param url
#' @param strategy
#' @param interval
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
pagespeed_raw_v4 <- function(url, strategy = NULL, interval = 0.5, keep_tmp = FALSE,
                             key = Sys.getenv("PAGESPEED_API_KEY"), filter_third_party = NULL,
                             locale = NULL, rule = NULL, screenshot = NULL, snapshots = NULL,
                             utm_campaign = NULL, utm_source = NULL, fields = NULL)
{
  # downloading ---------------------------------------------------------------
  if (is.null(key) | nchar(key) == 0){
    stop("API key is a NULL - please check it and provide a proper API key.", call. = FALSE)}

  req <- httr::GET(
    url = "https://www.googleapis.com/pagespeedonline/v4/runPagespeed",
    query = list(url = url, key = key, filter_third_party = filter_third_party,
                 locale = locale, rule = rule, screenshot = screenshot,
                 snapshots = snapshots, strategy = strategy,
                 utm_campaign = utm_campaign, utm_source = utm_source,
                 fields = fields))

  # parsing -------------------------------------------------------------------
  # httr::stop_for_status(req) # we don't want to stop for error as we want to know which URL's wasn't properly returned
  if (req$status_code == 200){
    if (httr::http_type(req) != "application/json") {
      stop("API did not return json", call. = FALSE)
    }
    con <- httr::content(req, "text")
    parsed <- jsonlite::fromJSON(con)
    if (keep_tmp) { # saving tmp file for debugging in dev
      rnd <- paste0(
        do.call(paste0,
                replicate(n = 3, sample(x = LETTERS, size = 1, replace = TRUE), simplify = FALSE)),
        sprintf("%03d", sample(x = 999,  size = 1, replace = TRUE)),
        sample(x = LETTERS, size = 1, replace = TRUE))

      # save(parsed, file = paste0("tmp_", url, "_", Sys.Date(), "_",  rnd, ".RData"))
      save(parsed, file = paste0("tmp_",  rnd, ".RData"))
    }
    full_results <- parsed
    return(full_results)
  } else {
    full_results <- NULL
    Sys.sleep(interval)
    return(full_results)
  }
}
