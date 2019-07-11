#' Authorize your Pagespeed Insights API with a API Key (Token)
#'
#' @param api_key string. Token acquired on
#'     https://console.developers.google.com/ page.
#' @param verbose logical. Defaults to TRUE. Set to FALSE to stop printing
#'     status in the console
#'
#' @return invisibly returns API token into environment variable
#'     PAGESPEED_API_KEY and prints the status
#'
#' @import assertthat
#' @importFrom httr GET
#'
#' @export
#'
#' @examples
#' \dontrun{
#' auth_pagespeed("12345")
#' }
auth_pagespeed <- function(api_key, verbose = TRUE){

  assert_that(
    noNA(api_key), not_empty(api_key), is.string(api_key),
    nchar(api_key) > 0,
    noNA(verbose), not_empty(verbose), is.logical(verbose))

  # testing query
  x <- GET(
    url = "https://www.googleapis.com/pagespeedonline/v4/runPagespeed",
    query = list(
      url = "https://www.w3.org/", #"https://www.google.com/" \
      key = api_key,
      strategy = "desktop"))

  Sys.sleep(0.5)

  if (x$status_code == 200) {
    Sys.setenv("PAGESPEED_API_KEY" = api_key)
    if (verbose) message("API key authorized.")
  } else {
    stop(paste0("Authorization error: HTTP status code ", x$status_code, ". Check your API key."))
  }
}

