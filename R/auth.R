#' Authorize your Pagespeed Insights API with a API Key (Token)
#'
#' @param api_key string. Token acquired on
#'     https://console.developers.google.com/ page.
#' @param verbose logical. Defaults to TRUE. Set to FALSE to stop printing
#'     status in the console
#'
#' @return invisibly returns API token into environment variable
#'     PAGESPEED_API_KEY and prints the status
#' @export
#'
#' @examples
#' \dontrun{
#' auth_pagespeed("12345")
#' }
auth_pagespeed <- function(api_key, verbose = TRUE){

  assertthat::assert_that(noNA(api_key), not_empty(api_key), is.string(api_key),
                          noNA(verbose), not_empty(verbose), is.logical(verbose))

  # testing query
  x <- httr::GET(url = "https://www.googleapis.com/pagespeedonline/v4/runPagespeed",
                 query = list(url = "https://www.google.com/", #"https://www.w3.org/", #,
                              key = api_key,
                              strategy = "desktop"))

  if (x$status_code == 200){
    Sys.setenv("PAGESPEED_API_KEY" = api_key)
    if (verbose) {
      message("API key authorized.")
    } else {
      warning(
        paste0("Authorization error: HTTP status code ", x$status_code, ". Check your API key.")
      )
    }
  }
}
