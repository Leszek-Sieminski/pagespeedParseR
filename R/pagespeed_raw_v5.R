#' Download Pagespeed v4 raport for an URL as a nested list
#'
#' @description This function can check a single URL (character) and parse
#'    the output into a data frame. This data frame contain all the possible
#'    information from Pagespeed ver 5.
#'
#' @details This function uses version 5 of the API.
#'    Check function \code{pagespeed_raw_v4} for version 4.
#'    If you need less information but in form of a data frame,
#'    use \code{pagespeed_simple_v5}.
#'
#' @param url string. The URL to fetch and analyze
#' @param key string. Pagespeed API key to authenticate. Defaults to
#'     "PAGESPEED_API_KEY" enviroment variable.
#' @param strategy string. The analysis strategy to use. Options: "desktop" or
#'     "mobile". Defaults to "desktop"
#' @param category string. A Lighthouse category to run. Defaults to
#'     "performance". See more in Details section
#' @param interval numeric. Number of seconds to wait between multiple queries.
#'     Defaults to 0.5 second.
#' @param keep_tmp logical. Set to TRUE if you need to keep temporary Rdata file
#'     with parsed response. Defaults to FALSE
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
#' @return nested list
#'
#' @examples
#' \dontrun{
#' single_url_raw_output_5 <- pagespeed_raw_v5("https://www.google.com/")
#' }
pagespeed_raw_v5 <- function(url, key = Sys.getenv("PAGESPEED_API_KEY"),
                             strategy = NULL, category = "performance",
                             interval = 0.5, keep_tmp = FALSE, locale = NULL,
                             utm_campaign = NULL, utm_source = NULL)
{
  message("please wait, v5 is under development")
}
