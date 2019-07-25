#' Download new Pagespeed report (Lighthouse) (Pagespeed API ver. 5)
#'
#' @description Download PageSpeed Insights v5 report ("Lighthouse")
#'      for single or multiple URLs in variety of options.
#'      You can choose the the resulting object to be an original nested list
#'      object (\code{as_reference = FALSE}) returned into enviroment, or a
#'      reference (\code{as_reference = TRUE}) to such list saved as the
#'      physical compressed binary file containing all the output.
#'
#' @param url vector of character strings. The URLs to fetch and analyze
#'     MUST contain \code{"http://"} or \code{"https://"}
#' @param key string. Pagespeed API key to authenticate. Use
#'     \code{auth_pagespeed()} function to authenticate before requesting
#'     report
#' @param strategy string/character vector. The analysis strategy to use.
#'     Options: \code{"desktop"}, \code{"mobile"},
#'     or \code{"c("desktop", "mobile")"} to return both results in one
#'     function call
#' @param categories string. A Lighthouse categories to run.
#'     Defaults to \code{"performance".} See more in Details section
#' @param as_reference logical. If TRUE, function will return only a reference
#'     to an output list stored in a local cache. Defaults to \code{FALSE}.
#'     *IMPORTANT*:
#'     if you choose the reference mode, output object will appear in R's
#'     global enviroment and will be accessible, but deleting \code{'.llo'}
#'     file will break the reference and the data will be no more accessible.
#'     This may be a good choice when you want to download more than 500-1000
#'     URLs as this should help you avoid memory & allocation issues.
#'     See more in Details section
#' @param reference_path string. If you decide to export the binary file with
#'     the exported reports, you may want to choose different directory to
#'     avoid overwriting it later. Proper path MUST be ending with
#'     '\code{.llo}' format. If NULL, the file will be stored in current
#'     working directory and might be overwritten later. Defaults to NULL
#' @param interval numeric. Number of seconds to wait between multiple queries.
#'     Defaults to \code{0.5} second
#' @param locale string. The locale used to localize formatted results.
#'     Defaults to \code{NULL}
#' @param utm_campaign string. Campaign name for analytics. Defaults to
#'     \code{NULL}
#' @param utm_source string. Campaign source for analytics. Defaults to
#'     \code{NULL}
#'
#' @details
#' \itemize{
#'   \item \code{output_type}: this parameter regulates how the output will be
#'     parsed and stored.
#'     For "simple" - formatted data frame that contains most of the data
#'     (scores, recommendations and error occurences).
#'     For "raw" - unformatted nested list that contains all the data that
#'     was returned by the API.
#'
#'   \item \code{api_version}: this parameter regulates which API version is to
#'     create the report. Legacy version 4 is a classic Pagespeed, and the
#'     new version 5 returns Lighthouse reports.
#'
#'   \item \code{categories}: this parameter regulates which of the tests'
#'     categories from Lighthouse are to be run. You can select more than one
#'     in a vector.
#'     Options: "accessibility", "best-practices", "performance", "pwa",
#'     "seo".
#'
#'   \item \code{as_reference}: this parameter controls the output type. If
#'   \code{FALSE}, it will returned a nested list with all the reports. If
#'   set to \code{TRUE}, it will return only a reference to a compressed
#'   temporary binary file \code{'db.llo'} stored in the current working
#'   directory.
#'
#'   Caching results like this may be a good choice if you plan to
#'   check hundreds/thousands of URLs because this way their combined size
#'   should not be causing memory issues in R as growing the \code{'db.llo'}
#'   file doesn't require loading it to do memory every time and copying.
#'
#'   You can then access contents of the referenced file easily as it behaves
#'   like a normal list. For example, \code{str(pagespeed_results)} will return
#'   the structure of the list inside the binary file, and for example
#'   \code{pagespeed_results[[1]]} will return its first element.
#'
#'   IMPORTANT:
#'   if you choose the reference mode, mind that output object will appear in
#'   R's global enviroment and will be accessible like a normal nested list
#'   object, but deleting it's \code{'.llo'} file will break the reference.
#'   While the object will seemingly stay in global enviroment, the data will
#'   be lost. There should appear a warning that file does no longer exist.
#'
#'   Setting \code{as_reference = FALSE} will delete the temporary file so you
#'   don't have to worry about managing physical files. However, returning big
#'   reports (depending on your machine 500-1000 URLs and more) to global
#'   enviroment may cause memory issues.
#'
#'   For more information, please read:
#'   https://cran.r-project.org/web/packages/largeList/vignettes/intro_largeList.html
#' }
#'
#' @return two options: nested list (if \code{as_reference = FALSE}) or a
#'    reference to a list in a binary \code{'db.llo'} file
#'    (if \code{as_reference = TRUE}).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # download nested list with "Performance" Lighthouse report for Google.com
#' lh_nl_1 <- download_lighthouse(
#'   url = "https://www.google.com") # return nested list with the report
#'
#'
#'
#' # check "Performance" for Google.com & Bing.com for both desktop & mobile and
#' # return in a nested list
#' lh_nl_2 <- download_lighthouse(
#'   url = c("https://www.google.com",
#'           "https://www.bing.com/"),
#'   strategy = c("desktop", # check both desktop and mobile, bind
#'                "mobile"),
#'   interval = 1,           # wait 1 second between the calls to API
#'   categories = "performance") # which Lighthouse reports
#'                               # are to be run?
#'
#'
#'
#' # download a reference with "Performance" Lighthouse report for Google.com
#' lh_ref_1 <- download_lighthouse(
#'   url = "https://www.google.com"
#'   as_reference = TRUE) # return a reference to a temporary file with the
#'                        # report
#'
#' # data in the file can be easily checked or accessed:
#'
#' > lh_ref_1[[1]][[1]]
#' [1] "CAPTCHA_NOT_NEEDED"
#'
#' > str(lh_ref_1)
#' List of 1
#' $ :List of 7
#' ..$ captchaResult          : chr "CAPTCHA_NOT_NEEDED"
#' ..$ kind                   : chr "pagespeedonline#result"
#' ..$ id                     : chr "https://www.google.com/"
#' ..$ loadingExperience      :List of 4
#' .. ..$ id              : chr "https://www.google.com/"
#' .. ..$ metrics         :List of 2
#' .. .. ..$ FIRST_CONTENTFUL_PAINT_MS:List of 3
#' .. .. .. ..$ percentile   : int 6846
#' .. .. .. ..$ distributions:'data.frame':	3 obs. of  3 variables:
#'   .. .. .. .. ..$ min       : int [1:3] 0 1000 2500
#' .. .. .. .. ..$ max       : int [1:3] 1000 2500 NA
#' .. .. .. .. ..$ proportion: num [1:3] 0.53 0.226 0.244
#' .. .. .. ..$ category     : chr "SLOW"
#' .. .. ..$ FIRST_INPUT_DELAY_MS     :List of 3
#' .. .. .. ..$ percentile   : int 148
#' .. .. .. ..$ distributions:'data.frame':	3 obs. of  3 variables:
#'   .. .. .. .. ..$ min       : int [1:3] 0 50 250
#' .. .. .. .. ..$ max       : int [1:3] 50 250 NA
#' .. .. .. .. ..$ proportion: num [1:3] 0.8951 0.0726 0.0323
#' .. .. .. ..$ category     : chr "AVERAGE"
#' .. ..$ overall_category: chr "SLOW"
#' .. ..$ initial_url     : chr "https://www.google.com/"
#' # (truncated)
#' }
download_lighthouse <- function(
  url,
  key            = Sys.getenv("PAGESPEED_API_KEY"),
  strategy       = "desktop",
  categories     = "performance",
  as_reference   = FALSE,
  reference_path = NULL,
  interval       = 0.5,
  locale         = NULL,
  utm_campaign   = NULL,
  utm_source     = NULL)
{
  # safety net ----------------------------------------------------------------
  if (is.null(key) | nchar(key) == 0){
    stop("API key is a NULL or has 0 characters.
         Please check it and provide a proper API key.", call. = FALSE)}

  assert_that(
    all(not_empty(url)), all(!is.null(url)), all(is.character(url)) & length(url) > 0, all(grepl(".", url, fixed = T)),
    is.string(key), all(!is.na(strategy)) & (is.null(strategy) || (is.character(strategy) & all(strategy %in% c("desktop", "mobile")))),
    is.null(categories) || (is.character(categories) & categories %in% c("accessibility", "best-practices", "performance", "pwa", "seo")),
    is.logical(as_reference), not_empty(as_reference), !is.na(as_reference),
    is.null(reference_path) || (is.string(reference_path) & nchar(reference_path) > 0 & grepl(".llo", reference_path, fixed = T)),
    is.number(interval) & interval >= 0 & interval <= 120,
    (is.string(locale) & nchar(locale) > 0) || is.null(locale), is.string(utm_campaign) | is.null(utm_campaign),
    is.string(utm_source) | is.null(utm_source))

  # large list cache preparation ----------------------------------------------
  saveList(
    object   = list(),
    file     = paste0(getwd(), "/db.llo"),
    append   = FALSE,
    compress = TRUE)

  # creating report -----------------------------------------------------------
  lh_raw_2_vec(
    url = url, key = key, strategy = strategy, categories = categories,
    interval = interval, locale = locale,
    utm_campaign = utm_campaign, utm_source = utm_source)

  # exporting -----------------------------------------------------------------
  if (!as_reference) {
      # export: nested list ---------------------------------------------------
      results <- readList(file = paste0(getwd(), "/db.llo"))
      file.remove(paste0(getwd(), "/db.llo"))
      return(results)
  } else {
      # export: reference -----------------------------------------------------
      if (!is.null(reference_path)) {
        file.copy(from = paste0(getwd(), "/db.llo"), to = reference_path, overwrite = T)
        file.remove(paste0(getwd(), "/db.llo"))
        results <- getList(file = reference_path)
        return(results)
      } else {
        results <- getList(file = "/db.llo")
        return(results)
      }
    }
}
