#' Extract the general information from single URL Lighthouse report
#'
#' @param x object of single report
#'
#' @return data frame
#'
#' @examples
#' \dontrun{
#' res <- fun_lh_extract_general_info(x = obj)
#'
#' }
fun_lh_extract_general_info <- function(x) {
  assert_that(
    !(is.null(x) || is.na(x))
    & is.list(x)
    & !is.data.frame(x)
    # , not_empty(x)
    )

  categories <- x$lighthouseResult$configSettings$onlyCategories

  # 01 creating baseline data frame ---------------------------------------------
  baseline <- data.frame(
    device           = x$lighthouseResult$configSettings$emulatedFormFactor,
    url              = x$lighthouseResult$requestedUrl,
    finalUrl         = x$lighthouseResult$finalUrl,
    status_code      = 200,
    stringsAsFactors = F)

  # 02 report category object ---------------------------------------------------
  report_cat_df <- data.frame(
    category         = character(),
    report_name      = character(),
    stringsAsFactors = F)

  # source("R/fun_lh_cat_id.R")
  for(i in 1:length(categories)){
    res <- fun_lh_cat_id(categories[i], x)
    report_cat_df <- rbind(report_cat_df, res)
  }
  rm(res)

  # 03 basic lighthouse data extraction -----------------------------------------
  # source("R/fun_lh_enhanced_extract.R")
  full_results <- cbind(
    baseline,
    fun_lh_enhanced_extract(x$loadingExperience),
    fun_lh_enhanced_extract(x$lighthouseResult$audits, report_cat_df))

  for(i in 1:length(categories)){
    full_results[paste0("score.", categories[i])] <- x$lighthouseResult$categories[[categories[i]]]$score
  }

  # 04 sorting the columns ------------------------------------------------------
  # capturing column names
  colnames_v <- colnames(full_results)
  # sorting column names A-Z
  colnames_sorted <- sort(colnames_v)
  # deleting worthless columns
  colnames_sorted <- colnames_sorted[!grepl("_details_type|_id", colnames_sorted)]
  # deleting baseline columns (they need to go first!)
  col_scores <- colnames_sorted[grepl("^score.*", colnames_sorted)] # main scores
  colnames_sorted <- colnames_sorted[!colnames_sorted %in% c('device', 'url', 'finalUrl', 'status_code', col_scores)]
  # adding base cols at the beggining
  colnames_sorted <- c(c('device', 'url', 'finalUrl', 'status_code', col_scores), colnames_sorted)

  # choosing the df columns in order
  full_results <- full_results[, colnames_sorted]

  # 05 returning ----------------------------------------------------------------
  return(full_results)
}
