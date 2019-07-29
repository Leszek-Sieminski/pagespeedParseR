#' Helper function for extracting audit categories
#'
#' @param id id of the category
#' @param parsed parsed LH output object
#'
#' @return data frame
#'
#' @import assertthat
#' @examples
#' \dontrun{
#' for(i in 1:length(categories)){
#'   res <- fun_lh_cat_id(categories[i], parsed)
#'   report_cat_df <- rbind(report_cat_df, res)
#' }
#'
#' str(report_cat_df)
#' 'data.frame':	198 obs. of  2 variables:
#' $ category   : chr  "performance" "performance" "performance" "performance"
#'     ...
#' $ report_name: chr  "first_contentful_paint" "first_meaningful_paint"
#'     "speed_index" "interactive" ...
#' }
fun_lh_cat_id <- function(id, parsed) {
  # safety net ----------------------------------------------------------------
  assert_that(
    !(is.na(id) || is.null(id)),
    is.string(id),
    nchar(id) >= 3,
    length(id) == 1,
    id %in% c("performance", "accessibility", "best-practices", "seo", "pwa"),
    !(is.na(parsed) || is.null(parsed)),
    is.list(parsed),
    !is.data.frame(parsed)
    # length(parsed) >
  )

  # create a df with ids ------------------------------------------------------
  result <- data.frame(
    category = id,
    report_name = gsub("-", "_", parsed$lighthouseResult$categories[[id]]$auditRefs$id, fixed = T),
    stringsAsFactors = F)
  return(result)
}
