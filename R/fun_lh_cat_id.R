#' Helper function for extracting audit categories
#'
#' @param id id of the category
#' @param parsed parsed LH output object
#'
#' @return data frame
#'
#' @examples
#' \dontrun{
#' for(i in 1:length(categories)){
#'   res <- fun_lh_extract_cat_id(categories[i], parsed)
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
  result <- data.frame(
    category = id,
    report_name = gsub("-", "_", parsed$lighthouseResult$categories[[id]]$auditRefs$id, fixed = T),
    stringsAsFactors = F)
  return(result)
}
