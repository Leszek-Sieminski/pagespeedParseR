#' Extraction of basic details from Lighthouse reports
#'
#' @param audits audits object
#' @param report_cat_df reports category object
#'
#' @return data frame with basic Lighthouse columns
fun_lh_basic_extract <- function(audits, report_cat_df){
  results <- data.frame(1)
  cat <- NULL
  for (i in 1:length(audits)) {
    # creating df with the data
    x <- data.frame(
      description   = ifelse(!is.null(audits[[i]]$description),  audits[[i]]$description, NA),
      score         = ifelse(!is.null(audits[[i]]$score),        audits[[i]]$score * 100, NA),
      display_value = ifelse(!is.null(audits[[i]]$displayValue), audits[[i]]$displayValue, NA),
      stringsAsFactors = FALSE)

    # finding which category is the report in
    # cat <- filter(report_cat_df, grepl(gsub("-", "_", audits[[i]]$id), report_name))
    cat <- dplyr::filter(report_cat_df, grepl(gsub("-", "_", audits[[i]]$id), report_cat_df$report_name))
    cat <- gsub("-", "_", cat$category)

    # safety net if 1 report is in more than 1 category
    if (length(cat) > 1) {
      cat <- cat[1]
    }

    # finding id
    id <- gsub("-", "_", audits[[i]]$id, fixed = TRUE)

    # renaming the columns to keep information which column is in which category & report
    x <- plyr::rename(x, c(
      "description"   = paste0(cat, ".", id, "_description"),
      "score"         = paste0(cat, ".", id, "_score"),
      "display_value" = paste0(cat, ".", id, "_display_value")))

    results <- cbind(results, x)
  }

  results$X1 <- NULL

  return(results)
}
