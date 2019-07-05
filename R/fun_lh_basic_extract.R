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
    trigger_score <- !is.null(audits[[i]]$score)
    trigger_descr <- !is.null(audits[[i]]$description)
    trigger_displ <- !is.null(audits[[i]]$displayValue)

    x <- data.frame(
      description   = `if`(trigger_descr, audits[[i]]$description, NA),
      score         = `if`(trigger_score, audits[[i]]$score * 100, NA),
      display_value = `if`(trigger_displ, audits[[i]]$displayValue, NA),
      stringsAsFactors = FALSE)

    # finding which category is the report in
    # cat <- filter(report_cat_df, grepl(gsub("-", "_", audits[[i]]$id), report_name))
    cat <- report_cat_df[grepl(gsub("-", "_", audits[[i]]$id, fixed = TRUE), report_cat_df$report_name, fixed = TRUE),]
    cat <- gsub("-", "_", cat$category, fixed = TRUE)

    # safety net if 1 report is in more than 1 category
    cat <- cat[1]

    # finding id
    id <- gsub("-", "_", audits[[i]]$id, fixed = TRUE)

    colnames(x) <- c(paste0(cat, "_", id, "_description"),
                     paste0(cat, "_", id, "_score"),
                     paste0(cat, "_", id, "_display_value"))

    results <- cbind(results, x)
  }

  results$X1 <- NULL

  return(results)
}
