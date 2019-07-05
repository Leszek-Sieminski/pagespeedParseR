#' Extraction of extended details from Lighthouse reports
#'
#' @param audits audits object
#' @param report_cat_df reports category object, defaults to null
#'
#' @return data frame with basic Lighthouse columns
#' @examples
#' \dontrun{
#'     results_load_exp <- fun_lh_enhanced_extract(parsed$loadingExperience)
#'     results_audits   <- fun_lh_enhanced_extract(audits, report_cat_df)
#' }
fun_lh_enhanced_extract <- function(audits, report_cat_df = NULL) {
  all_v <- c()
  if (is.null(report_cat_df)) {
    for (i in 1:length(audits)) {
      extracted <- unlist(audits[i])
      names(extracted) <- gsub("-", "_", names(extracted), fixed = T)
      all_v <- c(all_v, extracted)
    }
  } else {
    for (i in 1:length(audits)) {
      cat <- report_cat_df[grepl(gsub("-", "_", audits[[i]]$id, fixed = T), report_cat_df$report_name, fixed = T),]
      cat <- gsub("-", "_", cat$category, fixed = TRUE)[1]
      extracted <- unlist(audits[i])
      names(extracted) <- paste0(cat, "_", gsub("-", "_", names(extracted), fixed = T))
      all_v <- c(all_v, extracted)
    }
  }

  df <- data.frame(matrix(0, ncol = length(all_v), nrow = 1))

  colnames(df) <- names(all_v)
  colnames(df) <- gsub(".", "_", colnames(df), fixed = TRUE)

  for(i in 1:length(all_v)) {
    df[1, i] <- all_v[i]
  }
  return(df)
}

# str(audits$`aria-required-attr`$details)
#
# pętla dla każdego audytu
#   pętla dla każdego elementu w audycie
#     (jeśli ilość podelementów > 1)
#         cośtam
#     (jeśli ilość podelementów < 1)
#         cośtam
#
#
# xd <- fun_lh_enhanced_extract(parsed$loadingExperience)
# xd <- fun_lh_enhanced_extract(audits, report_cat_df = report_cat_df)
#
#
# str(audits$`aria-required-attr`)
# str(audits$`aria-required-attr`$details)
# str(audits$`aria-required-attr`$details$debugData)
# str(audits$`aria-required-attr`$details$headings)
# str(audits$`aria-required-attr`$details$items)
# str(audits$`aria-required-attr`$details$type)
# df$`aria-required-children.id`
# df$`aria-required-children.title`
# df$`aria-required-children.description`
# df$`aria-required-children.score`
# df$`aria-required-children.scoreDisplayMode`
# df$`aria-required-children.details.type`
#
# str(parsed$loadingExperience)
