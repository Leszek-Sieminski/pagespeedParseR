fun_lh_basic_extract <- function(audits){
  results <- data.frame(1)
  for (i in 1:length(audits)) {
    # i = 2
    x <- data.frame(
      description   = ifelse(!is.null(audits[[i]]$description),  audits[[i]]$description, NA),
      score         = ifelse(!is.null(audits[[i]]$score),        audits[[i]]$score * 100, NA),
      display_value = ifelse(!is.null(audits[[i]]$displayValue), audits[[i]]$displayValue, NA),
      stringsAsFactors = FALSE)

    x <- plyr::rename(x, c(
      "description"   = paste0(gsub("-", "_", audits[[i]]$id, fixed = TRUE), "_description"),
      "score"         = paste0(gsub("-", "_", audits[[i]]$id, fixed = TRUE), "_score"),
      "display_value" = paste0(gsub("-", "_", audits[[i]]$id, fixed = TRUE), "_display_value")))

    results <- cbind(results, x)
  }
  return(results)
}

# basic <- fun_lh_basic_extract(audits)
# basic <- basic[, -1]
# View(basic)
