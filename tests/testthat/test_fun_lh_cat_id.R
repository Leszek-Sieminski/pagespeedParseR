# setup -----------------------------------------------------------------------
context("LH helper: category ID for columns")
library(pagespeedParseR)
library(testthat)

# defensive -------------------------------------------------------------------
test_that("id param doesn't accept bad values", {
  expect_error(pagespeedParseR:::fun_lh_cat_id(id = NA,        parsed = parsed_test))
  expect_error(pagespeedParseR:::fun_lh_cat_id(id = NULL,      parsed = parsed_test))
  expect_error(pagespeedParseR:::fun_lh_cat_id(id = "desktop", parsed = parsed_test))
  expect_error(pagespeedParseR:::fun_lh_cat_id(id = "",        parsed = parsed_test))
})

test_that("parsed param doesn't accept bad values", {
  expect_error(pagespeedParseR:::fun_lh_cat_id(id = "performance", parsed = NA))
  expect_error(pagespeedParseR:::fun_lh_cat_id(id = "performance", parsed = NULL))
  expect_error(pagespeedParseR:::fun_lh_cat_id(id = "performance", parsed = ""))
  expect_error(pagespeedParseR:::fun_lh_cat_id(id = "performance", parsed = "parsed_test"))
  expect_error(pagespeedParseR:::fun_lh_cat_id(id = "performance", parsed = list()))
})

# output ----------------------------------------------------------------------
test_that("function return proper ids", {
  categories <- c("performance", "best-practices", "accessibility", "seo", "pwa")
  report_cat_df <- data.frame(category = character(), report_name = character(), stringsAsFactors = F)
  for(i in 1:length(categories)) {
    res <- pagespeedParseR:::fun_lh_cat_id(categories[i], parsed_test)
    report_cat_df <- rbind(report_cat_df, res)
  }

expect_equal(length(dim(report_cat_df)), 2)
expect(is.data.frame(report_cat_df), "report_cat_df not a data frame!")
expect_equal(ncol(report_cat_df), 2)
expect(all(unique(report_cat_df$category) %in% categories), failure_message = 'resulting categories not in c("performance", "best-practices", "accessibility", "seo", "pwa")')
})
