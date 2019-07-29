# setup -----------------------------------------------------------------------
context("LH helper: extract general info from 1 URL")
library(pagespeedParseR)
library(testthat)

# defensive -------------------------------------------------------------------
test_that("x param doesn't accept bad values", {
  expect_error(pagespeedParseR:::fun_lh_extract_general_info(x = NA))
  expect_error(pagespeedParseR:::fun_lh_extract_general_info(x = NULL))
  expect_error(pagespeedParseR:::fun_lh_extract_general_info(x = "desktop"))
  expect_error(pagespeedParseR:::fun_lh_extract_general_info(x = ""))
  expect_error(pagespeedParseR:::fun_lh_extract_general_info(x = list()))
})

# output ----------------------------------------------------------------------
test_that("general info object have proper columns", {
  general_info <- pagespeedParseR:::fun_lh_extract_general_info(x = parsed_test)
  expect(all(c('device', 'url', 'finalUrl', 'status_code') %in% colnames(general_info)),
         "Columns 'device', 'url', 'finalUrl', 'status_code' not in final file")
  expect(ncol(general_info) > 1000, "Final number of columns less than 1000")
})
