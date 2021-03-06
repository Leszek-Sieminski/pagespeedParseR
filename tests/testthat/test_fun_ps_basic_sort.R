context("PS helper: sort")
library(pagespeedParseR)
library(testthat)

test_that("function properly sorts dataframe's columns order", {
  before <- data.frame("device"      = "device",
                       "url"         = "url",
                       "title"       = "title",
                       "status_code" = "status_code",
                       "C"           = "C",
                       "A"           = "A",
                       "B"           = "B",
                       stringsAsFactors = F)

  ideal <- data.frame("device"      = "device",
                      "title"       = "title",
                      "url"         = "url",
                      "status_code" = "status_code",
                      "A"           = "A",
                      "B"           = "B",
                      "C"           = "C",
                      stringsAsFactors = F)

  after <- pagespeedParseR:::fun_ps_basic_sort(before)
  expect_identical(after, ideal)

})
