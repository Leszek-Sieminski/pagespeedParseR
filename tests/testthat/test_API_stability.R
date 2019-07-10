# context("API Stability due to no changes (2019-07-08)")
# library(pagespeedParseR)
#
# testthat::test_that("no new data in API - stable 820 columns", {
#   x <- pagespeedParseR:::lh_simple_1("https://www.w3.org/", strategy = "desktop", interval = 0)
#   testthat::expect_equal(ncol(x), 1217)
# })
#
# testthat::test_that("no new data in API - stable 820 columns", {
#   x <- pagespeedParseR:::lh_simple_1(
#     url        = "https://www.w3.org/",
#     strategy   = "mobile",
#     interval   = 0,
#     categories = c("performance", "accessibility", "best-practices", "seo", "pwa"))
#
#   testthat::expect_equal(ncol(x), 1799)
# })
#
# testthat::test_that("basic output df has proper dimensions (desktop)", {
#   x <- pagespeedParseR:::lh_simple_2_vec("https://www.google.com", strategy = "desktop", interval = 1)
#   y <- pagespeedParseR:::lh_simple_2_vec("https://www.google.com", strategy = "desktop", interval = 1, long_result = F)
#   testthat::expect_equal(nrow(x), 708)
#   testthat::expect_equal(ncol(y), 709)
# })
