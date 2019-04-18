context("LH Simple lvl 1")
library(pagespeedParseR)

testthat::test_that("url param doesn't accept bad values", {
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_v5(url = "", strategy = "desktop"))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_v5(url = NA, strategy = "desktop"))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_v5(url = NULL, strategy = "desktop"))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_v5(url = "google com", strategy = "desktop"))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_v5(url = "", strategy = "mobile"))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_v5(url = NA, strategy = "mobile"))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_v5(url = NULL, strategy = "mobile"))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_v5(url = "google com", strategy = "mobile"))
})

testthat::test_that("API key doesn't accept wrong values", {
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_v5("https://www.google.com", strategy = "desktop", key = ""), regexp = "API key is a NULL or has length = 0. Please check it and provide a proper API key.")
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_v5("https://www.google.com", strategy = "desktop", key = NULL), regexp = "argument is of length zero")
})

testthat::test_that("strategy param doesn't accept bad values", {
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_v5("https://www.google.com", strategy = NA))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_v5("https://www.google.com", strategy = ""))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_v5("https://www.google.com", strategy = c("desktop", "mobile")))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_v5("https://www.google.com", strategy = "loremipsum"))
})

# categories = "performance"
testthat::test_that("categories param doesn't accept bad values", {
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_v5("https://www.google.com", categories = ""))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_v5("https://www.google.com", categories = NA))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_v5("https://www.google.com", categories = "loremipsum"))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_v5("https://www.google.com", categories = 1))
  # testthat::expect_error(pagespeedParseR:::pagespeed_simple_v5("https://www.google.com", categories = ""))
  # testthat::expect_error(pagespeedParseR:::pagespeed_simple_v5("https://www.google.com", categories = ""))
  # testthat::expect_error(pagespeedParseR:::pagespeed_simple_v5("https://www.google.com", categories = ""))
})

testthat::test_that("interval param doesn't accept bad values", {
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_v5("https://www.google.com", strategy = "desktop", interval = -1))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_v5("https://www.google.com", strategy = "mobile", interval = -1))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_v5("https://www.google.com", strategy = "desktop", interval = 121))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_v5("https://www.google.com", strategy = "mobile", interval = 121))
})

testthat::test_that("locale param doesn't accept bad values", {
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_v5("https://www.google.com", strategy = "desktop", locale = ""))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_v5("https://www.google.com", strategy = "mobile", locale = NA))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_v5("https://www.google.com", strategy = "mobile", locale = 1))
})

testthat::test_that("basic output df has proper dimensions (desktop)", {
  x <- pagespeedParseR:::pagespeed_simple_v5("https://www.google.com", strategy = "desktop", interval = 0)
  testthat::expect_equal(nrow(x), 1)
  # testthat::expect_equal(ncol(x), 102)
})

testthat::test_that("basic output df has proper dimensions (mobile)", {
  x <- pagespeedParseR:::pagespeed_simple_v5("https://www.google.com", strategy = "mobile", interval = 0)
  testthat::expect_equal(nrow(x), 1)
  # testthat::expect_equal(ncol(x), 102)
})

