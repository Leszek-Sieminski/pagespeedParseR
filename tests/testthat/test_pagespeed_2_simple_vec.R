context("PSI Simple lvl 2")
library(pagespeedParseR)

testthat::test_that("url param doesn't accept wrong values", {
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4(url = "", strategy = "desktop"))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4(url = NA, strategy = "desktop"))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4(url = NULL, strategy = "desktop"))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4(url = "google com", strategy = "desktop"))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4(url = "", strategy = "mobile"))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4(url = NA, strategy = "mobile"))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4(url = NULL, strategy = "mobile"))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4(url = "google com", strategy = "mobile"))
})

testthat::test_that("url param doesn't accept wrong vectors", {
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4(url = c(""), strategy = "desktop"))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4(url = c("", "https://www.google.com"), strategy = "desktop"))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4(url = c(NA, "https://www.google.com"), strategy = "mobile"))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4(url = c("https://www.google.com", "google com"), strategy = "mobile"))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4(url = c("https://www.google.com", ""), strategy = "desktop"))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4(url = c("https://www.google.com", NA), strategy = "mobile"))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4(url = c("https://www.google.com", "google com"), strategy = "mobile"))
})

testthat::test_that("API key doesn't accept wrong values", {
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4("https://www.google.com", strategy = "desktop", key = ""), regexp = "API key is a NULL or has length = 0. Please check it and provide a proper API key.")
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4("https://www.google.com", strategy = "desktop", key = NULL), regexp = "argument is of length zero")
})

testthat::test_that("strategy param doesn't accept wrong values", {
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4("https://www.google.com", strategy = NA))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4("https://www.google.com", strategy = ""))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4("https://www.google.com", strategy = "loremipsum"))
})

testthat::test_that("interval param doesn't accept wrong values", {
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4("https://www.google.com", strategy = "desktop", interval = -1))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4("https://www.google.com", strategy = "mobile", interval = -1))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4("https://www.google.com", strategy = "desktop", interval = 121))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4("https://www.google.com", strategy = "mobile", interval = 121))
})

testthat::test_that("filter_third_party param doesn't accept wrong values", {
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4("https://www.google.com", strategy = "desktop", filter_third_party = ""))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4("https://www.google.com", strategy = "mobile", filter_third_party = NA))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4("https://www.google.com", strategy = "mobile", filter_third_party = 2))
})

testthat::test_that("locale param doesn't accept wrong values", {
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4("https://www.google.com", strategy = "desktop", locale = ""))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4("https://www.google.com", strategy = "mobile", locale = NA))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4("https://www.google.com", strategy = "mobile", locale = 1))
})

testthat::test_that("rule param doesn't accept wrong values", {
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4("https://www.google.com", strategy = "desktop", rule = ""))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4("https://www.google.com", strategy = "mobile", rule = NA))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4("https://www.google.com", strategy = "mobile", rule = 1))
})


testthat::test_that("screenshot param doesn't accept wrong values", {
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4("https://www.google.com", strategy = "desktop", screenshot = ""))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4("https://www.google.com", strategy = "mobile", screenshot = NA))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4("https://www.google.com", strategy = "mobile", screenshot = 2))
})

testthat::test_that("snapshots param doesn't accept wrong values", {
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4("https://www.google.com", strategy = "desktop", snapshots = ""))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4("https://www.google.com", strategy = "mobile", snapshots = NA))
  testthat::expect_error(pagespeedParseR:::pagespeed_simple_list_v4("https://www.google.com", strategy = "mobile", snapshots = 2))
})

testthat::test_that("basic output df has proper dimensions (desktop)", {
  x <- pagespeedParseR:::pagespeed_simple_list_v4("https://www.google.com", strategy = "desktop", interval = 0)
  testthat::expect_equal(nrow(x), 1)
  testthat::expect_equal(ncol(x), 54)
})

testthat::test_that("basic output df has proper dimensions (mobile)", {
  x <- pagespeedParseR:::pagespeed_simple_list_v4("https://www.google.com", strategy = "mobile", interval = 0)
  testthat::expect_equal(nrow(x), 1)
  testthat::expect_equal(ncol(x), 54)
})

testthat::test_that("basic output df has proper dimensions (both devices)", {
  x <- pagespeedParseR:::pagespeed_simple_list_v4("https://www.google.com", strategy = c("mobile", "desktop"), interval = 0)
  testthat::expect_equal(nrow(x), 2)
  testthat::expect_equal(ncol(x), 54)
})

testthat::test_that("output df returns proper dimensions despite the NULL", {
  x <- pagespeedParseR:::pagespeed_simple_list_v4(url = c(NULL, "https://www.google.com"), strategy = "desktop", interval = 0)
  testthat::expect_equal(nrow(x), 1)
  testthat::expect_equal(ncol(x), 54)
  x <- pagespeedParseR:::pagespeed_simple_list_v4(url = c("https://www.google.com", NULL), strategy = "mobile", interval = 0)
  testthat::expect_equal(nrow(x), 1)
  testthat::expect_equal(ncol(x), 54)
})

testthat::test_that("output df returns rows with errors", {
  x <- pagespeedParseR:::pagespeed_simple_list_v4(url = c("loremipsumdolorametpageparsererrortest.com", "https://www.google.com"), strategy = "desktop", interval = 0)
  testthat::expect_equal(nrow(x), 2)
  testthat::expect_equal(ncol(x), 54)
})
