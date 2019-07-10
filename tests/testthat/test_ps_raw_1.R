context("PSI Raw lvl 1")
library(pagespeedParseR)
library(testthat)

test_that("url param doesn't accept bad values", {
  expect_error(pagespeedParseR:::ps_raw_1(url = "", strategy = "desktop"))
  expect_error(pagespeedParseR:::ps_raw_1(url = NA, strategy = "desktop"))
  expect_error(pagespeedParseR:::ps_raw_1(url = NULL, strategy = "desktop"))
  expect_error(pagespeedParseR:::ps_raw_1(url = "google com", strategy = "desktop"))
  expect_error(pagespeedParseR:::ps_raw_1(url = "", strategy = "mobile"))
  expect_error(pagespeedParseR:::ps_raw_1(url = NA, strategy = "mobile"))
  expect_error(pagespeedParseR:::ps_raw_1(url = NULL, strategy = "mobile"))
  expect_error(pagespeedParseR:::ps_raw_1(url = "google com", strategy = "mobile"))
})

test_that("API key doesn't accept wrong values", {
  expect_error(pagespeedParseR:::ps_raw_1("https://www.w3.org/", strategy = "desktop", key = ""), regexp = "API key is a NULL or has length = 0. Please check it and provide a proper API key.")
  expect_error(pagespeedParseR:::ps_raw_1("https://www.w3.org/", strategy = "desktop", key = NULL), regexp = "argument is of length zero")
})

test_that("strategy param doesn't accept bad values", {
  expect_error(pagespeedParseR:::ps_raw_1("https://www.w3.org/", strategy = NA))
  expect_error(pagespeedParseR:::ps_raw_1("https://www.w3.org/", strategy = ""))
  expect_error(pagespeedParseR:::ps_raw_1("https://www.w3.org/", strategy = c("desktop", "mobile")))
  expect_error(pagespeedParseR:::ps_raw_1("https://www.w3.org/", strategy = "loremipsum"))
})

test_that("interval param doesn't accept bad values", {
  expect_error(pagespeedParseR:::ps_raw_1("https://www.w3.org/", strategy = "desktop", interval = -1))
  expect_error(pagespeedParseR:::ps_raw_1("https://www.w3.org/", strategy = "mobile", interval = -1))
  expect_error(pagespeedParseR:::ps_raw_1("https://www.w3.org/", strategy = "desktop", interval = 121))
  expect_error(pagespeedParseR:::ps_raw_1("https://www.w3.org/", strategy = "mobile", interval = 121))
})

test_that("filter_third_party param doesn't accept bad values", {
  expect_error(pagespeedParseR:::ps_raw_1("https://www.w3.org/", strategy = "desktop", filter_third_party = ""))
  expect_error(pagespeedParseR:::ps_raw_1("https://www.w3.org/", strategy = "mobile", filter_third_party = NA))
  expect_error(pagespeedParseR:::ps_raw_1("https://www.w3.org/", strategy = "mobile", filter_third_party = 2))
})

test_that("locale param doesn't accept bad values", {
  expect_error(pagespeedParseR:::ps_raw_1("https://www.w3.org/", strategy = "desktop", locale = ""))
  expect_error(pagespeedParseR:::ps_raw_1("https://www.w3.org/", strategy = "mobile", locale = NA))
  expect_error(pagespeedParseR:::ps_raw_1("https://www.w3.org/", strategy = "mobile", locale = 1))
})

test_that("rule param doesn't accept bad values", {
  expect_error(pagespeedParseR:::ps_raw_1("https://www.w3.org/", strategy = "desktop", rule = ""))
  expect_error(pagespeedParseR:::ps_raw_1("https://www.w3.org/", strategy = "mobile", rule = NA))
  expect_error(pagespeedParseR:::ps_raw_1("https://www.w3.org/", strategy = "mobile", rule = 1))
})

test_that("screenshot param doesn't accept bad values", {
  expect_error(pagespeedParseR:::ps_raw_1("https://www.w3.org/", strategy = "desktop", screenshot = ""))
  expect_error(pagespeedParseR:::ps_raw_1("https://www.w3.org/", strategy = "mobile", screenshot = NA))
  expect_error(pagespeedParseR:::ps_raw_1("https://www.w3.org/", strategy = "mobile", screenshot = 2))
})

test_that("snapshots param doesn't accept bad values", {
  expect_error(pagespeedParseR:::ps_raw_1("https://www.w3.org/", strategy = "desktop", snapshots = ""))
  expect_error(pagespeedParseR:::ps_raw_1("https://www.w3.org/", strategy = "mobile", snapshots = NA))
  expect_error(pagespeedParseR:::ps_raw_1("https://www.w3.org/", strategy = "mobile", snapshots = 2))
})

test_that("basic output nested list has proper length (desktop)", {
  x <- pagespeedParseR:::ps_raw_1("https://www.w3.org/", strategy = "desktop", interval = 0)
  expect_equal(length(x), 10)
})

test_that("basic output nested list has proper length (mobile)", {
  x <- pagespeedParseR:::ps_raw_1("https://www.w3.org/", strategy = "mobile", interval = 0)
  expect_equal(length(x), 10)
})
