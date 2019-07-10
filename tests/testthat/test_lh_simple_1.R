# setup -----------------------------------------------------------------------
context("LH Simple lvl 1")
library(pagespeedParseR)
library(testthat)

# defensive -------------------------------------------------------------------
test_that("url param doesn't accept bad values", {
  expect_error(pagespeedParseR:::lh_simple_1(url = "", strategy = "desktop"))
  expect_error(pagespeedParseR:::lh_simple_1(url = NA, strategy = "desktop"))
  expect_error(pagespeedParseR:::lh_simple_1(url = NULL, strategy = "desktop"))
  expect_error(pagespeedParseR:::lh_simple_1(url = "google com", strategy = "desktop"))
  expect_error(pagespeedParseR:::lh_simple_1(url = "", strategy = "mobile"))
  expect_error(pagespeedParseR:::lh_simple_1(url = NA, strategy = "mobile"))
  expect_error(pagespeedParseR:::lh_simple_1(url = NULL, strategy = "mobile"))
  expect_error(pagespeedParseR:::lh_simple_1(url = "google com", strategy = "mobile"))
})

test_that("API key doesn't accept wrong values", {
  expect_error(
    pagespeedParseR:::lh_simple_1("https://www.w3.org/", strategy = "desktop", key = ""),
    regexp = "API key is a NULL or has length = 0. Please check it and provide a proper API key.")

  expect_error(
    pagespeedParseR:::lh_simple_1("https://www.w3.org/", strategy = "desktop", key = NULL),
    regexp = "argument is of length zero")
})

test_that("strategy param doesn't accept bad values", {
  expect_error(pagespeedParseR:::lh_simple_1("https://www.w3.org/", strategy = NA))
  expect_error(pagespeedParseR:::lh_simple_1("https://www.w3.org/", strategy = ""))
  expect_error(pagespeedParseR:::lh_simple_1("https://www.w3.org/", strategy = c("desktop", "mobile")))
  expect_error(pagespeedParseR:::lh_simple_1("https://www.w3.org/", strategy = "loremipsum"))
})

test_that("categories param doesn't accept bad values", {
  expect_error(pagespeedParseR:::lh_simple_1("https://www.w3.org/", categories = ""))
  expect_error(pagespeedParseR:::lh_simple_1("https://www.w3.org/", categories = NA))
  expect_error(pagespeedParseR:::lh_simple_1("https://www.w3.org/", categories = "loremipsum"))
  expect_error(pagespeedParseR:::lh_simple_1("https://www.w3.org/", categories = 1))
})

test_that("interval param doesn't accept bad values", {
  expect_error(pagespeedParseR:::lh_simple_1("https://www.w3.org/", strategy = "desktop", interval = -1))
  expect_error(pagespeedParseR:::lh_simple_1("https://www.w3.org/", strategy = "mobile", interval = -1))
  expect_error(pagespeedParseR:::lh_simple_1("https://www.w3.org/", strategy = "desktop", interval = 121))
  expect_error(pagespeedParseR:::lh_simple_1("https://www.w3.org/", strategy = "mobile", interval = 121))
})

test_that("locale param doesn't accept bad values", {
  expect_error(pagespeedParseR:::lh_simple_1("https://www.w3.org/", strategy = "desktop", locale = ""))
  expect_error(pagespeedParseR:::lh_simple_1("https://www.w3.org/", strategy = "mobile", locale = NA))
  expect_error(pagespeedParseR:::lh_simple_1("https://www.w3.org/", strategy = "mobile", locale = 1))
})

test_that("basic output df has proper dimensions (desktop)", {
  x <- pagespeedParseR:::lh_simple_1("https://www.w3.org/", strategy = "desktop", interval = 0)
  expect_equal(nrow(x), 1)
  expect_equal(length(dim(x)), 2)
})


# output verification ---------------------------------------------------------
test_that("basic output df has proper dimensions (mobile)", {
  x <- pagespeedParseR:::lh_simple_1("https://www.w3.org/", strategy = "mobile", interval = 0)
  expect_equal(nrow(x), 1)
  expect_equal(length(dim(x)), 2)
})

test_that("advanced output df contains reports (mobile)", {
  x <- pagespeedParseR:::lh_simple_1("https://www.w3.org/", strategy = "mobile", interval = 0, categories = c("performance", "accessibility", "best-practices", "seo", "pwa"))
  expect_equal(nrow(x), 1)
  expect_equal(length(dim(x)), 2)
})

# test_that("advanced output df contains proper columns (mobile)", {
#   x <- pagespeedParseR:::lh_simple_1("https://www.w3.org/", strategy = "mobile", interval = 0, categories = c("performance", "accessibility", "best-practices", "seo", "pwa"))
#   y <- pagespeedParseR:::lh_simple_1("https://loremipsumpackagetestingsiteexample.com", strategy = "mobile", interval = 0, categories = c("performance", "accessibility", "best-practices", "seo", "pwa"))
#   col_set <- colnames(pagespeedParseR:::v5_placeholder_basic(categories = c("performance", "accessibility", "best-practices", "seo", "pwa")))
#   expect_true(all(col_set %in% colnames(x)))
#   expect_true(all(col_set %in% colnames(y)))
# })
