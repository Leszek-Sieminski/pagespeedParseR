context("LH Download lvl 3")
library(pagespeedParseR)
library(testthat)

# simple ----------------------------------------------------------------------
test_that("url param doesn't accept wrong values", {
  expect_error(download_lighthouse(url = "", strategy = "desktop", output_type = "simple"))
  expect_error(download_lighthouse(url = NA, strategy = "desktop", output_type = "simple"))
  expect_error(download_lighthouse(url = NULL, strategy = "desktop", output_type = "simple"))
  expect_error(download_lighthouse(url = "google com", strategy = "desktop", output_type = "simple"))
  expect_error(download_lighthouse(url = "", strategy = "mobile", output_type = "simple"))
  expect_error(download_lighthouse(url = NA, strategy = "mobile", output_type = "simple"))
  expect_error(download_lighthouse(url = NULL, strategy = "mobile", output_type = "simple"))
  expect_error(download_lighthouse(url = "google com", strategy = "mobile", output_type = "simple"))
})

test_that("url param doesn't accept wrong vectors", {
  expect_error(download_lighthouse(url = c(""), strategy = "desktop", output_type = "simple"))
  expect_error(download_lighthouse(url = c("", "https://www.w3.org/"), strategy = "desktop", output_type = "simple"))
  expect_error(download_lighthouse(url = c(NA, "https://www.w3.org/"), strategy = "mobile", output_type = "simple"))
  expect_error(download_lighthouse(url = c("https://www.w3.org/", "google com"), strategy = "mobile", output_type = "simple"))
  expect_error(download_lighthouse(url = c("https://www.w3.org/", ""), strategy = "desktop", output_type = "simple"))
  expect_error(download_lighthouse(url = c("https://www.w3.org/", NA), strategy = "mobile", output_type = "simple"))
  expect_error(download_lighthouse(url = c("https://www.w3.org/", "google com"), strategy = "mobile", output_type = "simple"))
})

test_that("API key doesn't accept wrong values", {
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "desktop", key = "", output_type = "simple"), regexp = "API key is a NULL or has length = 0. Please check it and provide a proper API key.")
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "desktop", key = NULL, output_type = "simple"), regexp = "argument is of length zero")
})

test_that("strategy param doesn't accept wrong values", {
  expect_error(download_lighthouse("https://www.w3.org/", strategy = NA, output_type = "simple"))
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "", output_type = "simple"))
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "loremipsum", output_type = "simple"))
})

test_that("interval param doesn't accept wrong values", {
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "desktop", interval = -1, output_type = "simple"))
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "mobile", interval = -1, output_type = "simple"))
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "desktop", interval = 121, output_type = "simple"))
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "mobile", interval = 121, output_type = "simple"))
})

test_that("filter_third_party param doesn't accept wrong values", {
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "desktop", filter_third_party = "", output_type = "simple"))
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "mobile", filter_third_party = NA, output_type = "simple"))
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "mobile", filter_third_party = 2, output_type = "simple"))
})

test_that("locale param doesn't accept wrong values", {
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "desktop", locale = "", output_type = "simple"))
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "mobile", locale = NA, output_type = "simple"))
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "mobile", locale = 1, output_type = "simple"))
})

test_that("rule param doesn't accept wrong values", {
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "desktop", rule = "", output_type = "simple"))
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "mobile", rule = NA, output_type = "simple"))
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "mobile", rule = 1, output_type = "simple"))
})


test_that("screenshot param doesn't accept wrong values", {
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "desktop", screenshot = "", output_type = "simple"))
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "mobile", screenshot = NA, output_type = "simple"))
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "mobile", screenshot = 2, output_type = "simple"))
})

test_that("snapshots param doesn't accept wrong values", {
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "desktop", snapshots = "", output_type = "simple"))
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "mobile", snapshots = NA, output_type = "simple"))
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "mobile", snapshots = 2, output_type = "simple"))
})

test_that("basic output df has proper dimensions (desktop)", {
  x <- download_lighthouse("https://www.w3.org/", strategy = "desktop", interval = 0, output_type = "simple")
  y <- download_lighthouse("https://www.w3.org/", strategy = "desktop", interval = 0, output_type = "simple", long_result = F)
  expect_equal(ncol(x), 2)
  expect_equal(nrow(y), 1)
})

test_that("basic output df has proper dimensions (mobile)", {
  x <- download_lighthouse("https://www.w3.org/", strategy = "mobile", interval = 0, output_type = "simple")
  y <- download_lighthouse("https://www.w3.org/", strategy = "mobile", interval = 0, output_type = "simple", long_result = F)
  expect_equal(ncol(x), 2)
  expect_equal(nrow(y), 1)
})

test_that("basic output df has proper dimensions (both devices)", {
  x <- download_lighthouse("https://www.w3.org/", strategy = c("mobile", "desktop"), interval = 0, output_type = "simple")
  y <- download_lighthouse("https://www.w3.org/", strategy = c("mobile", "desktop"), interval = 0, output_type = "simple", long_result = F)
  expect_equal(ncol(x), 3)
  expect_equal(nrow(y), 2)
})

test_that("output df returns proper dimensions despite the NULL", {
  x <- download_lighthouse(url = c(NULL, "https://www.w3.org/"), strategy = "desktop", interval = 0, output_type = "simple")
  y <- download_lighthouse(url = c("https://www.w3.org/", NULL), strategy = "mobile", interval = 0, output_type = "simple")
  x_len <- download_lighthouse(url = c(NULL, "https://www.w3.org/"), strategy = "desktop", interval = 0, output_type = "simple", long_result = F)
  y_len <- download_lighthouse(url = c("https://www.w3.org/", NULL), strategy = "mobile", interval = 0, output_type = "simple", long_result = F)
  expect_equal(ncol(x), 2)
  expect_equal(ncol(y), 2)
  expect_equal(nrow(x_len), 1)
  expect_equal(nrow(y_len), 1)
})

test_that("output df returns rows with errors", {
  x <- download_lighthouse(url = c("loremipsumdolorametpageparsererrortest.com", "https://www.w3.org/"), strategy = "desktop", interval = 0, output_type = "simple")
  expect_equal(ncol(x), 3)
})

# test_that("advanced output has all columns (mobile)", {
#   x <- download_lighthouse(c("https://www.w3.org/", "https://www.google.co.uk"), strategy = "mobile", output_type = "simple", interval = 0, categories = c("performance", "accessibility", "best-practices", "seo", "pwa"))
#   placeholder_cols <- v5_placeholder_basic(categories = c("performance", "accessibility", "best-practices", "seo", "pwa"))
#   expect_equal(nrow(x), 2)
#   expect_true(all(colnames(x) %in% colnames(placeholder_cols)))
#   expect_true(all(colnames(placeholder_cols) %in% colnames(x)))
# })

# raw -------------------------------------------------------------------------
test_that("url param doesn't accept bad values", {
  expect_error(download_lighthouse(url = "", strategy = "desktop", output_type = "raw"))
  expect_error(download_lighthouse(url = NA, strategy = "desktop", output_type = "raw"))
  expect_error(download_lighthouse(url = NULL, strategy = "desktop", output_type = "raw"))
  expect_error(download_lighthouse(url = "google com", strategy = "desktop", output_type = "raw"))
  expect_error(download_lighthouse(url = "", strategy = "mobile", output_type = "raw"))
  expect_error(download_lighthouse(url = NA, strategy = "mobile", output_type = "raw"))
  expect_error(download_lighthouse(url = NULL, strategy = "mobile", output_type = "raw"))
  expect_error(download_lighthouse(url = "google com", strategy = "mobile", output_type = "raw"))
})

test_that("API key doesn't accept wrong values", {
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "desktop", key = "", output_type = "raw"), regexp = "API key is a NULL or has length = 0. Please check it and provide a proper API key.")
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "desktop", key = NULL, output_type = "raw"), regexp = "argument is of length zero")
})

test_that("strategy param doesn't accept bad values", {
  expect_error(download_lighthouse("https://www.w3.org/", strategy = NA, output_type = "raw"))
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "", output_type = "raw"))
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "loremipsum", output_type = "raw"))
})

# categories = "performance"
test_that("categories param doesn't accept bad values", {
  expect_error(download_lighthouse("https://www.w3.org/", categories = "", output_type = "raw"))
  expect_error(download_lighthouse("https://www.w3.org/", categories = NA, output_type = "raw"))
  expect_error(download_lighthouse("https://www.w3.org/", categories = "loremipsum", output_type = "raw"))
  expect_error(download_lighthouse("https://www.w3.org/", categories = 1, output_type = "raw"))
})

test_that("interval param doesn't accept bad values", {
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "desktop", interval = -1, output_type = "raw"))
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "mobile", interval = -1, output_type = "raw"))
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "desktop", interval = 121, output_type = "raw"))
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "mobile", interval = 121, output_type = "raw"))
})

test_that("locale param doesn't accept bad values", {
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "desktop", locale = "", output_type = "raw"))
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "mobile", locale = NA, output_type = "raw"))
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "mobile", locale = 1, output_type = "raw"))
})

# TODO check what's wrong with those tests (works on server, fails on travis & appveyor)
test_that("basic output nested list contains the report", {
  x <- download_lighthouse("https://www.w3.org/", strategy = "desktop", interval = 0, output_type = "raw")
  expect_identical(x$desktop[[1]]$id, "https://www.w3.org/")
})

# # test_that("basic output nested list has proper length (mobile)", {
# #   Sys.sleep(4)
# #   x <- download_lighthouse("https://www.w3.org/", strategy = "mobile", interval = 0, output_type = "raw")
# # expect_true(exists("x"))
# #   expect_equal(length(x[[1]]), 7)
# # })
