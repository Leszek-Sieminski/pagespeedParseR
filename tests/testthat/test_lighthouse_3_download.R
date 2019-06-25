context("LH Download lvl 3")
library(pagespeedParseR)

# simple ----------------------------------------------------------------------
testthat::test_that("url param doesn't accept wrong values", {
  testthat::expect_error(pagespeedParseR:::download_lighthouse(url = "", strategy = "desktop", output_type = "simple"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse(url = NA, strategy = "desktop", output_type = "simple"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse(url = NULL, strategy = "desktop", output_type = "simple"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse(url = "google com", strategy = "desktop", output_type = "simple"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse(url = "", strategy = "mobile", output_type = "simple"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse(url = NA, strategy = "mobile", output_type = "simple"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse(url = NULL, strategy = "mobile", output_type = "simple"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse(url = "google com", strategy = "mobile", output_type = "simple"))
})

testthat::test_that("url param doesn't accept wrong vectors", {
  testthat::expect_error(pagespeedParseR:::download_lighthouse(url = c(""), strategy = "desktop", output_type = "simple"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse(url = c("", "https://www.google.com"), strategy = "desktop", output_type = "simple"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse(url = c(NA, "https://www.google.com"), strategy = "mobile", output_type = "simple"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse(url = c("https://www.google.com", "google com"), strategy = "mobile", output_type = "simple"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse(url = c("https://www.google.com", ""), strategy = "desktop", output_type = "simple"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse(url = c("https://www.google.com", NA), strategy = "mobile", output_type = "simple"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse(url = c("https://www.google.com", "google com"), strategy = "mobile", output_type = "simple"))
})

testthat::test_that("API key doesn't accept wrong values", {
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "desktop", key = "", output_type = "simple"), regexp = "API key is a NULL or has length = 0. Please check it and provide a proper API key.")
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "desktop", key = NULL, output_type = "simple"), regexp = "argument is of length zero")
})

testthat::test_that("strategy param doesn't accept wrong values", {
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = NA, output_type = "simple"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "", output_type = "simple"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "loremipsum", output_type = "simple"))
})

testthat::test_that("interval param doesn't accept wrong values", {
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "desktop", interval = -1, output_type = "simple"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "mobile", interval = -1, output_type = "simple"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "desktop", interval = 121, output_type = "simple"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "mobile", interval = 121, output_type = "simple"))
})

testthat::test_that("filter_third_party param doesn't accept wrong values", {
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "desktop", filter_third_party = "", output_type = "simple"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "mobile", filter_third_party = NA, output_type = "simple"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "mobile", filter_third_party = 2, output_type = "simple"))
})

testthat::test_that("locale param doesn't accept wrong values", {
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "desktop", locale = "", output_type = "simple"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "mobile", locale = NA, output_type = "simple"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "mobile", locale = 1, output_type = "simple"))
})

testthat::test_that("rule param doesn't accept wrong values", {
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "desktop", rule = "", output_type = "simple"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "mobile", rule = NA, output_type = "simple"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "mobile", rule = 1, output_type = "simple"))
})


testthat::test_that("screenshot param doesn't accept wrong values", {
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "desktop", screenshot = "", output_type = "simple"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "mobile", screenshot = NA, output_type = "simple"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "mobile", screenshot = 2, output_type = "simple"))
})

testthat::test_that("snapshots param doesn't accept wrong values", {
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "desktop", snapshots = "", output_type = "simple"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "mobile", snapshots = NA, output_type = "simple"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "mobile", snapshots = 2, output_type = "simple"))
})

testthat::test_that("basic output df has proper dimensions (desktop)", {
  x <- pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "desktop", interval = 0, output_type = "simple")
  testthat::expect_equal(nrow(x), 1)
})

testthat::test_that("basic output df has proper dimensions (mobile)", {
  x <- pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "mobile", interval = 0, output_type = "simple")
  testthat::expect_equal(nrow(x), 1)
})

testthat::test_that("basic output df has proper dimensions (both devices)", {
  x <- pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = c("mobile", "desktop"), interval = 0, output_type = "simple")
  testthat::expect_equal(nrow(x), 2)
})

testthat::test_that("output df returns proper dimensions despite the NULL", {
  x <- pagespeedParseR:::download_lighthouse(url = c(NULL, "https://www.google.com"), strategy = "desktop", interval = 0, output_type = "simple")
  testthat::expect_equal(nrow(x), 1)
  x <- pagespeedParseR:::download_lighthouse(url = c("https://www.google.com", NULL), strategy = "mobile", interval = 0, output_type = "simple")
  testthat::expect_equal(nrow(x), 1)
})

testthat::test_that("output df returns rows with errors", {
  y <- pagespeedParseR:::download_lighthouse(url = c("loremipsumdolorametpageparsererrortest.com", "https://www.google.com"), strategy = "desktop", interval = 0, output_type = "simple")
  testthat::expect_equal(nrow(y), 2)
})

# testthat::test_that("advanced output has all columns (mobile)", {
#   x <- pagespeedParseR:::download_lighthouse(c("https://www.google.com", "https://www.google.co.uk"), strategy = "mobile", output_type = "simple", interval = 0, categories = c("performance", "accessibility", "best-practices", "seo", "pwa"))
#   placeholder_cols <- pagespeedParseR:::v5_placeholder_basic(categories = c("performance", "accessibility", "best-practices", "seo", "pwa"))
#   testthat::expect_equal(nrow(x), 2)
#   testthat::expect_true(all(colnames(x) %in% colnames(placeholder_cols)))
#   testthat::expect_true(all(colnames(placeholder_cols) %in% colnames(x)))
# })

# raw -------------------------------------------------------------------------
testthat::test_that("url param doesn't accept bad values", {
  testthat::expect_error(pagespeedParseR:::download_lighthouse(url = "", strategy = "desktop", output_type = "raw"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse(url = NA, strategy = "desktop", output_type = "raw"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse(url = NULL, strategy = "desktop", output_type = "raw"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse(url = "google com", strategy = "desktop", output_type = "raw"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse(url = "", strategy = "mobile", output_type = "raw"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse(url = NA, strategy = "mobile", output_type = "raw"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse(url = NULL, strategy = "mobile", output_type = "raw"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse(url = "google com", strategy = "mobile", output_type = "raw"))
})

testthat::test_that("API key doesn't accept wrong values", {
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "desktop", key = "", output_type = "raw"), regexp = "API key is a NULL or has length = 0. Please check it and provide a proper API key.")
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "desktop", key = NULL, output_type = "raw"), regexp = "argument is of length zero")
})

testthat::test_that("strategy param doesn't accept bad values", {
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = NA, output_type = "raw"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "", output_type = "raw"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "loremipsum", output_type = "raw"))
})

# categories = "performance"
testthat::test_that("categories param doesn't accept bad values", {
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", categories = "", output_type = "raw"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", categories = NA, output_type = "raw"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", categories = "loremipsum", output_type = "raw"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", categories = 1, output_type = "raw"))
})

testthat::test_that("interval param doesn't accept bad values", {
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "desktop", interval = -1, output_type = "raw"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "mobile", interval = -1, output_type = "raw"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "desktop", interval = 121, output_type = "raw"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "mobile", interval = 121, output_type = "raw"))
})

testthat::test_that("locale param doesn't accept bad values", {
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "desktop", locale = "", output_type = "raw"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "mobile", locale = NA, output_type = "raw"))
  testthat::expect_error(pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "mobile", locale = 1, output_type = "raw"))
})

# TODO check what's wrong with those tests (works on server, fails on travis & appveyor)
testthat::test_that("basic output nested list has proper length (desktop)", {
  # Sys.sleep(4)
  x <- pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "desktop", interval = 0, output_type = "raw")
  testthat::expect_equal(length(x[[1]]), 7)
})

testthat::test_that("basic output nested list has proper length (mobile)", {
  Sys.sleep(4)
  x <- pagespeedParseR:::download_lighthouse("https://www.google.com", strategy = "mobile", interval = 0, output_type = "raw")
testthat::expect_true(exists("x"))
  testthat::expect_equal(length(x[[1]]), 7)
})
