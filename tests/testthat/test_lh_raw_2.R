context("LH Raw lvl 2")
library(pagespeedParseR)

testthat::test_that("url param doesn't accept bad values", {
  testthat::expect_error(pagespeedParseR:::lh_raw_2_vec(url = "", strategy = "desktop"))
  testthat::expect_error(pagespeedParseR:::lh_raw_2_vec(url = NA, strategy = "desktop"))
  testthat::expect_error(pagespeedParseR:::lh_raw_2_vec(url = NULL, strategy = "desktop"))
  testthat::expect_error(pagespeedParseR:::lh_raw_2_vec(url = "google com", strategy = "desktop"))
  testthat::expect_error(pagespeedParseR:::lh_raw_2_vec(url = "", strategy = "mobile"))
  testthat::expect_error(pagespeedParseR:::lh_raw_2_vec(url = NA, strategy = "mobile"))
  testthat::expect_error(pagespeedParseR:::lh_raw_2_vec(url = NULL, strategy = "mobile"))
  testthat::expect_error(pagespeedParseR:::lh_raw_2_vec(url = "google com", strategy = "mobile"))
})

testthat::test_that("API key doesn't accept wrong values", {
  testthat::expect_error(pagespeedParseR:::lh_raw_2_vec("https://www.google.com", strategy = "desktop", key = ""), regexp = "API key is a NULL or has length = 0. Please check it and provide a proper API key.")
  testthat::expect_error(pagespeedParseR:::lh_raw_2_vec("https://www.google.com", strategy = "desktop", key = NULL), regexp = "argument is of length zero")
})

testthat::test_that("strategy param doesn't accept bad values", {
  testthat::expect_error(pagespeedParseR:::lh_raw_2_vec("https://www.google.com", strategy = NA))
  testthat::expect_error(pagespeedParseR:::lh_raw_2_vec("https://www.google.com", strategy = ""))
  testthat::expect_error(pagespeedParseR:::lh_raw_2_vec("https://www.google.com", strategy = "loremipsum"))
})

# categories = "performance"
testthat::test_that("categories param doesn't accept bad values", {
  testthat::expect_error(pagespeedParseR:::lh_raw_2_vec("https://www.google.com", categories = ""))
  testthat::expect_error(pagespeedParseR:::lh_raw_2_vec("https://www.google.com", categories = NA))
  testthat::expect_error(pagespeedParseR:::lh_raw_2_vec("https://www.google.com", categories = "loremipsum"))
  testthat::expect_error(pagespeedParseR:::lh_raw_2_vec("https://www.google.com", categories = 1))
})

testthat::test_that("interval param doesn't accept bad values", {
  testthat::expect_error(pagespeedParseR:::lh_raw_2_vec("https://www.google.com", strategy = "desktop", interval = -1))
  testthat::expect_error(pagespeedParseR:::lh_raw_2_vec("https://www.google.com", strategy = "mobile", interval = -1))
  testthat::expect_error(pagespeedParseR:::lh_raw_2_vec("https://www.google.com", strategy = "desktop", interval = 121))
  testthat::expect_error(pagespeedParseR:::lh_raw_2_vec("https://www.google.com", strategy = "mobile", interval = 121))
})

testthat::test_that("locale param doesn't accept bad values", {
  testthat::expect_error(pagespeedParseR:::lh_raw_2_vec("https://www.google.com", strategy = "desktop", locale = ""))
  testthat::expect_error(pagespeedParseR:::lh_raw_2_vec("https://www.google.com", strategy = "mobile", locale = NA))
  testthat::expect_error(pagespeedParseR:::lh_raw_2_vec("https://www.google.com", strategy = "mobile", locale = 1))
})

testthat::test_that("basic output nested list has proper length (desktop)", {
  x <- pagespeedParseR:::lh_raw_2_vec("https://www.google.com", strategy = "desktop", interval = 0)
  testthat::expect_equal(length(x[[1]]), 7)
})

testthat::test_that("basic output nested list has proper length (mobile)", {
  x <- pagespeedParseR:::lh_raw_2_vec("https://www.google.com", strategy = "mobile", interval = 0)
  testthat::expect_equal(length(x[[1]]), 7)
})

testthat::test_that("output nested list for multiple URLs has proper length (mobile)", {
  x <- pagespeedParseR:::lh_raw_2_vec(c("https://www.google.com", "https://www.bing"), strategy = "mobile", interval = 0)
  testthat::expect_equal(length(x), 2)
  testthat::expect_equal(length(x[[1]]), 7)
})

testthat::test_that("output nested list for multiple URLs and multiple devices has proper length", {
  x <- pagespeedParseR:::lh_raw_2_vec(c("https://www.google.com", "https://www.bing"), strategy = c("mobile", "desktop"), interval = 0)
  testthat::expect_equal(length(x), 2)
  testthat::expect_equal(length(x[[1]]), 2)
  testthat::expect_equal(length(x[[1]][[1]]), 7)
  testthat::expect_true(all(names(x) %in% c("desktop", "mobile")))
})
