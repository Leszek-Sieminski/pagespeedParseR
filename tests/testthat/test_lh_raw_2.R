context("LH Raw lvl 2")
library(pagespeedParseR)
library(testthat)

test_that("url param doesn't accept bad values", {
  expect_error(pagespeedParseR:::lh_raw_2_vec(url = "", strategy = "desktop"))
  expect_error(pagespeedParseR:::lh_raw_2_vec(url = NA, strategy = "desktop"))
  expect_error(pagespeedParseR:::lh_raw_2_vec(url = NULL, strategy = "desktop"))
  expect_error(pagespeedParseR:::lh_raw_2_vec(url = "google com", strategy = "desktop"))
  expect_error(pagespeedParseR:::lh_raw_2_vec(url = "", strategy = "mobile"))
  expect_error(pagespeedParseR:::lh_raw_2_vec(url = NA, strategy = "mobile"))
  expect_error(pagespeedParseR:::lh_raw_2_vec(url = NULL, strategy = "mobile"))
  expect_error(pagespeedParseR:::lh_raw_2_vec(url = "google com", strategy = "mobile"))
})

test_that("API key doesn't accept wrong values", {
  expect_error(pagespeedParseR:::lh_raw_2_vec("https://www.w3.org/", strategy = "desktop", key = ""),
               regexp = "API key is a NULL or has length = 0.\n         Please check it and provide a proper API key.")
  expect_error(pagespeedParseR:::lh_raw_2_vec("https://www.w3.org/", strategy = "desktop", key = NULL),
               regexp = "argument is of length zero")
})

test_that("strategy param doesn't accept bad values", {
  expect_error(pagespeedParseR:::lh_raw_2_vec("https://www.w3.org/", strategy = NA))
  expect_error(pagespeedParseR:::lh_raw_2_vec("https://www.w3.org/", strategy = ""))
  expect_error(pagespeedParseR:::lh_raw_2_vec("https://www.w3.org/", strategy = "loremipsum"))
})

# categories = "performance"
test_that("categories param doesn't accept bad values", {
  expect_error(pagespeedParseR:::lh_raw_2_vec("https://www.w3.org/", categories = ""))
  expect_error(pagespeedParseR:::lh_raw_2_vec("https://www.w3.org/", categories = NA))
  expect_error(pagespeedParseR:::lh_raw_2_vec("https://www.w3.org/", categories = "loremipsum"))
  expect_error(pagespeedParseR:::lh_raw_2_vec("https://www.w3.org/", categories = 1))
})

test_that("interval param doesn't accept bad values", {
  expect_error(pagespeedParseR:::lh_raw_2_vec("https://www.w3.org/", strategy = "desktop", interval = -1))
  expect_error(pagespeedParseR:::lh_raw_2_vec("https://www.w3.org/", strategy = "mobile", interval = -1))
  expect_error(pagespeedParseR:::lh_raw_2_vec("https://www.w3.org/", strategy = "desktop", interval = 121))
  expect_error(pagespeedParseR:::lh_raw_2_vec("https://www.w3.org/", strategy = "mobile", interval = 121))
})

test_that("locale param doesn't accept bad values", {
  expect_error(pagespeedParseR:::lh_raw_2_vec("https://www.w3.org/", strategy = "desktop", locale = ""))
  expect_error(pagespeedParseR:::lh_raw_2_vec("https://www.w3.org/", strategy = "mobile", locale = NA))
  expect_error(pagespeedParseR:::lh_raw_2_vec("https://www.w3.org/", strategy = "mobile", locale = 1))
})

test_that("basic output nested list has proper length (desktop)", {
  path <- paste0(getwd(), "/db.llo")
  saveList(object = list(), file = path, append = FALSE, compress = TRUE)
  pagespeedParseR:::lh_raw_2_vec("https://www.w3.org/", strategy = "desktop", interval = 0)
  x <- readList(file = path)
  file.remove(path)
  expect_equal(length(x[[1]]), 7)
})

test_that("basic output nested list has proper length (mobile)", {
  path <- paste0(getwd(), "/db.llo")
  saveList(object = list(), file = path, append = FALSE, compress = TRUE)
  pagespeedParseR:::lh_raw_2_vec("https://www.w3.org/", strategy = "mobile", interval = 0)
  x <- readList(file = path)
  file.remove(path)
  expect_equal(length(x[[1]]), 7)
})

test_that("output nested list for multiple URLs has proper length (mobile)", {
  path <- paste0(getwd(), "/db.llo")
  saveList(object = list(), file = path, append = FALSE, compress = TRUE)
  pagespeedParseR:::lh_raw_2_vec(c("https://www.w3.org/", "https://www.bing"), strategy = "mobile", interval = 0)
  x <- readList(file = path)
  file.remove(path)
  expect_equal(length(x), 2)
  expect_equal(length(x[[1]]), 7)
})

test_that("output nested list for multiple URLs and multiple devices has proper length", {
  path <- paste0(getwd(), "/db.llo")
  saveList(object = list(), file = path, append = FALSE, compress = TRUE)
  pagespeedParseR:::lh_raw_2_vec(c("https://www.w3.org/", "https://www.bing"), strategy = c("mobile", "desktop"), interval = 0)
  x <- readList(file = path)
  file.remove(path)
  expect_equal(length(x), 4)
  expect_equal(length(x[[1]]), 7)
})
