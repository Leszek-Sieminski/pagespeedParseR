context("LH Download lvl 3")
library(pagespeedParseR)
library(testthat)

# raw -------------------------------------------------------------------------
test_that("url param doesn't accept bad values", {
  expect_error(download_lighthouse(url = "", strategy = "desktop"))
  expect_error(download_lighthouse(url = NA, strategy = "desktop"))
  expect_error(download_lighthouse(url = NULL, strategy = "desktop"))
  expect_error(download_lighthouse(url = "google com", strategy = "desktop"))
  expect_error(download_lighthouse(url = "", strategy = "mobile"))
  expect_error(download_lighthouse(url = NA, strategy = "mobile"))
  expect_error(download_lighthouse(url = NULL, strategy = "mobile"))
  expect_error(download_lighthouse(url = "google com", strategy = "mobile"))
})

test_that("API key doesn't accept wrong values", {
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "desktop", key = ""),
               regexp = "API key is a NULL or has 0 characters.\n         Please check it and provide a proper API key.")
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "desktop", key = NULL),
               regexp = "argument is of length zero")
})

test_that("strategy param doesn't accept bad values", {
  expect_error(download_lighthouse("https://www.w3.org/", strategy = NA))
  expect_error(download_lighthouse("https://www.w3.org/", strategy = ""))
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "loremipsum"))
})

# categories = "performance"
test_that("categories param doesn't accept bad values", {
  expect_error(download_lighthouse("https://www.w3.org/", categories = ""))
  expect_error(download_lighthouse("https://www.w3.org/", categories = NA))
  expect_error(download_lighthouse("https://www.w3.org/", categories = "loremipsum"))
  expect_error(download_lighthouse("https://www.w3.org/", categories = 1))
})

test_that("as_reference param doesn't accept bad values", {
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "desktop", as_reference = NA))
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "mobile", as_reference = NULL))
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "desktop", as_reference = ""))
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "mobile", as_reference = "testtest"))
})

test_that("reference_path param doesn't accept bad values", {
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "desktop", reference_path = NA))
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "desktop", reference_path = ""))
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "mobile", reference_path = "testtest"))
})

test_that("interval param doesn't accept bad values", {
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "desktop", interval = -1))
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "mobile", interval = -1))
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "desktop", interval = 121))
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "mobile", interval = 121))
})

test_that("locale param doesn't accept bad values", {
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "desktop", locale = ""))
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "mobile", locale = NA))
  expect_error(download_lighthouse("https://www.w3.org/", strategy = "mobile", locale = 1))
})

# TODO check what's wrong with those tests (works on server, fails on travis & appveyor)
test_that("basic output nested list contains the report", {
  x <- download_lighthouse("https://www.w3.org/", strategy = "desktop", interval = 0)
  expect_identical(x[[1]]$id, "https://www.w3.org/")
})

# # test_that("basic output nested list has proper length (mobile)", {
# #   Sys.sleep(4)
# #   x <- download_lighthouse("https://www.w3.org/", strategy = "mobile", interval = 0)
# # expect_true(exists("x"))
# #   expect_equal(length(x[[1]]), 7)
# # })
