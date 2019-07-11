context("PSI Download lvl 3")
library(pagespeedParseR)
library(testthat)

test_that("url param doesn't accept wrong values (simple)", {
  expect_error(download_pagespeed(url = "", strategy = "desktop"))
  expect_error(download_pagespeed(url = NA, strategy = "desktop"))
  expect_error(download_pagespeed(url = NULL, strategy = "desktop"))
  expect_error(download_pagespeed(url = "google com", strategy = "desktop"))
  expect_error(download_pagespeed(url = "", strategy = "mobile"))
  expect_error(download_pagespeed(url = NA, strategy = "mobile"))
  expect_error(download_pagespeed(url = NULL, strategy = "mobile"))
  expect_error(download_pagespeed(url = "google com", strategy = "mobile"))
})

test_that("url param doesn't accept wrong vectors (simple)", {
  expect_error(download_pagespeed(url = c(""), strategy = "desktop"))
  expect_error(download_pagespeed(url = c("", "https://www.w3.org/"), strategy = "desktop"))
  expect_error(download_pagespeed(url = c(NA, "https://www.w3.org/"), strategy = "mobile"))
  expect_error(download_pagespeed(url = c("https://www.w3.org/", "google com"), strategy = "mobile"))
  expect_error(download_pagespeed(url = c("https://www.w3.org/", ""), strategy = "desktop"))
  expect_error(download_pagespeed(url = c("https://www.w3.org/", NA), strategy = "mobile"))
  expect_error(download_pagespeed(url = c("https://www.w3.org/", "google com"), strategy = "mobile"))
})

test_that("API key doesn't accept wrong values (simple)", {
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "desktop", key = ""), regexp = "API key is a NULL or has length = 0. Please check it and provide a proper API key.")
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "desktop", key = NULL), regexp = "argument is of length zero")
})

test_that("strategy param doesn't accept wrong values (simple)", {
  expect_error(download_pagespeed("https://www.w3.org/", strategy = NA))
  expect_error(download_pagespeed("https://www.w3.org/", strategy = ""))
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "loremipsum"))
})

test_that("interval param doesn't accept wrong values (simple)", {
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "desktop", interval = -1))
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "mobile", interval = -1))
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "desktop", interval = 121))
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "mobile", interval = 121))
})

test_that("filter_third_party param doesn't accept wrong values (simple)", {
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "desktop", filter_third_party = ""))
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "mobile", filter_third_party = NA))
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "mobile", filter_third_party = 2))
})

test_that("locale param doesn't accept wrong values (simple)", {
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "desktop", locale = ""))
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "mobile", locale = NA))
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "mobile", locale = 1))
})

test_that("rule param doesn't accept wrong values (simple)", {
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "desktop", rule = ""))
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "mobile", rule = NA))
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "mobile", rule = 1))
})


test_that("screenshot param doesn't accept wrong values (simple)", {
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "desktop", screenshot = ""))
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "mobile", screenshot = NA))
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "mobile", screenshot = 2))
})

test_that("snapshots param doesn't accept wrong values (simple)", {
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "desktop", snapshots = ""))
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "mobile", snapshots = NA))
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "mobile", snapshots = 2))
})

test_that("basic output df has proper dimensions (desktop, simple)", {
  x <- download_pagespeed("https://www.w3.org/", strategy = "desktop", interval = 0)
  expect_equal(nrow(x), 1)
  expect_equal(ncol(x), 54)
})

test_that("basic output df has proper dimensions (mobile, simple)", {
  x <- download_pagespeed("https://www.w3.org/", strategy = "mobile", interval = 0)
  expect_equal(nrow(x), 1)
  expect_equal(ncol(x), 54)
})

test_that("basic output df has proper dimensions (both, simple)", {
  x <- download_pagespeed("https://www.w3.org/", strategy = c("mobile", "desktop"), interval = 0)
  expect_equal(nrow(x), 2)
  expect_equal(ncol(x), 54)
})

test_that("output df returns proper dimensions despite the NULL (simple)", {
  x <- download_pagespeed(url = c(NULL, "https://www.w3.org/"), strategy = "desktop", interval = 0)
  expect_equal(nrow(x), 1)
  expect_equal(ncol(x), 54)
  x <- download_pagespeed(url = c("https://www.w3.org/", NULL), strategy = "mobile", interval = 0)
  expect_equal(nrow(x), 1)
  expect_equal(ncol(x), 54)
})

test_that("output df returns rows with errors (simple)", {
  x <- download_pagespeed(url = c("loremipsumdolorametpageparsererrortest.com", "https://www.w3.org/"), strategy = "desktop", interval = 0)
  expect_equal(nrow(x), 2)
  expect_equal(ncol(x), 54)
})

test_that("interval slows down the downloading (desktop, simple)", {
  start_time <- Sys.time()
  x <- download_pagespeed("https://www.w3.org/", strategy = "desktop", output_type = "simple", interval = 0)
  end_time <- Sys.time()
  diff_1 <- as.numeric(difftime(end_time, start_time))

  start_time <- Sys.time()
  x <- download_pagespeed("https://www.google.co.uk", strategy = "desktop", output_type = "simple", interval = 2)
  end_time <- Sys.time()
  diff_2 <- as.numeric(difftime(end_time, start_time))

  diff_fin <- diff_2 - diff_1
  expect_gte(diff_fin, 2)
})


# -------------------------------------------------------------------------------------------------------------------------------------------
test_that("url param doesn't accept wrong values (raw)", {
  expect_error(download_pagespeed(url = "", strategy = "desktop", output_type = "raw"))
  expect_error(download_pagespeed(url = NA, strategy = "desktop", output_type = "raw"))
  expect_error(download_pagespeed(url = NULL, strategy = "desktop", output_type = "raw"))
  expect_error(download_pagespeed(url = "google com", strategy = "desktop", output_type = "raw"))
  expect_error(download_pagespeed(url = "", strategy = "mobile", output_type = "raw"))
  expect_error(download_pagespeed(url = NA, strategy = "mobile", output_type = "raw"))
  expect_error(download_pagespeed(url = NULL, strategy = "mobile", output_type = "raw"))
  expect_error(download_pagespeed(url = "google com", strategy = "mobile", output_type = "raw"))
})

test_that("API key doesn't accept wrong values (raw)", {
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "desktop", key = "", output_type = "raw"), regexp = "API key is a NULL or has length = 0. Please check it and provide a proper API key.")
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "desktop", key = NULL, output_type = "raw"), regexp = "argument is of length zero")
})

test_that("strategy param doesn't accept wrong values (raw)", {
  expect_error(download_pagespeed("https://www.w3.org/", strategy = NA, output_type = "raw"))
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "", output_type = "raw"))
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "loremipsum", output_type = "raw"))
})

test_that("interval param doesn't accept wrong values (raw)", {
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "desktop", output_type = "raw", interval = -1))
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "mobile", output_type = "raw", interval = -1))
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "desktop", output_type = "raw", interval = 121))
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "mobile", output_type = "raw", interval = 121))
})

test_that("filter_third_party param doesn't accept wrong values (raw)", {
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "desktop", output_type = "raw", filter_third_party = ""))
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "mobile", output_type = "raw", filter_third_party = NA))
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "mobile", output_type = "raw", filter_third_party = 2))
})

test_that("locale param doesn't accept wrong values (raw)", {
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "desktop", output_type = "raw", locale = ""))
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "mobile", output_type = "raw", locale = NA))
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "mobile", output_type = "raw", locale = 1))
})

test_that("rule param doesn't accept wrong values (raw)", {
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "desktop", output_type = "raw", rule = ""))
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "mobile", output_type = "raw", rule = NA))
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "mobile", output_type = "raw", rule = 1))
})

test_that("screenshot param doesn't accept wrong values (raw)", {
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "desktop", output_type = "raw", screenshot = ""))
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "mobile", output_type = "raw", screenshot = NA))
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "mobile", output_type = "raw", screenshot = 2))
})

test_that("snapshots param doesn't accept wrong values (raw)", {
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "desktop", output_type = "raw", snapshots = ""))
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "mobile", output_type = "raw", snapshots = NA))
  expect_error(download_pagespeed("https://www.w3.org/", strategy = "mobile", output_type = "raw", snapshots = 2))
})

test_that("basic output nested list has proper length (desktop, raw)", {
  x <- download_pagespeed("https://www.w3.org/", strategy = "desktop", output_type = "raw", interval = 0)
  expect_equal(length(x), 1)
  expect_equal(length(x[[1]]), 10)
  expect_equal(length(x[[1]]$formattedResults), 2)
  expect_equal(length(x[[1]]$formattedResults$ruleResults), 10)
})

test_that("basic output nested list has proper length (mobile, raw)", {
  x <- download_pagespeed("https://www.w3.org/", strategy = "mobile", output_type = "raw", interval = 0)
  expect_equal(length(x), 1)
  expect_equal(length(x[[1]]), 10)
  expect_equal(length(x[[1]]$formattedResults), 2)
  expect_equal(length(x[[1]]$formattedResults$ruleResults), 10)
})

test_that("interval slows down the downloading (mobile, raw)", {
  start_time <- Sys.time()
  x <- download_pagespeed("https://www.w3.org/", strategy = "mobile", output_type = "raw", interval = 0)
  end_time <- Sys.time()
  diff_1 <- as.numeric(difftime(end_time, start_time))

  start_time <- Sys.time()
  x <- download_pagespeed("https://www.google.co.uk", strategy = "mobile", output_type = "raw", interval = 2)
  end_time <- Sys.time()
  diff_2 <- as.numeric(difftime(end_time, start_time))

  diff_fin <- diff_2 - diff_1
  expect_gte(diff_fin, 2)
})
