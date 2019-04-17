context("PSI Download lvl 3")
library(pagespeedParseR)

testthat::test_that("url param doesn't accept wrong values (simple)", {
  testthat::expect_error(pagespeedParseR:::download_pagespeed(url = "", strategy = "desktop"))
  testthat::expect_error(pagespeedParseR:::download_pagespeed(url = NA, strategy = "desktop"))
  testthat::expect_error(pagespeedParseR:::download_pagespeed(url = NULL, strategy = "desktop"))
  testthat::expect_error(pagespeedParseR:::download_pagespeed(url = "google com", strategy = "desktop"))
  testthat::expect_error(pagespeedParseR:::download_pagespeed(url = "", strategy = "mobile"))
  testthat::expect_error(pagespeedParseR:::download_pagespeed(url = NA, strategy = "mobile"))
  testthat::expect_error(pagespeedParseR:::download_pagespeed(url = NULL, strategy = "mobile"))
  testthat::expect_error(pagespeedParseR:::download_pagespeed(url = "google com", strategy = "mobile"))
})

testthat::test_that("url param doesn't accept wrong vectors (simple)", {
  testthat::expect_error(pagespeedParseR:::download_pagespeed(url = c(""), strategy = "desktop"))
  testthat::expect_error(pagespeedParseR:::download_pagespeed(url = c("", "https://www.google.com"), strategy = "desktop"))
  testthat::expect_error(pagespeedParseR:::download_pagespeed(url = c(NA, "https://www.google.com"), strategy = "mobile"))
  testthat::expect_error(pagespeedParseR:::download_pagespeed(url = c("https://www.google.com", "google com"), strategy = "mobile"))
  testthat::expect_error(pagespeedParseR:::download_pagespeed(url = c("https://www.google.com", ""), strategy = "desktop"))
  testthat::expect_error(pagespeedParseR:::download_pagespeed(url = c("https://www.google.com", NA), strategy = "mobile"))
  testthat::expect_error(pagespeedParseR:::download_pagespeed(url = c("https://www.google.com", "google com"), strategy = "mobile"))
})

testthat::test_that("API key doesn't accept wrong values (simple)", {
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "desktop", api_key = ""), regexp = "API key is a NULL or has length = 0. Please check it and provide a proper API key.")
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "desktop", api_key = NULL), regexp = "API key is a NULL or has length = 0. Please check it and provide a proper API key.")
})

testthat::test_that("strategy param doesn't accept wrong values (simple)", {
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = NA))
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = ""))
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "loremipsum"))
})

testthat::test_that("interval param doesn't accept wrong values (simple)", {
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "desktop", interval = -1))
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "mobile", interval = -1))
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "desktop", interval = 121))
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "mobile", interval = 121))
})

testthat::test_that("filter_third_party param doesn't accept wrong values (simple)", {
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "desktop", filter_third_party = ""))
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "mobile", filter_third_party = NA))
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "mobile", filter_third_party = 2))
})

testthat::test_that("locale param doesn't accept wrong values (simple)", {
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "desktop", locale = ""))
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "mobile", locale = NA))
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "mobile", locale = 1))
})

testthat::test_that("rule param doesn't accept wrong values (simple)", {
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "desktop", rule = ""))
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "mobile", rule = NA))
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "mobile", rule = 1))
})


testthat::test_that("screenshot param doesn't accept wrong values (simple)", {
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "desktop", screenshot = ""))
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "mobile", screenshot = NA))
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "mobile", screenshot = 2))
})

testthat::test_that("snapshots param doesn't accept wrong values (simple)", {
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "desktop", snapshots = ""))
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "mobile", snapshots = NA))
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "mobile", snapshots = 2))
})

testthat::test_that("basic output df has proper dimensions (desktop, simple)", {
  x <- pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "desktop", interval = 0)
  testthat::expect_equal(nrow(x), 1)
  testthat::expect_equal(ncol(x), 54)
})

testthat::test_that("basic output df has proper dimensions (mobile, simple)", {
  x <- pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "mobile", interval = 0)
  testthat::expect_equal(nrow(x), 1)
  testthat::expect_equal(ncol(x), 54)
})

testthat::test_that("output df returns proper dimensions despite the NULL (simple)", {
  x <- pagespeedParseR:::download_pagespeed(url = c(NULL, "https://www.google.com"), strategy = "desktop", interval = 0)
  testthat::expect_equal(nrow(x), 1)
  testthat::expect_equal(ncol(x), 54)
  x <- pagespeedParseR:::download_pagespeed(url = c("https://www.google.com", NULL), strategy = "mobile", interval = 0)
  testthat::expect_equal(nrow(x), 1)
  testthat::expect_equal(ncol(x), 54)
})

testthat::test_that("output df returns rows with errors (simple)", {
  x <- pagespeedParseR:::download_pagespeed(url = c("loremipsumdolorametpageparsererrortest.com", "https://www.google.com"), strategy = "desktop", interval = 0)
  testthat::expect_equal(nrow(x), 2)
  testthat::expect_equal(ncol(x), 54)
})

testthat::test_that("interval slows down the downloading (desktop, simple)", {
  start_time <- Sys.time()
  x <- pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "desktop", output_type = "simple", interval = 0)
  end_time <- Sys.time()
  diff_1 <- as.numeric(difftime(end_time, start_time))

  start_time <- Sys.time()
  x <- pagespeedParseR:::download_pagespeed("https://www.google.co.uk", strategy = "desktop", output_type = "simple", interval = 2)
  end_time <- Sys.time()
  diff_2 <- as.numeric(difftime(end_time, start_time))

  diff_fin <- diff_2 - diff_1
  testthat::expect_gte(diff_fin, 2)
})


# -------------------------------------------------------------------------------------------------------------------------------------------
testthat::test_that("url param doesn't accept wrong values (raw)", {
  testthat::expect_error(pagespeedParseR:::download_pagespeed(url = "", strategy = "desktop", output_type = "raw"))
  testthat::expect_error(pagespeedParseR:::download_pagespeed(url = NA, strategy = "desktop", output_type = "raw"))
  testthat::expect_error(pagespeedParseR:::download_pagespeed(url = NULL, strategy = "desktop", output_type = "raw"))
  testthat::expect_error(pagespeedParseR:::download_pagespeed(url = "google com", strategy = "desktop", output_type = "raw"))
  testthat::expect_error(pagespeedParseR:::download_pagespeed(url = "", strategy = "mobile", output_type = "raw"))
  testthat::expect_error(pagespeedParseR:::download_pagespeed(url = NA, strategy = "mobile", output_type = "raw"))
  testthat::expect_error(pagespeedParseR:::download_pagespeed(url = NULL, strategy = "mobile", output_type = "raw"))
  testthat::expect_error(pagespeedParseR:::download_pagespeed(url = "google com", strategy = "mobile", output_type = "raw"))
})

testthat::test_that("API key doesn't accept wrong values (raw)", {
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "desktop", api_key = "", output_type = "raw"), regexp = "API key is a NULL or has length = 0. Please check it and provide a proper API key.")
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "desktop", api_key = NULL, output_type = "raw"), regexp = "API key is a NULL or has length = 0. Please check it and provide a proper API key.")
})

testthat::test_that("strategy param doesn't accept wrong values (raw)", {
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = NA, output_type = "raw"))
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "", output_type = "raw"))
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = c("desktop", "mobile"), output_type = "raw"))
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "loremipsum", output_type = "raw"))
})

testthat::test_that("interval param doesn't accept wrong values (raw)", {
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "desktop", output_type = "raw", interval = -1))
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "mobile", output_type = "raw", interval = -1))
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "desktop", output_type = "raw", interval = 121))
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "mobile", output_type = "raw", interval = 121))
})

testthat::test_that("filter_third_party param doesn't accept wrong values (raw)", {
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "desktop", output_type = "raw", filter_third_party = ""))
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "mobile", output_type = "raw", filter_third_party = NA))
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "mobile", output_type = "raw", filter_third_party = 2))
})

testthat::test_that("locale param doesn't accept wrong values (raw)", {
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "desktop", output_type = "raw", locale = ""))
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "mobile", output_type = "raw", locale = NA))
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "mobile", output_type = "raw", locale = 1))
})

testthat::test_that("rule param doesn't accept wrong values (raw)", {
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "desktop", output_type = "raw", rule = ""))
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "mobile", output_type = "raw", rule = NA))
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "mobile", output_type = "raw", rule = 1))
})

testthat::test_that("screenshot param doesn't accept wrong values (raw)", {
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "desktop", output_type = "raw", screenshot = ""))
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "mobile", output_type = "raw", screenshot = NA))
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "mobile", output_type = "raw", screenshot = 2))
})

testthat::test_that("snapshots param doesn't accept wrong values (raw)", {
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "desktop", output_type = "raw", snapshots = ""))
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "mobile", output_type = "raw", snapshots = NA))
  testthat::expect_error(pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "mobile", output_type = "raw", snapshots = 2))
})

testthat::test_that("basic output nested list has proper length (desktop, raw)", {
  x <- pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "desktop", output_type = "raw", interval = 0)
  testthat::expect_equal(length(x), 1)
  testthat::expect_equal(length(x[[1]]), 10)
  testthat::expect_equal(length(x[[1]]$pageStats), 12)
  testthat::expect_equal(length(x[[1]]$formattedResults), 2)
  testthat::expect_equal(length(x[[1]]$formattedResults$ruleResults), 10)
})

testthat::test_that("basic output nested list has proper length (mobile, raw)", {
  x <- pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "mobile", output_type = "raw", interval = 0)
  testthat::expect_equal(length(x), 1)
  testthat::expect_equal(length(x[[1]]), 10)
  testthat::expect_equal(length(x[[1]]$pageStats), 14)
  testthat::expect_equal(length(x[[1]]$formattedResults), 2)
  testthat::expect_equal(length(x[[1]]$formattedResults$ruleResults), 10)
})

testthat::test_that("interval slows down the downloading (mobile, raw)", {
  start_time <- Sys.time()
  x <- pagespeedParseR:::download_pagespeed("https://www.google.com", strategy = "mobile", output_type = "raw", interval = 0)
  end_time <- Sys.time()
  diff_1 <- as.numeric(difftime(end_time, start_time))

  start_time <- Sys.time()
  x <- pagespeedParseR:::download_pagespeed("https://www.google.co.uk", strategy = "mobile", output_type = "raw", interval = 2)
  end_time <- Sys.time()
  diff_2 <- as.numeric(difftime(end_time, start_time))

  diff_fin <- diff_2 - diff_1
  testthat::expect_gte(diff_fin, 2)
})
