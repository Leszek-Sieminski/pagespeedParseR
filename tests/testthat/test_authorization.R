context("Authorization")
library(pagespeedParseR)

testthat::test_that("API key parameter cannot be NA or empty", {
  testthat::expect_error(auth_pagespeed(api_key = ""))
  testthat::expect_error(auth_pagespeed(api_key = NA), "api_key contains 1 missing values")
  testthat::expect_error(auth_pagespeed(api_key = NULL), "api_key has an empty dimension")
})

testthat::test_that("API key parameter works when proper", {
  testthat::expect_message(auth_pagespeed(api_key = Sys.getenv("PAGESPEED_API_KEY")), "API key authorized.")
})

testthat::test_that("API key parameter fails when wrong", {
  testthat::expect_error(auth_pagespeed(api_key = "abcd123"), regexp = "Authorization error: HTTP status code [0-9]{3}. Check your API key.")
})

testthat::test_that("verbose parameter cannot be NA or empty", {
  testthat::expect_error(auth_pagespeed(api_key = Sys.getenv("PAGESPEED_API_KEY"), verbose = ""))
  testthat::expect_error(auth_pagespeed(api_key = Sys.getenv("PAGESPEED_API_KEY"), verbose = NA))
  testthat::expect_error(auth_pagespeed(api_key = Sys.getenv("PAGESPEED_API_KEY"), verbose = NULL))
})

testthat::test_that("verbose parameter works when proper", {
  testthat::expect_message(auth_pagespeed(api_key = Sys.getenv("PAGESPEED_API_KEY"), verbose = TRUE), "API key authorized.")
  testthat::expect_silent(auth_pagespeed(api_key = Sys.getenv("PAGESPEED_API_KEY"), verbose = FALSE))
})
