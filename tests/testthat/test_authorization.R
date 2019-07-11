context("Authorization")
library(pagespeedParseR)
library(testthat)

test_that("API key parameter cannot be NA or empty", {
  expect_error(auth_pagespeed(api_key = ""))
  expect_error(auth_pagespeed(api_key = NA), "api_key contains 1 missing values")
  expect_error(auth_pagespeed(api_key = NULL), "api_key has an empty dimension")
})

test_that("API key parameter works when proper", {
  expect_message(auth_pagespeed(api_key = Sys.getenv("PAGESPEED_API_KEY")), "API key authorized.")
})

test_that("verbose parameter cannot be NA or empty", {
  expect_error(auth_pagespeed(api_key = Sys.getenv("PAGESPEED_API_KEY"), verbose = ""))
  expect_error(auth_pagespeed(api_key = Sys.getenv("PAGESPEED_API_KEY"), verbose = NA))
  expect_error(auth_pagespeed(api_key = Sys.getenv("PAGESPEED_API_KEY"), verbose = NULL))
})

test_that("verbose parameter works when proper", {
  expect_message(auth_pagespeed(api_key = Sys.getenv("PAGESPEED_API_KEY"), verbose = TRUE), "API key authorized.")
  expect_silent(auth_pagespeed(api_key = Sys.getenv("PAGESPEED_API_KEY"), verbose = FALSE))
})
