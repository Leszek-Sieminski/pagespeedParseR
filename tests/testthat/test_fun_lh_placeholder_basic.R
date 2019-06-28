context("LH placeholder (basic)")
library(pagespeedParseR)


testthat::test_that("placeholder returns proper columns (accessibility)", {
  x <- pagespeedParseR:::v5_placeholder_basic(categories = c("accessibility"))
  testthat::expect_equal(ncol(x), 3 + 141)
})

testthat::test_that("placeholder returns proper columns (best-practices)", {
  x <- pagespeedParseR:::v5_placeholder_basic(categories = c("best-practices"))
  testthat::expect_equal(ncol(x), 3 + 42)
})

testthat::test_that("placeholder returns proper columns (performance)", {
  x <- pagespeedParseR:::v5_placeholder_basic(categories = c("performance"))
  testthat::expect_equal(ncol(x), 3 + 114)
})

testthat::test_that("placeholder returns proper columns (pwa)", {
  x <- pagespeedParseR:::v5_placeholder_basic(categories = c("pwa"))
  testthat::expect_equal(ncol(x), 3 + 39)
})

testthat::test_that("placeholder returns proper columns (seo)", {
  x <- pagespeedParseR:::v5_placeholder_basic(categories = c("seo"))
  testthat::expect_equal(ncol(x), 3 + 33)
})

testthat::test_that("placeholder returns proper columns (all)", {
  x <- pagespeedParseR:::v5_placeholder_basic(categories = c("accessibility", "best-practices", "performance", "pwa", "seo"))
  testthat::expect_equal(ncol(x), 3 + 141 + 42 + 114 + 39 + 33)
})
