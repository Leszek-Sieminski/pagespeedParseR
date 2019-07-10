context("PS helper: url extract")
library(pagespeedParseR)
library(testthat)

test_that("function properly returns NULL from NULL object", {
  expect_identical(pagespeedParseR:::ps_url_extract(NULL), NULL)
})

test_that("function properly returns collapsed URLs from nested list", {
  x <- list("A" = list("category" = "category_1", "value" = "http://random-website.com/"),
            "B" = list("category" = "category_2", "value" = "https://www.randomurl.com"))

  expect_identical(pagespeedParseR:::ps_url_extract(x), "http://random-website.com/, https://www.randomurl.com")
})
