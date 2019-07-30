# setup -----------------------------------------------------------------------
context("Extract General Info: extract general info from all URLs")
library(pagespeedParseR)
library(testthat)

# defensive -------------------------------------------------------------------
# output ----------------------------------------------------------------------


test_that("general info REFERNCE have proper columns", {
   # download Lighthouse report as a reference to file
  lh_ref <- download_lighthouse(
    url = sample(url_sample, 2), categories = c("performance", "accessibility", "best-practices", "pwa", "seo"),
    as_reference = TRUE, reference_path = "ref_path.llo")

  scores_ref <- extract_lighthouse_general_info(lh_ref)
  rm(lh_ref)
  file.remove("ref_path.llo")

  expect_equal(length(dim(scores_ref)), 2)
  expect_gte(ncol(scores_ref), 1000)
  expect_equal(nrow(scores_ref), 2)
})

test_that("general info OBJECT have proper columns", {
  # download Lighthouse report as a global enviroment object
  lh_object <- download_lighthouse(
    url = sample(url_sample, 2), categories = c("performance", "accessibility", "best-practices", "pwa", "seo"))

  # extraction
  scores_obj  <- extract_lighthouse_general_info(lh_object)

  # checks
  expect_equal(length(dim(scores_obj)), 2)
  expect_gte(ncol(scores_obj), 1000)
  expect_equal(nrow(scores_obj), 2)
})
