# setup -----------------------------------------------------------------------
context("Extract General Info: extract general info from all URLs")
library(pagespeedParseR)
library(testthat)

# defensive -------------------------------------------------------------------
# output ----------------------------------------------------------------------
test_that("general info object have proper columns", {
  # download Lighthouse report as a global enviroment object
  lh_object <- download_lighthouse(
    url        = c("https://www.w3.org/",
                   "https://www.w3schools.com/"),
    categories = c("performance",
                   "accessibility",
                   "best-practices",
                   "pwa",
                   "seo"))

  # download Lighthouse report as a reference to file
  lh_ref <- download_lighthouse(
    url            = c("https://www.w3.org/",
                       "https://www.w3schools.com/"),
    categories     = c("performance",
                       "accessibility",
                       "best-practices",
                       "pwa",
                       "seo"),
    as_reference   = TRUE,
    reference_path = "ref_path.llo")

  # extraction
  scores_obj  <- extract_lighthouse_general_info(lh_object)
  scores_ref  <- extract_lighthouse_general_info(lh_ref)

  # cleaning
  rm(lh_ref)
  file.remove("ref_path.llo")

  # checks
  expect_equal(length(dim(scores_obj)), 2)
  expect_gte(ncol(scores_obj), 1000)
  expect_equal(nrow(scores_obj), 2)

  expect_equal(length(dim(scores_ref)), 2)
  expect_gte(ncol(scores_ref), 1000)
  expect_equal(nrow(scores_ref), 2)
})
