# setup -----------------------------------------------------------------------
context("Extract Scores: extract scores from all URLs")
library(pagespeedParseR)
library(testthat)

# defensive -------------------------------------------------------------------
# output ----------------------------------------------------------------------
test_that("category scores object have proper columns", {
  # download Lighthouse report as a global enviroment object
  lh_object <- download_lighthouse(
    url        = c("https://www.w3.org/",
                   "https://www.w3.org/"),
    categories = c("performance",
                   "accessibility",
                   "best-practices",
                   "pwa",
                   "seo"))

  # download Lighthouse report as a reference to file
  lh_ref <- download_lighthouse(
    url            = c("https://www.w3.org/",
                       "https://www.w3.org/"),
    categories     = c("performance",
                       "accessibility",
                       "best-practices",
                       "pwa",
                       "seo"),
    as_reference   = TRUE,
    reference_path = "ref_path.llo")

  # extraction
  scores_obj  <- pagespeedParseR::extract_lighthouse_scores(lh_object)
  scores_ref <- pagespeedParseR::extract_lighthouse_scores(lh_ref)

  # cleaning
  rm(lh_ref)
  file.remove("ref_path.llo")

  # checks
  expect_equal(length(dim(scores_obj)), 2)
  expect_equal(ncol(scores_obj), 7)
  expect_equal(nrow(scores_obj), 2)

  expect_equal(length(dim(scores_ref)), 2)
  expect_equal(ncol(scores_ref), 7)
  expect_equal(nrow(scores_ref), 2)
})
