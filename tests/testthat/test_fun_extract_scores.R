# setup -----------------------------------------------------------------------
context("LH helper: extract scores from 1 URL")
library(pagespeedParseR)
library(testthat)

# defensive -------------------------------------------------------------------

# output ----------------------------------------------------------------------
test_that("category scores object have proper columns", {
  # download
  lh_object <- download_lighthouse(
    url = c("https://www.w3.org/"),
    categories = c("performance", "accessibility", "best-practices", "pwa","seo"))

  lh_ref <- download_lighthouse(
    url = c("https://www.w3.org/"),
    categories = c("performance", "accessibility", "best-practices", "pwa", "seo"),
    as_reference = TRUE,
    reference_path = "ref_path.llo")

  # extraction
  scores_obj <- vector(mode = "list", length = length(lh_object))
  scores_ref <- vector(mode = "list", length = length(lh_ref))

  for(i in 1:length(scores_obj)) res <- pagespeedParseR:::fun_lh_extract_scores(lh_object[[i]]) ; scores_obj[i] <- list(res)
  scores_obj <- map_dfr(scores_obj, `[`)

  for(i in 1:length(scores_ref)) res <- pagespeedParseR:::fun_lh_extract_scores(lh_ref[[i]]) ; scores_ref[i] <- list(res)
  scores_ref <- map_dfr(scores_ref, `[`)

  rm(lh_ref)
  file.remove("ref_path.llo")

  # check
  expect_equal(length(dim(scores_obj)), 2)
  expect_equal(ncol(scores_obj), 7)
  expect_equal(nrow(scores_obj), 1)

  expect_equal(length(dim(scores_ref)), 2)
  expect_equal(ncol(scores_ref), 7)
  expect_equal(nrow(scores_ref), 1)
})





