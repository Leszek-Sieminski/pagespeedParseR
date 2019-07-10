# context("LH helper: sort")
# library(pagespeedParseR)
# library(testthat)
#
# testthat::test_that("function properly sorts dataframe's columns order", {
#   before <- data.frame(
#     "device"              = "device",
#     "url"                 = "url",
#     "score.performance"   = "score.performance",
#     "status_code"         = "status_code",
#     "C"                   = "C",
#     "A"                   = "A",
#     "B"                   = "B",
#     "score.seo"           = "score.seo",
#     stringsAsFactors      = F)
#
#   ideal <- data.frame(
#     "device"              = "device",
#     "url"                 = "url",
#     "status_code"         = "status_code",
#     "score.performance"   = "score.performance",
#     "score.seo"           = "score.seo",
#     "A"                   = "A",
#     "B"                   = "B",
#     "C"                   = "C",
#     stringsAsFactors = F)
#
#   after <- pagespeedParseR:::fun_lh_basic_sort(before)
#   testthat::expect_identical(after, ideal)
#
# })
