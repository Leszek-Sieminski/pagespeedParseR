context("LH helper: exist")
library(pagespeedParseR)

# testthat::test_that("", {
#   testthat::expect_identical()
# })

testthat::test_that("not detecting LH report sublist's child when NA", {
  results <- NA
  existence <- pagespeedParseR:::fun_lhex(report = "report_1", elem = NULL, object_name = "results")
  testthat::expect_identical(FALSE, existence)
})

testthat::test_that("not detecting LH report sublist's child when NA", {
  results <- NA
  existence <- pagespeedParseR:::fun_lhex(report = "report_1", elem = "element_1.2", object_name = "results")
  testthat::expect_identical(FALSE, existence)
})

testthat::test_that("detecting LH report sublist works", {
  results <- list(
    "report_1" = list(
      "items" = list(
        "element_1.1" = "1.1",
        "element_1.2" = "1.2")),
    "report_2" = list(
      "items" = list(
        "element_2.1" = "2.1",
        "element_2.2" = "2.2")))

  existence <- pagespeedParseR:::fun_lhex(report = "report_1", elem = NULL, object_name = "results")
  # testu <- function(object_name = "results"){
  #   object <- get(object_name, parent.frame())
  #   return(object)
  # }
  #
  # testu()

  testthat::expect_identical(TRUE, existence)
})

testthat::test_that("detecting LH report sublist's child works", {
  results <- list(
    "report_1" = list(
      "items" = list(
        "element_1.1" = "1.1",
        "element_1.2" = "1.2")),
    "report_2" = list(
      "items" = list(
        "element_2.1" = "2.1",
        "element_2.2" = "2.2")))

  existence <- pagespeedParseR:::fun_lhex(report = "report_1", elem = "element_1.2", object_name = "results")
  testthat::expect_identical(TRUE, existence)
})

testthat::test_that("not detecting LH report sublist when missing", {
  results <- list(
    "report_3" = list(
      "items" = list(
        "element_1.1" = "1.1",
        "element_1.2" = "1.2")),
    "report_2" = list(
      "items" = list(
        "element_2.1" = "2.1",
        "element_2.2" = "2.2")))

  existence <- pagespeedParseR:::fun_lhex(report = "report_1", elem = NULL, object_name = "results")
  testthat::expect_identical(FALSE, existence)
})

testthat::test_that("not detecting LH report sublist's child when missing (1st level)", {
  results <- list(
    "report_3" = list(
      "items" = list(
        "element_1.1" = "1.1",
        "element_1.2" = "1.2")),
    "report_2" = list(
      "items" = list(
        "element_2.1" = "2.1",
        "element_2.2" = "2.2")))

  existence <- pagespeedParseR:::fun_lhex(report = "report_1", elem = "element_1.2", object_name = "results")
  testthat::expect_identical(FALSE, existence)
})

testthat::test_that("not detecting LH report sublist's child when missing (2nd level)", {
  results <- list(
    "report_1" = list(
      "ninetendo" = list(
        "element_1.1" = "1.1",
        "element_1.2" = "1.2")),
    "report_2" = list(
      "items" = list(
        "element_2.1" = "2.1",
        "element_2.2" = "2.2")))

  existence <- pagespeedParseR:::fun_lhex(report = "report_1", elem = "element_1.2", object_name = "results")
  testthat::expect_identical(FALSE, existence)
})

testthat::test_that("not detecting LH report sublist's child when missing (3rd level)", {
  results <- list(
    "report_1" = list(
      "items" = list(
        "element_1.1" = "1.1",
        "gibberish" = "1.2")),
    "report_2" = list(
      "items" = list(
        "element_2.1" = "2.1",
        "element_2.2" = "2.2")))

  existence <- pagespeedParseR:::fun_lhex(report = "report_1", elem = "element_1.2", object_name = "results")
  testthat::expect_identical(FALSE, existence)
})
