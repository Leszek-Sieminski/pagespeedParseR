# libraries -------------------------------------------------------------------
library(httr)
library(assertthat)
library(purrr)
library(jsonlite)
library(pagespeedParseR)
library(dplyr)
library(tidyr)
library(testthat)

# parsed object ---------------------------------------------------------------
req_test <- GET(url = "https://www.googleapis.com/pagespeedonline/v5/runPagespeed",
  query = list(url = "https://www.w3.org/", strategy = NULL, key = Sys.getenv("PAGESPEED_API_KEY"),
               locale = NULL, category = "performance", category = "seo", category = "accessibility",
               category = "best-practices", category = "pwa", utm_campaign = NULL, utm_source = NULL))

parsed_test <- fromJSON(content(req_test, as = "text", encoding = "UTF-8"))

rm(req_test)

# url_sample ------------------------------------------------------------------
url_sample <- c(
  # "https://www.w3.org/",
  "https://archive.org/",
  "https://www.loc.gov/",
  "https://www.wikipedia.org/",
  "https://www.who.int/",
  "https://www.worldbank.org/",
  "https://www.un.org/en/"
)
