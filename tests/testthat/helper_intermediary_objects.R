message("Helper: intermediary objects")

# libraries -------------------------------------------------------------------
library(httr)
library(assertthat)
library(purrr)
library(jsonlite)
library(pagespeedParseR)
library(dplyr)
library(tidyr)
library(testthat)

# keys vector -----------------------------------------------------------------
keys_vector <- c(
  Sys.getenv("PAGESPEED_API_KEY_1"),
  Sys.getenv("PAGESPEED_API_KEY_2"),
  Sys.getenv("PAGESPEED_API_KEY_3"))

# url_sample ------------------------------------------------------------------
url_sample <- c(
  "https://www.bing.com/",
  "https://www.aol.com/",
  "https://yandex.com/",
  "https://archive.org/",
  "https://www.loc.gov/",
  "https://www.wikipedia.org/",
  "https://www.who.int/",
  "https://www.worldbank.org/",
  "https://www.un.org/en/",
  "https://www.wp.pl/",
  "https://www.onet.pl/")

# parsed object ---------------------------------------------------------------
req_test <- GET(
  url = "https://www.googleapis.com/pagespeedonline/v5/runPagespeed",
  query = list(
    url          = sample(url_sample, 1),
    strategy     = NULL,
    key          = sample(keys_vector, 1),
    locale       = NULL,
    category     = "performance",
    category     = "seo",
    category     = "accessibility",
    category     = "best-practices",
    category     = "pwa",
    utm_campaign = NULL,
    utm_source   = NULL))

parsed_test <- fromJSON(content(req_test, as = "text", encoding = "UTF-8"))
rm(req_test)
