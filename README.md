# pagespeedParseR  v0.1.0.9000 (under development)
R wrapper for Google Pagespeed Insights API

* [What is Google Pagespeed Insights?](#what-is-google-pagespeed-insights)
* [Other Pagespeed API packages in R](#other-pagespeed-api-packages-in-r)
* [Why pagespeedParseR when there are other packages?](#why-another-r-package-for-pagespeed)
* [Features](#features)
* [Acquiring API access token](#acquiring-api-access-token)
* [Installation](#installation)
* [Authentication](#authentication)
* [Usage](#usage)

## What is Google Pagespeed Insights?
It's an online tool that identifies performance issues for a given URL and provides suggestions and optimizations.

## Other Pagespeed packages in R
There are other R packages for Pagespeed that I find very valuable:

* googlePageSpeedR - [Phippsy/googlePageSpeedR](https://github.com/Phippsy/googlePageSpeedR)
* gpagespeed - [simitpatel/gpagespeed](https://github.com/simitpatel/gpagespeed)
* pagespeed - [mhairi/pagespeed](https://github.com/mhairi/pagespeed)

## But why another R package for Pagespeed API?
Parsing **API response returns nested list object** in R which isn't very convenient to extract data from (at least in R), so the outputs of above packages' functions are limited (doesn't contain recommendations). The goal of pagespeedParseR is to **create simple output in form of data frame** which contains both performance scores and most of recommendations + list of errors. Alternatively, **user can decide to obtain the output in raw form of nested list** and parse it by himself/herself if that's not enough.

## Features
* Authenticate with an API key
* Choose output format: simple (conveniently parsed data frame with most important info) or raw (nested list with all the data provided by the API)
* Query multiple URLs in one function call
* Query every page for both Desktop and Mobile in one function call
* Find out which URLs weren't analyzed if error happened - output object keeps information about pages that didn't create the report
* Control API limits usage with simple interval mechanism
* [IN PROGRESS] Download data for two API versions (4th = ready, 5th = under development)
* [IN PROGRESS] Save the output as the tmp.Rdata files in case of trouble

## Acquiring API access token
Visit your [Google Developers Console](https://console.developers.google.com/) page, create the project, [switch on](https://console.developers.google.com/apis/library/pagespeedonline.googleapis.com) Pagespeed API and copy your API access token into R from the credentials screen.

## Installation

```r
install.packages("devtools")
devtools::install_github("Leszek-Sieminski/pagespeedParseR")
```

## Authentication

```r
library(pagespeedParseR)
api_key <- "12345" # example
auth_pagespeed(api_key)
```

## Usage

```r
library(pagespeedParseR)
auth_pagespeed("12345") # not run, example
checked_url <- download_pagespeed("https://www.google.com/")

checked_url_raw_list <- download_pagespeed(
  url = c("https://www.google.com/", "https://www.bing.com/"),
  strategy = c("desktop", "mobile"),
  output_type = "raw",
  api_version = 4,
  interval = 2)

checked_url_simple_df <- download_pagespeed(
  url = c("https://www.google.com/", "https://www.bing.com/"),
  strategy = c("desktop", "mobile"),
  api_version = 4)

```
