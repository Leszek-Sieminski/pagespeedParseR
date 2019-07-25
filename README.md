# pagespeedParseR  v1.0.0
![Lifecycle_Status](https://img.shields.io/badge/lifecycle-maturing-blue.svg)
[![Build status](https://travis-ci.org/Leszek-Sieminski/pagespeedParseR.svg?branch=master)](https://travis-ci.org/Leszek-Sieminski/pagespeedParseR)
[![Coverage status](https://codecov.io/gh/Leszek-Sieminski/pagespeedParseR/branch/master/graph/badge.svg)](https://codecov.io/github/Leszek-Sieminski/pagespeedParseR?branch=master)

R wrapper for Google Pagespeed Insights API

* [News](#news)
* [What is Google Pagespeed Insights?](#what-is-google-pagespeed-insights)
* [Other Pagespeed API packages in R](#other-pagespeed-packages-in-r)
* [Why pagespeedParseR when there are other packages?](#but-why-another-r-package-for-pagespeed-api)
* [Features](#features)
* [Acquiring API access token](#acquiring-api-access-token)
* [Installation](#installation)
* [Authentication](#authentication)
* [Usage](#usage)

## News 

##### **2019-07-...**, v1.0.0 (package api shift + release):
Big paradigm shift, may break exisiting code!

* there is only **one** function that downloads just the nested lists - *download_lighthouse()*. Parameters *output_type* & *long_result* are now obsolete (and will break the function) and were moved into specialised extraction functions (see below)
* to extract the data from the export in the form of data frames, use new functions: *extract_lighthouse_...()*. You can choose to export 1) just the category scores, 2) most of the data from all audits or 3) detailed data from choosen audit (including errors & recommendations)
* pagespeedParseR should be able to handle thousands of URLs thanks to the new cache functionality! The results are cached into temporary binary file 'db.llo' saved in a current working directory and thus should not cause memory allocation issues
* you can choose to export the API output as a binary file to location of your choosing and access its data without loading it up into the global enviroment. You can just use the pointer overloading. See more in *download_lighthouse()* documentation
* if you happen to find a bug, don't hesitate to inform me and/or [add an issue](https://github.com/Leszek-Sieminski/pagespeedParseR/issues)

To find out more, please read the documentation.

## What is Google Pagespeed Insights?
Google Pagespeed is an online tool that identifies performance issues for a given URL and provides suggestions and optimizations. See [more details](https://developers.google.com/speed/pagespeed/insights/?hl=pl).

Google Lighthouse is another tool for webdevelopers that helps in fixing page performance. It was recently featured in Pagespeed API (version 5). You can try it via Chrome browser, see [more details](https://developers.google.com/web/tools/lighthouse/).

## Other Pagespeed packages in R
There are other R packages for Pagespeed that I find very valuable:

* googlePageSpeedR - [Phippsy/googlePageSpeedR](https://github.com/Phippsy/googlePageSpeedR)
* gpagespeed - [simitpatel/gpagespeed](https://github.com/simitpatel/gpagespeed)
* pagespeed - [mhairi/pagespeed](https://github.com/mhairi/pagespeed)

## But why another R package for Pagespeed API?
Parsing **API response returns nested list object** in R which isn't very convenient to extract data from (at least in R), so the outputs of above packages' functions are limited (doesn't contain recommendations). The goal of pagespeedParseR is to **create simple output in form of data frame** which contains both performance scores and most of recommendations + list of errors. Alternatively, **user can decide to obtain the output in raw form of nested list** and parse it by himself/herself if that's not enough.

## Features
* Authenticate with an API key
* Choose report content: use download_pagespeed() for classic PageSpeed v4 results OR download_lighthouse() for new Lighthouse (PageSpeed v5) results
* Choose output format: simple (conveniently parsed data frame with most important info) or raw (nested list with all the data provided by the API)
* Query multiple URLs in one function call and keep information which URL's failed the check
* Query every page for both Desktop and Mobile in one function call
* Control API limits usage with simple interval mechanism

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

### Download & load

```r
# libraries -------------------------------------------------------------------
install.packages("devtools")
library(devtools)
devtools::install_github("Leszek-Sieminski/pagespeedParseR")
library(pagespeedParseR)
```

### Authentication
```r
# authentication --------------------------------------------------------------
auth_pagespeed("12345") # Not run, example
```

### Downloading Lighthouse reports as objects to global enviroment
```r
# downloading Lighthouse as objects -------------------------------------------
obj1 <- download_lighthouse(
  url        = c("https://www.w3.org/"), # single URL
  categories = c("performance",
                 "accessibility",
                 "best-practices",
                 "pwa",
                 "seo"))

obj2 <- download_lighthouse(
  url        = c("https://www.w3.org/",     # multi-URL
                 "https://www.google.com"),
  categories = c("performance",
                 "accessibility",
                 "best-practices",
                 "pwa",
                 "seo"))
```

### Downloading Lighthouse as references to physical cache files on disk
```r
# downloading Lighthouse as references to physical files on disk --------------
ref1 <- download_lighthouse(
  url            = c("https://www.w3.org/"), # single URL
  categories     = c("performance",
                     "accessibility",
                     "best-practices",
                     "pwa",
                     "seo"),
  as_reference   = TRUE,
  reference_path = "ref_path_1.llo")

ref2 <- download_lighthouse(
  url            = c("https://www.w3.org/",     # multi-URL
                     "https://www.google.com"),
  categories     = c("performance",
                 "accessibility",
                 "best-practices",
                 "pwa",
                 "seo"),
  as_reference   = TRUE,
  reference_path = "ref_path_2.llo")
```

### Extracting only Lighthouse category scores
```r
# extracting only category scores ---------------------------------------------
scor_ref_1 <- pagespeedParseR::extract_lighthouse_scores(ref1)
scor_ref_2 <- pagespeedParseR::extract_lighthouse_scores(ref2)
scor_obj_1 <- pagespeedParseR::extract_lighthouse_scores(obj1)
scor_obj_2 <- pagespeedParseR::extract_lighthouse_scores(obj2)

str(scor_obj_2)
# 'data.frame':	2 obs. of  7 variables:
# $ url           : chr  "https://www.w3.org/" "https://www.google.com/"
# $ device        : chr  "desktop" "desktop"
# $ performance   : num  1 1
# $ accessibility : num  0.78 0.89
# $ best.practices: num  0.77 0.92
# $ seo           : num  1 0.8
# $ pwa           : num  0.54 0.46
```

### Extracting most important Lighthouse data from all audits
```r
# extracting most important data from all audits ------------------------------
gen_ref_1  <- pagespeedParseR::extract_lighthouse_general_info(ref1)
gen_obj_1  <- pagespeedParseR::extract_lighthouse_general_info(obj1)
gen_obj_2  <- pagespeedParseR::extract_lighthouse_general_info(obj2)
gen_ref_2  <- pagespeedParseR::extract_lighthouse_general_info(ref2)

str(gen_ref_2)
# 'data.frame':	2 obs. of  1630 variables:
# $ device               : chr  "desktop" "desktop"
# $ url                  : chr  "https://www.w3.org/" "https://www.google.com/"
# $ finalUrl             : chr  "https://www.w3.org/" "https://www.google.com/"
# $ status_code          : num  200 200
# $ score.accessibility  : num  0.78 0.89
# $ score.best-practices : num  0.77 0.92
# $ score.performance    : num  1 1
# $ score.pwa            : num  0.54 0.46
# $ score.seo            : num  1 0.8
# ... (truncated)
```

### PageSpeed reports as wide data frames (most important data)
```r
# PageSpeed reports -  Data Frames --------------------------------------------
# download simple data frame with Pagespeed report for Google.com
ps_df_1 <- download_pagespeed(url = "https://www.google.com", output_type = "simple")

# run Pagespeed reports for Google.com & Bing.com for mobile and
# return in a data frame with most important columns
ps_df_2 <- download_pagespeed(url = c("https://www.google.com", 
                                      "https://www.bing.com/"), 
                              output_type = "simple",      # return the results in a wide data frame
                              strategy = "mobile",         # run tests for mobile
                              interval = 1)                # wait 1 second between the calls to API 

# run Pagespeed reports for Google.com & Bing.com for both desktop & mobile and
# return in a data frame with most important columns                              
ps_df_3 <- download_pagespeed(url = c("https://www.google.com", 
                                      "https://www.bing.com/"), 
                              output_type = "simple",      # return the results in a wide data frame
                              strategy = c("desktop",      # check both desktop and mobile, bind
                                           "mobile"), 
                              interval = 2)                # wait 2 seconds between the calls to API 
```

### PageSpeed reports as nested lists (all data)
```r
# PageSpeed reports - Nested Lists --------------------------------------------
# download nested list with Pagespeed report for Google.com
ps_nl_1 <- download_pagespeed(url = "https://www.google.com", 
                              output_type = "raw")

# run Pagespeed for Google.com & Bing.com for desktop and
# return in a nested list with all possible data
ps_nl_2 <- download_pagespeed(url = c("https://www.google.com", 
                                      "https://www.bing.com/"), 
                              output_type = "raw", 
                              strategy = "desktop", 
                              interval = 1)

# check "Performance" for Google.com & Bing.com for both desktop & mobile and
# return in a nested list with all possible data
ps_nl_3 <- download_pagespeed(url = c("https://www.google.com", 
                                      "https://www.bing.com/"), 
                              output_type = "raw", 
                              strategy = c("desktop", 
                                           "mobile"), 
                              interval = 2)
```
