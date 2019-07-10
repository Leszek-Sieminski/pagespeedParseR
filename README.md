# pagespeedParseR  v0.3.1.9000
![Lifecycle_Status](https://img.shields.io/badge/lifecycle-maturing-blue.svg)
[![Build status](https://travis-ci.org/Leszek-Sieminski/pagespeedParseR.svg?branch=master)](https://travis-ci.org/Leszek-Sieminski/pagespeedParseR)
[![Coverage status](https://codecov.io/gh/Leszek-Sieminski/pagespeedParseR/branch/master/graph/badge.svg)](https://codecov.io/github/Leszek-Sieminski/pagespeedParseR?branch=master)

R wrapper for Google Pagespeed Insights API

* [News and important information](#news-and-important-information)
* [What is Google Pagespeed Insights?](#what-is-google-pagespeed-insights)
* [Other Pagespeed API packages in R](#other-pagespeed-packages-in-r)
* [Why pagespeedParseR when there are other packages?](#but-why-another-r-package-for-pagespeed-api)
* [Features](#features)
* [Acquiring API access token](#acquiring-api-access-token)
* [Installation](#installation)
* [Authentication](#authentication)
* [Usage](#usage)

## News and plans
##### **2019-07-10**, ver. 0.3.1.9000:
* small bugfixes to *long_result = T* parameter in *download_lighthouse(..., output_type = "simple")*
* small bugfixes to tests

##### **2019-07-05**, ver. 0.3.0.9000 (Lighthouse overhaul):
* big overhaul of *download_lighthouse()* function. Parsing to data frame with *output_type = "simple"* parameter will now provide much more data. However, **it can generate literally hundreds/thousands of columns** (up to ~2500). What is more, the **number of columns IS NOT STABLE**, because it depends on the number of found errors and/or their type
* to ease the pain of dealing with such data frames, I added *long_result* parameter that defaults to *FALSE*. Setting it to TRUE will force the function to spread the data frame into messy, long-like form that I hope to be easier to comprehend
* the behaviour of download_lighthouse(output_type = "raw") or other functions didn't change
* please mind that this is experimental and may cause unexpected errors. If you happen to find one, don't hesitate to inform me and/or add an Issue

##### **Plans for future**:
* reworking the way user uses the package. I want to create only **one** function that downloads just the nested lists and create additional functions that will be used on downloaded nested lists to extract choosen report categories or audits or error examples. I think it will speed up the process, especially for the users that do not want to parse everything but need only some specific information. It would also allow me to add some parallelization features to speed everything up (and wouldn't blow up the API)
* improving quantity of data parsed into data frame
* speeding things up


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

### Startup
```r
library(pagespeedParseR)
auth_pagespeed("12345") # not run, example
```

### Lighthouse reports as wide data frames (most important data)

```r
# Lighthouse reports - Data Frames --------------------------------------------
# download simple data frame with "Performance" Lighthouse report for Google.com:
# that's a lot of columns and you will have problems but you can
# spread/gather them as you like

lh_df_1 <- download_lighthouse(
 url = "https://www.google.com",
 output_type = "simple") # return the results in a wide data frame

class(lh_df_1)
# [1] "data.frame"
dim(lh_df_1)   # 1 row, 779 columns. The number of columns may wildly differ
# [1]   1 779  # because it depends also to number of spotted errors and their types



# this time let's download it and parse into messy long-like table:
lh_df_1_long <- download_lighthouse(
  url = "https://www.google.com",
  output_type = "simple", # return the results in a wide data frame
  long_result = TRUE) # spread the data into easier-to-digest form

class(lh_df_1_long)
# [1] "data.frame"
dim(lh_df_1_long) # 780 rows in 3 columns
# [1] 780   3



# check "Performance" for Google.com & Bing.com for both desktop & mobile and
# return in a data frame with most important columns
lh_df_2 <- download_lighthouse(
  url = c("https://www.google.com",
          "https://www.bing.com/"),
  output_type = "simple", # return the results in a wide data frame
  strategy = c("desktop", # check both desktop and mobile, bind
               "mobile"),
  interval = 1, # wait 1 second between the calls to API
  categories = "performance") # which Lighthouse reports
                              # are to be run?

class(lh_df_2)
# [1] "data.frame"
dim(lh_df_2)
# [1]    4 1231



# check "Performance" and "Accessibility" for Google.com & Bing.com for
# both desktop & mobile and return in a data frame with most important columns
lh_df_3 <- download_lighthouse(
  url = c("https://www.google.com",
          "https://www.bing.com/"),
  output_type = "simple", # return the results in a wide data frame
  strategy = c("desktop", # check both desktop and mobile, bind
               "mobile"),
  interval = 2,           # wait 2 seconds between the calls to API
  categories = c("performance", # run performance & accessibility
                 "accessibility"))

class(lh_df_3)
# [1] "data.frame"
dim(lh_df_3)
# [1]    4 1637



# check "Performance" and "Accessibility" for Google.com & Bing.com for
# both desktop & mobile and return in a data frame with even more data,
# including error occurences and the importance of each report result
lh_df_4 <- download_lighthouse(
  url = c("https://www.google.com",
          "https://www.bing.com/"),
  output_type = "simple", # return the results in a wide data frame
  strategy = c("desktop", # check both desktop and mobile, bind
               "mobile"),
  interval = 2,           # wait 2 seconds between the calls to API
  categories = c("performance", # run performance & accessibility
                 "accessibility"))



# another run for a messy long-like data frame
lh_df_4_long <- download_lighthouse(
  url = c("https://www.google.com",
          "https://www.bing.com/"),
  output_type = "simple", # return the results in a wide data frame
  strategy = c("desktop", # check both desktop and mobile, bind
               "mobile"),
  interval = 2,           # wait 2 seconds between the calls to API
  categories = c("performance", # run performance & accessibility
                 "accessibility"),
  long_result = TRUE) # spread into 4 columns

class(lh_df_4_long)
# [1] "data.frame"
dim(lh_df_4_long)
# 4 columns ("device" + "parameter" + pages values x2) and 1637 rows
# [1]    4 1637

```

### Lighthouse reports as nested lists (all data)

```r
# Lighthouse reports - Nested Lists -------------------------------------------                                           
# download nested list with "Performance" Lighthouse report for Google.com
lh_nl_1 <- download_lighthouse(url = "https://www.google.com", 
                               output_type = "raw")           # return nested list with all possible data

# check "Performance" for Google.com & Bing.com for both desktop & mobile and
# return in a nested list with all possible data
lh_nl_2 <- download_lighthouse(url = c("https://www.google.com", 
                                       "https://www.bing.com/"), 
                               output_type = "raw",           # return nested list with all possible data
                               strategy = c("desktop",        # check both desktop and mobile, bind
                                            "mobile"), 
                               interval = 1,                  # wait 1 second between the calls to API 
                               categories = "performance")    # which Lighthouse reports are to be run?

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
