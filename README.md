# pagespeedParseR  v0.2.3
![Lifecycle_Status](https://img.shields.io/badge/lifecycle-maturing-blue.svg)
[![Travis_CI_Build_Status](https://travis-ci.org/Leszek-Sieminski/pagespeedParseR.svg?branch=master)](https://travis-ci.org/Leszek-Sieminski/pagespeedParseR)

R wrapper for Google Pagespeed Insights API

* [What is Google Pagespeed Insights?](#what-is-google-pagespeed-insights)
* [Other Pagespeed API packages in R](#other-pagespeed-packages-in-r)
* [Why pagespeedParseR when there are other packages?](#but-why-another-r-package-for-pagespeed-api)
* [Features](#features)
* [Acquiring API access token](#acquiring-api-access-token)
* [Installation](#installation)
* [Authentication](#authentication)
* [Usage](#usage)

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
* [IN PROGRESS] Check every error address and influence in Lighthouse reports ('enhanced_lighthouse' parameter)

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

### Lighthouse reports as wide data frames (most important data)

```r
# Lighthouse reports - Data Frames --------------------------------------------
# download simple data frame with "Performance" Lighthouse report for Google.com
lh_df_1 <- download_lighthouse(url = "https://www.google.com", 
                             output_type = "simple")          # return the results in a wide data frame

# check "Performance" for Google.com & Bing.com for both desktop & mobile and
# return in a data frame with most important columns
lh_df_2 <- download_lighthouse(url = c("https://www.google.com", 
                                       "https://www.bing.com/"), 
                               output_type = "simple",        # return the results in a wide data frame
                               strategy = c("desktop",        # check both desktop and mobile, bind
                                            "mobile"), 
                               interval = 1,                  # wait 1 second between the calls to API 
                               categories = "performance")    # which Lighthouse reports are to be run?

# check "Performance" and "Accessibility" for Google.com & Bing.com for 
# both desktop & mobile and return in a data frame with most important columns
lh_df_3 <- download_lighthouse(url = c("https://www.google.com", 
                                       "https://www.bing.com/"), 
                               output_type = "simple",          # return the results in a wide data frame
                               strategy = c("desktop",          # check both desktop and mobile, bind
                                            "mobile"), 
                               interval = 2,                    # wait 2 seconds between the calls to API 
                               categories = c("performance",    # run performance & accessibility... 
                                              "accessibility")) # ... Lighthouse reports


# check "Performance" and "Accessibility" for Google.com & Bing.com for 
# both desktop & mobile and return in a data frame with even more data,
# including error occurences and the importance of each report result
lh_df_4 <- download_lighthouse(url = c("https://www.google.com", 
                                       "https://www.bing.com/"), 
                               output_type = "simple",          # return the results in a wide data frame
                               strategy = c("desktop",          # check both desktop and mobile, bind
                                            "mobile"), 
                               interval = 2,                    # wait 2 seconds between the calls to API 
                               enhanced_lighthouse = TRUE       # set to TRUE to obtain more data about errors
                               categories = c("performance",    # run performance & accessibility... 
                                              "accessibility")) # ... Lighthouse reports
       
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
