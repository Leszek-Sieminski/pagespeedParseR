# pagespeedParseR
R wrapper for Google Pagespeed Insights API

## Currently under development! Wait for stable version

## What is Google Pagespeed Insights?
It's an online tool that identifies performance issues for a given URL and provides suggestions and optimizations.

## Other Pagespeed packages in R
There are multiple other R packages for Pagespeed that I find very valuable:
* googlePageSpeedR - [Phippsy/googlePageSpeedR](https://github.com/Phippsy/googlePageSpeedR)
* gpagespeed - [simitpatel/gpagespeed](https://github.com/simitpatel/gpagespeed)
* pagespeed - [mhairi/pagespeed](https://github.com/mhairi/pagespeed)

## But why another R package for Pagespeed API?
Parsing **API response returns nested list object** in R which isn't very convenient to extract data from (at least in R), so the outputs of above packages' functions are limited (doesn't contain recommendations). The goal of pagespeedParseR is to **create simple output in form of data frame** which contains both performance scores and most of recommendations. Alternatively, **user can decide to obtain the output in raw form of nested list** and parse it by himself/herself if pagespeedParseR parsing function isn't enough.

## Features
* Authenticate with an API key
* Query multiple URLs in one function call
* Find out which URLs weren't analyzed if error happened - output object keeps information about pages that didn't create the report
* Query every page for both Desktop and Mobile in one function call
* Control API limits usage with simple interval mechanism
* [IN PROGRESS] Download data for two API versions (4th = ready, 5th = under development)
* Choose data format: simple (conveniently parsed data frame with most important info) or raw (nested list with all the data provided by the API)
* Saving the output as the tmp.Rdata files in case of trouble

## To be continued...
