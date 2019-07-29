# libraries -------------------------------------------------------------------
library(httr)
library(assertthat)
library(purrr)
library(jsonlite)
library(pagespeedParseR)
library(dplyr)
library(tidyr)
library(testthat)

# autoryzacja -----------------------------------------------------------------
Sys.setenv("PAGESPEED_API_KEY"      = "AIzaSyBM_1-9SZnHciE7accznf73MsCvSNXBOKM")
Sys.setenv("PAGESPEED_API_KEY_TEST" = "AIzaSyBM_1-9SZnHciE7accznf73MsCvSNXBOKM")
