% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lh_raw_2_vec.R
\name{lh_raw_2_vec}
\alias{lh_raw_2_vec}
\title{Download Pagespeed v5 raport (Lighthouse) for multiple URLs as a one nested list}
\usage{
lh_raw_2_vec(url, key = Sys.getenv("PAGESPEED_API_KEY"),
  strategy = NULL, categories = "performance", interval = 0.5,
  locale = NULL, utm_campaign = NULL, utm_source = NULL)
}
\arguments{
\item{url}{vector of character strings. The URLs to fetch and analyze}

\item{key}{string. Pagespeed API key to authenticate. Defaults to
"PAGESPEED_API_KEY" enviroment variable.}

\item{strategy}{string. The analysis strategy to use. Options: "desktop" or
"mobile". Defaults to "desktop"}

\item{categories}{string. A Lighthouse category/categories to run.
Defaults to "performance". See more in Details section}

\item{interval}{numeric. Number of seconds to wait between multiple queries.
Defaults to 0.5 second.}

\item{locale}{string. The locale used to localize formatted results}

\item{utm_campaign}{string. Campaign name for analytics. Defaults to NULL}

\item{utm_source}{string. Campaign source for analytics. Defaults to NULL}
}
\value{
unformatted nested list
}
\description{
This function can check multiple URLs (character vector)
   and parse the output into a data frame. This data frame contain all
   the possible information from Pagespeed ver 5.
}
\details{
This function uses legacy version 4 of the API.
   Check function \code{lh_raw_2_vec()} for version 5.
   If you need less information but in form of a data frame,
   use \code{ps_simple_2_vec()}.
}
\examples{
\dontrun{
multiple_urls_raw_output <- lh_raw_2_vec("https://www.google.com/")
}
}
