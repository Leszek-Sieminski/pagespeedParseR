% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auth.R
\name{auth_pagespeed}
\alias{auth_pagespeed}
\title{Authorize your Pagespeed Insights API with a API Key (Token)}
\usage{
auth_pagespeed(api_key, verbose = TRUE)
}
\arguments{
\item{api_key}{string. Token acquired on
https://console.developers.google.com/ page.}

\item{verbose}{logical. Defaults to TRUE. Set to FALSE to stop printing
status in the console}
}
\value{
invisibly returns API token into environment variable
    PAGESPEED_API_KEY and prints the status
}
\description{
Authorize your Pagespeed Insights API with a API Key (Token)
}
\examples{
\dontrun{
auth_pagespeed("12345")
}
}
