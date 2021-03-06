% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/method.R
\name{get_format}
\alias{get_format}
\title{Get the format that matches with the extension passed like parameter}
\usage{
get_format(ext)
}
\arguments{
\item{ext}{A string that contains the extension}
}
\value{
A string
}
\description{
Get the format that matches with the extension passed like parameter
}
\examples{
library(dataesgobr)
format <- get_format(".csv")
format <- get_format(".pdf")
}
