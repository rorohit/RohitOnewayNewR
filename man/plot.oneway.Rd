% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/Rohit_Oneway.R
\name{plot.oneway}
\alias{plot.oneway}
\title{Plot method for the summary method.}
\usage{
\method{plot}{oneway}(x, ...)
}
\arguments{
\item{x}{an object of class \code{"oneway"}, i.e., a fitted model.}

\item{\dots}{not used.}
}
\description{
Plot method for the summary method.
}
\examples{
{
library(faraway)
data(coagulation)
DefaultData <- RohitOnewayNewR:::oneway.default(unstack(coagulation))
RohitOnewayNewR:::plot.oneway(DefaultData)
}
}

