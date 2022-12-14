% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/miniTools.R
\name{shy_lib}
\alias{shy_lib}
\title{Shy Lib}
\usage{
shy_lib(...)
}
\arguments{
\item{...}{Name of libraries to load}
}
\value{
a \code{list} of attached packages
}
\description{
shy_lib
Function to call packages without printing all the verbose
(only getting the essentials, like warning messages for example)
}
\examples{
\donttest{
   W4MRUtils::shy_lib("base", "utils")
}

}
\author{
M.Petera
}
