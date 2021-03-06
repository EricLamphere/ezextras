% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{\%&\%}
\alias{\%&\%}
\title{Concatenate Strings}
\usage{
x \%&\% y
}
\arguments{
\item{x}{First R object to be converted to a character vector and
concatenated to the left of \code{y}.}

\item{y}{Second R object to be converted to a character vector and
concatenated to the right of \code{x}.}
}
\value{
Character, concatenated strings \code{x} and \code{y}, less NAs. See
\code{paste} documentation for more information.
}
\description{
Concatenate vectors after converting to character and ignoring
NAs.
}
