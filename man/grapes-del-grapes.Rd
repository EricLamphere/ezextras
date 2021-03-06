% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{\%del\%}
\alias{\%del\%}
\title{Remove Patterns Within String}
\usage{
x \%del\% pattern
}
\arguments{
\item{x}{Character vector.}

\item{pattern}{Character string containing a regular expression to be matched
and removed from \code{x}.}
}
\value{
Character, \code{x} with \code{pattern} removed
}
\description{
\code{gsub} with \code{ignore.case = TRUE, perl = TRUE, fixed =
  FALSE}.
}
