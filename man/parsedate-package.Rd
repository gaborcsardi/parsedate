% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parsedate-package.r
\docType{package}
\name{parsedate-package}
\alias{parsedate}
\alias{parsedate-package}
\title{Parse date from any format, including ISO 8601}
\description{
Three useful functions to parse and format dates.
\itemize{
  \item \code{\link{parse_iso_8601}} recognizes and parses all valid ISO
    8601 date and time formats. It can also be used as an ISO 8601
    validator.
  \item \code{\link{parse_date}} can parse a date when you don't know
    which format it is in. First it tries all ISO 8601 formats.
    Then it tries git's versatile date parser. Lastly, it tries
    \code{as.POSIXct}.
  \item \code{\link{format_iso_8601}} formats a date (and time) in
    a specific ISO 8601 format.
}
}
\seealso{
Useful links:
\itemize{
  \item \url{https://github.com/gaborcsardi/parsedate}
  \item Report bugs at \url{https://github.com/gaborcsardi/parsedate/issues}
}

}
