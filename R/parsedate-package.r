## --------------------------------------------------------------------
#' Parse date from any format
#'
#' This package uses the date parser from the git project,
#' see http://git-scm.com/.
#'
#' @docType package
#' @name parsedate-package
#' @useDynLib parsedate

NULL

## --------------------------------------------------------------------
#' Parse date from any format
#'
#' @param dates A character vector.
#' @param approx Logical flag, whether to try harder. If this is
#'   set to \code{TRUE}, then the current time is used to fill
#'   in the missing parts of the date and time.
#' @return A \code{POSIXct} vector.
#'
#' @export

parse_date <- function(dates, approx = TRUE) {
  .Call("R_parse_date", dates, approx, PACKAGE = "parsedate")
}
