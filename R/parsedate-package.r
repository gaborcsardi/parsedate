## --------------------------------------------------------------------
#' Parse date from any format, including ISO 8601
#'
#' Three useful functions to parse and format dates.
#' \itemize{
#'   \item \code{\link{parse_iso_8601}} recognizes and parses all valid ISO
#'     8601 date and time formats. It can also be used as an ISO 8601
#'     validator.
#'   \item \code{\link{parse_date}} can parse a date when you don't know
#'     which format it is in. First it tries all ISO 8601 formats.
#'     Then it tries git's versatize date parser. Lastly, it tries
#'     \code{as.POSIXct}.
#'   \item \code{\link{format_iso_8601}} formats a date (and time) in
#'     a specific ISO 8601 format.
#' }
#'
#' @docType package
#' @name parsedate-package
#' @useDynLib parsedate
#' @importFrom methods reconcilePropertiesAndPrototype

NULL

## Some simple utility functions. We used to take them from lubridate,
## but that brings in plyr, Rcpp, etc. Better to keep dependencies light.
## These are of course not general replacements for lubridate functions,
## but they suffice for our purposes.

milliseconds <- function(x) as.difftime(as.numeric(x) / 1000, units = "secs")
seconds <- function(x) as.difftime(as.numeric(x), units = "secs")
minutes <- function(x) as.difftime(as.numeric(x), units = "mins")
hours <- function(x) as.difftime(as.numeric(x), units = "hours")
days <- function(x) as.difftime(as.numeric(x), units = "days")
weeks <- function(x) as.difftime(as.numeric(x), units = "weeks")
wday <- function(x) as.POSIXlt(x, tz = "UTC")$wday + 1
with_tz <- function(x, tzone = "") as.POSIXct(as.POSIXlt(x, tz = tzone))
ymd <- function(x) as.POSIXct(x, format = "%Y %m %d", tz = "UTC")
yj <- function(x) as.POSIXct(x, format = "%Y %j", tz = "UTC")

## --------------------------------------------------------------------
#' Parse date from any format
#'
#' Recognize and parse dates from a wide range of formats. The current
#' algorithm is the following:
#' \enumerate{
#'   \item Try parsing dates using all valid ISO 8601 formats, by
#'     calling \code{\link{parse_iso_8601}}.
#'   \item If this fails, then try parsing them using the git
#'     date parser.
#'   \item If this fails, then try parsing them using \code{as.POSIXct}.
#'     (It is unlikely that this step will parse any dates that the
#'     first two steps couldn't, but it is still a logical fallback,
#'     to make sure that we can parse at least as many dates as
#'     \code{as.POSIXct}.
#' }
#'
#' @param dates A character vector. An error is reported if
#'   the function cannot coerce this parameter to a character vector.
#' @param approx Logical flag, whether the git parse should try
#'   hard(er). If this is set to \code{TRUE}, then the current time is used
#'   to fill in the missing parts of the date and time.
#' @return A \code{POSIXct} vector. \code{NA} is returned for
#'   the dates that \code{parse_date} could not parse.
#'
#' @export
#' @examples
#' # Some easy examples
#' parse_date("2014-12-12")
#' parse_date("04/15/99")
#' parse_date("15/04/99")
#'
#' # Ambiguous format, parsed assuming MM/DD/YY
#' parse_date("12/11/99")
#' parse_date("11/12/99")
#'
#' # Fill in the current date and time
#' parse_date("03/20")
#' parse_date("12")
#'
#' # But not for this, because this is ISO 8601
#' parse_date("2014")

parse_date <- function(dates, approx = TRUE) {
  result <- parse_iso_8601(dates)
  result[is.na(result)] <- parse_git(dates[is.na(result)],
                                     approx = approx)
  result[is.na(result)] <- parse_rbase(dates[is.na(result)])
  result
}

## --------------------------------------------------------------------
#' Parse date from an ISO 8601 format
#'
#' See \url{http://en.wikipedia.org/wiki/ISO_8601} and links therein
#' for the complete standard.
#'
#' @param dates A character vector. An error is reported if
#'   the function cannot coerce this parameter to a character vector.
#' @return A \code{POSIXct} vector. \code{NA} is returned for
#'   the dates that \code{parse_date} could not parse.
#'
#' @export
#' @examples
#' # Missing fields
#' parse_iso_8601("2013-02-08 09")
#' parse_iso_8601("2013-02-08 09:30")
#'
#' # Separator between date and time can be a 'T'
#' parse_iso_8601("2013-02-08T09")
#' parse_iso_8601("2013-02-08T09:30")
#' parse_iso_8601("2013-02-08T09:30:26")
#'
#' # Fractional seconds, minutes, hours
#' parse_iso_8601("2013-02-08T09:30:26.123")
#' parse_iso_8601("2013-02-08T09:30.5")
#' parse_iso_8601("2013-02-08T09,25")
#'
#' # Zulu time zone is UTC
#' parse_iso_8601("2013-02-08T09:30:26Z")
#'
#' # ISO weeks, not very intuitive
#' parse_iso_8601("2013-W06-5")
#' parse_iso_8601("2013-W01-1")
#' parse_iso_8601("2009-W01-1")
#' parse_iso_8601("2009-W53-7")
#'
#' # Day of the year
#' parse_iso_8601("2013-039")
#' parse_iso_8601("2013-039 09:30:26Z")

parse_iso_8601 <- function(dates) {
  dates <- as.character(dates)
  match <- regexpr(iso_regex, dates, perl = TRUE)
  matching <- sapply(match, function(x)
    ! identical(x, -1L) && ! identical(x, -1))
  match_list <- regexp_to_df(dates, match)
  result <- rep(NA_real_, length(dates))
  result[matching] <- sapply(match_list, parse_iso_single)

  result <- unlist(result)
  class(result) <- c("POSIXct", "POSIXt")
  with_tz(result, "UTC")
}

regexp_to_df <- function(text, match) {
  positive <- match != -1
  g_text <- text[positive]
  g_start <- attr(match, "capture.start")[positive, , drop = FALSE]
  g_length <- attr(match, "capture.length")[positive, , drop = FALSE]

  lapply(seq_len(sum(positive)), function(i) {
    data.frame(start = g_start[i,],
               length = g_length[i,],
               match = substring(text[i], g_start[i,],
                 g_start[i,] + g_length[i,] - 1),
               stringsAsFactors = FALSE)
  })
}

iso_regex <- paste0(
  "^\\s*",
  "(?<year>[\\+-]?\\d{4}(?!\\d{2}\\b))",
  "(?<restafteryear>(?<dash>-?)",
   "(?<dateafteryear>(?<month>0[1-9]|1[0-2])",
    "(?<dashandday>\\g{dash}(?<day>[12]\\d|0[1-9]|3[01]))?",
    "|W(?<week>[0-4]\\d|5[0-3])(?<pmweekday>-?(?<weekday>[1-7]))?",
    "|(?<yearday>00[1-9]|0[1-9]\\d|[12]\\d{2}|3",
      "(?<yeardaytail>[0-5]\\d|6[1-6])))",
   "(?<time>[T\\s](?<hourminfrac>(?<hourmin>(?<hour>[01]\\d|2[0-3])",
            "(?<colonmin>(?<colon>:?)(?<min>[0-5]\\d))?|24\\:?00)",
           "(?<frac>[\\.,]\\d+(?!:))?)?",
    "(?<colonsec>\\g{colon}(?<sec>[0-5]\\d)(?<secfrac>[\\.,]\\d+)?)?",
    "(?<tz>[zZ]|(?<tzpm>[\\+-])",
     "(?<tzhour>[01]\\d|2[0-3]):?(?<tzmin>[0-5]\\d)?)?)?)?$"
  )

parse_iso_single <- function(match) {
  parts <- structure(match[,3], names = rownames(match))
  ## Date first

  ## Years-days?
  date <- if (parts["yearday"] != "") {
    yj(paste(
      parts["year"],
      parts["yearday"]
      )
    )

  ## Years-weeks-days?
  } else if (parts["week"] != "" && parts["weekday"] != "") {
    iso_week(parts["year"], parts["week"], parts["weekday"])

  ## Years-weeks?
  } else if (parts["week"] != "") {
    iso_week(parts["year"], parts["week"], "1")

  ## Years-months-days
  } else if (parts["month"] != "" && parts["day"] != "") {
    ymd(paste(
      parts["year"],
      parts["month"],
      parts["day"]
      )
    )

  ## Years-months
  } else if (parts["month"] != "") {
    ymd(paste(
      parts["year"],
      parts["month"],
      "01"
      )
    )

  ## Years
  } else {
    ymd(paste(
      parts["year"],
      "01",
      "01"
      )
    )
  }

  ## Add the rest
  if (parts["hour"] != "") { date <- date + hours(parts["hour"]) }
  if (parts["min"] != "") { date <- date + minutes(parts["min"]) }
  if (parts["sec"] != "") { date <- date + seconds(parts["sec"]) }

  ## Fractional time
  if (parts["frac"] != "") {
    frac <- as.numeric(sub(",", ".", parts["frac"]))
    if (parts["sec"] != "") {
      ## fractions of a second
      date <- date + milliseconds(round(frac * 1000))
    } else if (parts["min"] != "") {
      ## fractions of a minute
      sec <- trunc(frac * 60)
      milli <- round((frac * 60 - sec) * 1000)
      date <- date + seconds(sec) + milliseconds(milli)
    } else {
      ## fractions of an hour
      min <- trunc(frac * 60)
      sec <- trunc((frac * 60 - min) * 60)
      milli <- round((((frac * 60) - min) * 60 - sec) * 1000)
      date <- date + minutes(min) + seconds(sec) + milliseconds(milli)
    }
  }

  ## Set time zone
  if (parts["tzpm"] != "") {
    m <- if (parts["tzpm"] == "+") -1 else 1
    if (parts["tzhour"] != "") { date <- date + m * hours(parts["tzhour"]) }
    if (parts["tzmin"] != "") { date <- date + m * minutes(parts["tzmin"]) }
    date <- as.POSIXct(date, "UTC")
  } else if (parts["tz"] != "") {
    if (parts["tz"] == "Z") {
      date <- as.POSIXct(date, "UTC")
    } else {
      date <- as.POSIXct(date, parts["tz"])
    }
  } else {
    date <- as.POSIXct(date, "UTC")
  }

  date
}

iso_week <- function(year, week, weekday) {

  wdmon <- function(date) { (wday(date) + 5L) %% 7L }
  thu <- function(date) { date - days(wdmon(date) - 3L) }

  thu(ymd(paste(year, "01", "04"))) + weeks(as.numeric(week) - 1L) +
    days(as.numeric(weekday) - 4L)
}

parse_rbase <- function(dates) {
  result <- lapply(dates, function(x) { try(as.POSIXct(x), silent = TRUE) })
  bad <- vapply(result, inherits, "try-error", FUN.VALUE = TRUE)
  result[bad] <- NA
  unlist(result)
}

parse_git <- function(dates, approx) {
  .Call("R_parse_date", dates, approx, PACKAGE="parsedate")
}

## --------------------------------------------------------------------
#' Format date and time according to ISO 8601
#'
#' Format a date in a fixed format that is ISO 8601 valid, and
#' can be used to compare dates as character strings. It converts
#' the date(s) to UTC.
#'
#' @param date The date(s) to format.
#' @return Character vector of formatted dates.
#'
#' @export
#' @examples
#' format_iso_8601(parse_iso_8601("2013-02-08"))
#' format_iso_8601(parse_iso_8601("2013-02-08 09:34:00"))
#' format_iso_8601(parse_iso_8601("2013-02-08 09:34:00+01:00"))
#' format_iso_8601(parse_iso_8601("2013-W06-5"))
#' format_iso_8601(parse_iso_8601("2013-039"))

format_iso_8601 <- function(date) {
  format(as.POSIXlt(date, tz = "UTC"), "%Y-%m-%dT%H:%M:%S+00:00")
}
