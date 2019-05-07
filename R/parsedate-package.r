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
#'     Then it tries git's versatile date parser. Lastly, it tries
#'     \code{as.POSIXct}.
#'   \item \code{\link{format_iso_8601}} formats a date (and time) in
#'     a specific ISO 8601 format.
#' }
#'
#' @docType package
#' @name parsedate-package
#' @useDynLib parsedate, .registration = TRUE, .fixes = "C_"
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
#' \code{parse_date} returns quickly in case of empty input elements.
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
#'
#' # Handle vectors and empty input
#' parse_date(c("2014","2015","","2016"))

parse_date <- function(dates, approx = TRUE) {
  result <- rep(as.POSIXct(NA), length.out = length(dates))
  dates <- trimws(dates)
  dates <- sapply(dates, replace.unparseable, USE.NAMES = FALSE)

  result[dates.to.parse(dates, result)] <- parse_iso_8601(dates[dates.to.parse(dates, result)])
  result[dates.to.parse(dates, result)] <- parse_git(dates[dates.to.parse(dates, result)],
                                     approx = approx)
  result[dates.to.parse(dates, result)] <- parse_rbase(dates[dates.to.parse(dates, result)])
  result
}

replace.unparseable <- function(date) {
  gsub(pattern = "[^ A-Za-z0-9:.-/]", replacement = "", date)
}

dates.to.parse <- function(dates, results) {
  dates != "" & is.na(results)
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
  match <- rematch2::re_match(dates, iso_regex)
  matching <- !is.na(match$.match)
  result <- parse_iso_parts(match)
  class(result) <- c("POSIXct", "POSIXt")
  with_tz(result, "UTC")
}

parse_iso_parts <- function(mm) {

  num <- nrow(mm)

  ## -----------------------------------------------------------------
  ## Date first

  date <- .POSIXct(rep(NA_real_, num))

  ## Years-days
  fyd <- is.na(date) & mm$yearday != ""
  date[fyd] <- yj(paste(mm$year[fyd], mm$yearday[fyd]))

  ## Years-weeks-days
  fywd <- is.na(date) & mm$week != "" & mm$weekday != ""
  date[fywd] <- iso_week(mm$year[fywd], mm$week[fywd], mm$weekday[fywd])

  ## Years-weeks
  fyw <- is.na(date) & mm$week != ""
  date[fyw] <- iso_week(mm$year[fyw], mm$week[fyw], "1")

  ## Years-months-days
  fymd <- is.na(date) & mm$month != "" & mm$day != ""
  date[fymd] <- ymd(paste(mm$year[fymd], mm$month[fymd], mm$day[fymd]))

  ## Years-months
  fym <- is.na(date) & mm$month != ""
  date[fym] <- ymd(paste(mm$year[fym], mm$month[fym], "01"))

  ## Years
  fy <- is.na(date)
  date[fy] <- ymd(paste(mm$year, "01", "01"))

  ## -----------------------------------------------------------------
  ## Now the time

  th <- mm$hour != ""
  date[th] <- date[th] + hours(mm$hour[th])

  tm <- mm$min != ""
  date[tm] <- date[tm] + minutes(mm$min[tm])

  ts <- mm$sec != ""
  date[ts] <- date[ts] + seconds(mm$sec[ts])

  ## -----------------------------------------------------------------
  ## Fractional time

  frac <- as.numeric(sub(",", ".", mm$frac))

  tfs <- !is.na(frac) & mm$sec != ""
  date[tfs] <- date[tfs] + milliseconds(round(frac[tfs] * 1000))

  tfm <- !is.na(frac) & mm$sec == "" & mm$min != ""
  sec <- trunc(frac[tfm] * 60)
  mil <- round((frac[tfm] * 60 - sec) * 1000)
  date[tfm] <- date[tfm] + seconds(sec) + milliseconds(mil)

  tfh <- !is.na(frac) & mm$sec == "" & mm$min == ""
  min <- trunc(frac[tfh] * 60)
  sec <- trunc((frac[tfh] * 60 - min) * 60)
  mil <- round((((frac[tfh] * 60) - min) * 60 - sec) * 1000)
  date[tfh] <- date[tfh] + minutes(min) + seconds(sec) + milliseconds(mil)

  ## -----------------------------------------------------------------
  ## Time zone

  ftzpm <- mm$tzpm != ""
  m <- ifelse(mm$tzpm[ftzpm] == "+", -1, 1)
  ftzpmh <- ftzpm & mm$tzhour != ""
  date[ftzpmh] <- date[ftzpmh] + m * hours(mm$tzhour[ftzpmh])
  ftzpmm <- ftzpm & mm$tzmin != ""
  date[ftzpmm] <- date[ftzpmm] + m * minutes(mm$tzmin[ftzpmm])

  ftzz <- mm$tz == "Z"
  date[ftzz] <- as.POSIXct(date[ftzz], "UTC")

  ftz <- mm$tz != "Z" & mm$tz != ""
  date[ftz] <- as.POSIXct(date[ftz], mm$tz[ftz])

  as.POSIXct(date, "UTC")
}

iso_regex <- paste0(
  "^\\s*",
  "(?<year>[\\+-]?\\d{4}(?!\\d{2}\\b))",
  "(?:(?<dash>-?)",
   "(?:(?<month>0[1-9]|1[0-2])",
    "(?:\\g{dash}(?<day>[12]\\d|0[1-9]|3[01]))?",
    "|W(?<week>[0-4]\\d|5[0-3])(?:-?(?<weekday>[1-7]))?",
    "|(?<yearday>00[1-9]|0[1-9]\\d|[12]\\d{2}|3",
      "(?:[0-5]\\d|6[1-6])))",
   "(?<time>[T\\s](?:(?:(?<hour>[01]\\d|2[0-3])",
            "(?:(?<colon>:?)(?<min>[0-5]\\d))?|24\\:?00)",
           "(?<frac>[\\.,]\\d+(?!:))?)?",
    "(?:\\g{colon}(?<sec>[0-5]\\d)(?:[\\.,]\\d+)?)?",
    "(?<tz>[zZ]|(?<tzpm>[\\+-])",
     "(?<tzhour>[01]\\d|2[0-3]):?(?<tzmin>[0-5]\\d)?)?)?)?$"
  )

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
  .Call(C_R_parse_date, dates, approx)
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
