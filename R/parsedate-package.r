## --------------------------------------------------------------------
#' Parse date from any format
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
  result <- parse_iso_8601(dates)
  result[is.na(result)] <- parse_git(dates[is.na(result)],
                                     approx = approx)
  result[is.na(result)] <- parse_rbase(dates[is.na(result)])
  result
}

## --------------------------------------------------------------------
#' Parse date from an ISO 8601 format
#'
#' @param dates A character vector containing the dates
#' @return A \code{POSIXct} vector.
#'
#' @export
#' @importFrom lubridate with_tz

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
    "|W(?<week>[0-4]\\d|5[0-2])(?<pmweekday>-?(?<weekday>[1-7]))?",
    "|(?<yearday>00[1-9]|0[1-9]\\d|[12]\\d{2}|3",
      "(?<yeardaytail>[0-5]\\d|6[1-6])))",
   "(?<time>[T\\s](?<hourminfrac>(?<hourmin>(?<hour>[01]\\d|2[0-3])",
            "(?<colonmin>(?<colon>:?)(?<min>[0-5]\\d))?|24\\:?00)",
           "(?<frac>[\\.,]\\d+(?!:))?)?",
    "(?<colonsec>\\g{colon}(?<sec>[0-5]\\d)(?<secfrac>[\\.,]\\d+)?)?",
    "(?<tz>[zZ]|(?<tzpm>[\\+-])",
     "(?<tzhour>[01]\\d|2[0-3]):?(?<tzmin>[0-5]\\d)?)?)?)?$"
  )

#' @importFrom lubridate parse_date_time "tz<-" minutes hours
#'   seconds milliseconds

parse_iso_single <- function(match) {
  parts <- structure(match[,3], names = rownames(match))
  ## Date first

  ## Years-days?
  date <- if (parts["yearday"] != "") {
    parse_date_time(paste(
      parts["year"],
      parts["yearday"]
      ), orders = "Y! j"
    )

  ## Years-weeks-days?
  } else if (parts["week"] != "" && parts["weekday"] != "") {
    iso_week(parts["year"], parts["week"], parts["weekday"])

  ## Years-weeks?
  } else if (parts["week"] != "") {
    iso_week(parts["year"], parts["week"], "1")

  ## Years-months-days
  } else if (parts["month"] != "" && parts["day"] != "") {
    parse_date_time(paste(
      parts["year"],
      parts["month"],
      parts["day"]
      ), orders = "Y! m*! d!"
    )

  ## Years-months
  } else if (parts["month"] != "") {
    parse_date_time(paste(
      parts["year"],
      parts["month"]
      ), orders = "Y! m*!"
    )

  ## Years
  } else {
    parse_date_time(paste(
      parts["year"]
      ), orders = "Y!"
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
    tz(date) <- "UTC"
  } else if (parts["tz"] != "") {
    if (parts["tz"] == "Z") {
      tz(date) <- "UTC"
    } else {
      tz(date) <- parts["tz"]
    }
  } else {
    tz(date) <- "UTC"
  }

  date
}

#' @importFrom lubridate ymd wday days weeks
NULL

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
#' @param date The date(s) for format
#' @param tz Time zone.
#' @export

format_iso_8601 <- function(date, tz = "UTC") {
  sub("(\\d\\d)$", ":\\1",
      format(with_tz(date, tz), "%Y-%m-%dT%H:%M:%S%z"))
}
