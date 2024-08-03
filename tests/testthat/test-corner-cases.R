# Dates and empty strings ----

test_that("Bug #10 is fixed", {

  dates <- c("", "2014-10-29", "2012-12-05")
  pd <- parse_date(dates)
  pd2 <- cdate(parse_date(""),
           parse_date("2014-10-29"),
           parse_date("2012-12-05")
           )

  expect_equal(pd, pd2)

})

test_that("Empty strings are replaced by NAs", {

  dates <- c("", "", "")
  expected <- .POSIXct(c(NA_real_, NA_real_, NA_real_), "UTC")
  actual <- parse_date(dates)

  expect_equal(expected, actual)

})

test_that("White-space string is replaced by NAs", {

  date <- " "
  expected <- .POSIXct(NA_real_, "UTC")
  actual <- parse_date(date)

  expect_equal(expected, actual)

})

test_that("White-spaces are gracefully removed", {

  date <- " 22.2.2222   "
  expected <- parse_date("22.2.2222")
  actual <- parse_date(date)

  expect_equal(expected, actual)

})


test_that("Non-sensical input is removed", {

  date <- "?=)(!$§#$%"
  expected <- .POSIXct(NA_real_, "UTC")
  actual <- parse_date(date)

  expect_equal(expected, actual)

})

test_that("Approx parameter is passed on", {

  date <- "31.12."
  pd1 <- parse_date(date, approx = FALSE)
  pd2 <- parse_date(date, approx = TRUE)

  expect_true(is.na(pd1))
  expect_false(is.na(pd2))
})

test_that("zero length input (issue #20)", {
  expect_identical(
    parse_date(character(0)),
    structure(numeric(0), class = c("POSIXct", "POSIXt"), tzone = "UTC"))

  expect_identical(
    parse_iso_8601(character(0)),
    structure(numeric(0), class = c("POSIXct", "POSIXt"), tzone = "UTC"))
})

test_that("multiple date formats do not cause a warning (issue #36)", {
  expect_silent(
    parse_iso_8601(c("2020-03", "2020"))
  )
  expect_equal(
    parse_iso_8601(c("2020-03", "2020")),
    as.POSIXct(c("2020-03-01", "2020-01-01"), tz = "UTC")
  )

  # a related set of tests that causes a similar issue to #36, but was not
  # reported in that issue.  This covers when minute and non-minute timezones
  # are provided at the same time.
  valid_datetimes <-
    c(
      "2024-08-03T01:02:03Z", # Zulu timezone
      "2024-08-03T01:02:03+00", # Zulu timezone as numeric
      "2024-08-03T01:02:03-04", # negative hour timezone without minutes
      "2024-08-03T01:02:03+04", # positive hour timezone without minutes
      "2024-08-03T01:02:03-04:00", # negative hour timezone with minutes
      "2024-08-03T01:02:03+04:15", # positive hour timezone with minutes
      "2024-08-03T01:02:03.123+04:15" # positive hour timezone with minutes and fractional seconds
    )
  expect_silent(
    parse_iso_8601(valid_datetimes)
  )
  expect_equal(
    parse_iso_8601(valid_datetimes),
    as.POSIXct(c("2024-08-03 01:02:03", "2024-08-03 01:02:03", "2024-08-02 21:02:03", "2024-08-03 05:02:03", "2024-08-02 21:02:03", "2024-08-02 20:47:03", "2024-08-02 20:47:03.123"), tz = "UTC")
  )
})
