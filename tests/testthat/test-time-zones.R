# time zones ----

test_that("output time zone is always UTC", {
  expect_equal(
    attr(parse_date("2010-07-01"), "tzone"),
    "UTC")

  expect_equal(
    attr(parse_date("2010-07-01", default_tz = "CET"), "tzone"),
    "UTC")

  expect_equal(
    attr(parse_iso_8601("2010-07-01"), "tzone"),
    "UTC")

  expect_equal(
    attr(parse_iso_8601("2010-07-01", default_tz = "CET"), "tzone"),
    "UTC")

  expect_equal(
    attr(parse_git("07-01-2010 12:13:15", FALSE, "UTC"), "tzone"),
    "UTC")

  expect_equal(
    attr(parse_git("07-01-2010 12:13:15", FALSE, "CET"), "tzone"),
    "UTC")

  expect_equal(
    attr(parse_rbase("2010-07-01", "UTC"), "tzone"),
    "UTC")

  expect_equal(
    attr(parse_rbase("2010-07-01", "CET"), "tzone"),
    "UTC")
})

test_that("input time zone is respected (local CET)", {
  withr::local_timezone("CET")

  d <- parse_iso_8601("2010-07-01", default_tz = "CET")
  expect_equal(d, .POSIXct(as.POSIXct("2010-07-01", "CET"), "UTC"))

  d <- parse_git("2010-07-01 00:00:00", FALSE, default_tz = "CET")
  expect_equal(d, .POSIXct(as.POSIXct("2010-07-01", "CET"), "UTC"))

  d <- parse_rbase("2010-07-01", default_tz = "CET")
  expect_equal(d, .POSIXct(as.POSIXct("2010-07-01", "CET"), "UTC"))

  d <- parse_date("2010-07-01", default_tz = "US/Pacific")
  expect_equal(d, .POSIXct(as.POSIXct("2010-07-01", "US/Pacific"), "UTC"))

  d <- parse_iso_8601("2010-07-01", default_tz = "US/Pacific")
  expect_equal(d, .POSIXct(as.POSIXct("2010-07-01", "US/Pacific"), "UTC"))

  d <- parse_git("2010-07-01 00:00:00", FALSE, default_tz = "US/Pacific")
  expect_equal(d, .POSIXct(as.POSIXct("2010-07-01", "US/Pacific"), "UTC"))

  d <- parse_rbase("2010-07-01", default_tz = "US/Pacific")
  expect_equal(d, .POSIXct(as.POSIXct("2010-07-01", "US/Pacific"), "UTC"))
})

test_that("input time zone is respected (local US/Pacific)", {
  withr::local_timezone("US/Pacific")

  d <- parse_iso_8601("2010-07-01", default_tz = "CET")
  expect_equal(d, .POSIXct(as.POSIXct("2010-07-01", "CET"), "UTC"))

  d <- parse_git("2010-07-01 00:00:00", FALSE, default_tz = "CET")
  expect_equal(d, .POSIXct(as.POSIXct("2010-07-01", "CET"), "UTC"))

  d <- parse_rbase("2010-07-01", default_tz = "CET")
  expect_equal(d, .POSIXct(as.POSIXct("2010-07-01", "CET"), "UTC"))

  d <- parse_date("2010-07-01", default_tz = "US/Pacific")
  expect_equal(d, .POSIXct(as.POSIXct("2010-07-01", "US/Pacific"), "UTC"))

  d <- parse_iso_8601("2010-07-01", default_tz = "US/Pacific")
  expect_equal(d, .POSIXct(as.POSIXct("2010-07-01", "US/Pacific"), "UTC"))

  d <- parse_git("2010-07-01 00:00:00", FALSE, default_tz = "US/Pacific")
  expect_equal(d, .POSIXct(as.POSIXct("2010-07-01", "US/Pacific"), "UTC"))

  d <- parse_rbase("2010-07-01", default_tz = "US/Pacific")
  expect_equal(d, .POSIXct(as.POSIXct("2010-07-01", "US/Pacific"), "UTC"))
})

test_that("empty default time zone is the local time zone (CET)", {
  withr::local_timezone("CET")

  d <- parse_date("2010-07-01", default_tz = "")
  expect_equal(d, .POSIXct(as.POSIXct("2010-07-01", "CET"), "UTC"))

  d <- parse_iso_8601("2010-07-01", default_tz = "")
  expect_equal(d, .POSIXct(as.POSIXct("2010-07-01", "CET"), "UTC"))

  d <- parse_git("2010-07-01 00:00:00", FALSE, default_tz = "")
  expect_equal(d, .POSIXct(as.POSIXct("2010-07-01", "CET"), "UTC"))

  d <- parse_rbase("2010-07-01", default_tz = "")
  expect_equal(d, .POSIXct(as.POSIXct("2010-07-01", "CET"), "UTC"))

  d <- parse_date("2010-07-01", default_tz = "CET")
  expect_equal(d, .POSIXct(as.POSIXct("2010-07-01", "CET"), "UTC"))
})

test_that("empty default time zone is the local time zone (US/Pacific)", {
  withr::local_timezone("US/Pacific")

  d <- parse_date("2010-07-01", default_tz = "")
  expect_equal(d, .POSIXct(as.POSIXct("2010-07-01", "US/Pacific"), "UTC"))

  d <- parse_iso_8601("2010-07-01", default_tz = "")
  expect_equal(d, .POSIXct(as.POSIXct("2010-07-01", "US/Pacific"), "UTC"))

  d <- parse_git("2010-07-01 00:00:00", FALSE, default_tz = "")
  expect_equal(d, .POSIXct(as.POSIXct("2010-07-01", "US/Pacific"), "UTC"))

  d <- parse_rbase("2010-07-01", default_tz = "")
  expect_equal(d, .POSIXct(as.POSIXct("2010-07-01", "US/Pacific"), "UTC"))
})

test_that("mixing explicit and default time zones", {
  withr::local_timezone("CET")

  exp <- .POSIXct(as.POSIXct("2010-07-01", "%Y-%m-%d", tz = "CET"), "UTC")
  d <- parse_date(
    c("2010-07-01", "2010-06-30T22:00:00Z", "2010-07-01TCET",
      "2010-06-30T16:00:00-06:00"),
    default_tz = "")
  expect_equal(d, rep(exp, 4))
})
