
context("parse_date")

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
    attr(parse_git("07-01-2010 12:13:!5", FALSE, "UTC"), "tzone"),
    "UTC")

  expect_equal(
    attr(parse_git("07-01-2010 12:13:!5", FALSE, "CET"), "tzone"),
    "UTC")

  expect_equal(
    attr(parse_rbase("2010-07-01", "UTC"), "tzone"),
    "UTC")

  expect_equal(
    attr(parse_rbase("2010-07-01", "CET"), "tzone"),
    "UTC")
})

test_that("input time zone is respected", {

})
