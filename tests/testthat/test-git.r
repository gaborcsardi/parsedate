
context("Git dates")

test_that("Filling in is correct", {

  current_year <- lubridate::year(lubridate::today())

  expect_equal(format_iso_8601(parse_date("8/21")),
               paste0(current_year, "-08-21T00:00:00+00:00"))

  expect_equal(format_iso_8601(parse_date("8/21 15:50")),
               paste0(current_year, "-08-21T15:50:00+00:00"))

})

test_that("Issue #3 is resolved", {

  d1 <- parse_date("070809")
  d2 <- parse_date("07-08-09")
  expect_equal(d1, d2)

  d1 <- parse_date("20071211")
  d2 <- parse_date("2007-12-11")
  expect_equal(d1, d2)

  d1 <- parse_date("070809 14:35:42")
  d2 <- parse_date("07-08-09 14:35:42")
  expect_equal(d1, d2)

  d1 <- parse_date("20070809 14:35:42")
  d2 <- parse_date("2007-08-09 14:35:42")
  expect_equal(d1, d2)
  
})

test_that("Issue #4 is resolved", {

  d1 <- parse_date("000102")
  expect_equal(d1, as.POSIXct(NA))

})

test_that("Issue #5 is resolved", {

  d1 <- parse_date("010203")
  d2 <- parse_date("2003-01-02")
  expect_equal(d1, d2)

  d1 <- parse_date("070203")
  d2 <- parse_date("2003-07-02")
  expect_equal(d1, d2)

  d1 <- parse_date("010203 15:56:25")
  d2 <- parse_date("2003-01-02 15:56:25")
  expect_equal(d1, d2)

  d1 <- parse_date("070203 15:56:25")
  d2 <- parse_date("2003-07-02 15:56:25")
  expect_equal(d1, d2)

})
