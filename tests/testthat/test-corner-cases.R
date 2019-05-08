
context("Dates and empty strings")

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
