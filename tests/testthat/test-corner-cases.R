
context("Dates and empty strings")

test_that("Bug #10 is fixed", {

  dates <- c("", "2014-10-29", "2012-12-05")
  pd <- parse_date(dates)
  pd2 <- c(parse_date(""),
           parse_date("2014-10-29"),
           parse_date("2012-12-05")
           )

  expect_equal(pd, pd2)
  
})

test_that("Empty strings are replaced by NAs", {

  dates <- c("", "", "")
  expected <- as.POSIXct(c(NA, NA, NA))
  actual <- c(parse_date(dates))

  expect_equal(expected, actual)
  
})

test_that("Approx parameter is passed on", {

  date <- "31.12."
  pd1 <- parse_date(date, approx = FALSE)
  pd2 <- parse_date(date, approx = TRUE)

  expect_true(is.na(pd1))
  expect_false(is.na(pd2))
})
