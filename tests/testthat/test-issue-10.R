
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
