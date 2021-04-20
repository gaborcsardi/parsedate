library(testthat)
library(parsedate)

test_check("parsedate")

run <- function(tz) {
  source("testthat/helper.R")
  withr::local_timezone(tz)
  test_check("parsedate")
}

run("CET")

run("US/Pacific")
