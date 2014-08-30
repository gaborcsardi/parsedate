library(testthat)
library(parsedate)

if (Sys.getenv("_R_CHECK_TIMINGS_") == "") {
  test_check("parsedate")
}
