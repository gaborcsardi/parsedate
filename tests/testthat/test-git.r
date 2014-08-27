
context("Git dates")

test_that("Filling in is correct", {

  current_year <- lubridate::year(lubridate::today())

  expect_equal(format_iso_8601(parse_date("8/21")),
               paste0(current_year, "-08-21T00:00:00+00:00"))

  expect_equal(format_iso_8601(parse_date("8/21 15:50")),
               paste0(current_year, "-08-21T15:50:00+00:00"))

})
