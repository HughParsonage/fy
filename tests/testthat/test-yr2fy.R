context("test-yr2fy")

test_that("yr2fy works", {
  expect_equal(yr2fy(2016), "2015-16", check.attributes = FALSE)
  expect_equal(yr2fy(2016:2017), c("2015-16", "2016-17"), check.attributes = FALSE)
})

test_that(".yr2fy", {
  expect_equal(yr2fy(2999, FALSE), .yr2fy(2999))
})

test_that("fy2yr works", {
  expect_error(fy2yr(c("2014-15", "2015-15")),
               regexp = "contains non-FYs",
               fixed = TRUE)
  expect_identical(fy2yr("2014-15"), 2015L)
  expect_equal(fy2yr(c("201415", "2015 16", "2015-16")),
               c(2015, 2016, 2016))
})

test_that("date2fy", {
  expect_equal(date2fy("2015-01-01"), "2014-15")
  expect_equal(date2fy(c("2015-01-01", "2015-06-30")), c("2014-15", "2014-15"))
  expect_equal(date2fy(as.Date(c("2015-01-01", "2015-06-30"))),
               c("2014-15", "2014-15"))
})

test_that("accelerator", {
  expect_equal(accel_repetitive_input(c("2015-04-04", "2017-04-04", "2016-04-04"),
                                      as.Date,
                                      THRESHOLD = 2L),
               accel_repetitive_input(c("2015-04-04", "2017-04-04", "2016-04-04"),
                                      as.Date,
                                      THRESHOLD = 5L))
  expect_equal(accel_repetitive_input(c("2015-04-04", "2017-04-04", "2016-04-04"),
                                      as.Date,
                                      THRESHOLD = 2L),
               as.Date(c("2015-04-04", "2017-04-04", "2016-04-04")))
})

test_that("yr2fy if given a list", {
  expect_error(yr2fy(list(x = 1, y = 2:3)),
               regexp = "atomic")
})


