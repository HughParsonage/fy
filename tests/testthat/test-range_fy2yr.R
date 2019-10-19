context("test-range_fy2yr")

test_that("range_fy2yr works", {
  expect_equal(min_fy2yr("2015-16"), 2016L)
  expect_equal(max_fy2yr("2015-16"), 2016L)
  x <- yr2fy(2015:2016)
  expect_true(all(range_fy2yr(x) == 2015:2016))
  # With the attributes
  expect_true(all(range_fy2yr(x) == 2015:2016))
})

test_that("range verbose", {
  skip_on_cran()
  skip_if_not_installed("rlang")
  rlang::with_options({
    fya <- yr2fy(1980:2015)
    expect_output(range(fya), regexp = "range\\.fy")
  },
  verbose = TRUE)
})

test_that("range_fy2yr if given a list", {
  expect_error(range_fy2yr(list(x = 1, y = 2:3)),
               regexp = "atomic")
})

