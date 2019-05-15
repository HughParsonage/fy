expect_equal <- function(object, expected,
                         check.attributes = FALSE,
                         info = NULL,
                         label = NULL,
                         expected.label = NULL) {
  if (inherits(object, "fy") && is.character(expected)) {
    object <- as.character(object)
  }
  if (inherits(expected, "fy") && is.character(object)) {
    expected <- as.character(expected)
  }
  testthat::expect_equal(object, expected,
                         check.attributes = check.attributes,
                         info = info,
                         label = label,
                         expected.label = expected.label)
}


