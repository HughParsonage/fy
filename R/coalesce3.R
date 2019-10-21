#' @noRd
#' @param x,y,... Integer vectors.
#' @details Same as coalesce(x, y, ...) but records anyNA value on exit via a private attribute
#' and evaluates ... lazily.

# integers guaranteed
coalesce3i <- function(x, y, ...) {
  if (!anyNA3i(x)) {
    setattr(x, name = "x_anyNA_3i", value = FALSE)
    return(x)
  }
  if (missing(y)) {
    return(x)
  }

  x[is.na(x)] <- y[is.na(x)]
  if (missing(..1)) {
    setattr(x, name = "x_anyNA_3i", value = anyNA(x))
    return(x)
  }
  coalesce3i(x, ...)
}

anyNA3i <- function(x) {
  o <- attr(x, "x_anyNA_3i")
  if (is.null(o)) {
    return(anyNA(x))
  }
  o
}
