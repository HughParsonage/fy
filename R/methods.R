#' @export
"+.fy" <- function(x, h) {
  if (length(h) == 1L || length(h) == length(x)) {
    if (is.integer(h)) {
      out <- yr2fy(fy2yr(x) + h)
    } else if (is.double(h)) {
      if (!all(h == as.integer(h), na.rm = TRUE)) {
        warning("`h = ", deparse(substitute(h)), " was not integerish.")
        h <- as.integer(h)
      }
      out <- yr2fy(fy2yr(x) + h)
    } else {
      stop("non-numeric to arithmetic operation on fy")
    }
  } else {
    stop("`length(h) = ", length(h), "`, yet the length of `x` is ",
         length(x), "`.")
  }
}

#' @export
"-.fy" <- function(x, h) {
  if (length(h) == 1L || length(h) == length(x)) {
    if (is.integer(h)) {
      out <- yr2fy(fy2yr(x) - h)
    } else if (is.double(h)) {
      if (!all(h == as.integer(h), na.rm = TRUE)) {
        warning("`h = ", deparse(substitute(h)), " was not integerish.")
        h <- as.integer(h)
      }
      out <- yr2fy(fy2yr(x) - h)
    } else {
      stop("non-numeric to arithmetic operation on fy")
    }
  } else {
    stop("`length(h) = ", length(h), "`, yet the length of `x` is ",
         length(x), "`.")
  }
}
