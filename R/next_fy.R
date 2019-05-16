#' Next and previous financial years
#' @name next_fy
#' @param fy A financial year as a character vector.
#' @param h An integer, the "horizon" to go forward (for \code{next_fy}) or
#' backward (for \code{prev_fy}).
#' @export next_fy prev_fy

next_fy <- function(fy, h = 1L) {
  if (!is.integer(h)) {
    if (!is.double(h) || !all(h == as.integer(h), na.rm = TRUE)) {
      stop("`h` was not a whole number. Ensure `h` is an integer")
    }
  }
  if (length(h) != 1L && length(h) != length(fy)) {
    stop("`h` had length ", length(h),
         ". `h` must have the same length as `fy` (",
         length(fy), ") or length-one.")
  }
  yr2fy(as.integer(substr(fy, 0L, 4L)) + 1L + h)
}

#' @rdname next_fy
prev_fy <- function(fy, h = 1L) {
  next_fy(fy, h = -h)
}


