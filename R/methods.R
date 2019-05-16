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

#' @export
range.fy <- function(...) {
  if (.getOption("verbose", FALSE)) {
    cat("Using range.fy method\n")
  }
  R <- range_fy2yr(..1)
  yr2fy(R)
}

#' @export
min.fy <- function(...) {
  range.fy(...)[1]
}

#' @export
max.fy <- function(...) {
  range.fy(...)[2]
}

# nocov start

#' @export
print.fy <- function(x, ...) {
  if (length(x) < 10) {
    cat("fy (", length(x), "): ", paste0(x, collapse = "  "), sep = "")
  } else {
    cat("fy (", prettyNum(length(x), big.mark = " "), "): ",
        paste(head(x), collapse = "  "), "...\t",
        if (!is.null(miny <- attr(x, "fy_min_yr"))) paste0("  min_yr = ", miny),
        if (!is.null(maxy <- attr(x, "fy_max_yr"))) paste0("  max_yr = ", maxy),
        sep = "")
  }
}
# nocov end


