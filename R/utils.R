.getOption <- function (x, default = NULL) {
  ans <- getOption(x)
  if (is.null(ans)) {
    default
  } else {
    ans
  }
}

accel_repetitive_input <- function (x, FUN, ..., THRESHOLD = 1000L) {
  .FUN <- match.fun(FUN)
  if (length(x) <= 1L || length(x) < THRESHOLD) {
    .FUN(x)
  } else {
    DT <- setDT(list(x = x))
    .subset2(DT[, `:=`("res", .FUN(.BY[[1L]], ...)), by = "x"],
             "res")
  }
}


#' @noRd
#' @param ... Message
#' @param n Level to emit the error message
#' @return Error message but the calling handle at the 'user-level'
stopn <- function (..., n = -sys.nframe()) {
  error_message <- paste0(..., collapse = "")
  condition <- function(subclass, message, call = sys.call(-1),
                        ...) {
    structure(class = c(subclass, "condition"), list(message = message,
                                                     call = call), ...)
  }
  custom_stop <- function(subclass, message, call = sys.call(n - 1L), ...) {
    ER <- condition(c("my_error", "error"), error_message,
                    call = call, ...)
    stop(ER)
  }
  custom_stop()
}


