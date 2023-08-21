
#' @export
stopf <- function(...) {
  stop(sprintf(...))
}

#' @export
stopaste <- function(...) {
  stop(paste(...))
}

#' @export
stopaste0 <- function(...) {
  stop(paste0(...))
}

#' @export
printf <- function(...) {
  print(sprintf(...))
}

#' @export
printfp <- function(x, ...) {
  print(sprintf(do.call(paste, x), ...))
}

#' @export
printp <- function(...) {
  print(paste(...))
}

#' @export
collapse <- function(..., sep = "") {
  paste0(..., collapse = sep, sep = "")
}

#' @export
collapse_lines <- function(..., sep = "\n") {
  paste0(..., collapse = sep, sep = "")
}

#' @export
check_param_type_n_length <- function(
  value,
  expected_type,
  expected_size = 1,
  nth = NULL,
  func_name = NULL,
  param_name = NULL
) {
  if (is.null(param_name)) {
    param_name <- deparse(substitute(value))
  }
  if (is.null(func_name)) {
    func_name <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
  }
  check_parameter_type(
    value,
    expected_type,
    nth = nth,
    func_name = func_name,
    param_name = param_name
  )
  check_parameter_length(
    value,
    expected_size,
    nth = nth,
    func_name = func_name,
    param_name = param_name
  )
}

#' @export
check_parameter_type <- function(
  value,
  expected_type,
  nth = NULL,
  func_name = NULL,
  param_name = NULL
) {
  if (is.null(param_name)) {
    param_name <- deparse(substitute(value))
  }
  if (is.null(func_name)) {
    func_name <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
  }
  if (! is.null(nth) && is.character(nth) && length(nth) == 1) {
    nth <- sprintf(" (%s)", nth)
  } else {
    nth <- ""
  }
  if (!is(value, expected_type)) {
    stopf(
      "The '%s'%s parameter for %s must be a %s, not a %s.",
      param_name,
      nth,
      func_name,
      expected_type,
      paste(class(value), collapse = "/")
    )
  }
}

#' @export
check_parameter_length <- function(
  value,
  expected_size,
  nth = NULL,
  func_name = NULL,
  param_name = NULL
) {
  if (is.null(param_name)) {
    param_name <- deparse(substitute(value))
  }
  if (is.null(func_name)) {
    func_name <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])
  }
  if (! is.null(nth) && is.character(nth) && length(nth) == 1) {
    nth <- sprintf(" (%s)", nth)
  } else {
    nth <- ""
  }
  if (length(value) != expected_size) {
    stopf(
      "The '%s'%s parameter for %s must be %s element long, not %s.",
      param_name, nth, func_name, expected_size, length(value)
    )
  }
}
