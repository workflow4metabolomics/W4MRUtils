


#' @export
optparse_flag <- function(
  help = "No documentation yet.",
  short = NULL,
  default = FALSE
) {
  return(list(
    opt_str = c(short),
    action = "store_true",
    help = help,
    default = default
  ))
}

#' @export
optparse_numeric <- function(
  help = "No documentation yet.",
  short = NULL,
  default = 0
) {
  return(list(
    opt_str = c(short),
    type = "numeric",
    action = "store",
    help = help,
    default = default,
    metavar = "numeric"
  ))
}

#' @export
optparse_integer <- function(
  help = "No documentation yet.",
  short = NULL,
  default = 0
) {
  return(list(
    opt_str = c(short),
    type = "integer",
    action = "store",
    help = help,
    default = default,
    metavar = "number"
  ))
}

#' @export
optparse_character <- function(
  help = "No documentation yet.",
  short = NULL,
  default = 0
) {
  return(list(
    opt_str = c(short),
    type = "character",
    action = "store",
    help = help,
    default = default,
    metavar = "character"
  ))
}

#' @export
optparse_parameters <- function(
  ...,
  fix_hyphens = TRUE,
  fix_dots = TRUE,
  add_trailing_hyphens = TRUE
) {
  if (!suppressWarnings(requireNamespace("optparse", quietly = TRUE))) {
    stop(paste(
      "To uses `optparse_parameters`, you need to install the",
      "\"optparse\" package or to add it to your tool's dependencies"
    ))
  }
  optparse <- loadNamespace("optparse")
  parser <- optparse$OptionParser()
  param_definition <- list(...)
  for (long in names(param_definition)) {
    original <- long
    definition <- param_definition[[long]]
    if (fix_hyphens) {
      long <- gsub("_", "-", long, fixed = TRUE)
    }
    if (fix_dots) {
      long <- gsub(".", "-", long, fixed = TRUE)
    }
    if (add_trailing_hyphens) {
      long <- paste0("--", long)
    }
    definition$object <- parser
    definition$opt_str <- c(definition$opt_str, long)
    definition$dest <- original
    parser <- do.call(optparse$add_option, definition)
  }
  return(optparse$parse_args(parser))
}