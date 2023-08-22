
#' @export
optparse_flag <- function(
  help = "No documentation yet.",
  short = NULL,
  default = FALSE
) {
  return(list(
    opt_str = short,
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
    opt_str = short,
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
    opt_str = short,
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
    opt_str = short,
    type = "character",
    action = "store",
    help = help,
    default = default,
    metavar = "character"
  ))
}

#' @export
optparse_list <- function(
  help = "No documentation yet.",
  short = NULL,
  default = "",
  of = "character",
  sep = ","
) {
  return(list(
    opt_str = short,
    type = "character",
    action = "callback",
    help = help,
    default = default,
    metavar = of,
    callback = function(option_parser, param, value, ...) {
      if (of == "character") {
        transfo <- as.character
      } else if (of == "numeric") {
        transfo <- as.numeric
      } else if (of == "integer") {
        transfo <- as.integer
      } else if (of == "logical") {
        transfo <- function(x) {
          x %in% truevalues
        }
      } else {
        stopf("Unknown type: %s. Cannot transform without a convertor", of)
      }
      return(lapply(strsplit(value[[1]], sep, fixed = TRUE)[[1]], transfo))
    }
  ))
}

#' @export
optparse_parameters <- function(
  ...,
  fix_hyphens = TRUE,
  fix_dots = TRUE,
  add_trailing_hyphens = TRUE,
  args = NULL,
  no_optparse = FALSE
) {
  if (
    !suppressWarnings(requireNamespace("optparse", quietly = TRUE))
    || no_optparse
  ) {
    stopaste(
      "To uses `optparse_parameters`, you need to install the",
      "\"optparse\" package or to add it to your tool's dependencies"
    )
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
    definition$help <- definition$help
    definition$callback <- definition$callback
    parser <- do.call(optparse$add_option, definition)
  }
  if (!is.null(args)) {
    result <- optparse$parse_args(parser, args = args)
  } else {
    result <- optparse$parse_args(parser)
  }
  return(result)
}
