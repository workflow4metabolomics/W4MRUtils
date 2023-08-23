#'
#' @importFrom utils capture.output
#' @importFrom utils str
#'
NULL


## taken from lib/galaxy/util/__init__.py:mapped_chars

mapped_chars__ <- list(
  `>` = "__gt__",
  `<` = "__lt__",
  `'` = "__sq__",
  '\"' = "__dq__",
  `[` = "__ob__",
  `]` = "__cb__",
  `{` = "__oc__",
  `}` = "__cc__",
  `@` = "__at__",
  `\n` = "__cn__",
  `\r` = "__cr__",
  `\t` = "__tc__",
  `#` = "__pd__"
)

mapped_chars_regex__ <- paste0(mapped_chars__, collapse = "|")


#' @export
run_galaxy_function <- function(tool_name, func, ...) {
  check_param_type_n_length(tool_name, "character")
  check_param_type_n_length(func, "function")
  env <- new.env()
  env$func <- func
  return(run_galaxy_processing(
    tool_name,
    func(args, logger), ## nolint
    env = env,
    ...
  ))
}

#' @export
run_galaxy_processing <- function(
  tool_name,
  code,
  tool_version = "unknown",
  unmangle_parameters = TRUE,
  args = NULL,
  logger = NULL,
  source_files = c(),
  env = NULL
) {
  if (is.null(logger)) {
    logger <- get_logger(tool_name)
  }
  if (is.null(env)) {
    env <- new.env()
  }
  if (is.null(args)) {
    args <- suppressWarnings(parse_args(args = args)) # nolint:object_usage_linter
  }
  check_param_type_n_length(tool_name, "character")
  check_param_type_n_length(tool_version, "character")
  check_param_type_n_length(unmangle_parameters, "logical")
  check_parameter_type(args, "list")
  check_param_type_n_length(logger, "W4MLogger")
  check_parameter_type(source_files, "character", or_null = TRUE)
  check_parameter_type(env, "environment")
  if (in_galaxy_env()) {
    if (unmangle_parameters) {
      args <- unmangle_galaxy_param(args)
    }
    show_galaxy_header(
      tool_name,
      tool_version = tool_version,
      args = args,
      logger = logger
    )
  }
  env$args <- args
  env$logger <- logger
  in_error <- FALSE
  for (file in source_files) {
    logger$infof("Sourcing %s...", file)
    source_local(file, env = env)
  }
  result <- tryCatch({
      eval(rlang::enexpr(code), env)
    },
    error = function(error) {
      in_error <<- TRUE
      logger$errorf("The tool %s has crashed.", tool_name)
      logger$errorf("Error: %s", error)
      return(NULL)
    }
  )
  if (in_galaxy_env()) {
    show_galaxy_footer(
      tool_name,
      tool_version = tool_version,
      logger = logger
    )
  }
  if (in_error) {
    stopf(
      "The tool %s - version %s ended in error.",
      tool_name,
      tool_version
    )
  }
  return(invisible(result))
}

#' @export
show_galaxy_header <- function(
  tool_name,
  tool_version,
  args = NULL,
  logger = NULL,
  show_start_time = TRUE,
  show_sys = TRUE,
  show_parameters = TRUE
) {
  if (is.null(args)) {
    args <- commandArgs()
  }
  sep <- collapse(rep("-", 68))
  gx_header <- ""
  if (show_start_time) {
    gx_header <- sprintf(
      "%sJob starting time:\n%s\n%s\n",
      gx_header,
      format(Sys.time(), "%a %d %b %Y %X"),
      sep
    )
  }
  if (show_sys) {
    gx_header <- sprintf(
      "%s\n%s\n%s\n",
      gx_header,
      collapse_lines(capture.output(print(get_r_env()))),
      sep
    )
  }
  if (show_parameters) {
    gx_header <- sprintf(
      "%s\nParameters used in %s - version %s:\n\n%s\n%s\n",
      gx_header,
      tool_name,
      tool_version,
      collapse_lines(capture.output(str(as.list(args)))),
      sep
    )
  }
  if (is.null(logger)) {
    cat(gx_header)
  } else {
    logger$default(gx_header)
  }
}

#' @export
show_galaxy_footer <- function(
  tool_name,
  tool_version,
  args = NULL,
  logger = NULL
) {
  add <- function(text = "") {
    gx_footer <<- sprintf("%s\n%s", gx_footer, text)
  }
  gx_footer <- sprintf(
    "End of '%s' Galaxy module call: %s",
    tool_name,
    as.character(Sys.time())
  )
  add()
  sessioninfo <- sessionInfo()
  add(sessioninfo$R.version$version.string)
  add()
  add("Main packages:")
  for (pkg in names(sessioninfo$otherPkgs)) {
    add(paste(pkg, packageVersion(pkg)))
  }
  add()
  add("Other loaded packages:")
  for (pkg in names(sessioninfo$loadedOnly)) {
    add(paste(pkg, packageVersion(pkg)))
  }
  if (is.null(logger)) {
    cat(gx_footer)
  } else {
    logger$default(gx_footer)
  }
}

#' @export
get_r_env <- function() {
  env_vars <- Sys.getenv()
  env_vars[grepl(x = names(env_vars), pattern = "^R_.*$")]
}

#' @export
show_sys <- function() {
  print(get_r_env())
}

#' @name chocisse
#' @description
#' in case you can't spell "show sys" lol
#' @export
chocisse <- show_sys ## lol

#' @export
in_galaxy_env <- function() {
  sysvars <- Sys.getenv()
  sysvarnames <- names(sysvars)
  return(any(
    c(
      "_GALAXY_JOB_HOME_DIR",
      "_GALAXY_JOB_TMP_DIR",
      "GALAXY_MEMORY_MB",
      "GALAXY_MEMORY_MB_PER_SLOT",
      "GALAXY_SLOTS"
    ) %in% sysvarnames
  ))
}

#' @export
unmangle_galaxy_param <- function(args) {
  check_parameter_type(args, "list")
  for (param in names(args)) {
    value <- args[[param]]
    if (is.character(value)) {
      value <- unmangle_galaxy_string(value)
    }
    unmangled_param <- unmangle_galaxy_string(param)
    if (unmangled_param != param) {
      args[[param]] <- NULL
    }
    args[[unmangled_param]] <- value
  }
  return(args)
}

#' @export
unmangle_galaxy_string <- function(string) {
  check_param_type_n_length(string, "character")
  if (! any(grepl(
    mapped_chars_regex__,
    string,
    ignore.case = TRUE
  ))) {
    return(string)
  }
  for (char in names(mapped_chars__)) {
    string <- gsub(
      mapped_chars__[[char]],
      char,
      string,
      fixed = TRUE
    )
  }
  return(string)
}
