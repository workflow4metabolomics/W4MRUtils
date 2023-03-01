#'
#' @importFrom utils capture.output
#' @importFrom utils str
#'
NULL

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
  sep <- paste0(rep("-", 68), collapse = "")
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
      "%s%s\n%s\n",
      gx_header,
      paste0(capture.output(print(get_r_env())), collapse = "\n"),
      sep
    )
  }
  if (show_parameters) {
    gx_header <- sprintf(
      "%sParameters used in %s - %s:\n\n%s\n%s\n",
      gx_header,
      tool_name,
      tool_version,
      paste0(capture.output(str(as.list(args))), collapse = "\n"),
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
  ## taken from lib/galaxy/util/__init__.py:mapped_chars
  mapped_chars <- list(
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
  for (param in names(args)) {
    if (is.character(args[[param]])) {
      for (char in names(mapped_chars)) {
        mangled <- mapped_chars[[char]]
        args[[param]] <- gsub(
          mangled,
          char,
          args[[param]],
          fixed = TRUE
        )
      }
    }
  }
  return(args)
}
