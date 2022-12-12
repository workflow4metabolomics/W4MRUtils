#' @title Mini tools for Galaxy scripting
#'
#' @name mini_tools
#'
#' @description Mini tools for Galaxy scripting
#' Mini tools for Galaxy scripting
#' Coded by: M.Petera,
#'
#' R functions to use in R scripts and wrappers
#' to make things easier (lightening code, reducing verbose...)
#'
#' V0: script structure + first functions
#' V1: addition of functions to handle special characters in identifiers
#'
#' @title source local
#'
#' @description source_local
#' Fonction pour sourcer les scripts R requis
#' /!\ ATTENTION : actuellement la fonction n'est pas chargee au lancement du
#'  script, il faut donc la copier-coller dans le wrapper R pour pouvoir
#' l'utiliser.
#'
#' @examples
#' \donttest{
#'    source_local("filter_script.R","RcheckLibrary.R")
#'  }
#' @examples
#' \donttest{
#'   source_local <- function(...) {
#'     argv <- commandArgs(trailingOnly = FALSE)
#'     base_dir <- dirname(substring(argv[grep("--file=", argv)], 8))
#'     lapply(
#'       list(...),
#'       function(path) {
#'         source(paste(base_dir, path, sep = "/"))
#'       }
#'     )
#'   }
#' }
NULL

#' @title Shy Lib
#'
#' @description shy_lib
#' Function to call packages without printing all the verbose
#' (only getting the essentials, like warning messages for example)
#'
#' @param ... Name of libraries to load
#' @return a \code{list} of attached packages
#'
#' @examples
#' \donttest{shy_lib("xcms","pcaMethods")}
#'
#' @author M.Petera
#'
#' @export
shy_lib <- function(...) {
  lapply(
    list(...),
    function(package) {
      suppressPackageStartupMessages(library(package, character.only = TRUE))
    }
  )
}

#'
#' @title Parse Command arguments
#'
#' @description parse_args
#'  Replacement for the parseCommandArgs utility from batch.
#'  Note that inputs like `script.R some-list c(1, 2, 3)` will result in
#'  args$`some-list` to be the string "c(1, 2, 3)", and not a vector anymore
#'  as this ability was permitted by dangerous behaviours from the
#'  batch package (the usage of `eval` which MUST NEVER be used on user's
#'  inputs).
#'
#'  To get a list of numeric from users, instead of using the `c(1, 2)` trick,
#'  please, use regular lists parsing:
#'
#'  ```
#'  > args$`some-list`
#'  [1] "1,2"
#'  args$`some-list` <- as.numeric(strsplit(args$`some-list`, ",")[[1]])
#'  > args$`some-list`
#'  [1] 1 2
#'  ```
#'
#' @param args optional, provide arguments to parse.
#'  This function will use 'commandArgs()' if args is not provided
#' @param convert_booleans logical - tells the function to convert
#'  values into logical if their value is "TRUE" or "FALSE".
#' @param convert_numerics logical - tells the function to convert
#'  values into numeric if possible.
#' @return a named \code{list} object containing the input parameters in values
#'  and the parameters names in names
#'
#' @author L.Pavot
#' @examples
#' parameters <- parse_args()
#' print(parameters$`some-parameter`)
#'
#' @export
parse_args <- function(
  args = NULL,
  convert_booleans = TRUE,
  convert_numerics = TRUE
) {
  warning(
    "Please, use the 'optparse' library instead of the 'parse_args' function."
  )
  if (is.null(args)) {
    args <- commandArgs()
  }
  start <- which(args == "--args")[1] + 1
  if (is.na(start)) {
    return(list())
  }
  seq_by2 <- seq(start, length(args), by = 2)
  result <- as.list(args[seq_by2 + 1])
  names(result) <- args[seq_by2]
  converters <- c()
  if (convert_booleans) {
    converters <- c(
      converters,
      function(x) {
        return(if (x == "TRUE") TRUE else if (x == "FALSE") FALSE else x)
      }
    )
  }
  if (convert_numerics) {
    converters <- c(
      converters,
      function(x) {
        return(if (is.na(y <- as.numeric(x))) x else y)
      }
    )
  }
  return(convert_parameters(result, converters))
}

#'
#' @title Convert Parameters
#'
#' @description convert_parameters
#'  Applies a list of converters to each values on a list.
#'  If a value is modified during the conversion (successfull conversion)
#'  then, no further convert will be applied to this value, so values are
#'  only converted once.
#'
#' @param args a named list, which values will be converted.
#' @param converters a vector of function. Each function will be applied to
#'  each values with the exception of values already converted by a
#'  previous converter.
#' @return a named \code{list} object with values converted by converters.
#'
#' @author L.Pavot
#' @examples
#' boolean_converter <- function(x) {
#'   return(if (x == "TRUE") TRUE else if (x == "FALSE") FALSE else x)
#' }
#' parameters <- convert_parameters(list("x" = "TRUE"), c(boolean_converter))
#' print(parameters$`some-parameter`)
#' ## "TRUE" has becomes TRUE.
#'
#' @export
convert_parameters <- function(args, converters) {
  suppressWarnings(
    for (param in names(args)) {
      for (converter in converters) {
        old_value <- args[[param]]
        args[[param]] <- converter(args[[param]])
        if (!identical(args[[param]], old_value)) {
          ## The value has been modified by the converter, and
          ## we don't want values to be converted multiple times,
          ## so we pass to the next value.
          break
        }
      }
    }
  )
  return(args)
}

#' @title Stock ID
#'
#' @description stock_id
#' Functions to stock identifiers before applying make.names() and
#' to reinject it into final matrices.
#' stock_id stocks original identifiers and original order
#' needs checked data regarding table match.
#'
#' @param data_matrix a \code{data.frame} containing the data_matrix
#' @param metadata a \code{data.frame} containing samplemetadata or
#'  variablemetadata
#' @param metadata_type "sample" or "variable" depending on metadata content
#' @return a names \code{list} with three elements:
#'  - id.match a \code{data.frame} that contains original order of ids, names ;
#'  - dataMatrix the modified data matrix with names sanitized
#'  - Metadata the modified metadata matrix with names sanitized
#' This object can be used in reproduce_id() to replace sanitized names in data
#' matrix by original ones, in the right order.
#'
#' @examples
#' \donttest{
#' A <- stock_id(myDM, mysM, "sample")
#' myDM <- A$data_matrix
#' mysM <- A$metadata
#' A <- A$id_match
#' }
#'
#' @author M.Petera
#'
#' @export
stock_id <- function(data_matrix, metadata, metadata_type) {
  # data_matrix = data.frame containing data_matrix
  # metadata = data.frame containing samplemetadata or variablemetadata
  # metadata_type = "sample" or "variable" depending on metadata content
  cname <- colnames(data_matrix)[1]
  # data_matrix temporary-stock + transfo - - - -
  if (metadata_type == "sample") {
    id_ori <- colnames(data_matrix)[-1]
    colnames(data_matrix) <- make.names(colnames(data_matrix))
  }
  if (metadata_type == "variable") {
    id_ori <- data_matrix[, 1]
    data_matrix[, 1] <- make.names(data_matrix[, 1])
  }
  # global stock - - - - - - - - - - - - - - - -
  id_new <- data.frame(
    order.ori = seq_along(metadata[, 1]), metadata[, 1],
    id.new = make.names(metadata[, 1]), id_ori,
    id.new.DM = make.names(id_ori), stringsAsFactors = FALSE
  )
  colnames(id_new)[c(2, 4)] <- c(colnames(metadata)[1], cname)
  # metadata transfo + returning data - - - - -
  metadata[, 1] <- make.names(metadata[, 1])
  return(list(
    id.match = id_new,
    dataMatrix = data_matrix,
    Metadata = metadata
  ))
}

#' @title Reproduce ID
#'
#' @description reproduce_id
#' reproduce_id() reinjects original identifiers and original order into
#'  final tables
#'
#' @param data_matrix data.frame containing data_matrix
#' @param metadata data.frame containing samplemetadata or variablemetadata
#' @param metadata_type "sample" or "variable" depending on metadata content
#' @param id_match 'id_match' element produced by stock_id
#' @return a named \code{list} with two elements:
#'  data_matrix: the processed data matrix with its original names and order
#'  metadata: the processed metadata, with its original names and order.
#'
#' @examples
#' \donttest{
#' B <- reproduce_id(datamatrix, sample_metadata, "sample", A)
#' datamatrix <- B$data_matrix
#' sample_metadata <- B$metadata
#' }
#'
#' @author M.Petera
#'
#' @export
reproduce_id <- function(data_matrix, metadata, metadata_type, id_match) {
  # Metadada - - - - - - - - - - - - - -
  temp_table <- id_match[, c(1, 2, 3)]
  ## Removing deleted rows
  for (i in 1:(dim(id_match)[1])) {
    if (!(temp_table[i, 3] %in% metadata[, 1])) {
      temp_table[i, 1] <- 0
    }
  }
  if (length(which(temp_table[, 1] == 0)) != 0) {
    temp_table <- temp_table[-c(which(temp_table[, 1] == 0)), ]
  }
  ## Restoring original identifiers and order
  temp_table <- merge(x = temp_table, y = metadata, by.x = 3, by.y = 1)
  temp_table <- temp_table[order(temp_table$order.ori), ]
  metadata <- temp_table[, -c(1, 2)]
  rownames(metadata) <- NULL
  # data_matrix - - - - - - - - - - - - -
  rownames(data_matrix) <- data_matrix[, 1]
  if (metadata_type == "sample") {
    data_matrix <- t(data_matrix[, -1])
  }
  temp_table <- id_match[, c(1, 4, 5)]
  ## Removing deleted rows
  for (i in seq_len(dim(id_match)[1])) {
    if (!(temp_table[i, 3] %in% rownames(data_matrix))) {
      temp_table[i, 1] <- 0
    }
  }
  if (length(which(temp_table[, 1] == 0)) != 0) {
    temp_table <- temp_table[-c(which(temp_table[, 1] == 0)), ]
  }
  ## Restoring original identifiers and order
  temp_table <- merge(x = temp_table, y = data_matrix, by.x = 3, by.y = 0)
  temp_table <- temp_table[order(temp_table$order.ori), ]
  if (metadata_type == "variable") {
    data_matrix <- temp_table[, -c(1, 2, 4)]
    colnames(data_matrix)[1] <- colnames(id_match)[4]
  } else {
    rownames(temp_table) <- temp_table[, 3]
    temp_table <- t(temp_table[, -c(1, 2, 3)])
    data_matrix <- data.frame(
      rownames(temp_table),
      temp_table,
      check.names = FALSE
    )
    colnames(data_matrix)[1] <- colnames(id_match)[4]
  }
  rownames(data_matrix) <- NULL
  # return datasets - - - - - - - - - - -
  return(list(data_matrix = data_matrix, metadata = metadata))
}
