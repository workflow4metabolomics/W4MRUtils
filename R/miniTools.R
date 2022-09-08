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
#' \dontrun{source_local("filter_script.R","RcheckLibrary.R")}
#' @examples
#' \dontrun{
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
#' @examples
#' \dontrun{shy_lib("xcms","pcaMethods")}
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

#' @title Parse Command Args True/False
#'
#' @description parse_command_args_tf
#' Function to replace the default batch::parseCommandArgs to
#' solve an issue with batch if arguments are logical TRUE/FALSE
#'
#' @examples
#' \dontrun{
#' #interpretation of arguments given in command line as an R list of objects
#' args <- parse_command_args_tf(evaluate = FALSE)
#' }
#'
#' @author M.Petera
#'
#' @export
parse_command_args_tf <- function(...) {
  args <- batch::parseCommandArgs(...)
  for (key in names(args)) {
    if (args[key] %in% c("TRUE", "FALSE")) {
      args[key] <- as.logical(args[key])
    }
  }
  return(args)
}


#' @title Stock ID
#'
#' @description stock_id
#' Functions to stock identifiers before applying make.names() and
#' to reinject it into final matrices
#' Note: it reproduces the original order of datasets' identifiers
#' Function to be used at the very end, when exporting tables
#' @param stock_id: stocks original identifiers and original order
#'  needs checked data regarding table match
#' @param reproduce_id: reinjects original identifiers and original order into
#'  final tables
#'
#' @examples
#' \dontrun{
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
    id_new = make.names(metadata[, 1]), id_ori,
    id_new.DM = make.names(id_ori), stringsAsFactors = FALSE
  )
  colnames(id_new)[c(2, 4)] <- c(colnames(metadata)[1], cname)
  # metadata transfo + returning data - - - - -
  metadata[, 1] <- make.names(metadata[, 1])
  return(list(
    id_match = id_new,
    data_matrix = data_matrix,
    metadata = metadata
  ))
}

#' @title Reproduce ID
#'
#' @description reproduce_id
#'
#' @param data_matrix: data.frame containing data_matrix
#' @param metadata: data.frame containing samplemetadata or variablemetadata
#' @param metadata_type: "sample" or "variable" depending on metadata content
#' @param id_match: 'id_match' element produced by stock_id
#'
#' @examples
#' \dontrun{
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
