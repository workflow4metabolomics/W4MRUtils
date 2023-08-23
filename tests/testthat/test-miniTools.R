
build_fake_r_file <- function() {
  path <- tempfile(fileext = ".R")
  file.create(path)
  writeLines(c(
    "setup_logger <- function(args, logger) {",
    "  if (!is.null(args$verbose) && args$verbose) {",
    "    logger$set_verbose(TRUE)",
    "  }",
    "  if (!is.null(args$debug) && args$debug) {",
    "    logger$set_debug(TRUE)",
    "  }",
    "  if (!is.null(args$logs)) {",
    "    logger$add_out_paths(args$logs)",
    "  }",
    "}",
    "processing <- function(args, logger) {",
    "  logger$info(\"The tool is working...\")",
    "  Sys.sleep(1)",
    "  logger$infof(\"Input: %s.\", args$input)",
    "  logger$info(\"The tool stoping.\")",
    "  return(NULL)",
    "}"
    ), con = path
  )
  return(path)
}

test_that("Testing source_local", {
  testthat::expect_warning(
    testthat::expect_error(source_local("/tmp/test.R"))
  )
})

test_that("Testing source_local with env", {
  env <- new.env()
  testthat::expect_no_error(source_local(build_fake_r_file(), env = env))
  testthat::expect_no_error(env$processing)
})

test_that("Testing shy_lib", {
  testthat::expect_warning(shy_lib("base"), regex = NA)
})

test_that("Testing parse_args", {
  testthat::expect_warning(
    testthat::expect_identical(parse_args(), list()),
    regex = paste(
      "Please, use the 'optparse' library instead of the",
      "'parse_args' function."
    ),
    fixed = TRUE
  )
  suppressWarnings({
    testthat::expect_identical(
      parse_args(args = c("--args", "x", "TRUE")),
      list(x = TRUE)
    )
    testthat::expect_identical(
      parse_args(args = c("--args", "x", "1")),
      list(x = 1)
    )
    testthat::expect_identical(
      parse_args(args = c("--args", "x", "1"), convert_numerics = FALSE),
      list(x = "1")
    )
    testthat::expect_identical(
      parse_args(args = c("--args", "x", "TRUE"), convert_booleans = FALSE),
      list(x = "TRUE")
    )
  })
})

test_that("Testing stock_id - variable", {
  data_matrix <- data.frame(id = c("a", "b"), a = 2:3, b = 3:4)
  var_meta <- data.frame(a = 1:2, b = 2:3, c = 3:4)
  testthat::expect_identical(
    stock_id(data_matrix = data_matrix, var_meta, "variable"),
    list(
      id.match = data.frame(
        order.ori = 1:2,
        a = 1:2,
        id.new = make.names(c("X1", "X2")),
        id = c("a", "b"),
        id.new.DM = make.names(c("a", "b"))
      ),
      dataMatrix = data.frame(
        id = make.names(c("a", "b")),
        a = 2:3,
        b = 3:4
      ),
      Metadata = data.frame(
        a = make.names(c("X1", "X2")),
        b = 2:3,
        c = 3:4
      )
    )
  )
})

test_that("Testing reproduce_id - variable", {
  data_matrix <- data.frame(id = c("a", "b"), a = 2:3, b = 3:4)
  var_meta <- data.frame(a = 1:2, b = 2:3, c = 3:4)

  datamatrix_a <- data.frame(
    data = 1:6, a = 2:7, b = 3:8, c = 2:7, d = 3:8, e = 2:7
  )
  variablemeta_a <- data.frame(
    variable = 1:6, x = 4:9, y = 2:7, z = 3:8
  )
  stored <- stock_id(datamatrix_a, variablemeta_a, "variable")

  datamatrix <- as.data.frame(stored$dataMatrix)
  variable_metadata <- as.data.frame(stored$Metadata)
  stored <- stored$id.match

  reproduced <- reproduce_id(datamatrix, variable_metadata, "variable", stored)
  testthat::expect_identical(datamatrix_a, reproduced$dataMatrix)
  testthat::expect_identical(variablemeta_a, reproduced$Metadata)
})

test_that("Testing stock_id - sample", {
  data_matrix <- data.frame(id = 1:3, a = 2:4, b = 3:5, c = 4:6)
  sample_meta <- data.frame(x = 1:3, y = 2:4, z = 3:5)
  testthat::expect_identical(
    stock_id(data_matrix = data_matrix, sample_meta, "sample"),
    list(
      id.match = data.frame(
        order.ori = 1:3,
        x = 1:3,
        id.new = make.names(1:3),
        id = c("a", "b", "c"),
        id.new.DM = c("a", "b", "c")
      ),
      dataMatrix = data.frame(
        id = 1:3,
        a = 2:4,
        b = 3:5,
        c = 4:6
      ),
      Metadata = data.frame(
        x = make.names(1:3),
        y = 2:4,
        z = 3:5
      )
    )
  )
})

test_that("Testing reproduce_id - sample", {

  datamatrix_a <- data.frame(id = c("a", "b", "c"), a = 2:4, b = 3:5, c = 4:6)
  sample_meta_a <- data.frame(a = 1:3, b = 2:4, c = 3:5)
  stored <- stock_id(datamatrix_a, sample_meta_a, "sample")

  datamatrix <- as.data.frame(stored$dataMatrix)
  sample_metadata <- as.data.frame(stored$Metadata)
  stored <- stored$id.match

  reproduced <- reproduce_id(datamatrix, sample_metadata, "sample", stored)
  testthat::expect_identical(reproduced$dataMatrix, datamatrix_a)
  testthat::expect_identical(reproduced$Metadata, sample_meta_a)
})
