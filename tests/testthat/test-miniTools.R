
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

test_that("Testing stock_id", {
  data_matrix <- data.frame(id = 1:9, a = 2:10, b = 3:11)
  var_meta <- data.frame(x = 1:9, y = 2:10, z = 3:11)
  testthat::expect_identical(
    stock_id(data_matrix = data_matrix, var_meta, "variable"),
    list(
      id.match = data.frame(
        order.ori = 1:9,
        x = 1:9,
        id.new = make.names(1:9),
        id = 1:9,
        id.new.DM = make.names(1:9)
      ),
      dataMatrix = data.frame(
        id = make.names(1:9),
        a = 2:10,
        b = 3:11
      ),
      Metadata = data.frame(
        x = make.names(1:9),
        y = 2:10,
        z = 3:11
      )
    )
  )
})

test_that("Testing reproduce_id", {

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
