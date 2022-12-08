
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
})
