
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
})

test_that("Testing reproduce_id", {
})
