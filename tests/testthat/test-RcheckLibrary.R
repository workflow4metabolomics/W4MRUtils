
test_that("Testing check_err", {
  testthat::expect_error(
    check_err("Some error"),
    regex = "\n- - - - - - - - -\nSome error\n- - - - - - - - -\n",
    fixed = TRUE
  )
})

test_that("Testing match2", {
  
})

test_that("Testing match3", {
  
})
