
test_that("Utilities works fine", {
  capture.output({
  testthat::expect_error(
    stopf("Error: %s", "problem"),
    regexp = "Error: problem",
    fixed = TRUE
  )
  testthat::expect_error(
    stopaste("Error:", "problem", sep = " "),
    regexp = "Error: problem",
    fixed = TRUE
  )
  testthat::expect_error(
    stopaste0("Error: ", "problem"),
    regexp = "Error: problem",
    fixed = TRUE
  )
  testthat::expect_output(
    printf("Info: %s", "message"),
    regexp = "Info: message",
    fixed = TRUE
  )
  testthat::expect_output(
    printfp(list(
      "Info: %s",
      "Sur: %s",
      "Plusieurs: %s",
      "Lignes: %s"
    ),
      "1",
      "2",
      "3",
      "4"
    ),
    regexp = "Info: 1 Sur: 2 Plusieurs: 3 Lignes: 4",
    fixed = TRUE
  )
  testthat::expect_output(
    printp(
      "Info", "sur", "plusieurs", "lignes",
      sep = " "
    ),
    regexp = "Info sur plusieurs lignes",
    fixed = TRUE
  )
  })
})

testthat::test_that("Parameter checkers works fine", {
  uwu <- function(x) {
    check_parameter_type(x, "character")
  }
  `òwó` <- function(x) { #nolint
    check_parameter_length(x, 1)
  }
  testthat::expect_null(uwu(""))
  testthat::expect_null(òwó(""))
  testthat::expect_error(uwu(42))
  testthat::expect_error(òwó(c("", "")))
})