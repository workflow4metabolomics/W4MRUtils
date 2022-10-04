
test_that("Testing df_is", {
  testthat::expect_true(df_is(
    data.frame(a = c(1)),
    "numeric"
  ))
  testthat::expect_false(df_is(
    data.frame(a = c(1)),
    "character"
  ))
  testthat::expect_false(df_is(
    data.frame(a = c(1), b = c("test")),
    "numeric"
  ))
  testthat::expect_false(df_is(
    data.frame(a = c(2, 1), b = c(TRUE, FALSE)),
    "numeric"
  ))
})

test_that("Testing df_force_numeric", {
  some_integers <- as.integer(c(1, 2))
  testthat::expect_false(
    df_is(
      df_force_numeric(data.frame(a = c("1.2"))),
      "numeric"
    )
  )
  testthat::expect_false(
    df_is(
      df_force_numeric(data.frame(a = c("1", "2"))),
      "numeric"
    )
  )
  testthat::expect_false(
    df_is(
      df_force_numeric(
        data.frame(a = c("1", "2"), b = c("3", "4")),
        cols = c("a")
      ),
      "numeric"
    )
  )
  testthat::expect_true(
    df_is(
      df_force_numeric(
        data.frame(a = some_integers, b = c("3", "4")),
        cols = c("a")
      )[, "a"],
      "numeric"
    )
  )
  testthat::expect_true(
    df_is(
      df_force_numeric(
        data.frame(a = c(1, 2), b = c("3", "4")),
        cols = c("a")
      )[, "a"],
      "numeric"
    )
  )
  testthat::expect_null(df_force_numeric(NULL))
  testthat::expect_identical(df_force_numeric(data.frame()), data.frame())
})

test_that("Testing df_read_table", {
  original_df <- data.frame(a = c(6, 7, 8), b = c(4, 5, 6))
  path <- tempfile()
  file.create(path)
  write.table(original_df, file = path)
  suppressWarnings({
    loaded_df <- df_read_table(path, force_numeric = TRUE)
  })
  testthat::expect_identical(loaded_df, original_df)

  original_df <- data.frame(a = c(6, 7, 8), b = c(4, 5, 6))
  path <- tempfile()
  file.create(path)
  write.table(original_df, file = path)
  suppressWarnings({
    loaded_df <- df_read_table(path, force_numeric = FALSE)
  })
  testthat::expect_false(identical(loaded_df, original_df))

  ## FIXME: This tests should work
  # original_df <- data.frame(a = c(6, 7, 8), b = c("4", "5", "6"))            ##nolint
  # path <- tempfile()                                                         ##nolint
  # file.create(path)                                                          ##nolint
  # write.table(original_df, file = path, row.names = FALSE)                   ##nolint
  # loaded_df <- df_read_table(path, force_numeric = c("a"), header = TRUE)    ##nolint
  # testthat::expect_identical(loaded_df, original_df)                         ##nolint

  original_df <- data.frame(a = c("7", "8", "9"), b = c(4, 5, 6))
  path <- tempfile()
  file.create(path)
  write.table(
    original_df,
    file = path,
    row.names = FALSE
  )
  suppressWarnings({
    loaded_df <- df_read_table(
      path,
      colClasses = c(a = "character", b = "numeric"),
      header = TRUE
    )
  })
  testthat::expect_identical(loaded_df, original_df)

  original_df <- data.frame(a = c("7", "8", "9"), b = c("4", "5", "6"))
  path <- tempfile()
  file.create(path)
  write.table(original_df, file = path, row.names = FALSE)
  suppressWarnings({
    loaded_df <- df_read_table(
      path,
      colClasses = c(a = "character", b = "character"),
      header = TRUE
    )
  })
  testthat::expect_identical(loaded_df, original_df)
})
