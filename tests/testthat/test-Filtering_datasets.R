test_that("Testing generic_filter - num", {

  datamatrix_a <- data.frame(id = c("V1", "V2", "V3", "V4"), a = 2:5, b = 3:6, c = 4:7)
  sample_meta_a <- data.frame(id = c("a", "b", "c"), C1 = 2:4, C2 = 3:5)
  variable_meta_a <- data.frame(id = c("V1", "V2", "V3", "V4"), P1 = 2:5, P2 = 3:6, P3 = 4:7)
  filtered <- generic_filter(datamatrix_a, sample_meta_a, variable_meta_a, NUM = TRUE, ls.num = list(c("variable","P3","upper","6.5")))

  expected_dm <- data.frame(id = c("V1", "V2", "V3"), a = 2:4, b = 3:5, c = 4:6)
  expected_sm <- data.frame(id = c("a", "b", "c"), C1 = 2:4, C2 = 3:5)
  expected_vm <- data.frame(id = c("V1", "V2", "V3"), P1 = 2:4, P2 = 3:5, P3 = 4:6)

  testthat::expect_identical(filtered$dataMatrix, expected_dm)
  testthat::expect_identical(filtered$sampleMetadata, expected_sm)
  testthat::expect_identical(filtered$variableMetadata, expected_vm)
})