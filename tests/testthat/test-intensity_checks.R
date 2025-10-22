test_that("Testing intensity_checks - one_class", {

  datamatrix_a <- data.frame(id = c("V1", "V2", "V3", "V4"), a = 2:5, b = 3:6, c = 4:7, d = 8:11)
  sample_meta_a <- data.frame(id = c("a", "b", "c", "d"), C1 = c("A", "A", "B", "B"), C2 = 1:4)
  variable_meta_a <- data.frame(id = c("V1", "V2", "V3", "V4"), P1 = 2:5, P2 = 3:6)
  completed_vm <- intens_check(datamatrix_a, sample_meta_a, variable_meta_a, "one_class", "mean,NA" , "C1", "No",
                         "A", NULL, "none", tempfile())

  expected_vm <- data.frame(id = c("V1", "V2", "V3", "V4"), P1 = 2:5, P2 = 3:6, Mean_A = c(2.5, 3.5, 4.5, 5.5),
                            Mean_Other = c(6, 7, 8, 9), NA_A = rep(0, 4), NA_Other = rep(0, 4), Pct_NA_A = rep(0, 4),
                            Pct_NA_Other = rep(0, 4))
  rownames(expected_vm) <- c("V1", "V2", "V3", "V4")

  testthat::expect_identical(completed_vm , expected_vm)
})