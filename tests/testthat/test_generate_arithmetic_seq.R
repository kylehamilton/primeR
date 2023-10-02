test_that("Arithmetic sequence working", {
  expect_equal(generate_arithmetic_seq(1, 2, 5), c(1, 3, 5, 7, 9))
})
