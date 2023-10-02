test_that("Collatz sequence generator works correctly", {
  expect_equal(collatz_sequence(6), c(6, 3, 10, 5, 16, 8, 4, 2, 1))
})