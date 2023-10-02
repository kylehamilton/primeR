test_that("Triangular number function works correctly", {
  expect_equal(nth_triangle_num(4), 10)
  expect_equal(nth_triangle_num(7), 28)
})