test_that("Geometric sequence working", {
  expect_equal(generate_geometric_seq(1, 2, 5), c(1, 2, 4, 8, 16))
})
