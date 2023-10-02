test_that("prime_factorization ", {
  expect_equal(as.numeric(prime_factorization(60)), c(2,1,1))
  expect_equal(as.numeric(prime_factorization(100)), c(2,2))
  expect_equal(as.numeric(prime_factorization(29)), 1)
})