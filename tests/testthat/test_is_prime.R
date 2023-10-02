test_that("is_prime() returns TRUE or FALSE correctly", {
  expect_true(is_prime(7))
  expect_false(is_prime(24601))
  expect_false(is_prime(1))
})
