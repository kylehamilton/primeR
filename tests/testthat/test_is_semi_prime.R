test_that("is_semi_prime returns TRUE for semi prime numbers", {
  expect_true(is_semi_prime(4))
  expect_true(is_semi_prime(6))
  expect_true(is_semi_prime(9))
  expect_true(is_semi_prime(10))
})

test_that("is_semi_prime returns FALSE for non semi prime numbers", {
  expect_false(is_semi_prime(5))
  expect_false(is_semi_prime(7))
  expect_false(is_semi_prime(12))
  expect_false(is_semi_prime(16))
})
