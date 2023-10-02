test_that("nth_prime is working ", {
  expect_equal(nth_prime(1), 2)    # First prime
  expect_equal(nth_prime(10), 29)  # Tenth prime
  expect_equal(nth_prime(100), 541) # Hundredth prime
})
