test_that("generate_primes all three methods return the right numbers", {
  
  primes_up_to_100 <- c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97)
  
  # Test Eratosthenes
  expect_equal(generate_primes(100, "eratosthenes"), primes_up_to_100)
  expect_equal(length(generate_primes(100, "eratosthenes")), 25)
  # Test Sundaram
  expect_equal(generate_primes(100, "sundaram"), primes_up_to_100)
  expect_equal(length(generate_primes(100, "sundaram")), 25)
  
  # Test Atkin
  expect_equal(generate_primes(100, "atkin"), primes_up_to_100)
  expect_equal(length(generate_primes(100, "atkin")), 25)
})
