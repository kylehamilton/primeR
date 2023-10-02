test_that("is_palindrome returns TRUE or FALSE correctly", {
  # Test positive cases
  expect_true(is_palindrome(1234321))
  expect_true(is_palindrome(246010642))
  
  expect_false(is_palindrome(1234123))
  expect_false(is_palindrome(2460124601))
})
