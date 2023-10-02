#' Prime Factorization of a Number
#'
#' This function returns the prime factorization of a given number.
#'
#' @param n An integer to be factorized into its prime factors.
#'
#' @return A named vector where names are the prime factors and the values are their powers.
#'
#' @examples
#' prime_factorization(60)    # 2^2 * 3^1 * 5^1
#' prime_factorization(100)   # 2^2 * 5^2
#' @export

prime_factorization <- function(n) {
  if (n <= 0)
    stop("n should be a positive integer.")
  if (n == 1)
    return("1 has no prime factorization")
  
  factors <- integer()
  
  # Factor out 2s
  while (n %% 2 == 0) {
    factors <- c(factors, 2)
    n <- n / 2
  }
  
  # Factor out odd primes
  for (i in seq(3, sqrt(n), by = 2)) {
    while (n %% i == 0) {
      factors <- c(factors, i)
      n <- n / i
    }
  }
  
  # If n is a prime > 2
  if (n > 2) {
    factors <- c(factors, n)
  }
  
  # Return factors with their counts
  table(factors)
}