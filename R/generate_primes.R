#' Generate Prime Numbers up to a Given Limit
#'
#' Generate a list of prime numbers up to a specified integer using one of the specified sieve algorithms.
#'
#' @param n An integer, the upper limit up to which prime numbers should be generated.
#' @param method A character string, indicating the sieve algorithm to be used.
#'   Options are "eratosthenes" (default), "sundaram", and "atkin".
#'
#' @return A vector of prime numbers up to the specified integer.
#'
#' @examples
#' generate_primes(10)
#' generate_primes(10, "atkin")
#' generate_primes(10, "sundaram")
#' @export
#'
#' @references Horsley, S. (1772). The Sieve of  Eratosthenes. Being an Account of His Method of Finding
#' All the Prime Numbers, by the Rev. Samuel Horsley, F. R. S. Philosophical Transactions
#' (1683-1775), 62, 327–347. http://www.jstor.org/stable/106053
#'
#' V. Ramaswami Aiyar (1934). "Sundaram's Sieve for Prime Numbers". The Mathematics Student. 2 (2): 73.
#'
#' Atkin, A. O. L., & D. J. Bernstein. (2004). Prime Sieves Using Binary Quadratic Forms.
#' Mathematics of Computation, 73(246), 1023–1030. http://www.jstor.org/stable/4099818
#'
#' Sequence A000040 in The On-Line Encyclopedia of Integer Sequences, (2010), published electronically at https://oeis.org

generate_primes <-
  function(n,
           method = c("eratosthenes", "sundaram", "atkin")) {
    method <- match.arg(method)
    
    if (n < 2)
      return(integer(0))
    
    if (method == "eratosthenes") {
      if (n < 2)
        return(integer(0))
      sieve <- rep(TRUE, n)
      sieve[1] <- FALSE
      for (i in 2:sqrt(n)) {
        if (sieve[i]) {
          sieve[seq(i ^ 2, n, by = i)] <- FALSE
        }
      }
      return(which(sieve))
      
    } else if (method == "sundaram") {
      n_new <- (n - 1) %/% 2
      sieve <- rep(TRUE, n_new)
      for (i in 1:n_new) {
        j <- 1
        while (i + j + 2 * i * j <= n_new) {
          sieve[i + j + 2 * i * j] <- FALSE
          j <- j + 1
        }
      }
      primes <- 2 * which(sieve) + 1
      if (n >= 2) {
        primes <-
          c(2, primes)
      }
      return(primes)
    } else if (method == "atkin") {
      sieve <- rep(FALSE, n + 1)
      limit_square <- as.integer(sqrt(n))
      
      for (x in 1:limit_square) {
        for (y in 1:limit_square) {
          n_val <- (4 * x ^ 2) + (y ^ 2)
          if (n_val <= n &&
              (n_val %% 12 == 1 || n_val %% 12 == 5)) {
            sieve[n_val] <- !sieve[n_val]
          }
          n_val <- (3 * x ^ 2) + (y ^ 2)
          if (n_val <= n && n_val %% 12 == 7) {
            sieve[n_val] <- !sieve[n_val]
          }
          if (x > y) {
            n_val <- (3 * x ^ 2) - (y ^ 2)
            if (n_val <= n && n_val %% 12 == 11) {
              sieve[n_val] <- !sieve[n_val]
            }
          }
        }
      }
      
      for (i in 5:limit_square) {
        if (sieve[i] && i ^ 2 <= n) {
          for (j in seq(i ^ 2, n, by = i ^ 2)) {
            sieve[j] <- FALSE
          }
        }
      }
      
      if (n > 2)
        sieve[2] <- TRUE
      if (n > 3)
        sieve[3] <- TRUE
      
      return(which(sieve))
    }
  }
