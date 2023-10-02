#' Get the N-th Prime Number
#'
#' This function returns the n-th prime number.
#'
#' @param n An integer indicating the position of the prime number to return.
#'
#' @return The n-th prime number.
#'
#' @examples
#' nth_prime(10)    # 29
#' nth_prime(100)   # 541
#' @export
#'
#' @references Sequence A002113 in The On-Line Encyclopedia of Integer Sequences, (2010), published electronically at https://oeis.org

nth_prime <- function(n) {
  if (n <= 0)
    stop("n should be a positive integer.")
  if (n == 1)
    return(2)
  
  count <- 1
  num <- 3
  
  while (count < n) {
    if (is_prime(num)) {
      count <- count + 1
    }
    if (count < n)
      num <- num + 2
  }
  
  return(num)
}