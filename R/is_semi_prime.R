#' Check if a Number is a Semi-Prime
#'
#' This function checks if a given number is a semi-prime
#'
#' @param n An integer to be checked for being semi-prime.
#'
#' @return A logical value indicating whether the input number is a semi-prime.
#'
#' @examples
#' is_semi_prime(10)   # TRUE
#' is_semi_prime(12)   # FALSE
#' @export
#'
#' @references Sequence A000040 in The On-Line Encyclopedia of Integer Sequences, (2010), published electronically at https://oeis.org

is_semi_prime <- function(n) {
  if (n < 4)
    return(FALSE) # Smallest semi-prime is 4
  
  for (i in 2:sqrt(n)) {
    if (n %% i == 0 && is_prime(i) && is_prime(n / i)) {
      return(TRUE)
    }
  }
  return(FALSE)
}
