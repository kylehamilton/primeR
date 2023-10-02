#' Get Next Prime After a Given Number
#'
#' Returns the next prime number after the given number.
#'
#' @param n Numeric the reference number.
#'
#' @return Numeric the next prime after n.
#'
#' @examples
#' next_prime(7)
#' @export
#' 
#' @references Sequence A000040 in The On-Line Encyclopedia of Integer Sequences, (2010), published electronically at https://oeis.org

next_prime <- function(n) {
  candidate <- n + 1
  while (!is_prime(candidate)) {
    candidate <- candidate + 1
  }
  return(candidate)
}
