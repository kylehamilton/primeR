#' Generate the Collatz Sequence
#'
#' @param n The starting number for the sequence.
#'
#' @return A numeric vector representing the Collatz sequence starting from the given number.
#' @examples
#' collatz_sequence(6) # 6, 3, 10, 5, 16, 8, 4, 2, 1
#' @export

collatz_sequence <- function(n) {
  seq <- c(n)
  while (n != 1) {
    if (n %% 2 == 0) {
      n <- n / 2
    } else {
      n <- 3 * n + 1
    }
    seq <- c(seq, n)
  }
  return(seq)
}