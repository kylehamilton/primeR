#' Generate an Arithmetic Sequence
#'
#' @param a The first term in the sequence.
#' @param d The common difference.
#' @param n The number of terms to generate.
#'
#' @return A numeric vector representing the arithmetic sequence.
#' @examples
#' generate_arithmetic_seq(1, 2, 5) # 1, 3, 5, 7, 9
#' @export

generate_arithmetic_seq <- function(a, d, n) {
  seq(a, by = d, length.out = n)
}