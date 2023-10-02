#' Generate a Geometric Sequence
#'
#' @param a The first term in the sequence.
#' @param r The common ratio.
#' @param n The number of terms to generate.
#'
#' @return A numeric vector representing the geometric sequence.
#' @examples
#' generate_geometric_seq(1, 2, 5) # 1, 2, 4, 8, 16
#' @export

generate_geometric_seq <- function(a, r, n) {
  a * r ^ (seq(0, n - 1))
}