#' Get the N-th Triangular Number
#'
#' @param n The position of the triangular number.
#'
#' @return The n-th triangular number.
#' @examples
#' nth_triangle_num(4) # 10
#' nth_triangle_num(7) # 28
#' @export
#
nth_triangle_num <- function(n) {
  n * (n + 1) / 2
}