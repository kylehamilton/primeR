#' Check if Two Numbers are Amicable
#'
#' @param a The first number.
#' @param b The second number.
#'
#' @return A logical value indicating whether the two numbers are amicable.
#' @examples
#' is_amicable(220, 284)
#' @export

is_amicable <- function(a, b) {
  sum_divisors_a <- sum((1:(a / 2))[a %% (1:(a / 2)) == 0])
  sum_divisors_b <- sum((1:(b / 2))[b %% (1:(b / 2)) == 0])
  
  return(sum_divisors_a == b && sum_divisors_b == a)
}
