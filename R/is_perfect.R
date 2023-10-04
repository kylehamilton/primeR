#' Check if a Number is Perfect
#'
#' @param num An integer to check.
#'
#' @return A logical value indicating whether the number is perfect.
#' @examples
#' is_perfect(28)
#' @export

is_perfect <- function(num) {
  divisors <- 1:(num / 2)
  sum_divisors <- sum(divisors[num %% divisors == 0])
  return(sum_divisors == num)
}