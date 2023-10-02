#' Check if a Number is in the Fibonacci Sequence
#'
#' @param num The number to check.
#'
#' @return A logical value indicating whether the number is part of the Fibonacci sequence.
#' @examples
#' is_fibonacci(13) # TRUE
#' is_fibonacci(15) # FALSE
#' @export

is_fibonacci <- function(num) {
  if (num < 0)
    return(FALSE)
  phi <- (1 + sqrt(5)) / 2
  inverse_phi <- (1 - sqrt(5)) / 2
  return(round(phi ^ num / sqrt(5)) == num ||
           round(inverse_phi ^ num / sqrt(5)) == num)
}