#' Check if a Number is Prime
#'
#' This function checks if a given number is prime.
#'
#' @param num Numeric, the number to check.
#''
#' @return Logical, TRUE if the number is prime, otherwise FALSE.
#'
#' @examples
#' is_prime(7) # TRUE
#' is_prime(10) # FALSE
#' @export

is_prime <- function(num) {
  if (num <= 1)
    return(FALSE)
  if (num <= 3)
    return(TRUE)
  
  if (num %% 2 == 0 || num %% 3 == 0)
    return(FALSE)
  
  i <- 5
  while (i * i <= num) {
    if (num %% i == 0 || num %% (i + 2) == 0)
      return(FALSE)
    i <- i + 6
  }
  return(TRUE)
}