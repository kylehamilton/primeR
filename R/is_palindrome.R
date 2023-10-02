#' Check if a Number is a Palindrome
#'
#' This function takes a number as input and returns TRUE if the number is a palindrome and FALSE otherwise.
#'
#' @param n A numeric value to check if it's a palindrome.
#'
#' @return A logical value indicating if the number is a palindrome.
#'
#' @examples
#' is_palindrome(121)     # TRUE
#' is_palindrome(123456)  # FALSE
#' @export

is_palindrome <- function(n) {
  str_n <- as.character(n)
  
  reversed_str_n <- rev(strsplit(str_n, split = "")[[1]])
  reversed_n <- paste0(reversed_str_n, collapse = "")
  
  return(str_n == reversed_n)
}
