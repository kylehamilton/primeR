#' Generate Harshad Numbers
#'
#' @param n The number of Harshad numbers to generate.
#'
#' @return A numeric vector containing the first n Harshad numbers.
#' @examples
#' generate_harshad(5)
#' @export

generate_harshad <- function(n) {
  count <- 0
  num <- 1
  harshads <- c()
  
  while (count < n) {
    digit_sum <-
      sum(as.numeric(unlist(strsplit(
        as.character(num), ""
      ))))
    if (num %% digit_sum == 0) {
      harshads <- c(harshads, num)
      count <- count + 1
    }
    num <- num + 1
  }
  
  return(harshads)
}