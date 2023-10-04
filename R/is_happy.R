#' Check if a Number is Happy
#'
#' @param num An integer to check.
#'
#' @return A logical value indicating whether the number is happy.
#' @examples
#' is_happy(19)
#' @export

is_happy <- function(num) {
  seen <- c()
  while (num != 1 && !(num %in% seen)) {
    seen <- c(seen, num)
    num <-
      sum(as.numeric(unlist(strsplit(
        as.character(num), ""
      ))) ^ 2)
  }
  
  return(num == 1)
}