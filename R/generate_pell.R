#' Generate Pell Sequence
#'
#' @param n The number of terms to generate.
#'
#' @return A numeric vector containing the first n terms of the Pell sequence.
#' @examples
#' generate_pell(5)
#' @export

generate_pell <- function(n) {
  if (n == 1)
    return(0)
  if (n == 2)
    return(c(0, 1))
  
  pell <- c(0, 1)
  for (i in 3:n) {
    next_val <- 2 * pell[i - 1] + pell[i - 2]
    pell <- c(pell, next_val)
  }
  
  return(pell)
}