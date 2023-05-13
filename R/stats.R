#' Proportion, adjusted for zero- and one-inflation
#'
#' @export prop_adj
#'
#' @param x Number of successes
#' @param n Number of tries
#' 
#' @returns A numeric scalar.
#' 
#' @examples prop_adj(4, 60)
#' 
prop_adj <- function(x, n) {
  (x + 2) / (n + 4)
}
