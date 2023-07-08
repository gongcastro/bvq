#' Proportion, adjusted for zero- and one-inflation
#' 
#' @details
#' It is very common that a large proportion of the participants know or do not know some word.
#' Vocabulary sizes and word prevalence norms in package are calculated using an estimate that
#' adjusts for zero- and one-inflation so that, at the population level such estimates are more
#' likely to be accurate.
#' 
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
