#' Proportion, adjusted for zero- and one- inflation
#' 
#' @export prop_adj
#' 
#' @param x Number of successes
#' @param n Number of tries
#' @examples prop_adj_se(4, 60)
prop_adj <- function(x, n) {
    (x + 2) / (n + 4)
}

#' Standard error of proportion, adjusted for zero- and one-inflation
#' 
#' @export prop_adj_se
#' 
#' @param x Number of successes
#' @param n Number of trials
#' @examples prop_adj_se(4, 60)
prop_adj_se <- function(x, n) {
    e <- (x + 2) / (n + 4)
    sqrt(e * (1 - e) / (n + 4))
}

#' Confidence interval of proportion, adjusted for zero- and one-inflation
#'
#' @importFrom stats qnorm
#' @export prop_adj_ci
#' @param x Number of successes
#' @param n Number of tries
#' @param .width Confidence level (defaults to .95)
#' @examples prop_adj_ci(4, 60, .width = 0.89)
prop_adj_ci <- function(x, n, .width = 0.95) {
    e <- (x + 2) / (n + 4)
    se <- sqrt(e * (1 - e) / (n + 4))
    ci <- e + qnorm(c((1 - .width) / 2, (1 - (1 - .width) / 2))) * se
    ci[1] <- ifelse(ci[1] < 0, 0, ci[1]) # truncate at 0
    ci[2] <- ifelse(ci[2] > 1, 1, ci[2]) # truncate at 1
    return(ci)
}