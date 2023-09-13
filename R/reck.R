#' @title Easily Changeable Kurtosis Distribution
#'
#' @description
#' Density, distribution function, quantile function and random generation
#' for the Easily Changeable Kurtosis Distribution with parameters a and p.
#'
#' @param n positive number of observations
#' @param a positive scale parameter
#' @param p shape parameter: p>-1
#' @return The function returns random generation values for the Easily Changeable Kurtosis Distribution.
#' @rdname reck
#'
#' @details
#' Probability density function
#' see formula (1) or (3) in the article
#' Cumulative distribution function
#' see formula (4)
#' Quantile functon
#' see formula (20)
#' Random number generator
#' see formula (41)
#'
#' @author
#' Piotr Sulewski, \email{piotr.sulewski@upsl.edu.pl}, Pomeranian UNiwersity in Slupsk.
#'
#' @references
#' {Sulewski, P. (2022). \emph{Easily Changeable Kurtosis Distribution.} Austrian Journal of Statistics 52, 1-24.}
#'
#' @examples
#' deck(1,2,3)
#' peck(1,2,3)
#' qeck(0.5,2,3)
#' reck(10,2,3)
#'
#' @export

reck <- function(n, a, p) {
  qbeta <- NULL
  runif <- NULL
  if (a > 0 & p > -1)
  {
    x <- numeric(n)
    for (i in 1:n)
    {
      R <- runif(1, 0, 1)
      if (R >= 0.5) x[i] <- a * sqrt(qbeta(2 * R - 1, 0.5, p + 1))
      if (R < 0.5)  x[i] <- -a * sqrt(qbeta(1 - 2 * R, 0.5, p + 1))
    }
    return(x)
  }
  else
  {
    return("a>0 and p>-1")
  }
}
