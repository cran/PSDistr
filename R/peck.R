#' @title Easily Changeable Kurtosis Distribution
#'
#' @description
#' Density, distribution function, quantile function and random generation
#' for the Easily Changeable Kurtosis Distribution with parameters a and p.
#'
#' @param x -a<x<a for -1<p<0 or -a<=x<=a for p>=1
#' @param a positive scale parameter
#' @param p shape parameter: p>-1
#' @return The function returns the value of the cumulative distribution function for the Easily Changeable Kurtosis Distribution.
#' @rdname peck
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

peck <- function(x, a, p) {
  pbeta <- NULL; sign <- NULL
  if (a > 0 & p > -1)
  {
    return(0.5 + 0.5 * sign(x) * pbeta(x * x / a / a, 0.5, p + 1))
  }
  else
  {
    return("a>0 and p>-1")
  }
}
