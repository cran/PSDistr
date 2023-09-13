#' @title Easily Changeable Kurtosis Distribution
#'
#' @description
#' Density, distribution function, quantile function and random generation
#' for the Easily Changeable Kurtosis Distribution with parameters a and p.
#'
#' @param q probability between 0 and 1
#' @param a positive scale parameter
#' @param p shape parameter: p>-1
#' @return The function returns the value of the quantile function for the Easily Changeable Kurtosis Distribution.
#' @rdname qeck
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

qeck <- function(q, a, p) {
  uniroot <- NULL
  if (a > 0 & p > -1)
  {
    hp <- function(x, q, a, p) return(peck(x, a, p) - q)
    return(uniroot(hp, c(-a + 0.001, a - 0.001), tol = 0.0001,
                   q = q, a = a, p = p)$root)
  }
  else
  {
    return("a>0 and p>-1")
  }
}
