#' @title Sulewski Plasticizing Component Distribution
#'
#' @description
#' Density, distribution function, quantile function and random generation
#' for the Sulewski plasticizing component distribution with parameters a, b, c, d and teta.
#'
#' @param x real argument
#' @param a multipurpose parameter (a>=0)
#' @param b multipurpose parameter (b>=0, a+b>0)
#' @param c multipurpose parameter
#' @param d multipurpose parameter (d>=1)
#' @param teta position parameter
#' @return The function returns the value of the probability density function for the Sulewski plasticizing component distribution.
#' @rdname dspc
#'
#' @details
#' Probability density function
#' see formula (2.1) in the article
#' Cumulative distribution function
#' see formula (2.2)
#' Quantile functon
#' see formulas (2.3-2.5)
#' Random number generator
#' see proposition (4)
#'
#' @author
#' Piotr Sulewski, \email{piotr.sulewski@upsl.edu.pl}, Pomeranian UNiwersity in Slupsk.
#'
#' @references
#' {Sulewski, P., Volodin, A. (2022). \emph{Sulewski Plasticizing Component Distribution: properties and applications.} Lobachtetavskii Journal of Mathtetamatics 43(8), 2286-2300.}
#'
#' @examples
#' dspc(0,1,1,1,1,0)
#' pspc(0,1,1,1,1,0)
#' qspc(0.5,1,1,1,1,0)
#' rspc(10,1,1,1,1,0)
#'
#' @export

dspc <- function(x, a, b, c, d, teta) {
  dnorm <- NULL; abs <- NULL
  if (a >= 0 & b >= 0 & a + b > 0 & d >= 1)
  {
    u <- a * (x - teta) ^ 3 + b * (x - teta) + c
    return(d * (3 * a * (x - teta) ^ 2 + b) * abs(u) ^ (d - 1) * dnorm(abs(u) ^ d,0,1))
  }
  else
  {
    return("a >= 0 and b >= 0 and a + b > 0 and d >= 1")
  }
}
