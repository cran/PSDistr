#' @title DS Normal Distribution
#'
#' @description
#' Density, distribution function, quantile function and random generation
#' for the DS normal distribution with parameters a, b, c and d.
#'
#' @param x real argument
#' @param a non-negative multipurpose parameter and a+b>0
#' @param b non-negative multipurpose parameter and a+b>0
#' @param c real multipurpose parameter
#' @param teta real position parameter
#' @return The function returns the value of the probability density function for the DS normal distribution
#' @rdname ddsn
#'
#' @details
#' Probability density function in Latex
#' see formula (5) in the paper
#' Cumulative distribution function in Latex
#' see formula (6)
#' Quantile function
#' see formulas (8,9,10)
#' Random number generator
#' see Theorem (5)
#'
#' @author
#' Piotr Sulewski, \email{piotr.sulewski@upsl.edu.pl}, Pomeranian Uniwersity in Slupsk.
#'
#' @references
#' {Sulewski P. (2021). \emph{DS Normal Distribution: properties and applications.} Lobachevskii Journal of Mathematics 42(12), 2980-2999.}
#'
#' @examples
#' ddsn(-0.5,2,2,2,0)
#' pdsn(-0.5,2,2,2,0)
#' qdsn(0.5,2,2,2,0)
#' rdsn(10,2,2,2,0)
#'
#' @export

ddsn <- function(x, a, b, c, teta) {
  dnorm <- NULL
  if (a >= 0 & b >= 0 & a + b > 0)
  {
    return((3 * a * (x - teta) ^ 2 + b ) *
             dnorm(a * (x - teta) ^ 3 + b * (x - teta) + c, 0, 1))
  }
  else
  {
    return('a>=0 and b>=0 and a+b>0')
  }
}

