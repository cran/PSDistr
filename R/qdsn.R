#' @title DS Normal Distribution
#'
#' @description
#' Density, distribution function, quantile function and random generation
#' for the DS normal distribution with parameters a, b, c and d.
#'
#' @param p probability between 0 and 1
#' @param a non-negative multipurpose parameter and a+b>0
#' @param b non-negative multipurpose parameter and a+b>0
#' @param c real multipurpose parameter
#' @param teta real position parameter
#' @return The function returns the value of the quantile function for the DS normal distribution
#' @rdname qdsn
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
#' @author
#' Piotr Sulewski, \email{piotr.sulewski@upsl.edu.pl}, Pomeranian UNiwersity in Slupsk.
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
#' @importFrom pracma nthroot
#'
#' @export

qdsn <- function(p, a, b, c, teta) {
  qnorm <- NULL
  if (p > 0 & p < 1 & a >= 0 & b >= 0 & a + b > 0)
  {
    qru <- qnorm(p, 0, 1)
    if(a == 0 & b != 0)
    {
      x <- (qru - c) / b + teta
    }
    if(a != 0 & b == 0)
    {
      x <- nthroot((qru - c) / a, 3) + teta
    }
    if(a!=0 & b!=0)
    {
      q <- (c - qru) / a
      t <- 0.5 * (-q + sqrt(q ^ 2 + 4 * (b / a) ^ 3 / 27))
      x <- nthroot(t,3) - b / a / 3 / nthroot(t,3) + teta
    }
    return(x)
  }
  else
  {
    return('p>0 and p<1')
  }
}
