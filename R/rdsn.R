#' @title DS Normal Distribution
#'
#' @description
#' Density, distribution function, quantile function and random generation
#' for the DS normal distribution with parameters a, b, c and d.
#'
#' @param n positive number of observations
#' @param a non-negative multipurpose parameter and a+b>0
#' @param b non-negative multipurpose parameter and a+b>0
#' @param c real multipurpose parameter
#' @param teta real position parameter
#' @return The function returns random generator values for the DS normal distribution
#' @rdname rdsn
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

rdsn <- function(n, a, b, c, teta) {
  qnorm <- NULL; runif <- NULL
  if (n > 0 & a >= 0 & b >= 0 & a + b > 0)
  {
    x <- numeric(n)
    for (i in 1:n)
    {
      qru <- qnorm(runif(1, 0, 1), 0, 1)
      if(a == 0 & b != 0)
      {
        x[i] <- (qru - c) / b + teta
      }
      if(a != 0 & b == 0)
      {
        x[i] <- nthroot((qru - c) / a, 3) + teta
      }
      if(a != 0 & b != 0)
      {
        q <- (c - qru) / a
        t <- 0.5 * (-q + sqrt(q ^ 2 + 4 * (b / a) ^ 3 / 27))
        x[i] <- nthroot(t,3) - b / a / 3 / nthroot(t,3) + teta
      }
    }
    return(x)
  }
  else
  {
    return('n>0 and a>=0 and b>=0 and a+b>0')
  }
}
