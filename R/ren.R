#' @title Expnormal Distribution
#'
#' @description
#' Density, distribution function, quantile function and random generation
#' for the Expnormal distribution with parameters a1, b1, a2, b2 and c.
#'
#' @param n positive number of observations
#' @param a1 position parameter
#' @param b1 positive scale parameter
#' @param a2 position parameter
#' @param b2 positive scale parameter
#' @param c semi-fraction parameter
#' @return The function returns random generator values for the Expnormal distribution
#' @rdname ren
#'
#' @details
#' Probability density function
#' see formula (2.1) in the article
#' Cumulative distribution function
#' see formula (2.3)
#' Quantile functon
#' see proposition (2.2)
#' Random number generator
#' see proposition (2.6)
#'
#' @author
#' Piotr Sulewski, \email{piotr.sulewski@upsl.edu.pl}, Pomeranian UNiwersity in Slupsk.
#'
#' @references
#' {Sulewski, P. (2022). \emph{New Members of The Johnson Family of Probability Distributions:Properties and Application,} Accepted: February 2022. REVSTAT-Statistical Journal.}
#'
#' @examples
#' den(1,1,2,2,2,1)
#' pen(1,1,2,2,2,1)
#' qen(0.5,1,2,2,2,1)
#' ren(10,1,2,2,2,1)
#'
#' @export

ren = function(n, a1, b1, a2, b2, c) {
  runif <- NULL
  if (b1 > 0 & b2 > 0)
  {
    x <- numeric(n)
    for (i in 1:n)
    {
      x[i] <- qen(runif(1, 0, 1), a1, b1, a2, b2, c)
    }
    return(x)
  }
  else
  {
    return("b1 > 0 and b2 > 0")
  }
}
