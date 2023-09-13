#' @title Plasticizing Component
#'
#' @description
#' Density, distribution function, quantile function and random generation
#' for the plasticizing component with parameters teta, s2 and c.
#'
#' @param n positive number of observations
#' @param teta position parameter
#' @param s2 positive scale parameter
#' @param c shape parameter (c>=1)
#' @return The function returns random generator values for the plasticizing component.
#' @rdname rpc
#'
#' @details
#' Probability density function
#' see formula (2) in the article
#' Cumulative distribution function
#' see formula (4)
#' Quantile functon
#' see formula (9)
#' Random number generator
#' see formula (23)
#'
#' @author
#' Piotr Sulewski, \email{piotr.sulewski@upsl.edu.pl}, Pomeranian UNiwersity in Slupsk.
#'
#'
#' @references
#' {Sulewski, P. (2020). \emph{Normal Distribution with Plasticizing Component,} Communications in Statistics ? Theory and Method 51(11), 3806-3835.}
#'
#' @examples
#' dpc(0,1,2,2)
#' ppc(0,1,2,2)
#' qpc(0.5,1,2,2)
#' rpc(10,1,2,2)
#'
#' @export

rpc <- function(n, teta, s2, c) {
  rnorm <- NULL
  if (s2 > 0 & c >= 1)
  {
    x <- numeric(n)
    for (i in 1:n)
    {
      rn <- rnorm(1, 0, 1)
      x[i] <- ifelse(rn < 0, -s2 * (-rn) ^ (1 / c) + teta,
              s2 * (rn) ^ (1 / c) + teta)
    }
    return (x)
  }
  else
  {
    return('s2>0 and c>=1')
  }
}
