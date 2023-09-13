#' @title Plasticizing Component
#'
#' @description
#' Density, distribution function, quantile function and random generation
#' for the plasticizing component with parameters teta, s2 and c.
#'
#' @param x real argument
#' @param teta position parameter
#' @param s2 positive scale parameter
#' @param c shape parameter (c>=1)
#' @return The function returns the value of the probability density function for the plasticizing component.
#' @rdname dpc
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

dpc <- function(x, teta, s2, c) {
  dnorm <- NULL; abs <- NULL
  if (s2 > 0 & c >= 1)
  {
    wew <- (x - teta) / s2
    return(c / s2 *abs(wew) ^ (c - 1) * dnorm(abs(wew) ^ c, 0, 1))
  }
  else
  {
    return('s2>0 and c>=1')
  }
}
