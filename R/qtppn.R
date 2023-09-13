#' @title Two-Piece Power Normal Distribution
#'
#' @description
#' Density, distribution function, quantile function and random generation
#' for the two-piece power normal distribution with parameters teta, s1, s2 and c.
#'
#' @param p probability between 0 and 1
#' @param teta position parameter
#' @param s1 positive scale parameter
#' @param s2 positive scale parameter
#' @param c shape parameter (c>=1)
#' @return The function returns the value of the quantile function for the two-piece power normal distribution.
#' @rdname qtppn
#'
#' @details
#' Probability density function
#' see formula (4) in the article
#' Cumulative distribution function
#' see formula (5)
#' Quantile functon
#' see formula (10)
#' Random number generator
#' see formula (21)
#'
#' @author
#' Piotr Sulewski, \email{piotr.sulewski@upsl.edu.pl}, Pomeranian UNiwersity in Slupsk.
#'
#' @references
#' {Sulewski, P. (2021). \emph{Two-Piece Power Normal Distribution,} Communications in Statistics - Theory and Method 50(11), 2619-2639.}
#'
#' @importFrom pracma nthroot
#'
#' @examples
#' dtppn(2,1,1,1,2)
#' ptppn(2,1,1,1,2)
#' qtppn(0.5,1,1,1,2)
#' rtppn(10,1,1,1,2)
#'
#' @export

qtppn <- function(p, teta, s1, s2, c) {
  qnorm <- NULL
  if (p > 0 & p < 1 & s1 > 0 & s2 > 0 & c >= 1)
  {
    return(ifelse(p < 0.5,teta - s1 * nthroot(-qnorm(p, 0, 1), c),
                  teta + s2 * nthroot(qnorm(p, 0, 1), c)))
  }
  else
  {
    return('p>0 and p<1 and s1>0 and s2>0 and c>=1')
  }
}
