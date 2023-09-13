#' @title Plasticizing Component
#'
#' @description
#' Density, distribution function, quantile function and random generation
#' for the plasticizing component with parameters teta, s2 and c.
#'
#' @param p probability between 0 and 1
#' @param teta position parameter
#' @param s2 positive scale parameter
#' @param c shape parameter (c>=1)
#' @return The function returns the value of the quantile function for the plasticizing component.
#' @rdname qpc
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
#' S{Sulewski, P. (2020). \emph{Normal Distribution with Plasticizing Component,} Communications in Statistics ? Theory and Method 51(11), 3806-3835.}
#'
#' @examples
#' dpc(0,1,2,2)
#' ppc(0,1,2,2)
#' qpc(0.5,1,2,2)
#' rpc(10,1,2,2)
#'
#' @importFrom pracma nthroot
#'
#' @export

qpc <- function(p, teta, s2, c) {
  qnorm <- NULL
  if (p > 0 & p < 1 & s2 > 0 & c >= 1)
  {
    return(ifelse(p < 0.5,teta - s2 * nthroot(-qnorm(p, 0, 1), c),
                  teta + s2 * nthroot(qnorm(p, 0, 1), c)))
  }
  else
  {
    return('p>0 and p<1 and s2>0 and c>=1')
  }
}
