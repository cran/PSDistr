#' @title Sulewski Plasticizing Component Distribution
#'
#' @description
#' Density, distribution function, quantile function and random generation
#' for the Sulewski plasticizing component distribution with parameters a, b, c, d and teta.
#'
#' @param p probability between 0 and 1
#' @param a multipurpose parameter (a>=0)
#' @param b multipurpose parameter (b>=0, a+b>0)
#' @param c multipurpose parameter
#' @param d multipurpose parameter (d>=1)
#' @param teta position parameter
#' @return The function returns the value of the quantile function for the Sulewski plasticizing component distribution.
#' @rdname qspc
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
#' @importFrom pracma nthroot
#'
#' @examples
#' dspc(0,1,1,1,1,0)
#' pspc(0,1,1,1,1,0)
#' qspc(0.5,1,1,1,1,0)
#' rspc(10,1,1,1,1,0)
#'
#' @export

qspc <- function (p, a, b, c, d, teta) {
  qnorm <- NULL
  if (a >= 0 & b >= 0 & a + b > 0 & d >= 1)
  {
    if(p < 0.5) w <- -nthroot(-qnorm(p, 0, 1), d) else w <- nthroot(qnorm(p, 0, 1), d)
    t <- 0.5 * ((w - c) / a + sqrt((c -w) ^ 2 / a / a + 4 * b ^ 3 / 27 / a ^ 3))
    if (a == 0 & b != 0) xp <- (w - c) / b + teta
    if (a != 0 & b == 0) xp <- nthroot((w - c) / a, 3) + teta
    if (a != 0 & b != 0) xp <- nthroot(t, 3) - b / (3 * a * nthroot(t, 3)) + teta
    return(xp)
  }
  else
  {
    return("a >= 0 and b >= 0 and a + b > 0 and d >= 1")
  }
}
