#' @title Sulewski Plasticizing Component Distribution
#'
#' @description
#' Density, distribution function, quantile function and random generation
#' for the Sulewski plasticizing component distribution with parameters a, b, c, d and teta.
#'
#' @param n positive number of observations
#' @param a multipurpose parameter (a>=0)
#' @param b multipurpose parameter (b>=0, a+b>0)
#' @param c multipurpose parameter
#' @param d multipurpose parameter (d>=1)
#' @param teta position parameter
#' @return The function returns random generator values for the Sulewski plasticizing component distribution.
#' @rdname rspc
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
#' @import pracma
#'
#' @export

rspc <- function(n, a, b, c, d, teta) {
  qnorm <- NULL
  runif <- NULL
  if (a >= 0 & b >= 0 & a + b > 0 & d >= 1)
  {
    x <- numeric(n)
    for (i in 1:n)
    {
      los <- runif(1,0,1)
      jeden <- qnorm(los, 0, 1) ^ (1 / d)
      if(los >= 0.5)
      {
        if(a == 0 & b != 0)
        {
          x[i] <- (jeden - c) / b + teta
        }
        if(a != 0 & b == 0)
        {
          x[i]<-nthroot((jeden - c) / a, 3) + teta
        }
        if(a != 0 & b != 0)
        {
          q <- (c - jeden) / a
          t <- 0.5 * (-q + sqrt(q ^ 2 + 4 * (b / a) ^ 3 / 27))
          x[i] <- t ^ (1 / 3) - b / a / 3 / t ^ (1 / 3) + teta
        }
      }
      if (los < 0.5)
      {
        dwa <- -(-qnorm(los, 0, 1))^(1/d)
        if(a == 0 & b != 0)
        {
          x[i] <- (dwa - c) / b + teta
        }
        if(a != 0 & b == 0)
        {
          x[i] <- nthroot((dwa - c) / a, 3) + teta
        }
        if(a != 0 & b != 0)
        {
          q <- (c - dwa) / a
          t <- 0.5 * (-q + sqrt(q ^ 2 + 4 * (b / a) ^ 3 / 27))
          x[i] <- t ^ (1 / 3) - b / a / 3 / t ^ (1 / 3) + teta
        }
      }
    }
    return(x)
  }
  else
  {
    return("a >= 0 and b >= 0 and a + b > 0 and d >= 1")
  }
}
