#'
#' @title Access Data from Study and Response Variables by \code{formula}.
#' @description Data access and transformations for dependent and independent variables in regressions and model checking analysis.
#' @details Internal function.
#' Commonly used by methods relationed to regression and model checking analysis.
#' @param formula a string character to be transformed as an object of class \code{\link[stats]{formula}}.
#' @param subset optional vector which specifies the subset of observations to be used in the analysis.
#' @param weight a list or numerical value for weigthed analysis.
#' @param family a string character with the name of the error distribution and link function to be used in the analysis.
#' If \code{family} is set to 'binomial', it transforms the vector of observed responses \code{y.var}
#' into \code{\link[base]{factors}}.
#' If \code{family} is NULL, it does not transform the data.
#'
#' @return A list with components:
#' \item{bind.x}{a data matrix with the independent variables and their respective values.}
#' \item{bind.y}{a data matrix, with the response variable and its respective values.}
#' @author Paula R. Costa e Silva
#' @export
#'


getWilcoxTest <- function(x, mu, conf.int=FALSE) {

  bind.x <- getVarByName(x)

  x <- bind.x - mu
  zeroes <- any(x == 0)
  if (zeroes)
    x <- x <- x[x != 0]
  n <- as.double(length(x))
  r <- rank(abs(x))
  statistic <- setNames(sum(r[x > 0]), "V")
  rLength <- length(r)
  uniqueValues <- unique(r)

  if (conf.int) {
    x <- bind.x + mu
    muMin <- min(x)
    muMax <- max(x)
  }

}
