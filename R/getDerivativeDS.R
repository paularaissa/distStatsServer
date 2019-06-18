#'
#' @title Partial Derivatives Computation of \eqn{\beta}'s.
#' @description Computes the matrix of second derivatives (Hessian) of the log'likelihood function \eqn{H=XWX^T},
#' and the equation \eqn{X(y-P_i)} that composes the formula to compute the estimated \eqn{\beta}'s.
#' @details The Hessian (matrix of second derivatives) of the log-likelihood is \deqn{H = XWX^T}
#' where W (computed by  \code{\link{findW}}) is a diagonal matrix of the derivatives \eqn{P_i} (computed by  \code{\link{findPi}}),
#' and \code{X} is the matrix of observations (computed by \code{\link{getVarbyFormula}}).
#' The solutions for \eqn{\delta} is
#' \deqn{\delta_k = (XW_kX^T)^-1 X(y-P_i)}
#' and for the partial derivatives computation, it was divided in two equations
#' \deqn{W = (XW_kX^T)} \deqn{Y = X(y-P_i)}
#' where W is the current matrix of derivatives, y is the vector of observed responses and \eqn{P_i} is the vector of probabilities.
#' @param formula a string character to be transformed as an object of class \code{\link[stats]{formula}}.
#' @param beta is a list of regression coefficients (estimated \code{beta}'s).
#' @param family a string character with the name of the error distribution and link function to be used in the analysis.
#' If \code{family} is set to 'binomial', it defines the link function as logit and likelihood as binomial.
#' If \code{family} is set to 'poisson', it defines the link function as log and likelihood as poisson.
#' @section Dependencies:
#' \code{\link{getVarbyFormula}}, \code{\link{findPi}}, \code{\link{findW}}
#' @return A list with components:
#' \item{xtxw}{a data matrix, the \emph{Hessian} matrix.}
#' \item{xtyp}{a data matrix, the \emph{Y} matrix that represents a partial computation of derivatives.}
#' @author Paula R. Costa e Silva
#' @export
#'

getDerivativeDS <- function(formula, beta, family) {
  bindxy <- getVarbyFormula(formula=formula, family=family)
  x <- data.matrix(bindxy$x)
  y <- data.matrix(bindxy$y)

  #Remove missing data, because it is not possible to compute the scalar product considering missing data.
  xy <- cbind(y, x)
  xy <- xy[complete.cases(xy), ]
  bind.x <- data.matrix(xy[, -1])
  bind.y <- data.matrix(xy[, 1])

  beta.aux <- as.numeric(unlist(strsplit(beta, split = "x")))
  beta.fim <- as.matrix(beta.aux)

  pi <- data.matrix(findPi(bind.x, beta.fim, family))
  W <- data.matrix(findW(pi, family))

  xtxw <- t(bind.x) %*% W %*% bind.x
  xtyp <- t(bind.x) %*% (bind.y - pi)

  return(list(xtxw = xtxw, xtyp = xtyp))
}
