#'
#' @title Matrix W
#' @description Calculate the matrix W with all diagonal values as p(i).
#' @details It is a partial computation for Hessian Matrix (second derivative).
#' If the Hessian Matrix is \eqn{H=XWX^T}, the W matrix is \eqn{W = P_i(1-P_i)}.
#' @param pi is a list of predictions on each observation.
#' @param family a character that determines the likelihood function.
#' If \code{family} is set to 'binomial', a likelihood for binary logistic regression is applied
#' If \code{family} is set to 'poisson', a likelihood for poisson regression is applied.
#' @return the W matrix
#' @author Paula R. Costa e Silva
#' @export
#'

findW <- function(pi, family){

  W <- matrix(0,length(pi),length(pi))
  if(family == "binomial") {
    for (i in 1:length(pi)){
      #likelihood for binary logistic regression
      W[i,i] <- pi[i]*(1-pi[i]) 
    }
  }
  if(family == "poisson") {
    for (i in 1:length(pi)){
      #likelihood for binary poisson regression
      W[i,i] <- pi[i] 
    }
  }
  return(W)
    
}