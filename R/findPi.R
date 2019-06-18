#'
#' @title Predictions on Each Observation
#' @description Computes the value of predictions on each observation (P(i)), given bind.x(input) and estimated betas.
#' @details Internal function.
#' Computes the predictions on each observation by a logit function (used for logistic regression) 
#' or log function (used for poisson regression).
#' @param bind.x is a data matrix of independent variables.
#' @param beta is a list of regression coefficients (estimated betas).
#' @param family a string character with the name of the link function to be used in the analysis.
#' If \code{family} is set to 'binomial', a logit function is applied.
#' If \code{family} is set to 'poisson', a log function applied.
#' @return a vector of predictions on each observation.
#' @author Paula R. Costa e Silva
#' @export
#'

findPi <- function(bind.x, beta, family){
  
  pi <- 1:nrow(bind.x)
  expon <- 1:nrow(bind.x)
  
  for (i in 1:nrow(bind.x)){
    expon[i] <- 0
    for (j in 1:ncol(bind.x)){
      expo <- bind.x[i,j] * beta[j]
      expon[i] <- expo + expon[i]
    }
    if(family == "binomial") #logit function for logistic regression
      pi[i] <- exp(expon[i])/(1+exp(expon[i]))
    if(family == "poisson") #log function for Poisson regression
      pi[i] <- exp(expon[i])
  }
  
  return(pi)
  
}