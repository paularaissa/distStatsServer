#' @title Partial Model Predictions
#' @description Partial computation of prediction for logistic regression models. 
#' @details This function already needs some tests and improvements.
#' @param y is independent variable for a logistic model.
#' @param pi ia a vector with predictions values for each observation. 
#' @return a vector or matrix of predictions.
#' @author Paula R. Costa e Silva
#' @export

calculatellDS <- function(y,pi) {
  ll <- 1
  ll_unit <- 1:length(y)
  for (i in 1:length(y)){
    ll_unit[i] <- ifelse(y[i] == 1,pi[i],1-pi[i])
    ll = ll_unit[i]*ll
  }
  return(ll)
}