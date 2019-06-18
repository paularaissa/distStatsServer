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
#' @param datasources a list of data connection parameters.
#'
#' @return A list with components:
#' \item{bind.x}{a data matrix with the independent variables and their respective values.}
#' \item{bind.y}{a data matrix, with the response variable and its respective values.}
#' @author Paula R. Costa e Silva
#' @export
#'

getVarByCondFormula <- function(formula, cVar, relation, relation2, threshold, threshold2){
  options( warn = -1 )
  model.formula <- as.formula(formula)
  vars <- all.vars(model.formula)
  vars <- cbind(vars, cVar)

  if(is.null(data)){
    dataTable <- NULL
  }else{
    dataTable <- eval(parse(text=data))
  }

  model <- model.frame(model.formula)

  originalFormula <- Reduce(paste, deparse(formula))
  formula2use <- as.formula(paste0(Reduce(paste, deparse(originalFormula)))) # here we need the formula as a 'call' object

  model <- list()
  if (!is.null(relation) && is.null(relation2)) {
    switch(relation,
           gte={
             model <- model.frame(formula=model.formula, subset=eval(parse(text = cVar)) >= threshold)
           },
           lt={
             model <- model.frame(formula=model.formula, subset=eval(parse(text=cVar)) < threshold)
           },
           lte={
             model <- model.frame(formula=model.formula, subset=eval(parse(text=cVar)) <= threshold)
           },
           eq={
             model <- model.frame(formula=model.formula, subset=eval(parse(text=cVar)) == threshold)
           },
           gt={
             model <- model.frame(formula=model.formula, subset=eval(parse(text=cVar)) > threshold)
           }
    )
  }

  if (!is.null(relation) && !is.null(relation2)) {
    switch(relation,
           gte = {
             switch(relation2,
                    lte = {
                      model <- model.frame(formula = model.formula,
                                           subset = eval(parse(text = cVar)) >= threshold & eval(parse(text = cVar)) <= threshold2)
                    },
                    lt = {
                      model <- model.frame(formula = model.formula,
                                           subset = eval(parse(text = cVar)) >= threshold & eval(parse(text = cVar)) < threshold2)
                    })
           },
           gt = {
             switch(relation2,
                    lte = {
                      model <- model.frame(formula = model.formula,
                                           subset = eval(parse(text = cVar)) > threshold & eval(parse(text = cVar)) <= threshold2)
                    },
                    lt = {
                      model <- model.frame(formula = model.formula,
                                           subset = eval(parse(text = cVar)) > threshold & eval(parse(text = cVar)) < threshold2)
                    })}
    )
  }

  model.fim <- dplyr::mutate_(model, ratio=model.formula)
  calcValues <- data.matrix(model.fim$ratio)
  return(calcValues)
}

