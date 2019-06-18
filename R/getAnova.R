#' @title Anova
#' @description Partial computations for Anova analysis.
#' @details Computes the partial equations for test regression models.
#' @param beta is a list of the regression coefficients.
#' @param formula a string character to be transformed as an object of class formula.
#' @param media.y a numeric value, the arithmetic mean for a given response variable.
#'
#' @return A list with components:
#' \item{rows.x}{the number of rows present in x.}
#' \item{rows.y}{the number of rows present in y.}
#' \item{cols.x}{the number of cols present in x.}
#' \item{cols.y}{the number of cols present in y.}
#' \item{residuals}{the prediction error.}
#' \item{sst}{total sum of squares.}
#' \item{sse}{sum of squares error.}
#' \item{lbx}{the number of study variables.}
#' \item{quad.y}{sum of total squares for response variable.}
#' \item{y.hat}{the fitted values.}
#' @author Paula Raissa Silva
#' @export

getAnova <- function(beta, formula, media.y) {
  #Data transformations
  beta.reg.aux <- as.numeric(unlist(strsplit(beta, split="x")))
  beta.reg <- data.matrix(beta.reg.aux)
  mean.y <- as.numeric(media.y)

  bindxy <- getVarbyFormula(formula=formula)
  bind.x <- data.matrix(bindxy$x)
  bind.y <- data.matrix(bindxy$y)

  y.hat <- fittedDS(beta=beta.reg, x=bind.x)

  #prediction error
  residuals <- bind.y - y.hat

  #lenght bind_x
  lbx <- length(bind.x)

  #rows x
  rows.x <- nrow(bind.x)
  #rows y
  rows.y <- nrow(bind.y)

  #cols x
  cols.x <- ncol(bind.x)
  #cols y
  cols.y <- ncol(bind.y)

  #k number of estimatiors
  k <- cols.x - 1

  s2 <- sum(residuals ^ 2) / (rows.x - k - 1)
  s <- sqrt(s2)

  #sse : sum of squares error
  sse <- sum(residuals ^ 2)

  #sst : total sum of squares
  sst <- sum((bind.y - mean.y) ^ 2)

  #standard error of the estimate
  std.residuals <- sum(bind.y - residuals)

  #sum of total squares
  quad.y <- sum(bind.y ^ 2)

  ## estimate of sigma-squared
  ##dSigmaSq <- sum((bind.y - bind.x%*%beta.reg)^2)/(nrow(bind.x)-ncol(bind.x))
  ##d.sigma.sq.part <- sum((bind.y - bind.x%*%beta.reg)^2)

  #Standardized
  residuos.std <- residuals / s


  return(list(rows.x = rows.x, rows.y = rows.y, cols.x = cols.x, cols.y=cols.y, residuals = residuals, sst = sst, sse = sse,
             lbx = lbx, quad.y = quad.y, y.hat = y.hat, std.residuals = std.residuals))

}
