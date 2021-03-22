# ols Objects ==================================================================

#' Compute  least squares estimator.
ols <- function(y, X) {

  # Compute OLS coefficient
  XX_inv <- solve(crossprod(X))
  coef <- XX_inv %*% crossprod(X, y)
    
  # Organize and return estimate
  coef <- as.matrix(coef)
  try(rownames(coef) <- colnames(X)) # assign coefficient names
  output <- list(coef = coef, y = y, X = X)
  class(output) <- "ols" # define S3 class
  return(output)
}#OLS

# Complementary methods ========================================================

#' Prediction method for ols fits.
predict.ols <- function(obj, newdata = NULL){
  
  # Obtain datamatrix
  if (is.null(newdata)) {
    
    newdata <- obj$X
    
  } else if (obj$const) {
    
    newdata <- cbind(1, newdata)
    
  }#IFELSE
  
  # Calculate and return fitted values with the OLS coefficient
  fitted <- newdata%*%obj$coef
  return(fitted)
}#PREDICT.OLS

#' Inference for ols fits.
#'
#' To do: implement additional HC and HAC types, including clustered se.
#'     Implement se for wls.
summary.ols <- function(obj,
                        type = "const") {
  
  # Data parameters
  nobs <- length(obj$y); ncol_X <- ncol(obj$X)
  
  # Calculate standard errors, t-statistic, and p-value
  resid <- as.numeric(obj$y - predict(obj))
  
  XX_inv <- solve(crossprod(obj$X))
  
  if (type == "const") {
    
    se <- sqrt(diag(sum(resid^2) * XX_inv) / (nobs - ncol_X)) # with dof adj.
    
  } else if (type == "HC1"){
    
    XuuX <- crossprod(obj$X *(resid^2), obj$X)
    S1 <- XX_inv %*% XuuX * (nobs/(nobs - ncol_X))
    se <- sqrt(diag(S1 %*% XX_inv)) # in two steps for numerical accuracy
    
  }#IFELSE
  t_stat <- obj$coef / se
  p_val <- 2 * pnorm(-abs(t_stat))
  
  # Compile estimate and se
  res <- cbind(obj$coef, se, t_stat, p_val)
  rownames(res) <- rownames(obj$coef)
  colnames(res) <- c("Coef.", "S.E.", "t Stat.", "p-val.")
  
  # Compute R^2
  R2 <- 1 - var(resid) / var(obj$y)
  
  # Compile output
  output <- list(res = res, nobs = nobs, R2 = R2)
  return(output)
}#SUMMARY.OLS