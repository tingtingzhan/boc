

#' @title Bootstrap-based Optimism Correction
#' 
#' @description 
#' 
#' Multivariable regression model with bootstrap-based optimism correction on the dichotomized predictors.
#' 
#' @param object the apparent model
#' 
#' @param bo boot strap optimism, returned value from function [boot_optimism()]
#' 
#' @param ... additional parameters of function [boot_optimism()]
#' 
#' @details
#' 
#' Function [boc()] obtains a multivariable regression model with 
#' bootstrap-based optimism correction on the dichotomized predictors.
#' Specifically,
#' 
#' \enumerate{
#' 
#' \item{Obtain the bootstrap optimism based on \eqn{R} copies of bootstrap samples.
#' The \link[stats]{median} of bootstrap-based optimism over \eqn{R} bootstrap copies
#' is the ***optimism-correction*** of the dichotomized predictors \eqn{\tilde{x}_1,\cdots,\tilde{x}_k}.}
# In future, we may expand the options to include the use of trimmed-mean \link[base]{mean.default}`(, trim)`, etc.
#' 
#' \item{Subtract the optimism-correction (in Step 2) from the apparent performance estimates (in Step 1), 
#' only for \eqn{\tilde{x}_1,\cdots,\tilde{x}_k}. 
#' The apparent performance estimates for additional predictors \eqn{z}'s, if any, are not modified.
#' Neither the variance-covariance (\link[stats]{vcov}) estimates 
#' nor the other regression diagnostics, e.g.,
#' \link[stats]{resid}uals,
#' \link[stats]{logLik}elihood,
#' etc.,
#' of the apparent performance are modified for now.
#' This coefficient-only, partially-modified regression model is  
#' the ***optimism-corrected performance***.
#' }
#' }
#' 
#' 
#' 
#' @returns 
#' Function [boc()] returns a regression model.
#' 
#' @keywords internal
#' @name boc
#' @export
boc <- function(object, ...) UseMethod(generic = 'boc')

#' @rdname boc
#' @importFrom matrixStats colMedians
#' @export
boc.add_dummies <- function(
    object, 
    bo = boot_optimism.add_dummies(object = object, ...), 
    ...
) {

  med_bo <- colMedians(bo, useNames = TRUE, na.rm = TRUE)
  ## later: trimmed-mean ?
  
  ret <- attr(object, which = 'model', exact = TRUE)
  ncf <- length(ret$coefficients)
  q <- length(object) # number of predictors to be dichotomized
  ret$coefficients[(ncf-q+1L):ncf] <- ret$coefficients[(ncf-q+1L):ncf] - med_bo
  ## Subtract the mean optimism estimates from the apparent performance 
  ## estimates to obtain the optimism-corrected performance estimates.
  
  # Tingting: we update the `$coefficients` of `ret`
  # so that the Wald-type z-statistics and p-values can be automatically calculated using ?summary
  # We need to update
  # ret$var
  # we still need cov(ret$coefficients, med_bo)
  # !!!! for now, just leave the variance/covariance as it was !!!
  # end of Tingting
  
  class(ret) <- c('boc.add_dummies', class(ret))
  return(ret)
  
}



