
#' @title Bootstrap-Based Optimism
#' 
#' @param object the apparent model
#' 
#' @param mc.cores \link[base]{integer} scalar, see function \link[parallel]{mclapply}
#' 
#' @param ... additional parameters of function [boot_rule()]
#' 
#' @details
#' Bootstrap-based optimism
#' of the dichotomized predictors. Specifically,
#' 
#' \enumerate{
#' 
#' \item{\eqn{R} copies of bootstrap samples are generated. In the \eqn{j}-th bootstrap sample,
#' \enumerate{
#' \item{obtain the dichotomizing rules \eqn{\mathbf{\mathcal{D}}^{(j)}} of predictors \eqn{x_1^{(j)},\cdots,x_k^{(j)}} based on response \eqn{y^{(j)}} (via [boot_rule])}
#' \item{multivariable regression (with additional predictors \eqn{z^{(j)}}, if any) coefficient estimates \eqn{\mathbf{\hat{\beta}}^{(j)} = \left(\hat{\beta}_1^{(j)},\cdots,\hat{\beta}_k^{(j)}\right)^t} of 
#' the dichotomized predictors \eqn{\left(\tilde{x}_1^{(j)},\cdots,\tilde{x}_k^{(j)}\right) = \mathcal{D}^{(j)}\left(x_1^{(j)},\cdots,x_k^{(j)}\right)} (via [add_dummies]) 
#' are the ***bootstrap performance estimate***.}
#' }
#' }
#' 
#' \item{Dichotomize \eqn{x_1,\cdots,x_k} in the *entire data* using each of the bootstrap rules \eqn{\mathcal{D}^{(1)},\cdots,\mathcal{D}^{(R)}}.
#' Multivariable regression (with additional predictors \eqn{z}, if any) coefficient estimates \eqn{\mathbf{\hat{\beta}}^{[j]} = \left(\hat{\beta}_1^{[j]},\cdots,\hat{\beta}_k^{[j]}\right)^t} of 
#' the dichotomized predictors \eqn{\left(\tilde{x}_1^{[j]},\cdots,\tilde{x}_k^{[j]}\right) = \mathcal{D}^{(j)}\left(x_1,\cdots,x_k\right)} (via [add_dummies])
#' are the ***test performance estimate***.}
#' 
#' \item{Difference between the bootstrap and test performance estimates, 
#' an \eqn{R\times k} \link[base]{matrix} of \eqn{\left(\mathbf{\hat{\beta}}^{(1)},\cdots,\mathbf{\hat{\beta}}^{(R)}\right)} minus 
#' another \eqn{R\times k} \link[base]{matrix} of \eqn{\left(\mathbf{\hat{\beta}}^{[1]},\cdots,\mathbf{\hat{\beta}}^{[R]}\right)},
#' are the ***bootstrap-based optimism***.}
#' 
#' }
#' 
#' @returns 
#' Function [boot_optimism.add_dummies()] returns an \eqn{R\times k} \link[base]{double} \link[base]{matrix}.
#'
#' @references 
#' Ewout W. Steyerberg (2009) Clinical Prediction Models.
#' \doi{10.1007/978-0-387-77244-8}
#' 
#' Frank E. Harrell Jr., Kerry L. Lee, Daniel B. Mark. (1996) 
#' Multivariable prognostic models: issues in developing models, evaluating
#' assumptions and adequacy, and measuring and reducing errors.
#' \doi{10.1002/(SICI)1097-0258(19960229)15:4<361::AID-SIM168>3.0.CO;2-4} 
#' 
#' @name boot_optimism
#' @export
boot_optimism <- function(object, ...) UseMethod(generic = 'boot_optimism')


#' @rdname boot_optimism
#' @importFrom parallel mclapply detectCores
#' @importFrom stats update
#' @export boot_optimism.add_dummies
#' @export
boot_optimism.add_dummies <- function(
    object, 
    mc.cores = getOption('mc.cores'),
    ...
) {
  
  # full data!
  data <- eval(attr(object, which = 'data.name', exact = TRUE), envir = parent.frame())
  fomd <- attr(object, which = 'formula', exact = TRUE)
  start.model <- attr(object, which = 'start.model', exact = TRUE)
  
  rules_bt <- boot_rule.add_dummies(object = object, mc.cores = mc.cores, ...)
  R <- length(rules_bt)
  
  boot_cf_ <- lapply(rules_bt, FUN = attr, which = 'effsize', exact = TRUE)
  boot_cf <- unlist(boot_cf_, use.names = FALSE)
  dim(boot_cf) <- c(length(boot_cf) / R, R)
  
  test_cf_ <- mclapply(rules_bt, mc.cores = mc.cores, FUN = \(rule) {
    add_dummies(
      formula = fomd, start.model = start.model, data = data, 
      rule = rule # force `rule` !!!
    ) |>
      attr(which = 'effsize', exact = TRUE)
  })
  test_cf <- unlist(test_cf_, use.names = FALSE)
  dim(test_cf) <- c(length(test_cf) / R, R)
  
  ret <- t.default(boot_cf - test_cf)
  #class(ret) <- c('boot_optimism', class(ret))
  return(ret)
  
}


# @export
#print.boot_optimism <- function(x, ...) {
#  cat('\nBootstrap Optimism\n\n')
#  x0 <- unclass(x)
#  atr <- attributes(x0)
#  attributes(x0)[setdiff(names(atr), 'dim')] <- NULL
#  print(x0)
#}



