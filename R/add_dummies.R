
#' @title add_dummies
#' 
#' @param start.model a regression model, 
#' currently \link[stats]{lm}, \link[stats]{glm} or \link[survival]{coxph} models are supported
#' 
#' @param formula \link[stats]{formula}, 
#' additional \link[base]{numeric} covariates \eqn{x_1,\cdots,x_k} 
#' to be dichotomized
#' 
#' @param data (optional) \link[base]{data.frame}
#' 
#' @param rule a \link[stats]{listof} partitioning rules,
#' default is the returned value of function [rpart1.()]
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @details
#' 
#' First, obtain the dichotomizing rules \eqn{\mathbf{\mathcal{D}}} of predictors \eqn{x_1,\cdots,x_k} based on 
#' response \eqn{y} (via function [rpart1.()]).
#' 
#' Then, \link[stats]{update} the starting multivariable regression `start.model` 
#' with dichotomized predictors \eqn{\left(\tilde{x}_1,\cdots,\tilde{x}_k\right) = \mathcal{D}\left(x_1,\cdots,x_k\right)}. 
#' 
#' @returns
#' Function [add_dummies()] returns an object of class `'add_dummies'`.
# \item{`attr(,'rule')`}{a \link[base]{list} of dichotomizing rules [rpart1] for the \eqn{k} predictors}
#' 
#' @note
#' Different from function \link[maxEff]{add_dummy}, 
#' function [add_dummies()] adds *multiple* dichotomized predictors.
#' 
#' @examples
#' # see ?boc
#' @importFrom stats terms update model.frame.default na.pass
#' @importFrom utils tail
#' @export
add_dummies <- function(
    start.model, 
    formula, 
    data,
    rule = rpart1.(y = data[[terms(start.model)[[2L]]]], X = X),
    ...
) {
  
  if (missing(data)) {
    if (!is.language(data.name <- start.model$call$data)) stop('must have `data`-call')
    data <- eval(data.name, envir = parent.frame())
  } else data.name <- NULL
  
  X <- model.frame.default(formula = formula, data = data, na.action = na.pass)
  if (!all(vapply(X, FUN = is.numeric, FUN.VALUE = NA))) stop('all cols of `X` must be numeric')
  
  # `rule` based on full data (not the (potentially reduced) data of `start.model`)
  force(rule)
  
  # then reduce `data` and `X` to match `start.model`
  y <- start.model$y
  if (!length(y)) stop('`start.model` response?')
  if (length(y) != .row_names_info(data, type = 2L)) {
    old_na <- start.model$na.action
    if (!length(old_na)) stop('`start.model` `na.action` not available?')
    data <- data[-old_na, , drop = FALSE]
    X <- X[-old_na, , drop = FALSE]
  }
  
  X. <- mapply(FUN = function(f, x) f(x), f = rule, x = X[names(rule)], SIMPLIFY = FALSE)
  
  # identify duplicated elements in `X.`
  dupX <- duplicated.default(X.)
  if (any(dupX)) {
    ids <- match(x = X., table = X.[!dupX])
    X. <- X.[!dupX]
  }
  nms <- names(X.)
  data[nms] <- X. # ?stats::update looks for `nms` in `data`
  
  fom <- eval(call(
    name = '~', 
    quote(.), # lhs
    Reduce(f = function(e1, e2) {
      call(name = '+', e1, e2)
    }, c(list(quote(.)), lapply(nms, FUN = as.symbol)))
  )) # `. ~ . + x1 + x2 + x3`
  newmod <- update(start.model, formula. = fom, data = data)
  
  #if (anyNA(newmod$coefficients)) {
  #  print(mod)
  #  warning('still could happen')
  #}
  
  cf_ <- tail(newmod$coefficients, n = length(nms))
  if (any(dupX)) cf_ <- cf_[ids] # to expand `cf_`
  attr(rule, which = 'effsize') <- cf_ # for [boot_optimism.add_dummies]
  
  attr(rule, which = 'formula') <- formula # for [boot_rule.add_dummies]
  attr(rule, which = 'data.name') <- data.name # for [boot_rule.add_dummies]
  attr(rule, which = 'model') <- newmod # for [boc.add_dummies]
  attr(rule, which = 'start.model') <- start.model # for [boot_rule.add_dummies]

  class(rule) <- c('add_dummies', class(rule))
  return(rule)
  
}







#' @title print.add_dummies
#' 
#' @param x an [add_dummies] object
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @details
#' ..
#' 
#' @returns
#' Function [print.add_dummies()] does not have a returned value
#' 
#' @keywords internal
#' @export print.add_dummies
#' @export
print.add_dummies <- function(x, ...) {
  
  x0 <- unclass(x)
  xnm <- names(x)
  mapply(FUN = function(x0, xnm) {
    cat('\nDichotomizing Rule for', sQuote(xnm), 'based on Recursive Partitioning:\n\n')
    print(unclass(x0))
  }, x0 = x0, xnm = xnm)
  
  atr <- attributes(x)
  if (!length(atr)) return(invisible())
  cat('\nDevelopers, use\n')
  if (length(atr$effsize)) cat('\nattr(.,\'effsize\') to see the regression coefficient, i.e., effect size, of the dichotomized variables in augmented\n')
  if (length(atr$model)) cat('\nattr(.,\'model\') to see the augmented model\n\n')

}






#' @title Batch Operation of Function [rpart1()]
#' 
#' @param X \link[base]{data.frame} or \link[base]{list}
#' 
#' @param mc.cores \link[base]{integer} scalar, see function \link[parallel]{mclapply}
#' 
#' @param ... additional parameters of function [rpart1()]
#' 
#' @returns 
#' Function [rpart1.()] returns a \link[stats]{listof} function [rpart1()] returns.
#' 
#' @keywords internal
#' @importFrom parallel mclapply detectCores
#' @importFrom maxEff rpart1
#' @export
rpart1. <- function(X, mc.cores = switch(.Platform$OS.type, windows = 1L, detectCores()), ...) {
  ret <- mclapply(X, mc.cores = mc.cores, FUN = function(x) rpart1(x = x, ...))
  class(ret) <- 'listof'
  return(ret)
}



