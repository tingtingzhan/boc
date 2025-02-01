
#' @title Bootstrap-Based Dichotoming Rules
#' 
#' @param object the apparent model
#' 
#' @param R positive \link[base]{integer} scalar, 
#' number of bootstrap replicates \eqn{R}
#' 
#' @param mc.cores \link[base]{integer} scalar, see function \link[parallel]{mclapply}
#' 
#' @param ... additional parameters, currently no use
#' 
#' @details
#' Bootstrap-based optimism
#' of the dichotomized predictors. Specifically,
#' 
#' \enumerate{
#' 
#' \item{\eqn{R} copies of bootstrap samples are generated. In the \eqn{j}-th bootstrap sample,
#' \enumerate{
#' \item{obtain the dichotomizing rules \eqn{\mathbf{\mathcal{D}}^{(j)}} of predictors \eqn{x_1^{(j)},\cdots,x_k^{(j)}} based on response \eqn{y^{(j)}} (via \link[maxEff]{rpart1})}
#' }
#' }
#' 
#' }
#' 
#' @returns 
#' Function [boot_rule.add_dummies] returns a \link[base]{length}-\eqn{R} \link[base]{list} 
#' of `???`
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
#' @name boot_rule
#' @export
boot_rule <- function(object, R, ...) UseMethod('boot_rule')


#' @rdname boot_rule
#' @importFrom parallel mclapply detectCores
#' @importFrom stats update
#' @export boot_rule.add_dummies
#' @export
boot_rule.add_dummies <- function(
    object, 
    R = 1e3L,
    mc.cores = switch(.Platform$OS.type, windows = 1L, detectCores()),
    ...
) {
  
  # full data!
  data <- eval(attr(object, which = 'data.name', exact = TRUE), envir = parent.frame())
  fomd <- attr(object, which = 'formula', exact = TRUE)
  start.model <- attr(object, which = 'start.model', exact = TRUE)
  
  b_ <- bootid(n = .row_names_info(data, type = 2L), R = R) # \eqn{R} copies of 'integer' vectors
  
  rules_bt <- mclapply(b_, mc.cores = mc.cores, FUN = function(b) { # (b = b_[[1L]])
    b_data <- data[b, , drop = FALSE]
    add_dummies(
      formula = fomd, 
      start.model = update(start.model, data = b_data), # smart!
      data = b_data
      # `rule` to be determined by `b_data`
    )
  })

  return(rules_bt)
  
}


