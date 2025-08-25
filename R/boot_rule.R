
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
#' \item{obtain the dichotomizing rules \eqn{\mathbf{\mathcal{D}}^{(j)}} of predictors \eqn{x_1^{(j)},\cdots,x_k^{(j)}} based on response \eqn{y^{(j)}} (via \link[maxEff]{node1})}
#' }
#' }
#' 
#' }
#' 
#' @returns 
#' Function [boot_rule.add_dummies()] returns a \link[base]{length}-\eqn{R} \link[base]{list} 
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
#' @keywords internal
#' @name boot_rule
#' @export
boot_rule <- function(object, R, ...) UseMethod(generic = 'boot_rule')


#' @rdname boot_rule
#' @importFrom parallel mclapply detectCores
#' @importFrom stats update
#' @export boot_rule.add_dummies
#' @export
boot_rule.add_dummies <- function(
    object, 
    R = 1e3L,
    mc.cores = getOption('mc.cores'),
    ...
) {
  
  # full data!
  data <- object |>
    attr(which = 'data.name', exact = TRUE) |>
    eval(envir = parent.frame())
  fomd <- object |>
    attr(which = 'formula', exact = TRUE)
  start.model <- object |>
    attr(which = 'start.model', exact = TRUE)
  
  ret <- data |>
    .row_names_info(x = _, type = 2L) |>
    bootid(n = _, R = R) |>
    mclapply(mc.cores = mc.cores, FUN = \(b) {
      b_data <- data[b, , drop = FALSE]
      add_dummies(
        formula = fomd, 
        start.model = update(start.model, data = b_data), # smart!
        data = b_data
      )
    })
  
  class(ret) <- 'listof'
  return(ret)
  
}


