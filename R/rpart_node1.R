

#' @title Batch Operation of Functions \link[rpart]{rpart} \link[maxEff]{node1}
#' 
#' @param X \link[base]{data.frame} or \link[base]{list}
#' 
#' @param y \link[base]{vector}
#' 
#' @param mc.cores \link[base]{integer} scalar, see function \link[parallel]{mclapply}
#' 
#' @returns 
#' Function [rpart1.()] returns a \link[stats]{listof} function \link[maxEff]{node1} returns.
#' 
#' @keywords internal
#' @importFrom parallel mclapply detectCores
#' @importFrom maxEff node1
#' @importFrom rpart rpart
#' @export
rpart1. <- function(
    X, 
    y,
    mc.cores = getOption('mc.cores'), 
    ...
) {
  
  if (FALSE) {
  ret <- X |>
    mclapply(mc.cores = mc.cores, FUN = \(x) {
      rpart(
        formula = y ~ x, 
        data = data.frame(y = y, x = x), 
        cp = .Machine$double.eps, # to force a split even if the overall lack of fit is not decreased
        maxdepth = 2L, # only the first node is needed
        ...
      ) |>
        node1()
    }) 
  }
  
  rp <- X |>
    mclapply(mc.cores = mc.cores, FUN = \(x) {
      rpart(
        formula = y ~ x, 
        cp = .Machine$double.eps, # to force a split even if the overall lack of fit is not decreased
        maxdepth = 2L, # only the first node is needed
        ...
      )
    }) 
  
  ret <- mapply(
    FUN = node1, 
    object = rp, 
    nm = X |> names() |> lapply(FUN = as.symbol),
    SIMPLIFY = FALSE)
  
  class(ret) <- 'listof'
  return(ret)
  
}









