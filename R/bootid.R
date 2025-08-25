
#' @title Bootstrap Indices
#' 
#' @description
#' To generate a \link[stats]{listof} \link[boot]{boot}strap indices.
#' 
#' @param n positive \link[base]{integer} scalar, sample size \eqn{n}
#' 
#' @param R positive \link[base]{integer} scalar, number of bootstrap replicates \eqn{R}
#' 
#' @returns 
#' Function [bootid()] returns a length-\eqn{R} \link[stats]{listof} 
#' positive \link[base]{integer} \link[base]{vector}s.
#' Each element is the length-\eqn{n} indices of each bootstrap sample.
#' 
#' @seealso 
#' Function [bootid()] generates \link[base]{identical} bootstrap indices as 
#' those generated from the default options of function \link[boot]{boot} 
#' (i.e., with parameters `sim = 'ordinary'` and `m = 0`), 
#' see more in functions `boot:::index.array` and `boot:::ordinary.array`.
#' 
#' @examples
#' set.seed(1345); (bt1 = boot::boot(data = 1:10, statistic = \(data, ind) ind, R = 3L)[['t']])
#' set.seed(1345); (bt2 = bootid(10L, R = 3L))
#' stopifnot(identical(c(t(bt1)), unlist(bt2)))
#' @keywords internal
#' @export
bootid <- function(n, R) {
  ret <- sample.int(n = n, size = n * R, replace = TRUE)
  dim(ret) <- c(R, n)
  ret <- R |>
    seq_len() |>
    lapply(FUN = \(i) ret[i,])
  class(ret) <- c('bootid', 'listof')
  return(ret)
}

