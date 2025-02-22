% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_dummies.R
\name{rpart1.}
\alias{rpart1.}
\title{Batch Operation of Function \link{rpart1}}
\usage{
rpart1.(
  X,
  mc.cores = switch(.Platform$OS.type, windows = 1L, detectCores()),
  ...
)
}
\arguments{
\item{X}{\link[base]{data.frame} or \link[base]{list}}

\item{mc.cores}{\link[base]{integer} scalar, see function \link[parallel]{mclapply}}

\item{...}{additional parameters of function \link{rpart1}}
}
\value{
Function \link{rpart1.} returns a \link[base]{list} of \link{rpart1} returns.
}
\description{
Batch Operation of Function \link{rpart1}
}
\keyword{internal}
