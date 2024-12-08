% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LIKELIHOODS_main.R
\name{remove.likelihood.instructions}
\alias{remove.likelihood.instructions}
\title{Remove Likelihood Instructions}
\usage{
remove.likelihood.instructions(
  codes,
  throw.error.if.code.not.registered = F,
  error.prefix = "Error removing likelihood instructions: "
)
}
\arguments{
\item{codes}{One or more valid codes}

\item{throw.error.if.code.not.registered}{Should an error be thrown listing any codes that are not already registered?}
}
\description{
Un-registers likelihood instructions by code
}
