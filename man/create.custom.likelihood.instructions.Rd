% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LIKELIHOODS_custom_likelihood.R
\name{create.custom.likelihood.instructions}
\alias{create.custom.likelihood.instructions}
\title{Create Custom Likelihood Instructions}
\usage{
create.custom.likelihood.instructions(
  name,
  compute.function,
  get.data.function = NULL,
  verbose = F
)
}
\arguments{
\item{compute.function}{A function that takes only two arguments: 'sim', 'data', and 'log'.}
}
\description{
Create instructions for a likelihood that computes using a custom function.
}
