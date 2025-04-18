% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LIKELIHOODS_bernoulli_likelihood.R
\name{create.bernoulli.likelihood.instructions}
\alias{create.bernoulli.likelihood.instructions}
\title{Create Bernoulli Likelihood Instructions}
\usage{
create.bernoulli.likelihood.instructions(
  outcome.for.sim,
  years,
  probability.decreasing,
  weights,
  dimensions = character(0),
  levels.of.stratification = NULL,
  name = outcome.for.sim
)
}
\arguments{
\item{outcome.for.sim}{A single character vector specifying the simulation outcome to use.}

\item{years}{A length two numeric vector specifying the baseline year (first) and the year which will be assessed against it (second).}

\item{probability.decreasing}{A single numeric value between 0 and 1 specifying probability that the outcome value is lower in the second year than in the first.}

\item{weights}{A list containing only numeric vectors and objects of class 'jheem.likelihood.weights', which are generated by \code{\link{create.likelihood.weights}}}

\item{dimensions}{A character vector of dimensions, excluding year, from which stratifications will be generated.}

\item{levels.of.stratification}{A numeric vector specifying how the dimensions should be combined to form strata. '0' indicates totals (not stratified) while '1' indicates strata that are each stratified by one dimension at a time, '2' indicates strat that are each stratified by a combination of two dimensions at a time, etc. May not exceed the number of dimensions. Defaults to NULL, which is equivalent to '0'.}
}
\description{
Create Bernoulli Likelihood Instructions
}
