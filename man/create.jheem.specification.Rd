% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SPECIFICATION_model_specification.R
\name{create.jheem.specification}
\alias{create.jheem.specification}
\title{Create a Model Specification for Running the JHEEM}
\usage{
create.jheem.specification(
  version,
  iteration,
  description,
  start.year,
  parent.version = NULL,
  do.not.inherit.model.quantity.names = character(),
  do.not.inherit.model.outcome.names = character(),
  do.not.inherit.transitions.for.dimension = character(),
  do.not.inherit.components.with.tags = character(),
  compartments.for.infected.only = list(),
  compartments.for.uninfected.only = list(),
  compartments.for.infected.and.uninfected = list(),
  age.endpoints = NULL,
  compartment.value.aliases = list(),
  labels = NULL
)
}
\arguments{
\item{version}{A single character value denoting the version}

\item{iteration}{A single character or numeric value denoting the iteration of the model specification for this version}

\item{description}{A short text description of this version}

\item{start.year}{The numeric year at which simulations should start. If NULL inherits the parent specification's start year}

\item{parent.version}{The character version of the specification from which this new specification should inherit. Must have been registered}

\item{do.not.inherit.model.quantity.names}{A vector of names of model.quantities which should NOT be inherited from ancestor specifications}

\item{do.not.inherit.model.outcome.names}{A vector of names of model outcomes which should NOT be inherited from ancestor specifications}

\item{do.not.inherit.transitions.for.dimension}{A vector of names of dimensions for which transitions should NOT be inherited from ancestor specifications}

\item{do.not.inherit.components.with.tags}{A vector of tags for which core components (transmission, natality, mortality, transitions, aging) should NOT be inherited from ancestor specifications}

\item{compartments.for.infected.only, compartments.for.uninfected.only, compartments.for.infected.and.uninfected}{Named lists of character vectors F@specifying the compartments for uninfected and infected groups, or compartments shared by both. The names of the lists represent dimensions, and the values the compartments for each dimension. Compartments can either be string referencing the compartments themselves, or strings representing aliases passed to compartment.value.aliases}

\item{age.endpoints}{Optional. A numeric vector (with at least two elements) giving the endpoints of the age brackets to use for the 'age' dimension. Results in length(age.endpoints)-1 different brackets, where the first bracket spans age.endpoints[1] (inclusive) to age.endpoints[2] (exclusive), the second bracket spans age.endpoints[2] to age.endpoints[3], etc}

\item{compartment.value.aliases}{A named list representing substitutions to be made into compartment names (both in compartments.for.infected.only, compartments.for.uninfected.only, compartments.for.infected.and.uninfected and in subsequent references in registering model quantities). The names of the list represent what is to be replaced. The values of the list can be either (1) character vectors that are substituted in place or (2) functions that take parameter 'location' and return a character vector}

\item{labels}{A named character vector, representing how compartments should be labeled when making figures/charts, etc. Eg c(msm="MSM", idu="PWID")}
}
\description{
Create a Model Specification for Running the JHEEM
}
