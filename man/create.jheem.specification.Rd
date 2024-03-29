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
  parent.version = NULL,
  do.not.inherit.model.quantity.names = character(),
  do.not.inherit.transitions.for.dimension = character(),
  compartments.for.infected.only,
  compartments.for.uninfected.only,
  compartments.for.infected.and.uninfected,
  transmission.modes,
  age.endpoints,
  compartment.value.aliases,
  enable.perinatal.transmission,
  parent.child.concordant.dimensions,
  all.births.into.compartments,
  fix.strata.sizes.prior.to = NULL,
  fix.strata.sizes.for.dimensions = NULL
)
}
\arguments{
\item{version}{A single character value denoting the version}

\item{iteration}{A single character or numeric value denoting the iteration of the model specification for this version}

\item{description}{A short text description of this version}

\item{parent.version}{The character version of the specification from which this new specification should inherit. Must have been registered}

\item{do.not.inherit.model.quantity.names}{A vector of names of model.quantities which should NOT be inherited from ancestor specifications}

\item{do.not.inherit.transitions.for.dimension}{A vector of names of dimensions for which transitions should NOT be inherited from ancestor specifications}

\item{compartments.for.infected.only, compartments.for.uninfected.only, compartments.for.infected.and.uninfected}{Named lists of character vectors F@specifying the compartments for uninfected and infected groups, or compartments shared by both. The names of the lists represent dimensions, and the values the compartments for each dimension. Compartments can either be string referencing the compartments themselves, or strings representing aliases passed to compartment.value.aliases}

\item{transmission.modes}{A character vector of one or more modes of transmission}

\item{age.endpoints}{Optional. A numeric vector (with at least two elements) giving the endpoints of the age brackets to use for the 'age' dimension. Results in length(age.endpoints)-1 different brackets, where the first bracket spans age.endpoints[1] (inclusive) to age.endpoints[2] (exclusive), the second bracket spans age.endpoints[2] to age.endpoints[3], etc}

\item{compartment.value.aliases}{A named list representing substitutions to be made into compartment names (both in compartments.for.infected.only, compartments.for.uninfected.only, compartments.for.infected.and.uninfected and in subsequent references in registering model quantities). The names of the list represent what is to be replaced. The values of the list can be either (1) character vectors that are substituted in place or (2) functions that take parameter 'location' and return a character vector}

\item{enable.perinatal.transmission}{A logical indicator of whether infection can be passed through birth}

\item{parent.child.concordant.dimensions}{A character vector listing the names of dimensions which must be the same for a child as for their parent (eg, if parent.child.concordant.dimensions='race', then the model will force new births into the model to have the same race as their parent)}

\item{all.births.into.compartments}{A named list, each element of which is a single character or integeter value. The names of the list represent dimensions, and the values are the compartments for those dimensions into which new births must fall}

\item{fix.strata.sizes.prior.to}{A numeric time, prior to which strata sizes across dimensions specified by fix.strata.sizes.for.dimensions are held constant}

\item{fix.strata.sizes.for.dimensions}{A character vector of dimension names across which strata sizes should be held constant prior to fix.strata.sizes.prior.to. Must be a subset of names(compartments.for.infected.and.uninfected)}
}
\description{
Create a Model Specification for Running the JHEEM
}
