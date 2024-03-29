% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/JHEEM_engine.R
\name{set.element.functional.form.interaction.alphas}
\alias{set.element.functional.form.interaction.alphas}
\title{Set Interaction Alpha Values for a Functional Form for a Model Element}
\usage{
set.element.functional.form.interaction.alphas(
  jheem.engine,
  element.name,
  alpha.name,
  value,
  applies.to.dimension.values = names(values),
  dimensions = names(applies.to.dimension.values),
  check.consistency = !jheem.engine$has.been.crunched()
)
}
\arguments{
\item{jheem.engine}{A jheem.engine object (created by \code{\link{create.jheem.engine}})}

\item{element.name}{The name of the model element whose functional form we wish to modify}

\item{alpha.name}{The name of the functional form parameter that we want to set alphas for}

\item{value}{A single numeric value, which will apply to every combination of applies.to.dimension.values which are from different dimensions}

\item{applies.to.dimension.values}{Either a vector (character or integer) or a list where each element is either a single character value or single integer value, indicating the compartments to which value applies}

\item{dimensions}{The dimensions to which values in applies.to.dimension.values apply. Can be either (1) a character vector with the same length as applies.to.dimension.values with the corresponding dimension for each value, (2) a single character value - a dimension that applies to all values, or (3) NULL, in which case the dimensions are inferred for applies.to.dimension.values (this is computationally slower)}

\item{check.consistency}{A logical indicator whether to sanitize arguments and check for consistency during calculations. Setting to F increases performance}
}
\description{
Set Interaction Alpha Values for a Functional Form for a Model Element
}
\details{
Alphas for an 'interaction' will apply to every combination of applies.to.dimension.values from different dimensions (as opposed to 'main effect' apply to all values in a dimension
}
\seealso{
Other Functions to create and modify a jheem.engine: 
\code{\link{create.jheem.engine}()},
\code{\link{crunch.jheem.engine}()},
\code{\link{run.jheem.engine}()},
\code{\link{set.element.functional.form.from.time}()},
\code{\link{set.element.functional.form.future.slope}()},
\code{\link{set.element.functional.form.main.effect.alphas}()},
\code{\link{set.element.ramp.times}()},
\code{\link{set.element.values.from.parameters}()},
\code{\link{set.element.value}()}
}
\concept{Functions to create and modify a jheem.engine}
