% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/JHEEM_engine.R
\name{set.element.functional.form.from.time}
\alias{set.element.functional.form.from.time}
\alias{set.element.functional.form.to.time}
\title{Set the Times From and To Which the Functional Form Determines a Model Element's Value}
\usage{
set.element.functional.form.from.time(
  jheem.engine,
  element.name,
  from.time,
  check.consistency = !jheem.engine$has.been.crunched()
)

set.element.functional.form.to.time(
  jheem.engine,
  element.name,
  to.time,
  check.consistency = !jheem.engine$has.been.crunched()
)
}
\arguments{
\item{jheem.engine}{A jheem.engine object (created by \code{\link{create.jheem.engine}})}

\item{element.name}{The name of the model element to set functional.form times for}

\item{from.time, to.time}{The time from or to which the functional form is active}

\item{check.consistency}{A logical indicator whether to sanitize arguments and check for consistency during calculations. Setting to F increases performance}
}
\description{
Set the Times From and To Which the Functional Form Determines a Model Element's Value
}
\section{Functions}{
\itemize{
\item \code{set.element.functional.form.to.time()}: 

}}
\seealso{
Other Functions to create and modify a jheem.engine: 
\code{\link{create.jheem.engine}()},
\code{\link{crunch.jheem.engine}()},
\code{\link{run.jheem.engine}()},
\code{\link{set.element.functional.form.future.slope}()},
\code{\link{set.element.functional.form.interaction.alphas}()},
\code{\link{set.element.functional.form.main.effect.alphas}()},
\code{\link{set.element.ramp.times}()},
\code{\link{set.element.values.from.parameters}()},
\code{\link{set.element.value}()}
}
\concept{Functions to create and modify a jheem.engine}
