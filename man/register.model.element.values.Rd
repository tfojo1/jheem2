% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SPECIFICATION_model_specification.R
\name{register.model.element.values}
\alias{register.model.element.values}
\title{A convenience function to streamline registering multiple model.elements for which we only need to specify a value}
\usage{
register.model.element.values(specification, ..., scale)
}
\arguments{
\item{specification}{The jheem.specification object to modify}

\item{...}{A set of named values. The names correspond to the names of the model elements, and the values should be either (1) numeric objects or (2) functions that take arguments 'location' and 'settings' and returns a numeric value}

\item{scale}{The scale at which the elements apply}
}
\description{
A convenience function to streamline registering multiple model.elements for which we only need to specify a value
}
