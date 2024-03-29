% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HELPERS_array_helpers.R
\name{expand.array}
\alias{expand.array}
\title{Expand an Array to Greater Dimensions}
\usage{
expand.array(to.expand, target.dim.names)
}
\arguments{
\item{to.expand}{The value to be expanded. Can be either (1) an array or matrix with named dimensions and dimnames set, (2) a scalar value, or (3) a named vector whose names are a superset of one of the elements of target.dim.names}

\item{target.dim.names}{A named list containing the values of dimensions to subset for. The names of dimension.values should correspond to the names of dimnames. The elements of dimension.values should be character, integer, or logical vectors}
}
\description{
Expand an Array to Greater Dimensions
}
