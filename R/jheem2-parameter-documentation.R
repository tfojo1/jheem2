#' @title JHEEM Parameters
#' @param version The version of the model specification (must have been previously registered)
#' @param sub.version
#' @param location
#' @param data.manager A jheem.data.manager object
#' @param error.prefix (Optional) A message to prepend to error messages.
#' @param sim A 'jheem.simulation.set' object
#' @param simset A 'jheem.simulation.set' object
#' @param likelihood A 'jheem.likelihood' object
#' @param likelihood.instructions A 'jheem.likelihood.instructions' object
#' @param debug (dev argument)
#' @param dimension.values
#' @param verbose
#' @name jheem2-params
NULL

#' @title JHEEM Data Manager Parameters
#' @inheritParams jheem2-params
#' @param metric The type of measurement. The default value is "estimate", but other options include "cv", "variance", and "sd".
#' @param ont An ontology object as created by \code{\link{ontology()}}
#' @param outcomes A character vector with the names of outcomes previously registered to this data manager
#' @param sources A character vector with the names of sources previously registered to this data manager
#' @name jheem2-data-manager-params
NULL

#' @title JHEEM Calibration Parameters
#' @inheritParams jheem2-params
#' @param calibration.code
#' @param root.dir
#' @name jheem2-calibration-params
NULL

#' @title JHEEM Likelihood Parameters
#' @inheritParams  jheem2-params
#' @param log Whether to use log likelihood
#' @param instructions A 'jheem.likelihood.instructions' object
#' @name jheem2-likelihood-params
NULL

#' @title JHEEM Simulation Set Parameters
#' @inheritParams jheem2-params
#' @param match.names A regex to select parameters by name. If NULL, all parameters are used.
#' @param chains Which chains to use, defaulting to all.
#' @name jheem2-simset-params
NULL
