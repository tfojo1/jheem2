#' @title Create Custom Likelihood Instructions
#' @description
#' Create instructions for a likelihood that computes using a custom function.
#' @inheritParams create.basic.likelihood.instructions
#' @param compute.function A function that takes only two arguments: 'sim', 'data', and 'log'.
#' @export
create.custom.likelihood.instructions <- function(name,
                                                  compute.function,
                                                  get.data.function = NULL,
                                                  verbose = F) {
    JHEEM.CUSTOM.LIKELIHOOD.INSTRUCTIONS$new(name = name,
                                             compute.function = compute.function,
                                             get.data.function = get.data.function)
}

JHEEM.CUSTOM.LIKELIHOOD.INSTRUCTIONS <- R6::R6Class(
    "jheem.custom.likelihood.instructions",
    inherit = JHEEM.LIKELIHOOD.INSTRUCTIONS,
    public = list(
        initialize = function(name,
                              compute.function,
                              get.data.function) {
            error.prefix = "Error initializing 'jheem.custom.likelihood.instructions': "

            # *name* is a single non-NA, non-empty character vector
            if (!is.character(name) || length(name) > 1 || is.null(name) || is.na(name)) {
                stop(paste0(error.prefix, "'name' must be a single non-NA, non-empty character vector"))
            }
            
            # *compute.function* is a function taking only args "sim", "data" and "log"
            if (!is.function(compute.function))
                stop(paste0(error.prefix, "'compute.function' must be a function"))
            if (!setequal(names(formals(compute.function)), c("sim", "log")) &&
                !setequal(names(formals(compute.function)), c("sim", "data", "log")))
                stop(paste0(error.prefix, "'compute.function' must be a function taking only arguments ('sim', 'data' and 'log') or only ('sim' and 'log')"))
            
            # *get.data.function* is a function taking only args "version" and "location"
            if (is.null(get.data.function))
            {}
            else if (!is.function(get.data.function) || !setequal(names(formals(get.data.function)), c("version", "location")))
                stop(paste0(error.prefix, "'get.data.function' must be a function taking only arguments 'version' and 'location'"))
            
            private$i.name = name
            private$i.compute.function = compute.function
            private$i.get.data.function = get.data.function
        },
        instantiate.likelihood = function(version,
                                          location,
                                          verbose=F) {
            JHEEM.CUSTOM.LIKELIHOOD$new(instructions = self,
                                        version=version,
                                        location=location)
        },
        equals = function(other) {}
    ),
    active = list(
        compute.function = function(value) {
            if (missing(value)) {
                private$i.compute.function
            } else {
                stop("Cannot modify a jheem.likelihood.instruction's 'compute.function' - it is read-only")
            }
        },
        get.data.function = function(value) {
            if (missing(value)) {
                private$i.get.data.function
            } else {
                stop("Cannot modify a jheem.likelihood.instruction's 'get.data.function' - it is read-only")
            }
        }
    ),
    private = list(
        i.compute.function = NULL,
        i.get.data.function = NULL
    )
)

JHEEM.CUSTOM.LIKELIHOOD <- R6::R6Class(
    "jheem.custom.likelihood",
    inherit = JHEEM.LIKELIHOOD,
    portable = F, # necessary??
    public = list(
        ### now takes a 'get.data' function takes 'version' and 'location'
        initialize = function(instructions,
                              version,
                              location,
                              verbose) {
            
            # Purposely SKIP the super$initialize for likelihoods (STILL TRUE? CONSIDER REVISING THIS BECAUSE WE USE VERSION/LOCATION NOW)
            
            private$i.name <- instructions$name
            private$i.compute.function <- instructions$compute.function
            private$i.check.consistency.flag <- T
            
            private$i.compute.function.takes.data = any(names(formals(private$i.compute.function)) == 'data')
            
            if (is.null(instructions$get.data.function))
                private$i.data = NULL
            else
            {
                private$i.data <- instructions$get.data.function(version, location)
#                tryCatch({private$i.data <- instructions$get.data.function(version, location)},
#                         error=function(e) {stop(paste0("Error instantiating likelihood '", private$i.name, "': error in 'get.data.function'"))})
            }
        },
        check = function() {browser()}
    ),
    private = list(
        i.compute.function = NULL,
        i.compute.function.takes.data = NULL,
        i.data = NULL,
        
        do.compute = function(sim, log, use.optimized.get, check.consistency, debug) {
            if (private$i.compute.function.takes.data)
                likelihood = private$i.compute.function(sim=sim, data=private$i.data, log=log)
            else
                likelihood = private$i.compute.function(sim=sim, log=log)
            
            error.prefix = paste0("Error computing custom likelihood '", self$name, "': ")
            
            if (!is.numeric(likelihood) || length(likelihood)!=1 || is.na(likelihood))
            {
                .GlobalEnv$errored.likelihood = self
                .GlobalEnv$errored.sim = self
                stop(paste0(error.prefix, "the likelihood value returned from the 'compute.function' for '", self$name,
                            "' must be a single numeric value. The simulation and likelihood which produced this error have been stored in the global environment as 'errored.sim' and 'errored.likelihood'"))
            }
            # error check: non-negative unless log
            if (!log && likelihood<0)
                stop(paste0(error.prefix, "the likelihood value returned from the 'compute.function' must be non-negative if 'log' is FALSE"))
            if (debug) browser()
            likelihood
        }
    )
)