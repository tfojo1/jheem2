#' @title Create Custom Likelihood Instructions
#' @description
#' Create instructions for a likelihood that computes using a custom function.
#' @inheritParams create.basic.likelihood.instructions
#' @param compute.function A function that takes only two arguments: 'sim' and 'log'.
#' @export
create.custom.likelihood.instructions <- function(name,
                                                  compute.function,
                                                  verbose = F) {
    JHEEM.CUSTOM.LIKELIHOOD.INSTRUCTIONS$new(name = name,
                                             compute.function = compute.function)
}

JHEEM.CUSTOM.LIKELIHOOD.INSTRUCTIONS <- R6::R6Class(
    "jheem.custom.likelihood.instructions",
    inherit = JHEEM.LIKELIHOOD.INSTRUCTIONS,
    public = list(
        initialize = function(name,
                              compute.function) {
            error.prefix = "Error initializing 'jheem.custom.likelihood.instructions': "

            # *name* is a single non-NA, non-empty character vector
            if (!is.character(name) || length(name) > 1 || is.null(name) || is.na(name)) {
                stop(paste0(error.prefix, "'name' must be a single non-NA, non-empty character vector"))
            }
            
            # *compute.function* is a function taking only args "sim" and "log"
            if (!is.function(compute.function) || !setequal(names(formals(compute.function)), c("sim", "log")))
                stop(paste0(error.prefix, "'compute.function' must be a function taking only arguments 'sim' and 'log'"))
            
            private$i.name = name
            private$i.compute.function = compute.function
        },
        instantiate.likelihood = function() {
            JHEEM.CUSTOM.LIKELIHOOD$new(instructions = self)
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
        }
    ),
    private = list(
        i.compute.function = NULL
    )
)

JHEEM.CUSTOM.LIKELIHOOD <- R6::R6Class(
    "jheem.custom.likelihood",
    inherit = JHEEM.LIKELIHOOD,
    portable = F, # necessary??
    public = list(
        initialize = function(instructions,
                              verbose) {
            
            # Purposely SKIP the super$initialize for likelihoods
            
            private$i.name <- instructions$name
            private$i.compute.function <- instructions$compute.function
            private$i.check.consistency.flag <- T
        },
        check = function() {browser()}
    ),
    private = list(
        i.compute.function = NULL,
        
        do.compute = function(sim, log, use.optimized.get, check.consistency, debug) {
            likelihood = private$i.compute.function(sim, log)
            # error check: non-negative unless log
            if (!is.numeric(likelihood) || length(likelihood)!=1 || is.na(likelihood))
                stop(paste0(error.prefix, "the likelihood value returned from the 'compute.function' must be a single numeric value"))
            if (!log && likelihood<0)
                stop(paste0(error.prefix, "the likelihood value returned from the 'compute.function' must be non-negative if 'log' is FALSE"))
            if (debug) browser()
            likelihood
        }
    )
)