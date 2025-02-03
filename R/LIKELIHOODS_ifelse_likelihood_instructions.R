# Note: didn't include instruction "codes" in this since we haven't used them

#' @title Create If-Else Likelihood Instructions
#'
#' @details Upon being instantiated, these instructions will attempt to instantiate the first provided set of instructions. If that fails, it will try the next, etc.
#'
#' @param ... A list of JHEEM likelihood instructions in order of preference
#'
#' @export
create.ifelse.likelihood.instructions <- function(...) {
    error.prefix <- "Error creating 'jheem.ifelse.likelihood.instructions': "
    # all arguments must be JHEEM.LIKELIHOOD.INSTRUCTIONS and none joint
    sub.instructions <- list(...)
    if (any(sapply(sub.instructions, function(x) {
        !R6::is.R6(x) || !is(x, "jheem.likelihood.instructions") || is(x, "jheem.joint.likelihood.instructions")
    }))) {
        stop(paste0(error.prefix, "all arguments must be 'jheem.likelihood.instructions' objects and none may 'jheem.joint.likelihood.instructions' objects"))
    }

    # all outcomes for sim must be identical
    outcomes <- sapply(sub.instructions, function(x) {
        x$outcome.for.sim
    })
    if (length(unique(outcomes)) != 1) {
        stop(paste0(error.prefix, "all supplied instructions must have the same 'outcome.for.sim'"))
    }

    JHEEM.IFELSE.LIKELIHOOD.INSTRUCTIONS$new(sub.instructions = sub.instructions)
}

JHEEM.IFELSE.LIKELIHOOD.INSTRUCTIONS <- R6::R6Class(
    "jheem.ifelse.likelihood.instructions",
    inherit = JHEEM.LIKELIHOOD.INSTRUCTIONS,
    public = list(
        initialize = function(sub.instructions,
                              error.prefix = "Error initializing 'jheem.ifelse.likelihood.instructions: ") {
            # sub.instructions must be a list containing only jheem.likelihood.instructions and no joint likelihood instructions
            if (!is.list(sub.instructions) || length(sub.instructions) == 0 || any(sapply(sub.instructions, function(x) {
                !R6::is.R6(x) || !is(x, "jheem.likelihood.instructions")
            }))) {
                stop("Error creating ifelse likelihood instructions: 'sub.instructions' must be a list containing only 'jheem.likelihood.instructions' objects and no 'jheem.joint.likelihood.instructions' objects")
            }
            # all outcomes for sim must be identical
            outcomes <- sapply(sub.instructions, function(x) {
                x$outcome.for.sim
            })
            if (length(unique(outcomes)) != 1) {
                stop(paste0(error.prefix, "all instructions in 'sub.instructions' must have the same 'outcome.for.sim'"))
            }
            private$i.sub.instructions <- sub.instructions
        },
        # NOT YET IMPLEMENTED
        equals = function(other) {},
        instantiate.likelihood = function(version,
                                          location,
                                          sub.version = NULL,
                                          data.manager = get.default.data.manager(),
                                          throw.error.if.no.data = F,
                                          verbose = F,
                                          error.prefix = "") {
            for (sub.instr in private$i.sub.instructions) {
                rv <- tryCatch( # won't work for custom right now!
                    {
                        do.ifelse.instantiate.likelihood(sub.instr,
                                                         version = version,
                                                         location = location,
                                                         sub.version = sub.version,
                                                         data.manager = data.manager,
                                                         additional.weights = list(),
                                                         throw.error.if.no.data = throw.error.if.no.data,
                                                         verbose = verbose,
                                                         error.prefix = error.prefix)
                    },
                    error = function(e) {
                        NULL
                    }
                )
                if (!is.null(rv)) {
                    return(rv)
                }
            }
            # If didn't return anything,
            stop("Error instantiating 'jheem.ifelse.likelihood.instructions': All likelihood instructions failed to instantiate")
        }
    ),
    active = list(
        sub.instructions = function(value) {
            if (missing(value)) {
                private$i.sub.instructions
            } else {
                stop("Cannot modify a jheem.likelihood.instruction's 'sub.instructions' - they are read-only")
            }
        }
    ),
    private = list(
        i.sub.instructions = NULL
    )
)

do.ifelse.instantiate.likelihood <- function(instructions,
                                             version,
                                             sub.version,
                                             location,
                                             data.manager,
                                             additional.weights,
                                             throw.error.if.no.data,
                                             verbose,
                                             error.prefix) {
    if (is.null(error.prefix)) {
        error.prefix <- paste0("Error initializing likelihood for '", instructions$outcome.for.sim, "': ")
    }
    # *error.prefix* is a single non-NA, non-empty character vector
    if (!is.character(error.prefix) || length(error.prefix) > 1 || is.null(error.prefix) || is.na(error.prefix)) {
        stop(paste0(error.prefix, "'error.prefix' must be a single non-NA, non-empty character vector"))
    }

    for (sub.instr in instructions$sub.instructions) {
        rv <- tryCatch( # won't work for custom right now!
            {
                do.instantiate.likelihood(sub.instr,
                                          version = version,
                                          location = location,
                                          sub.version = sub.version,
                                          data.manager = data.manager,
                                          additional.weights = additional.weights,
                                          throw.error.if.no.data = throw.error.if.no.data,
                                          verbose = verbose,
                                          error.prefix = error.prefix)
            },
            error = function(e) {
                NULL
            }
        )
        if (!is.null(rv)) {
            return(rv)
        }
    }
}
