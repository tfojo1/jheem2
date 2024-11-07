#' @title Make a single (joint) set of likelihood instructions out of two or more likelihood instructions objects
#'
#' @param ... One or more jheem.likelihood.instructions objects or lists which contain only jheem.likelihood.instructions objects #OR it could just be codes
#'
#' @export
join.likelihood.instructions <- function(...) {
    # Each argument is either an instructions object or a code.
    sub.instructions <- lapply(list(...), function(x) {
        if (is(x, "jheem.likelihood.instructions")) {
            x
        } else {
            get.likelihood.instructions(code = x)
        }
    })

    JHEEM.JOINT.LIKELIHOOD.INSTRUCTIONS$new(sub.instructions = sub.instructions)
}


JHEEM.JOINT.LIKELIHOOD.INSTRUCTIONS <- R6::R6Class(
    "jheem.joint.likelihood.instructions",
    inherit = JHEEM.LIKELIHOOD.INSTRUCTIONS,
    public = list(
        initialize = function(sub.instructions) {
            # Validate sub-instructions
            # Make sure to flatten out list of sub-instructions

            for (sub.instr in sub.instructions) {
                private$i.outcomes <- paste(private$i.outcomes, sub.instr$outcomes, sep = "__")
                if (is(sub.instr, "jheem.joint.likelihood.instructions")) {
                    private$i.sub.instructions <- c(private$i.sub.instructions, sub.instr$sub.instructions)
                } else {
                    private$i.sub.instructions <- c(private$i.sub.instructions, sub.instr)
                }
            }
            private$i.outcomes <- trimws(private$i.outcomes, "left", "[__]")
            names(private$i.sub.instructions) <- strsplit(private$i.outcomes, "__")[[1]]
        },
        equals = function(other) {
            if (!is.null(self$code) && !is.null(other$code)) {
                self$code == other$code
            } else {
                if (is(other, "jheem.joint.likelihood.instructions")) {
                    all(sapply(self$sub.instructions, function(x) {
                        any(sapply(other$sub.instructions, function(y) {
                            x$equals(y)
                        }))
                    }))
                } else {
                    F
                }
            }
        },
        instantiate.likelihood = function(version, location, sub.version = NULL, data.manager = get.default.data.manager(), throw.error.if.no.data = F, error.prefix = "") {
            # Validate location
            # Check that these instructions are registered?
            # Make sure all outcomes for the instructions are registered to the specification of the version
            JHEEM.JOINT.LIKELIHOOD$new(
                instructions = self,
                version = version,
                sub.version = sub.version,
                location = location,
                data.manager = data.manager,
                throw.error.if.no.data = throw.error.if.no.data,
                error.prefix = error.prefix
            )
        }
    ),
    active = list(

        # @Andrew - fill in getters
        sub.instructions = function(value) {
            if (missing(value)) {
                private$i.sub.instructions
            }
        },
        details = function(value) {
            if (missing(value)) {
                details <- NULL
                for (instr in private$i.sub.instructions) {
                    details <- rbind(details, instr$details)
                }
            } else {
                stop("Cannot modify a jheem.likelihood.instruction's 'details' - they are read-only")
            }
        },
        outcomes = function(value) {
            if (missing(value)) {
                private$i.outcomes
            } else {
                stop("Cannot modify a jheem.joint.likelihood.instruction's 'outcomes' - they are read-only")
            }
        }
    ),
    private = list(
        i.sub.instructions = NULL,
        i.outcomes = NULL
    )
)


JHEEM.JOINT.LIKELIHOOD <- R6::R6Class(
    "jheem.joint.likelihood",
    inherit = JHEEM.LIKELIHOOD,
    portable = F,
    public = list(
        initialize = function(instructions,
                              version,
                              sub.version,
                              location,
                              data.manager,
                              throw.error.if.no.data,
                              error.prefix) {
            super$initialize(
                instructions = instructions,
                version = version,
                sub.version = sub.version,
                location = location,
                error.prefix = error.prefix
            )

            private$i.sub.likelihoods <- lapply(instructions$sub.instructions, function(instr) {
                instr$instantiate.likelihood(
                    version = version,
                    sub.version = sub.version,
                    location = location,
                    data.manager = data.manager,
                    throw.error.if.no.data = throw.error.if.no.data,
                    error.prefix = error.prefix
                )
            })
            names(private$i.sub.likelihoods) <- sapply(private$i.sub.likelihoods, function(lik) {
                lik$outcome.for.sim
            })
            private$i.sub.likelihoods
        },
        get.outcome.location.mapping = function() {
            sub.lik.mappings <- lapply(private$i.sub.likelihoods, function(sub.lik) {
                sub.lik$get.outcome.location.mapping()
            })
            join.outcome.location.mappings(sub.lik.mappings)
        },
        compute.piecewise = function(sim, log = T, use.optimized.get = F, check.consistency = T, error.prefix = "Error computing liklihood: ", debug = F) {
            # why do I need to use return here?
            return(sub.values = sapply(private$i.sub.likelihoods, function(like) {
                like$compute(sim, log = log, use.optimized.get = use.optimized.get, check.consistency = check.consistency, error.prefix = error.prefix, debug = debug)
            }))
        },
        check = function() {
            browser()
        }
    ),
    private = list(
        i.sub.likelihoods = NULL,
        do.compute = function(sim, log, use.optimized.get, check.consistency, debug) {
            sub.values <- sapply(private$i.sub.likelihoods, function(like) {
                like$compute(sim, log = log, use.optimized.get = use.optimized.get, check.consistency = check.consistency, error.prefix = "Error computing liklihood: ", debug)
            })
            # print(sub.values)
            if (log) {
                sum(sub.values)
            } else {
                prod(sub.values)
            }
        }
    )
)
