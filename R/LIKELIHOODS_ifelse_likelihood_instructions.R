#' @title Create If-Else Likelihood Instructions
#' @description
#' Create instructions that can try a sequence of likelihood instructions until the first one that works without error.
#' 
#' @param ... 'jheem.likelihood.instructions' objects that all have the same 'outcome.for.sim'.
#' @param name Name for the ifelse; will default to 
#' @details The instructions are attempted in the order that they are supplied.
#' @export
create.ifelse.likelihood.instructions <- function(..., name=NULL) {
    do.create.ifelse.likelihood.instructions(list(...), name=name, FUN=get.default.ifelse.function())
}

#' @title Create Location Based If-Else Likelihood Instructions
#' @description
#' Create instructions that can instantiate a different 'jheem.likelihood' depending on the location.
#' 
#' @inheritParams create.ifelse.likelihood.instructions
#' @param locations.list A list of character vectors of locations that will use the corresponding instruction.
#' The list should be length one shorter than the number of instructions. For example, if you have three instructions,
#' the first vector has locations that will use the first instruction and second vector has locations that will
#' use the second instruction. The third instruction will be used for all other locations.
#' @export
create.location.based.ifelse.likelihood.instructions <- function(..., name=NULL, locations.list=list()) {
    error.prefix <- "Error creating 'jheem.ifelse.likelihood.instructions': "
    sub.instructions <- list(...)
    if (!is.list(locations.list) || length(locations.list)!=length(sub.instructions)-1 ||
        any(sapply(locations.list, function(v) {!is.character(v)})))
        stop(paste0(error.prefix, "'locations.list' must be a list of character vectors with length one less than the number of instructions in '...'"))
    do.create.ifelse.likelihood.instructions(sub.instructions, name=name, FUN=get.location.based.ifelse.function(locations.list))
}

do.create.ifelse.likelihood.instructions <- function(sub.instructions, name, FUN=get.default.ifelse.function()) {
    error.prefix <- "Error creating 'jheem.ifelse.likelihood.instructions': "
    # all arguments must be JHEEM.LIKELIHOOD.INSTRUCTIONS and none joint
    if (any(sapply(sub.instructions, function(x) {
        !R6::is.R6(x) || !is(x, "jheem.likelihood.instructions") || is(x, "jheem.joint.likelihood.instructions")
    }))) {
        stop(paste0(error.prefix, "all arguments in '...' must be 'jheem.likelihood.instructions' objects and none may be 'jheem.joint.likelihood.instructions' objects"))
    }
    
    # all outcomes for sim must be identical
    outcomes <- sapply(sub.instructions, function(x) {
        x$outcome.for.sim
    })
    if (length(unique(outcomes)) != 1) {
        stop(paste0(error.prefix, "all supplied instructions must have the same 'outcome.for.sim'"))
    }
    
    if (is.null(name))
        name = sub.instructions[[1]]$name
    
    # FUN must take certain arguments
    JHEEM.IFELSE.LIKELIHOOD.INSTRUCTIONS$new(sub.instructions = sub.instructions,
                                             name = name,
                                             FUN = FUN)
}

# We have to have this separate because we want to keep "additional.weights" out of the class's instantiate
do.ifelse.instantiate.likelihood <- function(instructions,
                                             version,
                                             sub.version,
                                             location,
                                             data.manager,
                                             additional.weights,
                                             throw.error.if.no.data,
                                             verbose,
                                             error.prefix) {
    FUN = instructions$get.FUN()

    resulting.likelihood = FUN(sub.instructions = instructions$sub.instructions,
                               version = version,
                               location = location,
                               sub.version = sub.version,
                               data.manager = data.manager,
                               additional.weights = additional.weights,
                               throw.error.if.no.data = throw.error.if.no.data,
                               verbose = verbose,
                               error.prefix = error.prefix)
}

JHEEM.IFELSE.LIKELIHOOD.INSTRUCTIONS <- R6::R6Class(
    "jheem.ifelse.likelihood.instructions",
    inherit = JHEEM.LIKELIHOOD.INSTRUCTIONS,
    public = list(
        initialize = function(sub.instructions,
                              name,
                              FUN,
                              error.prefix = "Error initializing 'jheem.ifelse.likelihood.instructions: ") {
            # sub.instructions must be a list containing only jheem.likelihood.instructions and no joint likelihood instructions
            if (!is.list(sub.instructions) || length(sub.instructions) == 0 || any(sapply(sub.instructions, function(x) {
                !R6::is.R6(x) || !is(x, "jheem.likelihood.instructions")
            }))) {
                stop(paste0(error.prefix, "'sub.instructions' must be a list containing only 'jheem.likelihood.instructions' objects and no 'jheem.joint.likelihood.instructions' objects"))
            }
            # It has to have at least one element
            if (length(sub.instructions)==0)
                stop(paste0(error.prefix, "'sub.instructions' must contain at least one element"))
            # all outcomes for sim must be identical
            outcomes <- sapply(sub.instructions, function(x) {
                x$outcome.for.sim
            })
            if (length(unique(outcomes)) != 1) {
                stop(paste0(error.prefix, "all instructions in 'sub.instructions' must have the same 'outcome.for.sim'"))
            }
            
            # *name* is a single character vector
            if (!is.character(name) || length(name) > 1 || is.na(name)) {
                stop(paste0(error.prefix, "'name' must be a character vector of length 1"))
            }
            
            private$i.sub.instructions <- sub.instructions
            private$i.name <- name
            private$i.FUN <- FUN
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
            do.ifelse.instantiate.likelihood(instructions = self,
                                             version = version,
                                             location = location,
                                             sub.version = sub.version,
                                             data.manager = data.manager,
                                             additional.weights = list(),
                                             throw.error.if.no.data = throw.error.if.no.data,
                                             verbose = verbose,
                                             error.prefix = error.prefix)
        },
        # This wasn't working as a typical active binding for some reason, maybe related to it being a function.
        get.FUN = function() {private$i.FUN}
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
        i.sub.instructions = NULL,
        i.FUN = NULL
    )
)

get.default.ifelse.function <- function() {
    function(sub.instructions,
             version,
             location,
             sub.version=NULL,
             data.manager=get.default.data.manager(),
             additional.weights = list(),
             verbose=F,
             throw.error.if.no.data=F,
             error.prefix = "Error instantiating likelihood from instructions: ") {
        resulting.likelihood = NULL
        
        for (sub.instr in sub.instructions) {
            resulting.likelihood <- tryCatch(
                {
                    do.instantiate.likelihood(instructions = sub.instr,
                                              version = version,
                                              location = location,
                                              sub.version = sub.version,
                                              data.manager = data.manager,
                                              additional.weights = additional.weights,
                                              throw.error.if.no.data = throw.error.if.no.data,
                                              verbose = verbose,
                                              error.prefix = error.prefix)
                },
                error = function(e) {NULL})
            if (!is.null(resulting.likelihood)) return(resulting.likelihood)
        }
        
        if (is.null(resulting.likelihood))
            stop(paste0(error.prefix, "all alternative likelihoods failed"))
    }
}

get.location.based.ifelse.function <- function(locations.list) {
    function(sub.instructions,
             version,
             location,
             sub.version=NULL,
             data.manager=get.default.data.manager(),
             additional.weights = list(),
             verbose=F,
             throw.error.if.no.data=F,
             error.prefix = "Error instantiating likelihood from instructions: ") {
        
        location.in.list <- sapply(locations.list, function(x) {location %in% x})
        which.instr.to.use <- which(location.in.list)
        if (length(which.instr.to.use)==0) which.instr.to.use <- length(sub.instructions)
        do.instantiate.likelihood(instructions=sub.instructions[[which.instr.to.use]],
                                  version = version,
                                  location = location,
                                  sub.version = sub.version,
                                  data.manager = data.manager,
                                  additional.weights = additional.weights,
                                  throw.error.if.no.data = throw.error.if.no.data,
                                  verbose = verbose,
                                  error.prefix = error.prefix)
    }
}