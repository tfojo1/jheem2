# Note: didn't include instruction "codes" in this since we haven't used them

#'@export
create.ifelse.likelihood.instructions = function(...) {
    error.prefix = "Error creating 'jheem.ifelse.likelihood.instructions': "
    # all arguments must be JHEEM.LIKELIHOOD.INSTRUCTIONS and none joint
    sub.instructions = list(...)
    if (any(sapply(sub.instructions, function(x) {!R6::is.R6(x) || !is(x, 'jheem.likelihood.instructions') || is(x, 'jheem.joint.likelihood.instructions')})))
        stop(paste0(error.prefix, "all arguments must be 'jheem.likelihood.instructions' objects and none may 'jheem.joint.likelihood.instructions' objects"))
    
    # all outcomes for sim must be identical
    outcomes = sapply(sub.instructions, function(x) {x$outcome.for.sim})
    if (length(unique(outcomes)) != 1)
        stop(paste0(error.prefix, "all supplied instructions must have the same 'outcome.for.sim'"))
    
    JHEEM.IFELSE.LIKELIHOOD.INSTRUCTIONS$new(sub.instructions = sub.instructions)
}

JHEEM.IFELSE.LIKELIHOOD.INSTRUCTIONS = R6::R6Class(
    'jheem.ifelse.likelihood.instructions',
    inherit = JHEEM.LIKELIHOOD.INSTRUCTIONS,
    
    public = list(
        
        initialize = function(sub.instructions,
                              error.prefix = "Error initializing 'jheem.ifelse.likelihood.instructions: ")
        {
            # sub.instructions must be a list containing only jheem.likelihood.instructions and no joint likelihood instructions
            if (!is.list(sub.instructions) || length(sub.instructions)==0 || any(sapply(sub.instructions, function(x) {!R6::is.R6(x) || !is(x, 'jheem.likelihood.instructions')})))
                stop("Error creating ifelse likelihood instructions: 'sub.instructions' must be a list containing only 'jheem.likelihood.instructions' objects and no 'jheem.joint.likelihood.instructions' objects")
            # all outcomes for sim must be identical
            outcomes = sapply(sub.instructions, function(x) {x$outcome.for.sim})
            if (length(unique(outcomes)) != 1)
                stop(paste0(error.prefix, "all instructions in 'sub.instructions' must have the same 'outcome.for.sim'"))
            private$i.sub.instructions = sub.instructions
        },
        # NOT YET IMPLEMENTED
        equals = function(other)
        {},
        
        instantiate.likelihood = function(version, location, sub.version=NULL, data.manager=get.default.data.manager(), throw.error.if.no.data=F, error.prefix='')
        {
            for (sub.instr in private$i.sub.instructions) {
                rv = tryCatch(
                    {(sub.instr$instantiate.likelihood(version=version,
                                                       location=location,
                                                       sub.version=sub.version,
                                                       data.manager=data.manager,
                                                       throw.error.if.no.data=throw.error.if.no.data,
                                                       error.prefix=error.prefix))},
                    error = function(e) {NULL}
                )
                if (!is.null(rv)) return(rv)
            }
            # If didn't return anything,
            stop("Error instantiating 'jheem.ifelse.likelihood.instructions': All likelihood instructions failed to instantiate")
        }),
    
    private = list(
        i.sub.instructions = NULL
    )
)