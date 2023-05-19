

##----------------------##
##----------------------##
##-- PUBLIC INTERFACE --##
##----------------------##
##----------------------##




##-----------------------##
##-----------------------##
##-- CLASS DEFINITIONS --##
##-----------------------##
##-----------------------##

JHEEM.SIMSET = R6::R6Class(
    'jheem.simset',
    inherit = JHEEM.ENTITY,
    
    public = list(
        initialize = function()
        {
            
        },
        
        get.quantity = function(outcome)
        {
            inner.dim.names = NULL
            rv = sapply(private$i.simulations, function(sim){
                sub.rv = sim$get.quantity
                if (is.null(inner.dim.names))
                    inner.dim.names <<- dimnames(sub.rv)
            })
            
            dim.names = c(inner.dim.names,
                          list(sim=1:self$n.sim))
            
            dim(rv) = sapply(dim.names, length)
            dimnames(rv) = dim.names
            
            rv
        },
        
        get.quantities = function(outcomes)
        {
            inner.dim.names = NULL
            rv = sapply(outcomes, function(outcome){
                sub.rv = self$get.quantity()
                if (is.null(inner.dim.names))
                    inner.dim.names <<- dimnames(sub.rv)
            })
            
            dim.names = c(inner.dim.names,
                          list(outcome=outcomes))
            dim(rv) = sapply(dim.names, length)
            dimnames(rv) = dim.names
            
            rv
        }
        
    ),
    
    active = list(
        
        n.sim = function(value)
        {
            if (missing(value))
                length(private$i.simulations)
            else
                stop("Cannot set 'n.sim' for a jheem.simset - it is read-only")
        },
        
        years = function(value)
        {
            
        },
        
        intervention.code = function(value)
        {
            
        },
        
        intervention = function(value)
        {
            
        }
    ),
    
    private = list(
        
        i.simulations = NULL
        
    )
)