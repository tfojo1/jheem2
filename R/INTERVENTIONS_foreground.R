
#'@name Create a Model Foreground Object
#'
#'@details A model foreground object applies multiple effects to multiple subpopulations *simultaneously*, with all effects targeting the same model quantity
#'
#'@param ... The intervention.effects and target.populations for this foreground. Can be either: (1) an alternating set of jheem.intervention.effect objects and target.population objects (ie, one intervention.effect followed by one target.population, then the next pair, or vice versa) or (2) one list of intervention effects and one list of target populations, both of the same length
#'
#'@export
create.model.foreground <- function(..., error.prefix='')
{
    args = list(...)
    invalid.dot.dot.dot.msg = paste0(error.prefix, "The ... arguments to create.model.foreground() must be either (1) an alternating set of jheem.intervention.effect objects and target.population objects or (2) two lists, one of intervention.effect objects and one of target.population objects")
    if (is(args[[1]], 'target.population') || is(args[[1]], 'jheem.intervention.effect')) # we're doing the alternating thing
    {
        intervention.effects = list()
        target.populations = list()
        
        if (any(!sapply(args, is, 'jheem.intervention.effect') & !sapply(args, is, 'target.population')))
            stop(invalid.dot.dot.dot.msg)
        
        if (length(args)%%2 == 1) #ie odd number of args
            stop(paste0(error.prefix, "The ... arguments to create.model.foreground() must comprise an EVEN number of elements"))

        expected.types = c('jheem.intervention.effect', 'target.population')
        if (is(args[[1]], 'target.population'))
            expected.types = rev(expected.types)
        
        i = 1
        while (i < length(args))
        {
            if (!is(args[[i]], expected.types[1]) || !is(args[[i+1]], expected.types[2]))
                stop(paste0(error.prefix, "The ... arguments to create.model.foreground() must be an *ALTERNATING* set of jheem.intervention.effect objects and target.population objects"))
            
            if (expected.types[1]=='jheem.intervention.effect')
            {
                intervention.effects = c(intervention.effects, args[i])
                target.populations = c(target.populations, args[i+1])
            }
            else
            {
                target.populations = c(target.populations, args[i])
                intervention.effects = c(intervention.effects, args[i+1])
            }
        }
    }
    else # we're doing the two lists thing
    {
        if (length(args) != 2 || !is.list(args[[1]]) || !is.list(args[[2]]))
            stop(invalid.dot.dot.dot.msg)
        
        if (!(all(sapply(args[[1]], is, 'jheem.intervention.effect')) && all(sapply(args[[2]], is, 'target.population'))) &&
            !(all(sapply(args[[2]], is, 'jheem.intervention.effect')) && all(sapply(args[[1]], is, 'target.population'))))
            stop(invalid.dot.dot.dot.msg)
        
        if (is(args[[1]][[1]], 'jheem.intervention.effect'))
        {
            intervention.effects = args[[1]]
            target.populations = args[[2]]
        }
        else
        {
            target.populations = args[[2]]
            intervention.effects = args[[1]]
        }
        
        if (length(intervention.effects) != length(target.populations))
            stop("If a list of jheeem.intervention.effect objects and a list of target.population objects are passed to ... for create.model.foreground(), they must be the same length")
    }
    
    JHEEM.MODEL.FOREGROUND$new(intervention.effects = intervention.effects,
                               target.populations = target.populations,
                               error.prefix = error.prefix)
}

JHEEM.MODEL.FOREGROUND = R6::R6Class(
    'jheem.model.foreground',
    
    public = list(
        
        initialize = function(intervention.effects,
                              target.populations,
                              error.prefix)
        {
            # Intervention effects is a list with only intervention.effect objects
            # Target population is a list with only target.population objects
            
            # intervention.effects and target.populations have the same length
            # intervention.effects all target the same quantity
            
            # target.populations do not overlap
            
            # Store variables
        },
        
        resolve.target.populations = function(specification.metadata, error.prefix)
        {
            
        },
        
        resolve.effects = function(parameters, error.prefix)
        {
            
        },
        
        get.all.times.for.scale = function(scale)
        {
            
        },
        
        get.effects.for.scale = function(scale)
        {
            private$.intervention.effects[self$get.effect.indices.for.scale(scale)]
        },
        
        get.effect.indices.for.scale = function(scale)
        {
            
        }
    ),
    
    active = list(
        
        name = function(value)
        {
            
        },
        
        quantity.name = function(value)
        {
            
        },
        
        n.effects = function(value)
        {
            
        },
        
        target.populations = function(value)
        {
            
        },
        
        target.population.masks = function(value)
        {
            
        },
        
        effects = function(value)
        {
            
        },
        
        depends.on = function(value)
        {
            
        },
        
        target.populations.are.resolved = function(value)
        {
            
        },
        
        effects.are.resolved = function(value)
        {
            
        },
        
        min.start.time = function(value)
        {
            
        },
        
        max.end.time = function(value)
        {
            
        },
        
        all.effect.times = function(value)
        {
            
        },
        
        distinct.scales = function(value)
        {
            
        },
        
        scales = function(value)
        {
            
        },
        
        start.time.by.effect = function(value)
        {
            
        },
        
        end.time.by.effect = function(value)
        {
            
        },
        
        min.effect.time.by.effect = function(value)
        {
            
        },
        
        max.effect.time.by.effect = function(value)
        {
            
        },
        
        scale.by.effect = function(value)
        {
            
        }
    ),
    
    private = list(
        
        i.intervention.effects = NULL,
        i.target.populations = NULL
    )
)

COMPILED.MODEL.FOREGROUND = R6::R6Class(
    'compiled.model.foreground',
    inherit = JHEEM.ENTITY,
    portable = F,
    
    public = list(
        
        initialize = function(foreground,
                              ontology,
                              version,
                              location,
                              error.prefix)
        {
            # Call the super-class constructor
            
            # ontology is an ontology object or valid dim.names
            # foreground is a jheem.model.foreground object
        }
    ),
    
    active = list(
        
        n = function(value)
        {
            
        },
        
        target.population.masks = function(value)
        {
            
        },
        
        intervention.effects = function(value)
        {
            
        },
        
        depends.on = function(value)
        {
            
        }
    )
)