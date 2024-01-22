
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
            
            i = i+2
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
            target.populations = args[[1]]
            intervention.effects = args[[2]]
        }
        
        if (length(intervention.effects) != length(target.populations))
            stop("If a list of jheeem.intervention.effect objects and a list of target.population objects are passed to ... for create.model.foreground(), they must be the same length")
    }
    
    # Make sure Intervention.effects all target the same quantity
    quantity.names = unique(sapply(intervention.effects, function(eff){
        eff$quantity.name
    }))
    
    if (length(quantity.names)>1)
        stop(paste0(error.prefix, "All the intervention.effects in a model.foreground must target the same quantity. The given intervention effects target ",
                    length(quantity.names), " quantities: ",
                    collapse.with.and("'", quantity.names, "'")))
    
    # Make sure target.populations do not overlap
    if (length(target.populations)>1)
    {
        for ( i in 1:(length(target.populations)-1) )
        {
            for (j in (i+1):length(target.populations))
            {
                if (target.populations[[i]]$overlaps(target.populations[[j]]))
                    stop(paste0(error.prefix, "target.populations for a model.foreground cannot overlap, but target populations ",
                                target.populations[[i]]$get.name(), " and ",
                                target.populations[[j]]$get.name(), " DO overlap"))
            }
        }
    }
    
    JHEEM.MODEL.FOREGROUND$new(intervention.effects = intervention.effects,
                               target.populations = target.populations,
                               error.prefix = error.prefix)
}

JHEEM.MODEL.FOREGROUND = R6::R6Class(
    'jheem.model.foreground',
    
    public = list(
        
        # This is largely a 'dumb' constructor
        # Consistency checks are done by the create.model.foreground wrapper when we're getting stuff from the user
        # Other calls to the constructor come from within the object, so we assume those are formed correctly
        #
        # Characteristics that we presume are true:
        # - Intervention effects is a list with only intervention.effect objects
        # - Intervention.effects all target the same quantity
        # - Target population is a list with only target.population objects
        # - Intervention.effects and target.populations have the same length
        # - target.populations (and target.population.masks) do not overlap
        #
        initialize = function(intervention.effects,
                              target.populations,
                              target.population.masks = NULL,
                              version = NULL,
                              location = NULL,
                              effects.are.resolved = NULL,
                              effect.times.are.resolved = NULL,
                              depends.on = NULL,
                              min.start.time = NULL,
                              max.end.time = NULL,
                              all.effect.times = NULL,
                              all.effect.step.change.times = NULL,
                              distinct.scales = NULL,
                              target.population.dimensions = NULL,
                              error.prefix)
        {
            # Store variables
            private$i.intervention.effects = intervention.effects
            private$i.target.populations = target.populations
            private$i.target.population.masks = target.population.masks
            private$i.version = version
            private$i.location = location
            
            private$i.quantity.name = private$i.intervention.effects[[1]]$quantity.name
            
            if (is.null(effects.are.resolved))
            {
                private$i.effects.are.resolved = all(sapply(private$i.intervention.effects, function(eff){
                    eff$is.resolved
                }))
            }
            else
                private$i.effects.are.resolved = effects.are.resolved
            
            if (is.null(effect.times.are.resolved))
            {
                private$i.effect.times.are.resolved = all(sapply(private$i.intervention.effects, function(eff){
                    eff$times.are.resolved
                }))
            }
            else
                private$i.effect.times.are.resolved = effect.times.are.resolved
            
            if (is.null(depends.on))
            {
                private$i.depends.on = character()
                if (!private$i.effects.are.resolved)
                {
                    for (eff in private$i.intervention.effects)
                        private$i.depends.on = union(private$i.depends.on,
                                                     eff$depends.on)
                }
            }
            else
                private$i.depends.on = depends.on
            
            if (is.null(min.start.time))
            {
                if (private$i.effect.times.are.resolved)
                {
                    private$i.min.start.time = min(sapply(private$i.intervention.effects, function(eff){
                        eff$start.time
                    }))
                }
                else
                    private$i.min.start.time = NULL
            }
            else
                private$i.min.start.time = min.start.time
            
            if (is.null(max.end.time))
            {
                if (private$i.effect.times.are.resolved)
                {
                    private$i.max.end.time = max(sapply(private$i.intervention.effects, function(eff){
                        eff$end.time
                    }))
                }
                else
                    private$i.max.end.time = NULL
            }
            else
                private$i.max.end.time = max.end.time
            
            if (is.null(all.effect.times))
            {
                if (private$i.effect.times.are.resolved)
                {
                    private$i.all.effect.times = union_sorted_vectors(
                        sapply(private$i.intervention.effects, function(eff){
                            eff$all.times
                        }))
                }
                else
                    private$i.all.effect.times = NULL
            }
            else
                private$i.all.effect.times = all.effect.times
            
            if (is.null(all.effect.step.change.times))
            {
                if (private$i.effect.times.are.resolved)
                {
                    private$i.all.effect.step.change.times = union_sorted_vectors(
                        sapply(private$i.intervention.effects, function(eff){
                            eff$step.change.times
                        }))
                }
                else
                    private$i.all.effect.step.change.times = NULL
            }
            else
                private$i.all.effect.step.change.times = all.effect.step.change.times
            
            if (private$i.effect.times.are.resolved)
                private$i.min.effect.time.by.effect = sapply(private$i.intervention.effects, function(eff){
                    eff$times[1]
                })
            else
                private$i.min.effect.time.by.effect = NULL
            
            if (private$i.effect.times.are.resolved)
                private$i.max.effect.time.by.effect = sapply(private$i.intervention.effects, function(eff){
                    eff$times[length(eff$times)]
                })
            else
                private$i.max.effect.time.by.effect = NULL
            
            if (is.null(distinct.scales))
                private$i.distinct.scales = unique(self$scales)
            else
                private$i.distinct.scales = distinct.scales

            if (is.null(target.population.dimensions))
            {
                private$i.target.population.dimensions = character()
                for (tpop in private$i.target.populations)
                    private$i.target.population.dimensions = union(
                        private$i.target.population.dimensions,
                        tpop$dimensions
                    )
            }
            else
                private$i.target.population.dimensions = target.population.dimensions
        },
        
        anchor = function(location, 
                          specification.metadata,
                          quantity.dim.names,
                          error.prefix = '')
        {
            if (!is.null(private$i.location) || !is.null(private$i.version))
                stop(paste0(error.prefix, "The foreground has already been anchored (to '", location, "' and version '", private$i.version, "')"))
            
            # Check arguments
            if (!is.character(location) || length(location)!=1 || is.na(location))
                stop(paste0(error.prefix, "'location' must be a single, non-NA character value"))
            
            if (!is(specification.metadata, 'specification.metadata'))
                stop(paste0(error.prefix, "'specification.metadata' must be an object of class 'specification.metadata'"))
            
            check.dim.names.valid(quantity.dim.names,
                                  variable.name.for.error = 'quantity.dim.names', 
                                  allow.empty = F, 
                                  allow.duplicate.values.across.dimensions = T,
                                  error.prefix = error.prefix)
            
            # Render tpop masks
            tpop.masks = lapply(private$i.target.populations, function(tpop){
                tpop$render.population.mask(specification.metadata,
                                            dim.names = quantity.dim.names,
                                            render.only.relevant.dimensions = T,
                                            error.prefix = error.prefix)
            })

            # Make sure target.population.masks do not overlap
            if (length(private$i.target.populations)>1)
            {
                for ( i in 1:(length(private$i.target.populations)-1) )
                {
                    for (j in (i+1):length(private$i.target.populations))
                    {
                        if (any(tpop.masks[[i]] & tpop.masks[[j]]))
                            stop(paste0(error.prefix, "target.populations for a model.foreground cannot overlap, but target populations ",
                                        private$i.target.populations[[i]]$get.name(), " and ",
                                        private$i.target.populations[[j]]$get.name(), " DO overlap"))
                    }
                }
            }
            
            # Anchor the intervention effects
            effects = lapply(private$i.intervention.effects, function(eff){
                 eff$anchor.location.and.version(location = location,
                                                 specification.metadata = specification.metadata,
                                                 error.prefix = error.prefix)
            })
               
            # Return a new (resolved) object 
            JHEEM.MODEL.FOREGROUND$new(intervention.effects = effects,
                                       target.populations = private$i.target.populations,
                                       target.population.masks = tpop.masks,
                                       location = location,
                                       version = specification.metadata$version,
                                       effects.are.resolved = private$i.effects.are.resolved,
                                       effect.times.are.resolved = private$i.effect.times.are.resolved,
                                       depends.on = private$i.depends.on,
                                       min.start.time = private$i.min.start.time,
                                       max.end.time = private$i.max.end.time,
                                       all.effect.times = private$i.all.effect.times,
                                       all.effect.step.change.times = private$i.all.effect.step.change.times,
                                       distinct.scales = private$i.distinct.scales,
                                       target.population.dimensions = private$i.target.population.dimensions,
                                       error.prefix = error.prefix)
            
        },
        
        resolve.effects = function(parameters, error.prefix)
        {
            # Check arguments
            if (self$effects.are.resolved)
                stop(paste0(error.prefix, "The foreground's effects have already been resolved"))
            
            # Resolve each effect
            bindings = as.list(parameters)
            
            resolved.effects = lapply(private$i.intervention.effects, function(eff){
                eff$resolve(bindings = bindings,
                            error.prefix = error.prefix)
            })
            
            # Return a new (resolved) object
            JHEEM.MODEL.FOREGROUND$new(intervention.effects = resolved.effects,
                                       target.populations = private$i.target.populations,
                                       target.population.masks = private$i.target.population.masks,
                                       version = private$i.version,
                                       effects.are.resolved = private$i.effects.are.resolved,
                                       effect.times.are.resolved = private$i.effect.times.are.resolved,
                                       depends.on = private$i.depends.on,
                                       min.start.time = private$i.min.start.time,
                                       max.end.time = private$i.max.end.time,
                                       all.effect.times = private$i.all.effect.times,
                                       all.effect.step.change.times = private$i.all.effect.step.change.times,
                                       distinct.scales = private$i.distinct.scales,
                                       target.population.dimensions = private$i.target.population.dimensions,
                                       error.prefix = error.prefix)
        },

        effect.times.equal = function(other)
        {
            if (!is(other, 'jheem.model.foreground') ||
                !self$effects.are.resolved || !other$effects.are.resolved || 
                other$n.effects != self$n.effects)
                F
            else
            {
                all(sapply(1:self$n.effects, function(i){
                    e1 = self$effects[[i]]
                    e2 = other$effects[[i]]
                    
                    e1$start.time == e2$start.time &&
                        e1$end.time == e2$end.time &&
                        length(e1$times) == length(e2$times) &&
                        all(e1$times == e2$times)
                }))
            }
        },
        
        equals = function(other)
        {
            if (setequal(class(self), class(other)))
            {
                if (private$i.quantity.name == other$quantity.name &&
                    identical(private$i.version, other$version) &&
                    identical(private$i.location, other$location))
                {
                    if (length(private$i.target.populations) == length(other$target.populations))
                    {
                        unmatched.i2 = 1:length(other$target.populations)
                        
                        for (i1 in 1:length(private$i.target.populations))
                        {
                            tpop1 = private$i.target.populations[[i1]]
                            eff1 = private$i.intervention.effects[[i1]]
                            
                            found.match = F
                            for (i2 in unmatched.i2)
                            {
                                tpop2 = other$target.populations[[i2]]
                                eff2 = other$effects[[i2]]
                                
                                if (tpop1$equals(tpop2) && eff1$equals(eff2))
                                {
                                    found.match = T
                                    unmatched.i2 = unmatched.i2[unmatched.i2 != i2]
                                    break
                                }
                            }
                            
                            if (!found.match)
                                return (F)
                        }
                        
                        T
                    }
                    else
                        F
                }
                else 
                    F
            }
            else
                F  
        }
    ),
    
    active = list(
        
        quantity.name = function(value)
        {
            if (missing(value))
                private$i.quantity.name
            else
                stop("Cannot modify 'quantity.name' for a jheem.model.foreground - it is read-only")
        },
        
        n.effects = function(value)
        {
            if (missing(value))
                length(private$i.intervention.effects)
            else
                stop("Cannot modify 'n.effects' for a jheem.model.foreground - it is read-only")
        },
        
        target.populations = function(value)
        {
            if (missing(value))
                private$i.target.populations
            else
                stop("Cannot modify 'target.populations' for a jheem.model.foreground - they are read-only")
        },
        
        target.population.masks = function(value)
        {
            if (missing(value))
                private$i.target.population.masks
            else
                stop("Cannot modify 'target.population.masks' for a jheem.model.foreground - they are read-only")
        },
        
        target.population.dimensions = function(value)
        {
            if (missing(value))
                private$i.target.population.dimensions
            else
                stop("Cannot modify 'target.population.dimensions' for a jheem.model.foreground - they are read-only")
        },
        
        effects = function(value)
        {
            if (missing(value))
                private$i.intervention.effects
            else
                stop("Cannot modify 'effects' for a jheem.model.foreground - they are read-only")
        },
        
        depends.on = function(value)
        {
            if (missing(value))
                private$i.depends.on
            else
                stop("Cannot modify 'depends.on' for a jheem.model.foreground - it is read-only")
        },
        
        version = function(value)
        {
            if (missing(value))
                private$i.version
            else
                stop("Cannot modify 'version' for a jheem.model.foreground - it is read-only")
        },
        
        is.anchored = function(value)
        {
            if (missing(value))
                !is.null(private$i.location)
            else
                stop("Cannot modify a JHEEM's 'is.anchored' - it is read-only")
        },
        
        location = function(value)
        {
            if (missing(value))
                private$i.location
            else
                stop("Cannot modify 'location' for a jheem.model.foreground - it is read-only")
        },
        
        target.populations.are.resolved = function(value)
        {
            if (missing(value))
                !is.null(private$i.target.population.masks)
            else
                stop("Cannot modify 'target.populations.are.resolved' for a jheem.model.foreground - it is read-only")
        },
        
        effects.are.resolved = function(value)
        {
            if (missing(value))
                private$i.effects.are.resolved
            else
                stop("Cannot modify 'effects.are.resolved' for a jheem.model.foreground - it is read-only")
        },
        
        effect.times.are.resolved = function(value)
        {
            if (missing(value))
                private$i.effect.times.are.resolved
            else
                stop("Cannot modify 'effect.times.are.resolved' for a jheem.model.foreground - it is read-only")
        },
        
        min.start.time = function(value)
        {
            if (missing(value))
                private$i.min.start.time
            else
                stop("Cannot modify 'min.start.time' for a jheem.model.foreground - it is read-only")
        },
        
        max.end.time = function(value)
        {
            if (missing(value))
                private$i.max.end.time
            else
                stop("Cannot modify 'max.end.time' for a jheem.model.foreground - it is read-only")
        },
        
        all.effect.times = function(value)
        {
            if (missing(value))
                private$i.all.effect.times
            else
                stop("Cannot modify 'all.effect.times' for a jheem.model.foreground - it is read-only")
        },
        
        all.effect.step.change.times = function(value)
        {
            if (missing(value))
                private$i.all.effect.step.change.times
            else
                stop("Cannot modify 'all.effect.step.change.times' for a jheem.model.foreground - it is read-only")
        },
        
        distinct.scales = function(value)
        {
            if (missing(value))
                private$i.distinct.scales
            else
                stop("Cannot modify 'distinct.scales' for a jheem.model.foreground - they are read-only")
        },
        
        scales = function(value)
        {
            if (missing(value))
                sapply(private$i.intervention.effects, function(eff){
                    eff$scale
                })
            else
                stop("Cannot modify 'scales' for a jheem.model.foreground - they are read-only")
        },
        
        start.time.by.effect = function(value)
        {
            if (missing(value))
            {
                if (self$effect.times.are.resolved)
                    sapply(private$i.intervention.effects, function(eff){
                        eff$start.time
                    })
                else
                    NULL
            }
            else
                stop("Cannot modify 'start.time.by.effect' for a jheem.model.foreground - it is read-only")
        },
        
        end.time.by.effect = function(value)
        {
            if (missing(value))
            {
                if (self$effect.times.are.resolved)
                    sapply(private$i.intervention.effects, function(eff){
                        eff$end.time
                    })
                else
                    NULL
            }
            else
                stop("Cannot modify 'end.time.by.effect' for a jheem.model.foreground - it is read-only")
        },
        
        min.effect.time.by.effect = function(value)
        {
            if (missing(value))
                private$i.min.effect.time.by.effect
            else
                stop("Cannot modify 'min.effect.time.by.effect' for a jheem.model.foreground - it is read-only")
        },
        
        max.effect.time.by.effect = function(value)
        {
            if (missing(value))
                private$i.max.effect.time.by.effect
            else
                stop("Cannot modify 'max.effect.time.by.effect' for a jheem.model.foreground - it is read-only")
        }
    ),
    
    private = list(
        
        i.quantity.name = NULL,
        
        i.intervention.effects = NULL,
        i.target.populations = NULL,
        
        i.version = NULL,
        i.location = NULL,
        i.target.population.masks = NULL,
        i.target.population.dimensions = NULL,
        
        i.effects.are.resolved = NULL,
        i.effect.times.are.resolved = NULL,
        i.depends.on = NULL,
        
        i.min.start.time = NULL,
        i.max.end.time = NULL,
        i.all.effect.times = NULL,
        i.all.effect.step.change.times = NULL,
        i.min.effect.time.by.effect = NULL,
        i.max.effect.time.by.effect = NULL,
        
        i.distinct.scales = NULL
    )
)
