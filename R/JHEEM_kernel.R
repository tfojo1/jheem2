
#-- A JHEEM.KERNEL is all the data from a specification object necessary to run one engine in a location
#--  as well as to run interventions on a simulation
#-- These get saved independently of the specification code, and can live on in simulation objects

JHEEM.KERNEL = R6::R6Class(
    'jheem.kernel',
    portable = F,
    
    public = list(
        
        initialize = function(version,
                              location,
                              
                              specification,
                              
                              dependent.quantity.names,
                              dependee.element.names,
                              dependee.quantity.names,
                              dependent.top.level.quantity.names,
                              non.cumulative.dependent.outcome.names,
                              outcome.dependee.element.names,
                              outcome.dependee.quantity.names,
                              outcome.non.cumulative.dependendee.outcome.names,
                              outcome.non.cumulative.direct.dependendee.outcome.names,
                              outcome.non.cumulative.dependent.outcome.names,
                              outcome.direct.dependee.outcome.names,
                              outcome.numerator.direct.dependee.outcome.names,
                              direct.dependent.outcome.numerator.names,
                              outcome.direct.dependee.quantity.names)
        {
            error.prefix = paste0("Error creating '", version, "' specification kernel for '", location, "': ")
            
            # We're not going to error check these
            private$i.version = version
            private$i.location = location
            private$i.created.date = Sys.Date()
            private$i.created.time = Sys.time()
            
            #-- Specification Metadata --#
            private$i.specification.metadata.for.null.sub.version = do.get.specification.metadata(version = version,
                                                                                                  location = location,
                                                                                                  sub.version = NULL)
            private$i.specification.metadata.for.sub.version = lapply(specification$sub.versions, function(sub.version){
                do.get.specification.metadata(version = version,
                                              location = location,
                                              sub.version = sub.version)
            })
            names(private$i.specification.metadata.for.sub.version) = specification$sub.versions
            
            
            # Dependency data structures
            private$i.dependent.quantity.names = dependent.quantity.names
            private$i.dependee.element.names = dependee.element.names
            private$i.dependee.quantity.names = dependee.quantity.names
            private$i.dependent.top.level.quantity.names = dependent.top.level.quantity.names
            private$i.non.cumulative.dependent.outcome.names = non.cumulative.dependent.outcome.names
            private$i.outcome.dependee.element.names = outcome.dependee.element.names
            private$i.outcome.dependee.quantity.names = outcome.dependee.quantity.names
            private$i.outcome.non.cumulative.dependendee.outcome.names = outcome.non.cumulative.dependendee.outcome.names
            private$i.outcome.non.cumulative.direct.dependendee.outcome.names = outcome.non.cumulative.direct.dependendee.outcome.names
            private$i.outcome.non.cumulative.dependent.outcome.names = outcome.non.cumulative.dependent.outcome.names
            private$i.outcome.direct.dependee.outcome.names = outcome.direct.dependee.outcome.names
            private$i.outcome.numerator.direct.dependee.outcome.names = outcome.numerator.direct.dependee.outcome.names
            private$i.direct.dependent.outcome.numerator.names = direct.dependent.outcome.numerator.names
            private$i.outcome.direct.dependee.quantity.names = outcome.direct.dependee.quantity.names
            
            #-- Quantity Stuff --#
            private$i.ordered.quantity.names = specification$ordered.quantity.names
            private$i.ordered.quantity.names.except.initial.population = specification$ordered.quantity.names.except.initial.population
            
            
            private$i.quantity.kernels = lapply(specification$quantities, function(quantity){
                
                list(
                    name = quantity$name,
                    depends.on = quantity$depends.on,
                    
                    original.name = quantity$get.original.name(private$i.version),
                    max.dim.names = quantity$max.dim.names,
                    required.dim.names = quantity$required.dim.names,
                    
                    functional.form.scale = quantity$functional.form.scale,
                    scale = quantity$scale,
                    
                    ramp.value.application = quantity$ramp.value.application,
                    all.ramp.applications.identity = quantity$all.ramp.applications.identity,
                    ramp.interpolate.links = quantity$ramp.interpolate.links,
                    
                    taper.value.application = quantity$taper.value.application,
                    all.taper.applications.identity = quantity$all.taper.applications.identity,
                    taper.interpolate.links = quantity$taper.interpolate.links,
                    
                    n.components = quantity$n.components,
                    components = lapply(quantity$components, function(comp){
                        
                        kernel = list(
                            max.dim.names = comp$max.dim.names,
                            applies.to = comp$applies.to,
                            value.type = comp$value.type,
                            depends.on = comp$depends.on,
                            apply.function = comp$apply.function,
                            reversed.dimension.alias.mapping = comp$reversed.dimension.alias.mapping,
                            reversed.aliases = comp$reversed.aliases,
                            
                            evaluate = comp$get.evaluate.function(parent.environment = self,
                                                                  error.prefix = error.prefix)
                        )
                        
                        if (comp$value.type == 'function')
                            kernel$value.function.name = comp$value$value.function.name
                        
                        if (comp$value.type == 'numeric')
                            kernel$value.dim.names = dimnames(comp$value)
                        
                        kernel
                    })
                 )
                
            })
            
            private$i.element.backgrounds = lapply(specification$element.names, function(elem.name){
                elem = specification$get.quantity(elem.name)
                elem$get.element.background(specification.metadata = private$i.specification.metadata.for.null.sub.version,
                                                   error.prefix = paste0("Error creating specification kernel for version '", private$i.version, "' and location '", private$i.location, "': "))
            })
            names(private$i.element.backgrounds) = private$i.element.names = specification$element.names
            private$i.element.name.mappings = specification$element.name.mappings
            
            #-- Outcome Stuff --#
            private$i.outcome.kernels = lapply(specification$outcome.names, function(outcome.name){
                outcome = specification$get.outcome(outcome.name)
                
                list(
                    name = outcome$name,
                    
                    is.intrinsic = outcome$is.intrinsic,
                    is.dynamic = outcome$is.dynamic,
                    is.cumulative = outcome$is.cumulative,
                    
                    from.year = outcome$from.year,
                    to.year = outcome$to.year,
                    
                    dim.names = outcome$dim.names,
                    unrenamed.dim.names = outcome$unrenamed.dim.names,
                    
                    depends.on = outcome$depends.on,
                    
                    denominator.outcome = outcome$denominator.outcome,
                    value.is.numerator = outcome$value.is.numerator,
                    subset.dimension.values = outcome$subset.dimension.values,
                    rename.dimension.values = outcome$rename.dimension.values,
                    
                    dimension.aliases = outcome$dimension.aliases,
                    dimension.alias.suffix = outcome$dimension.alias.suffix,
                    
                    
                    
                    calculate.values = outcome$get.calculate.values.function(parent.environment = self,
                                                                             error.prefix = error.prefix)
                )
            })
            names(private$i.outcome.kernels) = specification$outcome.names
            
            private$i.outcome.names.for.null.sub.version = specification$get.outcome.names.for.sub.version(NULL)
            private$i.outcome.names.for.sub.version = lapply(specification$sub.versions, specification$get.outcome.names.for.sub.version)
            names(private$i.outcome.names.for.sub.version) = specification$sub.versions
            
            #-- Foregrounds --#
            private$i.foregrounds = specification$foregrounds
            
            #-- Parameter Mapping --#
            calibrated.parameters.distribution = get.parameters.distribution.for.version(private$i.version, type='calibrated')
            if (!is.null(calibrated.parameters.distribution))
            {
                private$i.calibrated.parameter.names = calibrated.parameters.distribution@var.names
                calibrated.parameters.apply.fn = get.parameters.apply.function.for.version(private$i.version, type='calibrated')
                private$i.calibrated.parameters.apply.function = bundle.function.and.dependees(calibrated.parameters.apply.fn,
                                                                                               parent.environment = self,
                                                                                         fn.name.for.error = "The calibrated parameters apply function",
                                                                                         error.prefix = error.prefix)
            }
                      
            sampled.parameters.distribution = get.parameters.distribution.for.version(private$i.version, type='sampled')
            if (!is.null(sampled.parameters.distribution))
            {
                private$i.sampled.parameter.names = sampled.parameters.distribution@var.names
                sampled.parameters.apply.fn = get.parameters.apply.function.for.version(private$i.version, type='sampled')
                private$i.sampled.parameters.apply.function = bundle.function.and.dependees(sampled.parameters.apply.fn,
                                                                                            parent.environment = self,
                                                                                      fn.name.for.error = "The sampled parameters apply function",
                                                                                      error.prefix = error.prefix)
            }
            
            #-- Core Components --#
            
            dynamic.outcome.names = names(private$i.outcome.kernels)[sapply(private$i.outcome.kernels, function(outcome){outcome$is.dynamic})]
            dynamic.outcomes = sapply(dynamic.outcome.names, specification$get.outcome)
            
            private$i.core.components = lapply(specification$core.components, function(comp){
                
                # Pull mechanism types
                comp$mechanism.types = comp$schema$mechanism.types

                # Figure out which dynamic outcomes apply
                outcome.applies.to.comp = sapply(dynamic.outcomes, comp$schema$dynamic.tracker.involves.component, comp=comp)
                comp$outcome.names.that.apply = dynamic.outcome.names[outcome.applies.to.comp]
                
                # Get rid of the schema so we don't inadvertently save its environment
                comp$schema = NULL
                
                comp
            })
            
            #-- Misc --#
            private$i.default.parameter.values = specification$default.parameter.values
            
            # Some more stuff here
            
            # for (elem.name in names(private))
            # {
            #     print(elem.name)
            #     x = private[[elem.name]]
            #     save(x, file=paste0("size_check/", elem.name, ".Rdata"))
            # }
            # env = environment(private$i.calibrated.parameters.apply.function)
            # save(env, file = "size_check/env.Rdata")
            # save(self, file = 'size_check/kernel.Rdata')
            # 
            # browser()
        },
        
        get.quantity = function(quantity.name)
        {
            
        },
        
        get.quantity.kernel = function(quantity.name)
        {
            private$i.quantity.kernels[[quantity.name]]
        },
        
        get.outcome.kernel = function(outcome.name)
        {
            private$i.outcome.kernels[[outcome.name]]  
        },
        
        get.specification.metadata = function(sub.version)
        {
            if (is.null(sub.version))
                private$i.specification.metadata.for.null.sub.version
            else
            {
                if (all(sub.version != names(private$i.specification.metadata.for.sub.version)))
                    stop(paste0("The specfication kernel for ",
                                private$i.version, " (created on ",
                                private$i.created.date,
                                ") does not include sub.version '", sub.version, "'"))
                
                private$i.specification.metadata.for.sub.version[[sub.version]]
            }
        },
        
        # Get dependency functions
        get.dependent.quantity.names = function(quantity.names)
        {
            unlist(private$i.dependent.quantity.names[quantity.names])
        },
        
        get.dependee.element.names = function(quantity.names)
        {
            unlist(private$i.dependee.element.names[quantity.names])
        },
        
        get.dependee.quantity.names = function(quantity.names)
        {
            unlist(private$i.dependee.quantity.names[quantity.names])
        },
        
        get.dependent.top.level.quantity.names = function(quantity.names)
        {
            unlist(private$i.dependent.top.level.quantity.names[quantity.names])
        },
        
        get.non.cumulative.dependent.outcome.names = function(quantity.names)
        {
            unlist(private$i.non.cumulative.dependent.outcome.names[quantity.names])
        },
        
        get.outcome.dependee.element.names = function(outcome.names)
        {
            unlist(private$i.outcome.dependee.element.names[outcome.names])
        },
        
        get.outcome.dependee.quantity.names = function(outcome.names)
        {
            unlist(private$i.outcome.dependee.quantity.names[outcome.names])
        },
        
        get.outcome.non.cumulative.dependendee.outcome.names = function(outcome.names)
        {
            unlist(private$i.outcome.non.cumulative.dependendee.outcome.names[outcome.names])
        },
        
        get.outcome.non.cumulative.direct.dependendee.outcome.names = function(outcome.names)
        {
            unlist(private$i.outcome.non.cumulative.direct.dependendee.outcome.names[outcome.names])
        },
        
        get.outcome.non.cumulative.dependent.outcome.names = function(outcome.names)
        {
            unlist(private$i.outcome.non.cumulative.dependent.outcome.names[outcome.names])
        },
        
        get.outcome.direct.dependee.outcome.names = function(outcome.names)
        {
            unlist(private$i.outcome.direct.dependee.outcome.names[outcome.names])
        },
        
        get.outcome.numerator.direct.dependee.outcome.names = function(outcome.names)
        {
            unlist(private$i.outcome.numerator.direct.dependee.outcome.names[outcome.names])
        },
        
        get.direct.dependent.outcome.numerator.names = function(quantity.names)
        {
            unlist(private$i.direct.dependent.outcome.numerator.names[quantity.names])
        },
        
        get.outcome.direct.dependee.quantity.names = function(outcome.names)
        {
            unlist(private$i.outcome.direct.dependee.quantity.names[outcome.names])
        },
        
        get.outcome.names.for.sub.version = function(sub.version)
        {
            if (is.null(sub.version))
                private$i.outcome.names.for.null.sub.version
            else
                private$i.outcome.names.for.sub.version[[sub.version]]
        }
    ),
    
    active = list(
        
        version = function(value)
        {
            if (missing(value))
                private$i.version
            else
                stop("Cannot modify a specification kernel's version - it is read-only")
        },
        
        location = function(value)
        {
            if (missing(value))
                private$i.location
            else
                stop("Cannot modify a specification kernel's location - it is read-only")
        },
        
        ordered.quantity.names = function(value)
        {
            if (missing(value))
                private$i.ordered.quantity.names
            else
                stop("Cannot modify a specification kernel's ordered.quantity.names - they are read-only")
        },
        
        ordered.quantity.names.except.initial.population = function(value)
        {
            if (missing(value))
                private$i.ordered.quantity.names.except.initial.population
            else
                stop("Cannot modify a specification kernel's ordered.quantity.names.except.initial.population - they are read-only")
        },
        
        quantity.kernels = function(value)
        {
            if (missing(value))
                private$i.quantity.kernels
            else
                stop("Cannot modify a specification kernel's quantity.kernels - they are read-only")
        },
        
        quantity.names = function(value)
        {
            if (missing(value))
                names(private$i.quantity.kernels)
            else
                stop("Cannot modify a specification kernel's element.names - they are read-only")
        },
        
        element.names = function(value)
        {
            if (missing(value))
                private$i.element.names
            else
                stop("Cannot modify a specification kernel's element.names - they are read-only")
        },
        
        element.backgrounds = function(value)
        {
            if (missing(value))
                private$i.element.backgrounds
            else
                stop("Cannot modify a specification kernel's element.backgrounds - they are read-only")
        },
        
        element.name.mappings = function(value)
        {
            if (missing(value))
                private$i.element.name.mappings
            else
                stop("Cannot modify a specification kernel's element.name.mappings - they are read-only")
        },
        
        outcome.kernels = function(value)
        {
            if (missing(value))
                private$i.outcome.kernels
            else
                stop("Cannot modify a specification kernel's outcomes - they are read-only")
        },
        
        outcome.names = function(value)
        {
            if (missing(value))
                names(private$i.outcome.kernels)
            else
                stop("Cannot modify a specification kernel's outcome.names - they are read-only")
        },
        
        default.parameter.values = function(value)
        {
            if (missing(value))
                names(private$i.default.parameter.values)
            else
                stop("Cannot modify a specification kernel's default.parameter.values - they are read-only")
        },
        
        foregrounds = function(value)
        {
            if (missing(value))
                names(private$i.foregrounds)
            else
                stop("Cannot modify a specification kernel's foregrounds - they are read-only")
        },
        
        calibrated.parameter.names = function(value)
        {
            if (missing(value))
                private$i.calibrated.parameter.names
            else
                stop("Cannot modify a specification kernel's calibrated.parameter.names - they are read-only")
        },
        
        sampled.parameter.names = function(value)
        {
            if (missing(value))
                private$i.sampled.parameter.names
            else
                stop("Cannot modify a specification kernel's sampled.parameter.names - they are read-only")
        },
        
        calibrated.parameters.apply.function = function(value)
        {
            if (missing(value))
                private$i.calibrated.parameters.apply.function
            else
                stop("Cannot modify a specification kernel's calibrated.parameters.apply.function - they are read-only")
        },
        
        sampled.parameters.apply.function = function(value)
        {
            if (missing(value))
                private$i.sampled.parameters.apply.function
            else
                stop("Cannot modify a specification kernel's sampled.parameters.apply.function - they are read-only")
        }
    ),
    
    private = list(
        
        #-- General Stuff --#
        i.version = NULL,
        i.location = NULL,
        i.created.date = NULL,
        i.created.time = NULL,
        
        i.specification.metadata.for.null.sub.version = NULL,
        i.specification.metadata.for.sub.version = NULL,
        
        #-- Quantity Stuff --#
        i.ordered.quantity.names = NULL,
        i.ordered.quantity.names.except.initial.population = NULL,
        
        i.quantity.kernels = NULL,
        i.element.names = NULL,
        i.element.backgrounds = NULL,
        i.element.name.mappings = NULL,
        
        #-- Foregrounds --#
        i.foregrounds = NULL,
        
        #-- Outcome Stuff --#
        i.outcome.kernels = NULL,
        
        i.outcome.names.for.sub.version = NULL,
        i.outcome.names.for.null.sub.version = NULL,
        
        #-- Core Components --#
        i.core.components = NULL,
        
        #-- Parameter Mappings --#
        i.calibrated.parameter.names = NULL,
        i.sampled.parameter.names = NULL,
        
        i.calibrated.parameters.apply.function = NULL,
        i.sampled.parameters.apply.function = NULL,
        
        #-- Misc --#
        i.default.parameter.values = NULL,
        
        #-- Dependency data structures --#
        i.dependent.quantity.names = NULL,
        i.dependee.element.names = NULL,
        i.dependee.quantity.names = NULL,
        i.dependent.top.level.quantity.names = NULL,
        i.non.cumulative.dependent.outcome.names = NULL,
        i.outcome.dependee.element.names = NULL,
        i.outcome.dependee.quantity.names = NULL,
        i.outcome.non.cumulative.dependendee.outcome.names = NULL,
        i.outcome.non.cumulative.direct.dependendee.outcome.names = NULL,
        i.outcome.non.cumulative.dependent.outcome.names = NULL,
        i.outcome.direct.dependee.outcome.names = NULL,
        i.outcome.numerator.direct.dependee.outcome.names = NULL,
        i.direct.dependent.outcome.numerator.names = NULL,
        i.outcome.direct.dependee.quantity.names = NULL
    )
)