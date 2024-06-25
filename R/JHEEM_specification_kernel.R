
#-- A SPECIFICATION.KERNEL is all the data from a specification object necessary to run one engine in a location
#--  as well as to run interventions on a simulation
#-- These get saved independently of the specification code, and can live on in simulation objects

SPECIFICATION.KERNEL = R6::R6Class(
    'jheem.specification.kernel',
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
            
            # Quantity Stuff
            private$i.ordered.quantity.names = specification$ordered.quantity.names
            private$i.ordered.quantity.names.except.initial.population = specification$ordered.quantity.names.except.initial.population
            
            
            private$i.quantity.kernels = lapply(specification$quantities, function(quantity){
                
                list(original.name = quantity$get.original.name(private$i.version),
                     max.dim.names = quantity$max.dim.names,
                     required.dim.names = quantity$required.dim.names,
                     components = quantity$components
                     )
                
            })
            
            private$i.element.backgrounds = lapply(specification$element.names, function(elem.name){
                elem = specification$get.quantity(elem.name)
                elem$get.element.background(specification.metadata = private$i.specification.metadata.for.null.sub.version,
                                                   error.prefix = paste0("Error creating specification kernel for version '", private$i.version, "' and location '", private$i.location, "': "))
            })
            names(private$i.element.backgrounds) = private$i.element.names = specification$element.names
            private$i.element.name.mappings = specification$element.name.mappings
            
            # Outcome Stuff
            private$i.outcome.kernels = lapply(specification$outcome.names, function(outcome.name){
                
                outcome = specification$get.outcome(outcome.name)
                
                list(
                    name = outcome$name,
                    dim.names = outcome$dim.names,
                    unrenamed.dim.names = outcome$unrenamed.dim.names
                )
            })
            names(private$i.outcome.kernels) = specification$outcome.names
            
            #-- Foregrounds --#
            private$i.foregrounds = specification$foregrounds
            
            #-- Misc --#
            private$i.default.parameter.values = specification$default.parameter.values
            
            # Some more stuff here
        },
        
        get.quantity = function(quantity.name)
        {
            
        },
        
        get.quantity.kernel = function(quantity.name)
        {
            private$i.quantity.kernels[[quantity.name]]
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
                stop("Cannot modify a specification kernel's outcome.kernels - they are read-only")
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