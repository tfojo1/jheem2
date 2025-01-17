
##-- ABOUT THIS CLASS --##
##
## A general, abstract class for all JHEEM-related objects to which 
##  BOTH a version AND a location apply
##  
## To define a subclass, you must overwrite the private$get.current.code.iteration method
##  This enables entities to know if they are out of date by comparing their stored
##  code iteration against the current code iteration
##  

JHEEM.ENTITY = R6::R6Class(
    'jheem.entity',
    portable = F,
    
    public = list(
        
        initialize = function(version,
                              sub.version,
                              location,
                              type,
                              error.prefix)
        {
            # Validate version
            if (!is.character(version) || length(version)!=1 || is.na(version))
                stop(paste0(error.prefix, "'version' must be a single, non-NA character value"))
        
#            if (!is.specification.registered.for.version(version))
#                stop(paste0(error.prefix, "No specification has been registered for version '", version, "'"))
        
            
            # Validate location
            if (!is.character(location) || length(location)!=1 || is.na(location) || nchar(location)==0)
                stop(paste0(error.prefix, "'location' must be a single, non-NA, non-empty character value"))
            
#            if (!locations::is.location.valid(location))
#                stop(paste0("'", location, "' is not recognized as a registered as a location"))
            
            
            # Validate sub-version
            if (!is.null(sub.version))
            {
                if (!is.character(sub.version) || length(sub.version)!=1 || is.na(sub.version) || nchar(sub.version)==0)
                    stop(paste0(error.prefix, "'sub.version' must be a single, non-NA, non-empty character value"))
                
#                if (!any(sub.version==jheem.kernel$sub.versions))
#                    stop(paste0(error.prefix, "'", sub.version, "' is not a registered sub-version for the '", version, "' specification"))
            }
            
            
            # Validate the type
            if (!is.character(type) || length(type)!=1 || is.na(type) || nchar(type)==0)
                stop(paste0(error.prefix, "'type' must be a single, non-NA, non-empty character value"))
            
            
            # Pull the code iteration
            code.iteration = private$get.current.code.iteration()
            if (!is.character(code.iteration) || length(code.iteration)!=1 || is.na(code.iteration) || nchar(code.iteration)==0)
                stop(paste0(error.prefix, "The 'code.iteration' returned by the subclass method of private$get.current.code.iteration (in ",
                            paste0(setdiff(class(self), c("R6",'jheem.entity')), collapse=','),
                            ") must be a single, non-NA, non-empty character value"))
            

            
            #-- Store Variables --#
            private$i.version = version
            private$i.sub.version = sub.version
            private$i.location = locations::sanitize(location)
            private$i.code.iteration = private$get.current.code.iteration()
            private$i.type = type
        },
        
        is.out.of.date = function()
        {
            private$i.code.iteration != private$get.current.code.iteration()
        }
    ),
    
    active = list(
        
        version = function(value)
        {
            if (missing(value))
                private$i.version
            else
                stop(paste0("Cannot set 'version' for a ", private$i.type, " - it is read-only"))
        },
        
        sub.version = function(value)
        {
            if (missing(value))
                private$i.sub.version
            else
                stop(paste0("Cannot set 'sub.version' for a ", private$i.type, " - it is read-only"))
        },
        
        code.iteration = function(value)
        {
            if (missing(value))
                private$i.code.iteration
            else
                stop(paste0("Cannot set 'code.iteration' for a ", private$i.type, " - it is read-only"))
        },
        
        location = function(value)
        {
            if (missing(value))
                private$i.location
            else
                stop(paste0("Cannot set 'location' for a ", private$i.type, " - it is read-only"))
        },
        
        type = function(value)
        {
            if (missing(value))
                private$i.type
            else
                stop(paste0("Cannot set 'type' for a ", private$i.type, " - it is read-only"))
        }
    ),
    
    private = list(
        
        i.version = NULL,
        i.sub.version = NULL,
        i.code.iteration = NULL,
        i.location = NULL,
        i.type = NULL,
        
        get.current.code.iteration = function()
        {
            stop(paste0("get.current.code.iteration must be implented at the subclass level (",
                        paste0(setdiff(class(self), c("R6",'jheem.entity')), collapse=','),
                        ") of this jheem.entity"))
        }
        
    )
)
