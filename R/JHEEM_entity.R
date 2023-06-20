
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
                              location,
                              type,
                              error.prefix)
        {
            # Validate version
            if (!is.character(version) || length(version)!=1 || is.na(version))
                stop(paste0(error.prefix, "'version' must be a single, non-NA character value"))
            
            if (!is.specification.registered.for.version(version))
                stop(paste0(error.prefix, "No specification has been registered for version '", version, "'"))
            
            # Validate location
            if (!is.character(location) || length(location)!=1 || is.na(location) || nchar(location)==0)
                stop(paste0(error.prefix, "'location' must be a single, non-NA, non-empty character value"))
            
            if (!is.location.valid(location))
                stop(paste0("'", location, "' is not recognized as a registered as a location"))
            
            # Validate the type
            if (!is.character(type) || length(type)!=1 || is.na(type) || nchar(type)==0)
                stop(paste0(error.prefix, "'type' must be a single, non-NA, non-empty character value"))
            
            # Pull the code iteration
            code.iteration = private$get.current.code.iteration
            if (!is.character(code.iteration) || length(code.iteration)!=1 || is.na(code.iteration) || nchar(code.iteration)==0)
                stop(paste0(error.prefix, "The 'code.iteration' returrned by the subclass method of private$get.current.code.iteration (in ",
                            paste0(setdiff(class(self), c("R6",'jheem.entity')), collapse=','),
                            ") must be a single, non-NA, non-empty character value"))
            

            
            #-- Store Variables --#
            private$i.version = version
            private$i.location = location
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
        },
        
        # Lazily evaluates the specification metadata - to avoid infinite loop if this IS a specification.metadata object
        specification.metadata = function(value)
        {
            if (missing(value))
            {
                if (is.null(private$i.specification.metadata) || private$i.specification.info$is.out.of.date())
                {
                    private$i.specification.metadata = get.specification.metadata(version = private$i.version,
                                                                                  location = private$i.location,
                                                                                  error.prefix = paste0("Error deriving the specification-metadata for '", version, "' and location '", location, "': "))
                }
                private$i.specification.metadata
            }
            else
                stop(paste0("Cannot set 'specification.metadata' for a ", private$i.type, " - it is read-only"))
        },
        
        # Lazily evaluates the simulation metadata - to avoid infinite loop if this IS a simulation.metadata object
        simulation.metadata = function(value)
        {
            if (missing(value))
            {
                if (is.null(private$i.simulation.metadata) || private$i.simulation.info$is.out.of.date())
                {
                    private$i.simulation.metadata = get.simulation.metadata(version = private$i.version,
                                                                                  location = private$i.location,
                                                                                  error.prefix = paste0("Error deriving the simulation-metadata for '", version, "' and location '", location, "': "))
                }
                private$i.simulation.metadata
            }
            else
                stop(paste0("Cannot set 'simulation.metadata' for a ", private$i.type, " - it is read-only"))
        }
        
    ),
    
    private = list(
        
        i.version = NULL,
        i.code.iteration = NULL,
        i.location = NULL,
        i.type = NULL,
        
        i.specification.metadata = NULL,
        i.simulation.metadata = NULL,
        
        get.current.code.iteration = function()
        {
            stop(paste0("get.current.code.iteration must be implented at the subclass level (",
                        paste0(setdiff(class(self), c("R6",'jheem.entity')), collapse=','),
                        ") of this jheem.entity"))
        }
        
    )
)

JHEEM.SIMULATED.ENTITY = R6::R6Class(
    'jheem.simulated.entity',
    inherit = JHEEM.ENTITY,
    portable = F,
    
    public = list(
        
        initialize = function()
        {
            
            
            if (length(calibration.index)==1 && is.na(calibration.index))
                calibration.index = as.character(NA)
            else
            {
                if (!is.character(calibration.index) || length(calibration.index)!=1 || nchar(calibration.index)==0)
                    stop(paste0(error.prefix, "'calibration.index' must either be NA or a single, non-empty character value"))
                
                if (string.contains.invalid.characters(calibration.index, valid.characters=NUMBERS.LETTERS.DASH.PERIOD))
                {
                    invalid = setdiff(strsplit(calibration.index, '')[[1]], strsplit(NUMBERS.LETTERS.DASH.PERIOD, '')[[1]])
                    stop(paste0(error.prefix,
                                "Invalid ",
                                ifelse(length(invalid)==1, "character", "characters"),
                                "(", paste0(invalid, collapse=', '), 
                                ") in calibration.index '", calibration.index, 
                                "' - can only contain numbers, letters, periods, and dashes"))
                    
                }
            }
            
            if (length(intervention.code)==1 && is.na(intervention.code))
                intervention.code = as.character(NA)
            else
            {
                if (!is.na(calibration.index))
                    stop(paste0(error.prefix, "'intervention.code' can only be specified if 'calibration.index' is not NA"))
                
                if (!is.character(intervention.code) || length(intervention.code)!=1 || nchar(intervention.code)==0)
                    stop(paste0(error.prefix, "'intervention.code' must either be NA or a single, non-empty character value"))
                
                if (string.contains.invalid.characters(intervention.code, valid.characters=NUMBERS.LETTERS.DASH.PERIOD))
                {
                    invalid = setdiff(strsplit(intervention.code, '')[[1]], strsplit(NUMBERS.LETTERS.DASH.PERIOD, '')[[1]])
                    stop(paste0(error.prefix,
                                "Invalid ",
                                ifelse(length(invalid)==1, "character", "characters"),
                                "(", paste0(invalid, collapse=', '), 
                                ") in intervention.code '", intervention.code, 
                                "' - can only contain numbers, letters, periods, and dashes"))
                    
                }
            }
            
            
        },
        
        get.filename = function(extension='Rdata')
        {
            if (length(extension)==1 && is.na(extension))
                extension = ''
            else
            {
                if (!is.character(extension) || length(extension)!=1)
                    stop("'extension' must either be NA or a single, non-empty character value")
                
                if (substr(extension, 1,1) == '.')
                    extension = substr(extension, 2, nchar(extension))
                
                if (nchar(extension)==0)
                    stop("'extension' cannot be an empty string")
                
                if (string.contains.invalid.characters(extension, LETTERS.NUMBERS))
                    stop(paste0("'extension' can only contain letters and numbers ('", extension, "' contains extraneous characters)"))
                
                extension = paste0(".", extension)
            }
            
            elements = c(private$i.version,
                         private$i.location,
                         private$i.calibration.index,
                         private$i.intervention.code)
            elements = elements[!is.na(elements)]
            
            paste0(paste0(elements, collapse='_'), extension)
        }
    ),
    
    private = list(
        
    )
)