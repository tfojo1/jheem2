

#'@title Get the Specification-Info Object for an Instance of a JHEEM Entity
#'
#'@param jheem.entity An object of class jheem.entity (such as a jheem.engine, jheem.simset, or jheem.sim)
#'
#'@return An object of class 'jheem.specification.info'
#'
#'@export
get.specification.info <- function(jheem.entity)
{
    if (!is(jheem.entity, "R6") || !is(jheem.entity, "jheem.entity"))
        stop("jheem.entity must be an R6 object of class 'jheem.entity'")
    
    jheem.entity$get.specification.info()
}


JHEEM.ENTITY = R6::R6Class(
    'jheem.entity',
    portable = F,
    
    public = list(
        
        initialize = function(version,
                              location,
                              code.iteration,
                              intervention.code,
                              calibration.index,
                              type,
                              error.prefix)
        {
            if (!is.character(version) || length(version)!=1 || is.na(version))
                stop(paste0(error.prefix, "'version' must be a single, non-NA character value"))
            
            if (!is.specification.registered.for.version(version))
                stop(paste0(error.prefix, "No specification has been registered for version '", version, "'"))
            
            if (!is.character(location) || length(location)!=1 || is.na(location) || nchar(location)==0)
                stop(paste0(error.prefix, "'location' must be a single, non-NA, non-empty character value"))

            # We should probably check that this is a registered location in the location manager
                        
            if (!is.character(code.iteration) || length(code.iteration)!=1 || is.na(code.iteration) || nchar(code.iteration)==0)
                stop(paste0(error.prefix, "'code.iteration' must be a single, non-NA, non-empty character value"))
            
            if (!is.character(type) || length(type)!=1 || is.na(type) || nchar(type)==0)
                stop(paste0(error.prefix, "'type' must be a single, non-NA, non-empty character value"))

                        
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
            
            #-- Store Variables --#
            private$i.version = version
            private$i.location = location
            private$i.code.iteration = code.iteration
            private$i.intervention.code = intervention.code
            private$i.type = type
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
        },
        
        # Lazily evaluates the specification info
        get.specification.info = function(value)
        {
            if (missing(value))
            {
                if (is.null(private$i.specification.info) ||
                    get.specification.for.version(private$i.version)$iteration != private$i.specification.info$specification.iteration)
                {
                    private$i.specification.info = SPECIFICATION.INFO$new(version = version,
                                                                          location = private$i.location,
                                                                          error.prefix = paste0("Error deriving the specification-info for '", version, "' and location '", location, "': "))
                }
                private$i.specification.info
            }
            else
                stop(paste0("Cannot set 'specification.info' for a ", private$i.type, " - it is read-only"))
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
        
        infected.compartments = function(value)
        {
            
        },
        
        uninfected.compartments = function(value)
        {
            
        }
        
    ),
    
    private = list(
        
        i.version = NULL,
        i.code.iteration = NULL,
        i.intervention.code = NULL,
        i.location = NULL,
        i.type = NULL,
        
        i.specification.info = NULL
        
    )
)