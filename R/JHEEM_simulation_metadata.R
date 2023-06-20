

#'@name Get a Simulation Metadata Object
#'
#'@param version The version of the model specification (must have been previously registered) for which to get metadata
#'@param location A single character value representing the location for the metadata
#'@param error.prefix A string to prepend to any errors generated in getting the metadata object
#'
#'@details A simulation.metadata object contains metadata, such as the dim.names to which its contents will conform
#'
#'@export
get.simulation.metadata <- function(version, location,
                                       error.prefix = paste0("Error deriving the simulation-metadata for '", version, "' and location '", location, "': "))
{
    SIMULATION.METADATA$new(version=version,
                               location=location,
                               error.prefix=error.prefix)
}


SIMULATION.METADATA = R6::R6Class(
    'simulation.metadata',
    inherit = JHEEM.ENTITY,
    portable = F,
    
    public = list(
        
        initialize = function(version,
                              location,
                              error.prefix)
        {
            #-- Call the superclass constructor --#
            super$initialize(version = version,
                             location = location,
                             intervention.code = NA,
                             calibration.index = NA,
                             type = "Simulation Metadata",
                             error.prefix = error.prefix)
        },
        
        
        # Returns the dimnames that the results of a call to simulation$get will have
        # It's arguments mirror simulation$get
        get.dim.names = function(outcomes,
                                 years,
                                 keep.dimensions,
                                 dimension.values,
                                 error.prefix = "Error getting dimnames of simulation results: ")
        {
            
        }
        
    ),
    
    active = list(
        
        outcomes = function(value)
        {
            
        },
        
        outcome.dim.names = function(value)
        {
            
        }
    ),
    
    private = list(
        
        get.current.code.iteration = function()
        {
            
        }
    )
    
)