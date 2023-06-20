

##----------------------##
##----------------------##
##-- PUBLIC INTERFACE --##
##----------------------##
##----------------------##


get.sim.quantity <- function(sim, outcome)
{
    
}


##-----------------------##
##-----------------------##
##-- CLASS DEFINITIONS --##
##-----------------------##
##-----------------------##

JHEEM.SIMULATION = R6::R6Class(
    'jheem.simulation',
    inherit = JHEEM.SIMULATED.ENTITY,
    
    public = list(
        initialize = function()
        {
            
        },
        
        get = function(outcomes,
                       years,
                       keep.dimensions,
                       dimension.values,
                       error.prefix = "Error getting simulation results: ")
        {
            
        }
        
    ),
    
    active = list(
        
        years = function(value)
        {
            
        }
    ),
    
    private = list(
        
        get.current.code.iteration = function()
        {
            
        }
        
    )
)