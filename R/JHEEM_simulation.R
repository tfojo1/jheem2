

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
    inherit = JHEEM.ENTITY,
    
    public = list(
        initialize = function()
        {
            
        },
        
        get.quantity <- function()
        {
            
        },
        
        get.quantities <- function()
        {
            rv = sapply(outcomes, function(outcome){
                sim$get.quantity()
            })
            
            rv
        }
        
    ),
    
    active = list(
        
        years = function(value)
        {
            
        }
    ),
    
    private = list(
        
    )
)