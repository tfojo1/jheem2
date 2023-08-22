

JHEEM.MODEL.FOREGROUND = R6::R6Class(
    'model.foreground',
    inherit = JHEEM.ENTITY,
    portable = F,
    
    public = list(
        
        initialize = function(version,
                              location,
                              target.population,
                              intervention.effects,
                              engine = NULL,
                              error.prefix)
        {
            super$initialize(version = version,
                             location = location,
                             type = 'foreground',
                             error.prefix = error.prefix)
        },
        
        resolve = function(bindings)
        {
            
        }
    ),
    
    active = list(
        
        n.effects = function(value)
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