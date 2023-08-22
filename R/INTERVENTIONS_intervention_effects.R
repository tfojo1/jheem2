
#'@name Create an Effect for an Intervention to Have on a Model Quantity
#'
#'@param target.quantity.name The name of the quantity, defined in a jheem.model.specification, which an intervention affects
#'@param start.time The time at which the intervention begins to take effect (ie, depart from what the value of the quantity would otherwise have been)
#'@param values The values to apply to the quantity at times. Must be either (1) a numeric vector of the same length as times, (2) a character vector of the same length as times, (3) a list containing scalar numeric values, single character values, expressions, or calls, with the same length as times, or (d) a single expression or call value, only if length(times)==1
#'@param times The times as which values apply to the quantity. 
#'@param end.time The time at which the intervention stops taking effect (ie, returns to what the value of the quantity otherwise would have been)
#'@param apply.values.as A character vector indicating how values should be applied to the quantity. Choices are (1) 'absolute' - the quantity takes the given value, (2) 'multiplier' - the value is multiplied by the value the quantity would otherwise have taken, or (3) 'addend' - the value is added to the value the quantity would otherwise have taken. Must either be a single character value (in which case, it is applied to all the times) or have the same length as times and values
#'@param scale The scale at which values are applied to the quantity
#'@param allow.values.less.than.otherwise,allow.values.greater.than.otherwise Logical indicators of whether the intervention may cause the value of the quantity to be less/greater than it otherwise would have been
#'
#'@export
create.intervention.effect <- function(target.quantity.name,
                                       start.time,
                                       values,
                                       times,
                                       end.time=Inf,
                                       apply.values.as = c('absolute','multiplier','addend')[1],
                                       scale,
                                       allow.values.less.than.otherwise,
                                       allow.values.greater.than.otherwise)
{
    
}

##-----------------------##
##-----------------------##
##-- CLASS DEFINITIONS --##
##-----------------------##
##-----------------------##

JHEEM.INTERVENTION.EFFECT = R6::R6Class(
    'jheem.intervention.effect',
    
    public = list(
        
        initialize = function(target.quantity.name,
                              start.time,
                              values,
                              times,
                              end.time=Inf,
                              scale,
                              allow.values.less.than.otherwise,
                              allow.values.greater.than.otherwise)
        {
            # Validate target quantity name
            error.prefix = "Cannot create intervention effect: "
            if (!is.character(target.quantity.name) || length(target.quantity.name)!=1 ||
                is.na(target.quantity.name) || nchar(target.quantity.name)==0)
                stop(paste0(error.prefix, "'target.quantity.name' must be a single, non-NA, non-empty character value"))
            
            error.prefix = paste0("Cannot create intervention effect for quantity '", target.quantity.name, "': ")
            
            # Validate times (start.time, end.time, times)
            
            # Validate values
            
            # Validate scale
            
            # Validate allow.values.less.than.otherwise, allow.values.greater.than.otherwise
            if (!is.logical(allow.values.less.than.otherwise) || length(allow.values.less.than.otherwise)!=1 ||
                is.na(allow.values.less.than.otherwise))
                stop(paste0(error.prefix, "'allow.values.less.than.otherwise', must be a single, non-NA logical value"))
            
            if (!is.logical(allow.values.greater.than.otherwise) || length(allow.values.greater.than.otherwise)!=1 ||
                is.na(allow.values.greater.than.otherwise))
                stop(paste0(error.prefix, "'allow.values.greater.than.otherwise', must be a single, non-NA logical value"))
            
            if (allow.values.less.than.otherwise && allow.values.greater.than.otherwise)
                stop(paste0(error.prefix, "'allow.values.less.than.otherwise' and 'allow.values.greater.than.otherwise' cannot BOTH be TRUE"))
            
            # Store values
        }
        
    ),
    
    active = list(
        
    ),
    
    private = list(
        
    )
)