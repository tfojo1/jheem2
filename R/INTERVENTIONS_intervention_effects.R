
#'@name Create an Effect for an Intervention to Have on a Model Quantity
#'
#'@param quantity.name The name of the quantity, defined in a jheem.model.specification, which an intervention affects
#'@param start.time The time at which the intervention begins to take effect (ie, depart from what the value of the quantity would otherwise have been)
#'@param effect.values The values to apply to the quantity at times. Must be either (1) a numeric vector, (2) a character vector, (3) an expression or call, or (4) a list containing only numeric vectors, character vectors, expressions, or calls
#'@param times The times as which values apply to the quantity. 
#'@param end.time The time at which the intervention stops taking effect (ie, returns to what the value of the quantity otherwise would have been)
#'@param apply.effects.as A single character value indicating how values should be applied to the quantity. Choices are (1) 'value' - the quantity takes the given value, (2) 'multiplier' - the value is multiplied by the value the quantity would otherwise have taken, or (3) 'addend' - the value is added to the value the quantity would otherwise have taken. 
#'@param scale The scale at which values are applied to the quantity
#'@param allow.values.less.than.otherwise,allow.values.greater.than.otherwise Logical indicators of whether the intervention may cause the value of the quantity to be less/greater than it otherwise would have been
#'@param bindings A named list. The names must correspond to variables on which the intervention effect applies, if it is a variable. The elements of the list must be either (1) numeric values or (2) functions must take only "location" and "specification.metadata" as arguments and return numeric vectors with no NA values
#'@param ... Additional arguments to be passed to any bindings which are functions
#'
#'@export
create.intervention.effect <- function(quantity.name,
                                       start.time,
                                       effect.values,
                                       times,
                                       end.time=Inf,
                                       apply.effects.as,
                                       scale,
                                       allow.values.less.than.otherwise,
                                       allow.values.greater.than.otherwise,
                                       bindings=NULL,
                                       ...)
{
    # Validate target quantity name
    error.prefix = "Cannot create intervention effect: "
    if (!is.character(quantity.name) || length(quantity.name)!=1 ||
        is.na(quantity.name) || nchar(quantity.name)==0)
        stop(paste0(error.prefix, "'quantity.name' must be a single, non-NA, non-empty character value"))
    
    error.prefix = paste0("Cannot create intervention effect for quantity '", quantity.name, "': ")
    
    # Process times (start.time, end.time, times)
    start.time = create.effect.evaluatable.value(value = start.time, 
                                                 value.name = 'start.time', 
                                                 allowed.expression.functions = ALLOWED.EFFECT.START.END.TIME.EXPRESSION.FUNCTIONS, 
                                                 return.as.list = F,
                                                 error.prefix = error.prefix)
    
    end.time = create.effect.evaluatable.value(value = end.time, 
                                               value.name = 'end.time', 
                                               allowed.expression.functions = ALLOWED.EFFECT.START.END.TIME.EXPRESSION.FUNCTIONS, 
                                               return.as.list = F,
                                               error.prefix = error.prefix)
    
    times = create.effect.evaluatable.value(value = times, 
                                            value.name = 'times', 
                                            allowed.expression.functions = ALLOWED.EFFECT.TIMES.EXPRESSION.FUNCTIONS, 
                                            return.as.list = T,
                                            error.prefix = error.prefix)
    
    # Process values
    effect.values = create.effect.evaluatable.value(value = effect.values, 
                                                    value.name = 'effect.values', 
                                                    allowed.expression.functions = ALLOWED.EFFECT.VALUES.EXPRESSION.FUNCTIONS, 
                                                    return.as.list = T,
                                                    error.prefix = error.prefix)
    
    # Validate scale
    check.model.scale(scale = scale,
                      error.prefix = error.prefix)
    
    
    # Validate allow.values.less.than.otherwise, allow.values.greater.than.otherwise
    if (!is.logical(allow.values.less.than.otherwise) || length(allow.values.less.than.otherwise)!=1 ||
        is.na(allow.values.less.than.otherwise))
        stop(paste0(error.prefix, "'allow.values.less.than.otherwise', must be a single, non-NA logical value"))
    
    if (!is.logical(allow.values.greater.than.otherwise) || length(allow.values.greater.than.otherwise)!=1 ||
        is.na(allow.values.greater.than.otherwise))
        stop(paste0(error.prefix, "'allow.values.greater.than.otherwise', must be a single, non-NA logical value"))
    
    if (!allow.values.less.than.otherwise && !allow.values.greater.than.otherwise)
        stop(paste0(error.prefix, "'allow.values.less.than.otherwise' and 'allow.values.greater.than.otherwise' cannot BOTH be FALSE"))
    
    # Validate apply.effects.as
    if (!is.character(apply.effects.as) || length(apply.effects.as)!=1 || is.na(apply.effects.as))
        stop(paste0(error.prefix, "'apply.effects.as' must be a single, non-NA character value"))
    valid.apply.effects.as = c('value','multiplier','addend')
    if (all(apply.effects.as != valid.apply.effects.as))
        stop(paste0(error.prefix,
                    "'", apply.effects.as, 
                    "' is not a valid value for 'apply.effects.as'. Must be either ",
                    collapse.with.or("'", valid.apply.effects.as, "'")))
    
    
    # Validate bindings
    if (!is.null(bindings))
    {
        if (!is.list(bindings))
            stop(paste0(error.prefix, "'bindings' must be a named list"))
        
        if (length(bindings)>0)
        {
            if (is.null(names(bindings)))
                stop(paste0(error.prefix, "'bindings' must be a NAMED list"))
            
            binding.is.function = sapply(bindings, is.function)
            if (any(!sapply(bindings, is.numeric) & !binding.is.function))
                stop(paste0(error.prefix, "The elements of 'bindings' must be either numeric vectors or functions"))
            
            bindings[binding.is.function] = lapply(names(bindings)[binding.is.function], function(name){
                fn = bindings[[name]]
                
                FUNCTION.WRAPPER$new(fn=fn,
                                     ...,
                                     fn.name = paste0("the binding function for '", name, "'"),
                                     require.all.arguments.up.front = T,
                                     throw.error.if.missing.arguments.at.execute = T,
                                     arguments.to.be.supplied.later = c('specification.metadata','location'),
                                     error.prefix=error.prefix)
            })
            
            if (any(sapply(bindings[!binding.is.function], function(val){
                any(is.na(val))
            })))
                stop(paste0(error.prefix, "The elements of 'bindings' cannot contain NA values"))
        }
    }
    
    # Call the constructor
    rv = JHEEM.INTERVENTION.EFFECT$new(quantity.name = quantity.name,
                                       start.time = start.time,
                                       effect.values = effect.values,
                                       times = times,
                                       end.time = end.time,
                                       apply.effects.as = apply.effects.as,
                                       scale = scale,
                                       allow.values.less.than.otherwise = allow.values.less.than.otherwise,
                                       allow.values.greater.than.otherwise = allow.values.greater.than.otherwise,
                                       bindings = bindings,
                                       error.prefix = error.prefix)
    
    # Resolve if appropriate
    if (!rv$is.resolved && length(rv$depends.on)==0)
        rv$resolve(bindings = list(), error.prefix = error.prefix)
    else
        rv
}

##---------------------------##
##-- CONSTANTS and HELPERS --##
##---------------------------##

ALLOWED.EFFECT.START.END.TIME.EXPRESSION.FUNCTIONS = c("+","-","*","/","(","log","exp","sqrt")
ALLOWED.EFFECT.TIMES.EXPRESSION.FUNCTIONS = c("+","-","*","/","(","log","exp","sqrt", "c", ":")
ALLOWED.EFFECT.VALUES.EXPRESSION.FUNCTIONS = ALLOWED.EFFECT.TIMES.EXPRESSION.FUNCTIONS

# A helper
# If numeric, just returns the numeric value
create.effect.evaluatable.value <- function(value, 
                                            allowed.expression.functions, 
                                            value.name, 
                                            return.as.list,
                                            error.prefix)
{
    if (is.numeric(value))
        value
    else
    {
        was.list = is.list(value)
        if (!is.list(value))
            value = list(value)
        
        rv = lapply(1:length(value), function(i){
            
            one.val = value[[i]]
            if (is.numeric(one.val))
                one.val
            else
            {
                one.rv = EVALUATABLE.VALUE$new(na.replacement = as.numeric(NA),
                                               allow.numeric.value = T,
                                               allow.character.value = T,
                                               allow.expression.value = T,
                                               allow.function.value = F,
                                               allowed.expression.functions = allowed.expression.functions,
                                               function.arguments.to.be.supplied.later = character(), 
                                               error.prefix = error.prefix)
                
                one.rv$set.value(value = one.val, 
                             value.name = ifelse(was.list, paste0("the ", get.ordinal(i), " element of ", value.name), value.name),
                             error.prefix = error.prefix)
                
                one.rv
            }
        })
        
        if (return.as.list)
            rv
        else
            rv[[1]]
    }
}

##-----------------------##
##-----------------------##
##-- CLASS DEFINITIONS --##
##-----------------------##
##-----------------------##

JHEEM.INTERVENTION.EFFECT = R6::R6Class(
    'jheem.intervention.effect',
    
    public = list(
        
        initialize = function(quantity.name,
                              start.time,
                              effect.values,
                              times,
                              end.time,
                              apply.effects.as,
                              scale,
                              allow.values.less.than.otherwise,
                              allow.values.greater.than.otherwise,
                              bindings,
                              all.times = NULL, #here to save time if we have already calculated
                              location = NULL,
                              version = NULL,
                              error.prefix)
        {
            # This constructor is partly 'dumb' - some error checking is done by other functions that call it
            
            if (is.numeric(times))
                n = length(times)
            else if (is.numeric(effect.values))
                n = length(effect.values)
            else
                n = NA
            
            # Check for NULL
            if (is.null(times))
                stop(paste0(error.prefix, "'times' cannot be NULL"))
            
            # Check that times, effect.values, and apply.effects.as are all the same length
            if (is.numeric(times) || is.numeric(effect.values))
            {
                if (is.numeric(times) && is.numeric(effect.values))
                {
                    if (length(times) != length(effect.values))
                        stop(paste0(error.prefix, "'times' (length ", length(times), 
                                    ") and 'effect.values' (length ", length(effect.values),
                                    ") must be the same length"))
                }
            }
            
            # Check start.time
            if (is.numeric(start.time))
            {
                if (length(start.time)!=1)
                    stop(paste0(error.prefix, "'start.time' must be a SINGLE numeric value"))
                
                if (is.na(start.time))
                    stop(paste0(error.prefix, "'start.time' cannot be NA"))
            }
            
            # Check end.time
            if (is.numeric(end.time))
            {
                if (length(end.time)!=1)
                    stop(paste0(error.prefix, "'end.time' must be a SINGLE numeric value"))
                
                if (is.na(end.time))
                    stop(paste0(error.prefix, "'end.time' cannot be NA"))
                
                if (is.numeric(start.time))
                {
                    if (start.time >= end.time)
                        stop(paste0(error.prefix, "'start.time' (", start.time, 
                                    ") must be BEFORE 'end.time' (", end.time, ")"))
                }
            }
            
            # Check times - for NAs, for order
            if (is.numeric(times))
            {
                if (any(is.na(times)))
                    stop(paste0(error.prefix, "'times' cannot contain NA values"))
                
                # times must be ascending
                if (any(times[-1]<times[-length(times)]))
                    stop(paste0(error.prefix, "'times' must be in ascending order"))

                # times can be repeated at most once
                if (length(times)>2)
                {
                    times.minus.last.two = times[-(length(times)-1:0)]
                    times.minus.first.two = times[-(1:2)]
                    times.minus.first.last = times[-c(1, length(times))]
                    
                    if (any(times.minus.last.two==times.minus.first.last &
                            times.minus.first.two==times.minus.first.last))
                        stop(paste0(error.prefix, "the values of 'times' can be repeated at most once - you can't have the same time three times in a row"))
                }
                
                # times cannot be infinite
                if (any(is.infinite(times)))
                    stop(paste0(error.prefix, "'times' cannot contain any infinite values"))
                    
                # the first time must be >= the start time
                if (is.numeric(start.time) && start.time > times[1])
                    stop(paste0(error.prefix, "'start.time' (",
                                start.time, ") must be LESS THAN OR EQUAL TO the first element of 'times' (",
                                times[1], ")"))
                
                # the last time must be <= the end time
                if (is.numeric(end.time) && end.time < times[length(times)])
                    stop(paste0(error.prefix, "'end.time' (",
                                end.time, ") must be GREATER THAN OR EQUAL TO the last element of 'times' (",
                                times[length(times)], ")"))
            }
            else
            {
                if (any(sapply(times, function(one.time){
                    is.numeric(one.time) && any(is.na(one.time))
                })))
                    stop(paste0(error.prefix, "'times' cannot contain numeric vectors with NA values"))
            }
            
            # Check effect values - for NA and >= 0 if multiplier
            if (is.numeric(effect.values))
            {
                if (any(is.na(effect.values)))
                    stop(paste0(error.prefix, "'effect.values' cannot contain NA values"))
                
                if (apply.effects.as=='multiplier' & any(effect.values<0))
                    stop(paste0(error.prefix, "When 'apply.effects.as' is 'multiplier', corresponding 'effect.values' must be non-negative"))
                
                if (any(sapply(effect.values, function(one.effect){
                    is.numeric(one.effect) && any(is.na(one.effect))
                })))
                    stop(paste0(error.prefix, "'effect.values' cannot contain numeric vectors with NA values"))
            }
            
            
            # Store values
            private$i.quantity.name = quantity.name
            private$i.start.time = start.time
            private$i.effect.values = effect.values
            private$i.times = times
            private$i.end.time = end.time
            private$i.apply.effects.as = apply.effects.as
            private$i.scale = scale
            private$i.allow.values.less.than.otherwise = allow.values.less.than.otherwise
            private$i.allow.values.greater.than.otherwise = allow.values.greater.than.otherwise
            private$i.bindings = bindings
            private$i.times.are.resolved = is.numeric(times) && is.numeric(start.time) && is.numeric(end.time)
            private$i.is.resolved = is.numeric(effect.values) && private$i.times.are.resolved
            
            private$i.depends.on = character()
            private$i.effect.values.depend.on = character()
            private$i.all.times.depend.on = character()
            
            private$i.location = location
            private$i.version = version
            
            if (!private$i.is.resolved)
            {
                if (!is.numeric(start.time))
                    private$i.all.times.depend.on = c(private$i.all.times.depend.on, start.time$depends.on)
                
                if (!is.numeric(end.time))
                    private$i.all.times.depend.on = c(private$i.all.times.depend.on, end.time$depends.on)
                
                if (!is.numeric(times))
                    private$i.all.times.depend.on = c(private$i.all.times.depend.on, 
                                             unlist(sapply(times, function(one.time){
                                                 if (is.numeric(one.time))
                                                     character()
                                                 else
                                                     one.time$depends.on
                                             })))
                
                if (!is.numeric(effect.values))
                    private$i.effect.values.depend.on = unlist(sapply(effect.values, function(one.effect){
                        if (is.numeric(one.effect))
                            character()
                        else
                            one.effect$depends.on
                    }))
                
                private$i.effect.values.depend.on = setdiff(private$i.effect.values.depend.on, names(bindings))
                private$i.all.times.depend.on = setdiff(private$i.all.times.depend.on, names(bindings))
                
                private$i.depends.on = union(private$i.effect.values.depend.on, private$i.all.times.depend.on)
            }
            
            if (is.null(all.times))
            {
                if (private$i.times.are.resolved)
                    private$i.all.times = union_sorted_vectors(list(private$i.start.time,
                                                                    private$i.times,
                                                                    private$i.end.time))
                else
                    private$i.all.times = NULL
            }
            else
                private$i.all.times = all.times
        },
        
        anchor.location.and.version = function(location, specification.metadata, error.prefix='')
        {
            # Check location and specification.metadata
            if (!is.character(location) || length(location)!=1 || is.na(location))
                stop(paste0(error.prefix, "'location' must be a single, non-NA character value"))
            
            if (!is(specification.metadata, 'specification.metadata'))
                stop(paste0(error.prefix, "'specification.metadata' must be an object of class 'specification.metadata'"))
            
            
            if (!is.null(private$i.location) || !is.null(private$i.version))
            {
                if (private$i.location != location || private$i.version != specification.metadata$version)
                    stop(paste0(error.prefix, "Cannot anchor the intervention.effect's location and version. It has already been anchored to location '",
                                private$i.location, "' and version '", private$i.version, "'"))
                
                rv = self
            }
            else
            {
                # Resolve function bindings
                binding.is.function = !sapply(private$i.bindings, is.numeric)
                bindings = private$i.bindings
                bindings[binding.is.function] = 
                    lapply(names(bindings)[binding.is.function], function(name){
                        wrapper = bindings[[name]]
                        val = wrapper$execute(bindings=list(location=location,
                                                            specification.metadata=specification.metadata),
                                              error.prefix='')
                        
                        if (!is.numeric(val) || length(val)==0 || any(is.na(val)))
                            stop(paste0(error.prefix,
                                        "When evaluated, the stored binding for '",
                                        name, "', must be a non-empty, numeric vector with no NA values"))
                        
                        val
                    })
                
                # Make resolved version
                rv = JHEEM.INTERVENTION.EFFECT$new(quantity.name = private$i.quantity.name,
                                                   start.time = private$i.start.time,
                                                   effect.values = private$i.effect.values,
                                                   times = private$i.times,
                                                   end.time = private$i.end.time,
                                                   apply.effects.as = private$i.apply.effects.as,
                                                   scale = private$i.scale,
                                                   allow.values.less.than.otherwise = private$i.allow.values.less.than.otherwise,
                                                   allow.values.greater.than.otherwise = private$i.allow.values.greater.than.otherwise,
                                                   bindings = bindings,
                                                   location = location,
                                                   version = specification.metadata$version,
                                                   error.prefix = error.prefix)
            }
            
            if (length(rv$depends.on)==0 && !rv$is.resolved)
                rv$resolve(error.prefix)
            else
                rv
        },
        
        resolve = function(bindings, error.prefix='')
        {
            if (private$i.is.resolved)
                self
            else
            {
                if (!self$is.anchored)
                    stop(paste0(error.prefix, "Cannot resolve intervention.effects until they have been anchored to a location and version using $anchor.location.and.version()"))
                    
                # Check for missing bindings
                missing.bindings = setdiff(private$i.depends.on, names(bindings))
                if (length(missing.bindings)>0)
                    stop(paste0(error.prefix, "Cannot resolve the intervention effect with the following ",
                                ifelse(length(missing.bindings)==1, "binding", "bindings"),
                                "missing: ",
                                collapse.with.and("'", missing.bindings, "'")))
                
                # Make sure bindings are all numeric - if not, we have not anchored to location/version
                if (any(!sapply(bindings, is.numeric)))
                    stop(paste0(error.prefix, "Cannot resolve intervention.effect - it has not been anchored to a location or version"))
                
                # Overwrite with stored bindings
                bindings[names(private$i.bindings)] = private$i.bindings
                
                if (is.numeric(private$i.start.time))
                    start.time = private$i.start.time
                else
                    start.time = private$i.start.time$evaluate(bindings, error.prefix = error.prefix)
                
                if (is.numeric(private$i.end.time))
                    end.time = private$i.end.time
                else
                    end.time = private$i.end.time$evaluate(bindings, error.prefix = error.prefix)
                
                if (is.numeric(private$i.times))
                    times = private$i.times
                else
                    times = unlist(lapply(private$i.times, function(one.time){
                        if (is.numeric(one.time))
                            one.time
                        else
                            one.time$evaluate(bindings, error.prefix = error.prefix)
                    }))
                
                if (is.numeric(private$i.effect.values))
                    effect.values = private$i.effect.values
                else
                    effect.values = unlist(lapply(private$i.effect.values, function(one.effect){
                        if (is.numeric(one.effect))
                            one.effect
                        else
                            one.effect$evaluate(bindings, error.prefix = error.prefix)
                    }))
                
                JHEEM.INTERVENTION.EFFECT$new(quantity.name = private$i.quantity.name,
                                              start.time = start.time,
                                              effect.values = effect.values,
                                              times = times,
                                              end.time = end.time,
                                              apply.effects.as = private$i.apply.effects.as,
                                              scale = private$i.scale,
                                              allow.values.less.than.otherwise = private$i.allow.values.less.than.otherwise,
                                              allow.values.greater.than.otherwise = private$i.allow.values.greater.than.otherwise,
                                              bindings = NULL,
                                              location = private$i.location,
                                              version = private$i.version,
                                              error.prefix = error.prefix)
            }
        }
    ),
    
    active = list(
        
        bindings = function(value)
        {
            if (missing(value))
                private$i.bindings
            else
                stop("Cannot set a jheem.intervention.effect's 'bindings' - they are read only")
        },
        
        
        is.anchored = function(value)
        {
            if (missing(value))
                !is.null(private$i.location)
            else
                stop("Cannot set a jheem.intervention.effect's 'is.anchored' - it is read only")
        },
        
        quantity.name = function(value)
        {
            if (missing(value))
                private$i.quantity.name
            else
                stop("Cannot set a jheem.intervention.effect's 'quantity.name' - it is read only")
        },
        
        start.time = function(value)
        {
            if (missing(value))
            {
                if (is.numeric(private$i.start.time))
                    private$i.start.time
                else
                    NULL
            }
            else
                stop("Cannot set a jheem.intervention.effect's 'start.time' - it is read only")
        },
        
        effect.values = function(value)
        {
            if (missing(value))
            {
                if (is.numeric(private$i.effect.values))
                    private$i.effect.values
                else
                    NULL
            }
            else
                stop("Cannot set a jheem.intervention.effect's 'effect.values' - they are read only")
        },
        
        times = function(value)
        {
            if (missing(value))
            {
                if (is.numeric(private$i.times))
                    private$i.times
                else
                    NULL
            }
            else
                stop("Cannot set a jheem.intervention.effect's 'times' - they are read only")
        },
        
        end.time = function(value)
        {
            if (missing(value))
            {
                if (is.numeric(private$i.end.time))
                    private$i.end.time
                else
                    NULL
            }
            else
                stop("Cannot set a jheem.intervention.effect's 'end.time' - it is read only")
        },
        
        all.times = function(value)
        {
            if (missing(value))
            {
                private$i.all.times
            }
            else
                stop("Cannot set a jheem.intervention.effect's 'all.times' - they are read only")
        },
        
        step.change.times = function(value)
        {
            if (missing(value))
            {
                mask = private$i.all.times[-1] == private$i.all.times[-length(private$i.all.times)]
                private$i.all.times[-1][mask]
            }
            else
                stop("Cannot set a jheem.intervention.effect's 'step.change.times' - they are read only")
        },
        
        apply.effects.as = function(value)
        {
            if (missing(value))
                private$i.apply.effects.as
            else
                stop("Cannot set a jheem.intervention.effect's 'apply.effects.as' - it is read only")
        },
        
        # This and the following two bindings are provided to make the cpp interface in engine_helpers.cpp easier to code
        apply.effects.as.value = function(value)
        {
            if (missing(value))
                private$i.apply.effects.as == 'value'
            else
                stop("Cannot set a jheem.intervention.effect's 'apply.effects.as.value' - it is read only")
        },
        
        apply.effects.as.multiplier = function(value)
        {
            if (missing(value))
                private$i.apply.effects.as == 'multiplier'
            else
                stop("Cannot set a jheem.intervention.effect's 'apply.effects.as.value' - it is read only")
        },
        
        apply.effects.as.addend = function(value)
        {
            if (missing(value))
                private$i.apply.effects.as == 'addend'
            else
                stop("Cannot set a jheem.intervention.effect's 'apply.effects.as.value' - it is read only")
        },
        
        scale = function(value)
        {
            if (missing(value))
                private$i.scale
            else
                stop("Cannot set a jheem.intervention.effect's 'scale' - it is read only")
        },
        
        allow.values.less.than.otherwise = function(value)
        {
            if (missing(value))
                private$i.allow.values.less.than.otherwise
            else
                stop("Cannot set a jheem.intervention.effect's 'allow.values.less.than.otherwise' - it is read only")
        },
        
        allow.values.greater.than.otherwise = function(value)
        {
            if (missing(value))
                private$i.allow.values.greater.than.otherwise
            else
                stop("Cannot set a jheem.intervention.effect's 'allow.values.greater.than.otherwise' - it is read only")
        },
        
        is.resolved = function(value)
        {
            if (missing(value))
                private$i.is.resolved
            else
                stop("Cannot set a jheem.intervention.effect's 'is.resolved' - it is read only")
        },
        
        times.are.resolved = function(value)
        {
            if (missing(value))
                private$i.times.are.resolved
            else
                stop("Cannot set a jheem.intervention.effect's 'times.are.resolved' - it is read only")
        },
        
        depends.on = function(value)
        {
            if (missing(value))
                private$i.depends.on
            else
                stop("Cannot set a jheem.intervention.effect's 'depends.on' - it is read only")
        },
        
        effect.values.depend.on = function(value)
        {
            if (missing(value))
                private$i.effect.values.depend.on
            else
                stop("Cannot set a jheem.intervention.effect's 'effect.values.depend.on' - it is read only")
        },
        
        all.times.depend.on = function(value)
        {
            if (missing(value))
                private$i.all.times.depend.on
            else
                stop("Cannot set a jheem.intervention.effect's 'all.times.depend.on' - it is read only")
        },
        
        location = function(value)
        {
            if (missing(value))
                private$i.location
            else
                stop("Cannot set a jheem.intervention.effect's 'location' - it is read only")
        },
        
        version = function(value)
        {
            if (missing(value))
                private$i.version
            else
                stop("Cannot set a jheem.intervention.effect's 'version' - it is read only")
        }
    ),
    
    private = list(
        
        i.quantity.name = NULL,
        i.start.time = NULL,
        i.effect.values = NULL,
        i.times = NULL,
        i.end.time = NULL,
        i.all.times = NULL,
        i.apply.effects.as = NULL,
        i.scale = NULL,
        i.allow.values.less.than.otherwise = NULL,
        i.allow.values.greater.than.otherwise = NULL,
        i.bindings = NULL,
        i.depends.on = NULL,
        i.effect.values.depend.on = NULL,
        i.all.times.depend.on = NULL,
        i.is.resolved = NULL,
        i.times.are.resolved = NULL,
        i.location = NULL,
        i.version = NULL
    )
)
