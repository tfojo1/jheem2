
##----------------------##
##----------------------##
##-- PUBLIC INTERFACE --##
##----------------------##
##----------------------##


##-----------------------------------------------##
##-- FUNCTIONS to MODIFY the ENGINE'S SETTINGS --##
##-----------------------------------------------##

#'@title Set the Value of a Model Element
#'
#'@param model.settings An object of class 'jheem.model.settings'
#'@param element.name The name of the model element to update a value for
#'@param value The new value for the model element
#'@param check.consistency A logical indicator whether to sanitize arguments and check for consistency during calculations. Setting to F increases performance
#'
#'@family Functions to modify model settings
#'
#'@export
set.element.value <- function(model.settings,
                              element.name,
                              value,
                              check.consistency = !jheem.engine$has.been.crunched())
{
    if (!is(model.settings, "R6") || !is(model.settings, "jheem.model.settings"))
        stop("model.settings must be an R6 object of class 'jheem.model.settings'")
    
    model.settings$set.element.value(element.name = element.name,
                                     value = value,
                                     check.consistency = check.consistency)
}

#'@title Set Main Effect Alpha Values for a Functional Form for a Model Element
#'
#'@details Alphas for a 'main effect' apply to all values in a dimension (as opposed to 'interaction effects', which apply to combinations of values)
#'
#'@inheritParams set.element.value
#'@param element.name The name of the model element whose functional form we wish to modify
#'@param alpha.name The name of the functional form parameter that we want to set alphas for
#'@param values A numeric vector of values (either a single value applied to all dimension values OR a vector of the same length as applies.to.dimension.values)
#'@param applies.to.dimension.values Either a vector (character or integer) or a list where each element is either a single character value or single integer value. Must have the same length as values, indicating the compartment to which each value in values applies
#'@param dimensions The dimensions to which values in applies.to.dimension.values apply. Can be either (1) a character vector with the same length as applies.to.dimension.values with the corresponding dimension for each value, (2) a single character value - a dimension that applies to all values, or (3) NULL, in which case the dimensions are inferred for applies.to.dimension.values (this is computationally slower)
#'
#'@family Functions to create and modify a jheem.engine
#'
#'@export
set.element.functional.form.main.effect.alphas <- function(model.settings,
                                                           element.name,
                                                           alpha.name,
                                                           values,
                                                           applies.to.dimension.values=names(values),
                                                           dimensions=names(applies.to.dimension.values),
                                                           check.consistency = !jheem.engine$has.been.crunched())
{
    if (!is(jheem.engine, "R6") || !is(jheem.engine, "jheem.engine"))
        stop("jheem.engine must be an R6 object of class 'jheem.engine'")
    
    model.settings$set.element.functional.form.main.effect.alphas(element.name = element.name,
                                                                alpha.name = alpha.name,
                                                                values = values,
                                                                applies.to.dimension.values = applies.to.dimension.values,
                                                                dimensions = dimensions,
                                                                check.consistency = check.consistency)
}

#'@title Set Interaction Alpha Values for a Functional Form for a Model Element
#'
#'@details Alphas for an 'interaction' will apply to every combination of applies.to.dimension.values from different dimensions (as opposed to 'main effect' apply to all values in a dimension
#'
#'@inheritParams set.element.functional.form.main.effect.alphas
#'@param value A single numeric value, which will apply to every combination of applies.to.dimension.values which are from different dimensions
#'@param applies.to.dimension.values Either a vector (character or integer) or a list where each element is either a single character value or single integer value, indicating the compartments to which value applies
#'
#'@family Functions to create and modify a jheem.engine
#'
#'@export
set.element.functional.form.interaction.alphas <- function(model.settings,
                                                           element.name,
                                                           alpha.name,
                                                           value,
                                                           applies.to.dimension.values=names(values),
                                                           dimensions=names(applies.to.dimension.values),
                                                           check.consistency = !jheem.engine$has.been.crunched())
{
    if (!is(jheem.engine, "R6") || !is(jheem.engine, "jheem.engine"))
        stop("jheem.engine must be an R6 object of class 'jheem.engine'")
    
    model.settings$set.element.functional.form.interaction.alphas(element.name = element.name,
                                                                alpha.name = alpha.name,
                                                                value = value,
                                                                applies.to.dimension.values = applies.to.dimension.values,
                                                                dimensions = dimensions,
                                                                check.consistency = check.consistency)
}

#'@family Functions to create and modify a jheem.engine
#' Still need to flesh out this interface
set.element.foreground <- function(model.settings,
                                   foreground,
                                   check.consistency = !self$has.been.crunched())
{
    if (!is(jheem.engine, "R6") || !is(jheem.engine, "jheem.engine"))
        stop("jheem.engine must be an R6 object of class 'jheem.engine'")
    
    stop("Need to implement")
}

#'@title Set the Times From and To Which the Functional Form Determines a Model Element's Value
#'
#'@inheritParams set.element.value
#'@param element.name The name of the model element to set functional.form times for
#'@param from.time,to.time The time from or to which the functional form is active
#'
#'@family Functions to create and modify a jheem.engine
#'
#'@export
set.element.functional.form.from.time <- function(model.settings,
                                                  element.name,
                                                  from.time,
                                                  check.consistency = !jheem.engine$has.been.crunched())
{
    if (!is(jheem.engine, "R6") || !is(jheem.engine, "jheem.engine"))
        stop("jheem.engine must be an R6 object of class 'jheem.engine'")
    
    model.settings$set.element.functional.form.from.time(element.name = element.name,
                                                       from.time = from.time,
                                                       check.consistency = check.consistency)
}


#'@describeIn set.element.functional.form.from.time
#'
#'@export
set.element.functional.form.to.time <- function(model.settings,
                                                element.name,
                                                to.time,
                                                check.consistency = !jheem.engine$has.been.crunched())
{
    if (!is(jheem.engine, "R6") || !is(jheem.engine, "jheem.engine"))
        stop("jheem.engine must be an R6 object of class 'jheem.engine'")
    
    model.settings$set.element.functional.form.to.time(element.name = element.name,
                                                     to.time = to.time,
                                                     check.consistency = check.consistency)
}


#'@title Set a ramp or taper (times and values) for a model element
#'
#'@details A ramp is a set of multipliers which are multiplied by the first functional.form value (ie, the value produced at functional.form.from.time) for an element. A taper is a set of multipliers which are multiplied by the last functional.form value (ie, the value produced at functional.form.to.time) for an element
#'
#'@inheritParams set.element.value
#'@param element.name The name of the model element to set the ramp/taper for
#'@param times,values The times or values to set for the ramp/taper
#'@param indices The indices of the ramp/taper to set times or values for. Can be either numeric indices, character indices (if the ramp/taper was named when setting up the specification), or logical indices
#'
#'@family Functions to create and modify a jheem.engine
#'
#'@export
set.element.ramp.times <- function(model.settings,
                                   element.name,
                                   times,
                                   indices=1:length(times),
                                   check.consistency = !jheem.engine$has.been.crunched())
{
    if (!is(jheem.engine, "R6") || !is(jheem.engine, "jheem.engine"))
        stop("jheem.engine must be an R6 object of class 'jheem.engine'")
    
    model.settings$set.element.ramp.times(element.name = element.name,
                                        times = times,
                                        indices = indices,
                                        check.consistency = check.consistency)
}

#'@describeIn set.element.ramp.times
#'
#'@export
set.element.ramp.values <- function(model.settings,
                                    element.name,
                                    values,
                                    indices=1:length(values),
                                    check.consistency = !jheem.engine$has.been.crunched())
{
    if (!is(jheem.engine, "R6") || !is(jheem.engine, "jheem.engine"))
        stop("jheem.engine must be an R6 object of class 'jheem.engine'")
    
    model.settings$set.element.ramp.values(element.name = element.name,
                                         values = values,
                                         indices = indices,
                                         check.consistency = check.consistency)
}

#'@describeIn set.element.ramp.times
#'
#'@export
set.element.taper.times <- function(model.settings,
                                    element.name,
                                    times,
                                    indices=1:length(times),
                                    check.consistency = !jheem.engine$has.been.crunched())
{
    if (!is(jheem.engine, "R6") || !is(jheem.engine, "jheem.engine"))
        stop("jheem.engine must be an R6 object of class 'jheem.engine'")
    
    model.settings$set.element.taper.times(element.name = element.name,
                                         times = times,
                                         indices = indices,
                                         check.consistency = check.consistency)
}

#'@describeIn set.element.ramp.times
#'
#'@export
set.element.taper.values <- function(model.settings,
                                     element.name,
                                     values,
                                     indices=1:length(values),
                                     check.consistency = !jheem.engine$has.been.crunched())
{
    if (!is(jheem.engine, "R6") || !is(jheem.engine, "jheem.engine"))
        stop("jheem.engine must be an R6 object of class 'jheem.engine'")
    
    model.settings$set.element.taper.values(element.name = element.name,
                                          values = values,
                                          indices = indices,
                                          check.consistency = check.consistency)
}

#'@title Set a "Future Slope" and the time after which it applies, for a model element
#'
#'@details A future slope is applied to model elements with a functional.form set AFTER a given point in time
#'
#'@inheritParams set.element.value
#'@param element.names One or more names of the model element to set the future.slope for
#'@param slope A single numeric value representing the slope
#'@param after.year The year after which the additional slope takes effect
#'
#'@family Functions to create and modify a jheem.engine
#'
#'@export
set.element.functional.form.future.slope <- function(model.settings,
                                                     element.names,
                                                     slope,
                                                     check.consistency = !jheem.engine$has.been.crunched())
{
    if (!is(jheem.engine, "R6") || !is(jheem.engine, "jheem.engine"))
        stop("jheem.engine must be an R6 object of class 'jheem.engine'")
    
    model.settings$set.element.functional.form.future.slope(element.names = element.names,
                                                          slope = slope,
                                                          check.consistency = check.consistency)
}


#'@describeIn set.element.functional.form.future.slope
#'
#'@export
set.element.functional.form.future.slope.after.time <- function(model.settings,
                                                                element.names,
                                                                after.year,
                                                                check.consistency = !jheem.engine$has.been.crunched())
{
    if (!is(jheem.engine, "R6") || !is(jheem.engine, "jheem.engine"))
        stop("jheem.engine must be an R6 object of class 'jheem.engine'")
    
    model.settings$set.element.functional.form.future.slope.after.time(element.names = element.names,
                                                                     after.year = after.year,
                                                                     check.consistency = check.consistency)
}


##-------------------------------------------------------------------------##
##-- SOME STREAMLINED, CONVENIENCE FUNCTIONS FOR SETTING MULTIPLE VALUES --##
##-------------------------------------------------------------------------##

#'@title Set Values for Multiple Model Elements
#'
#'@inheritParams set.element.value
#'@param element.names A character vector containing the names of the elements to set values for
#'@param parameters A named numeric vector with values for each of the elements of 'element.names'
#'
#'@return The names of the elements that had values set
#'
#'@family Functions to create and modify a jheem.engine
#'
#'@export
set.element.values.from.parameters <- function(model.settings,
                                               element.names,
                                               parameters,
                                               check.consistency = !jheem.engine$has.been.crunched())
{
    if (!is(jheem.engine, "R6") || !is(jheem.engine, "jheem.engine"))
        stop("jheem.engine must be an R6 object of class 'jheem.engine'")
    
    if (is.null(element.names))
        element.names = intersect(names(parameters), jheem.engine$element.names)
    
    if (check.consistency)
    {
        if (!is.numeric(parameters))
            stop("Cannot set element values from parameters: 'parameters' must be a named NUMERIC vector")
        
        if (is.null(names(parameters)))
            stop("Cannot set element values from parameters: 'parameters' must be a NAMED numeric vector")
        
        if (any(is.na(names(parameters))))
            stop("Cannot set element values from parameters: the names of 'parameters' cannot be NA")
            
        
        missing.elements = setdiff(element.names, names(parameters))
        if (length(missing.elements)>0)
            stop(paste0("Cannot set element values from parameters: 'parameters' is missing ",
                        ifelse(length(missing.elements)==1, 'the value for element', 'values for elements'),
                        " ", collapse.with.and("'", missing.elements, "'")))
    }
    
    for (elem.name in element.names)
        model.settings$set.element.value(element.name = elem.name,
                                       value = parameters[elem.name],
                                       check.consistency = check.consistency)
    
    # Return
    element.names
}

#'@title Set multiple functional form main effect alphas from parameters
#'
#'@inheritParams set.element.functional.form.main.effect.alphas
#'@param parameters A named numeric vector with values for interaction terms
#'@param parameter.name.prefix,parameter.name.suffix Character values to be prepended and addended to names of specific dimension values to generate specific parameter names
#'@param dimensions.with.values.referred.to.by.name Dimensions for which we should look for main effects in parameters, and make parameter names by combining parameter.name.prefix and parameter.name.suffix with the actual dimension values for the dimension
#'@param dimensions.with.values.referred.to.by.index Dimensions for which we should look for main effects in parameters, and make parameter names by combining parameter.name.prefix and parameter.name.suffix with indices of the values in each dimension
#'@param throw.error.if.no.parameters A logical indicator - if no parameter names match the pattern parameter.name.prefix<x>parameter.name.suffix, whether an error should be thrown
#'
#'@details This function will search for parameters with names in the format of either 
#'          parameter.name.prefix<dimension values for dimensions.with.values.referred.to.by.name>parameter.name.suffix 
#'          OR
#'          parameter.name.prefix<dimension in dimension.values.referred.to.by.name><1:length of dimension>parameter.name.suffix
#'          
#'          
#'@return The names of parameters that were used
#'
#'@export
set.element.functional.form.alphas.from.parameters <- function(model.settings,
                                                               element.name,
                                                               alpha.name,
                                                               parameters,
                                                               parameter.name.prefix,
                                                               parameter.name.suffix,
                                                               dimensions.with.values.referred.to.by.name = character(),
                                                               dimensions.with.values.referred.to.by.index = character(),
                                                               throw.error.if.no.parameters = T,
                                                               check.consistency = !jheem.engine$has.been.crunched())
{
    if (!is(jheem.engine, "R6") || !is(jheem.engine, "jheem.engine"))
        stop("jheem.engine must be an R6 object of class 'jheem.engine'")
    
    specification.metadata = model.settings$specification.metadata
    
    #-- Check Arguments --#
    if (check.consistency)
    {
        if (!is.numeric(parameters))
            stop("Cannot set functional.form alphas from parameters: 'parameters' must be a named NUMERIC vector")
        
        if (is.null(names(parameters)))
            stop("Cannot set functional.form alphas from parameters: 'parameters' must be a NAMED numeric vector")
        
        if (any(is.na(names(parameters))))
            stop("Cannot set functional.form alphas from parameters: the names of 'parameters' cannot be NA")
        
        invalid.dimensions.by.name = setdiff(dimensions.with.values.referred.to.by.name, 
                                             specification.metadata$dimensions)
        if (length(invalid.dimensions.by.name)>1)
            stop(paste0("Cannot set functional.form alphas from parameters: ",
                        collapse.with.and("'", invalid.dimensions.by.name, "'"),
                        " passed to 'dimensions.with.values.referred.to.by.name' ",
                        ifelse(length(invalid.dimensions.by.name)==1, "is not a valid dimension", "are not valid dimensions"),
                        " in the specification for version '", model.settings$version, "'"))
        
        invalid.dimensions.by.index = setdiff(dimensions.with.values.referred.to.by.index, 
                                             specification.metadata$dimensions)
        if (length(invalid.dimensions.by.index)>1)
            stop(paste0("Cannot set functional.form alphas from parameters: ",
                        collapse.with.and("'", invalid.dimensions.by.index, "'"),
                        " passed to 'dimensions.with.values.referred.to.by.index' ",
                        ifelse(length(invalid.dimensions.by.name)==1, "is not a valid dimension", "are not valid dimensions"),
                        " in the specification for version '", model.settings$version, "'"))
    }
    
    #-- Values for main effects --#
    
    # Values referred to by name
    if (length(dimensions.with.values.referred.to.by.name)>0)
    {
        parameter.dim.values = unlist(sapply(dimensions.with.values.referred.to.by.name, function(d){
            specification.metadata$dim.names[[d]]
        }))
        
        parameter.names = paste0(parameter.name.prefix, parameter.dim.values, parameter.name.suffix)
        
        parameter.dimensions = unlist(sapply(dimensions.with.values.referred.to.by.name, function(d){
            rep(d, length(specification.metadata$dim.names[[d]]))
        }))
    }
    else
        parameter.dim.values = parameter.names = parameter.dimensions = character()
    
        
    # Values referred to by index
    if (length(dimensions.with.values.referred.to.by.index)>0)
    {
        parameter.names = c(parameter.names, unlist(sapply(dimensions.with.values.referred.to.by.index, function(d){
            paste0(parameter.name.prefix, 
                   d, 1:length(specification.metadata$dim.names[[d]]),
                   parameter.name.suffix)
        })))
        
        parameter.dim.values = c(parameter.dim.values, unlist(sapply(dimensions.with.values.referred.to.by.index, function(d){
            specification.metadata$dim.names[[d]]
        })))
        
        parameter.dimensions = c(parameter.dimensions, unlist(sapply(dimensions.with.values.referred.to.by.index, function(d){
            rep(d, length(specification.metadata$dim.names[[d]]))
        })))
    }
    
    # Filter out only what's present
    parameter.values = parameters[parameter.names]
    mask = !is.na(parameter.values)
    
    parameter.values = parameter.values[mask]
    parameter.names = parameter.names[mask]
    parameter.dimensions = parameter.dimensions[mask]
    parameter.dim.values = parameter.dim.values[mask]
    
    if (length(parameter.values)==0)
    {
        if (throw.error.if.no.parameters)
            stop(paste0("Error setting functional.form alphas from parameters: no parameter names match the pattern '", parameter.name.prefix, "<x>", parameter.name.suffix, "'"))
    }
    else
    {
        # Push the main effect values to the engine
        model.settings$set.element.functional.form.main.effect.alphas(element.name = element.name,
                                                                    alpha.name = alpha.name,
                                                                    values = parameter.values,
                                                                    applies.to.dimension.values = parameter.dim.values,
                                                                    dimensions = parameter.dimensions, 
                                                                    check.consistency = check.consistency)
    }
    
    
    #-- Done, return parameter names --#
    parameter.names
}


##-----------------------##
##-----------------------##
##-- CLASS DEFINITIONS --##
##-----------------------##
##-----------------------##


##---------------##
##-- CONSTANTS --##
##---------------##

JHEEM.ENGINE.CODE.ITERATION = '2.0'


##------------------------------------##
##-- THE JHEEM MODEL SETTINGS CLASS --##
##                                    ##
##   This class is a pass-through     ##
##   wrapper to the jheem.engine      ##
##   class, so that we can expose     ##
##   some of the engine's methods     ##
##   while protecting others          ##
##------------------------------------##

JHEEM.MODEL.SETTINGS = R6::R6Class(
    'jheem.model.settings',
    portable = F,
    
    public = list(
        
        initialize = function(jheem.engine, check.consistency)
        {
            if (!is(jheem.engine, 'jheem.engine'))
                stop("Cannot create JHEEM Model Settings: jheem.engine must be an object of class 'jheem.engine'")
            
            if (!is.logical(check.consistency) || length(check.consistency)!=1 || is.na(check.consistency))
                stop("Cannot create JHEEM Model Settings: 'check.consistency' must be a single, non-NA logical value")
            
            private$i.engine = jheem.engine
            private$i.check.consistency = check.consistency
        },
        
        set.element.value = function(element.name,
                                     value)
        {
            private$i.engine$set.element.value(element.name = element.name,
                                               value = value,
                                               check.consistency = private$i.check.consistency)
        },
        
        set.element.functional.form.main.effect.alphas = function(element.name,
                                                                  alpha.name,
                                                                  values,
                                                                  applies.to.dimension.values=names(values),
                                                                  dimensions=names(applies.to.dimension.values))
        {
            private$i.engine$set.element.functional.form.main.effect.alphas(element.name = element.name,
                                                                            alpha.name = alpha.name,
                                                                            values = values,
                                                                            applies.to.dimension.values = applies.to.dimension.values,
                                                                            dimensions = dimensions,
                                                                            check.consistency = private$i.check.consistency)
        },
        
        set.element.functional.form.interaction.alphas = function(element.name,
                                                                  alpha.name,
                                                                  value,
                                                                  applies.to.dimension.values=names(values),
                                                                  dimensions=names(applies.to.dimension.values))
        {
            private$i.engine$set.element.functional.form.interaction.alphas(element.name = element.name,
                                                                            alpha.name = alpha.name,
                                                                            value = value,
                                                                            applies.to.dimension.values = applies.to.dimension.values,
                                                                            dimensions = dimensions,
                                                                            check.consistency = private$i.check.consistency)
        },
        
        #' Still need to flesh out this interface
        set.quantity.foreground = function(foreground)
        {
            private$i.engine$set.quantity.foreground(foreground = foreground,
                                                     check.consistency = private$i.check.consistency)
        },
        
        set.element.functional.form.from.time = function(element.name,
                                                         from.time)
        {
            private$i.engine$set.element.functional.form.from.time(element.name = element.name,
                                                                   from.time = from.time,
                                                                   check.consistency = private$i.check.consistency)
        },
        
        set.element.functional.form.to.time = function(element.name,
                                                       to.time)
        {
            private$i.engine$set.element.functional.form.to.time(element.name = element.name,
                                                                 to.time = to.time,
                                                                 check.consistency = private$i.check.consistency)
        },
        
        set.element.ramp.times = function(element.name,
                                          times,
                                          indices=1:length(times))
        {
            private$i.engine$set.element.ramp.times(element.name = element.name,
                                                    times = times,
                                                    indices = indices,
                                                    check.consistency = private$i.check.consistency)
        },
        
        set.element.ramp.values = function(element.name,
                                           values,
                                           indices=1:length(values))
        {
            private$i.engine$set.element.ramp.values(element.name = element.name,
                                                     values = values,
                                                     indices = indices,
                                                     check.consistency = private$i.check.consistency)
        },
        
        set.element.taper.times = function(element.name,
                                           times,
                                           indices=1:length(times))
        {
            private$i.engine$set.element.taper.times(element.name = element.name,
                                                     times = times,
                                                     indices = indices,
                                                     check.consistency = private$i.check.consistency)
        },
        
        set.element.taper.values = function(element.name,
                                            values,
                                            indices=1:length(values))
        {
            private$i.engine$set.element.taper.values(element.name = element.name,
                                                      values = values,
                                                      indices = indices,
                                                      check.consistency = private$i.check.consistency)
        },
        
        set.element.functional.form.future.slope = function(element.names,
                                                            slope)
        {
            private$i.engine$set.element.functional.form.future.slope(element.names,
                                                                      slope,
                                                                      check.consistency = private$i.check.consistency)
        },
        
        set.element.functional.form.future.slope.after.time = function(element.names,
                                                                       after.time)
        {
            private$i.engine$set.element.functional.form.future.slope.after.time(element.names = element.names,
                                                                                 after.time = after.time,
                                                                                 check.consistency = private$i.check.consistency)
        }
        
    ),
    
    active = list(
        
    ),
    
    private = list(
        
        i.engine = NULL,
        i.check.consistency = NULL,
        
        get.current.code.iteration = function()
        {
            JHEEM.ENGINE.CODE.ITERATION
        }
    )
)

##----------------------------##
##-- THE JHEEM ENGINE CLASS --##
##----------------------------##

JHEEM.ENGINE = R6::R6Class(
    'jheem.engine',
    inherit = JHEEM.ENTITY,
    portable = F,
    
    public = list(
        
        check = function()
        {
            browser()
        },
        
        ##-----------------##
        ##-- CONSTRUCTOR --##
        ##-----------------##
        
        initialize = function(version, location)
        {
            # Call the superclass constructor
            super$initialize(version = version,
                             location = location,
                             type = 'engine',
                             error.prefix = "Cannot create JHEEM Engine: ")
            
            private$set.up()
        },
        
        
        ##------------------------##
        ##-- MAIN RUN FUNCTIONS --##
        ##------------------------##
        
        crunch = function(start.year,
                          end.year,
                          check.consistency = !self$has.been.crunched())
        {
            # If the specification has changed since we last crunched/set-up, reset
            specification = get.specification.for.version(private$i.version)
            if (specification$iteration != self$specification.metadata$specification.iteration)
                private$set.up()
            
            # Set the times
            set.run.years(start.year = start.year,
                             end.year = end.year,
                             error.prefix = paste0("Error preparing JHEEM Engine to run: "))

            # Do the work
            specification = private$get.specification()
            sapply(specification$top.level.quantity.names, calculate.quantity.value,
                   check.consistency = check.consistency)
            
            private$i.diffeq.settings = prepare.diffeq.settings(settings = private$i.diffeq.settings,
                                                                quantity.dim.names = private$i.quantity.dim.names,
                                                                quantity.values = private$i.quantity.values,
                                                                error.prefix = paste0("Error preparing JHEEM Engine to run (while setting up the diffeq interface): "))
            
            # Set the i.has.been.crunched flag
            private$i.has.been.crunched = T
            
            # Done
            invisible(self)
        },
        
        run = function(start.year,
                       end.year,
                       check.consistency=!self$has.been.crunched(),
                       max.run.time.seconds=Inf,
                       prior.sim=NULL,
                       keep.years=start.year:end.year,
                       atol=1e-04, rtol=1e-04)
        {
            # Crunch
            self$crunch(start.year = start.year,
                        end.year = end.year,
                        check.consistency = check.consistency)
            
            # Handoff to the Rcpp
            
            initial.state = numeric(private$i.diffeq.settings$state_length)
            compute.fn = function(x, t){
                compute_dx(state = x,
                           time = t,
                           settings = private$i.diffeq.settings,
                           quantity_scratch_vector = private$i.diffeq.settings$quantity_scratch_vector,
                           scratch_vector = private$i.diffeq.settings$scratch_vector,
                           quantity_values = private$i.quantity.values,
                           quantity_times = private$i.quantity.value.times,
                           quantity_scratch_offsets = private$i.diffeq.settings$quantity_scratch_offsets,
                           natality_info = private$i.diffeq.settings$natality_info,
                           mortality_info = private$i.diffeq.settings$mortality_info,
                           transitions_info = private$i.diffeq.settings$transitions_info,
                           infections_info = private$i.diffeq.settings$infections_info,
                           remission_info = private$i.diffeq.settings$remission_info,
                           fixed_strata_info = private$i.diffeq.settings$fixed_strata_info,
                           population_trackers = private$i.diffeq.settings$population_trackers)
            }
            
            ode.results = odeintr::integrate_sys(sys = compute.fn,
                                                 init = private$i.diffeq.settings$initial_state,
                                                 duration = x, 
                                                 start = x,
                                                 atol = atol,
                                                 rtol = rtol)
            
            # Process the Results
            
            # for now, just return
            ode.results
        },
        
        ##------------------------------##
        ##-- MODIFY ELEMENT FUNCTIONS --##
        ##------------------------------##
    
        set.element.value = function(element.name,
                                     value,
                                     check.consistency = !self$has.been.crunched(),
                                     error.prefix='')
        {
            #-- Check Arguments --#
            if (check.consistency)
            {
                if (!is.character(element.name) || length(element.name)!=1 || is.na(element.name))
                    stop(paste0(error.prefix, "Cannot set element value - 'element.name' must be a single, non-NA, character value"))
                
                if (all(private$i.element.names!=element.name))
                    stop(paste0(error.prefix, "Cannot set element value - no element named '", element.name, "' exists for model specificaiton '", self$version, "'"))
                
                if (!is.null(private$i.element.backgrounds[[element.name]]$functional.form))
                    stop(paste0(error.prefix, 
                                "Cannot set value for element '", element.name,
                                "' - a functional.form has been specified; set.element.value can only be used when there is no functional.form"))
                
                # Check value dimensions
                if (!is.numeric(value))
                    stop(paste0(error.prefix,
                                "Cannot set value for element '", element.name,
                                "' - value must be a numeric object"))
                
                verify.dim.names.for.quantity(dim.names = dimnames(value),
                                              quantity = private$get.specification()$get.quantity(element.name),
                                              variable.name.for.error = "the dimnames of 'value'",
                                              error.prefix = paste0(error.prefix, "Cannot set value for element '", element.name, "' - "),
                                              wrt.version = self$version)
            }
            
            if (length(value)==1)
                names(value) = NULL

            
            
            #-- Clear Dependencies --#
            # Clear all values
            clear.dependent.values(element.name)
            
            # Clear dim.names (only if value's dim.names have changed)
            if (!dim.names.equal(dimnames(value),
                                 dimnames(private$i.element.backgrounds[[element.name]]$value),
                                 match.order.of.dimensions = T, match.order.within.dimensions = T))
                clear.dim.names(element.name)
            
            # No need to clear times
            
            
            #-- Set it --#
            private$i.element.backgrounds[[element.name]]$value = value
            
            
            #-- Log Instruction --#
            log.instruction(fn.name='set.element.value', 
                            element.name=element.name,
                            value=value,
                            check.consistency=check.consistency)
            
            
            #-- Done --#
            invisible(self)
        },
        
        set.element.functional.form.main.effect.alphas = function(element.name,
                                                                  alpha.name,
                                                                  values,
                                                                  applies.to.dimension.values=names(values),
                                                                  dimensions=names(applies.to.dimension.values),
                                                                  check.consistency = !self$has.been.crunched())
        {
            #-- Check Arguments --#
            if (check.consistency)
            {
                #-- Check valid element with a model --#
                if (!is.character(element.name) || length(element.name)!=1 || is.na(element.name))
                    stop("Cannot set functional.form alphas: 'element.name' must be a single, non-NA, character value")
                
                if (all(private$i.element.names!=element.name))
                    stop(paste0("Cannot set functional.form alphas: No element named '", element.name, "' exists for model specificaiton '", self$version, "'"))

                functional.form = private$i.element.backgrounds[[element.name]]$functional.form
                
                if (is.null(functional.form))
                    stop(paste0("Cannot set functional.form alphas: element '", element.name, 
                                "' does not have a functional.form (use 'set.element.value' to change its value)"))
                
                error.prefix = paste0("Cannot set functional.form alphas for element '", element.name, "': ")
                if (all(functional.form$alpha.names != alpha.name))
                    stop(paste0(error.prefix,
                                "'", alpha.name, 
                                "' is not the name of a valid alpha for this functional.form of type '",
                                functional.form$type, "'"))
                
                if (!is.numeric(values) || length(values)==0 || any(is.na(values)))
                    stop(paste0(error.prefix, "'values' must be a non-empty, non-NA numeric vector"))
            }
            
            error.prefix = paste0("Cannot set functional.form (main-effect) alphas for '", alpha.name, "' for element '", element.name, "': ")
            
            #-- Clear Dependencies --#
            
            # Clear all values
            clear.dependent.values(element.name)
            
            # Clear dim.names
            clear.dim.names(element.name)
            
            # No need to clear times
            
            #-- Set it --#
            private$i.element.backgrounds[[element.name]]$functional.form.alphas[[alpha.name]] = 
                set.alpha.main.effect.values(private$i.element.backgrounds[[element.name]]$functional.form.alphas[[alpha.name]],
                                             dimensions = dimensions,
                                             dimension.values = applies.to.dimension.values,
                                             values = values,
                                             check.consistency = check.consistency,
                                             error.prefix = error.prefix)
            
            #-- Log Instruction --#
            log.instruction(fn.name='set.element.functional.form.main.effect.alphas', 
                            element.name=element.name,
                            alpha.name=alpha.name,
                            values=values,
                            applies.to.dimension.values=applies.to.dimension.values,
                            dimensions=dimensions,
                            check.consistency=check.consistency)
            
            #-- Done --#
            invisible(self)
        },
        
        set.element.functional.form.interaction.alphas = function(element.name,
                                                                   alpha.name,
                                                                   value,
                                                                   applies.to.dimension.values=names(values),
                                                                   dimensions=names(applies.to.dimension.values),
                                                                   check.consistency = !self$has.been.crunched())
        {
            #-- Check Arguments --#
            if (check.consistency)
            {
                error.prefix = paste0("Cannot set functional.form alphas for element '", element.name, "': ")
                
                #-- Check valid element with a model --#
                if (!is.character(element.name) || length(element.name)!=1 || is.na(element.name))
                    stop("Cannot set functional.form alphas: 'element.name' must be a single, non-NA, character value")
                
                if (all(private$i.element.names!=element.name))
                    stop(paste0("Cannot set functional.form alphas: No element named '", element.name, "' exists for model specificaiton '", self$version, "'"))
                
                functional.form = private$i.element.backgrounds[[element.name]]$functional.form
                
                if (is.null(functional.form))
                    stop(paste0("Cannot set functional.form alphas: element '", element.name, 
                                "' does not have a functional.form (use 'set.element.value' to change its value)"))
                
                error.prefix = paste0("Cannot set functional.form alphas for element '", element.name, "': ")
                if (all(functional.form$alpha.names != alpha.name))
                    stop(paste0(error.prefix,
                                "'", alpha.name, 
                                "' is not the name of a valid alpha for this functional.form of type '",
                                functional.form$type, "'"))
                
                if (!is.numeric(value) || length(value)!=1 || is.na(value))
                    stop(paste0(error.prefix, "'value' must be a single, non-NA numeric value"))
                
            }
            
            error.prefix = paste0("Cannot set functional.form (interaction) alphas for '", alpha.name, "' for element '", element.name, "': ")

                        
            #-- Clear Dependencies --#
            
            # Clear all values
            clear.dependent.values(element.name)
            
            # Clear dim.names
            clear.dim.names(element.name)
            
            # No need to clear times

            #-- Set It --#
            
            private$i.element.backgrounds[[element.name]]$functional.form.alphas[[alpha.name]] = 
                set.alpha.interaction.value(private$i.element.backgrounds[[element.name]]$functional.form.alphas[[alpha.name]],
                                            dimensions = dimensions,
                                            dimension.values = applies.to.dimension.values,
                                            value = value,
                                            check.consistency = check.consistency,
                                            error.prefix = error.prefix)
                    
                        
            #-- Log Instruction --#
            log.instruction(fn.name='set.element.functional.form.interaction.alphas', 
                            element.name=element.name,
                            alpha.name=alpha.name,
                            value=value,
                            applies.to.dimension.values=applies.to.dimension.values,
                            dimensions=dimensions,
                            check.consistency=check.consistency)
            
            #-- Done --#
            invisible(self)
        },
        
        #' Still need to flesh out this interface
        set.quantity.foreground = function(foreground,
                                           check.consistency = !self$has.been.crunched())
        {
            # Check if quantity must be static
            # Check if quantity is intervenable
            
            private$i.quantity.is.static[[element.name]] = F
            private$i.quantity.is.static[[specification$get.dependent.quantity.names(element.name)]] = F
            
            # Clear dim.names
            # Clear all values after start of intervention
            # Clear times
        },
        
        set.element.functional.form.from.time = function(element.name,
                                                         from.time,
                                                         check.consistency = !self$has.been.crunched())
        {
            previous.from.time = i.element.backgrounds[[element.name]]$functional.form.from.time
            
            #-- Check Arguments --#
            if (check.consistency)
            {
                if (!is.character(element.name) || length(element.name)!=1 || is.na(element.name))
                    stop("Cannot set functional.form from-time: 'element.name' must be a single, non-NA, character value")
                
                if (all(names(private$i.element.backgrounds)!=element.name))
                    stop(paste0("Cannot set functional.form from-time: No element named '", element.name, 
                                "' exists in the specification for version '", private$i.version, "'"))
                
                if (is.null(private$i.element.backgrounds[[element.name]]$functional.form))
                    stop(paste0("Cannot set functional.form from-time: element '", element.name,
                                "does not have a functional.form"))
                
                if (is.null(previous.from.time))
                    stop(paste0("Cannot set functional.form from-time for element '", element.name,
                                "': the functional.form is static with no ramp or taper"))
                
                # Check from time
                if (!is.numeric(from.time) || length(from.time) != 1 || is.na(from.time))
                    stop(paste0("Cannot set functional.form from-time for element '", element.name,
                                "': from.time must be a single, non-NA, numeric value"))
                
                current.year = as.numeric(format(Sys.Date(), "%Y"))
                if (from.time < MIN.FUNCTIONAL.FORM.FROM.YEAR || from.time > (current.year+MAX.FUNCTIONAL.FORM.FROM.YEAR.OFFSET.FROM.CURRENT.YEAR))
                    stop(paste0("Cannot set functional.form from-time for element '", element.name,
                                "'. from.time (",
                                from.time, 
                                ") must be between ", MIN.FUNCTIONAL.FORM.FROM.YEAR,
                                " and ", (current.year+MAX.FUNCTIONAL.FORM.FROM.YEAR.OFFSET.FROM.CURRENT.YEAR)))
                
                if (from.time > private$i.element.backgrounds[[element.name]]$functional.form.to.time)
                    stop(paste0("Cannot set functional.form from-time for element '", element.name,
                                "'. from.time (",
                                from.time, 
                                ") must be a less than or equal to the previously specified 'to.time' (",
                                private$i.element.backgrounds[[element.name]]$functional.form.to.time, ")"))
                
                if (!is.null(private$ielement.backgrounds[[element.name]]$ramp.times) &&
                    any(from.time <= private$i.element.backgrounds[[element.name]]$ramp.times))
                    stop(paste0("Cannot set functional.form from-time for element '", element.name,
                                "'.  from.time (",
                                from.time, 
                                ") must be a less than or equal to the previously set ramp.times (",
                                private$i.element.backgrounds[[element.name]]$ramp.times[length(private$i.element.backgrounds[[element.name]]$ramp.times)],
                                ")"))
            }
            
            
            #-- Clear Dependencies --#
            
            # Clear values for all times prior to max(old from time, new from time)
            times.to.clear.values.for.mask = i.quantity.value.times[[element.name]] < max(i.element.backgrounds[[element.name]]$functional.form.from.time,
                                                                                          previous.from.time)
            clear.dependent.values(element.name, times = i.quantity.value.times[[element.name]][times.to.clear.values.for.mask])
            
            # Clear times
            clear.dependent.times(element.name)
            
            # No need to clear dim.names
            
            
            #-- Set It --#
            private$i.element.backgrounds[[element.name]]$functional.form.from.time = from.time
                        
            #-- Log Instruction --#
            log.instruction(fn.name='set.element.functional.form.from.time', 
                            element.name=element.name,
                            from.time=from.time,
                            check.consistency=check.consistency)
            
            #-- Done --#
            invisible(self)
        },
        
        set.element.functional.form.to.time = function(element.name,
                                                        to.time,
                                                        check.consistency = !self$has.been.crunched())
        {
            previous.from.time = i.element.backgrounds[[element.name]]$functional.form.to.time
            
            #-- Check Arguments --#
            if (check.consistency)
            {
                if (!is.character(element.name) || length(element.name)!=1 || is.na(element.name))
                    stop("Cannot set functional.form to-time: 'element.name' must be a single, non-NA, character value")
                
                if (all(names(private$i.element.backgrounds)!=element.name))
                    stop(paste0("Cannot set functional.form to-time: No element named '", element.name, 
                                "' exists in the specification for version '", private$i.version, "'"))
                
                if (is.null(private$i.element.backgrounds[[element.name]]$functional.form))
                    stop(paste0("Cannot set functional.form to-time: element '", element.name,
                                "' has no functional.form"))
                
                if (is.null(private$i.element.backgrounds[[element.name]]$functional.form.to.time))
                    stop(paste0("Cannot set functional.form from-time for element '", element.name,
                                "': the functional.form is static with no ramp or taper"))
                
                # Check to time
                if (!is.numeric(to.time) || length(to.time) != 1 || is.na(to.time))
                    stop(paste0("Cannot set functional.form to-time for element '", element.name,
                                "': 'to.time' must be a single, non-NA, numeric value"))
                
                if (to.time < private$i.element.backgrounds[[element.name]]$functional.form.from.time)
                    stop(paste0("Cannot set functional.form to-time for element '", element.name,
                                "'. to.time (",
                                to.time, 
                                ") must be a less than or equal to the previously specified 'from.time' (",
                                private$i.element.backgrounds[[element.name]]$functional.form.from.time, ")"))
                
                
                if (!is.null(private$i.element.backgrounds[[element.name]]$taper.times) &&
                    any(to.time >= private$i.element.backgrounds[[element.name]]$taper.times))
                    stop(paste0("Cannot set functional.form to-time for element '", element.name,
                                "'.  'to.time' (",
                                to.time, 
                                ") must be a less than or equal to the previously set taper.times (",
                                private$i.element.backgrounds[[element.name]]$taper.times[1], ")"))
                
            }
            
            
            #-- Clear Dependencies --#
            
            # Clear values for all times after min(old to time, new to time)
            times.to.clear.values.for.mask = private$i.quantity.value.times[[element.name]] > min(i.element.backgrounds[[element.name]]$functional.form.to.time,
                                                                                          previous.to.time)
            clear.dependent.values(element.name, times = private$i.quantity.value.times[[element.name]][times.to.clear.values.for.mask])
            
            # Clear times
            clear.dependent.times(element.name)
            
            # No need to clear dim.names
            
            
            #-- Set It --#
            private$i.element.backgrounds[[element.name]]$functional.form.to.time = to.time
            
            
            #-- Log Instruction --#
            log.instruction(fn.name='set.element.functional.form.to.time', 
                            element.name=element.name,
                            to.time=to.time,
                            check.consistency=check.consistency)
            
            #-- Done --#
            invisible(self)
        },
        
        set.element.ramp.times = function(element.name,
                                           times,
                                           indices=1:length(times),
                                           check.consistency = !self$has.been.crunched())
        {
            #-- Check Arguments --#
            
            if (check.consistency)
            {
                if (!is.character(element.name) || length(element.name)!=1 || is.na(element.name))
                    stop("Cannot set ramp.times: 'element.name' must be a single, non-NA, character value")
                
                if (all(names(private$i.element.backgrounds)!=element.name))
                    stop(paste0("Cannot set ramp.times: No element named '", element.name, "' exists in the '", self$version, "' model specification"))
                
                if (is.null(private$i.element.backgrounds[[element.name]]$functional.form))
                    stop(paste0("Cannot set ramp.times: element '", element.name,
                                "' is a static value with no functional.form"))
                
                if (is.null(private$i.element.backgrounds[[element.name]]$ramp.values))
                    stop(paste0("Cannot set ramp.times for element '", element.name,
                                "': the functional.form is static with no ramp or taper"))
                
                indices = private$check.ramp.or.taper.values.and.indices(
                    values = times,
                    indices = indices,
                    current.values = private$i.element.backgrounds[[element.name]]$ramp.times,
                    is.ramp = T,
                    is.times = T,
                    error.prefix = paste0("Cannot set ramp.times for element '", element.name, "': "))
            }
            
            #-- Check ordering of times --#
            new.times = private$i.element.backgrounds[[element.name]]$ramp.times
            new.times[indices] = times
            
            if (check.consistency)
            {
                if (!all(new.times == sort(new.times)))
                    stop(paste0("Cannot set ramp.times for element '", element.name,
                                "': The ramp.times are not in ascending order"))
                
                if (new.times[length(new.times)] >= private$i.element.backgrounds[[element.name]]$functional.form.from.time)
                    stop(paste0("Cannot set ramp.times for element '", element.name,
                                "': All ramp.times must be BEFORE the previously set functional.form from-time (",
                                private$i.element.backgrounds[[element.name]]$functional.form.from.time, ")"))
            }
            
            #-- Clear Dependencies --#
            
            # Clear values for all times prior to functional.form.from.time
            times.to.clear.values.for.mask = i.quantity.value.times[[element.name]] < i.element.backgrounds[[element.name]]$functional.form.from.time
            clear.dependent.values(element.name, times = i.quantity.value.times[[element.name]][times.to.clear.values.for.mask])
            
            # Clear times
            clear.dependent.times(element.name)
            
            # No need to clear dim.names
            
            
            #-- Set the Value --#
            private$i.element.backgrounds[[element.name]]$ramp.times = new.times
            
            
            #-- Log Instruction --#
            log.instruction(fn.name='set.element.ramp.times', 
                            times=times,
                            indices=indices,
                            check.consistency=check.consistency)
            
            #-- Done --#
            invisible(self)
        },
        
        set.element.ramp.values = function(element.name,
                                            values,
                                            indices=1:length(values),
                                            check.consistency = !self$has.been.crunched())
        {
            #-- Check Arguments --#
            if (check.consistency)
            {
                if (!is.character(element.name) || length(element.name)!=1 || is.na(element.name))
                    stop("Cannot set ramp.values: 'element.name' must be a single, non-NA, character value")
                
                if (all(names(private$i.element.backgrounds)!=element.name))
                    stop(paste0("Cannot set ramp.values: No element named '", element.name, "' exists in the '", self$version, "' model specification"))
                
                if (is.null(private$i.element.backgrounds[[element.name]]$functional.form))
                    stop(paste0("Cannot set ramp.values for element '", element.name,
                                "' is a static value with no functional.form"))
                
                if (is.null(private$i.element.backgrounds[[element.name]]$ramp.values))
                    stop(paste0("Cannot set ramp.values for element '", element.name,
                                "': the functional.form is static with no ramp or taper"))
                
                
                indices = private$check.ramp.or.taper.values.and.indices(
                    values = values,
                    indices = indices,
                    current.values = private$i.element.backgrounds[[element.name]]$ramp.times,
                    is.ramp = T,
                    is.times = F,
                    error.prefix=paste0("Cannot set ramp.values for element '",
                                        element.name, "': "))
            }
            
            
            #-- Clear Dependencies --#
            
            # Clear values for all times prior to functional.form.from.time
            times.to.clear.values.for.mask = i.quantity.value.times[[element.name]] < i.element.backgrounds[[element.name]]$functional.form.from.time
            clear.dependent.values(element.name, times = i.quantity.value.times[[element.name]][times.to.clear.values.for.mask])
            
            # No need to clear times
            # No need to clear dim.names
            
            
            #-- Set the Values --#
            private$i.element.backgrounds[[element.name]]$ramp.values[indices] = values
            
            
            #-- Log Instruction --#
            log.instruction(fn.name='set.element.ramp.values', 
                            values=values,
                            indices=indices,
                            check.consistency=check.consistency)
            
            #-- Done --#
            invisible(self)
        },
        
        set.element.taper.times = function(element.name,
                                            times,
                                            indices=1:length(times),
                                            check.consistency = !self$has.been.crunched())
        {
            #-- Check Arguments --#
            
            if (check.arguments)
            {
                if (!is.character(element.name) || length(element.name)!=1 || is.na(element.name))
                    stop("Cannot set taper.times: 'element.name' must be a single, non-NA, character value")
                
                if (all(names(private$i.element.backgrounds)!=element.name))
                    stop(paste0("Cannot set taper.times: No element named '", element.name, "' exists in the '", self$version, "' model specification"))
                
                if (is.null(private$i.element.backgrounds[[element.name]]$functional.form))
                    stop(paste0("Cannot set taper.times for element '", element.name,
                                "' is a static value with no functional.form"))
                
                if (is.null(private$i.element.backgrounds[[element.name]]$taper.values))
                    stop(paste0("Cannot set taper.times for element '", element.name,
                                "': the functional.form is static with no ramp or taper"))
                
                
                indices = private$check.ramp.or.taper.values.and.indices(
                    values = times,
                    indices = indices,
                    current.values = private$i.element.backgrounds[[element.name]]$taper.times,
                    is.ramp = F,
                    is.times = T,
                    error.prefix = paste0("Cannot set taper.times for element '", element.name, "': "))
            }
            
            #-- Check ordering of times --#
            new.times = private$i.element.backgrounds[[element.name]]$taper.times
            new.times[indices] = times
            
            if (check.consistency)
            {
                if (!all(new.times == sort(new.times)))
                    stop(paste0("Cannot set taper.times for element '", element.name,
                                "': The taper.times are not in ascending order"))
                
                if (new.times[1] <= private$i.element.backgrounds[[element.name]]$functional.form.to.time)
                    stop(paste0("Cannot set taper.times for element '", element.name,
                                "': All taper.times must be AFTER the previously set functional.form to-time (",
                                private$i.element.backgrounds[[element.name]]$functional.form.to.time, ")"))
            }
            
            
            #-- Clear Dependencies --#
            
            # Clear values for all times after functional.form.to.time
            times.to.clear.values.for.mask = i.quantity.value.times[[element.name]] > i.element.backgrounds[[element.name]]$functional.form.to.time
            clear.dependent.values(element.name, times = i.quantity.value.times[[element.name]][times.to.clear.values.for.mask])
            
            # Clear times
            clear.dependent.times(element.name)
            
            # No need to clear dim.names
            
            
            #-- Set the Value --#
            private$i.element.backgrounds[[element.name]]$taper.times = new.times
            
            
            #-- Log Instruction --#
            log.instruction(fn.name='set.element.taper.times', 
                            times=times,
                            indices=indices,
                            check.consistency=check.consistency)
            
            
            #-- Done --#
            invisible(self)
        },
        
        set.element.taper.values = function(element.name,
                                             values,
                                             indices=1:length(values),
                                             check.consistency = !self$has.been.crunched())
        {
            #-- Check Arguments --#
            if (check.consistency)
            {
                if (!is.character(element.name) || length(element.name)!=1 || is.na(element.name))
                    stop("Cannot set taper.values: 'element.name' must be a single, non-NA, character value")
                
                if (all(names(private$i.element.backgrounds)!=element.name))
                    stop(paste0("Cannot set taper.values: No element named '", element.name, "' exists in the '", self$version, "' model specification"))
                
                if (is.null(private$i.element.backgrounds[[element.name]]$functional.form))
                    stop(paste0("Cannot set taper.values for element '", element.name,
                                "' is a static value with no functional.form"))
                
                if (is.null(private$i.element.backgrounds[[element.name]]$taper.values))
                    stop(paste0("Cannot set taper.values for element '", element.name,
                                "': the functional.form is static with no ramp or taper"))
                
                
                indices = check.ramp.or.taper.values.and.indices(values=times,
                                                                 indices=indices,
                                                                 current.values=private$i.element.backgrounds[[element.name]]$taper.times,
                                                                 is.ramp=T,
                                                                 is.times=F,
                                                                 error.prefix=paste0("Cannot set taper.values for element '",
                                                                                     element.name, "':"))
            }
            
            
            #-- Clear Dependencies --#
            
            # Clear values for all times after functional.form.to.time
            times.to.clear.values.for.mask = i.quantity.value.times[[element.name]] > i.element.backgrounds[[element.name]]$functional.form.to.time
            clear.dependent.values(element.name, times = i.quantity.value.times[[element.name]][times.to.clear.values.for.mask])
            
            # No need to clear times
            # No need to clear dim.names
            
            
            #-- Set the Value --#
            private$i.element.backgrounds[[element.name]]$taper.values[indices] = values
            
            
            #-- Log Instruction --#
            log.instruction(fn.name='set.element.taper.values', 
                            values=values,
                            indices=indices,
                            check.consistency=check.consistency)
            
            #-- Done --#
            invisible(self)
        },
        
        set.element.functional.form.future.slope = function(element.names,
                                                            slope,
                                                            check.consistency = !self$has.been.crunched())
        {
            #-- Check Arguments --#
            if (check.consistency)
            {
                if (!is.numeric(slope) || length(slope)!=1 || is.na(slope))
                    stop("Cannot set functional.form future-slope: 'slope' must be a single, non-NA, numeric value")
                
                if (!is.character(element.names) || length(element.names)==0 || is.na(element.names))
                    stop("Cannot set functional.form future-slope: 'element.names' must be a non-empty, non-NA, character vector")
                
                invalid.names = setdiff(element.names, names(private$i.element.backgrounds))
                if (length(invalid.names)>0)
                    stop(paste0("Cannot set functional.form future-slope: No ",
                                ifelse(length(invalid.names)==1, "element", "elements"),
                                " named '",
                                collapse.with.and("'", invalid.names, "'"),
                                ifelse(length(invalid.names)==1, " exists", " exist"),
                                " in the '", self$version, "' model specification"))
                
                no.functional.form.mask = sapply(private$i.element.backgrounds[element.names], function(bkgd){
                    is.null(bkgd$functional.form)
                })
                if (any(no.functional.form.mask))
                    stop(paste0("Cannot set functional.form future-slope for ",
                                ifelse(sum(no.functional.form.mask)==1, "element ", "elements "),
                                collapse.with.or("'", element.names[no.model.mask], "'"),
                                ": ",
                                ifelse(sum(no.functional.form.mask)==1, 
                                       "This element is a static value",
                                       "These elements are static values"),
                                " with no functional.form"))

                static.mask = sapply(private$i.element.backgrounds[element.names], function(bkgd){
                    bkgd$functional.form$is.static
                })
                if (any(static.mask))
                    stop(paste0("Cannot set functioanl.form future-slope for ",
                                ifelse(sum(static.mask)==1, "element ", "elements "),
                                collapse.with.or("'", element.names[static.mask], "'"),
                                ": ",
                                ifelse(sum(static.mask)==1, "this element has", "these elements have"),
                                " a static functional form with no ramp or taper"))
            }
            
            #-- Clear Dependencies --#
            
            # Clear values for all times after functional.form.future.slope.after.time
            times.to.clear.values.for.mask = i.quantity.value.times[[element.name]] > i.element.backgrounds[[element.name]]$future.slope.after.time
            clear.dependent.values(element.name, times = i.quantity.value.times[[element.name]][times.to.clear.values.for.mask])
            
            # No need to clear times
            # No need to clear dim.names
            
            
            #-- Set the Value --#
            for (element.name in element.names)
                private$i.element.backgrounds[[element.name]][['future.slope']] = slope
            
            
            #-- Log Instruction --#
            log.instruction(fn.name='set.element.functional.form.future.slope', 
                            element.names=element.names,
                            slope=slope,
                            check.consistency=check.consistency)
            
            #-- Done --#
            invisible(self)
        },
        
        set.element.functional.form.future.slope.after.time = function(element.names,
                                                                       after.time,
                                                                       check.consistency = !self$has.been.crunched())
        {
            previous.future.slope.after.time = i.element.backgrounds[[element.name]]$future.slope.after.time
            
            #-- Check Arguments --#
            if (check.arguments)
            {
                if (!is.numeric(after.year) || length(after.year)!=1 || is.na(after.year))
                    stop("Cannot set functional.form future-slope-after-year: 'after.year' must be a single, non-NA, numeric value")
                
                if (!is.character(element.names) || length(element.names)==0 || is.na(element.names))
                    stop("Cannot set functional.form future-slope-after-year: 'element.names' must be a non-empty, non-NA, character vector")
                
                invalid.names = setdiff(element.names, names(private$i.element.backgrounds))
                if (length(invalid.names)>0)
                    stop(paste0("Cannot set functional.form future-slope-after-year: No ",
                                ifelse(length(invalid.names)==1, "element", "elements"),
                                " named '",
                                collapse.with.and("'", invalid.names, "'"),
                                ifelse(length(invalid.names)==1, " exists", " exist"),
                                " in the '", self$version, "' model specification"))
                
                no.functional.form.mask = sapply(private$i.element.backgrounds[element.names], function(bkgd){
                    is.null(bkgd$functional.form)
                })
                if (any(no.functional.form.mask))
                    stop(paste0("Cannot set functional.form future-slope-after-year for ",
                                ifelse(sum(no.functional.form.mask)==1, "element ", "elements "),
                                collapse.with.or("'", element.names[no.model.mask], "'"),
                                ": ",
                                ifelse(sum(no.functional.form.mask)==1, 
                                       "This element is a static value",
                                       "These elements are static values"),
                                " with no functional.form"))
                
                static.mask = sapply(private$i.element.backgrounds[element.names], function(bkgd){
                    bkgd$functional.form$is.static
                })
                if (any(static.mask))
                    stop(paste0("Cannot set functioanl.form future-slope-after-year for ",
                                ifelse(sum(static.mask)==1, "element ", "elements "),
                                collapse.with.or("'", element.names[static.mask], "'"),
                                ": ",
                                ifelse(sum(static.mask)==1, "this element has", "these elements have"),
                                " a static functional form with no ramp or taper"))
            }
 
            
            #-- Clear Dependencies --#
            
            # Clear values for all times after min(old, new functional.form.future.slope.after.time)
            times.to.clear.values.for.mask = i.quantity.value.times[[element.name]] > min(i.element.backgrounds[[element.name]]$future.slope.after.time,
                                                                                          previous.future.slope.after.time)
            clear.dependent.values(element.name, times = i.quantity.value.times[[element.name]][times.to.clear.values.for.mask])
            
            # No need to clear times
            # No need to clear dim.names
            
            #-- Set the Value --#
            for (element.name in element.names)
                private$i.element.backgrounds[[element.name]]$future.slope.after.year = after.year
            
            
            #-- Log Instruction --#
            log.instruction(fn.name='set.element.functional.form.future.slope.after.time', 
                            element.names=element.names,
                            after.time=after.time,
                            check.consistency=check.consistency)
            
            #-- Done --#
            invisible(self)
        },
        
        has.been.crunched = function()
        {
            private$i.has.been.crunched
        },
        
        test.crunch = function()
        {
            private$do.test.crunch()
            private$jheem
        },
        
        # Does NOT actually do anything with the intervention
        # Just records it to the engine
        set.intervention = function(intervention)
        {
            if (!is.null(private$i.intervention) || !is.null(private$i.intervention.code))
                stop(paste0("Cannot set an intervention for JHEEM engine: an intervention has already been set for this engine"))
                     
            if (!is(intervention, "jheem.intervention"))
                stop("Cannot set intervention for JHEEM engine: 'intervention' must be an object of class 'jheem.intervention'")
            
            if (is.null(intervention$code))
                private$i.intervention = intervention
            else
                private$i.intervention.code = intervention$code
        }
    ),
    
    active = list(
        
        element.names = function(value)
        {
            if (missing(value))
                names(private$i.element.backgrounds)
            else
                stop("Cannot modify a JHEEM Engine's 'element.names' - they are read-only")
        },
        
        # FOR DEBUGGING
        calculated.values = function(value)
        {
            if (missing(value))
                private$i.quantity.values
            else
                stop("Cannot modify a JHEEM Engine's 'calculated.values' - they are read-only")
        },
        
        value.times = function(value)
        {
            if (missing(value))
                private$i.quantity.value.times
            else
                stop("Cannot modify a JHEEM Engine's 'value.times' - they are read-only")
        },
        
        self.times = function(value)
        {
            if (missing(value))
                private$i.quantity.self.times
            else
                stop("Cannot modify a JHEEM Engine's 'self.times' - they are read-only")
        },
        
        element.backgrounds = function(value)
        {
            if (missing(value))
                private$i.element.backgrounds
            else
                stop("Cannot modify a JHEEM Engine's 'element.backgrounds' - they are read-only")
        }
        
    ),
    
    private = list(
        
        ##----------------------##
        ##----------------------##
        ##-- MEMBER VARIABLES --##
        ##----------------------##
        ##----------------------##
        
        i.element.names = NULL,
        i.element.backgrounds = NULL,
        i.instructions = NULL,
        
        i.quantity.max.dim.names = NULL,
        i.quantity.required.dim.names = NULL,
        i.quantity.component.max.dim.names = NULL,
        i.quantity.component.applies.to = NULL,
        
        i.quantity.self.times = NULL,
        i.quantity.value.times = NULL,
        i.quantity.values = NULL,
        
        i.quantity.dim.names = NULL,
        i.crunched.quantity.dim.names = NULL,
        i.quantity.component.dim.names = NULL,
        i.crunched.quantity.component.dim.names = NULL,
        
        i.quantity.component.depends.on.mapping.indices = NULL,
        i.quantity.mapping.indices = NULL,

        i.quantity.is.static = NULL,
        
        i.run.from.time = NULL,
        i.run.to.time = NULL,
        
        i.has.been.crunched = NULL,
        
        i.diffeq.settings = NULL,
        
        i.intervention = NULL,
        i.intervention.code = NULL,
        
        ##---------------------##
        ##---------------------##
        ##-- PRIVATE METHODS --##
        ##---------------------##
        ##---------------------##
        
        get.current.code.iteration = function()
        {
            JHEEM.ENGINE.CODE.ITERATION
        },
        
        ##------------##
        ##-- SET UP --##
        ##------------##
        
        set.up = function()
        {
            specification = get.compiled.specification.for.version(private$i.version)
            
            # Finalize max.dim.names and applies.to for quantities and components
            private$i.quantity.max.dim.names = lapply(specification$quantities, function(quantity){
                self$specification.metadata$apply.aliases(quantity$max.dim.names,
                                                           error.prefix=paste0("Error finalizing max.dim.names for model quantity ", 
                                                                               quantity$get.original.name(specification$version)))
            })
            
            private$i.quantity.required.dim.names = lapply(specification$quantities, function(quantity){
                self$specification.metadata$apply.aliases(quantity$required.dim.names,
                                                           error.prefix=paste0("Error finalizing required.dim.names for model quantity ", 
                                                                               quantity$get.original.name(specification$version)))
            })
            
            private$i.quantity.component.max.dim.names = lapply(specification$quantities, function(quantity){
                lapply(quantity$components, function(comp){
                    self$specification.metadata$apply.aliases(comp$max.dim.names,
                                                               error.prefix=paste0("Error finalizing max.dim.names for the ",
                                                                                   get.ordinal(i-1), " subset of model quantity ", 
                                                                                   quantity$get.original.name(specification$version)))
                    # ^ Should never trigger an error on the first component since it is the same as the quantity dim.names calculated above
                })
            })
            
            private$i.quantity.component.applies.to = lapply(specification$quantities, function(quantity){
                lapply(quantity$components, function(comp){
                    self$specification.metadata$apply.aliases(comp$applies.to,
                                                               error.prefix=paste0("Error finalizing applies.to for the ",
                                                                                   get.ordinal(i-1), " subset of model quantity ", 
                                                                                   quantity$get.original.name(specification$version)))
                    # ^ Should never trigger an error on the first component since applies.to is NULL for the first component
                })
            })
            
            # Set up the element backgrounds
            private$i.element.backgrounds = lapply(specification$element.names, function(elem.name){
                elem = specification$get.quantity(elem.name)
                bkgd = elem$get.element.background(specification.metadata = self$specification.metadata,
                                                   error.prefix = paste0("Error creating JHEEM Engine for version '", private$i.version, "' and location '", private$i.location, "': "))
                
                if (!is.null(bkgd$functional.form))
                {
                    bkgd$functional.form.alphas = lapply(bkgd$functional.form$alpha.names, 
                                                         create.functional.form.alphas,
                                                         functional.form = bkgd$functional.form,
                                                         maximum.dim.names = private$i.quantity.max.dim.names[[elem.name]],
                                                         check.consistency = T,
                                                         error.prefix = paste0("Error creating alphas object for model element '", elem.name, "': "))
                    names(bkgd$functional.form.alphas) = bkgd$functional.form$alpha.names
                }
                
                bkgd
            })
            names(private$i.element.backgrounds) = private$i.element.names = specification$element.names
            
            # Figure out if quantities are static
            private$i.quantity.is.static = rep(F, length(specification$quantity.names))
            names(private$i.quantity.is.static) = specification$quantity.names
            
            private$i.quantity.is.static[specification$element.names] = sapply(private$i.element.backgrounds[specification$element.names], function(bkgd){
                bkgd$is.static
            })
            
            non.element.quantity.names = setdiff(specification$quantity.names, specification$element.names)
            private$i.quantity.is.static[non.element.quantity.names] = sapply(non.element.quantity.names, function(quantity.name){
                all(private$i.quantity.is.static[specification$get.dependee.element.names(quantity.name)])
            })
            
            # Set up dim.names list holders
            private$i.quantity.dim.names = list()
            private$i.crunched.quantity.dim.names = list()
            private$i.quantity.component.dim.names = list()
            private$i.crunched.quantity.component.dim.names = list()
            
            # Set up values and times lists
            private$i.quantity.values = list()
            private$i.quantity.self.times = list()
            private$i.quantity.value.times = list()
            
            # Set up the diffeq settings
            private$i.diffeq.settings = create.diffeq.settings(engine = self,
                                                               error.prefix = paste0("Error creating diffeq settings for JHEEM Engine for version '", private$i.version, "' and location '", private$i.location, "': "))
            
            # Clear the i.has.been.crunched flag
            private$i.has.been.crunched = F
            
            # Re-process any instructions
            for (instr in private$i.instructions)
                private$execute.instruction(instr)
            
            # Going to need to do something about foregrounds here
        },
        
        
        ##------------------------##
        ##-- START and END YEAR --##
        ##------------------------##
        
        set.run.years = function(start.year,
                                 end.year,
                                 error.prefix)
        {
            #-- Error Checks --#
            if (!is.numeric(start.year))
                stop(paste0(error.prefix, "'start.year' must be a single NUMERIC value"))
            if (length(start.year)!=1)
                stop(paste0(error.prefix, "'start.year' must be a SINGLE numeric value"))
            if (is.na(start.year))
                stop(paste0(error.prefix, "'start.year' cannot be NA"))
            if (is.infinite(start.year))
                stop(paste0(error.prefix, "'start.year' cannot be infinite"))
            
            if (!is.numeric(end.year))
                stop(paste0(error.prefix, "'end.year' must be a single NUMERIC value"))
            if (length(end.year)!=1)
                stop(paste0(error.prefix, "'end.year' must be a SINGLE numeric value"))
            if (is.na(end.year))
                stop(paste0(error.prefix, "'end.year' cannot be NA"))
            if (is.infinite(end.year))
                stop(paste0(error.prefix, "'end.year' cannot be infinite"))
            
            if (end.year <= start.year)
                stop(paste0(error.prefix, "'end.year' (", end.year, ") must be greater than 'start.year' (", start.year, ")"))
            
            #-- Set it --#
            if (is.null(private$i.run.from.time) || is.null(private$i.run.to.time) ||
                private$i.run.from.time != start.year || private$i.run.to.time != end.year)
            {
                private$i.run.from.time = start.year
                private$i.run.to.time = end.year
            
                #-- Clear times (for all elements) --#
                clear.dependent.times(names(i.element.backgrounds))
            }
            
            #-- Done --#
            invisible(self)
        },
        
        
        ##----------------------------------------##
        ##-- CALCULATING QUANTITY/ELEMENT TIMES --##
        ##----------------------------------------##
        
        # Broadly speaking, "self" times are the times for which a quantity needs to produce values
        #   to capture it's time-varying changes
        # For an element, self times are:
        # - NULL if this is a static value
        # - If not static, the union of:  ramp and taper times, functional form times (within the bounds of the engine run time)
        # For a non-element quantity, self times are:
        # - NULL if all the quantities components have value.type=='numeric'
        # - The union of self times for all dependee elements

    
        calculate.quantity.self.times = function(quantity.name)
        {
            if (i.quantity.is.static[quantity.name])
                i.quantity.self.times[[quantity.name]] = 'all'
            else
            {
                specification = get.specification()
                quantity = specification$get.quantity(quantity.name)
                
                if (quantity$is.element)
                {
                    if (is.null(private$i.element.backgrounds[[quantity.name]]$ramp.interpolated.times))
                        private$i.element.backgrounds[[quantity.name]] = calculate.ramp.interpolated.times(private$i.element.backgrounds[[quantity.name]])
                    if (is.null(private$i.element.backgrounds[[quantity.name]]$functional.form.times))
                        private$i.element.backgrounds[[quantity.name]] = calculate.functional.form.times(private$i.element.backgrounds[[quantity.name]])
                    if (is.null(private$i.element.backgrounds[[quantity.name]]$taper.interpolated.times))
                        private$i.element.backgrounds[[quantity.name]] = calculate.taper.interpolated.times(private$i.element.backgrounds[[quantity.name]])
                    
                    bkgd = private$i.element.backgrounds[[quantity.name]]
                    private$i.quantity.self.times[[quantity.name]] = c(bkgd$ramp.interpolated.times,
                                                                       bkgd$functional.form.times,
                                                                       bkgd$taper.interpolated.times)
                }
                else
                {
                    # Figure out which elements go into this quantity and contribute to the time
                    dependee.element.names = specification$get.dependee.element.names(quantity.name)
                    dynamic.dependee.element.names = dependee.element.names[!i.quantity.is.static[dependee.element.names]]
                    
                    # Make sure there are self times for each dependee element
                    null.dependee.element.names = dynamic.dependee.element.names[as.logical(sapply(i.quantity.self.times[dynamic.dependee.element.names], is.null))]
                    sapply(null.dependee.element.names, calculate.quantity.self.times)
                    
                    # The self times is the union of all the dependee self times
                    private$i.quantity.self.times[[quantity.name]] = union_sorted_vectors(i.quantity.self.times[dynamic.dependee.element.names])
                }
            }
            
            
            # A debugging check
            if (length(i.quantity.self.times[[quantity.name]])==0)
                browser()
            
            # Done
            invisible(self)
        },
        
        calculate.functional.form.times = function(bkgd)
        { 
            # The functional.form times are the union of the from.time, the to.time, and all the integers in between
            from.time = max(bkgd$functional.form.from.time, i.run.from.time)
            if (from.time==ceiling(from.time))
                append.to.front = numeric()
            else
                append.to.front = from.time
            
            to.time = min(bkgd$functional.form.to.time, i.run.to.time)
            if (to.time==floor(to.time))
                append.to.back = numeric()
            else
                append.to.back = bkgd$functional.form.from.time
            
            if (ceiling(to.time) >= floor(from.time))
            {
                element = private$get.specification()$get.quantity(bkgd$name)
                if (bkgd$functional.form$type == 'linear spline' && bkgd$functional.form$link$type=='identity' &&
                    element$functional.form.scale == element$scale) 
                            #If these conditions are true, we can just let the native interpolation take care of the in-between points
                    functional.form.times = bkgd$functional.form$knot.times[bkgd$functional.form$knot.times>=from.time & bkgd$functional.form$knot.times<=to.time]
                else
                    functional.form.times = ceiling(from.time):floor(to.time)
            }
            else
                functional.form.times = numeric()
            
            bkgd$functional.form.times = c(append.to.front, functional.form.times, append.to.back)
            
            # A debugging check
            if (length(bkgd$functional.form.times)==0)
                browser()
            
            # Done - return the updated background
            bkgd
        },
        
        calculate.ramp.interpolated.times = function(bkgd)
        {
            if (is.null(bkgd$ramp.times))
                bkgd$ramp.interpolated.times = NULL
            else
            {
                element = get.specification()$get.quantity(bkgd$name)
                bkgd$ramp.interpolated.times = element$calculate.ramp.interpolated.times(ramp.times = bkgd$ramp.times,
                                                                                        functional.form.from.time = bkgd$functional.form.from.time)
                bkgd$ramp.interpolated.times = bkgd$ramp.interpolated.times[bkgd$ramp.interpolated.times >= i.run.from.time &
                                                                              bkgd$ramp.interpolated.times <= i.run.to.time]
            }
            
            bkgd
        },
        
        calculate.taper.interpolated.times = function(bkgd)
        {
            
            if (is.null(bkgd$taper.times))
                bkgd$taper.interpolated.times = NULL
            else
            {
                element = get.specification()$get.quantity(bkgd$name)
                bkgd$taper.interpolated.times = element$calculate.taper.interpolated.times(taper.times = bkgd$taper.times,
                                                                                          functional.form.to.time = bkgd$functional.form.to.time)
                bkgd$taper.interpolated.times = bkgd$taper.interpolated.times[bkgd$taper.interpolated.times >= i.run.from.time &
                                                                                bkgd$taper.interpolated.times <= i.run.to.time]
            }
            
            bkgd
        },
        
        # Broadly speaking, "value" times are the times for which a quantity needs to produce values
        #   EITHER for it's own time-varying changes OR to merge with other quantities in a higher-level
        #   quantity that depends on it and other quantities
        # For a non-element quantity, value times are:
        # - 'all' if all the quantity is static
        # - The union of this quantity's self-times and the self-times of any quantities that depend on it
        #   (functionally, we use only the top-level quantities that depend on it, since that is equivalent but with fewer quantities)
        calculate.quantity.value.times = function(quantity.name)
        {
            if (i.quantity.is.static[quantity.name])
            {
                private$i.quantity.value.times[[quantity.name]] = 'all'
            }
            else
            {
                specification = get.specification()
                quantity = specification$get.quantity(quantity.name)
                
                # Pull the top-level quantities that depend on this quantity
                top.level.dependent.names = specification$get.dependent.top.level.quantity.names(quantity.name)
                dynamic.top.level.dependent.names = top.level.dependent.names[!i.quantity.is.static[top.level.dependent.names]]
                
                # If this is a top-level quantity, we need to include the self times for the quantity itself
                if (any(specification$top.level.quantity.names==quantity.name))
                    dynamic.top.level.dependent.names = c(quantity.name, dynamic.top.level.dependent.names)
                
                # Make sure self times are calculated for each of these
                null.top.level.dependent.names = dynamic.top.level.dependent.names[as.logical(sapply(i.quantity.self.times[dynamic.top.level.dependent.names], is.null))]
                sapply(null.top.level.dependent.names, calculate.quantity.self.times)

                # Union the self times for each dependent top-level quantity
                # (and the self-time for this quantity if it is top-level)
                private$i.quantity.value.times[[quantity.name]] = union_sorted_vectors(i.quantity.self.times[dynamic.top.level.dependent.names])
            }
            
            # Notify diffeq settings
            private$i.diffeq.settings = notify.diffeq.settings.of.quantity.values.change(private$i.diffeq.settings,
                                                                                         quantity.name = quantity.name)
            
            # A debugging check
            if (length(i.quantity.value.times[[quantity.name]])==0)
                 browser()
            
            # Done
            invisible(self)
        },
        
        
        
        ##-----------------------------------------##
        ##-- CALCULATING QUANTITY/ELEMENT VALUES --##
        ##-----------------------------------------##
        
        # A note on when we calculate dim.names for components
        # - If a component has value.type=='expression', we can calculate its dimnames before evaluating it
        #   (and in fact, we have to, in order to apply indexing to its dependee quantities)
        # - If a component has value.type=='character' we can calculate its dimnames before evaluating, but we don't have to
        #   (they are just the dimnames of the single dependee quantity)
        # - If a component has value.type=='numeric', we can calculate its dimnames before evaluating, but don't have to
        #   (they are just the dimnames of the numeric value)
        # - If a component has value.type=='function' we CANNOT calculate its dimnames beforehand
        #   Furthermore, we have to validate after the fact that its dimnames are a subset of the max.dim.names for the component
        calculate.quantity.value = function(quantity.name, check.consistency,
                                            depth=0) #depth is for debugging
        {
            # For debugging
            if (is.element.name(quantity.name))
                print(paste0(paste0(rep(" ", depth), collapse=''),
                             "-Calculate element '", quantity.name, "'"))
            else
                print(paste0(paste0(rep(" ", depth), collapse=''),
                             "-Calculate quantity '", quantity.name, "'"))
            
            if (is.element.name(quantity.name))
                calculate.element.value(quantity.name, check.consistency=check.consistency)
            else
            {
                #-- Set-up Times --#
                if (is.null(private$i.quantity.value.times[[quantity.name]]))
                    calculate.quantity.value.times(quantity.name)
                required.times = as.character(private$i.quantity.value.times[[quantity.name]])
                missing.times = setdiff(required.times, names(private$i.quantity.values[[quantity.name]]))
 
                #-- Fill in missing values --#
                if (length(missing.times)>0)
                {
                    quantity = get.specification()$get.quantity(quantity.name)
                    
                    #-- Make sure the dependee quantities are all calculated --#
                    sapply(quantity$depends.on, calculate.quantity.value, check.consistency=check.consistency, depth=depth+1)
                    
                    #-- Loop through missing times --#
                    private$i.quantity.values[[quantity.name]][missing.times] = lapply(missing.times, function(time){
                        
                        #-- Calculate the values for each component --#
                        component.values = lapply(1:quantity$n.components, function(i){
                            
                            #-- Pull the component and set up the error prefix --#
                            comp = quantity$components[[i]]
                            
                            if (i==1)
                                error.prefix = paste0("Error evaluating value for quantity '", quantity.name, " at time ", time, ": ")
                            else
                                error.prefix = paste0("Error evaluating value for the ", get.ordinal(i-1), " subset of quantity '", quantity.name, " at time ", time, ": ")
                            
                            #-- Calculate dim.names (if we need to and we can) --#
                            if ((length(private$i.quantity.component.dim.names[[quantity.name]]) < i ||
                                 is.null(private$i.quantity.component.dim.names[[quantity.name]][[i]])) &&
                                comp$value.type!='function')
                                calculate.quantity.component.dim.names(quantity, component.index=i)
                   
                            #-- Bind the depends-on quantities --#
                            bindings = lapply(comp$depends.on, function(dep.on){
                                
                                if (i.quantity.is.static[dep.on])
                                    values = private$i.quantity.values[[dep.on]][['all']]
                                else
                                    values = private$i.quantity.values[[dep.on]][[time]]
                                
                                if (comp$value.type == 'expression')
                                {
                                    if (length(private$i.quantity.mapping.indices[[quantity.name]]$components.depends.on) < i ||
                                        is.null(private$i.quantity.mapping.indices[[quantity.name]]$components.depends.on[[i]][[dep.on]]))
                                    {
                                        calculate.quantity.component.depends.on.indices(quantity, 
                                                                                        component.index = i,
                                                                                        depends.on = dep.on)
                                    } 
                                    dep.on.indices = private$i.quantity.mapping.indices[[quantity.name]]$components.depends.on[[i]][[dep.on]]
                                    
                                    values[dep.on.indices]
                                }
                                else
                                    values
                            })
                            names(bindings) = comp$depends.on
                            
                            #-- Calculate the value --#
                            bindings$specification.metadata = self$specification.metadata
                            bindings$location = private$i.location
                            
                            value = comp$evaluate(bindings = bindings,
                                                  error.prefix = error.prefix)
                            
                            #-- If a function value.type, check the returned value and set its dim.names if needed --#
                            if (comp$value.type=='function')
                            {
                                if (check.consistency)
                                    check.function.quantity.component.value(value, quantity=quantity, component.index=i,
                                                                            time=time, error.prefix=error.prefix)
                                
                                if ((length(private$i.quantity.component.dim.names[[quantity.name]]) < i ||
                                     is.null(private$i.quantity.component.dim.names[[quantity.name]][[i]])) ||
                                    (check.consistency && time == missing.times[1]))
                                    calculate.quantity.component.dim.names(quantity, 
                                                                           component.index = i,
                                                                           value.for.function = value)
                                
                                if (check.consistency && !dim.names.equal(dim.names.1 = private$i.quantity.component.dim.names[[quantity.name]][[i]],
                                                                        dim.names.2 = dimnames(value),
                                                                        match.order.of.dimensions = T,
                                                                        match.order.within.dimensions = T))
                                    stop(paste0(error.prefix,
                                                "The dimnames for the value calculated at time ", time,
                                                " do not match the dimnames of values for previous times"))
                                    
                            }
                            
                            #-- A check --#
                            if (length(value)==0)
                                browser()
                            
                            #-- Return --#
                            value
                        })
                        
                        #-- Recalculate the dim.names if needed --#
                        if (is.null(i.quantity.dim.names[[quantity.name]]))
                            calculate.quantity.dim.names(quantity)
                            
                        rv = NULL                   
                        #-- Incorporate each component into the quantity value --#
                        for (i in 1:quantity$n.components)
                        {
                            comp = quantity$components[[i]]
                            value = component.values[[i]]
                            
                            #-- Recalculate the indices if we need to --#
                            if (length(private$i.quantity.mapping.indices[[quantity.name]]$components.expand)<i || 
                                is.null(private$i.quantity.mapping.indices[[quantity.name]]$components.expand[[i]]))
                                private$calculate.quantity.component.expand.access.indices(quantity, component.index=i)
                            
                            #-- Pull the access/expand indices --#
                            expand.indices = private$i.quantity.mapping.indices[[quantity.name]]$components.expand[[i]]
                            access.indices = private$i.quantity.mapping.indices[[quantity.name]]$components.access[[i]]
                            
               #     if (quantity.name == 'sexual.contact.by.race')        
                #        browser()
                            # Fold it in to the rv
                            if (is.null(access.indices))
                                rv = value[expand.indices]
                            else if (comp$apply.function=='overwrite')
                                #rv = do_access_overwrite(dst=rv, src=value, dst_indices=access.indices, src_indices=expand.indices)
                                 rv[access.indices] = value[expand.indices]
                            else if (comp$apply.function=='add')
                                #rv = do_access_add(dst=rv, src=value, dst_indices=access.indices, src_indices=expand.indices)
                                 rv[access.indices] = rv[access.indices] + value[expand.indices]
                            else if (comp$apply.function=='subtract')
                                #rv = do_access_subtract(dst=rv, src=value, dst_indices=access.indices, src_indices=expand.indices)
                                 rv[access.indices] = rv[access.indices] - value[expand.indices]
                            else if (comp$apply.function=='multiply')
                                #rv = do_access_multiply(dst=rv, src=value, dst_indices=access.indices, src_indices=expand.indices)
                                 rv[access.indices] = rv[access.indices] * value[expand.indices]
                            else if (comp$apply.function=='divide')
                                #rv = do_access_divide(dst=rv, src=value, dst_indices=access.indices, src_indices=expand.indices)
                                 rv[access.indices] = rv[access.indices] / value[expand.indices]
                            else
                                stop(paste0("Invalid apply.function '", comp$apply.function, "' for model quantity '", quantity.name,
                                            "'. Must be one of 'overwrite', 'add', 'subtract', 'multiply', or 'divide'"))
                        }
                        
                        #-- Check scale --#
                        if (check.consistency && !is.null(quantity$scale))
                            check.values.for.model.scale(values = rv, 
                                                         scale = quantity$scale, 
                                                         variable.name.for.error = paste0("the calculated values at time ", time),
                                                         error.prefix =  paste0("Error calculating values for model quantity '", quantity.name, "': "))
                        
                        #-- Check for NA --#
                        if (any(is.na(rv)))
                            browser()
#                            stop(paste0(paste0("Error calculating values for model quantity '", quantity.name, "': NA values were generated")))
                        
                        if (length(rv)==0)
                            browser()
                        
                        #-- Set the dimnames and return --#
                        if (length(private$i.quantity.dim.names[[quantity.name]]) > 0)
                        {
                            dim(rv) = sapply(private$i.quantity.dim.names[[quantity.name]], length)
                            dimnames(rv) = private$i.quantity.dim.names[[quantity.name]]
                        }
                        rv
                    })
                    
                    #-- Order the values by time --#
                    private$i.quantity.values[[quantity.name]] = private$i.quantity.values[[quantity.name]][required.times]
                }

                invisible(self)
            }
        },

        # interpolates ramp on the model scale
        calculate.element.value = function(element.name, check.consistency)
        {
            element = private$get.specification()$get.quantity(element.name)
            if (is.null(private$i.quantity.value.times[[element.name]]))
                calculate.quantity.value.times(element.name)
            if (is.null(private$i.quantity.dim.names[[element.name]]))
                calculate.quantity.dim.names(element)
            
            #-- First, if there is a functional form, make sure the alphas are crunched --#
            if (!is.null(private$i.element.backgrounds[[element.name]]$functional.form))
            {
                private$i.element.backgrounds[[element.name]]$functional.form.alphas = 
                    lapply(private$i.element.backgrounds[[element.name]]$functional.form.alphas, function(alphas){
                        crunch.alphas(alphas,
                                      betas = private$i.element.backgrounds[[element.name]]$functional.form$betas[[alphas$name]],
                                      target.dim.names = private$i.quantity.dim.names[[element.name]],
                                      error.prefix = paste0("Error calculating the value for model element ",
                                                            element$get.original.name(wrt.version=self$version),
                                                            " - in crunching '", alphas$name, "' alphas for the functional form: "))
                    })
            }
            
            #-- Pull the background --#
            bkgd = private$i.element.backgrounds[[element.name]]
            
            #-- Now actually calculate --#
            if (private$i.quantity.is.static[element.name])
            {
                if (is.null(i.quantity.values[['all']]))
                {
                    if (is.null(bkgd$functional.form))
                        private$i.quantity.values[[element.name]][['all']] = bkgd$value
                    else
                        private$i.quantity.values[[element.name]][['all']] =
                            convert.model.scale(bkgd$functional.form$project.static(alphas = bkgd$functional.form.alphas,
                                                                                    dim.names = i.quantity.dim.names[[element.name]],
                                                                                    check.consistency = check.consistency,
                                                                                    error.prefix = paste0("Error projecting values from the (static) functional form for element '", element.name, "': ")),
                                                convert.from.scale = element$functional.form.scale,
                                                convert.to.scale = element$scale)
                }
            }
            else
            {
                missing.times = setdiff_sorted_vectors(private$i.quantity.value.times[[element.name]], 
                                                       as.numeric(names(private$i.quantity.values[[element.name]])))
                if (length(missing.times)>0)
                {
                    #-- Calculate functional form values --#
                    
                    new.times = numeric()
                    
                    if (sorted_vectors_overlap(bkgd$functional.form.times, missing.times)) #practically, we will never have just one functional form value missing - it will either be all of them or none of them
                    {
                        private$i.quantity.values[[element.name]][as.character(bkgd$functional.form.times)] = 
                            bkgd$functional.form$project(years = bkgd$functional.form.times,
                                                                             alphas = bkgd$functional.form.alphas,
                                                                             dim.names = i.quantity.dim.names[[element.name]],
                                                                             future.slope = bkgd$future.slope,
                                                                             future.slope.after.year = bkgd$future.slope.after.time,
                                                                             future.slope.is.on.transformed.scale = F, #is this what we want?
                                                                             check.consistency = check.consistency,
                                                                             error.prefix = paste0("Error projecting values from the functional form for element '", element.name, "': "))
                        
                        new.times = bkgd$functional.form.times
                    }
                    
                    #-- Calculate ramp values --#
                    if (!is.null(bkgd$ramp.interpolated.times) && sorted_vectors_overlap(bkgd$ramp.interpolated.times, missing.times))
                    {
                        private$i.quantity.values[[element.name]][as.character(bkgd$ramp.interpolated.times)] = 
                            element$calculate.ramp.values(ramp.values = bkgd$ramp.values,
                                                          ramp.times = bkgd$ramp.times,
                                                          first.functional.form.value = i.quantity.values[[element.name]][[ as.character(bkgd$functional.form.times[1]) ]],
                                                          functional.form.from.time = bkgd$functional.form.times[1])[as.character(bkgd$ramp.interpolated.times)]
                        
                        new.times = c(bkgd$ramp.interpolated.times, new.times)
                    }
                    
                    #-- Calculate taper.values --#
                    if (!is.null(bkgd$taper.interpolated.times) && sorted_vectors_overlap(bkgd$taper.interpolated.times, missing.times))
                    {
                        n.functional.form.times = length(bkgd$functional.form.times)
                        private$i.quantity.values[[element.name]][as.character(bkgd$taper.interpolated.times)] = 
                            element$calculate.taper.values(taper.values = bkgd$taper.values,
                                                           taper.times = bkgd$taper.times,
                                                           last.functional.form.value = i.quantity.values[[element.name]][[ as.character(bkgd$functional.form.times[n.functional.form.times]) ]],
                                                           functional.form.to.time = bkgd$functional.form.times[n.functional.form.times])[as.character(bkgd$taper.interpolated.times)]
                        
                        new.times = c(bkgd$taper.interpolated.times, new.times)
                    }
                    
                    #-- Convert Scale if Needed --#
                    if (length(new.times)>0)
                    {
                        private$i.quantity.values[[element.name]][as.character(new.times)] = 
                            convert.model.scale(private$i.quantity.values[[element.name]][as.character(new.times)],
                                                convert.from.scale = element$functional.form.scale,
                                                convert.to.scale = element$scale)
                    }
                    
                    
                    #-- Other times to interpolate --#
                    missing.interpolated.times = setdiff_sorted_vectors(missing.times,
                                                                        c(bkgd$ramp.interpolated.times, 
                                                                          bkgd$functional.form.times, 
                                                                          bkgd$taper.interpolated.times))
                    
                    if (length(missing.interpolated.times)>0)
                    {
                        interpolate.from.times = c(bkgd$ramp.interpolated.times, bkgd$functional.form.times, bkgd$taper.interpolated.times)
                        
                        private$i.quantity.values[[element.name]][as.character(missing.interpolated.times)] = 
                            interpolate(values = i.quantity.values[[element.name]][as.character(interpolate.from.times)],
                                        value.times = interpolate.from.times,
                                        desired.times = missing.interpolated.times)
                    }                    
                    
                    #-- Sort the result --#
                    private$i.quantity.values[[element.name]] = private$i.quantity.values[[element.name]][ as.character(private$i.quantity.value.times[[element.name]]) ]
                }
            }
            
            # Fold in the foreground
            
            # A debug check
            if (any(sapply(private$i.quantity.values[[element.name]], length)==0))
                browser()
            
            # Check scale
            check.values.for.model.scale(private$i.quantity.values[[element.name]],
                                         scale = element$scale,
                                         variable.name.for.error = "the calculated values",
                                         error.prefix =  paste0("Error calculating values for model element '", element.name, "': "))

            # Done
            invisible(self)
        },

        # interpolates ramp on the element scale
        ORIG.calculate.element.value = function(element.name, check.consistency)
        {
            element = private$get.specification()$get.quantity(element.name)
            if (is.null(private$i.quantity.value.times[[element.name]]))
                calculate.quantity.value.times(element.name)
            if (is.null(private$i.quantity.dim.names[[element.name]]))
                calculate.quantity.dim.names(element)
            
            #-- First, if there is a functional form, make sure the alphas are crunched --#
            if (!is.null(private$i.element.backgrounds[[element.name]]$functional.form))
            {
                private$i.element.backgrounds[[element.name]]$functional.form.alphas = 
                    lapply(private$i.element.backgrounds[[element.name]]$functional.form.alphas, function(alphas){
                        crunch.alphas(alphas,
                                      betas = private$i.element.backgrounds[[element.name]]$functional.form$betas[[alphas$name]],
                                      target.dim.names = private$i.quantity.dim.names[[element.name]],
                                      error.prefix = paste0("Error calculating the value for model element ",
                                                            element$get.original.name(wrt.version=self$version),
                                                            " - in crunching '", alphas$name, "' alphas for the functional form: "))
                    })
            }
            
            #-- Pull the background --#
            bkgd = private$i.element.backgrounds[[element.name]]
            if (element.name=='testing')
                browser()
            #-- Now actually calculate --#
            if (private$i.quantity.is.static[element.name])
            {
                if (is.null(i.quantity.values[['all']]))
                {
                    if (is.null(bkgd$functional.form))
                        private$i.quantity.values[[element.name]][['all']] = bkgd$value
                    else
                        private$i.quantity.values[[element.name]][['all']] =
                            convert.model.scale(bkgd$functional.form$project.static(alphas = bkgd$functional.form.alphas,
                                                                                    dim.names = i.quantity.dim.names[[element.name]],
                                                                                    check.consistency = check.consistency,
                                                                                    error.prefix = paste0("Error projecting values from the (static) functional form for element '", element.name, "': ")),
                                                convert.from.scale = element$functional.form.scale,
                                                convert.to.scale = element$scale)
                }
            }
            else
            {
                missing.times = setdiff_sorted_vectors(private$i.quantity.value.times[[element.name]], 
                                                       as.numeric(names(private$i.quantity.values[[element.name]])))
                if (length(missing.times)>0)
                {
                    #-- Calculate functional form values --#
                    if (sorted_vectors_overlap(bkgd$functional.form.times, missing.times)) #practically, we will never have just one functional form value missing - it will either be all of them or none of them
                    {
                        private$i.quantity.values[[element.name]][as.character(bkgd$functional.form.times)] = 
                            convert.model.scale(bkgd$functional.form$project(years = bkgd$functional.form.times,
                                                                             alphas = bkgd$functional.form.alphas,
                                                                             dim.names = i.quantity.dim.names[[element.name]],
                                                                             future.slope = bkgd$future.slope,
                                                                             future.slope.after.year = bkgd$future.slope.after.time,
                                                                             future.slope.is.on.transformed.scale = F, #is this what we want?
                                                                             check.consistency = check.consistency,
                                                                             error.prefix = paste0("Error projecting values from the functional form for element '", element.name, "': ")),
                                                convert.from.scale = element$functional.form.scale,
                                                convert.to.scale = element$scale)
                    }
 
                    #-- Calculate ramp values --#
                    if (!is.null(bkgd$ramp.interpolated.times) && sorted_vectors_overlap(bkgd$ramp.interpolated.times, missing.times))
                    {
                        private$i.quantity.values[[element.name]][as.character(bkgd$ramp.interpolated.times)] = 
                            element$calculate.ramp.values(ramp.values = bkgd$ramp.values,
                                                          ramp.times = bkgd$ramp.times,
                                                          first.functional.form.value = i.quantity.values[[element.name]][[ as.character(bkgd$functional.form.times[1]) ]],
                                                          functional.form.from.time = bkgd$functional.form.times[1])[as.character(bkgd$ramp.interpolated.times)]
                    }
                    
                    #-- Calculate taper.values --#
                    if (!is.null(bkgd$taper.interpolated.times) && sorted_vectors_overlap(bkgd$taper.interpolated.times, missing.times))
                    {
                        n.functional.form.times = length(bkgd$functional.form.times)
                        private$i.quantity.values[[element.name]][as.character(bkgd$taper.interpolated.times)] = 
                            element$calculate.taper.values(taper.values = bkgd$taper.values,
                                                          taper.times = bkgd$taper.times,
                                                          last.functional.form.value = i.quantity.values[[element.name]][[ as.character(bkgd$functional.form.times[n.functional.form.times]) ]],
                                                          functional.form.to.time = bkgd$functional.form.times[n.functional.form.times])[as.character(bkgd$taper.interpolated.times)]
                    }
                    
                    
                    #-- Other times to interpolate --#
                    missing.interpolated.times = setdiff_sorted_vectors(missing.times,
                                                                       c(bkgd$ramp.interpolated.times, 
                                                                         bkgd$functional.form.times, 
                                                                         bkgd$taper.interpolated.times))
                    
                    if (length(missing.interpolated.times)>0)
                    {
                        interpolate.from.times = c(bkgd$ramp.interpolated.times, bkgd$functional.form.times, bkgd$taper.interpolated.times)
                        
                        private$i.quantity.values[[element.name]][as.character(missing.interpolated.times)] = 
                            interpolate(values = i.quantity.values[[element.name]][as.character(interpolate.from.times)],
                                        value.times = interpolate.from.times,
                                        desired.times = missing.interpolated.times)
                    }                    
                    
                    #-- Sort the result --#
                    private$i.quantity.values[[element.name]] = private$i.quantity.values[[element.name]][ as.character(private$i.quantity.value.times[[element.name]]) ]
                }
            }
            
            # Fold in the foreground
            
            # A debug check
            if (any(sapply(private$i.quantity.values[[element.name]], length)==0))
                browser()
            
            # Check scale
            check.values.for.model.scale(private$i.quantity.values[[element.name]],
                                         scale = element$scale,
                                         variable.name.for.error = "the calculated values",
                                         error.prefix =  paste0("Error calculating values for model element '", element.name, "': "))
            
            # Done
            invisible(self)
        },
        
        check.function.quantity.component.value = function(value, 
                                                           quantity, 
                                                           component.index, 
                                                           time,
                                                           error.prefix)
        {
            error.prefix = paste0(error.prefix,
                                  "Invalid value for ",
                                  ifelse(component.index==1, "", paste0("the ", get.ordinal(component.index-1), " subset of ")),
                                  "model quantity ",
                                  quantity$get.original.name(self$version),
                                  " was returned by function '",
                                  quantity$components[[component.index]]$value$value.function.name,
                                  "' for time ", time, " - ")   
            
            # Make sure it's numeric
            if (!is.numeric(value))
                stop(paste0(error.prefix, 
                            "the returned value must be numeric"))
            
            # Make sure it's not empty
            if (length(value)==0)
                stop(paste0(error.prefix, 
                            "the returned value cannot be length zero"))
            
            # Make sure it is either a scalar or has dim.names set
            if (length(value)>1)
            {
                if (is.null(dimnames(value)))
                    stop(paste0(error.prefix,
                                "if the returned value is not a scalar, it must have named dimnames set"))
             
                if (is.null(names(dimnames(value))))   
                    stop(paste0(error.prefix,
                                "if the returned value is not a scalar, it must have NAMED dimnames set"))
            }
            
            # Make sure the dim.names are a subset of max.dim.names
            if (!is.null(dimnames(value)) && !is.null(quantity$max.dim.names))
            {
                verify.dim.names.for.quantity(dim.names = dimnames(value),
                                              quantity = quantity,
                                              variable.name.for.error = "the returned value's dimnames",
                                              error.prefix = error.prefix,
                                              wrt.version = self$version,
                                              component.index = component.index)
            }
        },
        
        ##----------------------------------------------------------------##
        ##-- CALCULATING QUANTITY/ELEMENT DIM.NAMES and MAPPING INDICES --##
        ##----------------------------------------------------------------##
        
        calculate.quantity.dim.names = function(quantity)
        {
            quantity.name = quantity$name
            
            if (is.element.name(quantity.name))
            {
                # Element dim.names are a combination of:
                # - If a functional.form is present, its min.dim.names and the dimensions for which there are alphas
                # - If no functional.form is present, the dim.names of the value
                # - dim.names from the foreground (still not implemented)
                
                bkgd = i.element.backgrounds[[quantity.name]]
                if (is.null(bkgd$functional.form))
                {
                    if (is.null(dimnames(bkgd$value)))
                        private$i.quantity.dim.names[[quantity.name]] = list()
                    else
                        private$i.quantity.dim.names[[quantity.name]] = dimnames(bkgd$value)
                }
                else if (is.null(i.quantity.max.dim.names[[quantity.name]]))
                {
                    private$i.quantity.dim.names[[quantity.name]] = outer.join.dim.names(bkgd$functional.form$minimum.dim.names,
                                                                                         lapply(bkgd$functional.form.alphas, get.alphas.minimum.dim.names))
                }
                else # this formulation is equivalent to the block above, but more efficient IF we have max.dim.names set
                {
                    new.dimensions = union(names(bkgd$functional.form$minimum.dim.names),
                                           unlist(sapply(bkgd$functional.form.alphas, get.alphas.minimum.dimensions)))
                    private$i.quantity.dim.names[[quantity.name]] = i.quantity.max.dim.names[[quantity.name]][new.dimensions]
                }
                
                # need to fold in foreground dimensions here
            }
            else
            {
                # When max.dim.names are set, non-element quantity dim.names have every dimension in max.dim.names that
                # - Appears in a component's value OR
                # - Appears in a component's applies.to
                # When max.dim.names are not set, then dim.names are the outer-join (union) of
                # - The dimnames of the all components plus
                # - The applies.to from each component (after the first, which has no applies to)
                
                if (is.null(i.quantity.max.dim.names[[quantity.name]]))
                {
                    # If there is just one component, then the dimnames of the quantity are just the dimnames of the component
                    if (length(private$i.quantity.component.dim.names[[quantity.name]]) == 1)
                    {
                        private$i.quantity.dim.names[[quantity.name]] = private$i.quantity.component.dim.names[[quantity.name]][[1]]
                    }
                    # If there is more than one component, we need to join them and make sure they align
                    else
                    {
                        dim.names = outer.join.dim.names(private$i.quantity.component.dim.names[[quantity.name]],
                                                         i.quantity.component.applies.to[[quantity.name]])
                        private$i.quantity.dim.names[[quantity.name]] = dim.names
                        
                        # Check to make sure that dimension values from each component align
                        for (i in 1:quantity$n.components)
                        {
                            comp = quantity$components[[i]]
                            
                        #    sapply(names(private$i.quantity.component.dim.names[[quantity.name]][[i]], function(d){
                            sapply(setdiff(names(private$i.quantity.component.dim.names[[quantity.name]][[i]], names(private$i.quantity.component.applies.to[[quantity.name]][[i]])), function(d){
                                    #       if (any(d==names(private$i.quantity.component.applies.to[[quantity.name]][[i]])))
                         #       {
                         #           if (i>1 && !setequal(dim.names[[d]], private$i.quantity.component.applies.to[[quantity.name]][[i]]))
                         #               stop(paste0("Error calculating dimnames for quantity ",
                         #                          quantity$get.original.name(self$version),
                         #                           ": the dimnames of the ",
                         #                           get.ordinal(i-1),
                         #                           " subset do not match the applies.to values for dimension '", d, "'"))
                         #       }
                         #       else
                         #       {
                                    if (!setequal(dim.names[[d]], private$i.quantity.component.dim.names[[quantity.name]][[i]][[d]]))
                                        stop(paste0("Error calculating dimnames for quantity ",
                                                    quantity$get.original.name(self$version),
                                                    ": the dimnames of its sub-components do not align in dimension '", d, "'"))
                         #       }
                            }))
                        }
                    }
                }
                else
                {
                    mask = sapply(names(private$i.quantity.max.dim.names[[quantity.name]]), function(d){
                        any(sapply(1:quantity$n.components, function(component.index){
                            any(names(private$i.quantity.component.dim.names[[quantity.name]][[component.index]]) == d) ||
                                any(names(private$i.quantity.component.applies.to[[quantity.name]][[component.index]]) == d)
                        }))
                    })
                 
                    private$i.quantity.dim.names[[quantity.name]] = private$i.quantity.max.dim.names[[quantity.name]][mask]
                }
            }
            
            #-- Make sure dim.names are a superset of required.dim.names --#
            required.dim.names = private$i.quantity.required.dim.names[[quantity.name]]
            if (length(required.dim.names)>0)
            {
                dimensions.length.greater.than.one = sapply(required.dim.names, length)>1
                missing.required.dimensions = setdiff(names(required.dim.names)[dimensions.length.greater.than.one],
                                                      names(private$i.quantity.dim.names[[quantity.name]]))
                if (length(missing.required.dimensions)>0)
                    stop(paste0("Error calculating dimnames for quantity ",
                                quantity$get.original.name(self$version),
                                ": the quantity's dimnames are missing required ",
                                ifelse(length(missing.required.dimensions)==1, "dimension ", "dimensions "),
                                collapse.with.and("'", missing.required.dimensions, "'")))

                sapply(names(required.dim.names), function(d){
                    values = private$i.quantity.dim.names[[quantity.name]][[d]]
                    if (!is.null(values) && !setequal(values, required.dim.names[[d]]))
                        stop(paste0("Error calculating dimnames for quantity ",
                                    quantity$get.original.name(self$version),
                                    ": the quantity's dimnames for dimension '", d,
                                    "' (", paste0("'", values, "'", collapse=', '),
                                    ") do not match the required dimnames for '", d, 
                                    "' (", paste0("'", required.dim.names[[d]], "'", collapse=', '), ")"))
                })
                
#                private$i.quantity.dim.names[[quantity.name]][names(private$i.quantity.required.dim.names[[quantity.name]])] = private$i.quantity.required.dim.names[[quantity.name]]
            }
            
            #--  If the dim.names have changed from previous, clear dependencies --#
            #    (and set these dimnames to be the reference - ie crunched - dimnames)
            if (!is.null(i.crunched.quantity.dim.names[[quantity.name]]) &&
                !dim.names.equal(i.crunched.quantity.dim.names[[quantity.name]],
                                 i.quantity.dim.names[[quantity.name]]))
            {
                clear.dependent.on.quantity.dim.names(quantity.name)
                private$i.diffeq.settings = notify.diffeq.settings.of.quantity.dim.names.change(private$i.diffeq.settings,
                                                                                                quantity.name = quantity.name)
                i.crunched.quantity.dim.names[[quantity.name]] = i.quantity.dim.names[[quantity.name]]
            }
            
            #-- A debugging check --#
            if (any(is.na(names(private$i.quantity.dim.names[[quantity.name]]))) ||
                any(sapply(private$i.quantity.dim.names[[quantity.name]], length)==0))
                browser()
            
            #-- Done --#
            invisible(self)
            
        },
        
        calculate.quantity.component.dim.names = function(quantity, component.index,
                                                          value.for.function)
        {
            quantity.name = quantity$name
            comp = quantity$components[[component.index]]
            
            if (comp$value.type=='character')
            {
                private$i.quantity.component.dim.names[[quantity.name]][[component.index]] = 
                    comp$apply.reversed.dimension.aliases(i.quantity.dim.names[[comp$depends.on]])
            }
            else if (comp$value.type=='expression')
            {
                if (is.null(quantity$max.dim.names))
                {
                    dim.names = comp$apply.reversed.dimension.aliases(private$i.quantity.dim.names[[ comp$depends.on[1] ]])
                    for (dep.on in comp$depends.on[-1])
                    {
                        to.incorporate = comp$apply.reversed.dimension.aliases(private$i.quantity.dim.names[[dep.on]])
                        common.dimensions = intersect(names(dim.names), names(to.incorporate))
                        sapply(common.dimensions, function(d){
                            if (!setequal(dim.names[[d]], to.incorporate[[d]]))
                            {
                                stop(paste0("Error calculating dimnames for ",
                                            ifelse(component.index==1, '', paste0("the ", get.ordinal(component.index-1), " subset of ")),
                                            "model quantity ", quantity$get.original.name(self$version),
                                            " - the dimnames of dependee quantities do not align (values for dimension '",
                                            d, "' from quantity '", dep.on, "' do not match values other dependee quantities)"))
                            }
                        })
                        
                        dim.names[names(to.incorporate)] = to.incorporate
                    }
                    
                    private$i.quantity.component.dim.names[[quantity.name]][[component.index]] = dim.names
                }
                else
                {
                    dimension.mask = sapply(names(private$i.quantity.component.max.dim.names[[quantity.name]][[component.index]]), function(d){
                        any(sapply(comp$depends.on, function(dep.on){
                            any(comp$apply.reversed.dimension.aliases( names(private$i.quantity.dim.names[[dep.on]]) ) == d)
                        }))
                    })
                    
                    private$i.quantity.component.dim.names[[quantity.name]][[component.index]] =
                        i.quantity.component.max.dim.names[[quantity.name]][[component.index]][dimension.mask]
                }
            }
            else if (comp$value.type=='numeric')
            {
                dim.names = dimnames(comp$value)
                if (is.null(dim.names))
                    private$i.quantity.component.dim.names[[quantity.name]][[component.index]] = list()
                else
                    private$i.quantity.component.dim.names[[quantity.name]][[component.index]] = dim.names
            }
            else
            {
                if (missing(value.for.function))
                    stop(paste0("In calculating component dim.names for the ",
                                get.ordinal(component.index), " component of quantity '",
                                quantity.name, "', 'value.for.function' must be specified"))
                
                dim.names = dimnames(value.for.function)
                if (is.null(dim.names))
                    private$i.quantity.component.dim.names[[quantity.name]][[component.index]] = list()
                else
                    private$i.quantity.component.dim.names[[quantity.name]][[component.index]] = dim.names
            }
            
            #--  If the dim.names have changed from previous, clear dependencies --#
            #    (and set these dimnames to be the reference - ie crunched - dimnames)
            if (!is.null(i.crunched.quantity.component.dim.names[[quantity.name]][[component.index]]) &&
                !dim.names.equal(i.crunched.quantity.component.dim.names[[quantity.name]],
                                 i.quantity.component.dim.names[[quantity.name]]))
            {
                clear.dependent.on.quantity.component.dim.names(quantity.name, component.index = component.index)
                i.crunched.quantity.component.dim.names[[quantity.name]][[component.index]] = 
                    i.quantity.component.dim.names[[quantity.name]][[component.index]]
            }
            
            #-- Done --#
            invisible(self)
        },
        
        calculate.quantity.component.depends.on.indices = function(quantity, component.index, depends.on)
        {
            quantity.name = quantity$name
            comp = quantity$components[[component.index]]
            
            if (length(private$i.quantity.mapping.indices[[quantity.name]]$components.depends.on)<component.index)
                private$i.quantity.mapping.indices[[quantity.name]]$components.depends.on[[component.index]] = list()
            
            private$i.quantity.mapping.indices[[quantity.name]]$components.depends.on[[component.index]][[depends.on]] = 
                get.expand.array.indices(to.expand.dim.names = comp$apply.reversed.dimension.aliases(private$i.quantity.dim.names[[depends.on]]),
                                         target.dim.names = private$i.quantity.component.dim.names[[quantity.name]][[component.index]])
            
            # Done
            invisible(self)
        },
        
        
        calculate.quantity.component.expand.access.indices = function(quantity, component.index)
        {
            quantity.name = quantity$name
   
            # Expand indices
            # (what indices expand to the subset of the quantity value that this component applies to)
            expand.to.dim.names = private$i.quantity.dim.names[[quantity.name]]
            expand.to.dim.names[names(private$i.quantity.component.applies.to[[quantity.name]][[component.index]])] =
                private$i.quantity.component.applies.to[[quantity.name]][[component.index]]
            
            if (length(private$i.quantity.component.dim.names[[quantity.name]][[component.index]])==0)
                private$i.quantity.mapping.indices[[quantity.name]]$components.expand[[component.index]] = 
                    rep(1, prod(as.numeric(sapply(expand.to.dim.names, length))))
            else
                private$i.quantity.mapping.indices[[quantity.name]]$components.expand[[component.index]] = 
                    get.expand.array.indices(to.expand.dim.names = private$i.quantity.component.dim.names[[quantity.name]][[component.index]],
                                             target.dim.names = expand.to.dim.names)
            
            # Access indices
            # (what indices into the quantity value to access for this component's applies.to)
            if (!is.null(quantity$components[[component.index]]$applies.to))
                private$i.quantity.mapping.indices[[quantity.name]]$components.access[[component.index]] =
                    get.array.access.indices(arr.dim.names = private$i.quantity.dim.names[[quantity.name]],
                                             dimension.values = private$i.quantity.component.applies.to[[quantity.name]][[component.index]])
            
            # Save the dim.names that we used to construct these indices (in case we have to change later)
            private$i.quantity.mapping.indices[[quantity.name]]$based.on.dim.names = private$i.quantity.dim.names[[quantity.name]]
            
            # Done
            invisible(self)
        },
        
        
        ##-------------------------------##
        ##-- Clearing Dependent Values --##
        ##-------------------------------##
        
        # There are three categories of dependencies we need to consider:
        # 1) Values - when an element's value is changed, the values of quantities that depend on it (and the quantities
        #    that depend on those, and so forth) change as well
        #    - We know that when an element's value changes, all quantities that depend on it also change
        #      So we can clear the element's calculated values and all dependent quantity calculated values at once
        #    - We also may know, for certain changes to an element, that only a SUBSET of its values will change as a result
        #      (So we don't always need to clear every single value)
        #    
        # 2) Times - when an element's self-times change, the self-times of quantities that depend on it may or may not change
        #    (they may not change if the other dependee elements of dependent quantities have self-times that are a superset 
        #     of the times that changed in the element). Consequently, the value times of this element and other quantities
        #     may or may not change
        #     - We CAN say, however, that the set of quantities whose self-times may potentially change once a 
        #       given element's times change is (a) the element itself and (b) all quantities that depend on the element
        #     - The set of quantities whose value-times may potentially change once a given element's times change is
        #       (a) the element itself, (b) all quantities that depend on the element, and (c) all co-dependee elements
        #     - In practical terms, when an element's times are modified, we will clear all the times that could potentially
        #       change as a result, because determining whether they need to change is just as expensive computantionally
        #       as recalculating the times
        #       
        # 3) Mapping Indices (the access indices, expand indices, and component depends-on indices) that are used to map
        #    from quantities to the quantity components that depend on them (which may have different dim names). These
        #    indices fundamentally depend on the inherent dimnames of a quantity - if the dimnames change, the indices will as well
        #    - Caveat (1): when a part of an element changes (the value of it if static, or alphas for its functional form),
        #      it is not trivially clear whether this leads to a change in the element's dimnames
        #    - Caveat (2): it is NOT the case that when a quantity's dimnames change, all its dependents' dimnames change as well
        #      (A dependent quantity's dimnames, which depend)
        #    - We DO know, however, that a quantity's dimnames will never change unless ALL of its values have changed (ie, been cleared)
        #      (so we can be confident that, if a quantity's dimnames and associated mapping indices change, we will be forcing
        #       a re-calculation of its values as well, and can afford to delay recalculating indices until they are needed)
        #    - In practical terms, when we are asked to clear dim.names for an element (because they MAY have been changed),
        #      we clear only that element's dim.names, but not the indices that depend on it, nor the dim.names of quantities 
        #      that depend on the element. BUT we store a copy of the element's dim.names the last time it was crunched ($crunched.dim.names)
        #      Then, when asked to recalculate that element value (and we will be asked, because any change that alters the
        #      dimnames also alters the values), if the recalculated dimnames are not the same as the previously-crunched dimnames,
        #      we clear the dependent quantities' dimensions and the mapping indices (and repeat the process when those values 
        #      are calculated)

        # times == NULL --> clear all values
        clear.dependent.values = function(element.name, times=NULL)
        {
            specification = get.specification()
            to.clear = c(element.name, specification$get.dependent.quantity.names(element.name))
            
            if (is.null(times))
            {
                for (one.to.clear in to.clear)
                    private$i.quantity.values[[one.to.clear]] = list()
            }
            else
            {
                time.indices = as.character(times)
                for (one.to.clear in to.clear)
                    private$i.quantity.values[[one.to.clears]][time.indices] = NULL
            }
            
            invisible(self)
        },
        
        clear.dependent.times = function(element.names)
        {
            specification = get.specification()
            to.clear.self = c(element.names, 
                              unlist(sapply(element.names, specification$get.dependent.quantity.names)))
            to.clear.value = c(to.clear.self, 
                               unlist(sapply(element.names,specification$get.co.dependee.element.names)))
            
            for (one.to.clear in to.clear.self)
                private$i.quantity.self.times[[one.to.clear]] = NULL
            for (one.to.clear in to.clear.value)
                private$i.quantity.value.times[[one.to.clear]] = NULL
            
            invisible(self)
        },
        
        clear.dim.names = function(quantity.name)
        {
            private$i.quantity.dim.names[[quantity.name]] = NULL
        },
        
        clear.dependent.on.quantity.dim.names = function(quantity.name)
        {
            specification = get.specification()
            
            # Clear the dimensions of quantity *components* that depend on this quantity's dim.names
            sapply(specification$get.dependent.quantity.names(quantity.name), function(dependent.name){
                dependent.quantity = specification$get.quantity(dependent.name)
                sapply(1:dependent.quantity$n.components, function(i){
                    if (any(dependent.quantity$components[[i]]$depends.on==quantity.name))
                        private$i.quantity.component.dim.names[[dependent.name]][[i]] = NULL
                })
            })
            
            # Clear expand and access mappings for this quantity
            private$i.quantity.mapping.indices[[quantity.name]]$components.expand = list()
            private$i.quantity.mapping.indices[[quantity.name]]$components.access = list()
            
            # Clear depends on mappings for quantity components that depend on this quantity
            sapply(specification$get.dependent.quantity.names(quantity.name), function(dependent.name){
                dependent.quantity = specification$get.quantity(dependent.name)
                sapply(1:dependent.quantity$n.components, function(i){
                    if (any(dependent.quantity$components[[i]]$depends.on==quantity.name))
                        private$i.quantity.mapping.indices[[dependent.on]]$components.depends.on[[component.index]][[quantity.name]] = NULL
                })
            })
            
            # Done
            invisible(self)
        },
        
        clear.dependent.on.quantity.component.dim.names = function(quantity.name, component.index)
        {
            specification = get.specification()
            
            # Clear the dim.names of quantities that depend on this quantity component's dim.names
            private$i.quantity.dim.names[specification$get.dependent.quantity.names(quantity.name)] = NULL
            
            # Clear expand and access mappings that use this quantity component
            private$i.quantity.mapping.indices[[quantity.name]]$components.expand[[component.index]] = NULL
            private$i.quantity.mapping.indices[[quantity.name]]$components.access[[component.index]] = NULL
            
            # Clear all depends on mappings for this quantity component
            private$i.quantity.mapping.indices[[dependent.on]]$components.depends.on[[component.index]] = list()
            
            # Done
            invisible(self)
        },
        
        ##--------------------------##
        ##-- Logging Instructions --##
        ##--------------------------##
        
        log.instruction = function(fn.name, ...)
        {
            instr = list(fn.name = fn.name,
                         args = list(...))
            
            private$i.instructions = c(i.instructions, list(instr))
        },
        
        execute.instruction = function(instr)
        {
            do.call(what=instr$fn.name, args=instr$args)
        },
        
        
        ##--------------------------------##
        ##-- Low-Level Internal Helpers --##
        ##--------------------------------##
        
        is.element.name = function(quantity.name)
        {
            any(names(private$i.element.backgrounds)==quantity.name)
        },
        
        get.specification = function()
        {
            get.compiled.specification.for.version(private$i.version)
        },

        #returns a vector of validated indices
        check.ramp.or.taper.values.and.indices = function(values,
                                                          indices,
                                                          current.values,
                                                          is.ramp,
                                                          is.times,
                                                          error.prefix)
        {
            # Set up for error messages
            if (is.ramp)
                ramp.or.taper = 'ramp'
            else
                ramp.or.taper = 'taper'
            
            if (is.times)
                values.or.times = "'times'"
            else
                values.or.times = "'values'"
            
            # Check values
            if (!is.numeric(values) || length(values)==0 || any(is.na(values)))
                stop(paste0(error.prefix, values.or.times, " must be a non-empty, non-NA, numeric vector"))
            if (length(values) > length(current.values))
                stop(paste0(error.prefix, values.or.times, " has ",
                            length(values), " elements, but the ",
                            ramp.or.taper, " only takes ",
                            length(current.values), " ", values.or.times,
                            ifelse(length(current.values==1),
                                   "",
                                   "s"),
                            "."))
            if (any(values<0))
                stop(paste0(error.prefix, values.or.times, " must all be greater than or equal to zero"))
            
            
            # Infer indices if none specified
            if (is.null(indices))
            {
                if (is.null(names(values)) || is.null(names(current.values)))
                    indices = 1:length(values)
                else
                    indices = names(values)
            }
            
            # Check indices
            if (is.character(indices))
            {
                if (is.null(names(current.values)))
                    stop(paste0(error.prefix, 
                                "'indices' cannot be a character value for this element, because the ",
                                ramp.or.taper,
                                " elements have not been given names."))
                
                invalid.names = setdiff(indices, names(current.values))
                if (length(invalid.names)>0)
                    stop(paste0(error.prefix,
                                "Invalid name(s) in indices. None of the ", ramp.or.taper,
                                " elements is named ",
                                collapse.with.and("'", invalid.names, "'")))
                
                if (length(indices) != length(values) && length(values) != 1)
                    stop(paste0(error.prefix,
                                "'indices' has length ", length(indices),
                                " but ", values.or.times, " has length ", length(values)))
            }
            else if (is.integer(indices))
            {
                if (any(indices<1) || any(indices>length(current.values)))
                    stop(paste0(error.prefix,
                                "indices ",
                                collapse.with.and(indices[indices<1 | indices>length(current.values)]),
                                ifelse(sum(indices<1 | indices>length(current.values))==1, ' is ', ' are '),
                                "invalid. Indices must be between 1 and ", length(current.values)))
                
                if (length(indices) != length(values) && length(values) != 1)
                    stop(paste0(error.prefix,
                                "'indices' has length ", length(indices),
                                " but ", values.or.times, " has length ", length(values)))
            }
            else if (is.logical(indices))
            {
                if (length(indices) != length(current.values))
                    stop(paste0(error.prefix,
                                "'indices' (as a logical vector) has length ", length(indices),
                                " but the ", ramp.or.taper,
                                " takes ", length(current.values), " ", values.or.times,
                                ifelse(length(current.values)==1,
                                       "",
                                       "s"),
                                "."))
                
                if (sum(indices) != length(values) && length(values) != 1)
                    stop(paste0(error.prefix,
                                "'indices' (as a logical vector) references ", sum(indices),
                                " elements, but the ", ramp.or.taper,
                                " takes ", length(current.values), " ", values.or.times,
                                ifelse(length(current.values)==1,
                                       "",
                                       "s"),
                                "."))
            }
            
            # Return
            indices
        },
    
        # Uses the old JHEEM package for testing
        do.test.crunch = function()
        {
            need.to.push <- function(...){
                T
            }
            
            #-----------------------------------------#
            #-- Go one by one and push to the JHEEM --#
            #-----------------------------------------#
            
            #-- SET-UP JHEEM --#
            private$do.setup.jheem.skeleton()
            
            #-- INITIAL POPULATION --#
            private$push.initial.population(need.to.push)
            
            #-- BIRTHS --#
            private$push.fertility.hiv.negative(need.to.push)
            private$push.fertility.hiv.positive(need.to.push)
            
            private$push.birth.proportions.hiv.negative(need.to.push)
            private$push.birth.proportions.hiv.positive(need.to.push)
            
            #-- AGING --#
            private$push.aging.hiv.negative(need.to.push)
            private$push.aging.hiv.positive(need.to.push)
            
            #-- MORTALITY --#
            private$push.general.mortality.hiv.negative(need.to.push)
            private$push.general.mortality.hiv.positive(need.to.push)
            private$push.hiv.mortality(need.to.push)
            
            #-- TRANSITIONS --#
            private$push.transitions(need.to.push)
            
            #-- TRANSMISSION --#
            private$push.susceptibility(need.to.push)
            private$push.transmissibility(need.to.push)
            private$push.transmission.contact(need.to.push)
            private$push.new.infection.proportions(need.to.push)
            
            
        },
    
        jheem = NULL,

        do.setup.jheem.skeleton = function()
        {
            specification.metadata = self$specification.metadata
            private$jheem = initialize.jheem(version = self$VERSION,
                                                age.cutoffs = specification.metadata$age.endpoints,
                                                race.strata = specification.metadata$dim.names$race,
                                                subpopulations = 'all_subpopulations',
                                                locations = 'all_locations',
                                                sex.strata = specification.metadata$dim.names$sex,
                                                risk.strata = specification.metadata$dim.names$risk,
                                                nonhiv.subsets = 'all_hiv_negative',
                                                continuum.of.care.states = specification.metadata$dim.names$continuum,
                                                cd4.strata = specification.metadata$dim.names$stage,
                                                hiv.subsets = 'all_hiv_positive',
                                                transmission.route.names = c('sexual','idu'),
                                                first.diagnosed.hiv.continuum.states = 'diagnosed',
                                                all.diagnosed.hiv.continuum.states = 'diagnosed',
                                                new.diagnoses.keep.dimensions = c('age','race','subpopulation',#'location',
                                                                                  'sex','risk','cd4'))
            
            #by default, minimal tracking
            private$jheem = set.track.incidence.dimensions(private$jheem, 
                                                              dimensions=c('age','race','subpopulation',#'location',
                                                                           'sex', 'risk'))
            
            
            # Fix strata sizes
            #if (!is.null(specification$fix.strata.sizes.prior.to.year) && !is.na(specification$fix.strata.sizes.prior.to.year))
            #{
                private$jheem = set.fixed.size.strata(private$jheem, fix.age=T, fix.race=T, fix.sex=T)
                
                private$jheem = set.keep.strata.sizes.constant(private$jheem,
                                                                  fix.strata.sizes = T,
                                                                  time=-Inf)
                
                private$jheem = set.keep.strata.sizes.constant(private$jheem,
                                                                  fix.strata.sizes = F,
                                                                  time=2007)
            #}
        },
        
        
        push.initial.population = function(need.to.push)
        {
            if (need.to.push('initial.population.hiv.negative', 'initial.population.hiv.positive'))
            {
                specification.metadata = self$specification.metadata
                
                init.hiv.negative = expand.population(private$i.quantity.values$initial.population.uninfected[[1]],
                                                      target.dim.names = c(specification.metadata$dim.names[c('age','race')],
                                                                           list(subpopulation='all_subpopulations'),
                                                                           specification.metadata$dim.names[c('sex','risk')],
                                                                           list(non.hiv.subset='all_hiv_negative')))
                private$jheem = set.initial.population.hiv.negative(private$jheem, init = init.hiv.negative)
                
                init.hiv.positive = private$i.quantity.values$initial.population.infected[[1]]
                dim.names = dimnames(init.hiv.positive)
                names(dim.names)[names(dim.names)=='stage'] = 'cd4'
                dim(init.hiv.positive) = sapply(dim.names, length)
                dimnames(init.hiv.positive) = dim.names
                init.hiv.positive = expand.population(init.hiv.positive,
                                                      target.dim.names = c(specification.metadata$dim.names[c('age','race')],
                                                                           list(subpopulation='all_subpopulations'),
                                                                           specification.metadata$dim.names[c('sex','risk','continuum')],
                                                                           list(cd4=specification.metadata$dim.names$stage),
                                                                           list(hiv.subset='all_hiv_positive')))
                                                                           
                private$jheem = set.initial.population.hiv.positive(private$jheem, init = init.hiv.positive)
            }
        },
        
        push.fertility.hiv.negative = function(need.to.push)
        {
            private$do.push.quantity.to.jheem(quantity.name='fertility',
                                              set.function=set.fertility.hiv.negative,
                                              need.to.push=need.to.push)
        },
        
        push.fertility.hiv.positive = function(need.to.push)
        {
            private$do.push.quantity.to.jheem(quantity.name='fertility',
                                              set.function=set.fertility.hiv.positive,
                                              need.to.push=need.to.push)
        },
        
        push.birth.proportions.hiv.negative = function(need.to.push)
        {
            if (need.to.push('birth.proportions.hiv.negative')) # Check that proportions sum to 1 for each from stratum
            {
                specification.metadata = self$specification.metadata
                proportions = private$i.quantity.values[['uninfected.birth.proportions']][[1]]
                from.dimensions = grepl("\\.from$", names(dimnames(proportions)))
                should.sum.to.1 = rowSums(proportions, dims=sum(from.dimensions))
                if (any(should.sum.to.1 != 1))
                    stop(paste0("Error setting birth.proportions.hiv.negative - proportions do not sum to 1 in all 'from' strata"))
             
                dim.names = c(specification.metadata$dim.names[c('race.from')],
                              list(subpopulation.to = 'all_subpopulations'),
                              specification.metadata$dim.names[c('sex.to', 'risk.to')],
                              list(non.hiv.subset.to='all_hiv_negative'))
                full.proportions = array(0, dim=sapply(dim.names, length), dimnames=dim.names)  
                full.proportions[,1,,1,1] = proportions
      
                private$jheem = set.birth.proportions.hiv.negative(private$jheem, 
                                                                   full.proportions)
            }
        },
        
        push.birth.proportions.hiv.positive = function(need.to.push)
        {
            if (need.to.push('birth.proportions.hiv.positive')) # Check that proportions sum to 1 for each from stratum
            {
                specification.metadata = self$specification.metadata
                proportions = private$i.quantity.values[['uninfected.birth.proportions']][[1]]
                from.dimensions = grepl("\\.from$", names(dimnames(proportions)))
                should.sum.to.1 = rowSums(proportions, dims=sum(from.dimensions))
                if (any(should.sum.to.1 != 1))
                    stop(paste0("Error setting birth.proportions.hiv.negative - proportions do not sum to 1 in all 'from' strata"))
                
                dim.names = c(specification.metadata$dim.names[c('race.from')],
                              list(subpopulation.to = 'all_subpopulations'),
                              specification.metadata$dim.names[c('sex.to', 'risk.to')],
                              list(non.hiv.subset.to='all_hiv_negative'))
                full.proportions = array(0, dim=sapply(dim.names, length), dimnames=dim.names)  
                full.proportions[,1,,1,1] = proportions
                
                private$jheem = set.birth.proportions.hiv.positive(private$jheem, 
                                                                   full.proportions,
                                                                   fraction.births.infected = 0)
            }
        },
        
        push.aging.hiv.negative = function(need.to.push)
        {
            private$do.push.quantity.to.jheem(quantity.name='default.aging',
                                      set.function=set.aging.hiv.negative,
                                      need.to.push=need.to.push)
        },
        
        push.aging.hiv.positive = function(need.to.push)
        {
            private$do.push.quantity.to.jheem(quantity.name='hiv.positive.aging.rates',
                                      set.function=set.aging.hiv.positive,
                                      need.to.push=need.to.push)
        },   
        
        
        push.general.mortality.hiv.negative = function(need.to.push)
        {
            private$do.push.quantity.to.jheem(quantity.name='general.mortality',
                                      set.function=set.general.mortality.hiv.negative,
                                      need.to.push=need.to.push)
        },
        
        push.general.mortality.hiv.positive = function(need.to.push)
        {
            private$do.push.quantity.to.jheem(quantity.name='general.mortality',
                                      set.function=set.general.mortality.hiv.positive,
                                      need.to.push=need.to.push)
        },
        
        push.hiv.mortality = function(need.to.push)
        {
            private$do.push.quantity.to.jheem(quantity.name='infection.specific.mortality',
                                      set.function=set.hiv.specific.mortality,
                                      need.to.push=need.to.push)
        },
        
        push.transitions = function(need.to.push)
        {
            specification.metadata = self$specification.metadata
            spec = private$get.specification()
            refs = spec$top.level.references[sapply(spec$top.level.references, function(ref){ref$type=='transition.reference'})]
            
            for (ont in c('uninfected','infected'))
            {
                o.refs = refs[sapply(refs, function(ref){ref$ontology.name==ont})]
                transition.dimensions = unique(sapply(o.refs, function(ref){ref$dimension}))
                
                for (d in transition.dimensions)
                {
                    d.refs = o.refs[sapply(o.refs, function(ref){ref$dimension==d})]
                    
                    quantity.names = sapply(d.refs, function(ref){
                        ref$value.quantity.name
                    })
                    
                    dimensions = unique(c(paste0(d, c('.from','.to')),
                                        unlist(sapply(quantity.names, function(qname){
                                            names(private$i.quantity.dim.names[[qname]])
                                        }))))
                    
                    dim.names = specification.metadata$dim.names[intersect(specification.metadata$dimensions, dimensions)]
                    
                    
                    times = sort(unique(unlist(private$i.quantity.value.times[quantity.names[!private$i.quantity.is.static[quantity.names]]])))
                    if (length(times)==0)
                        times = 'all'
                    skeleton.arr = array(0, dim=sapply(dim.names, length), dimnames = dim.names)
                    
                    values = lapply(times, function(time){
                        arr = skeleton.arr
                        
                        for (ref in d.refs)
                        {
                            qname = ref$value.quantity.name
                            if (length(private$i.quantity.values[[qname]])==1)
                                val = private$i.quantity.values[[qname]][[1]]
                            else
                                val = interpolate(values = private$i.quantity.values[[qname]],
                                                  value.times = private$i.quantity.value.times[[qname]],
                                                  desired.times = time)[[1]]
                            
                            dimension.values = private$i.quantity.dim.names[[qname]]
                            dimension.values[[paste0(d, '.from')]] = ref$from.compartments
                            dimension.values[[paste0(d, '.to')]] = ref$to.compartments
                            array.access(arr, dimension.values = dimension.values) = val
                        }
                        
                        dim.names = dimnames(arr)
                        names(dim.names)[names(dim.names)=='stage'] = 'cd4'
                        names(dim.names)[names(dim.names)=='stage.from'] = 'cd4.from'
                        names(dim.names)[names(dim.names)=='stage.to'] = 'cd4.to'
                        
                        dim(arr) = sapply(dim.names, length)
                        dimnames(arr) = dim.names
                        
                        arr
                    })
                    
                    for (i in 1:length(times))
                    {
                        time = times[i]
                        if (time=='all')
                            time = -Inf
                        
                        if (ont == 'uninfected')
                        {
                            private$jheem = set.transition.array.hiv.negative(private$jheem,
                                                                              transition.array = values[[i]],
                                                                              time = time)
                        }
                        else
                        {
                            
                            private$jheem = set.transition.array.hiv.positive(private$jheem,
                                                                              transition.array = values[[i]],
                                                                              time = time)
                        }
                    }
                }
            }
        },
        
        push.susceptibility = function(need.to.push)
        {
            for (mode in c('sexual','idu'))
                private$do.push.quantity.to.jheem(quantity.name=paste0(mode, '.susceptibility'),
                                                       set.function=set.susceptibility,
                                                       need.to.push=need.to.push,
                                                       transmission.route.names = mode)
        },
        
        push.transmissibility = function(need.to.push)
        {
            for (mode in c('sexual','idu'))
                private$do.push.quantity.to.jheem(quantity.name=paste0(mode, '.transmissibility'),
                                                       set.function=set.transmissibility,
                                                       need.to.push=need.to.push,
                                                       transmission.route.names = mode)
        },
        
        push.transmission.contact = function(need.to.push)
        {
            for (mode in c('sexual','idu'))
                private$do.push.quantity.to.jheem(quantity.name=paste0(mode, '.contact'),
                                                       set.function=set.transmission.contact.array,
                                                       need.to.push=need.to.push,
                                                       transmission.route.names = mode)
        },
        
        push.new.infection.proportions = function(need.to.push)
        {
            #@need to do
            if (need.to.push('new.infection.proportions')) # Check that proportions sum to 1 for each stratum of non-hiv specific states
            {
#                specification = get.components.specification(components)
 #               sum.across = setdiff(names(specification$dimension.names.by.subgroup$all),
  #                                   c('continuum','cd4','hiv.subset'))
                
   #             proportions = private$i.quantity.values[['new.infection.proportions']][[1]]
    #            should.sum.to.1 = rowSums(proportions, dims=length(sum.across))
     #           if (max(abs(should.sum.to.1 - 1))>0.0000001)
      #              stop(paste0("Error setting new.infections - proportions do not sum to 1 in all strata of hiv-negative states (",
       #                         paste0(sum.across, collapse=' x '), ")"))
                
                private$do.push.quantity.to.jheem(quantity.name='new.infection.proportions',
                                          set.function=set.new.infection.proportions,
                                          need.to.push=need.to.push)
            }
        },
        
        
        
        # A general helper that pulls time varying quantities from the components
        # and pushes them to the jheem object
        do.push.quantity.to.jheem = function(quantity.name,
                                              set.function,
                                              need.to.push,
                                              ...)
        {
            print(quantity.name)
            if (need.to.push(quantity.name))
            {
                specification.metadata = self$specification.metadata

                values = private$i.quantity.values[[quantity.name]]
                values = lapply(values, function(val){
                    if (!is.null(dimnames(val)))
                    {
                        dim.names = dimnames(val)
                        dimension.values = specification.metadata$dim.names[names(dim.names)] 
                        
                        names(dim.names)[names(dim.names)=='stage'] = 'cd4'
                        names(dimension.values)[names(dimension.values)=='stage'] = 'cd4'
                        
                        dim(val) = sapply(dim.names, length)
                        dimnames(val) = dim.names
                        
                        array.access(val, dimension.values = dimension.values) 
                            #to get things in the right order
                    }
                })
                
                times = private$i.quantity.value.times[[quantity.name]]
                
                if (is.null(values))
                    stop(paste0("Quantity '", quantity.name, "' has not been calculated."))
                if (is.null(times))
                    private$jheem = set.function(jheem=private$jheem,
                                                    values[[1]],
                                                    ...)
                else
                {
                    for (i in 1:length(values))
                        private$jheem = set.function(jheem=private$jheem,
                                                     values[[i]],
                                                     time=times[i],
                                                     ...)
                    
                }
            }
        }

            
    )
)