
##----------------------##
##----------------------##
##-- PUBLIC INTERFACE --##
##----------------------##
##----------------------##


##----------------------------------------------##
##-- FUNCTIONS to MODIFY the JHEEM'S SETTINGS --##
##----------------------------------------------##

#'@title Set the Value of a Model Element
#'
#'@param model.settings An object of class 'jheem.model.settings'
#'@param element.name The name of the model element to update a value for
#'@param value The new value for the model element
#'
#'@family Functions to modify model settings
#'
#'@export
set.element.value <- function(model.settings,
                              element.name,
                              value)
{
    if (!is(model.settings, "R6") || !is(model.settings, "jheem.model.settings"))
        stop("model.settings must be an R6 object of class 'jheem.model.settings'")
    
    model.settings$set.element.value(element.name = element.name,
                                     value = value)
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
#'@family Functions to create and modify model settings
#'
#'@export
set.element.functional.form.main.effect.alphas <- function(model.settings,
                                                           element.name,
                                                           alpha.name,
                                                           values,
                                                           applies.to.dimension.values=names(values),
                                                           dimensions=names(applies.to.dimension.values))
{
    if (!is(model.settings, "R6") || !is(model.settings, "jheem.model.settings"))
        stop("model.settings must be an R6 object of class 'jheem.model.settings'")
    
    model.settings$set.element.functional.form.main.effect.alphas(element.name = element.name,
                                                                alpha.name = alpha.name,
                                                                values = values,
                                                                applies.to.dimension.values = applies.to.dimension.values,
                                                                dimensions = dimensions)
}

#'@title Set Interaction Alpha Values for a Functional Form for a Model Element
#'
#'@details Alphas for an 'interaction' will apply to every combination of applies.to.dimension.values from different dimensions (as opposed to 'main effect' apply to all values in a dimension
#'
#'@inheritParams set.element.functional.form.main.effect.alphas
#'@param value A single numeric value, which will apply to every combination of applies.to.dimension.values which are from different dimensions
#'@param applies.to.dimension.values Either a vector (character or integer) or a list where each element is either a single character value or single integer value, indicating the compartments to which value applies
#'
#'@family Functions to create and modify a model settings
#'
#'@export
set.element.functional.form.interaction.alphas <- function(model.settings,
                                                           element.name,
                                                           alpha.name,
                                                           value,
                                                           applies.to.dimension.values=names(values),
                                                           dimensions=names(applies.to.dimension.values))
{
    if (!is(model.settings, "R6") || !is(model.settings, "jheem.model.settings"))
        stop("model.settings must be an R6 object of class 'jheem.model.settings'")
    
    model.settings$set.element.functional.form.interaction.alphas(element.name = element.name,
                                                                alpha.name = alpha.name,
                                                                value = value,
                                                                applies.to.dimension.values = applies.to.dimension.values,
                                                                dimensions = dimensions)
}

#'@title Set the Times From and To Which the Functional Form Determines a Model Element's Value
#'
#'@inheritParams set.element.value
#'@param element.name The name of the model element to set functional.form times for
#'@param from.time,to.time The time from or to which the functional form is active
#'
#'@family Functions to create and modify a model settings
#'
#'@export
set.element.functional.form.from.time <- function(model.settings,
                                                  element.name,
                                                  from.time)
{
    if (!is(model.settings, "R6") || !is(model.settings, "jheem.model.settings"))
        stop("model.settings must be an R6 object of class 'jheem.model.settings'")
    
    model.settings$set.element.functional.form.from.time(element.name = element.name,
                                                       from.time = from.time)
}


#'@describeIn set.element.functional.form.from.time
#'
#'@export
set.element.functional.form.to.time <- function(model.settings,
                                                element.name,
                                                to.time)
{
    if (!is(model.settings, "R6") || !is(model.settings, "jheem.model.settings"))
        stop("model.settings must be an R6 object of class 'jheem.model.settings'")
    
    model.settings$set.element.functional.form.to.time(element.name = element.name,
                                                     to.time = to.time)
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
#'@family Functions to create and modify a model settings
#'
#'@export
set.element.ramp.times <- function(model.settings,
                                   element.name,
                                   times,
                                   indices=1:length(times))
{
    if (!is(model.settings, "R6") || !is(model.settings, "jheem.model.settings"))
        stop("model.settings must be an R6 object of class 'jheem.model.settings'")
    
    model.settings$set.element.ramp.times(element.name = element.name,
                                        times = times,
                                        indices = indices)
}

#'@describeIn set.element.ramp.times
#'
#'@export
set.element.ramp.values <- function(model.settings,
                                    element.name,
                                    values,
                                    indices=1:length(values))
{
    if (!is(model.settings, "R6") || !is(model.settings, "jheem.model.settings"))
        stop("model.settings must be an R6 object of class 'jheem.model.settings'")
    
    model.settings$set.element.ramp.values(element.name = element.name,
                                         values = values,
                                         indices = indices)
}

#'@describeIn set.element.ramp.times
#'
#'@export
set.element.taper.times <- function(model.settings,
                                    element.name,
                                    times,
                                    indices=1:length(times))
{
    if (!is(model.settings, "R6") || !is(model.settings, "jheem.model.settings"))
        stop("model.settings must be an R6 object of class 'jheem.model.settings'")
    
    model.settings$set.element.taper.times(element.name = element.name,
                                         times = times,
                                         indices = indices)
}

#'@describeIn set.element.ramp.times
#'
#'@export
set.element.taper.values <- function(model.settings,
                                     element.name,
                                     values,
                                     indices=1:length(values))
{
    if (!is(model.settings, "R6") || !is(model.settings, "jheem.model.settings"))
        stop("model.settings must be an R6 object of class 'jheem.model.settings'")
    
    model.settings$set.element.taper.values(element.name = element.name,
                                          values = values,
                                          indices = indices)
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
#'@family Functions to create and modify a model settings
#'
#'@export
set.element.functional.form.future.slope <- function(model.settings,
                                                     element.names,
                                                     slope)
{
    if (!is(model.settings, "R6") || !is(model.settings, "jheem.model.settings"))
        stop("model.settings must be an R6 object of class 'jheem.model.settings'")
    
    model.settings$set.element.functional.form.future.slope(element.names = element.names,
                                                          slope = slope)
}


#'@describeIn set.element.functional.form.future.slope
#'
#'@export
set.element.functional.form.future.slope.after.time <- function(model.settings,
                                                                element.names,
                                                                after.year)
{
    if (!is(model.settings, "R6") || !is(model.settings, "jheem.model.settings"))
        stop("model.settings must be an R6 object of class 'jheem.model.settings'")
    
    model.settings$set.element.functional.form.future.slope.after.time(element.names = element.names,
                                                                     after.year = after.year)
}


##-------------------------------------------------------------------------##
##-- SOME STREAMLINED, CONVENIENCE FUNCTIONS FOR SETTING MULTIPLE VALUES --##
##-------------------------------------------------------------------------##

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
                                                               throw.error.if.no.parameters = T)
{
    if (!is(model.settings, "R6") || !is(model.settings, "jheem.model.settings"))
        stop("model.settings must be an R6 object of class 'jheem.model.settings'")
    
    specification.metadata = model.settings$specification.metadata
    
    #-- Check Arguments --#
    if (!is.numeric(parameters) && !is(parameters, 'protected.numeric.vector'))
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
        # Push the main effect values to the settings
        model.settings$set.element.functional.form.main.effect.alphas(element.name = element.name,
                                                                    alpha.name = alpha.name,
                                                                    values = parameter.values,
                                                                    applies.to.dimension.values = parameter.dim.values,
                                                                    dimensions = parameter.dimensions)
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

JHEEM.CODE.ITERATION = '2.0'


##------------------------------------##
##-- THE JHEEM MODEL SETTINGS CLASS --##
##                                    ##
##   This class is a pass-through     ##
##   wrapper to the jheem             ##
##   class, so that we can expose     ##
##   some of the jheem's methods      ##
##   while protecting others          ##
##------------------------------------##

JHEEM.MODEL.SETTINGS = R6::R6Class(
    'jheem.model.settings',
    inherit = JHEEM.ENTITY,
    portable = F,
    
    public = list(
        
        initialize = function(jheem, check.consistency)
        {
            if (!is(jheem, "R6") || !is(jheem, "jheem"))
                stop("jheem must be an R6 object of class 'jheem'")
            
            super$initialize(version = jheem$version,
                             sub.version = jheem$sub.version,
                             location = jheem$location,
                             type = 'model.settings')
            
            if (!is.logical(check.consistency) || length(check.consistency)!=1 || is.na(check.consistency))
                stop("Cannot create JHEEM Model Settings: 'check.consistency' must be a single, non-NA logical value")
            
            private$i.jheem = jheem
            private$i.check.consistency = check.consistency
        },
        
        set.element.value = function(element.name,
                                     value)
        {
            private$i.jheem$set.element.value(element.name = element.name,
                                               value = value,
                                               check.consistency = private$i.check.consistency)
        },
        
        set.element.functional.form.main.effect.alphas = function(element.name,
                                                                  alpha.name,
                                                                  values,
                                                                  applies.to.dimension.values=names(values),
                                                                  dimensions=names(applies.to.dimension.values))
        {
            private$i.jheem$set.element.functional.form.main.effect.alphas(element.name = element.name,
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
            private$i.jheem$set.element.functional.form.interaction.alphas(element.name = element.name,
                                                                            alpha.name = alpha.name,
                                                                            value = value,
                                                                            applies.to.dimension.values = applies.to.dimension.values,
                                                                            dimensions = dimensions,
                                                                            check.consistency = private$i.check.consistency)
        },
        
        set.element.functional.form.from.time = function(element.name,
                                                         from.time)
        {
            private$i.jheem$set.element.functional.form.from.time(element.name = element.name,
                                                                   from.time = from.time,
                                                                   check.consistency = private$i.check.consistency)
        },
        
        set.element.functional.form.to.time = function(element.name,
                                                       to.time)
        {
            private$i.jheem$set.element.functional.form.to.time(element.name = element.name,
                                                                 to.time = to.time,
                                                                 check.consistency = private$i.check.consistency)
        },
        
        set.element.ramp.times = function(element.name,
                                          times,
                                          indices=1:length(times))
        {
            private$i.jheem$set.element.ramp.times(element.name = element.name,
                                                    times = times,
                                                    indices = indices,
                                                    check.consistency = private$i.check.consistency)
        },
        
        set.element.ramp.values = function(element.name,
                                           values,
                                           indices=1:length(values))
        {
            private$i.jheem$set.element.ramp.values(element.name = element.name,
                                                     values = values,
                                                     indices = indices,
                                                     check.consistency = private$i.check.consistency)
        },
        
        set.element.taper.times = function(element.name,
                                           times,
                                           indices=1:length(times))
        {
            private$i.jheem$set.element.taper.times(element.name = element.name,
                                                     times = times,
                                                     indices = indices,
                                                     check.consistency = private$i.check.consistency)
        },
        
        set.element.taper.values = function(element.name,
                                            values,
                                            indices=1:length(values))
        {
            private$i.jheem$set.element.taper.values(element.name = element.name,
                                                      values = values,
                                                      indices = indices,
                                                      check.consistency = private$i.check.consistency)
        },
        
        set.element.functional.form.future.slope = function(element.names,
                                                            slope)
        {
            private$i.jheem$set.element.functional.form.future.slope(element.names,
                                                                      slope,
                                                                      check.consistency = private$i.check.consistency)
        },
        
        set.element.functional.form.future.slope.after.time = function(element.names,
                                                                       after.time)
        {
            private$i.jheem$set.element.functional.form.future.slope.after.time(element.names = element.names,
                                                                                 after.time = after.time,
                                                                                 check.consistency = private$i.check.consistency)
        }
        
    ),
    
    active = list(
        
    ),
    
    private = list(
        
        i.jheem = NULL,
        i.check.consistency = NULL,
        
        get.current.code.iteration = function()
        {
            JHEEM.CODE.ITERATION
        }
    )
)


##----------------------------##
##-- THE JHEEM ENGINE CLASS --##
##----------------------------##

create.jheem.engine <- function(version,
                                location,
                                start.year,
                                end.year,
                                sub.version=NULL,
                                max.run.time.seconds=Inf,
                                prior.sim=NULL,
                                intervention.code = NULL,
                                calibration.code = NULL,
                                keep.from.year=start.year,
                                keep.to.year=end.year,
                                atol=1e-04, rtol=1e-04)
{
    jheem = JHEEM$new(version = version,
                      sub.version = sub.version,
                      location = location,
                      error.prefix = "Cannot create JHEEM Engine: ")
    
    JHEEM.ENGINE$new(jheem = jheem,
                     start.year = start.year,
                     end.year = end.year,
                     max.run.time.seconds = max.run.time.seconds,
                     keep.from.year = keep.from.year,
                     keep.to.year = keep.to.year,
                     atol = atol,
                     rtol = rtol,
                     intervention.code = intervention.code,
                     calibration.code = calibration.code)
}

check.sim.can.seed.run <- function(prior.simulation.set,
                                   start.year,
                                   error.prefix)
{
    if (start.year < prior.simulation.set$from.year ||
        start.year > (prior.simulation.set$to.year+1))
    {
        stop(paste0(error.prefix, "Cannot run simulations from ", start.year, 
                    " from previous simulations which only include projections from ",
                    prior.simulation.set$from.year, " to ", prior.simulation.set$to.year))
    }
}

DEFAULT.ATOL = 1e-03
DEFAULT.RTOL = 1e-03

JHEEM.ENGINE = R6::R6Class(
    'jheem.engine',
    inherit = JHEEM.ENTITY,
    portable = F,
    
    public = list(
        
        initialize = function(jheem,
                              start.year,
                              end.year,
                              prior.simulation.set=NULL,
                              max.run.time.seconds=Inf,
                              keep.from.year = start.year,
                              keep.to.year = end.year,
                              foregrounds = NULL,
                              atol= DEFAULT.ATOL, 
                              rtol = DEFAULT.RTOL,
                              intervention.code = NULL,
                              calibration.code = NULL,
                              error.prefix = "Cannot create JHEEM Engine: ")
        {
            #-- Check Arguments --#
            
            if (!is.character(error.prefix) || length(error.prefix)!=1 || is.na(error.prefix))
                stop("Cannot initialize JHEEM Engine - error.prefix must be a single, non-NA character vector")
            
            if (!is(jheem, 'jheem'))
                stop(paste0(error.prefix, "jheem must be an object of class 'jheem'"))
         
            super$initialize(version = jheem$version,
                             sub.version = jheem$sub.version,
                             location = jheem$location,
                             type = 'engine')
            
            jheem$set.intervention.code(intervention.code)
            jheem$set.calibration.code(calibration.code)
               
            # Start and end years
            if (!is.numeric(start.year) || length(start.year)!=1 || is.na(start.year))
                stop(paste0(error.prefix, "'start.year' must be a single, non-NA numeric value"))
            
            if (!is.numeric(end.year) || length(end.year)!=1 || is.na(end.year))
                stop(paste0(error.prefix, "'end.year' must be a single, non-NA numeric value"))
            
            if (start.year >= end.year)
                stop(paste0(error.prefix, "'start.year' (", start.year,
                            ") must be PRIOR to 'end.year' (", end.year, ")"))
            
            # Prior simulation set
            if (!is.null(prior.simulation.set))
            {
                if (!is(prior.simulation.set, 'jheem.simulation.set'))
                    stop(paste0(error.prefix, "If 'prior.simulation.set' is specified (ie not NULL), it must be an object of class 'jheem.simulation.set'"))
                
                if (prior.simulation.set$version != jheem$version)
                    stop(paste0(error.prefix, "The prior.simulation.set's version ('", prior.simulation.set$version, "') does not match the JHEEM version ('", jheem$version, "')"))
                    
                if (!identical(prior.simulation.set$sub.version, jheem$sub.version))
                    stop(paste0(error.prefix, "The prior.simulation.set's sub-version (", 
                                ifelse(is.null(prior.simulation.set$sub.version), "NULL", paste0("'", prior.simulation.set$sub.version, "'")), 
                                ") does not match the JHEEM sub-version (",
                                ifelse(is.null(jheem$sub.version), "NULL", paste0("'", jheem$sub.version, "'")), 
                                ")"))
                
                check.sim.can.seed.run(prior.simulation.set = prior.simulation.set,
                                       start.year = start.year,
                                       error.prefix = error.prefix)
            }
            
            # Keep years
            if (!is.numeric(keep.from.year) || length(keep.from.year)!=1 || is.na(keep.from.year))
                stop(paste0(error.prefix, "'keep.from.year' must be a single, non-NA numeric value"))
            
            if (!is.numeric(keep.to.year) || length(keep.to.year)!=1 || is.na(keep.to.year))
                stop(paste0(error.prefix, "'keep.to.year' must be a single, non-NA numeric value"))
            
            if (keep.from.year >= keep.to.year)
                stop(paste0(error.prefix, "'keep.from.year' (", keep.from.year,
                            ") must be PRIOR to 'keep.to.year' (", keep.to.year, ")"))
            
            if (keep.to.year > end.year)
                stop(paste0(error.prefix, "'keep.to.year' (", keep.to.year,
                            ") cannot be AFTER 'end.year' (", end.year, ")"))
            
            
            if (is.null(prior.simulation.set))
            {
                if (keep.from.year < start.year)
                    stop(paste0(error.prefix, "'keep.from.year' (", keep.from.year,
                                ") cannot be BEFORE 'start.year' (", start.year, ")"))
            }
            else
            {
                if (keep.from.year < prior.simulation.set$from.year)
                    stop(paste0(error.prefix, "'keep.from.year' (", keep.from.year,
                                ") cannot be BEFORE the prior.simulation.set's from.year (", prior.simulation.set$from.year, ")"))
            }
            
            
            # Max run time, atol, rtol
            if (is.null(max.run.time.seconds))
                max.run.time.seconds = Inf
            else if (!is.numeric(max.run.time.seconds) || length(max.run.time.seconds)!=1 || is.na(max.run.time.seconds) || max.run.time.seconds<=0)
                stop(paste0(error.prefix, "'max.run.time.seconds' must be a single, non-NA, positive numeric value"))
            
            if (is.null(atol))
                atol = DEFAULT.ATOL
            else if (!is.numeric(atol) || length(atol)!=1 || is.na(atol) || atol<=0)
                stop(paste0(error.prefix, "'atol' must be a single, non-NA, positive numeric value"))
            
            if (is.null(rtol))
                rtol = DEFAULT.RTOL
            else if (!is.numeric(rtol) || length(rtol)!=1 || is.na(rtol) || rtol<=0 || rtol>=1)
                stop(paste0(error.prefix, "'rtol' must be a single, non-NA numeric value between 0 and 1"))
            
            # Foregrounds
            if (!is.null(foregrounds))
            {
                if (is(foregrounds, 'jheem.model.foreground'))
                    foregrounds = list(foregrounds)
                else if (!is.list(foregrounds))
                    stop(paste0(error.prefix, "If 'foregrounds' is not NULL, it must be a list of jheem.model.foreground objects"))
                else if (any(!sapply(foregrounds, is, 'jheem.model.foreground')))
                    stop(paste0(error.prefix, "If 'foregrounds' is not NULL, it must be a list of jheem.model.foreground objects"))
                
                for (frgd in foregrounds)
                    jheem$set.quantity.foreground(foreground, check.consistency = T)
            }
            
            # intervention.code
            if (!is.null(intervention.code))
            {
                if (!is.character(intervention.code) || length(intervention.code)!=1 || is.na(intervention.code))
                        stop(paste0(error.prefix, "'intervention.code' must be a single, non-NA character value"))
                    
                if (is.null(get.intervention(intervention.code, throw.error.if.missing=F)))
                    stop(paste0(error.prefix, "No intervention with code '", intervention.code, "' has been registered. You must register the intervention before giving its code to be run"))
            }
            
            # Calibration.code
            if (!is.null(calibration.code))
            {
                if (!is.character(calibration.code) || length(calibration.code)!=1 || is.na(calibration.code))
                    stop(paste0(error.prefix, "If it is not NULL, 'calibration.code' must be a single, non-NA character value"))
            }
            
            #-- Store Values --#
            private$i.jheem = jheem
            private$i.intervention.code = intervention.code
            private$i.calibration.code = calibration.code
            
            private$i.start.year = start.year
            private$i.end.year = end.year
            private$i.max.run.time.seconds = max.run.time.seconds
            private$i.keep.from.year = keep.from.year
            private$i.keep.to.year = keep.to.year
            private$i.atol = atol
            private$i.rtol = rtol
            
            private$i.check.consistency = T
        },
        
        run = function(parameters=NULL, prior.sim.index=NULL)
        {
            private$prepare.to.run.or.crunch(parameters = parameters,
                                             prior.sim.index = prior.sim.index,
                                             error.prefix = 'Cannot run JHEEM Engine: ')
            
            private$i.jheem$run(start.year = private$i.start.year,
                                end.year = private$i.end.year,
                                check.consistency = private$i.check.consistency,
                                max.run.time.seconds = private$i.max.run.time.seconds,
                                prior.simulation.set = private$i.prior.simulation.set,
                                prior.sim.index = prior.sim.index,
                                keep.from.year = private$i.keep.from.year,
                                keep.to.year = private$i.keep.to.year,
                                atol = private$i.atol,
                                rtol = private$i.rtol)
        },
        
        crunch = function(parameters=NULL, prior.sim.index=NULL)
        {
            private$prepare.to.run.or.crunch(parameters = parameters,
                                             prior.sim.index = prior.sim.index,
                                             error.prefix = 'Cannot crunch JHEEM Engine: ')
            
            private$i.jheem$crunch(start.year = private$i.start.year,
                                   end.year = private$i.end.year,
                                   prior.simulation.set = private$i.prior.simulation.set,
                                   prior.sim.index = prior.sim.index,
                                   check.consistency = private$i.check.consistency)
            
            private$i.check.consistency = F
        },
        
        test = function()
        {
            private$i.jheem$test()
        },
        
        spawn = function(start.year = self$start.year,
                         end.year = self$to.year,
                         prior.simulation.set = NULL,
                         max.run.time.seconds = self$max.run.time.seconds,
                         keep.from.year = self$keep.to.year,
                         keep.to.year = self$keep.from.year,
                         foregrounds = NULL,
                         atol = NULL,
                         rtol = NULL,
                         intervention.code = self$intervention.code,
                         calibration.code = self$calibration.code,
                         error.prefix = "Cannot create copy of JHEEM Engine: ")
        {
            if (!is.character(error.prefix) || length(error.prefix)!=1 || is.na(error.prefix))
                stop(paste0("Error in jheem.engine$spawn() - 'error.prefix' must be a single, non-NA character vector"))
            
            if (is.null(prior.simulation.set))
                prior.simulation.set = private$i.prior.simulation.set
            
            if (is.null(atol))
                atol = private$i.atol
            
            if (is.null(rtol))
                rtol = private$i.rtol
            
            if (is.null(max.run.time.seconds))
                max.run.time.seconds = Inf
            
            if (!identical(calibration.code, private$i.calibration.code))
                stop(paste0("Error in jheem.engine$spawn() - cannot change the calibration.code"))
            
            if (!is.null(private$i.intervention.code))
            {
                if (is.null(intervention.code))
                    stop("Error in jheem.engine$spawn() - cannot set no intervention.code when the previous engine had an intervention.code set")
                else if (private$i.intervention.code != intervention.code)
                    stop("Error in jheem.engine$spawn() - cannot change the intervention.code from the one set for the previous engine")
            }
            
            JHEEM.ENGINE$new(jheem = private$i.jheem$clone(deep=T),
                             start.year = start.year,
                             end.year = end.year,
                             prior.simulation.set = prior.simulation.set,
                             max.run.time.seconds = max.run.time.seconds,
                             keep.from.year = keep.from.year,
                             keep.to.year = keep.to.year,
                             foregrounds = foregrounds,
                             atol = atol,
                             rtol = rtol,
                             intervention.code = intervention.code,
                             calibration.code = calibration.code,
                             error.prefix = error.prefix)
        }
    ),
    
    active = list(
        
        start.year = function(value)
        {
            if (missing(value))
                private$i.start.year
            else
                stop("Cannot modify a JHEEM engine's 'start.year' - they are read-only")
        },
        
        end.year = function(value)
        {
            if (missing(value))
                private$i.end.year
            else
                stop("Cannot modify a JHEEM engine's 'end.year' - they are read-only")
        },
        
        keep.from.year = function(value)
        {
            if (missing(value))
                private$i.keep.from.year
            else
                stop("Cannot modify a JHEEM engine's 'keep.from.year' - they are read-only")
        },
        
        keep.to.year = function(value)
        {
            if (missing(value))
                private$i.keep.to.year
            else
                stop("Cannot modify a JHEEM engine's 'keep.to.year' - they are read-only")
        },
        
        max.run.time.seconds = function(value)
        {
            if (missing(value))
                private$i.max.run.time.seconds
            else
                stop("Cannot modify a JHEEM engine's 'max.run.time.seconds' - they are read-only")
        },
        
        parameter.names = function(value)
        {
            if (missing(value))
                private$i.jheem$parameter.names
            else
                stop("Cannot modify a JHEEM engine's 'parameter.names' - they are read-only")
        },
        
        intervention.code = function(value)
        {
            if (missing(value))
                private$i.intervention.code
            else
                stop("Cannot modify a JHEEM engine's 'intervention.code' - it is read-only")
        },
        
        calibration.code = function(value)
        {
            if (missing(value))
                private$i.calibration.code
            else
                stop("Cannot modify a JHEEM engine's 'calibration.code' - it is read-only")
        }
    ),
    
    private = list(
        
        i.jheem = NULL,
        i.intervention.code = NULL,
        i.calibration.code = NULL,
        
        i.start.year = NULL,
        i.end.year = NULL,
        i.max.run.time.seconds = NULL,
        i.keep.from.year = NULL,
        i.keep.to.year = NULL,
        i.atol = NULL,
        i.rtol = NULL,
        
        i.check.consistency = NULL,
        
        set.parameters = function(parameters, error.prefix)
        {
            
            if (!missing(parameters) && !is.null(parameters))
            {
                if (!is.numeric(parameters) || any(is.na(parameters)))
                    stop(paste0(error.prefix, "'parameters' must be a named numeric vector with no NA values"))
                
                if (is.null(names(parameters)))
                    stop(paste0(error.prefix, "'parameters' must be a NAMED numeric vector"))
                
                parameters.to.set = private$i.jheem$parameters
                parameters.to.set[names(parameters)] = parameters
                
                private$i.jheem$set.parameters(parameters.to.set, check.consistency = private$i.check.consistency)
            }
        },
        
        prepare.to.run.or.crunch = function(parameters, prior.sim.index, error.prefix)
        {
            if (is.null(private$i.prior.simulation.set))
            {
                if (!is.null(prior.sim.index))
                    stop(paste0(error.prefix, "Cannot specify 'prior.sim.index' if the JHEEM engine was not built with a prior.simulation.set"))
            }
            else
            {
                if (is.null(prior.sim.index))
                    stop(paste0(error.prefix, "You MUST specify a 'prior.sim.index' if the JHEEM engine was built with a prior.simulation.set"))
                
                if (!is.numeric(prior.sim.index) || length(prior.sim.index)!=1 || is.na(prior.sim.index) || floor(prior.sim.index)!=prior.sim.index)
                    stop(paste0(error.prefix, "'prior.sim.index' must be a single, non-NA, integer value"))
                
                if (prior.sim.index < 1 || prior.sim.index > private$i.prior.simulation.set$n.sim)
                    stop(paste0(error.prefix, "'prior.sim.index' (", prior.sim.index, ") must be a valid index into the prior.simulation.set - i.e. between 1 and ", private$i.prior.simulation.set$n.sim))
                
                parameters = union(parameters, private$i.prior.simulation.set$parameters[,prior.sim.index])
            }
            
            private$set.parameters(parameters, error.prefix=error.prefix)
        },
        
        get.current.code.iteration = function()
        {
            JHEEM.CODE.ITERATION
        }
    )
)

PROTECTED.NUMERIC.VECTOR = R6::R6Class(
    'protected.numeric.vector',
    
    public = list(
        
        initialize = function(values)
        {
            private$i.values = values
            private$i.has.been.accessed = sapply(values, function(val){F})
        },
        
        '[' = function(indices)
        {
            rv = private$i.values[indices]
            
 #           na.mask = is.na(rv)
 #           if (any(na.mask))
 #           {
 #               if (is.logical(indices))
 #               {
 #                   stop("The length (", length(indices), ") of the logical vector used to access the parameter values does not match the length of the parameter values vector (", length(private$i.values))
 #               }
 #               else
 #               {
 #                   invalid.indices = indices[na.mask]
 #                   if (is.character(indices))
 #                   {
 #                       stop(paste0(collapse.with.and("'", invalid.indices, "'"),
 #                                   ifelse(length(invalid.indices)==1, 
 #                                          " is not a valid name for a parameter value",
 #                                          " are not valid names for parameter values")))
 #                   }
 #                   else 
 #                   {
 #                       stop(paste0(ifelse(length(invalid.indices)==1, "Index ", "Indices "),
 #                                   collapse.with.and(invalid.indices),
 #                                   ifelse(length(invalid.indices)==1, " is", " are"),
 #                                   " out of bounds for parameter value vector (length ", length(private$i.values), ")"))
 #                   }
 #               }
 #           }
            
            private$i.has.been.accessed[indices] = T
            rv
        },
        
        '[<-' = function(indices, value)
        {
            stop("Cannot modify parameter values in this protected numeric vector")
        },
        
        length = function()
        {
            length(private$i.values)
        },
        
        names = function()
        {
            names(private$i.values)
        },
        
        c = function(...)
        {
            stop("Cannot concatenate (ie call the c() function) parameter values in this protected numeric vector")
        },
        
        print = function(...)
        {
            base::print(private$i.values)
        }
    ),
    
    active = list(
        
        accessed.elements = function(value)
        {
            if (missing(value))
                names(private$i.has.been.accessed)[private$i.has.been.accessed]
            else
                stop("Cannot modify a JHEEM's 'private$i.has.been.accessed' - they are read-only")
        }
        
    ),
    
    private = list(
        i.values = NULL,
        i.has.been.accessed = NULL
    )
)

'[.protected.numeric.vector' <- function(obj, ...) {obj$'['(...)}
'[<-.protected.numeric.vector' <- function(obj, ...) {obj$'[<-'(...)}
'length.protected.numeric.vector' <- function(obj) {obj$length()}
'names.protected.numeric.vector' <- function(obj) {obj$names()}
'c.protected.numeric.vector' <- function(obj, ...) {obj$c(...)}





##---------------------##
##-- THE JHEEM CLASS --##
##---------------------##

JHEEM = R6::R6Class(
    'jheem',
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
        
        initialize = function(version, sub.version, location, error.prefix = "Cannot create JHEEM Instance: ")
        {
            # Call the superclass constructor
            super$initialize(version = version,
                             sub.version = sub.version,
                             location = location,
                             type = 'jheem',
                             error.prefix = error.prefix)
            
            private$set.up()
        },
        
        
        ##------------------------##
        ##-- MAIN RUN FUNCTIONS --##
        ##------------------------##
        
        crunch = function(start.year,
                          end.year,
                          prior.simulation.set,
                          prior.sim.index,
                          check.consistency = !self$has.been.crunched())
        {
            # If the specification has changed since we last crunched/set-up, reset
            specification = get.specification.for.version(private$i.version)
            if (specification$iteration != self$specification.metadata$specification.iteration)
                private$set.up()
            
            # Set the times
            set.run.years(start.year = start.year,
                          end.year = end.year,
                          error.prefix = paste0("Error preparing JHEEM to run: "))

            # Calculate all required quantity values
            specification = private$get.specification()
            
            if (is.null(prior.simulation.set))
                top.level.quantity.names = specification$top.level.quantity.names
            else
                top.level.quantity.names = specification$top.level.quantity.names.except.initial.population
            
            sapply(top.level.quantity.names, calculate.quantity.value,
                   check.consistency = check.consistency)
            
            # Use the quantity values to set up for diffeqs
            private$i.diffeq.settings = prepare.diffeq.settings(settings = private$i.diffeq.settings,
                                                                quantity.dim.names = private$i.quantity.dim.names,
                                                                quantity.values = private$i.quantity.values,
                                                                quantity.after.values = private$i.quantity.after.values,
                                                                quantity.times = private$i.quantity.value.times,
                                                                quantity.value.applies.mask = private$i.quantity.value.applies.mask,
                                                                quantity.after.value.applies.mask = private$i.quantity.after.value.applies.mask,
                                                                prior.simulation.set = prior.simulation.set,
                                                                prior.sim.index = prior.sim.index,
                                                                check.consistency = check.consistency,
                                                                error.prefix = paste0("Error preparing JHEEM to run (while setting up the diffeq interface): "))
            
            # Set the i.has.been.crunched flag
            private$i.has.been.crunched = T
            
            # Done
            invisible(self)
        },
        
        run = function(start.year,
                       end.year,
                       check.consistency,
                       max.run.time.seconds,
                       prior.simulation.set,
                       prior.sim.index,
                       keep.from.year,
                       keep.to.year,
                       atol,
                       rtol)
        {
            run.start.time = as.numeric(Sys.time())
            
            # Crunch
            self$crunch(start.year = start.year,
                        end.year = end.year,
                        prior.simulation.set = prior.simulation.set,
                        prior.sim.index = prior.sim.index,
                        check.consistency = check.consistency)
            
            # Set prior values
            if (!is.null(prior.simulation.set))
            {
                if (!is(prior.simulation.set, 'jheem.simulation.set'))
                    stop(paste0("Cannot run simulation: 'prior.simulation.set' must be an object of class 'jheem.simulation.set'"))
                
                if (!is.numeric(prior.sim.index) || length(prior.sim.index)!=1 || is.na(prior.sim.index) ||
                    floor(prior.sim.index)!=prior.sim.index)
                    stop(paste0("Cannot run simulation: 'prior.sim.index' must be a single, non-NA, integer value"))
            }
            else
                initial.state = private$i.diffeq.settings$initial_state

            
            terminated.for.time = F
            # Handoff to the Rcpp
            compute.fn = function(x, t){
                if (1==2)
                {
                    args = list(state = x,
                                time = t,
                                settings = private$i.diffeq.settings,
                                quantities_info = private$i.diffeq.settings$quantities.info,
                                quantity_scratch_vector = private$i.diffeq.settings$quantity_scratch_vector,
                                scratch_vector = private$i.diffeq.settings$scratch.vector,
                                natality_info = private$i.diffeq.settings$natality.info,
                                mortality_info = private$i.diffeq.settings$mortality.info,
                                transitions_info = private$i.diffeq.settings$transitions.info,
                                infections_info = private$i.diffeq.settings$infections.info,
                                remission_info = private$i.diffeq.settings$remission.info,
                                fixed_strata_info = private$i.diffeq.settings$fixed.strata.info,
                                population_trackers = private$i.diffeq.settings$population_trackers)
                    
                    save(args, file='R/local_testing/diffeq_test_args.Rdata')
                    
                    stop("saved - quitting for now")
                }
                
                run.time = as.numeric(Sys.time()) - run.start.time
                if (run.time > max.run.time.seconds)
                {} #for now, nothing, but we have to return a degenerate simulation
                
                compute_dx(state = x,
                           time = t,
                           settings = private$i.diffeq.settings,
                           quantities_info = private$i.diffeq.settings$quantities.info,
                           quantity_scratch_vector = private$i.diffeq.settings$quantity_scratch_vector,
                           scratch_vector = private$i.diffeq.settings$scratch.vector,
                           natality_info = private$i.diffeq.settings$natality.info,
                           mortality_info = private$i.diffeq.settings$mortality.info,
                           transitions_info = private$i.diffeq.settings$transitions.info,
                           infections_info = private$i.diffeq.settings$infections.info,
                           remission_info = private$i.diffeq.settings$remission.info,
                           fixed_strata_info = private$i.diffeq.settings$fixed.strata.info,
                           population_trackers = private$i.diffeq.settings$population_trackers)
            }
            
            end.preprocessing.time = as.numeric(Sys.time())
            
            ode.results = odeintr::integrate_sys(sys = compute.fn,
                                                 init = initial.state,
                                                 duration = private$i.run.to.time - private$i.run.from.time + 1, 
                                                 start = private$i.run.from.time,
                                                 atol = atol,
                                                 rtol = rtol)
            
            end.diffeq.time = as.numeric(Sys.time())
            
            ode.results = as.matrix(ode.results) # this cast to matrix is going to make pulling from the results MUCH faster
           
            private$i.outcome.numerators = list()
            private$i.outcome.denominators = list()
            
            # Process the Results
            outcome.numerators.and.denominators = private$prepare.outcomes.for.sim(ode.results,
                                                                                   prior.simulation.set = prior.simulation.set,
                                                                                   prior.sim.index = prior.sim.index,
                                                                                   is.degenerate = terminated.for.time)

            run.end.time = as.numeric(Sys.time())
            
            
            # Make the Simulation Object
            
            if (is.null(private$i.intervention.code))
            {
                if (is.null(private$i.calibration.code))
                    run.label = 'manual_run'
                else
                    run.label = private$i.calibration.code
            }
            else
                run.label = private$i.intervention

            run.metadata = create.single.run.metadata(run.time = run.end.time - run.start.time,
                                                      preprocessing.time = end.preprocessing.time - run.start.time,
                                                      diffeq.time = end.diffeq.time - end.preprocessing.time,
                                                      postprocessing.time = run.end.time - end.diffeq.time,
                                                      n.trials = 1,
                                                      labels = run.label)
            
            sim = create.single.simulation(version = private$i.version,
                                           sub.version = private$i.sub.version,
                                           location = private$i.location,
                                           from.year = private$i.run.from.time,
                                           to.year = private$i.run.to.time-1,
                                           outcome.numerators = outcome.numerators.and.denominators$numerators,
                                           outcome.denominators = outcome.numerators.and.denominators$denominators,
                                           parameters = private$i.parameters,
                                           run.metadata = run.metadata,
                                           intervention.code = private$i.intervention.code,
                                           calibration.code = private$i.calibration.code,
                                           is.degenerate = terminated.for.time)

            
            # Return
            sim
        },
        
        test = function()
        {
            browser()
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
            private$clear.dependent.values(element.name)
            
            # Clear dim.names (only if value's dim.names have changed)
            if (!dim.names.equal(dimnames(value),
                                 dimnames(private$i.element.backgrounds[[element.name]]$value),
                                 match.order.of.dimensions = T, match.order.within.dimensions = T))
                private$clear.dim.names(element.name)
            
            # No need to clear times
            
            
            #-- Set it --#
            private$i.element.backgrounds[[element.name]]$value = value
            
            
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
            private$clear.dependent.values(element.name)
            
            # Clear dim.names
            private$clear.dim.names(element.name)
            
            # No need to clear times
            
            #-- Set it --#
            private$i.element.backgrounds[[element.name]]$functional.form.alphas[[alpha.name]] = 
                set.alpha.main.effect.values(private$i.element.backgrounds[[element.name]]$functional.form.alphas[[alpha.name]],
                                             dimensions = dimensions,
                                             dimension.values = applies.to.dimension.values,
                                             values = values,
                                             check.consistency = check.consistency,
                                             error.prefix = error.prefix)
            
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
            private$clear.dependent.values(element.name)
            
            # Clear dim.names
            private$clear.dim.names(element.name)
            
            # No need to clear times

            #-- Set It --#
            
            private$i.element.backgrounds[[element.name]]$functional.form.alphas[[alpha.name]] = 
                set.alpha.interaction.value(private$i.element.backgrounds[[element.name]]$functional.form.alphas[[alpha.name]],
                                            dimensions = dimensions,
                                            dimension.values = applies.to.dimension.values,
                                            value = value,
                                            check.consistency = check.consistency,
                                            error.prefix = error.prefix)
                    
            #-- Done --#
            invisible(self)
        },
        
        #' Still need to flesh out this interface
        set.quantity.foreground = function(foreground,
                                           check.consistency = !self$has.been.crunched())
        {
            #-- Validation Steps --#
            
            # Make sure this is a foreground object
            if (!is(foreground, 'jheem.model.foreground'))
                stop("Cannot set.quantity.foreground() for JHEEM: 'foreground' must be an object of class 'jheem.model.foreground'")
            quantity.name = foreground$quantity.name
            error.prefix = paste0("Cannot set foreground for quantity '", quantity.name, "': ")
            
            specification = private$get.specification()
            
            #-- Make sure we can apply --#
            check.foreground.can.apply.to.quantity(foreground = foreground,
                                                   specification = specification,
                                                   error.prefix = error.prefix)
                # FYI, this function is defined in compiled_specification.R
            
            #-- Anchor to location/version --#
            if (!foreground$is.anchored)
            {
                foreground = foreground$anchor(location = self$location,
                                               specification.metadata = self$specification.metadata, 
                                               quantity.dim.names = private$i.quantity.max.dim.names[[quantity.name]],
                                               error.prefix = error.prefix)
            }
            else
            {
                if (foreground$location != self$location || foreground$version != self$version)
                    stop(paste0(error.prefix, "Cannot set foreground for quantity '", 
                                quantity.name, "' - the foreground has already been anchored to a (different) location/version: ",
                                "'", foreground$location, "' and version '", foreground$version, "'"))
            }
            
            #-- Generate an ID and store --#
            foreground.id = paste0('frgd', length(private$i.unresolved.foregrounds)+1)
            private$i.unresolved.foregrounds[[foreground.id]] = foreground
            
            #-- Figure out What Parameters this Depends On --#

            # Set up to track dependencies
            depends.on = foreground$depends.on
            updated.dependencies = lapply(private$i.dependent.foreground.ids.for.parameters[depends.on], function(dep.on.ids){
                    c(dep.on.ids, foreground.id)
                })
            private$i.dependent.foreground.ids.for.parameters[depends.on] = updated.dependencies
            
            # Flag these parameters as used in foregrounds
            private$i.parameter.names.for.foregrounds = union(private$i.parameter.names.for.foregrounds, foreground$depends.on)
            
            #-- Set static to false on this quantity and its dependent quantities and outcomes --#
            private$i.quantity.is.static[quantity.name] = F
            private$i.quantity.is.static[specification$get.dependent.quantity.names(quantity.name)] = F
            private$i.outcome.non.cumulative.is.static[specification$get.non.cumulative.dependent.outcome.names(quantity.name)] = F
            
            
            #-- Clear dim.names --#
            private$clear.dim.names(quantity.name)
            
            #-- Resolve it --#
            private$resolve.foreground(foreground.id)
        },

        
        set.parameters = function(parameters, check.consistency)
        {
            error.prefix = paste0("Error setting parameters for JHEEM for '", self$version, "' model in '", self$location, "': ")
            if (!is.numeric(parameters))
                stop(paste0(error.prefix, "'parameters' must be a numeric vector"))
            if (any(is.na(parameters)))
                stop(paste0(error.prefix, "'parameters' cannot contain any NA values"))
            if (is.null(names(parameters)))
                stop(paste0(error.prefix, "'parameters' must be a NAMED numeric vector"))
            
            # overwrite into parameters vector
            private$i.parameters[names(parameters)] = parameters
            
            # Set the values for any elements with matching names
            element.names.in.parameters = intersect(names(parameters), private$get.specification()$element.names)
            element.names.in.parameters = element.names.in.parameters[sapply(private$i.element.backgrounds[element.names.in.parameters], function(bkgd){
                is.null(bkgd$functional.form)
            })]
            
            for (elem.name in element.names.in.parameters)
                self$set.element.value(element.name = elem.name,
                                       value = parameters[elem.name],
                                       check.consistency = check.consistency)
            
            # Call the registered parameter setting function if there is one
            if (check.consistency)
                model.settings = private$i.checked.model.settings
            else
                model.settings = private$i.unchecked.model.settings
            
            used.parameter.names = element.names.in.parameters
            
            # For calibrated parameters
            calibrated.parameters.distribution = get.parameters.distribution.for.version(self$version, type='calibrated')
            if (!is.null(calibrated.parameters.distribution) && any(calibrated.parameters.distribution@var.names[1]==names(parameters)))
            {
                calibrated.parameters.apply.fn = get.parameters.apply.function.for.version(self$version, type='calibrated')
                
                if (check.consistency)
                    parameters.to.pass = PROTECTED.NUMERIC.VECTOR$new(parameters)
                else
                    parameters.to.pass = parameters
                
                calibrated.parameters.apply.fn(model.settings = model.settings,
                                               parameters = parameters.to.pass)
                
                if (check.consistency)
                    used.parameter.names = union(used.parameter.names, parameters.to.pass$accessed.elements)
            }

            # For sampled parameters
            sampled.parameters.distribution = get.parameters.distribution.for.version(self$version, type='sampled')
            if (!is.null(sampled.parameters.distribution) && any(sampled.parameters.distribution@var.names[1]==names(parameters)))
            {
                sampled.parameters.apply.fn = get.parameters.apply.function.for.version(self$version, type='sampled')
                used.from.sampled = sampled.parameters.apply.fn(model.settings = model.settings,
                                                                parameters = parameters,
                                                                track.used.parameters = check.consistency)
                
                if (check.consistency)
                    used.parameter.names = union(used.parameter.names, used.from.sampled)
            }
            
            # Check that there are no missing parameters for foregrounds
            if (check.consistency)
            {
                missing.parameters = setdiff(private$i.parameter.names.for.foregrounds, names(private$i.parameters))
                if (length(missing.parameters)>0)
                    stop(paste0(error.prefix, 
                                ifelse(length(missing.parameters)==1, "A value for parameter ", "Values for parameters "),
                                collapse.with.and("'", missing.parameters, "'"),
                                " - upon which one more more foregrounds depend - ",
                                ifelse(length(missing.parameters)==1, "has", "have"),
                                " not been set to the JHEEM"))
            }
            
            used.parameter.names = union(used.parameter.names, private$i.parameter.names.for.foregrounds)
            
            # Check that we used all the parameters
            if (check.consistency)
            {
                unused.parameters = setdiff(names(parameters), used.parameter.names)
                if (length(unused.parameters)>0)
                {
                    stop(paste0(error.prefix,
                                length(unused.parameters),
                                ifelse(length(unused.parameters)==1, " is", " are"),
                                " present in 'parameters' but ",
                                ifelse(length(unused.parameters)==1, "was", "were"),
                                " not used by the model specification: ",
                                collapse.with.and("'", unused.parameters, "'")
                                ))
                }
            }        
            
            # Re-resolve any dependent foregrounds
            if (length(private$i.unresolved.foregrounds)>0)
            {
                dependent.ids = unlist(private$i.dependent.foreground.ids.for.parameters[names(parameters)])
                re.resolve.mask = sapply(names(private$i.unresolved.foregrounds), function(id){
                    any(id == dependent.ids)
                })
                
                re.resolve.ids = names(private$i.unresolved.foregrounds)[re.resolve.mask]
                for (id in re.resolve.ids)
                    private$resolve.foreground(id)
            }
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
            private$clear.dependent.values(element.name, 
                                           clear.before.time = max(i.element.backgrounds[[element.name]]$functional.form.from.time,
                                                                   previous.from.time))

            # Clear times
            private$clear.element.background.self.times(element.name)
            
            # No need to clear dim.names
            
            
            #-- Set It --#
            private$i.element.backgrounds[[element.name]]$functional.form.from.time = from.time
                        
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
            times.to.clear.values.for.mask = private$i.quantity.value.times[[element.name]] > 
            private$clear.dependent.values(element.name,
                                           clear.after.time = min(i.element.backgrounds[[element.name]]$functional.form.to.time,
                                                                  previous.to.time))
            
            # Clear times
            private$clear.element.background.self.times(element.name)
            
            # No need to clear dim.names
            
            
            #-- Set It --#
            private$i.element.backgrounds[[element.name]]$functional.form.to.time = to.time
            
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
            private$clear.dependent.values(element.name, 
                                           clear.before.time = i.element.backgrounds[[element.name]]$functional.form.from.time)

            # Clear times
            private$clear.element.background.self.times(element.name)
            
            # No need to clear dim.names
            
            
            #-- Set the Value --#
            private$i.element.backgrounds[[element.name]]$ramp.times = new.times
            
            
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
            private$clear.dependent.values(element.name, 
                                           clear.before.time = i.element.backgrounds[[element.name]]$functional.form.from.time)
            
            # No need to clear times
            # No need to clear dim.names
            
            
            #-- Set the Values --#
            private$i.element.backgrounds[[element.name]]$ramp.values[indices] = values
            
            
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
            private$clear.dependent.values(element.name,
                                           clear.after.time = i.element.backgrounds[[element.name]]$functional.form.to.time)
            
            # Clear times
            private$clear.element.background.self.times(element.name)
            
            # No need to clear dim.names
            
            
            #-- Set the Value --#
            private$i.element.backgrounds[[element.name]]$taper.times = new.times
            
            
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
            private$clear.dependent.values(element.name, 
                                           clear.after.time = i.element.backgrounds[[element.name]]$functional.form.to.time)
            
            # No need to clear times
            # No need to clear dim.names
            
            
            #-- Set the Value --#
            private$i.element.backgrounds[[element.name]]$taper.values[indices] = values
            
            
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
            private$clear.dependent.values(element.name,
                                           clear.after.time = i.element.backgrounds[[element.name]]$future.slope.after.time)
            
            # No need to clear times
            # No need to clear dim.names
            
            
            #-- Set the Value --#
            for (element.name in element.names)
                private$i.element.backgrounds[[element.name]][['future.slope']] = slope
            
            
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
            private$clear.dependent.values(element.name, 
                                           clear.after.time = min(i.element.backgrounds[[element.name]]$future.slope.after.time,
                                                                  previous.future.slope.after.time))

            # No need to clear times
            # No need to clear dim.names
            
            #-- Set the Value --#
            for (element.name in element.names)
                private$i.element.backgrounds[[element.name]]$future.slope.after.year = after.year
            
            
            #-- Done --#
            invisible(self)
        },
    
        # Does NOT actually do anything with the intervention
        # Just stores it to the jheem object
        set.intervention.code = function(intervention.code)
        {
            if (!is.null(intervention.code))
            {
                if (!is.character(intervention.code) || length(intervention)!=1 || is.na(intervention))
                    stop("Cannot set intervention for JHEEM: 'intervention.code' must be a single, non-NA character value")
                
                if (is.null(get.intervention(intervention.code, throw.error.if.missing=F)))
                    stop(paste0("Cannot set intervention.code '", intervention.code, "' for JHEEM: no intervention with that code has been registered"))
            }

            private$i.intervention.code = intervention.code
        },
        
        set.calibration.code = function(calibration.code)
        {
            if (!is.null(calibration.code))
            {
                if (!is.character(calibration.code) || length(calibration.code)!=1 || is.na(calibration.code))
                    stop("Cannot set calibration code for JHEEM: 'calibration.code' must be a single, non-NA character value")
                
                if (is.null(get.calibration.info(calibration.code, throw.error.if.missing=F)))
                    stop(paste0("Cannot set calibration code '", calibration.code, "' for JHEEM: a calibration must have been registered to this code with register.calibration.info()"))
            }
            
            private$i.calibration.code = calibration.code
        },

        has.been.crunched = function()
        {
            private$i.has.been.crunched
        }
    ),
    
    active = list(
        
        element.names = function(value)
        {
            if (missing(value))
                names(private$i.element.backgrounds)
            else
                stop("Cannot modify a JHEEM's 'element.names' - they are read-only")
        },
        
        # FOR DEBUGGING
        calculated.values = function(value)
        {
            if (missing(value))
                private$i.quantity.values
            else
                stop("Cannot modify a JHEEM's 'calculated.values' - they are read-only")
        },
        
        value.times = function(value)
        {
            if (missing(value))
                private$i.quantity.value.times
            else
                stop("Cannot modify a JHEEM's 'value.times' - they are read-only")
        },
        
        element.backgrounds = function(value)
        {
            if (missing(value))
                private$i.element.backgrounds
            else
                stop("Cannot modify a JHEEM's 'element.backgrounds' - they are read-only")
        },
        
        parameter.names = function(value)
        {
            if (missing(value))
                names(private$i.parameters)
            else
                stop("Cannot modify a JHEEM engine's 'parameter.names' - they are read-only")
        }
        
    ),
    
    private = list(
        
        ##----------------------##
        ##----------------------##
        ##-- MEMBER VARIABLES --##
        ##----------------------##
        ##----------------------##
        
        #-- Element names/backgrounds --#
        i.element.names = NULL,
        i.element.backgrounds = NULL,

        #-- Times for quantities --#
        i.quantity.value.times = NULL,
        i.top.level.self.times = NULL,
        
        i.element.background.self.times = NULL,
        i.quantity.foreground.self.times = NULL,
        i.quantity.self.after.times = NULL,
        
        i.quantity.value.all.applies.for.time = NULL,
        i.top.level.value.may.not.apply.times = NULL,
        
        #-- Times for Outcomes --#
        i.outcome.non.cumulative.value.times = NULL,
        i.outcome.non.cumulative.value.time.is.after.time = NULL,
        i.outcome.value.may.not.apply.non.cumulative.times = NULL,
        
        i.outcome.non.cumulative.self.times = NULL,
        i.outcome.non.cumulative.self.after.times = NULL,
        
        i.outcome.value.times = NULL,
        i.outcome.value.time.is.after.time = NULL,
        
        #-- Values for quantities --#
        i.quantity.values = NULL,
        i.quantity.after.values = NULL,
        
        #-- Dim Names (and related) for quantities --#
        i.quantity.dim.names = NULL,
        i.crunched.quantity.dim.names = NULL,
        i.quantity.component.dim.names = NULL,
        i.crunched.quantity.component.dim.names = NULL,
        
        i.quantity.max.dim.names = NULL,
        i.quantity.required.dim.names = NULL,
        
        i.quantity.component.max.dim.names = NULL,
        i.quantity.component.applies.to = NULL,
        
        #-- Dim Names for Outcomes --#
        i.outcome.dim.names.sans.time = NULL,
        i.outcome.numerator.dim.names.sans.time = NULL,
        
        i.outcome.indices = NULL,
        
        #-- Masks for whether values apply --#
        i.quantity.value.applies.mask = NULL,
        i.quantity.after.value.applies.mask = NULL,
        
        #-- Indices for quantities --#
        i.quantity.component.depends.on.mapping.indices = NULL,
        i.quantity.mapping.indices = NULL,
        i.quantity.foreground.effect.indices = NULL,

        #-- Whether quantities are static --#
        i.quantity.is.static = NULL,
        i.outcome.non.cumulative.is.static = NULL,
        
        #-- Parameters --#
        i.parameters = NULL,
        i.parameter.names.for.foregrounds = NULL,
        i.dependent.foreground.ids.for.parameters = NULL, # A list of character vectors; names are parameters names, elements are vectors of foreground ids
        
        #-- Foregrounds --#
        i.unresolved.foregrounds = NULL, #list, indexed by foreground.id
        i.resolved.foregrounds = NULL, #list of lists, indexed [[quantity.name]][[foreground.id]]
        
        #-- Outcome Values/Denominators --#
        i.outcome.numerators = NULL,
        i.outcome.denominators = NULL,
        
        #-- On Crunched state (and the times to run from/to))
        i.run.from.time = NULL,
        i.run.to.time = NULL,
        
        i.has.been.crunched = NULL,
        
        #-- Diffeq Settings --#
        i.diffeq.settings = NULL,
        
        #-- Intervention/Calibration Settings --#
        i.intervention.code = NULL,
        i.calibration.code = NULL,
        
        #-- The Model Settings to Pass Along --#
        i.checked.model.settings = NULL,
        i.unchecked.model.settings = NULL,
        
        ##---------------------##
        ##---------------------##
        ##-- PRIVATE METHODS --##
        ##---------------------##
        ##---------------------##
        
        get.current.code.iteration = function()
        {
            JHEEM.CODE.ITERATION
        },
        
        ##------------##
        ##-- SET UP --##
        ##------------##
        
        set.up = function()
        {
            private$i.checked.model.settings = JHEEM.MODEL.SETTINGS$new(self, check.consistency = T)
            private$i.unchecked.model.settings = JHEEM.MODEL.SETTINGS$new(self, check.consistency = F)
            
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
                                                   error.prefix = paste0("Error creating JHEEM for version '", private$i.version, "' and location '", private$i.location, "': "))
                
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
            
            # Figure out if outcomes' non-cumulative parts are static
            private$i.outcome.non.cumulative.is.static = logical()
            sapply(specification$outcome.names, 
                   private$calculate.outcome.non.cumulative.is.static,
                   specification = specification)
            
            # Set up outcome dim.names
            private$i.outcome.dim.names.sans.time = lapply(specification$outcome.names, function(outcome.name){
                outcome = specification$get.outcome(outcome.name)
                specification.metadata$apply.aliases(outcome$dim.names, error.prefix=paste0("Error setting up dim.names for outcome '", outcome$name, "': "))
            })
            names(private$i.outcome.dim.names.sans.time) = specification$outcome.names
            
            # Set up dim.names list holders
            private$i.quantity.dim.names = list()
            private$i.crunched.quantity.dim.names = list()
            private$i.quantity.component.dim.names = list()
            private$i.crunched.quantity.component.dim.names = list()
            
            # Set up values and times lists
            private$i.quantity.values = list()
            private$i.quantity.after.values = list()
            
            private$i.quantity.value.times = list()
            private$i.top.level.self.times = list()
            
            private$i.element.background.self.times = list()
            private$i.quantity.foreground.self.times = list()
            
            
            private$i.outcome.non.cumulative.value.times = list()
            private$i.outcome.non.cumulative.value.time.is.after.time = list()
            private$i.outcome.value.may.not.apply.non.cumulative.times = list()
            
            private$i.outcome.non.cumulative.self.times = list()
            private$i.outcome.non.cumulative.self.after.times = list()
            
            private$i.outcome.value.times = list()
            private$i.outcome.value.time.is.after.time = list()
            
            
            private$i.quantity.value.all.applies.for.time = list()
            private$i.top.level.value.may.not.apply.times = list()
            
            private$i.quantity.value.applies.mask = list()
            private$i.quantity.after.value.applies.mask = list()
            
            
            
            # Import the default parameters
            private$i.parameters = numeric()
            private$i.parameters[names(specification$default.parameter.values)] = specification$default.parameter.values

            private$i.parameter.names.for.foregrounds = numeric()
            
            # Import the foregrounds
            for (frgd in specification$foregrounds)
            {
                self$set.quantity.foreground(foreground = frgd,
                                             check.consistency = T)
            }
            
            # Set up the diffeq settings
            private$i.diffeq.settings = create.diffeq.settings(jheem = self,
                                                               error.prefix = paste0("Error creating diffeq settings for JHEEM for version '", private$i.version, "' and location '", private$i.location, "': "))
            
            # Clear the i.has.been.crunched flag
            private$i.has.been.crunched = F
            
            # Re-process any instructions
            for (instr in private$i.instructions)
                private$execute.instruction(instr)
            
            # Going to need to do something about foregrounds here
        },
        
        calculate.outcome.non.cumulative.is.static = function(outcome.name,
                                                              specification)
        {
            if (is.na(private$i.outcome.non.cumulative.is.static[outcome.name]))
            {
                outcome = specification$get.outcome(outcome.name)
                
                if (is(outcome, 'intrinsic.model.outcome') || !all(private$i.quantity.is.static[specification$get.outcome.dependee.element.names(outcome.name)]))
                    private$i.outcome.non.cumulative.is.static[outcome.name] = F
                else
                {
                    depends.on.outcome.names = setdiff(specification$get.outcome.non.cumulative.dependendee.outcome.names(outcome.name),
                                                       outcome.name)
                    
                    sapply(depends.on.outcome.names, 
                           private$calculate.outcome.non.cumulative.is.static,
                           specification = specification)
                    
                    private$i.outcome.non.cumulative.is.static[outcome.name] =
                        all(private$i.outcome.non.cumulative.is.static[depends.on.outcome.names])
                }
            }
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
                private$i.run.from.time != start.year || private$i.run.to.time != (end.year+1))
            {
                private$i.run.from.time = start.year
                private$i.run.to.time = end.year+1
            
                #-- Clear times (for all elements/quantities) --#
                specification = private$get.specification()
                
                private$clear.element.background.self.times(specification$element.names)
                private$clear.quantity.foreground.self.times(specification$quantity.names)
                private$clear.outcome.value.times(specification$outcome.nam)
            }
            
            #-- Done --#
            invisible(self)
        },
        
        ##----------------------------------------##
        ##-- CALCULATING QUANTITY/ELEMENT TIMES --##
        ##----------------------------------------##
        
        # Broadly speaking, "self" times are the times for which a quantity needs to produce values
        #   to capture it's time-varying changes. There are background and potentially foreground self times
        #
        # We manage a number of main time vectors:
        #
        # 1) We need *quantity.value.times* for each quantity actually calculating values
        #    The value.times for a quantity is the union of all the *top.level.self.times* for any
        #       top.level.quantity that depends on this quantity (including itself),
        #       and *outcome.non.cumulative.self.times* for any outcome that depends on this quantity
        # 
        # 2) The *top.level.self.times* for a top.level.quantity is the union of:
        #    - All *element.background.self.times* for any element that the quantity depends on
        #    - All *quantity.foreground.self.times* for any quantity that the top.level.quantity depends on
        #
        # 3) The *outcome.non.cumulative.self.times* for an outcome is the union of
        #    - All *element.background.self.times* for any element that this outcome has a non-cumulative dependency on
        #    - All *quantity.foreground.self.times* for any quantity that this outcome has a non-cumulative dependency on
        #    - All model run times for any intrinsic outcomes that this outcome has a non-cumulative dependency on
        #
        # 4) The *element.background.self.times* for each element is calculated from the element's internal info. The times are:
        #    - 'all' if this is a static value with no foreground set
        #    - The first foreground start time if this is a static value with a foreground set
        #    - If not static, the union of:  ramp and taper times, functional form times (within the bounds of the run time)
        # 
        # 5) The *quantity.foreground.self.times* for each quantity is calculate from the quantity's set foregrounds
        # 5b) The *quantity.self.after.times* vector
        #          The set of times for which this quantity's foreground implies there is a separate value and after.value
        #
        # 6) We need *quantity.value.all.apply.times* for each quantity for efficiently calculating
        #    quantity.value.applies.mask
        #    The quantity.value.all.apply.times for a quantity is the setdiff of
        #    - The quantity's *value.times* minus
        #    - The union of *top.level.value.may.not.apply.times* for every top.level.quantity that depends on this quantity
        #       plus the union of *outcome.value.may.not.apply.non.cumulative.times* for every outcome that has a non-cumulative dependency on this quantity
        #    * (for speed, we don't actually store the set of times, we store a mask that is T for all value.all.apply)
        #       
        # 7) The *top.level.value.may.not.apply.times* for a top.level.quantity is the setdiff of
        #    - The top.level.quantity's *value.times* minus
        #    - The union of *element.background.self.times* for all elements that the quantity depends on
        #
        # 8) The *outcome.value.may.not.apply.non.cumulative.times* is the setdiff of
        #    - The outcomes *non.cumulative.value.times* minus
        #    - The union of *element.background.self.times* for all elements that the outcome has a non-cumulative dependency on
        #  
        # 9) The *outcome.non.cumulative.value.times* for each outcome is the union of all
        #       *outcome.non.cumulative.self.times* for any outcome that depends on this outcome (including itself)
        #
        # 10) The *outcome.value.times* is a vector of times for which this value is calculated
        #       For a cumulative outcome, it is just every year between max(run.from.time, and the outcome's from.time) and min(run.to.time, and the outcome's to.time)
        #       For a non-cumulative outcome, it is the *outcome.non.cumulative.value.times*
        #           Any times for which there is both a value and an after.value will appear in twice in the vector of times
        #
        # 11) The *outcome.value.time.is.after* is a logical indicator corresponding to the *outcome.value.times* for an outcome
        #       We store this as a logical vector for computational purposes, but
        #       the set of after.times for an outcome is the union of all *quantity.self.after.times* for any quantity that this value depends on
        #
        # 
        #
        #
        # NB: (For all this to work, we define a quantity as depending on itself)
        
        
        
        #-- Quantity Value Times --#
        # Broadly speaking, "value" times are the times for which a quantity needs to produce values
        #   EITHER for it's own time-varying changes OR to merge with other quantities in a higher-level
        #   quantity that depends on it and other quantities
        
        #    The value.times for a quantity is the union of all the *top.level.self.times* for any
        #       top.level.quantity that depends on this quantity (including itself),
        #       and *outcome.non.cumulative.self.times* for any outcome that depends on this quantity
        calculate.quantity.value.times = function(quantity.name)
        {
            if (private$i.quantity.is.static[quantity.name])
                private$i.quantity.value.times[[quantity.name]] = 'all'
            else
            {
                specification = private$get.specification()
                
                # Pull the top-level quantities that depend on this, and make sure their self-times are calculated
                top.level.quantities = specification$get.dependent.top.level.quantity.names(quantity.name)
                dynamic.top.level.quantities = top.level.quantities[ !private$i.quantity.is.static[top.level.quantities] ]
                null.top.level.times.mask = as.logical(sapply(private$i.top.level.self.times[dynamic.top.level.quantities], is.null))
                sapply(dynamic.top.level.quantities[null.top.level.times.mask], private$calculate.top.level.self.times)
                
                # Pull the outcomes that have a non-cumulative dependency on this, and make sure their self-times are calculated
                outcomes = specification$get.non.cumulative.dependent.outcome.names(quantity.name)
                dynamic.outcomes = outcomes[ !private$i.outcome.non.cumulative.is.static[outcomes] ]
                null.outcome.times.mask = as.logical(sapply(private$i.outcome.non.cumulative.self.times[dynamic.outcomes], is.null))
                sapply(dynamic.outcomes[null.outcome.times.mask], private$calculate.outcome.non.cumulative.self.times)
                
                # Value times is the union of these times
                private$i.quantity.value.times[[quantity.name]] = union_sorted_vectors(c(private$i.top.level.self.times[dynamic.top.level.quantities],
                                                                                         private$i.outcome.non.cumulative.self.times[dynamic.outcomes]))
            }
            
            invisible(self)
        },

        clear.quantity.value.times = function(quantity.names)
        {
            # Clear the times
            private$i.quantity.value.times[quantity.names] = NULL
            
            # Clear the value-all-applies times
            private$clear.quantity.value.all.applies.times(quantity.names)
            
            # Done
            invisible(self)
        },
        
        
        #-- Element Background Self-Times --#
        
        calculate.element.background.self.times = function(element.name)
        {
            if (private$i.quantity.is.static[element.name])
                private$i.element.background.self.times[[element.name]] = 'all'
            else
            {
                specification = get.specification()
                element = specification$get.quantity(element.name)
                
                if (private$i.element.backgrounds[[element.name]]$is.static)
                    private$i.element.background.self.times[[element.name]] = 
                        private$i.element.self.background.times[[element.name]] = numeric() #we'll let the foreground overwrite things
                else
                {
                    if (is.null(private$i.element.backgrounds[[element.name]]$ramp.interpolated.times))
                        private$i.element.backgrounds[[element.name]] = calculate.ramp.interpolated.times(private$i.element.backgrounds[[element.name]])
                    if (is.null(private$i.element.backgrounds[[element.name]]$functional.form.times))
                        private$i.element.backgrounds[[element.name]] = calculate.functional.form.times(private$i.element.backgrounds[[element.name]])
                    if (is.null(private$i.element.backgrounds[[element.name]]$taper.interpolated.times))
                        private$i.element.backgrounds[[element.name]] = calculate.taper.interpolated.times(private$i.element.backgrounds[[element.name]])
                    
                    bkgd = private$i.element.backgrounds[[element.name]]
                    private$i.element.background.self.times[[element.name]] = 
                        c(bkgd$ramp.interpolated.times,
                          bkgd$functional.form.times,
                          bkgd$taper.interpolated.times)
                }
            }
            
            # Done
            invisible(self)
        },
        
        clear.element.background.self.times = function(element.names)
        {
            # Clear the times
            private$i.element.background.self.times[element.names] = NULL
            
            # Clear top-level self-times for top-level quantities that depend on this element
            # Clear top-level value-may-not-apply times for top-level quantities that depend on this element
            specification = get.specification()
            
            top.level.quantities = specification$get.dependent.top.level.quantity.names(element.names)
            dynamic.top.level.quantities = top.level.quantities[ !private$i.quantity.is.static[top.level.quantities] ]
            private$clear.top.level.self.times(dynamic.top.level.quantities)
            private$clear.top.level.value.may.not.apply.times(dynamic.top.level.quantities)
            
            # Clear outcome non-cumulative self-times for outcomes that have a non-cumulative dependency on this element
            # Clear outcome non-cumulative value-may-not-apply times for outcomes that have a non-cumulative dependency on this element
            outcomes = specification$get.non.cumulative.dependent.outcome.names(element.names)
            dynamic.outcomes = outcomes[ !private$i.outcome.non.cumulative.is.static[outcomes] ]
            private$clear.outcome.non.cumulative.self.times(dynamic.outcomes)
            private$clear.outcome.value.may.not.apply.non.cumulative.times(dynamic.outcomes)
            
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
        
        
        #-- Quantity Foreground Self Times --#
        
        calculate.quantity.foreground.self.times = function(quantity.name)
        {
            if (is.null(private$i.resolved.foregrounds[[quantity.name]]))
            {
                private$i.quantity.foreground.self.times[[quantity.name]] = numeric()
                private$i.quantity.self.after.times[[quantity.name]] = numeric()
            }
            else
            {
                times.from.foregrounds = lapply(private$i.resolved.foregrounds[[quantity.name]], function(frgd){
                    frgd.times = frgd$all.effect.times[frgd$all.effect.times>=private$i.run.from.time & frgd$all.effect.times<=private$i.run.to.time]
                    
                    # Make sure we have a start time to interpolate from if start to first spans the start time for a 'value' effect type
                    required.start.times = frgd$start.time.by.effect[frgd$scale.by.effect=='value' & 
                                                                         frgd$min.effect.time.by.effect > private$i.run.from.time &
                                                                         frgd$start.time.by.effect < private$i.run.from.time] # if == on this line, would already be in frgd.times above
                    required.start.times[is.infinite(required.start.times)] = private$i.run.from.time
                    
                    
                    # Similarly, make sure we have an end time to interpolate to if last to end spans the end time for a 'value' effect type
                    required.end.times = frgd$end.time.by.effect[frgd$scale.by.effect=='value' &
                                                                     frgd$max.effect.time.by.effect < private$i.run.to.time &
                                                                     frgd$end.time.by.effect > private$i.run.to.time]# if == on this line, would already be in frgd.times above
                    required.end.times[is.infinite(required.end.times)] = private$i.run.to.time
                    
                    if (length(required.start.times)>0 || length(required.end.times)>0)
                        frgd.times = union_sorted_vectors(c(list(frgd.times, 
                                                                 as.list(required.start.times),
                                                                 as.list(required.end.times))))
                    
                    frgd.times
                })
                
                if (length(times.from.foregrounds)==1)
                    private$i.quantity.foreground.self.times[[quantity.name]] = times.from.foregrounds[[1]]
                else
                    private$i.quantity.foreground.self.times[[quantity.name]] = union_sorted_vectors(times.from.foregrounds)
                
                step.change.times = lapply(private$i.resolved.foregrounds[[quantity.name]], function(frgd){
                    frgd$all.effect.step.change.times
                })
                
                step.change.times = union_sorted_vectors(step.change.times);
                private$i.quantity.self.after.times[[quantity.name]] =
                    intersect_sorted_vectors(list(step.change.times,
                                                  private$i.quantity.foreground.self.times[[quantity.name]]))
            }
            
            invisible(self)
        },
        
        clear.quantity.foreground.self.times = function(quantity.names)
        {
            # Clear the times
            private$i.quantity.foreground.self.times[quantity.names] = NULL
            private$i.quantity.self.after.times[[quantity.names]] = NULL
                
            # Clear the top-level-quantity self-times for top-level quantities that depend on this one
            specification = get.specification()
            
            top.level.quantities = specification$get.dependent.top.level.quantity.names(quantity.names)
            dynamic.top.level.quantities = top.level.quantities[ !private$i.quantity.is.static[top.level.quantities] ]
            private$clear.top.level.self.times(dynamic.top.level.quantities)
            private$clear.top.level.value.may.not.apply.times(dynamic.top.level.quantities)
            
            # Clear outcome non-cumulative self-times for outcomes that have a non-cumulative dependency on this quantity
            # Clear outcome non-cumulative value-may-not-apply times for outcomes that have a non-cumulative dependency on this quantity
            outcomes = specification$get.non.cumulative.dependent.outcome.names(quantity.names)
            dynamic.outcomes = outcomes[ !private$i.outcome.non.cumulative.is.static[outcomes] ]
            private$clear.outcome.non.cumulative.self.times(dynamic.outcomes)
            private$clear.outcome.value.may.not.apply.non.cumulative.times(dynamic.outcomes)
            
            # Done
            invisible(self)
        },
        
        
        #-- Top-Level Self Times --#
        
        # The *top.level.self.times* for a top.level.quantity is the union of:
        #   All *element.background.self.times* for any element that the quantity depends on
        #   All *quantity.foreground.self.times* for any quantity that the top.level.quantity depends on
        calculate.top.level.self.times = function(top.level.name)
        {
            if (private$i.quantity.is.static[top.level.name])
                private$i.top.level.self.times[[top.level.name]] = 'all'
            else
            {
                specification = private$get.specification()
                
                # Pull the elements this depends on, and make sure their background self times are calculated
                dependee.elements = specification$get.dependee.element.names(top.level.name)
                dynamic.dependee.elements = dependee.elements[ !private$i.quantity.is.static[dependee.elements] ]
                null.dependee.element.times.mask = as.logical(sapply(private$i.element.background.self.times[dynamic.dependee.elements], is.null))
                sapply(dynamic.dependee.elements[null.dependee.element.times.mask], private$calculate.element.background.self.times)
                
                # Pull the quantities this depends on, and make sure their foreground self times are calculated
                dependee.quantities = specification$get.dependee.quantity.names(top.level.name)
                dynamic.dependee.quantities = dependee.quantities[ !private$i.quantity.is.static[dependee.quantities] ]
                null.dependee.quantity.times.mask = as.logical(sapply(private$i.quantity.foreground.self.times[dynamic.dependee.quantities], is.null))
                sapply(dynamic.dependee.quantities[null.dependee.quantity.times.mask], private$calculate.quantity.foreground.self.times)
                
                # top.level.self.times is the union of these times
                private$i.top.level.self.times[[top.level.name]] = union_sorted_vectors(c(private$i.element.background.self.times[dynamic.dependee.elements],
                                                                                          private$i.quantity.foreground.self.times[dynamic.dependee.quantities]))
            }
            
            invisible(self)
        },
        
        clear.top.level.self.times = function(top.level.names)
        {
            # Clear the times
            private$i.top.level.self.times[top.level.names] = NULL
            
            # Clear value.times for dependee.quantities
            specification = get.specification()
            dependee.quantities = specification$get.dependee.quantity.names(top.level.names)
            dynamic.dependee.quantities = dependee.quantities[ !private$i.quantity.is.static[dependee.quantities] ]
            private$clear.quantity.value.times(dynamic.dependee.quantities)
            
            # Done
            invisible(self)
        },
        
        #-- Outcome Non-Cumulative Self Times --# 
        # The *outcome.non.cumulative.self.times* for an outcome is the union of
        #    - All *element.background.self.times* for any element that this outcome has a non-cumulative dependency on
        #    - All *quantity.foreground.self.times* for any quantity that this outcome has a non-cumulative dependency on
        #    - All model run times for any intrinsic outcomes that this outcome has a non-cumulative dependency on
        
        calculate.outcome.non.cumulative.self.times = function(outcome.name)
        {
            if (private$i.outcome.non.cumulative.is.static[outcome.name])
            {
                private$i.outcome.non.cumulative.self.times[[outcome.name]] = 'all'
                private$i.outcome.non.cumulative.self.after.times[[outcome.name]] = numeric()
            }
            else
            {
                specification = private$get.specification()
                
                # Pull the elements this depends on, and make sure their background self times are calculated
                dependee.elements = specification$get.outcome.dependee.element.names(outcome.name)
                dynamic.dependee.elements = dependee.elements[ !private$i.quantity.is.static[dependee.elements] ]
                null.dependee.element.times.mask = as.logical(sapply(private$i.element.background.self.times[dynamic.dependee.elements], is.null))
                sapply(dynamic.dependee.elements[null.dependee.element.times.mask], private$calculate.element.background.self.times)
                
                # Pull the quantities this depends on, and make sure their foreground self times are calculated
                dependee.quantities = specification$get.outcome.dependee.quantity.names(outcome.name)
                dynamic.dependee.quantities = dependee.quantities[ !private$i.quantity.is.static[dependee.quantities] ]
                null.dependee.quantity.times.mask = as.logical(sapply(private$i.quantity.foreground.self.times[dynamic.dependee.quantities], is.null))
                sapply(dynamic.dependee.quantities[null.dependee.quantity.times.mask], private$calculate.quantity.foreground.self.times)
                
                # If this has a non-cumulative dependency on any intrinsic outcome, include all the run times
                dependee.outcomes = specification$get.outcome.non.cumulative.dependendee.outcome.names(outcome.name)
                dependee.outcomes.are.intrinsic = sapply(dependee.outcomes, function(dep.on.outcome.name){
                    is(specification$get.outcome(dep.on.outcome.name), 'intrinsic.model.outcome')
                })
                if (any(dependee.outcomes.are.intrinsic))
                    intrinsic.times = private$i.run.from.time:private$i.run.to.time
                else
                    intrinsic.times = NULL
                
                # outcome.non.cumulative.self.times is the union of these times
                private$i.outcome.non.cumulative.self.times[[outcome.name]] = union_sorted_vectors(c(private$i.element.background.self.times[dynamic.dependee.elements],
                                                                                                     private$i.quantity.foreground.self.times[dynamic.dependee.quantities],
                                                                                                     intrinsic.times))
                private$i.outcome.non.cumulative.self.after.times[[outcome.name]] = union_sorted_vectors(private$i.quantity.self.after.times[dynamic.dependee.quantities])
                
                if (length(private$i.outcome.non.cumulative.self.times[[outcome.name]])==0)
                    private$i.outcome.non.cumulative.self.times[[outcome.name]] = numeric()
                
                if (length(private$i.outcome.non.cumulative.self.after.times[[outcome.name]])==0)
                    private$i.outcome.non.cumulative.self.after.times[[outcome.name]] = numeric()
            }
        },
        
        clear.outcome.non.cumulative.self.times = function(outcome.names)
        {
            # Clear the times
            private$i.outcome.non.cumulative.self.times[outcome.names] = NULL
            private$i.outcome.non.cumulative.self.after.times[outcome.names] = NULL
            
            # Clear value.times for dependee.quantities
            specification = get.specification()
            dependee.quantities = specification$get.outcome.dependee.quantity.names(outcome.names)
            dynamic.dependee.quantities = dependee.quantities[ !private$i.quantity.is.static[dependee.quantities] ]
            private$clear.quantity.value.times(dynamic.dependee.quantities)
            
            # Done
            invisible(self)
        },
        
        #-- Quantity Value All Apply Times --#
        
        # When a quantity has a foreground set, it may have more value.times than it otherwise would
        # (ie, it has the foreground times in addition to the background times)
        # However, the foreground may not apply to every value in the quantity, which would mean that
        #   the values for the quantity unaffected by the foreground are DIFFERENT than they would be
        #   without that foreground set (ie, there are additional times for which background values are
        #   calculated, which, with the way we interpolate, means they may result in different behavior
        #   for top-level quantities)
        # To avoid this, we also calculate a mask that indicates whether the values in each cell of an
        #   element of i.quantity.values should apply at a given time
        # We build this mask by calculating from the bottom (elements), up, and propagating the mask
        #   (the mask gets overwritten with T anytime a foreground is applied).
        # This is potentially expensive, so we only want to bother to do it when we need to - times,
        #   when it would make a difference, which is only in top-level quantities
        # For a top-level quantity, the times at which all values may not apply are times when no
        #   dependee has a background set - so we only have to bother calculating a mask at these times
        #   (we can simplify further by saying times when no dependee ELEMENT has a background set)
        # Since the 'value.applies.mask' mask only matters for top-level quantities, we can save ourselves some work 
        #   and say that, for quantities which are not top-level quantities, the times at which values
        #   may not apply is the union of times-that-may-not-apply for all top-level quantities that depend on
        #   this quantity
        # And, in general, the times at which all values definitely apply is the setdiff of
        #   value times and times where not all values necessarily apply
        
        
        #    The quantity.value.all.apply.times for a quantity is the setdiff of
        #    - The quantity's *value.times* minus
        #    - The union of *top.level.value.may.not.apply.times* for every top.level.quantity that depends on this quantity
        #       plus the union of *outcome.value.may.not.apply.non.cumulative.times* for every outcome that has a non-cumulative dependency on this quantity
        #    * (for speed, we don't actually store the set of times, we store a mask that is T for all value.all.apply)
        calculate.quantity.value.all.applies.times = function(quantity.name)
        {
            private$i.quantity.value.all.applies.for.time[[quantity.name]] = rep(T, length(private$i.quantity.value.times[[quantity.name]]))
            names(private$i.quantity.value.all.applies.for.time[[quantity.name]]) = as.character(private$i.quantity.value.times[[quantity.name]])
            
            if (!private$i.quantity.is.static[quantity.name])
            {
                specification = private$get.specification()
                
                # Make sure the value times for this quantity are calculated
                if (is.null(private$i.quantity.value.times[[quantity.name]]))
                    private$calculate.quantity.value.times(quantity.name)
                
                # Pull the top-level quantities that depend on this, and make sure their value-may-not-apply times are calculated
                top.level.quantities = specification$get.dependent.top.level.quantity.names(quantity.name)
                dynamic.top.level.quantities = top.level.quantities[ !private$i.quantity.is.static[top.level.quantities] ]
                null.top.level.times.mask = as.logical(sapply(private$i.top.level.value.may.not.apply.times[dynamic.top.level.quantities], is.null))
                sapply(dynamic.top.level.quantities[null.top.level.times.mask], private$calculate.top.level.value.may.not.apply.times)
                
                # Pull the outcomes that depend on this, and make sure their value-may-not-apply times are calculated
                outcomes = specification$get.non.cumulative.dependent.outcome.names(quantity.name)
                dynamic.outcomes = outcomes[ !private$i.outcome.non.cumulative.is.static[outcomes] ]
                null.outcome.times.mask = as.logical(sapply(private$i.outcome.value.may.not.apply.non.cumulative.times[dynamic.outcomes], is.null))
                sapply(outcomes[null.outcome.times.mask], private$calculate.outcome.value.may.not.apply.non.cumulative.times)
                
                # union the top-level value-may-not-apply times
                all.may.not.apply.times = union_sorted_vectors(c(private$i.top.level.value.may.not.apply.times[dynamic.top.level.quantities],
                                                                 private$i.outcome.value.may.not.apply.non.cumulative.times[dynamic.outcomes]))
                
                # value.all.applies.times is the setdiff of value times and the may-not-apply times
                private$i.quantity.value.all.applies.for.time[[quantity.name]][as.character(all.may.not.apply.times)] = F
            }
            
            invisible(self)
        },
        
        clear.quantity.value.all.applies.times = function(quantity.names)
        {
            # Clear the times
            private$i.quantity.value.all.applies.for.time[quantity.names] = NULL
            
            # Done
            invisible(self)
        },
        
        
        #-- Top-Level Value May Not Apply Times --#
        
        # The *top.level.value.may.not.apply.times* for a top.level.quantity is the setdiff of
        #   The top.level.quantity's *value.times* minus
        #   The union of *element.background.self.times* for all elements that the quantity depends on
        calculate.top.level.value.may.not.apply.times = function(top.level.name)
        {
            if (private$i.quantity.is.static[top.level.name])
                private$i.top.level.value.may.not.apply.times[[top.level.name]] = numeric()
            else
            {
                specification = private$get.specification()
                
                # Make sure the value times for this quantity are calculated
                if (is.null(private$i.quantity.value.times[[top.level.name]]))
                    private$calculate.quantity.value.times(top.level.name)
                
                # Pull the elements this depends on, and make sure their background self times are calculated
                dependee.elements = specification$get.dependee.element.names(top.level.name)
                dynamic.dependee.elements = dependee.elements[ !private$i.quantity.is.static[dependee.elements] ]
                null.dependee.element.times.mask = as.logical(sapply(private$i.element.background.self.times[dynamic.dependee.elements], is.null))
                sapply(dynamic.dependee.elements[null.dependee.element.times.mask], private$calculate.element.background.self.times)
                
                # union the background.self.times
                all.background.times = union_sorted_vectors(private$i.element.background.self.times[dynamic.dependee.elements])
                
                # value.may.not.apply.times is the setdiff of value times and the background times
                private$i.top.level.value.may.not.apply.times[[top.level.name]] = setdiff_sorted_vectors(private$i.quantity.value.times[[top.level.name]],
                                                                                                         all.background.times)
            }
            
            invisible(self)
        },
        
        clear.top.level.value.may.not.apply.times = function(top.level.names)
        {
            # Clear the times
            private$i.top.level.value.may.not.apply.times[top.level.names] = NULL
            
            # Clear quantity.value.all.applies.times for all quantities that these top-level quantities depend on
            specification = get.specification()
            dependee.quantities = specification$get.dependee.quantity.names(top.level.names)
            dynamic.dependee.quantities = dependee.quantities[ !private$i.quantity.is.static[dependee.quantities] ]
            private$clear.quantity.value.all.applies.times(dynamic.dependee.quantities)
            
            # Done
            invisible(self)
        },
        
        #-- Outcome Value-May-Non-Apply-Non-Cumulative Times --#
        # The *outcome.value.may.not.apply.non.cumulative.times* is the setdiff of
        #    - The outcomes *outcome.non.cumulative.value.times* minus
        #    - The union of *element.background.self.times* for all elements that the outcome has a non-cumulative dependency on
        
        calculate.outcome.value.may.not.apply.non.cumulative.times = function(outcome.name)
        {
            if (private$i.outcome.non.cumulative.is.static[outcome.name])
                private$i.outcome.value.may.not.apply.non.cumulative.times[[outcome.name]] = numeric()
            else
            {
                specification = private$get.specification()
                
                # Make sure the value times for this quantity are calculated
                if (is.null(private$i.outcome.non.cumulative.value.times[[outcome.name]]))
                    private$calculate.outcome.non.cumulative.value.times(outcome.name)
                
                # Pull the elements this depends on, and make sure their background self times are calculated
                dependee.elements = specification$get.outcome.dependee.element.names(outcome.name)
                dynamic.dependee.elements = dependee.elements[ !private$i.quantity.is.static[dependee.elements] ]
                null.dependee.element.times.mask = as.logical(sapply(private$i.element.background.self.times[dynamic.dependee.elements], is.null))
                sapply(dynamic.dependee.elements[null.dependee.element.times.mask], private$calculate.element.background.self.times)
                
                # union the background.self.times
                all.background.times = union_sorted_vectors(private$i.element.background.self.times[dynamic.dependee.elements])
                
                # value.may.not.apply.times is the setdiff of value times and the background times
                private$i.outcome.value.may.not.apply.non.cumulative.times[[outcome.name]] = 
                    setdiff_sorted_vectors(private$i.outcome.non.cumulative.value.times[[outcome.name]],
                                           all.background.times)
            }
            
            invisible(self)
        },
    
        clear.outcome.value.may.not.apply.non.cumulative.times = function(outcome.names)
        {
            # Clear the times
            private$i.outcome.value.may.not.apply.non.cumulative.times[outcome.names] = NULL
            
            # Clear quantity.value.all.applies.times for all quantities that these top-level quantities depend on
            specification = get.specification()
            dependee.quantities = specification$get.outcome.dependee.quantity.names(outcome.names)
            dynamic.dependee.quantities = dependee.quantities[ !private$i.quantity.is.static[dependee.quantities] ]
            private$clear.quantity.value.all.applies.times(dynamic.dependee.quantities)
            
            # Done
            invisible(self)
        },
        
        #-- Outcome Non-Cumulative Self-Times --#
        # The *outome.non.cumulative.value.times* for each outcome is the union of all
        #       *outcome.non.cumulative.self.times* for any outcome that depends on this outcome (including itself)
        
        calculate.outcome.non.cumulative.value.times = function(outcome.name)
        {
            if (private$i.outcome.non.cumulative.is.static[outcome.name])
            {
                private$i.outcome.non.cumulative.value.times[[outcome.name]] = 'all'
                private$i.outcome.non.cumulative.value.time.is.after.time[[outcome.name]] = F
            }
            else
            {
                specification = private$get.specification()
             
                # Pull the outcomes that have a non-cumulative dependency on this, and make sure their self-times are calculated
                outcomes = specification$get.outcome.non.cumulative.dependent.outcome.names(outcome.name)
                dynamic.outcomes = outcomes[ !private$i.outcome.non.cumulative.is.static[outcomes] ]
                null.outcome.times.mask = as.logical(sapply(private$i.outcome.non.cumulative.self.times[dynamic.outcomes], is.null))
                sapply(dynamic.outcomes[null.outcome.times.mask], private$calculate.outcome.non.cumulative.self.times)
                
                # The value times and after value times are the union of these times
                
                self.times = union_sorted_vectors(private$i.outcome.non.cumulative.self.times[dynamic.outcomes])
                self.after.times = union_sorted_vectors(private$i.outcome.non.cumulative.self.after.times[dynamic.outcomes])
                
                if (length(self.after.times)==0)
                {
                    private$i.outcome.non.cumulative.value.times[[outcome.name]] = self.times
                    private$i.outcome.non.cumulative.value.time.is.after.time[[outcome.name]] = rep(F, length(self.times))
                }
                else
                {
                    times.and.indicators = 
                        interpolate_sorted_vectors(v1 = self.times,
                                                   v2 = self.after.times,
                                                   F,T)
                    
                    private$i.outcome.non.cumulative.value.times[[outcome.name]] = times.and.indictors$interpolated
                    private$i.outcome.non.cumulative.value.time.is.after.time[[outcome.name]] = times.and.indicators$indicators
                }
            }
            
            invisible(self)
        },
        
        clear.outcome.non.cumulative.value.times = function(outcome.names)
        {
            # Clear the times
            private$i.outcome.non.cumulative.value.times[outcome.names] = NULL
            private$i.outcome.non.cumulative.value.after.times[outcome.names] = NULL
            
            # Clear the value-all-applies times
            private$clear.outcome.non.cumulative.value.all.applies.times(outcome.names)
            
            # Done
            invisible(self)
        },
        
        #-- Outcome Value Times --#
        #   The times for which the final outcome will have a value
        
        # The *outcome.value.times* is a vector of times for which this value is calculated
        #       For a cumulative outcome, it is just every year between max(run.from.time, and the outcome's from.time) and min(run.to.time, and the outcome's to.time)
        #       For a non-cumulative outcome, it is the *outcome.non.cumulative.value.times*
        #           Any times for which there is both a value and an after.value will appear in twice in the vector of times
        # The *outcome.non.cumulative.value.time.is.after.time* is a logical indicator corresponding to the *outcome.value.times* for an outcome
        #       We store this as a logical vector for computational purposes, but
        #       the set of after.times for an outcome is the union of all *quantity.self.after.times* for any quantity that this value depends on
        calculate.outcome.value.times = function(outcome.name, specification)
        {
            outcome = specification$get.outcome(outcome.name)
            if (outcome$is.cumulative || is(outcome, 'intrinsic.model.outcome'))
            {
                if (outcome$is.cumulative)
                {
                    private$i.outcome.value.times[[outcome.name]] =
                        max(outcome$from.year, private$i.run.from.time) : min(outcome$to.year, private$i.run.to.time-1)
                }
                else
                {
                    private$i.outcome.value.times[[outcome.name]] =
                        max(outcome$from.year, private$i.run.from.time) : min(outcome$to.year, private$i.run.to.time)
                }
                
                private$i.outcome.value.time.is.after.time[[outcome.name]] = rep(F, length(private$i.outcome.value.times[[outcome.name]]))
            }
            else
            {
                if (is.null(private$i.outcome.non.cumulative.value.times[[outcome.name]]))
                    private$calculate.outcome.non.cumulative.value.times(outcome.name)
                
                private$i.outcome.value.times[[outcome.name]] = private$i.outcome.non.cumulative.value.times[[outcome.name]]
                private$i.outcome.value.time.is.after.time[[outcome.name]] = private$i.outcome.non.cumulative.value.time.is.after.time[[outcome.name]]
            }
        },
        
        clear.outcome.value.times = function(outcome.names)
        {
            private$i.outcome.value.times[outcome.names] = NULL
            private$i.outcome.value.time.is.after.time[outcome.names] = NULL
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
            # Figure out what times values are missing for
            if (is.null(private$i.quantity.value.times[[quantity.name]]))
                private$calculate.quantity.value.times(quantity.name)
            required.times = as.character(private$i.quantity.value.times[[quantity.name]])
            
            if (private$i.quantity.is.static[quantity.name])
            {
                if (is.null(private$i.quantity.values[['all']]))
                    missing.times = 'all'
                else
                    missing.times = numeric()
            }
            else
                missing.times = setdiff_sorted_vectors(private$i.quantity.value.times[[quantity.name]], 
                                                       as.numeric(names(private$i.quantity.values[[quantity.name]])))

            # If missing values, calculate them
            if (length(missing.times)>0)
            {
                quantity = private$get.specification()$get.quantity(quantity.name)
                
                #-- For debugging --#
      #          if (is.element.name(quantity.name))
      #              print(paste0(paste0(rep(" ", depth), collapse=''),
      #                           "-Calculate element '", quantity.name, "'"))
      #          else
      #              print(paste0(paste0(rep(" ", depth), collapse=''),
      #                           "-Calculate quantity '", quantity.name, "'"))

                
                #-- Fill in the background --#
                if (private$is.element.name(quantity.name))
                    private$calculate.element.background.value(quantity.name, 
                                                               missing.times = missing.times, 
                                                               check.consistency = check.consistency)
                else
                    private$calculate.quantity.background.value(quantity.name, 
                                                                missing.times = missing.times, 
                                                                depth = depth,
                                                                check.consistency = check.consistency)
                
                
                #-- Sort the background --#
                char.all.times = as.character(private$i.quantity.value.times[[quantity.name]])
                private$i.quantity.values[[quantity.name]] = private$i.quantity.values[[quantity.name]][ char.all.times ]
                private$i.quantity.after.values[[quantity.name]] = private$i.quantity.after.values[[quantity.name]][ char.all.times ]
                
                
                if (any(!sapply(private$i.quantity.after.values[[quantity.name]], is.null)))
                    browser()
                #-- Fold in foreground if there is any --#
                foregrounds = private$i.resolved.foregrounds[[quantity.name]]
                if (length(foregrounds)>0)
                {
                    if (length(private$i.quantity.foreground.effect.indices[[quantity.name]]) < length(foregrounds))
                        private$calculate.foreground.effect.indices(quantity.name)
                  
                    # The cpp function does all the work - hooray!
                    values.and.after.values = apply_foregrounds(values = private$i.quantity.values[[quantity.name]],
                                                                value_times = private$i.quantity.value.times[[quantity.name]],
                                                                after_values = private$i.quantity.after.values[[quantity.name]],
                                                                times_to_apply_to = missing.times,
                                                                foregrounds = foregrounds,
                                                                indices_per_effect_per_foreground = private$i.quantity.foreground.effect.indices[[quantity.name]],
                                                                scale = quantity$scale)
                    # Save the results
                    private$i.quantity.values[[quantity.name]] = values.and.after.values$values
                    private$i.quantity.after.values[[quantity.name]] = values.and.after.values$after.values
                }
                
                #-- Order the Values by Time --#
                char.times = as.character(private$i.quantity.value.times[[quantity.name]])
                private$i.quantity.values[[quantity.name]] = private$i.quantity.values[[quantity.name]][char.times]
                private$i.quantity.after.values[[quantity.name]] = private$i.quantity.after.values[[quantity.name]][char.times]
                
                #-- Check scale --#
                if (check.consistency && !is.null(quantity$scale))
                    check.values.for.model.scale(values = private$i.quantity.values[[quantity.name]][as.character(missing.times)], 
                                                 scale = quantity$scale, 
                                                 variable.name.for.error = "the calculated values",
                                                 error.prefix =  paste0("Error calculating values for model quantity '", quantity.name, "': "))
            }
            
            if (any(!sapply(private$i.quantity.after.values[[quantity.name]], is.null)))
                browser()
            
            #-- Calculate the value.applies.mask (if needed) --#
            private$calculate.quantity.value.applies.mask(quantity.name)
            
            #-- Done --#
            invisible(self)
        },

        calculate.quantity.background.value = function(quantity.name, missing.times,
                                                       depth, check.consistency)
        {
            missing.times = as.character(missing.times)
            
            #-- Fill in missing values --#
            quantity = private$get.specification()$get.quantity(quantity.name)
            
            #-- Make sure the dependee quantities are all calculated --#
            sapply(quantity$depends.on, private$calculate.quantity.value, check.consistency=check.consistency, depth=depth+1)
            
            #-- Loop through missing times --#
            for (time in missing.times)
            {
                char.time = as.character(time)
                depends.on.has.after = sapply(private$i.quantity.after.values[quantity$depends.on], function(values){
                    !is.null(values[[char.time]])
                })
                names(depends.on.has.after) = quantity$depends.on
                
                bindings = list(specification.metadata = self$specification.metadata,
                                location = private$i.location)
                
                for (is.after.time in c(F,T))
                {
                    if (is.after.time && !any(depends.on.has.after))
                        quant.value = NULL
                    else
                    {
                        #-- Calculate the values for each component --#
                        component.values = list()
                        for (i in 1:quantity$n.components) # From Todd in Nov 2023: I truly have no idea why, but doing this as a for loop was 30x faster than doing it as an sapply
                        {
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
                            if (is.after.time)
                                update.bindings.for = comp$depends.on[ depends.on.has.after[comp$depends.on] ]
                            else
                                update.bindings.for = comp$depends.on

                            for (dep.on in update.bindings.for)
                            {
#                            bindings[update.bindings.for] = lapply(update.bindings.for, function(dep.on){
                                
                                if (private$i.quantity.is.static[dep.on])
                                    values = private$i.quantity.values[[dep.on]][['all']]
                                else if (is.after.time)
                                    values = private$i.quantity.after.values[[dep.on]][[char.time]]
                                else
                                    values = private$i.quantity.values[[dep.on]][[char.time]]
                                
                                if (comp$value.type == 'expression')
                                {
                                    if (length(private$i.quantity.mapping.indices[[quantity.name]]$components.depends.on) < i ||
                                        is.null(private$i.quantity.mapping.indices[[quantity.name]]$components.depends.on[[i]][[dep.on]]))
                                    {
                                        private$calculate.quantity.component.depends.on.indices(quantity, 
                                                                                                component.index = i,
                                                                                                depends.on = dep.on)
                                    } 
                                    dep.on.indices = private$i.quantity.mapping.indices[[quantity.name]]$components.depends.on[[i]][[dep.on]]
                                    
                                    values = values[dep.on.indices]
                                }
#                                else
 #                                   values
                                
                                bindings[[dep.on]] = values
                            }
                           # })

                            #-- Calculate the value --#
                            value = comp$evaluate(bindings = bindings,
                                                  error.prefix = error.prefix)
                            
                            #-- If a function value.type, check the returned value and set its dim.names if needed --#
                            if (comp$value.type=='function')
                            {
                                if (check.consistency)
                                    private$check.function.quantity.component.value(value, quantity=quantity, component.index=i,
                                                                            time=time, error.prefix=error.prefix)
                                
                                if ((length(private$i.quantity.component.dim.names[[quantity.name]]) < i ||
                                     is.null(private$i.quantity.component.dim.names[[quantity.name]][[i]])) ||
                                    (check.consistency && time == missing.times[1]))
                                {
                                    private$calculate.quantity.component.dim.names(quantity, 
                                                                           component.index = i,
                                                                           value.for.function = value)
                                }
                                
                                if (check.consistency && !dim.names.equal(dim.names.1 = private$i.quantity.component.dim.names[[quantity.name]][[i]],
                                                                          dim.names.2 = dimnames(value),
                                                                          match.order.of.dimensions = T,
                                                                          match.order.within.dimensions = T))
                                {
                                    stop(paste0(error.prefix,
                                                "The dimnames for the value calculated at time ", time,
                                                " do not match the dimnames of values for previous times"))
                                }
                            }
                            
                            #-- A check --#
                            if (length(value)==0)
                                browser()
                            
                            #-- Package up the value --#
                            component.values[[i]] = value
                        }

                        #-- Recalculate the dim.names if needed --#
                        if (is.null(i.quantity.dim.names[[quantity.name]]))
                            calculate.quantity.dim.names(quantity)
                        
                        
                        #-- Incorporate each component into the quantity value --#
                        quant.value = NULL
                        
                        for (i in 1:quantity$n.components)
                        {
                            comp = quantity$components[[i]]
                            comp.value = component.values[[i]]
                            
                            #-- Recalculate the indices if we need to --#
                            if (length(private$i.quantity.mapping.indices[[quantity.name]]$components.expand)<i || 
                                is.null(private$i.quantity.mapping.indices[[quantity.name]]$components.expand[[i]]))
                                private$calculate.quantity.component.expand.access.indices(quantity, component.index=i)
                            
                            #-- Pull the access/expand indices --#
                            expand.indices = private$i.quantity.mapping.indices[[quantity.name]]$components.expand[[i]]
                            access.indices = private$i.quantity.mapping.indices[[quantity.name]]$components.access[[i]]
                            
                            # Fold it in to the comp.value
                            if (is.null(access.indices))
                                quant.value = comp.value[expand.indices]
                            else if (comp$apply.function=='overwrite')
                                #quant.value = do_access_overwrite(dst=quant.value, src=comp.value, dst_indices=access.indices, src_indices=expand.indices)
                                quant.value[access.indices] = comp.value[expand.indices]
                            else if (comp$apply.function=='add')
                                #quant.value = do_access_add(dst=quant.value, src=comp.value, dst_indices=access.indices, src_indices=expand.indices)
                                quant.value[access.indices] = quant.value[access.indices] + comp.value[expand.indices]
                            else if (comp$apply.function=='subtract')
                                #quant.value = do_access_subtract(dst=quant.value, src=comp.value, dst_indices=access.indices, src_indices=expand.indices)
                                quant.value[access.indices] = quant.value[access.indices] - comp.value[expand.indices]
                            else if (comp$apply.function=='multiply')
                                #quant.value = do_access_multiply(dst=quant.value, src=comp.value, dst_indices=access.indices, src_indices=expand.indices)
                                quant.value[access.indices] = quant.value[access.indices] * comp.value[expand.indices]
                            else if (comp$apply.function=='divide')
                                #quant.value = do_access_divide(dst=quant.value, src=comp.value, dst_indices=access.indices, src_indices=expand.indices)
                                quant.value[access.indices] = quant.value[access.indices] / comp.value[expand.indices]
                            else
                                stop(paste0("Invalid apply.function '", comp$apply.function, "' for model quantity '", quantity.name,
                                            "'. Must be one of 'overwrite', 'add', 'subtract', 'multiply', or 'divide'"))
                            
                            
                            if (any(is.na(quant.value)))
                                browser()
                        }
                        
                        
    
                        
                        #-- Check for NA --#
                        if (any(is.na(quant.value)))
                            browser()
                        #                            stop(paste0(paste0("Error calculating values for model quantity '", quantity.name, "': NA values were generated")))
                        
                        if (length(quant.value)==0)
                            browser()
                        
                        #-- Set the dimnames --#
                        if (length(private$i.quantity.dim.names[[quantity.name]]) > 0)
                        {
                            dim(quant.value) = sapply(private$i.quantity.dim.names[[quantity.name]], length)
                            dimnames(quant.value) = private$i.quantity.dim.names[[quantity.name]]
                        }
                        
                    }
                    
                    #-- Store the Value --#
                    if (is.after.time)
                        private$i.quantity.after.values[[quantity.name]][char.time] = list(quant.value) #wrapping in a list here lets us enter in NULL values
                    else
                        private$i.quantity.values[[quantity.name]][[char.time]] = quant.value
                }
            }
            
            # Done
            invisible(self)
        },
        

        # interpolates ramp on the model scale
        calculate.element.background.value = function(element.name, missing.times, check.consistency)
        {
            char.times = as.character(missing.times)
            element = private$get.specification()$get.quantity(element.name)
            
            if (is.null(private$i.quantity.dim.names[[element.name]]))
                private$calculate.quantity.dim.names(element)
            
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
            if (bkgd$is.static) #may no longer be static if we have a foreground
            {
                if (length(missing.times)>0)
                {
                    if (is.null(bkgd$functional.form))
                        value = bkgd$value
                    else
                        value = convert.model.scale(bkgd$functional.form$project.static(alphas = bkgd$functional.form.alphas,
                                                                                        dim.names = i.quantity.dim.names[[element.name]],
                                                                                        check.consistency = check.consistency,
                                                                                        error.prefix = paste0("Error projecting values from the (static) functional form for element '", element.name, "': ")),
                                                    convert.from.scale = element$functional.form.scale,
                                                    convert.to.scale = element$scale)
                    
                    if (private$i.quantity.is.static[element.name])
                    {
                        private$i.quantity.values[[element.name]][['all']] = value
                    }
                    else # This is a static value at baseline, but has a foreground overlaid
                    {
                        private$i.quantity.values[[element.name]][char.times] = lapply(1:length(missing.times), function(i){
                            value + 0 
                                # The +0 here forces a DEEP copy of value. 
                                # This is important, because applying foregrounds will overwrite in place,
                                #  so we need each time for the value to be referring to a different instance
                        })
                    }
                }
            }
            else # The values are determined by functional form
            {
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
                }
            }
            
            # Fill in after.values
            private$i.quantity.after.values[[element.name]][char.times] = lapply(missing.times, function(time){NULL})
            
            # A debug check
            if (any(sapply(private$i.quantity.values[[element.name]], length)==0))
                browser()
            
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
        

        ##--------------------------------------------------##
        ##-- CALCULATING THE QUANTITY VALUE APPLIES MASKS --##
        ##--------------------------------------------------##
        
        calculate.quantity.value.applies.mask = function(quantity.name)
        {
            if (is.null(private$i.quantity.value.times[[quantity.name]]))
                private$calculate.quantity.value.times(quantity.name)
            if (private$i.quantity.is.static[quantity.name])
            
            required.times = as.character(private$i.quantity.value.times[[quantity.name]])
            
            if (private$i.quantity.is.static[quantity.name])
            {
                if (is.null(private$i.quantity.values[['all']]))
                    missing.times = 'all'
                else
                    missing.times = numeric()
            }
            else
                missing.times = setdiff_sorted_vectors(private$i.quantity.value.times[[quantity.name]], 
                                                       as.numeric(names(private$i.quantity.value.applies.mask[[quantity.name]])))
            
            if (length(missing.times)>0)
            {
                #-- Calculate the value.applies.mask for background --#
                private$calculate.quantity.background.value.applies.masks(quantity.name, missing.times = missing.times)
                
                foregrounds = private$i.resolved.foregrounds[[quantity.name]]
                if (length(foregrounds)>0)
                {
                    # Update the value.applies.mask
                    update.value.applies.times = setdiff_sorted_vectors(missing.times, private$i.quantity.value.all.applies.for.time[[quantity.name]])
                    char.update.value.applies.times = as.character(update.value.applies.times)
                    
                    private$i.quantity.value.applies.mask[char.update.value.applies.times] = 
                        private$incorporate.foreground.into.value.applies.masks(quantity.name, times = update.value.applies.times, is.after = F)
                    
                    after.mask = !sapply(private$i.quantity.after.values[[quantity.name]][char.update.value.applies.times], is.null)
                    private$i.quantity.after.value.applies.mask[ char.update.value.applies.times[after.mask] ] = 
                        private$incorporate.foreground.into.value.applies.masks(quantity.name, times = update.value.applies.times[after.mask], is.after = T)
                }
            }
        },
        
        calculate.quantity.background.value.applies.masks = function(quantity.name, missing.times)
        {
            # Pull the quantity
            quantity = private$get.specification()$get.quantity(quantity.name)
            
            char.times = as.character(missing.times)
            
            #-- Set up the value.all.applies.times --#
            if (is.null(private$i.quantity.value.all.applies.for.time[[quantity.name]]))
                private$calculate.quantity.value.all.applies.times(quantity.name)
            
            if (is.null(private$i.quantity.value.applies.mask[[quantity.name]]))
                private$i.quantity.value.applies.mask[[quantity.name]] = list()
            
            private$i.quantity.after.value.applies.mask[[quantity.name]][char.times] = lapply(char.times, function(t){NULL})
            
            if (length(quantity$depends.on)==0)
            {
                private$i.quantity.value.applies.mask[[quantity.name]][char.times] = T
            }
            else
            {
                private$i.quantity.value.applies.mask[[quantity.name]][char.times] =
                    private$i.quantity.value.all.applies.for.time[[quantity.name]][char.times]
                
                for (time in missing.times)
                {
                    char.time = as.character(time)
                    
                    has.after.value = !is.null(private$quantity.after.values[[quantity.name]][char.time])
                    
                    for (is.after.time in c(F,T)[c(!private$i.quantity.value.all.applies.for.time[[quantity.name]][char.time], has.after.value)])
                    {
                        #-- Calculate the value.applies masks for each component --#
                        component.masks = lapply(1:quantity$n.components, function(i){
                            
                            comp = quantity$components[[i]]
                            
                            if (comp$value.type == 'numeric' || length(comp$depends.on)==0)
                                T
                            else 
                            {
                                non.static.depends.on = comp$depends.on[!private$i.quantity.is.static[comp$depends.on]]
                                
                                dep.on.masks = lapply(non.static.depends.on, function(dep.on){
                                    
                                    dep.on.mask = NULL
                                    if (is.after.time)
                                        dep.on.mask = private$i.quantity.after.value.applies.mask[[dep.on]][[char.time]]
                                    if (is.null(dep.on.mask))
                                        dep.on.mask = private$i.quantity.value.applies.mask[[dep.on]][[char.time]]

                                    if (length(dep.on.mask)==1)
                                        dep.on.mask
                                    else
                                    {
                                        dep.on.indices = private$i.quantity.mapping.indices[[quantity.name]]$components.depends.on[[i]][[dep.on]]
                                        dep.on.mask[dep.on.indices]
                                    }
                                })
                                
                                if (comp$value.type == 'expression' || comp$value.type == 'character')
                                {
                                    combined.mask = dep.on.masks[[1]]
                                    for (add.mask in dep.on.masks[-1])
                                        combined.mask = combined.mask | add.mask
                                    
                                    combined.mask
                                }
                                else # comp$value == 'function'
                                {
                                    any(sapply(dep.on.masks, any))
                                }
                            }
                        })
                        
                        #-- Fold the masks from all the components together --#
                        quant.value.applies = NULL
                        
                        for (i in 1:quantity$n.components)
                        {
                            comp = quantity$components[[i]]
                            comp.value.applies = component.masks[[i]]
                            
                            #-- Pull the access/expand indices --#
                            expand.indices = private$i.quantity.mapping.indices[[quantity.name]]$components.expand[[i]]
                            access.indices = private$i.quantity.mapping.indices[[quantity.name]]$components.access[[i]]
                            
                            if (length(comp.value.applies)==1)
                            {
                                if (is.null(access.indices))
                                    quant.value.applies = comp.value.applies
                                else if (comp$apply.function=='overwrite')
                                    quant.value.applies[access.indices] = comp.value.applies
                                else if (comp.value.applies)
                                    quant.value.applies[access.indices] = T
                            }
                            else
                            {
                                if (is.null(access.indices))
                                    quant.value.applies = comp.value.applies[expand.indices]
                                else if (comp$apply.function=='overwrite')
                                    quant.value.applies[access.indices] = comp.value.applies[expand.indices]
                                else
                                    quant.value.applies[access.indices] = quant.value.applies[access.indices] | comp.value.applies[expand.indices]
                            }
                        }
                        
                        if (is.null(quant.value.applies) || any(is.na(quant.value.applies)) || !any(quant.value.applies))
                            browser()
                        
                        if (all(quant.value.applies))
                            quant.value.applies = T
                        
                        if (is.after.time)
                            private$i.quantity.after.value.applies.mask[[quantity.name]][[char.time]] = quant.value.applies
                        else
                            private$i.quantity.value.applies.mask[[quantity.name]][[char.time]] = quant.value.applies
                    }
                }
            }
            
            #-- Order it and return --#
            char.all.times = as.character(private$i.quantity.value.times[[quantity.name]])
            private$i.quantity.value.applies.mask[[quantity.name]] = private$i.quantity.value.applies.mask[[quantity.name]][char.all.times]
            private$i.quantity.after.value.applies.mask[[quantity.name]] = private$i.quantity.after.value.applies.mask[[quantity.name]][char.all.times]
            
            invisible(self)
        },
        
        incorporate.foreground.into.value.applies.masks = function(quantity.name, times, is.after)
        {
            foregrounds = private$i.resolved.foregrounds[[quantity.name]]
            
            lapply(times, function(time){
                
                char.time = as.character(time)
                mask = NULL
                if (is.after)
                    mask = private$i.quantity.after.value.applies.mask[[quantity.name]][[char.time]]
                if (is.null(mask))
                    mask = private$i.quantity.value.applies.mask[[quantity.name]][[char.time]]
                
                if (length(mask)==1)
                    mask = array(mask,
                                 dim = sapply(private$i.quantity.dim.names[[quantity.name]], length),
                                 dimnames = private$i.quantity.dim.names[[quantity.name]])
                
                if (!mask[1])
                {
                    for (frgd.name in names(foregrounds))
                    {
                        frgd = foregrounds[[frgd.name]]
                        for (i in 1:length(frgd$effects))
                        {
                            effect = frgd$effects[[i]]
                            if (effect$start.time <= time && effect$end.time >= time)
                            {
                                indices = private$i.quantity.foreground.effect.indices[[quantity.name]][[frgd.name]][[i]]
                                mask[indices] = T
                            }
                        }   
                    }
                }
                
                mask
            })
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
                
                bkgd = private$i.element.backgrounds[[quantity.name]]
                if (is.null(bkgd$functional.form))
                {
                    if (is.null(dimnames(bkgd$value)))
                        private$i.quantity.dim.names[[quantity.name]] = list()
                    else
                        private$i.quantity.dim.names[[quantity.name]] = dimnames(bkgd$value)
                }
                else if (is.null(private$i.quantity.max.dim.names[[quantity.name]]))
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
            }
            else
            {
                # When max.dim.names are set, non-element quantity dim.names have every dimension in max.dim.names that
                # - Appears in a component's value OR
                # - Appears in a component's applies.to
                # When max.dim.names are not set, then dim.names are the outer-join (union) of
                # - The dimnames of the all components plus
                # - The applies.to from each component (after the first, which has no applies to)
                
                if (is.null(private$i.quantity.max.dim.names[[quantity.name]]))
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
                                                         private$i.quantity.component.applies.to[[quantity.name]])
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
            
            foregrounds = private$i.resolved.foregrounds[[quantity.name]]
            if (length(foregrounds)>0)
            {
                if (is.null(private$i.quantity.max.dim.names[[quantity.name]])) 
                    # this condition should never evaluate to TRUE
                    # we require that max.dim.names be non-NULL if we're going to apply a foreground
                    # but we'll leave this chunk of code in here to protect ourselves in case we ever
                    #  (for what reason I cannot fathom) change that requirement
                {
                    for (frgd in foregrounds)
                    {
                        for (tpop.mask in frgd$target.population.masks)
                        {
                            tpop.dim.names = dimnames(tpop.mask)
                            
                            # make sure we are not introducing new values to a dimension
                            # do I actually need to do this check? I think not - we'll just make the function handle them at their own risk
                            # and it's moot anyway at this time since this code should never actually execute
                            
                            # join them
                            private$i.quantity.component.dim.names[[quantity.name]] = 
                                outer.join.dim.names(private$i.quantity.component.dim.names[[quantity.name]],
                                                     tpop.dim.names)
                        }
                    }
                }
                else 
                {
                    dimensions = names(private$i.quantity.dim.names[[quantity.name]])
                    for (frgd in foregrounds)
                    {
                        for (tpop.mask in frgd$target.population.masks)
                        {
                            dimensions = union(dimensions, names(tpop.mask))
                        }
                    }
                    
                    private$i.quantity.dim.names[[quantity.name]] = private$i.quantity.max.dim.names[[quantity.name]][dimensions]
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
            if (!is.null(private$i.crunched.quantity.dim.names[[quantity.name]]) &&
                !dim.names.equal(private$i.crunched.quantity.dim.names[[quantity.name]],
                                 private$i.quantity.dim.names[[quantity.name]]))
            {
                private$clear.dependent.on.quantity.dim.names(quantity.name)
                private$i.diffeq.settings = notify.diffeq.settings.of.quantity.dim.names.change(private$i.diffeq.settings,
                                                                                                quantity.name = quantity.name)
                private$i.crunched.quantity.dim.names[[quantity.name]] = private$i.quantity.dim.names[[quantity.name]]
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

        calculate.foreground.effect.indices = function(quantity.name)
        {
            foreground.ids = names(private$i.resolved.foregrounds[[quantity.name]])
            
            # Calculate the ones we're missing
            for (foreground.id in foreground.ids)
            {
                if (is.null(private$i.quantity.foreground.effect.indices[[quantity.name]][[foreground.id]]))
                {
                    # these indices should be for C++, ie indexed from zero
                    indices.into.quantity = 0:(prod(sapply(private$i.quantity.dim.names[[quantity.name]], length))-1)
                    foreground = private$i.resolved.foregrounds[[quantity.name]][[foreground.id]]
                    
                    private$i.quantity.foreground.effect.indices[[quantity.name]][[foreground.id]] =
                        lapply(foreground$target.population.masks, function(tpop.mask){
                            expand.tpop.mask.indices = get.expand.array.indices(to.expand.dim.names = dimnames(tpop.mask), 
                                                                                target.dim.names = private$i.quantity.dim.names[[quantity.name]])
                            indices.into.quantity[ tpop.mask[expand.tpop.mask.indices] ]
                        })
                }
            }
            
            # make sure we're in the right order
            private$i.quantity.foreground.effect.indices[[quantity.name]] = 
                private$i.quantity.foreground.effect.indices[[quantity.name]][foreground.ids]
            
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
        clear.dependent.values = function(quantity.name, clear.after.time=-Inf, clear.before.time=Inf)
        {
            specification = get.specification()
            to.clear = specification$get.dependent.quantity.names(quantity.name) #a quantity is defined to always depend on itself

            if (clear.after.time == -Inf && clear.before.time == Inf)
            {
                for (one.to.clear in to.clear)
                {
                    private$i.quantity.values[[one.to.clear]] = list()
                    private$i.quantity.after.values[[one.to.clear]] = list()
                }
            }
            else
            {
                for (one.to.clear in to.clear)
                {
                    value.times = as.numeric(names(private$i.quantity.values[[one.to.clear]]))
                    to.clear.mask = value.times > clear.after.time & value.times < clear.before.time
                    # doing this for every quantity to clear is a defensive move
                    # theoretically, we could just calculate the times to clear from the declared quantity to clear
                    # this presumes that the times to clear in all dependents are present in the original quantity,
                    #   which *should* be true, but we won't assume that
                    
                    private$i.quantity.values[[one.to.clear]][to.clear.mask] = NULL
                    private$i.quantity.after.values[[one.to.clear]][to.clear.mask] = NULL
                }
            }
            
            invisible(self)
        },

        clear.dependent.value.applies.masks = function(quantity.name, clear.after.time=-Inf, clear.before.time=Inf)
        {
            specification = get.specification()
            to.clear = c(quantity.name, specification$get.dependent.quantity.names(element.name))
            
            if (clear.after.time == -Inf && clear.before.time == Inf)
            {
                for (one.to.clear in to.clear)
                {
                    private$i.quantity.value.applies.mask[[one.to.clear]] = list()
                    private$i.quantity.after.value.applies.mask[[one.to.clear]] = list()
                }
            }
            else
            {
                for (one.to.clear in to.clear)
                {
                    value.times = as.numeric(names(private$i.quantity.value.applies.mask[[one.to.clear]]))
                    to.clear.mask = value.times > clear.after.time & value.times < clear.before.time
                    # doing this for every quantity to clear is a defensive move
                    # theoretically, we could just calculate the times to clear from the declared quantity to clear
                    # this presumes that the times to clear in all dependents are present in the original quantity,
                    #   which *should* be true, but we won't assume that
                    
                    private$i.quantity.value.applies.mask[[one.to.clear]][to.clear.mask] = NULL
                    private$i.quantity.after.value.applies.mask[[one.to.clear]][to.clear.mask] = NULL
                }
            }
            
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
            
            # Clear foreground indices
            private$i.foreground.effect.indices[[quantity.name]] = NULL
            
            # Clear outcome dependencies
            privaet$clear.outcome.dependencies.on.quantity.dim.names(quantity.name, specification=specification)
            
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
        
        ##---------------------------##
        ##-- Resolving Foregrounds --##
        ##---------------------------##
        
        
        # This function will only be called
        # (a) when this foreground is first set or
        # (b) when a parameter this foreground depends on changes
        resolve.foreground = function(id)
        {
            unresolved.foreground = private$i.unresolved.foregrounds[[id]]
            
            # Store the old start and end times
            previously.resolved.foreground = private$i.resolved.foregrounds[[unresolved.foreground$quantity.name]][[id]]
            if (is.null(previously.resolved.foreground))
            {
                previous.start.time = Inf
                previous.end.time = -Inf
            }
            else
            {
                previous.start.time = previously.resolved.foreground$min.start.time
                previous.end.time = previously.resolved.foreground$max.end.time
            }
            
            # Store the resolved foreground
            resolved.foreground = unresolved.foreground$resolve.effects(private$i.parameters,
                                                                        error.prefix = paste0("Error resolving foreground effects for '", unresolved.foreground$quantity.name, "': "))
            private$i.resolved.foregrounds[[unresolved.foreground$quantity.name]][[id]] = resolved.foreground
            
            
            #-- Clear Dependent Values --#
            quantity.name = resolved.foreground$quantity.name
            
            # Clear all values after start of intervention (or previous start time, whichever comes first)
            clear.after.time = min(resolved.foreground$min.start.time,
                                   previous.start.time)
            clear.before.time = max(resolved.foreground$max.end.time,
                                    previous.end.time)
            
            private$clear.dependent.values(quantity.name, 
                                           clear.after.time = clear.after.time,
                                           clear.before.time = clear.before.time)
            
            # Clear times and value applies masks
            if (is.null(previously.resolved.foreground) || 
                (!unresolved.foreground$effect.times.are.resolved && !resolved.foreground$effect.times.equal(previously.resolved.foreground) ))
            {
                private$clear.quantity.foreground.self.times(quantity.name)
                
                # We only need to clear these if the times have changed
                private$clear.dependent.value.applies.masks(quantity.name, 
                                                            clear.after.time = clear.after.time,
                                                            clear.before.time = clear.before.time)
            }
        },
        

        ##----------------------------------##
        ##--  Calculating Outcome Values  --##
        ##--  (after the ODE solver runs) --##
        ##----------------------------------##

        prepare.outcomes.for.sim = function(ode.results,
                                            prior.simulation.set,
                                            prior.sim.index,
                                            is.degenerate)
        {
            if (!is.null(prior.simulation.set))
                stop("We need to work out how to integrate the prior simulation set")
            
            specification = private$get.specification()
            
            outcome.names = specification$get.outcome.names.for.sub.version(private$i.sub.version)
            
            if (!is.degenerate)
            {
                sapply(outcome.names, function(outcome.name){
                    private$calculate.outcome.numerator.and.denominator(outcome.name = outcome.name,
                                                                        ode.results = ode.results,
                                                                        specification = specification,
                                                                        is.degenerate = is.degenerate,
                                                                        error.prefix = paste0("Error calculating the value for outcome '", outcome.name, "': "))  
                })
            }
            
            outcome.numerators = lapply(outcome.names, function(outcome.name){
                
                outcome = specification$get.outcome(outcome.name)
                outcome.dim.names = c(list(year=private$i.outcome.value.times[[outcome.name]]),
                                      private$i.outcome.dim.names.sans.time[[outcome.name]])
                
                if (is.degenerate)
                    val = rep(as.numeric(NA), prod(sapply(outcome.dim.names, length)))
                else
                {
                    val = t(sapply(as.character(private$i.outcome.value.times[[outcome.name]]), function(y){
                        private$i.outcome.numerators[[outcome.name]][[y]]
                    }))
                }
                
                dim(val) = sapply(outcome.dim.names, length)
                dimnames(val) = outcome.dim.names
                
                val
            })
            names(outcome.numerators) = outcome.names
            
            outcome.denominators = lapply(outcome.names, function(outcome.name){
                
                if (is.null(private$i.outcome.denominators[[outcome.name]]))
                    NULL
                else
                {
                    outcome = specification$get.outcome(outcome.name)
                    outcome.dim.names = c(list(year=private$i.outcome.value.times[[outcome.name]]),
                                          private$i.outcome.dim.names.sans.time[[outcome.name]])
                    
                    if (is.degenerate)
                        val = outcome.numerators[[outcome.name]]
                    else
                    {
                        val = t(sapply(as.character(private$i.outcome.value.times[[outcome.name]]), function(y){
                            private$i.outcome.denominators[[outcome.name]][[y]]
                        }))
                    }                    
                    
                    dim(val) = sapply(outcome.dim.names, length)
                    dimnames(val) = outcome.dim.names
                    
                    val
                }
            })
            names(outcome.denominators) = outcome.names
            
            
            # Clear the values for numerators and denominators stored in the jheem object - we no longer need them
            private$i.outcome.numerators = list()
            private$i.outcome.denominators = list()
            
            list(numerators = outcome.numerators,
                 denominators = outcome.denominators)
        },

        # Depends on quantity.dim.names for quantities which this outcome has a direct, non.cumulative dependency on
        derive.outcome.numerator.dim.names.sans.time = function(outcome.name,
                                                                specification)
        {
            outcome = specification$get.outcome(outcome.name)
            if (is(outcome, 'intrinsic.model.outcome') || is(outcome, 'dynamic.model.outcome'))
                dim.names = private$i.outcome.dim.names.sans.time[[outcome.name]]
            else
            {
                numerator.depends.on.outcome.names = specification$get.outcome.numerator.direct.dependee.outcome.names(outcome.name)
                numerator.depends.on.quantity.names = specification$get.outcome.direct.dependee.quantity.names(outcome.name)
                
                dim.names.from.outcomes = NULL
                
                for (dep.on.outcome.name in numerator.depends.on.outcome.names)
                {
                    dim.names.from.outcomes = intersect.shared.dim.names(dim.names.from.outcomes,
                                                                         private$i.outcome.dim.names.sans.time[[dep.on.outcome.name]])
                }
                
                dim.names = intersect.joined.dim.names(private$i.outcome.dim.names.sans.time[[outcome.name]], dim.names.from.outcomes)
                for (dep.on.quantity.name in numerator.depends.on.quantity.names)
                {
                    quantity.dim.names = private$i.quantity.dim.names[[dep.on.quantity.name]]
                    
                    # Make the dimensions for the quantity are a subset of the dimensions we got from outcomes
                    if (!is.null(dim.names.from.outcomes))
                    {
                        excess.dimensions = setdiff(names(quantity.dim.names),
                                                    names(dim.names.from.outcomes))
                        
                        if (length(excess.dimensions)>0)
                            stop(paste0("Cannot calculate dim.names for the numerator of outcome '",
                                        outcome.name, ": dimension(s) ",
                                        collapse.with.and("'", excess.dimensions, "'"),
                                        " are present in quantity '", dep.on.quantity.name, 
                                        "', on which the outcome depends, but are not present in the derived dim.names of the outcome's numerator based on the outcome(s) on which it depends (",
                                        collapse.with.and("'", numerator.depends.on.outcome.names, "'"), ")"))
                    }
                    
                    # Join it to the rest
                    dim.names = intersect.joined.dim.names(dim.names, quantity.dim.names)
                }
                
                # Apply the subset.dimension.values
                dimensions.to.subset = intersect(names(outcome$subset.dimension.values),
                                                 names(dim.names))
                dim.names[dimensions.to.subset] = lapply(dimensions.to.subset, function(d){
                    intersect(dim.names[[d]], outcome$subset.dimension.values[[d]])
                })
            }
            
            # Set it
            private$i.outcome.numerator.dim.names.sans.time[[outcome.name]] = dim.names
        },

        clear.outcome.dependencies.on.quantity.dim.names = function(quantity.names, specification)
        {
            dependent.outcome.names = specification$get.direct.dependent.outcome.numerator.names(quantity.names)
                
            # clear the indices
            private$i.outcome.indices[[dependent.outcome.names]] = list()
            
            # We could try to be smarter, and only clear the indices if the numerator.dim.names change
            #  (we'd need to clear the outcome.indices[[outcome.name]]$value.from.quantity[[quantity.name]] regardless)
            #  but in practice, we are not going to change these dim.names very much so it doesn't seem worth it
        },

        calculate.outcome.numerator.and.denominator = function(outcome.name,
                                                               ode.results,
                                                               specification,
                                                               error.prefix = '')
        {
            error.prefix = paste0(error.prefix, "Error calculating the outcome values for '", outcome.name, "': ")
            
            if (is.null(private$i.outcome.numerators[[outcome.name]]))
            {
                #-- Initial Set-Up --#
                outcome = specification$get.outcome(outcome.name)
                outcome.dim.names = private$i.outcome.dim.names.sans.time[[outcome.name]]
                
                #-- Calculate the times --#
                if (is.null(private$i.outcome.value.times[[outcome.name]]))
                    private$calculate.outcome.value.times(outcome.name, specification=specification)
                
                #-- Calculate the values for all dependent outcomes --#
                depends.on.outcomes = specification$get.outcome.direct.dependee.outcome.names(outcome.name)
                sapply(depends.on.outcomes, 
                       private$calculate.outcome.numerator.and.denominator,
                       ode.results = ode.results,
                       specification = specification)
                
                #-- Calculate the dim.names --#
                if (is.null(private$i.outcome.numerator.dim.names.sans.time[[outcome.name]]))
                    private$derive.outcome.numerator.dim.names.sans.time(outcome.name=outcome.name, specification=specification)
                
                #-- Calculate the raw "value" of the outcome --#
                
                # If this is a dynamic or intrinsic outcome, pull the values from the ode results
                if (can.get.outcome.value.from.ode.output(outcome.name,
                                                          settings = private$i.diffeq.settings))
                {
                    raw.value = get.outcome.value.from.ode.output(outcome.name,
                                                                  settings = private$i.diffeq.settings,
                                                                  ode.results = ode.results,
                                                                  outcome.years = private$i.outcome.value.times[[outcome.name]])
                }
                else # calculate the value from the values of other outcomes/quantities
                {
                    #-- Figure out what times we need to pull from --#
                    if (is.null(private$i.outcome.non.cumulative.value.times[[outcome.name]]))
                        private$calculate.outcome.non.cumulative.value.times(outcome.name)
                    
                    if (length(private$i.outcome.non.cumulative.value.times[[outcome.name]])==0)
                    {   
                        times.to.pull = private$i.outcome.value.times[[outcome.name]]
                        is.after.time = rep(F, length(times.to.pull))
                    }
                    else
                    {
                        times.to.pull = private$i.outcome.non.cumulative.value.times[[outcome.name]]
                        is.after.time = private$i.outcome.non.cumulative.value.time.is.after.time[[outcome.name]]
                    }
                    char.times.to.pull = as.character(times.to.pull)
                    
                    #-- Map the bindings for dependee OUTCOMES to a list --#
                    bindings.of.outcomes = lapply(specification$get.outcome.numerator.direct.dependee.outcome.names(outcome.name), function(dep.on.outcome.name){
                        dep.on.outcome = specification$get.outcome(dep.on.outcome.name)
                        
                        if (is.null(private$i.outcome.non.cumulative.value.times[[dep.on.outcome.name]]))
                            private$calculate.outcome.non.cumulative.value.times(dep.on.outcome.name)
                        
                        if (is.null(private$i.outcome.indices[[outcome.name]]$value.from.outcome[[dep.on.outcome.name]]))
                            private$calculate.outcome.indices.from.outcome(outcome.name = outcome.name, dep.on.outcome.name = dep.on.outcome.name)
                        
                        binding = lapply(1:length(char.times.to.pull), function(i){
                            
                            time = char.times.to.pull[i]
                            after.or.not.mask = private$i.outcome.non.cumulative.value.time.is.after.time[[dep.on.outcome.name]]
                            
                            
                            if (!is.after.time[i])
                                after.or.not.mask = !after.or.not.mask
                            
                            if (is.null(dep.on.outcome$denominator.outcome))
                            {
                                if (is(dep.on.outcome, 'intrinsic.model.outcome'))
                                    dep.on.numerator = interpolate(private$i.outcome.numerators[[dep.on.outcome.name]], 
                                                                   value.times = private$i.outcome.value.times[[dep.on.outcome.name]],
                                                                   desired.time = times.to.pull[i])[[1]]
                                else
                                    dep.on.numerator = private$i.outcome.numerators[[dep.on.outcome.name]][after.or.not.mask][[time]]
                                
                                collapse.array.according.to.indices(arr = dep.on.numerator,
                                                                    small.indices = private$i.outcome.indices[[outcome.name]]$value.from.outcome[[dep.on.outcome.name]]$small.indices,
                                                                    large.indices = private$i.outcome.indices[[outcome.name]]$value.from.outcome[[dep.on.outcome.name]]$large.indices,
                                                                    small.n = private$i.outcome.indices[[outcome.name]]$value.from.outcome[[dep.on.outcome.name]]$small.n)
                            }
                            else
                            {
                                # NB - the intrinsic model outcomes ('infected' and 'uninfected' NEVER have a denominator set, so will never be used in this if statement)
                                
                                collapsed.denominator = collapse.array.according.to.indices(arr = private$i.outcome.denominators[[dep.on.outcome.name]][after.or.not.mask][[time]],
                                                                                        small.indices = private$i.outcome.indices[[outcome.name]]$value.from.outcome[[dep.on.outcome.name]]$small.indices,
                                                                                        large.indices = private$i.outcome.indices[[outcome.name]]$value.from.outcome[[dep.on.outcome.name]]$large.indices,
                                                                                        small.n = private$i.outcome.indices[[outcome.name]]$value.from.outcome[[dep.on.outcome.name]]$small.n)
                                
                                rv = collapse.array.according.to.indices(arr = private$i.outcome.numerators[[dep.on.outcome.name]][after.or.not.mask][[time]],
                                                                      small.indices = private$i.outcome.indices[[outcome.name]]$value.from.outcome[[dep.on.outcome.name]]$small.indices,
                                                                      large.indices = private$i.outcome.indices[[outcome.name]]$value.from.outcome[[dep.on.outcome.name]]$large.indices,
                                                                      small.n = private$i.outcome.indices[[outcome.name]]$value.from.outcome[[dep.on.outcome.name]]$small.n) /
                                    collapsed.denominator
                                    
                                
                                rv[collapsed.denominator==0] = 0
                                    
                            }
                        })
                        
                        names(binding) = char.times.to.pull
                        binding
                        
                        
                    })
                    names(bindings.of.outcomes) = specification$get.outcome.numerator.direct.dependee.outcome.names(outcome.name)
                    
                    
                    #-- Map the bindings for dependee QUANTITIES to a list --#
                    bindings.of.quantities = lapply(specification$get.outcome.direct.dependee.quantity.names(outcome.name), function(dep.on.quantity.name){
                        
                        if (is.null(private$i.outcome.indices[[outcome.name]]$value.from.quantity[[dep.on.quantity.name]]))
                            private$calculate.outcome.indices.from.quantity(outcome.name = outcome.name, dep.on.quantity.name = dep.on.quantity.name)
                        
                        binding = lapply(1:length(char.times.to.pull), function(i){
                            
                            time = as.character(char.times.to.pull[i])
                            val = NULL
                            if (private$i.quantity.is.static[dep.on.quantity.name])
                                val = private$i.quantity.values[[1]]
                            else if (is.after.time[i])
                                val = private$i.quantity.after.values[[dep.on.quantity.name]][[time]]
                            if (is.null(val))
                                val = private$i.quantity.values[[dep.on.quantity.name]][[time]]
                            
                            val[ private$i.outcome.indices[[outcome.name]]$value.from.quantity[[dep.on.quantity.name]] ]
                        })
                        
                        names(binding) = char.times.to.pull
                        binding
                    })
                    
                    bindings = c(bindings.of.outcomes, bindings.of.quantities)
                    
                    raw.value = outcome$calculate.values(desired.times = private$i.outcome.value.times[[outcome.name]],
                                                         bindings = bindings,
                                                         binding.times = times.to.pull,
                                                         cumulative.interval = 1,
                                                         error.prefix = '')
                }
                
                #-- Incorporate the denominator --#
                if (is.null(private$i.outcome.indices[[outcome.name]]$collapse.numerator))
                    private$calculate.outcome.collapse.value.indices(outcome.name, specification=specification)
                
                if (is.null(outcome$denominator.outcome))
                    denominator = NULL
                else
                {
                    if (is.null(private$i.outcome.denominators[[outcome$denominator.outcome]]))
                        denominator = private$i.outcome.numerators[[outcome$denominator.outcome]][as.character(private$i.outcome.value.times[[outcome.name]])]
                    else
                    {
                        denominator = lapply(as.character(private$i.outcome.value.times[[outcome.name]]), function(time){
                            
                            private$i.outcome.numerators[[outcome$denominator.outcome]][[time]] /
                                private$i.outcome.denominators[[outcome$denominator.outcome]]
                        })
                        names(denominator) = as.character(private$i.outcome.value.times[[outcome.name]])
                    }
                }

                if (!is.null(outcome$denominator.outcome) && !outcome$value.is.numerator)
                {
                    raw.value = lapply(as.character(private$i.outcome.value.times[[outcome.name]]), function(time){
                               
                        collapsed.denominator = collapse.array.according.to.indices(arr = denominator[[time]],
                                                                                 small.indices = private$i.outcome.indices[[outcome.name]]$collapse.denominator.for.numerator$small.indices,
                                                                                 large.indices = private$i.outcome.indices[[outcome.name]]$collapse.denominator.for.numerator$large.indices,
                                                                                 small.n = private$i.outcome.indices[[outcome.name]]$collapse.denominator.for.numerator$small.n)           
                        raw.value[[time]] * collapsed.denominator
                            
                    })
                    names(raw.value) = as.character(private$i.outcome.value.times[[outcome.name]])
                }
    
                numerator = lapply(raw.value,
                                   collapse.array.according.to.indices,
                                   small.indices = private$i.outcome.indices[[outcome.name]]$collapse.numerator$small.indices,
                                   large.indices = private$i.outcome.indices[[outcome.name]]$collapse.numerator$large.indices,
                                   small.n = private$i.outcome.indices[[outcome.name]]$collapse.numerator$small.n)
                
                if (!is.null(denominator))
                {
                    denominator = lapply(private$i.outcome.numerators[[outcome$denominator.outcome]],
                                         collapse.array.according.to.indices,
                                         small.indices = private$i.outcome.indices[[outcome.name]]$collapse.denominator$small.indices,
                                         large.indices = private$i.outcome.indices[[outcome.name]]$collapse.denominator$large.indices,
                                         small.n = private$i.outcome.indices[[outcome.name]]$collapse.denominator$small.n)
                }
                
                # Store it
                private$i.outcome.numerators[[outcome.name]] = numerator
                private$i.outcome.denominators[[outcome.name]] = denominator
                
                names(private$i.outcome.numerators[[outcome.name]]) = as.character(private$i.outcome.value.times[[outcome.name]])
                if (!is.null(private$i.outcome.denominators[[outcome.name]]))
                    names(private$i.outcome.denominators[[outcome.name]]) =
                        as.character(private$i.outcome.value.times[[outcome.name]])
            }
        },
        
        calculate.outcome.non.cumulative.value.all.applies.times = function()
        {
            
        },

        calculate.outcome.non.cumulative.value.applies.masks = function(outcome.name, specification, missing.times)
        {
            # Pull the quantity
            outcome = specification$get.outcome(outcome.name)
            
            char.times = as.character(missing.times)
            
            #-- Set up the value.all.applies.times --#
            if (is.null(private$i.outcome.non.cumulative.value.all.applies.for.time[[outcome.name]]))
                private$calculate.outcome.non.cumulative.value.all.applies.times(outcome.name)
            
            if (is.null(private$i.outcome.non.cumulative.value.applies.mask[[outcome.name]]))
                private$i.outcome.non.cumulative.value.applies.mask[[outcome.name]] = list()
            
            private$i.outcome.non.cumulative.after.value.applies.mask[[outcome.name]][char.times] = lapply(char.times, function(t){NULL})
            
            non.static.depends.on.quantities = specification$get.outcome.non.cumulative.dependent.outcome.names(outcome.name)
            non.cumulative.depends.on.outcomes = specification$get.outcome.direct.dependee.quantity.names(outcome.name)
            non.cumulative.depends.on.outcomes = non.cumulative.depends.on.outcomes[!private$i.quantity.is.static[non.cumulative.depends.on.outcomes]]
            
            if (length(non.static.depends.on.quantities)==0 && length(non.cumulative.depends.on.outcomes)==0)
            {
                private$i.quantity.value.applies.mask[[quantity.name]][char.times] = T
            }
            else
            {
                private$i.quantity.value.applies.mask[[quantity.name]][char.times] =
                    private$i.quantity.value.all.applies.for.time[[quantity.name]][char.times]
                
                for (time in missing.times)
                {
                    char.time = as.character(time)
                    
                    has.after.value = !is.null(private$quantity.after.values[[quantity.name]][char.time])
                    
                    for (is.after.time in c(F,T)[c(!private$i.quantity.value.all.applies.for.time[[quantity.name]][char.time], has.after.value)])
                    {
                        #-- Calculate the value.applies masks for each component --#
                        component.masks = lapply(1:quantity$n.components, function(i){
                            
                            comp = quantity$components[[i]]
                            
                            if (comp$value.type == 'numeric' || length(comp$depends.on)==0)
                                T
                            else 
                            {
                                dep.on.masks = lapply(comp$depends.on, function(dep.on){
                                    
                                    if (private$i.quantity.is.static[dep.on])
                                        dep.on.mask = private$i.quantity.value.applies.mask[[dep.on]][[1]]
                                    else 
                                    {
                                        dep.on.mask = NULL
                                        if (is.after.time)
                                            dep.on.mask = private$i.quantity.after.value.applies.mask[[dep.on]][[char.time]]
                                        if (is.null(dep.on.mask))
                                            dep.on.mask = private$i.quantity.value.applies.mask[[dep.on]][[char.time]]
                                    }
                                    
                                    if (length(dep.on.mask)==1)
                                        dep.on.mask
                                    else
                                    {
                                        dep.on.indices = private$i.quantity.mapping.indices[[quantity.name]]$components.depends.on[[i]][[dep.on]]
                                        dep.on.mask[dep.on.indices]
                                    }
                                })
                                
                                if (comp$value.type == 'expression' || comp$value.type == 'character')
                                {
                                    combined.mask = dep.on.masks[[1]]
                                    for (add.mask in dep.on.masks[-1])
                                        combined.mask = combined.mask | add.mask
                                    
                                    combined.mask
                                }
                                else # comp$value == 'function'
                                {
                                    any(sapply(dep.on.masks, any))
                                }
                            }
                        })
                        
                        #-- Fold the masks from all the components together --#
                        quant.value.applies = NULL
                        
                        for (i in 1:quantity$n.components)
                        {
                            comp = quantity$components[[i]]
                            comp.value.applies = component.masks[[i]]
                            
                            #-- Pull the access/expand indices --#
                            expand.indices = private$i.quantity.mapping.indices[[quantity.name]]$components.expand[[i]]
                            access.indices = private$i.quantity.mapping.indices[[quantity.name]]$components.access[[i]]
                            
                            if (length(comp.value.applies)==1)
                            {
                                if (is.null(access.indices))
                                    quant.value.applies = comp.value.applies
                                else if (comp$apply.function=='overwrite')
                                    quant.value.applies[access.indices] = comp.value.applies
                                else if (comp.value.applies)
                                    quant.value.applies[access.indices] = T
                            }
                            else
                            {
                                if (is.null(access.indices))
                                    quant.value.applies = comp.value.applies[expand.indices]
                                else if (comp$apply.function=='overwrite')
                                    quant.value.applies[access.indices] = comp.value.applies[expand.indices]
                                else
                                    quant.value.applies[access.indices] = quant.value.applies[access.indices] | comp.value.applies[expand.indices]
                            }
                        }
                        
                        if (is.null(quant.value.applies) || any(is.na(quant.value.applies)) || !any(quant.value.applies))
                            browser()
                        
                        if (all(quant.value.applies))
                            quant.value.applies = T
                        
                        if (is.after.time)
                            private$i.quantity.after.value.applies.mask[[quantity.name]][[char.time]] = quant.value.applies
                        else
                            private$i.quantity.value.applies.mask[[quantity.name]][[char.time]] = quant.value.applies
                    }
                }
            }
            
            #-- Order it and return --#
            char.all.times = as.character(private$i.quantity.value.times[[quantity.name]])
            private$i.quantity.value.applies.mask[[quantity.name]] = private$i.quantity.value.applies.mask[[quantity.name]][char.all.times]
            private$i.quantity.after.value.applies.mask[[quantity.name]] = private$i.quantity.after.value.applies.mask[[quantity.name]][char.all.times]
            
            invisible(self)
        },
        
        # Depends on outcome.numerator.dim.names.sans.time
        calculate.outcome.indices.from.outcome = function(outcome.name, dep.on.outcome.name)
        {
            private$i.outcome.indices[[outcome.name]]$value.from.outcome[[dep.on.outcome.name]] =
                get.collapse.array.indices(small.arr.dim.names = private$i.outcome.numerator.dim.names.sans.time[[outcome.name]],
                                           large.arr.dim.names = private$i.outcome.dim.names.sans.time[[dep.on.outcome.name]])
        },

        # Depends on outcome.numerator.dim.names.sans.time, quantity.dim.names
        calculate.outcome.indices.from.quantity = function(outcome.name, dep.on.quantity.name)
        {
            private$i.outcome.indices[[outcome.name]]$value.from.quantity[[dep.on.quantity.name]] =
                get.expand.array.indices(to.expand.dim.names = private$i.quantity.dim.names[[dep.on.quantity.name]],
                                         target.dim.names = private$i.outcome.numerator.dim.names.sans.time[[outcome.name]])
        },

        # Depends on outcome.numerator.dim.names.sans.time
        calculate.outcome.collapse.value.indices = function(outcome.name, specification)
        {
            private$i.outcome.indices[[outcome.name]]$collapse.numerator = 
                get.collapse.array.indices(small.arr.dim.names = private$i.outcome.dim.names.sans.time[[outcome.name]],
                                           large.arr.dim.names = private$i.outcome.numerator.dim.names.sans.time[[outcome.name]])
            
            outcome = specification$get.outcome(outcome.name)
            if (!is.null(outcome$denominator.outcome))
            {
                private$i.outcome.indices[[outcome.name]]$collapse.denominator =
                    get.collapse.array.indices(small.arr.dim.names = private$i.outcome.dim.names.sans.time[[outcome.name]],
                                            large.arr.dim.names = private$i.outcome.dim.names.sans.time[[outcome$denominator.outcome]])
                
                if (!outcome$value.is.numerator)
                {
                    private$i.outcome.indices[[outcome.name]]$collapse.denominator.for.numerator =
                        get.collapse.array.indices(small.arr.dim.names = private$i.outcome.numerator.dim.names.sans.time[[outcome.name]],
                                                large.arr.dim.names = private$i.outcome.dim.names.sans.time[[outcome$denominator.outcome]])
                }
            }
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
        }
            
    )
)