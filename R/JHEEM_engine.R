
##----------------------##
##----------------------##
##-- PUBLIC INTERFACE --##
##----------------------##
##----------------------##


##-----------------##
##-- CONSTRUCTOR --##
##-----------------##


#'@description Create a JHEEM Engine (to run simulations for a given version and location)
#'
#'@param version The name of the version to run. Must have had a model specification registered with \code{\link{register.model.specification}}
#'@param location A single character value indicating the location of the model
#'
#'@export
create.jheem.engine <- function(version,
                                location)
{
    JHEEM.ENGINE$new(version, location)
}

##-------------------------------------------------##
##-- FUNCTIONS to PRODUCE OUTPUT from the ENGINE --##
##-------------------------------------------------##

#'@description Perform Pre-Calculations Needed to Run a Simulation From a JHEEM Engine
#'
#'@param jheem.engine A jheem.engine object (created by \code{\link{create.jheem.engine}})
#'@param start.year,end.year The years across which the simulation is to run
#'@param check.consistency Whether to perform consistency checks during calculations, which can catch errors, but imposes a computational cost. In general, this is likely only necessary on the first time an engine is crunched or run
#'
#'@export
crunch.jheem.engine <- function(jheem.engine,
                                start.year,
                                end.year,
                                check.consistency = T)
{
    if (!is(jheem.engine, "R6") || !is(jheem.engine, "jheem.engine"))
        stop("jheem.engine must be an R6 object of class 'jheem.engine'")
    
    jheem.engine$crunch(start.year = start.year,
                        end.year = end.year,
                        check.consistency = check.consistency)
}

#'@description Produce a Simulation from a JHEEM Engine
#'
#'@inheritParams crunch.jheem.engine
#'@param max.run.time.seconds The maximum number of seconds a simulation is allowed to run before being terminated
#'@param prior.sim A jheem.simulation object from which to start this simulation. NULL if not running from a prior simulation
#'@param keep.years The years for which to keep simulation data
#'@param atol,rtol The absolute and relative tolerance parameters to be passed along to the differential equation solver
#'
#'@return A jheem.simulation object
#'
#'@export
run.jheem.engine <- function(jheem.engine,
                             start.year, 
                             end.year,
                             check.consistency = T,
                             max.run.time.seconds=Inf,
                             prior.sim=NULL,
                             keep.years=start.year:end.year,
                             atol=1e-04, rtol=1e-04)
{
    
}


##-----------------------------------------------##
##-- FUNCTIONS to MODIFY the ENGINE'S SETTINGS --##
##-----------------------------------------------##

#'@description Set the Value of a Model Element
#'
#'@inheritParams run.jheem.engine
#'@param element.name The name of the model element to update a value for
#'@param value The new value for the model element
#'@param check.consistency A logical indicator whether to sanitize arguments and check for consistency during calculations. Setting to F increases performance
#'
#'@export
set.element.value <- function(jheem.engine,
                              element.name,
                              value,
                              check.consistency=T)
{
    
}

#'@description Set Main Effect Alpha Values for a Functional Form for a Model Element
#'
#'@details Alphas for a 'main effect' apply to all values in a dimension (as opposed to 'interaction effects', which apply to combinations of values)
#'
#'@inheritParams set.element.value
#'@param element.name The name of the model element whose functional form we wish to modify
#'@param alpha.name The name of the functional form parameter that we want to set alphas for
#'@param values A numeric vector of values
#'@param applies.to.dimension.values Either a vector (character or integer) or a list where each element is either a single character value or single integer value. Must have the same length as values, indicating the compartment to which each value in values applies
#'@param dimensions The dimensions to which values in applies.to.dimension.values apply. Can be either (1) a character vector with the same length as applies.to.dimension.values with the corresponding dimension for each value, (2) a single character value - a dimension that applies to all values, or (3) NULL, in which case the dimensions are inferred for applies.to.dimension.values (this is computationally slower)
#'
#'@export
set.element.functional.form.main.effect.alphas <- function(jheem.engine,
                                                           element.name,
                                                           alpha.name,
                                                           values,
                                                           applies.to.dimension.values=names(values),
                                                           dimensions=names(applies.to.dimension.values),
                                                           check.consistency=T)
{
    
}

#'@description Set Interaction Alpha Values for a Functional Form for a Model Element
#'
#'@details Alphas for an 'interaction' will apply to every combination of applies.to.dimension.values from different dimensions (as opposed to 'main effect' apply to all values in a dimension
#'
#'@inheritParams set.element.functional.form.main.effect.alphas
#'@param value A single numeric value, which will apply to every combination of applies.to.dimension.values which are from different dimensions
#'@param applies.to.dimension.values Either a vector (character or integer) or a list where each element is either a single character value or single integer value, indicating the compartments to which value applies
#'
#'@export
set.element.functional.form.interaction.alphas <- function(jheem.engine,
                                                           element.name,
                                                           alpha.name,
                                                           value,
                                                           applies.to.dimension.values=names(values),
                                                           dimensions=names(applies.to.dimension.values),
                                                           check.consistency=T)
{
    
}

#' Still need to flesh out this interface
set.element.foreground <- function(jheem.engine,
                                   foreground,
                                   check.consistency=T)
{
    
}

#'@description Set the Times From and To Which the Functional Form Determines a Model Element's Value
#'
#'@inheritParams set.element.value
#'@param element.name The name of the model element to set functional.form times for
#'@param from.time,to.time The time from or to which the functional form is active
#'
#'@export
set.element.functional.form.from.time <- function(jheem.engine,
                                                  element.name,
                                                  from.time,
                                                  check.consistency=T)
{
    
}


#'@describeIn set.element.functional.form.from.time
#'
#'@export
set.element.functional.form.to.time <- function(jheem.engine,
                                                element.name,
                                                to.time,
                                                check.consistency=T)
{
    
}


set.element.ramp.times <- function(jheem.engine,
                                   element.name,
                                   times,
                                   indices=1:length(times),
                                   check.consistency=T)
{
    
}

set.element.ramp.values <- function(jheem.engine,
                                    element.name,
                                    values,
                                    indices=1:length(values),
                                    check.consistency=T)
{
    
}

set.element.taper.times <- function(jheem.engine,
                                    element.name,
                                    times,
                                    indices=1:length(times),
                                    check.consistency=T)
{
    
}

set.element.taper.values <- function(jheem.engine,
                                    element.name,
                                    values,
                                    indices=1:length(values),
                                    check.consistency=T)
{
    
}

set.element.functional.form.future.slope <- function(jheem.engine,
                                                     element.names,
                                                     slope,
                                                     check.consistency=T)
{
    
}


set.element.functional.form.future.slope.after.time <- function(components,
                                                                element.names,
                                                                after.year,
                                                                check.consistency=T)
{
    
}

##------------------##
##-- DEPENDENCIES --##
##------------------##


##-----------------------##
##-----------------------##
##-- CLASS DEFINITIONS --##
##-----------------------##
##-----------------------##

JHEEM.ENGINE.CODE.ITERATION = '2.0'

JHEEM.ENGINE = R6::R6Class(
    'jheem.engine',
    inherit = JHEEM.ENTITY,
    portable = F,
    
    public = list(
        
        ##-----------------##
        ##-- CONSTRUCTOR --##
        ##-----------------##
        
        initialize = function(version, location)
        {
            # Call the superclass constructor
            super$initialize(version = version,
                             location = location,
                             code.iteration = JHEEM.ENGINE.CODE.ITERATION,
                             intervention.code = NA,
                             calibration.index = NA,
                             type = 'JHEEM Engine',
                             error.prefix = "Cannot create JHEEM Engine: ")
            
            private$set.up()
        },
        
        
        ##------------------------##
        ##-- MAIN RUN FUNCTIONS --##
        ##------------------------##
        
        crunch = function(start.year,
                          end.year,
                          check.consistency=T)
        {
            # If the specification has changed since we last crunched/set-up, reset
            specification = get.specification.for.version(private$i.version)
            if (specification$iteration != private$i.specification.info$specification.iteration)
                private$set.up()
            
            # Set the times
            set.crunch.years(start.year = start.year,
                             end.year = end.year,
                             error.prefix = paste0("Error preparing JHEEM Engine to run: "))

            # Do the work
            specification = get.specification()
            sapply(specification$top.level.quantity.names, calculate.quantity.value,
                   check.consistency = check.consistency)
        },
        
        run = function()
        {
            # If the specification has changed since we last crunched/set-up, reset
            specification = get.specification.for.version(private$i.version)
            if (specification$iteration != private$i.specification.info$specification.iteration)
                private$set.up()
            
            # Do the work
        },
        
        ##------------------------------##
        ##-- MODIFY ELEMENT FUNCTIONS --##
        ##------------------------------##
    
        set.element.value = function(element.name,
                                      value,
                                      check.consistency=T)
        {
            
            #-- Clear Dependencies --#
            
            # Clear all values
            clear.dependent.values(element.name)
            
            # Clear dim.names
            clear.dim.names(element.name)
            
            # No need to clear times
            
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
                                                                   check.consistency=T)
        {
            
            #-- Clear Dependencies --#
            
            # Clear all values
            clear.dependent.values(element.name)
            
            # Clear dim.names
            clear.dim.names(element.name)
            
            # No need to clear times
            
            
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
                                                                   check.consistency=T)
        {
            
            #-- Clear Dependencies --#
            
            # Clear all values
            clear.dependent.values(element.name)
            
            # Clear dim.names
            clear.dim.names(element.name)
            
            # No need to clear times
            
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
        set.element.foreground = function(element.name,
                                           foreground,
                                           check.consistency=T)
        {
            private$i.quantity.is.static[[element.name]] = F
            private$i.quantity.is.static[[specification$get.dependent.quantity.names(element.name)]] = F
            
            # Clear dim.names
            # Clear all values after start of intervention
            # Clear times
        },
        
        set.element.functional.form.from.time = function(element.name,
                                                         from.time,
                                                         check.consistency=T)
        {
            previous.from.time = i.element.backgrounds[[element.name]]$functional.form.from.time
            
            
            #-- Clear Dependencies --#
            
            # Clear values for all times prior to max(old from time, new from time)
            times.to.clear.values.for.mask = i.quantity.value.times[[element.name]] < max(i.element.backgrounds[[element.name]]$functional.form.from.time,
                                                                                          previous.from.time)
            clear.dependent.values(element.name, times = i.quantity.value.times[[element.name]][times.to.clear.values.for.mask])
            
            # Clear times
            clear.dependent.times(element.name)
            
            # No need to clear dim.names
            
            
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
                                                        check.consistency=T)
        {
            previous.from.time = i.element.backgrounds[[element.name]]$functional.form.to.time
            
            # 
            # Clear times
            # No need to clear dim.names
            
            
            #-- Clear Dependencies --#
            
            # Clear values for all times after min(old to time, new to time)
            times.to.clear.values.for.mask = i.quantity.value.times[[element.name]] > min(i.element.backgrounds[[element.name]]$functional.form.to.time,
                                                                                          previous.to.time)
            clear.dependent.values(element.name, times = i.quantity.value.times[[element.name]][times.to.clear.values.for.mask])
            
            # Clear times
            clear.dependent.times(element.name)
            
            # No need to clear dim.names
            
            
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
                                           check.consistency=T)
        {
            
            
            #-- Clear Dependencies --#
            
            # Clear values for all times prior to functional.form.from.time
            times.to.clear.values.for.mask = i.quantity.value.times[[element.name]] < i.element.backgrounds[[element.name]]$functional.form.from.time
            clear.dependent.values(element.name, times = i.quantity.value.times[[element.name]][times.to.clear.values.for.mask])
            
            # Clear times
            clear.dependent.times(element.name)
            
            # No need to clear dim.names
            
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
                                            check.consistency=T)
        {
            
            #-- Clear Dependencies --#
            
            # Clear values for all times prior to functional.form.from.time
            times.to.clear.values.for.mask = i.quantity.value.times[[element.name]] < i.element.backgrounds[[element.name]]$functional.form.from.time
            clear.dependent.values(element.name, times = i.quantity.value.times[[element.name]][times.to.clear.values.for.mask])
            
            # No need to clear times
            # No need to clear dim.names
            
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
                                            check.consistency=T)
        {
            
            #-- Clear Dependencies --#
            
            # Clear values for all times after functional.form.to.time
            times.to.clear.values.for.mask = i.quantity.value.times[[element.name]] > i.element.backgrounds[[element.name]]$functional.form.to.time
            clear.dependent.values(element.name, times = i.quantity.value.times[[element.name]][times.to.clear.values.for.mask])
            
            # Clear times
            clear.dependent.times(element.name)
            
            
            #-- Log Instruction --#
            log.instruction(fn.name='set.element.taper.times', 
                            times=times,
                            indices=indices,
                            check.consistency=check.consistency)
            
            # No need to clear dim.names
            
            
            #-- Done --#
            invisible(self)
        },
        
        set.element.taper.values = function(element.name,
                                             values,
                                             indices=1:length(values),
                                             check.consistency=T)
        {
            
            #-- Clear Dependencies --#
            
            # Clear values for all times after functional.form.to.time
            times.to.clear.values.for.mask = i.quantity.value.times[[element.name]] > i.element.backgrounds[[element.name]]$functional.form.to.time
            clear.dependent.values(element.name, times = i.quantity.value.times[[element.name]][times.to.clear.values.for.mask])
            
            # No need to clear times
            # No need to clear dim.names
            
            
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
                                                            check.consistency=T)
        {
  
            #-- Clear Dependencies --#
            
            # Clear values for all times after functional.form.future.slope.after.time
            times.to.clear.values.for.mask = i.quantity.value.times[[element.name]] > i.element.backgrounds[[element.name]]$future.slope.after.time
            clear.dependent.values(element.name, times = i.quantity.value.times[[element.name]][times.to.clear.values.for.mask])
            
            # No need to clear times
            # No need to clear dim.names
            
            
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
                                                                       check.consistency=T)
        {
            previous.functional.form.after.time = i.element.backgrounds[[element.name]]$future.slope.after.time
            
 
            
            #-- Clear Dependencies --#
            
            # Clear values for all times after min(old, new functional.form.future.slope.after.time)
            times.to.clear.values.for.mask = i.quantity.value.times[[element.name]] > min(i.element.backgrounds[[element.name]]$future.slope.after.time,
                                                                                          previous.functional.form.after.time)
            clear.dependent.values(element.name, times = i.quantity.value.times[[element.name]][times.to.clear.values.for.mask])
            
            # No need to clear times
            # No need to clear dim.names
            
            
            #-- Log Instruction --#
            log.instruction(fn.name='set.element.functional.form.future.slope.after.time', 
                            element.names=element.names,
                            after.time=after.time,
                            check.consistency=check.consistency)
            
            #-- Done --#
            invisible(self)
        }
        
    ),
    
    active = list(
        
        
    ),
    
    private = list(
        
        ##----------------------##
        ##----------------------##
        ##-- MEMBER VARIABLES --##
        ##----------------------##
        ##----------------------##
        
        i.specification.info = NULL,
        
        i.element.backgrounds = NULL,
        i.instructions = NULL,
        
        i.quantity.max.dim.names = NULL,
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
        
        
        ##---------------------##
        ##---------------------##
        ##-- PRIVATE METHODS --##
        ##---------------------##
        ##---------------------##
        
        ##------------##
        ##-- SET UP --##
        ##------------##
        
        set.up = function()
        {
            specification = get.compiled.specification.for.version(version)
            
            # Pull the specification.info
            private$i.specification.info = SPECIFICATION.INFO$new(version = version,
                                                                  location = private$i.location,
                                                                  error.prefix = paste0("Error initializing JHEEM Engine, version '", version, "' for location '", location, "': "))
            
            # Finalize max.dim.names and applies.to for quantities and components
            private$i.quantity.max.dim.names = lapply(specification$quantities, function(quantity){
                private$i.specification.info$apply.aliases(quantity$max.dim.names,
                                                           error.prefix=paste0("Error finalizing max.dim.names for model quantity ", 
                                                                               quantity$get.original.name(specification$version)))
            })
            
            private$i.quantity.component.max.dim.names = lapply(specification$quantities, function(quantity){
                lapply(quantity$components, function(comp){
                    private$i.specification.info$apply.aliases(comp$max.dim.names,
                                                               error.prefix=paste0("Error finalizing max.dim.names for the ",
                                                                                   get.ordinal(i-1), " subset of model quantity ", 
                                                                                   quantity$get.original.name(specification$version)))
                    # ^ Should never trigger an error on the first component since it is the same as the quantity dim.names calculated above
                })
            })
            
            private$i.quantity.component.applies.to = lapply(specification$quantities, function(quantity){
                lapply(quantity$components, function(comp){
                    private$i.specification.info$apply.aliases(comp$applies.to,
                                                               error.prefix=paste0("Error finalizing applies.to for the ",
                                                                                   get.ordinal(i-1), " subset of model quantity ", 
                                                                                   quantity$get.original.name(specification$version)))
                    # ^ Should never trigger an error on the first component since applies.to is NULL for the first component
                })
            })
            
            # Set up the element backgrounds
            private$i.element.backgrounds = lapply(specification$element.names, function(elem.name){
                elem = specification$get.quantity(elem.name)
                bkgd = elem$get.element.background(specification.info = private$i.specification.info,
                                                   error.prefix = paste0("Error creating JHEEM Engine for version '", version, "' and location '", location, "': "))
                
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
            names(private$i.element.backgrounds) = specification$element.names
            
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
            
            # Re-process any instructions
            for (instr in private$i.instructions)
                private$execute.instruction(instr)
            
            # Going to need to do something about foregrounds here
        },
        
        
        ##------------------------##
        ##-- START and END YEAR --##
        ##------------------------##
        
        set.crunch.years = function(start.year,
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
                functional.form.times = ceiling(from.time):floor(to.time)
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
                            bindings$specification.info = private$i.specification.info
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
                            
                            x = rv
                            # Fold it in to the rv
                            if (is.null(access.indices))
                                rv = value[expand.indices]
                            else if (comp$apply.function=='overwrite')
                                rv = do_access_overwrite(dst=rv, src=value, dst_indices=access.indices, src_indices=expand.indices)
                                # rv[access.indices] = value[expand.indices]
                            else if (comp$apply.function=='add')
                                rv = do_access_add(dst=rv, src=value, dst_indices=access.indices, src_indices=expand.indices)
                                # rv[access.indices] = rv[access.indices] + value[expand.indices]
                            else if (comp$apply.function=='subtract')
                                rv = do_access_subtract(dst=rv, src=value, dst_indices=access.indices, src_indices=expand.indices)
                                # rv[access.indices] = rv[access.indices] - value[expand.indices]
                            else if (comp$apply.function=='multiply')
                                rv = do_access_multiply(dst=rv, src=value, dst_indices=access.indices, src_indices=expand.indices)
                                # rv[access.indices] = rv[access.indices] * value[expand.indices]
                            else if (comp$apply.function=='divide')
                                rv = do_access_divide(dst=rv, src=value, dst_indices=access.indices, src_indices=expand.indices)
                                # rv[access.indices] = rv[access.indices] / value[expand.indices]
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

                print(paste0(paste0(rep(" ", depth), collapse=''),
                             "* done calculating '", quantity.name, "'"))
                invisible(self)
            }
        },
        
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
                                      betas = private$i.element.backgrounds[[element.name]]$functional.form$betas,
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
                                              variable.name.for.error = 'the returned value',
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
                            sapply(names(private$i.quantity.component.dim.names[[quantity.name]][[i]], function(d){
                                if (any(d==names(private$i.quantity.component.applies.to[[quantity.name]][[i]])))
                                {
                                    if (i>1 && !setequal(dim.names[[d]], private$i.quantity.component.applies.to[[quantity.name]][[i]]))
                                        stop(paste0("Error calculating dimnames for quantity ",
                                                    quantity$get.original.name(self$version),
                                                    ": the dimnames of the ",
                                                    get.ordinal(i-1),
                                                    " subset do not match the applies.to values for dimension '", d, "'"))
                                }
                                else
                                {
                                    if (!setequal(dim.names[[d]], private$i.quantity.component.dim.names[[quantity.name]][[i]][[d]]))
                                        stop(paste0("Error calculating dimnames for quantity ",
                                                    quantity$get.original.name(self$version),
                                                    ": the dimnames of its sub-components do not align in dimension '", d, "'"))
                                }
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
            
            
            #--  If the dim.names have changed from previous, clear dependencies --#
            #    (and set these dimnames to be the reference - ie crunched - dimnames)
            if (!is.null(i.crunched.quantity.dim.names[[quantity.name]]) &&
                !dim.names.equal(i.crunched.quantity.dim.names[[quantity.name]],
                                 i.quantity.dim.names[[quantity.name]]))
            {
                clear.dependent.on.quantity.dim.names(quantity.name)
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
        }
        
    )
)