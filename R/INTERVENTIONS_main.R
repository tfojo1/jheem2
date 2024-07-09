

##---------------------------------------------------------##
##---------------------------------------------------------##
##-- THE INTERFACE for CREATING "STANDARD" INTERVENTIONS --##
##---------------------------------------------------------##
##---------------------------------------------------------##

#'@name Create an Intervention
#'
#'@param ... Either (a) objects of class 'target.population', (b) objects of class 'intervention.effect', or (d) lists containing only 'target.population' or 'intervention.effect' objects. Must contain at least one 'target.population' object and at least one 'intervention.effect' object
#'@param code A brief (2-30 character) string that will uniquely identify this intervention
#'@param name
#'@param parameter.distribution
#'
#'@details Creates a 'jheem.intervention' object where all the intervention effects given in ... apply to all the target populations given in ...
#'
#'@export
create.intervention <- function(..., code=NULL, name=NULL, parameter.distribution=NULL)
{
    #-- Parse ... for target.populations and intervention.effects --#
    target.populations = list()
    intervention.effects = list()
    
    for (elem in list(...))
    {
        if (is(elem, 'target.population'))
            target.populations = c(target.populations, list(elem))
        else if (is(elem, 'jheem.intervention.effect'))
            intervention.effects = c(intervention.effects, list(elem))
        else if (is.list(elem))
        {
            for (sub.elem in elem)
            {
                if (is(sub.elem, 'target.population'))
                    target.populations = c(target.populations, list(sub.elem))
                else if (is(sub.elem, 'jheem.intervention.effect'))
                    intervention.effects = c(intervention.effects, list(sub.elem))
                else
                    stop("Cannot create standard intervention: the elements of ... must all be either target.population objects, jheem.intervention.effect objects, or lists which contain only target.population and jheem.intervention.effect objects")
            }
        }
        else
            stop("Cannot create standard intervention: the elements of ... must all be either target.population objects, jheem.intervention.effect objects, or lists which contain only target.population and jheem.intervention.effect objects")
    }
    
    if (length(target.populations)==0)
        stop("Cannot create standard intervention: ... must contain at least one target.population object")
    
    if (length(intervention.effects)==0)
        stop("Cannot create standard intervention: ... must contain at least one intervention.effect object")
    
    joint.target.population = union.target.populations(target.populations)
    
    #-- Group into foregrounds --#
    foreground.quantity.names = sapply(intervention.effects, function(eff){eff$quantity.name})
    tabled.foreground.quantity.names = table(foreground.quantity.names)
    if (any(tabled.foreground.quantity.names>1))
        stop(paste0("Cannot create standard intervention: a given quantity cannot be affected by more than one intervention effect, but multiple intervention effects affect ",
                    ifelse(sum(tabled.foreground.quantity.names>1)==1, "quantity ", "quantities "),
                    collapse.with.and("'", names(tabled.foreground.quantity.names)[tabled.foreground.quantity.names>1], "'")))
    
    foregrounds = lapply(intervention.effects, function(eff){
        create.model.foreground(eff, joint.target.population)
    })
    
    JHEEM.STANDARD.INTERVENTION$new(foregrounds = foregrounds,
                                    code = code,
                                    name = name,
                                    parameter.distribution = parameter.distribution)
}

#'@name Join Multiple Interventions into a Single Intervention
#'
#'@inheritParams create.intervention
#'@param ... One or more interventions to join. These may be either objects of class jheem.intervention or lists which contain only objects of class jheem.intervention
#'@param sequential
#'
#'@export
join.interventions <- function(..., code=NULL, name=NULL, parameter.distribution=NULL, sequential=F)
{
    args = list(...)
    
    sub.interventions = list()
    for (i in 1:length(args))
    {
        elem = args[[i]]
        if (is.list(elem))
        {
            for (j in 1:length(elem))
            {
                sub.elem = elem[[j]]
                if (!is(sub.elem, 'jheem.intervention'))
                    stop(paste0("If the elements of ... are lists, theey must contain only objects of class 'jheem.intervention'. The ",
                                get.ordinal(j), " element of the ",
                                get.ordinal(i), " element of ... is not a jheem.intervention"))
            }
            sub.interventions = c(sub.interventions, elem)
        }
        else
        {
            if (is(elem, 'jheem.intervention'))
            {
                sub.interventions = c(sub.interventions,
                                          list(elem))
            }
            else
                stop(paste0("... must contain only objects of class 'jheem.intervention' or lists that contain only 'jheem.intervention' objects. The ",
                            get.ordinal(i), " element of ... was neither"))
        }
    }
    
    if (length(sub.interventions)==0)
        stop("You must pass at least two interventions to join")
    else if (length(sub.interventions)==1)
        sub.interventions[[1]]
    else if (any(!sapply(sub.interventions, is, "jheem.standard.intervention")))
        stop("Right now we only know how to combine jheem.standard.interventions")
    else if (sequential)
        stop("We haven't yet implemented sequential combination of interventions")
    else
        join.standard.interventions(interventions = sub.interventions,
                                    code = code,
                                    name = name,
                                    parameter.distribution = parameter.distribution)
}

join.standard.interventions <- function(interventions, code, name, parameter.distribution)
{
    all.foregrounds = list()
    for (int in interventions)
        all.foregrounds = c(all.foregrounds, int$foregrounds)
    
    foreground.quantity.names = sapply(all.foregrounds, function(frgd){frgd$quantity.name})
    
    new.foregrounds = lapply(unique(foreground.quantity.names), function(quantity.name){
        to.combine = all.foregrounds[foreground.quantity.names==quantity.name]
        
        target.populations = list()
        intervention.effects = list()
        
        for (frgd in to.combine)
        {
            target.populations = c(target.populations, frgd$target.populations)
            intervention.effects = c(intervention.effects, frgd$effects)
        }
        
        create.model.foreground(target.populations, intervention.effects, 
                                error.prefix = paste0("Cannot join standard interventions with respect to quantity '", quantity.name, "': "))
    })
    
    if (is.null(parameter.distribution))
    {
        for (int in interventions)
        {
            if (!is.null(int$parameter.distribution))
            {
                if (is.null(parameter.distribution))
                    parameter.distribution = int$parameter.distribution
                else
                {
                    overlapping.parameter.names = intersect(parameter.distribution@var.names,
                                                            int$parameter.distribution@var.names)
                    
                    if (length(overlapping.parameter.names)>0)
                        stop(paste0("Cannot join standard interventions: the same parameter ",
                                    ifelse(length(overlapping.parameter.names)==1, "name", "names"),
                                    "(", collapse.with.and("'", overlapping.parameter.names, "'"), ")",
                                    ifelse(length(overlapping.parameter.names)==1, "is", "are"),
                                    " referenced by the parameter.distribution from more than one intervention to combine. Consider explicitly specifying parameter.distribution in the call to join.interventions() to override automatic combination"))
                }
            }
        }
    }
    
    JHEEM.STANDARD.INTERVENTION$new(foregrounds = new.foregrounds,
                                    code = code,
                                    name = name,
                                    parameter.distribution = parameter.distribution)
}

#'@export
is.no.intervention <- function(intervention)
{
    is(intervention, 'null.intervention')
#    if (!is(intervention, 'jheem.intervention') && !R6::is.R6(intervention))
#        stop()
    
#    length(intervention$reduce.to.standard.intervention()$target.element.names) == 0
}


##------------------------------------##
##------------------------------------##
##-- INTERVENTION MANAGER INTERFACE --##
##------------------------------------##
##------------------------------------##

INTERVENTION.MANAGER = new.env()
INTERVENTION.MANAGER$interventions = list()

#'@export
get.intervention <- function(code, throw.error.if.missing=T, error.prefix='')
{
    get.intervention.from.code(code = code,
                               throw.error.if.missing = throw.error.if.missing,
                               error.prefix = error.prefix)
}

# an internal function, aliased to avoid clashes with R6 methods of the same name
get.intervention.from.code <- function(code, throw.error.if.missing=T, error.prefix='')
{
    if (!is.character(error.prefix) || length(error.prefix)!=1 || is.na(error.prefix))
        stop(paste0("Error in get.intervention(): 'error.prefix' must be a single, non-NA character value"))
    
    if (!is.character(code) || length(code)!=1 || is.na(code))
        stop(paste0(error.prefix, "'code' must be a single, non-NA character value"))
    
    rv = INTERVENTION.MANAGER$interventions[[code]]
    if (throw.error.if.missing && is.null(rv))
        stop(paste0(error.prefix, "No intervention with code '", code, "' has been registered"))
    
    rv
}

# This function is internal
# Interventions are automatically registered when they have a code set
register.intervention <- function(intervention, error.prefix='')
{
    if (!is(intervention, 'jheem.intervention') && R6::is.R6(int))
        stop(paste0(error.prefix, "Cannot register intervention - 'intervention' must be an R6 object with class 'jheem.intervention'"))
    
    error.prefix = paste0(error.prefix, "Cannot register intervention '", intervention$name, "' - ")
    
    # Check intervention validity
    if (is.null(intervention$code))
        stop(paste0(error.prefix, "interventions can only be registered if they have a code specified"))
          
    
    # Is something already registered?
    already.registered = get.intervention(intervention$code, throw.error.if.missing = F)
    if (!is.null(already.registered) && !already.registered$equals(intervention, trust.codes.to.indicate.equality = F))
        stop(paste0("A different intervention has already been registered with the code '", intervention$code, "'"))

    # Register it    
    INTERVENTION.MANAGER$interventions[[intervention$code]] = intervention
    
    # For convenience, return back the intervention (invisibly)
    invisible(intervention)
}

interventions.or.codes.are.equal <- function(int1, int2)
{
    if (is.null(int1))
        is.null(int2)
    else if (is.null(int2))
        F
    else if (is(int1, 'jheem.intervention'))
    {
        if (is(int2, 'jheem.intervention'))
            int1$equals(int2)
        else if (!is.null(int1$code) && is.character(int2) && length(int2)==1 && !is.na(int2))
            int1$code == int2
        else
            F
    }
    else if (is(int2, 'jheem.intervention'))
    {
        if (!is.null(int2$code) && is.character(int1) && length(int1)==1 && !is.na(int1))
            int2$code == int1
        else
            F
    }
    else
    {
        is.character(int1) && length(int1)==1 && !is.na(int1) &&
            is.character(int2) && length(int2)==1 && !is.na(int2) &&
            int1 == int2
    }
}

INTERVENTION.TEMPORARY.CODE.PREFIX = 'tmp'

get.unique.temporary.intervention.code <- function()
{
    time = gsub("[^0-9]", "", as.character(Sys.time()))
    prefix = paste0(INTERVENTION.TEMPORARY.CODE.PREFIX, substr(time, 3, 14))
    code = prefix
    
    counter = 1
    success = F
    while (!success)
    {
        success = all(names(INTERVENTION.MANAGER$interventions) != code)
        if (!success)
            code = paste0(prefix, "-", counter)
        counter = counter + 1
    }
    
    code
}

is.intervention.code.temporary <- function(code)
{
    substr(code, 1, nchar(INTERVENTION.TEMPORARY.CODE.PREFIX)) == INTERVENTION.TEMPORARY.CODE.PREFIX
}

##-----------------------##
##-----------------------##
##-- CLASS DEFINITIONS --##
##-----------------------##
##-----------------------##


MINIMUM.INTERVENTION.NAME.NCHAR = 5
MAXIMUM.INTERVENTION.NAME.NCHAR = 50

#'@export
JHEEM.INTERVENTION = R6::R6Class(
    'jheem.intervention',
    
    public = list(
        
        initialize = function(name,
                              code,
                              parameter.distribution)
        {
            error.prefix = "Cannot create intervention: "
            
            # Do some error checking
            if (is.null(code))
            {
                code = get.unique.temporary.intervention.code()
            }
            else
            {
                validate.intervention.code(code = code,
                                           error.prefix = error.prefix, 
                                           code.name.for.error = 'code')
            }
            
            if (!is.null(name))
            {
                if (!is.character(name) || length(name)!=1 || is.na(name) || 
                    nchar(name)<MINIMUM.INTERVENTION.NAME.NCHAR || nchar(name)>MAXIMUM.INTERVENTION.NAME.NCHAR)
                    stop(paste0(error.prefix, "Invalid intervention: 'name' must be a single, non-NA character value with between ",
                                MINIMUM.INTERVENTION.NAME.NCHAR, " and ", MAXIMUM.INTERVENTION.NAME.NCHAR, " letters"))
            }
            
            if (!is.null(parameter.distribution))
            {
                if (!is(parameter.distribution, "Distribution"))
                    stop(paste0(error.prefix, "'parameter.distribution' must be either NULL or an object of class 'Distribution'"))
                
                if (is.null(parameter.distribution@var.names))
                    stop(paste0(error.prefix, "'parameter.distribution' must have variable names set"))
            }
            
            # Store the values
            private$i.name = name
            private$i.code = code
            private$i.parameter.distribution = parameter.distribution

            # Register to the intervention manager
            if (!is.null(code))
                register.intervention(self)
        },
        
        run = function(sim,
                       start.year,
                       end.year,
                       max.run.time.seconds = NULL,
                       keep.from.year = sim$from.year,
                       keep.to.year = end.year,
                       seed = 12345,
                       verbose = F,
                       error.prefix='')
        {
            # Validate arguments
            if (!is.character(error.prefix) || length(error.prefix)!=1 || is.na(error.prefix))
                stop("Cannot run intervention: 'error.prefix' must be a single, non-NA character value")
            
            if (!is(sim, 'jheem.simulation.set'))
                stop(paste0(error.prefix, "'sim' must be an object of class 'jheem.simulation.set'"))
            
            if (!sim$is.finalized)
                stop(paste0(error.prefix, "Cannot run interventions on a simulation set that has not been finalized"))
            
            self$validate(jheem.kernel = sim$jheem.kernel,
                          sub.version = sim$sub.version,
                          simulation.metadata = get.simulation.metadata(version = sim$version,
                                                                        location = sim$location,
                                                                        sub.version = sim$sub.version,
                                                                        from.year = keep.from.year,
                                                                        to.year = keep.to.year)) #a sim IS a simulation metadata object
            
            # Generate the new parameters
            if (is.null(private$i.parameter.distribution))
            {
                new.parameters = NULL
                new.param.names = NULL
            }
            else
            {
                reset.seed = runif(1, 0, .Machine$integer.max)
                
                if (!is.numeric(seed) || length(seed)!=1 || is.na(seed))
                    stop(paste0(error.prefix, "'seed' must be a single, non-NA, integer value"))
                
                set.seed(seed)
                new.parameters = generate.random.samples(private$i.parameter.distribution, n=sim$n.sim)
                dim(new.parameters) = c(sim$n.sim, private$i.parameter.distribution@n.var)
                new.param.names = private$i.parameter.distribution@var.names
                
                set.seed(reset.seed) # this keeps our code from always setting to the same seed
            }

            # Get the engine
            engine = sim$get.engine(start.year = start.year,
                                    end.year = end.year,
                                    max.run.time.seconds = max.run.time.seconds,
                                    keep.from.year = keep.from.year,
                                    keep.to.year = keep.to.year,
                                    intervention.code = private$i.code,
                                    error.prefix = "Cannot get JHEEM Engine from simulation set")
            
            
            # Run
            private$prepare.to.run(engine, sim=sim, verbose=verbose)
            sim.list = lapply(1:sim$n.sim, function(i){
                params = new.parameters[i,]
                names(params) = new.param.names
                
                private$do.run(engine, 
                               sim.index = i,
                               parameters = params,
                               verbose = verbose)
            })
            
            join.simulation.sets(sim.list, finalize=T)
        },
        
        get.description = function(version)
        {
            stop("The method 'get.description' must be implemented in a descendant class of jheem.intervention")
        },
        
        equals = function(other, trust.codes.to.indicate.equality=T)
        {
            if (!setequal(class(self), class(other)))
                F
            else if (trust.codes.to.indicate.equality &&
                     !is.null(private$i.code) && !is.null(other$code) &&
                     private$i.code == other$code)
                T
            else
                private$is.equal.to(other)
        },
        
        get.intervention.foregrounds = function()
        {
            stop("The method 'get.intervention.foregrounds' must be implemented in a descendant class of jheem.intervention")
        },
        
        validate = function(jheem.kernel,
                            sub.version = NULL,
                            simulation.metadata = NULL)
        {
    #        if (!is.character(error.prefix) || length(error.prefix)!=1 || is.na(error.prefix))
    #            stop("Error in intervention$check.can.apply(): 'error.prefix' must be a single, non-NA character vector")
            error.prefix = paste0("Intervention cannot apply to '", jheem.kernel$version, "' specification instance in this simulation: ")
            
            foregrounds = self$get.intervention.foregrounds()
            
            foreground.quantities = sapply(foregrounds, function(frgd){frgd$quantity.name})
            invalid.foreground.quantities = setdiff(foreground.quantities, jheem.kernel$quantity.names)
            if (length(invalid.foreground.quantities)>0)
                stop(paste0(error.prefix, collapse.with.and("'", invalid.foreground.quantities, "'"),
                            ifelse(length(invalid.foreground.quantities)==1, " is not a model quantity", " are not model quantities"),
                            " defined in the '", jheem.kernel$version, "' specification instance in this simulation"))
            
            specification.metadata = jheem.kernel$specification.metadata
            
            # Use this function for its error checking
            for (frgd in foregrounds)
            {
                for (tpop in frgd$target.populations)
                    tpop$render.population.mask(specification.metadata, error.prefix=error.prefix)
            }
            
            private$do.validate(jheem.kernel,
                                sub.version = sub.version,
                                simulation.metadata = simulation.metadata,
                                error.prefix = error.prefix)
        }
    ),
    
    active = list(
        
        name = function(value)
        {
            if (missing(value))
                private$i.name
            else
                stop("Cannot modify 'name' for a jheem.intervention - it is read-only")
        },
        
        code = function(value)
        {
            if (missing(value))
                private$i.code
            else
                stop("Cannot modify 'code' for a jheem.intervention - it is read-only")
        },
        
        parameter.distribution = function(value)
        {
            if (missing(value))
                private$i.parameter.distribution
            else
                stop("Cannot modify 'parameter.distribution' for a jheem.intervention - it is read-only")
        },
        
        depends.on = function(value)
        {
            if (missing(value))
                stop("The 'depends.on' active binding must be implemented at the subclass-level for a jheem.intervention")
            else
                stop("Cannot modify 'depends.on' for a jheem.intervention - it is read-only")
        }
            
    ),
    
    private = list(
        
        i.name = NULL,
        i.code = NULL,
        i.parameter.distribution = NULL,
        
        prepare.to.run = function(engine, sim, verbose)
        {
            # The default does nothing
        },
        
        do.run = function(engine, sim.index, parameters, previous.parameter.values, verbose)
        {
            stop("The method 'do.run' must be implemented in a descendant class of jheem.intervention")
        },
        
        is.equal.to = function(other)
        {
            print("for now, testing equality of interventions with code alone")
            F
#            stop("The method 'is.equal.to' must be implemented in a descendant class of jheem.intervention")
        },

        do.validate = function(jheem.kernel,
                               sub.version,
                               simulation.metadata,
                               error.prefix)
        {
            # The default does nothing additional
        }
    )
)

NULL.INTERVENTION = R6::R6Class(
    'null.intervention',
    inherit = JHEEM.INTERVENTION,
    
    public = list(
        
        initialize = function()
        {
            super$initialize(code = 'noint',
                             name = "No Intervention",
                             parameter.distribution = NULL)
        },
        
        get.intervention.foregrounds = function()
        {
            list()
        }
    ),
    
    active = list(
        
        depends.on = function(value)
        {
            if (missing(value))
                character()
            else
                stop("Cannot modify 'depends.on' for a jheem.intervention - it is read-only")
        }
    ),
    
    private = list(
        do.run = function(engine, sim.index, parameters, previous.parameter.values, verbose)
        {
            engine$run(parameters = parameters,
                       prior.sim.index = sim.index)
        }, 
        
        is.equal.to = function(other)
        {
            is(other, 'null.intervention')   
        }
    )
)

# Make the NULL intervention
NULL.INTERVENTION$new()

get.null.intervention <- function()
{
    INTERVENTION.MANAGER$interventions$noint
}

SINGLE.ITERATION.INTERVENTION = R6::R6Class(
    'single.iteration.intervention',
    inherit = JHEEM.INTERVENTION,
    
    public = list(
        
        initialize = function(code, name, parameter.distribution)
        {
            super$initialize(code = code,
                             name = name,
                             parameter.distribution = parameter.distribution)
        },
        
        
        get.intervention.foregrounds = function()
        {
            stop("The method 'get.intervention.foregrounds' must be implemented in a descendant class of single.iteration.intervention")
        }
    ),
    
    active = list(
        
    ),
    
    private = list(
        
        do.run = function(engine, sim.index, parameters, verbose)
        {
            engine$run(parameters = parameters,
                       prior.sim.index = sim.index)
        }   
    )
)

JHEEM.STANDARD.INTERVENTION = R6::R6Class(
    'jheem.standard.intervention',
    inherit = SINGLE.ITERATION.INTERVENTION,
    
    public = list(
        
        initialize = function(foregrounds,
                              parameter.distribution,
                              code,
                              name)
        {
            super$initialize(code = code,
                             name = name,
                             parameter.distribution = parameter.distribution)
            
            private$i.foregrounds = foregrounds
            names(private$i.foregrounds) = sapply(private$i.foregrounds, function(frgd){
                frgd$quantity.name
            })
            private$i.foregrounds = private$i.foregrounds[order(names(private$i.foregrounds))]
        },
        
        get.description = function(jheem.kernel)
        {
            stop("need to implement")
        },
        
        get.intervention.foregrounds = function()
        {
            private$i.foregrounds
        }
    
    ),
    
    active = list(
        
        foregrounds = function(value)
        {
            if (missing(value))
                private$i.foregrounds
            else
                stop("Cannot modify 'foregrounds' for a jheem.intervention - they are read-only")
        },
        
        depends.on = function(value)
        {
            if (missing(value))
                unique(unlist(sapply(private$i.foregrounds, function(frgd){
                    frgd$depends.on
                })))
            else
                stop("Cannot modify 'depends.on' for a jheem.intervention - it is read-only")
        }
    ),
    
    private = list(
        
        i.foregrounds = NULL,
        
        is.equal.to = function(other)
        {
            if (setequal(names(private$i.foregrounds), names(other$foregrounds)))
            {
                all(sapply(private$i.foregrounds, function(frgd){
                    frgd$equals(other$foregrounds[[frgd$quantity.name]]) 
                }))
            }
            else
                F
        }
    )
)