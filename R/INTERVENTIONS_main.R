

##-----------------------------------------------------##
##-----------------------------------------------------##
##-- THE PUBLIC INTERFACE for CREATING INTERVENTIONS --##
##-----------------------------------------------------##
##-----------------------------------------------------##

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
    if (!is(intervention, 'jheem.intervention') && !R6::is.R6(intervention))
        stop()
    
    length(intervention$reduce.to.standard.intervention()$target.element.names) == 0
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
    if (!is.null(already.registered) && !already.registered$equals(intervention))
        stop(paste0("A different intervention has already been registered with the code '", intervention$code, "'"))

    # Register it    
    INTERVENTION.MANAGER$interventions[[intervention$code]] = intervention
    
    # For convenience, return back the intervention (invisibly)
    invisible(intervention)
}


##-----------------------##
##-----------------------##
##-- CLASS DEFINITIONS --##
##-----------------------##
##-----------------------##

MINIMUM.INTERVENTION.CODE.NCHAR = 2
MAXIMUM.INTERVENTION.CODE.NCHAR = 30

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
            if (!is.null(code))
            {
                if (!is.character(code) || length(code)!=1 || is.na(code) || 
                    nchar(code)<MINIMUM.INTERVENTION.CODE.NCHAR || nchar(code)>MAXIMUM.INTERVENTION.CODE.NCHAR)
                    stop(paste0(error.prefix, "'code' must be a single, non-NA character value with between ",
                                MINIMUM.INTERVENTION.CODE.NCHAR, " and ", MAXIMUM.INTERVENTION.CODE.NCHAR, " letters"))
                
                check.for.invalid.characters(str = code,
                                             valid.characters = NUMBERS.LETTERS.DASH.PERIOD,
                                             str.name = "An intervention's 'code'",
                                             valid.characters.description = 'numbers, letters, dashes, or periods',
                                             error.prefix = '')
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

            # Register to the intervention manager
            if (!is.null(code))
                register.intervention(self)
        },
        
        crunch = function(sim,
                          start.year,
                          end.year,
                          check.consistency = !self$has.been.crunched())
        {
            
        },
        
        run = function(sim,
                       start.year,
                       end.year,
                       max.run.time.seconds = NULL,
                       keep.from.year = start.year,
                       keep.to.year = end.year,
                       atol = NULL,
                       rtol = NULL,
                       seed = 12345,
                       error.prefix='')
        {
            if (!is.character(error.prefix) || length(error.prefix)!=1 || is.na(error.prefix))
                stop("Cannot run intervention: 'error.prefix' must be a single, non-NA character value")
            
            if (!is(sim, 'jheem.simulation.set'))
                stop(paste0(error.prefix, "'sim' must be an object of class 'jheem.simulation.set'"))
            
            # Generate the new parameters
            if (is.null(private$i.parameters.distribution))
                parameters = NULL
            else
            {
                reset.seed = runif(1, 0, .Machine$integer.max)
                
                if (!is.numeric(seed) || length(seed)!=1 || is.na(seed))
                    stop(paste0(error.prefix, "'seed' must be a single, non-NA, integer value"))
                
                set.seed(seed)
                parameters = generate.random.samples(private$i.parameters.distribution, n=sim$n.sim)
                set.seed(reset.seed) # this keeps our code from always setting to the same seed
            }
            
            # Get the engine
            engine = sim$get.engine(start.year = start.year,
                                    end.year = end.year,
                                    max.run.time.seconds = max.run.time.seconds,
                                    keep.from.year = keep.from.year,
                                    keep.to.year = keep.to.year,
                                    atol = atol,
                                    rtol = rtol,
                                    error.prefix = "Cannot get JHEEM Engine from simulation set")
            
            # Set this intervention to the engine
            engine$set.intervention(self)
            
            # Set up the engine for the intervention
            private$do.prepare.to.run(engine)
            
            # Run
            sim.list = lapply(1:sim$n.sim, function(i){
                private$do.run(engine, sim.index=1, parameters=parameters[i,])
            })
            
            join.simulation.sets(sim.list)
        },
        
        get.description = function(version)
        {
            stop("The method 'get.description' must be implemented in a descendant class of jheem.intervention")
        },
        
        equals = function(other)
        {
            if (!setequal(class(self), class(other)))
                F
            else
                stop("The method 'equals' must be implemented in a descendant class of jheem.intervention")
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
        }
            
    ),
    
    private = list(
        
        i.name = NULL,
        i.code = NULL,
        i.parameters.distribution = NULL,
        
        do.prepare.to.run = function(engine)
        {
            stop("The method 'do.prepare.to.run' must be implemented in a descendant class of jheem.intervention")
        },
        
        do.run = function(engine, sim.index, parameters)
        {
            engine$run(parameters = intervention.parameters[i,],
                       prior.sim.index = i)
        }
    )
)

SINGLE.ITERATION.INTERVENTION = R6::R6Class(
    'single.iteration.intervention',
    inherit = JHEEM.INTERVENTION,
    
    public = list(
        
        initialize = function(code, name, parameter.distribution)
        {
            super$initialize(code = code,
                             name = name,
                             parameter.distribution = parameter.distribution)
        }
        
        
    ),
    
    active = list(
        
    ),
    
    private = list(
        
    )
)

JHEEM.STANDARD.INTERVENTION = R6::R6Class(
    'jheem.standard.intervention',
    inherit = JHEEM.INTERVENTION,
    
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
        },
        
        get.description = function(model.specification)
        {
            stop("need to implement")
        },
        
        equals = function(other)
        {
            
        }
    
    ),
    
    active = list(

    ),
    
    private = list(
        
        i.foregrounds = NULL,
        
        do.prepare.to.run = function(engine)
        {
            stop("Need to fill in")
        }
    )
)


##-------------------------------------------------------##
##-------------------------------------------------------##
##-- PUBLIC WITHIN PACKAGE, but PRIVATE TO THE OUTSIDE --##
##-------------------------------------------------------##
##-------------------------------------------------------##

