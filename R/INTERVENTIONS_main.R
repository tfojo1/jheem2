

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
    if (!is(intervention, 'jheem.intervention') && !R6::is.R6(intervention))
        stop()
    
    length(intervention$reduce.to.standard.intervention()$target.element.names) == 0
}


##---------------------------------------------------------------##
##---------------------------------------------------------------##
##-- THE INTERFACE for CREATING "CRITERIA-BASED" INTERVENTIONS --##
##---------------------------------------------------------------##
##---------------------------------------------------------------##

#'@name Create a criterion for criteria-based interventions
#'
#'@param outcome A character vector indicating the name of the outcome to which the criterion applies
#'@param target.value A single, numeric value that we want the outcome to get to
#'@param min.acceptable.value,max.acceptable.value Indicators of the min and max possible values of the outcome we would be willing to accept. Must be either (1) single, numeric values or (2) numeric vectors. In this case, the first value is the min/max threshold after the intervention runs the first chunk, the second is the threshold after the second chunk, etc.
#'@param stratify.outcome.by.dimensions A character vector giving the names of dimensions according to which the outcome should be stratified. Each value in the stratified outcome for the sim must approach the target
#'@param dimension.values The dimension values for which we should pull the outcome. Must be a named list of either character vectors, integer vectors, or logical vectors
#'@param ... An alternative way of supplying dimensions values - must be named character vectors, integer vectors, or logical vectors
#'
#'@export
create.intervention.criterion <- function(outcome,
                                          target.value,
                                          min.acceptable.value,
                                          max.acceptable.value,
                                          stratify.outcome.by.dimensions = character(),
                                          dimension.values = list(),
                                          ...,
                                          score.metric = c('normal.with.coefficient.of.variance','normal.with.sqrt.sd','root.squared.error')[1],
                                          coefficient.of.variance = 0.1,
                                          aggregate.scores.as = c('mean','sum')[1],
                                          weight = 1)
{
    JHEEM.OUTCOME.INTERVENTION.CRITERION$new(outcome = outcome,
                                             target.value = target.value,
                                             min.acceptable.value = min.acceptable.value,
                                             max.acceptable.value = max.acceptable.value,
                                             stratify.outcome.by.dimensions = stratify.outcome.by.dimensions,
                                             dimension.values = dimension.values,
                                             ...,
                                             score.metric = score.metric,
                                             coefficient.of.variance = coefficient.of.variance,
                                             aggregate.scores.as = aggregate.scores.as,
                                             weight = weight)
}

#'@name Create a "Guess-and-Check" Intervention that Must Satisfy Some Criteria
#'
#'@param base.intervention A single.iteration.intervention, as created by \code{\link{create.intervention}} or \code{\link{join.interventions}}
#'@param completion.criteria Either a single jheem.intervention.criterion object, as created by \code{\link{create.intervention.criterion}} or a list containing only jheem.intervention.criterion objects
#'@param parameters.to.vary A character vector giving the names of parameters which will vary in trying to satisfy the given criteria
#'@param parameter.scales
#'@param initial.parameter.values Either (1) 
#'@param iterations.per.chunk The number of simulations to run in each chunk of the optimization. Either a single numeric value (the number of iterations for all chunks) or a numeric vector (in which case the first value is the number of iterations for the first chunk, the second the number of iterations for the second chunk, and so on)
#'@param max.chunks.before.giving.up the maximum number of chunks to run before declaring failure and throwing an error
#'@param draw.parameters.from.previous.sims
#'@param method
#'
#'@export
create.criteria.based.intervention <- function(base.intervention,
                                               completion.criteria,
                                               parameter.scales,
                                               initial.parameter.values,
                                               iterations.per.chunk,
                                               max.chunks.before.giving.up,
                                               code=NULL, 
                                               name=NULL, 
                                               parameter.distribution=NULL,
                                               parameters.to.vary = names(parameter.scales),
                                               draw.parameters.from.previous.sims = !is.function(initial.parameter.values),
                                               method = c("Nelder-Mead", "BFGS", "CG", "SANN")[1])
{
    CRITERIA.BASED.INTERVENTION$new(base.intervention = base.intervention,
                                    completion.criteria = completion.criteria,
                                    parameter.scales = parameter.scales,
                                    initial.parameter.values = initial.parameter.values,
                                    iterations.per.chunk = iterations.per.chunk,
                                    max.chunks.before.giving.up = max.chunks.before.giving.up,
                                    code = code,
                                    name = name,
                                    parameter.distribution = parameter.distribution,
                                    parameters.to.vary = parameters.to.vary,
                                    draw.parameters.from.previous.sims = draw.parameters.from.previous.sims,
                                    method = method)
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

MINIMUM.INTERVENTION.CODE.NCHAR = 2
MAXIMUM.INTERVENTION.CODE.NCHAR = 30

MINIMUM.INTERVENTION.NAME.NCHAR = 5
MAXIMUM.INTERVENTION.NAME.NCHAR = 50

DISALLOWED.INTERVENTION.CODE.PREFIXES = c(
    INTERVENTION.TEMPORARY.CODE.PREFIX
)

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
                code = get.unique.temporary.intervention.code()
            else
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
                
                disallowed.prefix.mask = tolower(code) == tolower(DISALLOWED.INTERVENTION.CODE.PREFIXES)
                if (any(disallowed.prefix.mask))
                    stop(paste0(error.prefix, "'code' cannot begin with '", 
                                DISALLOWED.INTERVENTION.CODE.PREFIXES[disallowed.prefix.mask], "' (case-insensitive)"))
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
            if (is.null(private$i.parameter.distribution))
                new.parameters = NULL
            else
            {
                reset.seed = runif(1, 0, .Machine$integer.max)
                
                if (!is.numeric(seed) || length(seed)!=1 || is.na(seed))
                    stop(paste0(error.prefix, "'seed' must be a single, non-NA, integer value"))
                
                set.seed(seed)
                new.parameters = generate.random.samples(private$i.parameter.distribution, n=sim$n.sim)
                set.seed(reset.seed) # this keeps our code from always setting to the same seed
            }

            
            # Get the engine
            engine = sim$get.engine(start.year = start.year,
                                    end.year = end.year,
                                    max.run.time.seconds = max.run.time.seconds,
                                    keep.from.year = keep.from.year,
                                    keep.to.year = keep.to.year,
                                    foregrounds = private$get.intervention.foregrounds(),
                                    atol = atol,
                                    rtol = rtol,
                                    intervention.code = private$i.code,
                                    error.prefix = "Cannot get JHEEM Engine from simulation set")
            
            
            # Run
            private$prepare.to.run(engine)
            sim.list = lapply(1:sim$n.sim, function(i){
                private$do.run(engine, 
                               sim.index = 1,
                               parameters = new.parameters[,i])
            })
            
            join.simulation.sets(sim.list)
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
        
        validate = function(version, location)
        {
            stop("The method 'validate' must be implemented in a descendant class of jheem.intervention")
        },
        
        get.intervention.foregrounds = function()
        {
            stop("The method 'get.intervention.foregrounds' must be implemented in a descendant class of jheem.intervention")
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
        i.parameter.distribution = NULL,
        
        prepare.to.run = function(engine)
        {
            # The default does nothing
        },
        
        do.run = function(engine, sim.index, parameters, previous.parameter.values)
        {
            stop("The method 'do.run' must be implemented in a descendant class of jheem.intervention")
        },
        
        is.equal.to = function(other)
        {
            print("for now, testing equality of interventions with code alone")
            F
#            stop("The method 'is.equal.to' must be implemented in a descendant class of jheem.intervention")
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
        }
    ),
    
    private = list(
        do.run = function(engine, sim.index, parameters, previous.parameter.values)
        {
            engine$run(parameters = parameters,
                       prior.sim.index = sim.index)
        }, 
        
        is.equal.to = function(other)
        {
            is(other, 'null.intervention')   
        },
        
        get.intervention.foregrounds = function()
        {
            list()
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
        
        do.run = function(engine, sim.index, parameters, previous.parameter.values)
        {
            engine$run(parameters = parameters,
                       prior.sim.index = sim.index)
        }   
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
            names(private$i.foregrounds) = sapply(private$i.foregrounds, function(frgd){
                frgd$quantity.name
            })
            private$i.foregrounds = private$i.foregrounds[order(names(private$i.foregrounds))]
        },
        
        get.description = function(model.specification)
        {
            stop("need to implement")
        },
    
        validate = function(version, location)
        {
            
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

CRITERIA.BASED.INTERVENTION = R6::R6Class(
    'multiple.iteration.intervention',
    inherit = JHEEM.INTERVENTION,
    
    public = list(
        
        initialize = function(base.intervention,
                              completion.criteria,
                              parameter.scales,
                              initial.parameter.values,
                              iterations.per.chunk,
                              max.chunks.before.giving.up,
                              code=NULL, 
                              name=NULL, 
                              parameter.distribution=NULL,
                              parameters.to.vary = names(parameter.scales),
                              draw.parameters.from.previous.sims = !is.function(initial.parameter.values),
                              method = c("Nelder-Mead", "BFGS", "CG", "SANN")[1])
        {
            super$initialize(code = code,
                             name = name,
                             parameter.distribution = parameter.distribution)
            
            error.prefix = "Cannot create criteria-based intervention: "
            
            # Base Intervention
            if (!is(base.intervention, 'jheem.standard.intervention'))
                stop(paste0(error.prefix, "'base.intervention' must be an object of class jheem.standard.intervention - as created by create.intervention() or join.interventions()"))
            
            # Completion Criteria
            if (is(completion.criteria, "jheem.intervention.criterion"))
                completion.criteria = list(completion.criteria)
            
            if (!is.list(completion.criteria))
                stop(paste0(error.prefix, "'completion.critera' must be either a 'jheem.intervention.criterion' object or a list of 'jheem.intervention.criterion' objects"))
            
            if (length(completion.criteria)==0)
                stop(paste0(error.prefix, "'completion.criteria' cannot be an empty list"))
            
            if (any(!sapply(completion.criteria, is, "jheem.intervention.criterion")))
                stop(paste0(error.prefix, "'completion.critera' must be either a 'jheem.intervention.criterion' object or a list of 'jheem.intervention.criterion' objects"))
            
            # parameters to vary
            if (!is.character(parameters.to.vary) || length(parameters.to.vary)==0 || any(is.na(parameters.to.vary)))
                stop(paste0(error.prefix, "'parameters.to.vary' must be a non-empty character vector with no NA values"))
            
            if (!is.character(parameter.scales) || any(is.na(parameter.scales)))
                stop(paste0(error.prefix, "'parameter.scales' must be a character vector with no NA values"))
            
            check.model.scale(scale = parameter.scales,
                              varname.for.error = "'parameter.scales'",
                              require.scalar = F,
                              error.prefix = error.prefix)
            
            if (is.null(names(parameter.scales)))
                stop(paste0(error.prefix, "'parameter.scales' must be a NAMED character vector"))
            
            if (length(setdiff(parameters.to.vary, names(parameter.scales))))
                stop(paste0(error.prefix, "'parameter.scales' must be named with the same values as those in 'parameters.to.vary"))
            parameter.scales = parameter.scales[parameters.to.vary]
            
            
            # initial.parameter.values
            if (is.numeric(initial.parameter.values))
            {
                if (any(is.na(initial.parameter.values)))
                    stop(paste0(error.prefix, "'initial.parameter.values' cannot contain NA values"))
                if (is.null(names(initial.parameter.values)))
                    stop(paste0(error.prefix, "If 'initial.parameter.values' is a numeric vector, it must be NAMED"))
                
                missing.parameters = setdiff(names(initial.parameter.values), parameters.to.vary)
                if (length(missing.parameters)>0)
                    stop(paste0(error.prefix, "If 'initial.parameter.values' is a numeric vector, it must contain values for all of parameters.to.vary (missing ",
                                collapse.with.and("'", missing.parameters, "'"), ")"))
            }
            else if (is.function(initial.parameter.values))
            {
                arg.names = get.function.argument.names(fn = initial.parameter.values,
                                                        exclude.arguments.with.default.values = T)
                
                if (length(arg.names)!=1 || arg.names!='sim')
                    stop(paste0(error.prefix, "If 'initial.parameter.values' is a function, it must take one and only one argument without a default value: 'sim'"))
            }
            else
                stop(paste0(error.prefix,
                            "'initial.parameter.values' must be either a named numeric vector, or a function that takes a sim and returns a named numeric vector"))
            
            # iterations.per.chunk
            if (!is.numeric(iterations.per.chunk) || length(iterations.per.chunk)==0 || any(is.na(iterations.per.chunk)))
                stop(paste0(error.prefix, "'iterations.per.chunk' must be a non-empty numeric vector with no NA values"))
            if (any(iterations.per.chunk<=0))
                stop(paste0(error.prefix, "'iterations.per.chunk' must contain only positive values"))
            
            # max.chunks.before.giving.up
            if (!is.numeric(max.chunks.before.giving.up) || length(max.chunks.before.giving.up)!=1 || is.na(max.chunks.before.giving.up))
                stop(paste0(error.prefix, "'max.chunks.before.giving.up' must be a single, non-NA numeric value"))
            
            # draw.parameters.from.previous.sims
            if (!is.logical(draw.parameters.from.previous.sims) || length(draw.parameters.from.previous.sims)!=1 || is.na(draw.parameters.from.previous.sims))
                stop(paste0(error.prefix, "'draw.parameters.from.previous.sims' must be a single, non-NA logical value"))
            
            # method
            allowed.methods = c("Nelder-Mead", "BFGS", "CG", "SANN")
            if (!is.character(method) || length(method)!=1 || is.na(method))
                stop(paste0(error.prefix, "'method' must be a single, non-NA character value"))
            
            if (all(method!=allowed.methods))
                stop(paste0(error.prefix, "Invalid method '", method, "' - must be one of ",
                            collapse.with.or("'", allowed.methods, "'")))
            
            
            # Store the variables
            private$i.base.intervention = base.intervention
            private$i.completion.criteria = completion.criteria
            
            private$i.parameters.to.vary = parameters.to.vary
            private$i.parameter.scales = parameter.scales
            
            private$i.initial.parameter.values = initial.parameter.values
            private$i.iterations.per.chunk = iterations.per.chunk
            private$i.max.chunks.before.giving.up = max.chunks.before.giving.up
            private$i.draw.parameters.from.previous.sims = draw.parameters.from.previous.sims
            
            private$i.method = method
            
        },
        
        validate = function(version, location)
        {
            
        },
        
        get.intervention.foregrounds = function()
        {
            private$i.base.intervention$get.intervention.foregrounds()
        }
    ),
    
    active = list(
        
        
    ),
    
    private = list(
        
        i.base.intervention = NULL,
        i.completion.criteria = NULL,
        
        i.parameters.to.vary = NULL,
        i.parameter.scales = NULL,
        
        i.initial.parameter.values = NULL,
        i.initial.parameters.fn.name = NULL,
        i.draw.parameters.from.previous.sims = NULL,
        
        i.iterations.per.chunk = NULL,
        i.max.chunks.before.giving.up = NULL,
        i.method = NULL,
        
        i.previous.parameter.values = NULL,
        
        
        is.equal.to = function(other)
        {
            stop("need to implement is.equal.to for criteria-based-intervention")
        },
        
        prepare.to.run = function(engine)
        {
            all.parameters = engine$parameter.names
            if (!is.null(private$i.parameter.distribution))
                all.parameters = union(all.parameters, private$i.parameter.distribution@var.names)
            invalid.parameters.to.track = setdiff(private$i.parameters.to.vary,
                                                  all.parameter.names)
            
            if (length(invalid.parameters.to.track))
                stop(paste0("Cannot run intervention: ",
                            collapse.with.and("'", invalid.parameters.to.track, "'"),
                            ifelse(length(invalid.parameters.to.track)==1, 
                                   " was set as a parameter.to.track, but is",
                                   " were set as parameters.to.track, but are"),
                            " not present in either the simulation or the intervention's parameter distribution"))
            
            private$i.previous.parameter.values = NULL
        },
        
        do.run = function(engine, sim.index, parameters)
        {
            error.prefix = "Cannot run criteria-based intervention: "
            
            #-- Pull Initial Values --#
            if (private$i.draw.parameters.from.previous.sims && 
                !is.null(private$i.previous.parameter.values))
            {
                initial.values = rowMeans(private$i.previous.parameter.values)
            }
            else if (is.numeric(private$i.initial.parameter.values))
                initial.values = private$i.initial.parameter.values
            else
            {
                initial.values = private$i.initial.parameter.values(sim)
                
                if (is.null(private$i.initial.parameters.fn.name))
                    descriptor = "the criteria's initial.parameter.values function"
                else
                    descriptor = paste0(private$i.initial.parameters.fn.name, "(the criteria's initial.parameter.values function)")
                
                if (!is.numeric(initial.values) || length(initial.values)!=length(private$i.parameters.to.vary) || any(is.na(initial.values)))
                {   
                    stop(paste0(error.prefix,
                                "The value returned by ", descriptor ," must be a numeric vector with exactly ",
                                length(private$i.parameters.to.vary), " non-NA ",
                                ifelse(length(private$i.parameters.to.vary)==1, 
                                       "value (for the parameter to vary)",
                                       "values (one for each parameter to vary)")))
                }
                
                if (is.null(names(initial.values)))
                    stop(paste0(error.prefix,
                                "The value returned by ", descriptor, " must be a NAMED numeric vector"))
                
                if (!setequal(names(initial.values), private$i.parameters.to.vary))
                {
                    if (length(private$i.parameters.to.vary)==1)
                        pl = ''
                    else
                        pl = 's'
                    
                    stop(paste0(error.prefix,
                                "The names of the value", pl, " returned by ", descriptor, 
                                " must exactly match the parameter", pl, " to vary (",
                                collapse.with.and("'", private$i.parameters.to.vary, "'"), ")"))
                }
            }
            
            if (private$i.draw.parameters.from.previous.sims)
                private$i.previous.parameter.values = cbind(private$i.previous.parameter.values, parameters[private$i.parameters.to.vary])
            
            initial.values = initial.values[private$i.parameter.scales]

            
            #-- Run optimization until criteria satisfied (or until we have to give up) --#
            criteria.satisfied = F
            chunk = 1
            transformed.current.parameters = transform.to.unbounded.scale(initial.values, private$i.parameter.scales)
            sim = NULL
            while (!criteria.satisfied && chunk <= private$i.max.chunks.before.giving.up)
            {
                max.iter = private$i.iterations.per.chunk[min(chunk, private$i.iterations.per.chunk)]
                
                optim.result = optim(par = transformed.current.parameters,
                                     fn = function(par){
                                         parameters[private$i.parameters.to.vary] = transform.from.unbounded.scale(par, private$i.parameter.scales)
                                         sim <<- engine$run(parameters = parameters,
                                                          prior.sim.index = sim.index)
                                         
                                         sum(sapply(private$i.completion.criteria, function(criterion){
                                             criterion$score.sim(sim=sim, chunk=chunk)
                                         }))
                                     },
                                     method = private$i.method,
                                     control = list(maxit=max.iter))
                
                transformed.current.parameters = optim.result$par
                chunk = chunk+1
                
                criteria.satisfied = all(sapply(private$i.completion.criteria, function(criterion){
                    criterion$is.satisfied(sim, chunk)
                }))
            }
            
            if (!criteria.satisfied)
                stop(paste0(error.prefix, "Unable to satisfy all criteria for the intervention after ", 
                            private$i.max.chunks.before.giving.up, " optimization ",
                            ifelse(length(private$i.max.chunks.before.giving.up)==1, "attempt", "attempts")))
            
            sim
        }   
    )
)

JHEEM.INTERVENTION.CRITERION = R6::R6Class(
    'jheem.intervention.criterion',
    
    public = list(
        
        initialize = function()
        {
            
        },
        
        is.satisfied = function(sim, chunk)
        {
            stop("is.satisfied() for a jheem.intervention.criterion must be implemented at the subclass level")
        },
        
        score.sim = function(sim, chunk)
        {
            stop("is.satisfied() for a jheem.intervention.criterion must be implemented at the subclass level")
        }
    )
)

JHEEM.OUTCOME.INTERVENTION.CRITERION = R6::R6Class(
    'jheem.outcome.intervention.criterion',
    inherit = JHEEM.INTERVENTION.CRITERION,
    
    public = list(
        
        initialize = function(outcome,
                              target.value,
                              min.acceptable.value,
                              max.acceptable.value,
                              stratify.outcome.by.dimensions = character(),
                              dimension.values = list(),
                              ...,
                              score.metric = c('normal.with.coefficient.of.variance','normal.with.sqrt.sd','root.squared.error')[1],
                              coefficient.of.variance = 0.1,
                              aggregate.scores.as = c('mean','sum')[1],
                              weight = 1)
        {
            super$initialize()
            
            # outcome, target
            if (!is.character(outcome) || length(outcome)!=1 || is.na(outcome))
                stop(paste0(error.prefix, "'outcome' must be a single, non-NA character value"))
            
            if (!is.numeric(target.value) || length(target.value)==0 || any(is.na(target.value)))
                stop(paste0(error.prefix, "'target.value' must be a non-empty, numeric vector with no NA values"))
            
            # min / max
            if (!is.numeric(min.acceptable.value) || length(min.acceptable.value)==0 || any(is.na(min.acceptable.value)))
                stop(paste0(error.prefix, "'min.acceptable.value' must be a non-empty, numeric vector with no NA values"))
            
            if (length(min.acceptable.value)>1 && any(min.acceptable.value[-1] > min.acceptable.value[-length(min.acceptable.value)]))
                stop(paste0(error.prefix, "It does not make sense for min.acceptable.values to INCREASE (ie become more restrictive) during subsequent chunks"))
            
            if (!is.numeric(max.acceptable.value) || length(max.acceptable.value)==0 || any(is.na(max.acceptable.value)))
                stop(paste0(error.prefix, "'max.acceptable.value' must be a non-empty, numeric vector with no NA values"))
            
            if (length(max.acceptable.value)>1 && any(max.acceptable.value[-1] < max.acceptable.value[-length(max.acceptable.value)]))
                stop(paste0(error.prefix, "It does not make sense for max.acceptable.values to DECREASE (ie become more restrictive) during subsequent chunks"))
            
            min.acceptable.value = pad.with.last.value(min.acceptable.value, length(max.acceptable.value))
            max.acceptable.value = pad.with.last.value(max.acceptable.value, length(min.acceptable.value))
            
            if (any(min.acceptable.value >= max.acceptable.value))
                stop(paste0(error.prefix, "The elements of min.acceptable.value must always be strictly LESS than the corresponding elements in max.acceptable.value"))
            
            # Dimension stuff
            if (!is.character(stratify.outcome.by.dimensions) || any(is.na(stratify.outcome.by.dimensions)))
                stop(paste0(error.prefix, "'stratify.outcome.by.dimensions' must be a character vector with no NA values"))
            
            if (length(unique(stratify.outcome.by.dimensions) != length(stratify.outcome.by.dimensions)))
                stop(paste0(error.prefix, "The elements of 'stratify.outcome.by.dimensions' must be unique"))
            
            
            dot.dot.dot = list(...)
            if (length(dot.dot.dot)>0 && length(dimension.values)>0)
                stop(paste0(error.prefix, "Cannot specify BOTH dimension.values and elements in ... - you must use one or the other"))
            
            if (length(dimension.values)==0)
            {
                dimension.values = dot.dot.dot
                dv.name = "the elements of ..."
            }
            else
                dv.name = 'dimension.values'
            
            check.dimension.values.valid(dimension.values = dimension.values,
                                         variable.name.for.error = dv.name,
                                         refer.to.dimensions.as = 'dimension',
                                         allow.empty = T,
                                         allow.duplicate.values.within.dimensions = F,
                                         error.prefix=error.prefix)
            
            # Score stuff
            if (!is.character(score.metric) || length(score.metric)!=1 || is.na(score.metric))
                stop(paste0(error.prefix, "'score.metric' must be a single, non-NA character value"))
            
            allowed.score.metrics = c('normal.with.coefficient.of.variance','normal.with.sqrt.sd','root.squared.error')
            if (all(score.metric != allowed.score.metrics))
                stop(paste0(error.prefix, "'score.metric' must be either ",
                            collapse.with.or("'", allowed.score.metrics, "'")))
            
            if (!is.numeric(coefficient.of.variance) || length(coefficient.of.variance)!=1 || is.na(coefficient.of.variance))
                stop(paste0(error.prefix, "'coefficient.of.variance' must be a single, non-NA, numeric value"))
            
            if (coefficient.of.variance <= 0)
                stop(paste0(error.prefix, "'coefficient.of.variance' must be a positive number"))
            
            if (!is.character(aggregate.scores.as) || length(aggregate.scores.as)!=1 || is.na(aggregate.scores.as))
                stop(paste0(error.prefix, "'aggregate.scores.as' must be a single, non-NA character value"))
            
            allowed.aggregate.scores.as = c('mean','sum')
            if (all(aggregate.scores.as != allowed.aggregate.scores.as))
                stop(paste0(error.prefix, "'aggregate.scores.as' must be one of ",
                            collapse.with.or("'", allowed.aggregate.scores.as, "'")))
            
            if (!is.numeric(weight) || length(weight)!=1 || is.na(weight))
                stop(paste0(error.prefix, "'weight' must be a single, non-NA, numeric value"))
            
            if (weight <= 0)
                stop(paste0(error.prefix, "'weight' must be a positive number"))
            
            
            # Store variables
            private$i.outcome = outcome
            private$i.target.value = target.value
            
            private$i.min.acceptable.value = min.acceptable.value
            private$i.max.acceptable.value = max.acceptable.value
            
            private$i.stratify.outcome.by.dimensions = stratify.outcome.by.dimensions
            private$i.dimension.values = dimension.values
            
            private$i.score.metric = score.metric
            private$i.coefficient.of.variance = coefficient.of.variance
            private$i.aggregate.scores.as = aggregate.scores.as
            private$i.score.weight = weight
        },
        
        is.satisfied = function(sim, chunk)
        {
            sim.value = private$get.sim.value(sim)
            
            min.acceptable = private$i.min.accetable.value[min(chunk, length(private$i.min.acceptable.value))]
            max.acceptable = private$i.max.accetable.value[min(chunk, length(private$i.max.acceptable.value))]
            
            all(sim.value >= min.acceptable & sim.value <= max.acceptable)
        },
        
        score.sim = function(sim, chunk)
        {
            sim.value = private$get.sim.value(sim)
            target.value = private$i.target.value[min(chunk, length(private$i.target.value))]
            
            if (private$i.score.metric=='normal.with.coefficient.of.variance'  ||
                private$i.score.metric=='normal.with.sqrt.sd')
            {
                if (private$i.score.metric=='normal.with.coefficient.of.variance')
                    sd.for.score = target.value * private$i.coefficient.of.variance
                else
                    sd.for.score = sqrt(target.value)
                
                scores = -dnorm(sim.value, mean=target.value, sd=sd.for.score)
                if (private$i.aggregate.scores.as=='sum')
                    score = sum(scores)
                else
                    score = mean(scores)
            }
            else
            {
                scores = (sim.value - private$i.target.value)^2
                
                if (private$i.aggregate.scores.as=='sum')
                    score = sqrt(sum(scores))
                else
                    score = sqrt(mean(scores))
            }
            
            weight = private$i.score.weight[min(chunk, length(private$i.score.weight))]
            score * weight
        },
        
        equals = function(other)
        {
            stop('need to implement equals for outcome-based criterion')
        },
        
        validate = function(version, location)
        {
            
        }
    ),
    
    private = list(
        
        i.outcome = NULL,
        
        i.target.value = NULL,
        i.min.acceptable.value = NULL,
        i.max.acceptable.value = NULL,
        
        i.stratify.outcome.by.dimensions = NULL,
        i.dimension.values = NULL,
        
        i.score.metric = NULL,
        i.coefficient.of.variance = NULL,
        i.aggregate.scores.as = NULL,
        i.score.weight = NULL,
        
        get.sim.value = function(sim)
        {
            sim$get(outcomes = private$i.outcome,
                    dimension.values = private$i.dimension.values,
                    keep.dimensions = private$i.stratify.outcome.by.dimensions)
        }
        
    )
)


##-------------------------------------------------------##
##-------------------------------------------------------##
##-- PUBLIC WITHIN PACKAGE, but PRIVATE TO THE OUTSIDE --##
##-------------------------------------------------------##
##-------------------------------------------------------##

