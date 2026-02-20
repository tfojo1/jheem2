

##---------------------------------------------------------##
##---------------------------------------------------------##
##-- THE INTERFACE for CREATING "STANDARD" INTERVENTIONS --##
##---------------------------------------------------------##
##---------------------------------------------------------##

#'@title Create an Intervention
#'
#'@param ... Either (a) objects of class 'target.population', (b) objects of class 'intervention.effect', or (d) lists containing only 'target.population' or 'intervention.effect' objects. Must contain at least one 'target.population' object and at least one 'intervention.effect' object
#'@param code A brief (2-30 character) string that will uniquely identify this intervention
#'@param name
#'@param parameter.distribution
#'
#'@details Creates a 'jheem.intervention' object where all the intervention effects given in ... apply to all the target populations given in ...
#'
#'@export
create.intervention <- function(..., code=NULL, name=NULL, parameters = NULL, parameter.distribution=NULL, generate.parameters.function=NULL, overwrite.existing.intervention=F)
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
    
    if (is.null(overwrite.existing.intervention) || !is.logical(overwrite.existing.intervention) || length(overwrite.existing.intervention)!=1 || is.na(overwrite.existing.intervention))
        stop(paste0(error.prefix, "'overwrite.existing.intervention' must be TRUE or FALSE"))
    
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
                                    parameters = parameters,
                                    parameter.distribution = parameter.distribution,
                                    generate.parameters.function = generate.parameters.function,
                                    overwrite.existing.intervention = overwrite.existing.intervention)
}

#'@title Join Multiple Interventions into a Single Intervention
#'
#'@inheritParams create.intervention
#'@param ... One or more interventions to join. These may be either objects of class jheem.intervention or lists which contain only objects of class jheem.intervention
#'@param sequential
#'
#'@export
join.interventions <- function(..., code=NULL, name=NULL, parameters=NULL, parameter.distribution=NULL, overwrite.existing.intervention=F, sequential=F)
{
    args = list(...)
    
    if (is.null(overwrite.existing.intervention) || !is.logical(overwrite.existing.intervention) || length(overwrite.existing.intervention)!=1 || is.na(overwrite.existing.intervention))
        stop(paste0(error.prefix, "'overwrite.existing.intervention' must be TRUE or FALSE"))
    
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
                                    parameters = parameters,
                                    parameter.distribution = parameter.distribution,
                                    overwrite.existing.intervention = overwrite.existing.intervention)
}

join.standard.interventions <- function(interventions, code, name, parameters, parameter.distribution, overwrite.existing.intervention)
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
    
    
    if (is.null(parameters))
    {
        for (int in interventions)
        {
            if (!is.null(int$parameters))
            {
                if (is.null(parameters))
                    parameters = int$parameters
                else
                {
                    overlapping.parameter.names = intersect(dimnames(parameters)[[1]],
                                                            dimnames(int$parameters)[[1]])
                    
                    if (length(overlapping.parameter.names)>0)
                        stop(paste0("Cannot join standard interventions: the same parameter ",
                                    ifelse(length(overlapping.parameter.names)==1, "name", "names"),
                                    "(", collapse.with.and("'", overlapping.parameter.names, "'"), ")",
                                    ifelse(length(overlapping.parameter.names)==1, "is", "are"),
                                    " contained in the parameters from more than one intervention to combine. Consider explicitly specifying parameters in the call to join.interventions() to override automatic combination"))
                    
                    # join distributions
                }
            }
        }
    }
    
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
                    
                    # join distributions
                }
            }
        }
    }
    
    JHEEM.STANDARD.INTERVENTION$new(foregrounds = new.foregrounds,
                                    code = code,
                                    name = name,
                                    parameters = parameters,
                                    parameter.distribution = parameter.distribution,
                                    overwrite.existing.intervention = overwrite.existing.intervention)
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

#'@title Clear JHEEM Interventions
#'@export
clear.interventions <- function()
{
    INTERVENTION.MANAGER$interventions = list()
    
    # Make the NULL intervention
    NULL.INTERVENTION$new()
}

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
register.intervention <- function(intervention, overwrite.existing = F, error.prefix='')
{
    if (!is(intervention, 'jheem.intervention') && R6::is.R6(int))
        stop(paste0(error.prefix, "Cannot register intervention - 'intervention' must be an R6 object with class 'jheem.intervention'"))
    
    error.prefix = paste0(error.prefix, "Cannot register intervention '", intervention$name, "' - ")
    
    # Check intervention validity
    if (is.null(intervention$code))
        stop(paste0(error.prefix, "interventions can only be registered if they have a code specified"))
          
    # Is something already registered?
    if (!overwrite.existing) {
        already.registered = get.intervention(intervention$code, throw.error.if.missing = F)
        if (!is.null(already.registered) && !already.registered$equals(intervention))
            stop(paste0("A different intervention has already been registered with the code '", intervention$code, "'"))
    }

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
                              parameters,
                              parameter.distribution,
                              generate.parameters.function,
                              overwrite.existing.intervention = F)
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
            
            if (!is.null(parameters))
            {
                if (!is.numeric(parameters) || !is.matrix(parameters))
                    stop(paste0(error.prefix, "'parameters' must be either NULL or a two-dimensional numeric matrix"))
                
                if (is.null(dimnames(parameters)[[1]]))
                    stop(paste0(error.prefix, "If specified, 'parameter.distribution' must have dimnames[[1]] set"))
                
                if (any(table(dimnames(parameters)[[1]])>1))
                    stop(paste0(error.prefix, "The dimnames[[1]] of'parameter.distribution' cannot contain repeated values (ie, each parameter name must be unique"))
                
                if (any(is.na(parameters)))
                    stop(paste0(error.prefix, "If specified, 'parameter.distribution' cannot contain NA values"))
                
                if (length(parameters)==0)
                    stop(paste0(error.prefix, "If specified, 'parameter.distribution' cannot have length = 0"))
            }
            
            if (!is.null(parameter.distribution))
            {
                if (!is(parameter.distribution, "Distribution"))
                    stop(paste0(error.prefix, "'parameter.distribution' must be either NULL or an object of class 'Distribution'"))
                
                if (is.null(parameter.distribution@var.names))
                    stop(paste0(error.prefix, "'parameter.distribution' must have variable names set"))
                
                if (!is.null(parameters))
                {
                    overlapping.parameters = intersect(dimnames(parameters)[[1]], parameter.distribution@var.names)
                    stop(paste0(error.prefix, "'parameters' and 'parameter.distribution' both contain ",
                                ifelse(length(overlapping.parameters), "a parameter named ", "parameters named "),
                                collapse.with.and("'", overlapping.parameters, "'"),
                                ". Parameter names must be unique across 'parameters' and 'parameter.distributions'"))
                }
            }
            
            if (is.null(overwrite.existing.intervention) || !is.logical(overwrite.existing.intervention) || length(overwrite.existing.intervention)!=1 || is.na(overwrite.existing.intervention))
                stop(paste0(error.prefix, "'overwrite.existing.intervention' must be TRUE or FALSE"))
            
            if (!is.null(generate.parameters.function))
            {
                if (!is.function(generate.parameters.function))
                    stop(paste0(error.prefix, "'generate.parameters.function' must be a function"))
                
                print("Need to do more error checking on generate.parameters.function in INTERVENTIONS_main.R")
            }
            
            # Store the values
            private$i.name = name
            private$i.code = code
            private$i.parameters = parameters
            private$i.parameter.distribution = parameter.distribution
            private$i.generate.parameters.function = generate.parameters.function
        },
        
        run = function(sim,
                       end.year,
                       start.year = NULL,
                       max.run.time.seconds = NULL,
                       keep.from.year = sim$from.year,
                       keep.to.year = end.year,
                       seed = 12345,
                       verbose = F,
                       listener = NULL,
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
            
            if (!is.null(listener))
            {
                if (!is.function(listener))
                    stop(paste0(error.prefix, "'listener' for running intervention must be a function which takes arguments 'index', 'total', and 'done'"))
                
                listener.formals = formals(listener)
                listener.formals.without.default = listener.formals[sapply(listener.formals, function(x){
                    is.name(x) & length(x)==1 & x==''
                })]
                
                if (length(listener.formals) != 3 || !setequal(names(listener.formals), c('index','total','done'))) {
                    stop(paste0(error.prefix, "'listener' for running intervention must be a function which takes ONLY arguments 'index', 'total', and 'done'"))
                }
                
            }
            
            # Generate the new parameters
            if (is.null(private$i.parameters))
            {
                new.parameters = NULL
                new.param.names = NULL
            }
            else
            {
                if (ncol(private$i.parameters) < sim$n.sim) #since private$i.parameters must be non-empty, sim$n.sim must be >1
                    stop(paste0(error.prefix, "The given 'sim' contains ",
                                sim$n.sim,
                                " simulations, but the parameters given in creating the intervention only contain ",
                                ifelse(ncol(private$i.parameters)==1, 
                                       "a single parameter set",
                                       paste0(ncol(private$i.parameters), " parameter sets"))))
                
                new.parameters = private$i.parameters[,1:sim$n.sim,drop=F]
                new.param.names = dimnames(new.parameters)[[1]]
            }
            
            if (!is.null(private$i.parameter.distribution))
            {
                reset.seed = runif(1, 0, .Machine$integer.max)
                
                if (!is.numeric(seed) || length(seed)!=1 || is.na(seed))
                    stop(paste0(error.prefix, "'seed' must be a single, non-NA, integer value"))
                
                #new.parameters = generate.random.samples(private$i.parameter.distribution, n=sim$n.sim)
                add.to.new.parameters = sapply(1:sim$n.sim, function(i){
                    set.seed(seed + sim$seed[i])
                    generate.random.samples(private$i.parameter.distribution, n=1)
                })
                dim(add.to.new.parameters) = c(sim$n.sim, private$i.parameter.distribution@n.var)
                new.parameters = rbind(new.parameters, t(add.to.new.parameters))
                
                new.param.names = c(new.param.names, private$i.parameter.distribution@var.names)
                
                set.seed(reset.seed) # this keeps our code from always setting to the same seed
            }
            
            if (!is.null(private$i.generate.parameters.function))
            {
                reset.seed = runif(1, 0, .Machine$integer.max)
                
                add.to.new.parameters = private$i.generate.parameters.function(n = sim$n.sim,
                                                                               parameters = new.parameters,
                                                                               sim = sim)
                
                # need to error check what comes out here
                if (is.null(dim(add.to.new.parameters)) || 
                    length(dim(add.to.new.parameters))==1)
                {
                    if (is.null(names(add.to.new.parameters)))
                        stop(paste0(error.prefix, "If the generate.parameters.function returns a vector (not an array), it must be a NAMED vector with one value for each new parameter (which will be the same for all simulations)"))
                    
                    add.to.new.parameters = matrix(rep(add.to.new.parameters, sim$n.sim), ncol = sim$n.sim, dimnames = list(names(add.to.new.parameters), NULL))
                }
                else if (length(dim(add.to.new.parameters))==2)
                {
                    
                    if (dim(add.to.new.parameters)[2] != sim$n.sim)
                        stop(paste0(error.prefix, "If the generate.parameters.function returns a matrix of new parameter values, it must have one column for each of the ", sim$n.sim, " simulation(s) we are running the intervention for"))
                    
                    if (is.null(dimnames(add.to.new.parameters)) || is.null(dimnames(add.to.new.parameters)[[1]]))
                        stop(paste0(error.prefix, "If the generate.parameters.function returns a matrix of new parameter values, it must have dimnames set for the rows - one name each new parameter"))
                }
                else
                    stop("need a smarter error message here")
                
                new.parameters = rbind(new.parameters, add.to.new.parameters)
                new.param.names = c(new.param.names, dimnames(add.to.new.parameters)[[1]])
                
                set.seed(reset.seed) # this keeps our code from always setting to the same seed
            }
            
            if (is.null(start.year))
            {
                # Note to self - actually need to bind the new parameters to the simsets existing parameters, and try to resolve that
                start.year = self$get.start.time(parameters = new.parameters, throw.error.if.unable.to.calculate = F)
                if (is.null(start.year))
                    stop(paste0("Cannot figure out start.year - some intervention effect foregrounds have not been resolved"))
                
                if (start.year == -Inf)
                    start.year = min(end.year-1, sim$to.year)
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
            private$prepare.to.run(engine, 
                                   sim = sim, 
                                   keep.from.year = keep.from.year,
                                   keep.to.year = keep.to.year,
                                   verbose=verbose)
            sim.list = lapply(1:sim$n.sim, function(i){
                params = new.parameters[,i]
                names(params) = new.param.names
                
                if (!is.null(listener))
                    listener(index = i, 
                             total = sim$n.sim, 
                             done = F)
                
                sim = private$do.run(engine, 
                               sim.index = i,
                               parameters = params,
                               verbose = verbose)
                
                
                if (!is.null(listener))
                    listener(index = i, 
                             total = sim$n.sim, 
                             done = T)
                
                sim
            })
            
            do.join.simulation.sets(sim.list, finalize=T)
        },
        
        get.description = function(version)
        {
            stop("The method 'get.description' must be implemented in a descendant class of jheem.intervention")
        },
        
        equals = function(other)
        {
            if (!setequal(class(self), class(other)))
                return(F)
            if (xor(is.null(private$i.parameter.distribution), is.null(other$parameter.distribution)))
                return(F)
            if (!is.null(private$i.parameter.distribution)) {
                if (!distributions::distributions.equal(private$i.parameter.distribution,
                                                        other$parameter.distribution))
                    return(F)
            }
            else private$is.equal.to(other)
        },
        
        get.start.time = function(parameters=self$parameters, throw.error.if.unable.to.calculate=T)
        {
            foregrounds = self$get.intervention.foregrounds()
            if (length(foregrounds)==0)
                -Inf
            else
            {
                foregrounds.min.start.time = lapply(foregrounds, function(frgd){frgd$min.start.time})
                min.start.time.is.null = sapply(foregrounds.min.start.time, is.null)
                if (any(min.start.time.is.null))
                {
                    if (!is.null(parameters))
                    {
                        print("We have not implemented trying to resolve against parameters to calculate the start time")
                        
                        # Note to self - need to do this for every set of parameters in paramters, then take the min
                        # foregrounds[min.start.time.is.null] = lapply(foregrounds[min.start.time.is.null], function(frgd){
                        #     frgd$resolve.effects(parameters = parameters, error.prefix="Error resolving effect when trying to get start time")  
                        # })
                        # 
                        # foregrounds.min.start.time = lapply(foregrounds, function(frgd){frgd$min.start.time})
                        # min.start.time.is.null = sapply(foregrounds.min.start.time, is.null)
                    }
                    
                    if (any(min.start.time.is.null))
                    {
                        if (throw.error.if.unable.to.calculate)
                            stop(paste0("Cannot get.start.time() for intervention ",
                                        ifelse(is.null(self$code), "",
                                               paste0(" '", self$code, "'")),
                                        " - the effect times for some foregrounds have not been resolved"))
                        else
                            return (NULL)
                    }
                }
                
                floor(min(unlist(foregrounds.min.start.time)))
            }
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
        
        parameters = function(value)
        {
            if (missing(value))
                private$i.parameters
            else
                stop("Cannot modify 'parameters' for a jheem.intervention - they are read-only")
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
                print("The 'depends.on' active binding must be implemented at the subclass-level for a jheem.intervention") #stop() won't build
            else
                stop("Cannot modify 'depends.on' for a jheem.intervention - it is read-only")
        }
            
    ),
    
    private = list(
        
        i.name = NULL,
        i.code = NULL,
        i.parameters = NULL,
        i.parameter.distribution = NULL,
        i.generate.parameters.function = NULL,
        
        prepare.to.run = function(engine, 
                                  sim,
                                  keep.from.year,
                                  keep.to.year,
                                  verbose)
        {
            # The default does nothing
        },
        
        do.run = function(engine, sim.index, parameters, previous.parameter.values, verbose)
        {
            stop("The method 'do.run' must be implemented in a descendant class of jheem.intervention")
        },
        
        is.equal.to = function(other)
        {
            # print("for now, testing equality of interventions with code alone")
            # F
           stop("The method 'is.equal.to' must be implemented in a descendant class of jheem.intervention")
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
                             parameters = NULL,
                             parameter.distribution = NULL,
                             generate.parameters.function = NULL,
                             overwrite.existing.intervention = F)
            # Register to the intervention manager
            register.intervention(self, overwrite.existing = F)
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
        
        initialize = function(code, name, parameters, parameter.distribution, generate.parameters.function, overwrite.existing.intervention=F)
        {
            super$initialize(code = code,
                             name = name,
                             parameters = parameters,
                             parameter.distribution = parameter.distribution,
                             generate.parameters.function = generate.parameters.function,
                             overwrite.existing.intervention = overwrite.existing.intervention)
            # Register to the intervention manager
            if (!is.null(code))
                register.intervention(self, overwrite.existing = overwrite.existing.intervention)
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
                              parameters,
                              parameter.distribution,
                              generate.parameters.function,
                              code,
                              name,
                              overwrite.existing.intervention=F)
        {
            super$initialize(code = code,
                             name = name,
                             parameters = parameters,
                             parameter.distribution = parameter.distribution,
                             generate.parameters.function = generate.parameters.function,
                             overwrite.existing.intervention = overwrite.existing.intervention)
            
            private$i.foregrounds = foregrounds
            names(private$i.foregrounds) = sapply(private$i.foregrounds, function(frgd){
                frgd$quantity.name
            })
            private$i.foregrounds = private$i.foregrounds[order(names(private$i.foregrounds))]
            # Register to the intervention manager
            if (!is.null(code))
                register.intervention(self, overwrite.existing = overwrite.existing.intervention)
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