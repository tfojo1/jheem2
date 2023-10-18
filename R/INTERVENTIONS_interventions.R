

##-----------------------------------------------------##
##-----------------------------------------------------##
##-- THE PUBLIC INTERFACE for CREATING INTERVENTIONS --##
##-----------------------------------------------------##
##-----------------------------------------------------##

#'@name Create an Intervention
#'
#'@param ... Either (a) objects of class 'target.population', (b) objects of class 'intervention.effect', or (c) lists containing only 'target.population' or 'intervention.effect' objects. Must contain at least one 'target.population' object and at least one 'intervention.effect' object
#'@param code
#'@param name
#'
#'@details Creates a 'jheem.intervention' object where all the intervention effects given in ... apply to all the target populations given in ...
#'
#'@export
create.intervention <- function(..., code=NULL, name=NULL)
{
    
}

#'@name Join Multiple Interventions into a Single Intervention
#'
#'@inheritParams create.intervention
#'@param ... One or more interventions to join. These may be either objects of class jheem.intervention or lists which contain only objects of class jheem.intervention
#'@param sequential
#'
#'@export
join.interventions <- function(..., code=NULL, name=NULL, sequential=F)
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
                if (is(sub.elem, 'jheem.intervention'))
                {
                    if (!sub.elem$is.combinable)
                        stop(paste0("Interventions passed to join.interventions must be *combinable*. The ",
                                    get.ordinal(j), " element of the ",
                                    get.ordinal(i), " element of ... is not combinable"))
                }
                else
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
                if (!elem$is.combinable)
                    stop(paste0("Interventions passed to join.interventions must be *combinable*. The ",
                                get.ordinal(i), " element of ... is not combinable"))
                else
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
    else
        JHEEM.COMBINED.STANDARD.INTERVENTION$new(name=name, code=code,
                                                 sub.interventions=sub.interentions)
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

#'@export
get.intervention <- function(code, throw.error.if.missing=T, error.prefix='')
{
    if (!is.character(code) || length(code)!=1 || is.na(code))
        stop(paste0(error.prefix, "'code' must be a single, non-NA character value"))
    
    rv = INTERVENTION.MANAGER[[code]]
    if (throw.error.if.missing && is.null(rv))
        stop(paste0(error.prefix, "No intervention with code '", code, "' has been registered"))
    
    rv
}

register.intervention <- function(intervention, error.prefix='')
{
    if (!is(intervention, 'jheem.intervention') && R6::is.R6(int))
        stop(paste0(error.prefix, "Cannot register intervention - 'intervention' must be an R6 object with class 'jheem.intervention'"))
    
    error.prefix = paste0(error.prefix, "Cannot register intervention '", intervention$name, "' - ")
    
    # Check intervention validity
    if (is.null(intervention$code))
        stop(paste0(error.prefix, "interventions can only be registered if they have a code specified"))
        
    if (!is.character(intervention$code) || length(intervention$code)!=1 || is.na(intervention$code) || 
        nchar(intervention$code)<MINIMUM.INTERVENTION.CODE.NCHAR || nchar(intervention$code)>MAXIMUM.INTERVENTION.CODE.NCHAR)
        stop(paste0(error.prefix, "Invalid intervention: 'code' must be a single, non-NA character value with between ",
                    MINIMUM.INTERVENTION.CODE.NCHAR, " and ", MAXIMUM.INTERVENTION.CODE.NCHAR, "letters"))
    
    if (!is.character(intervention$name) || length(intervention$name)!=1 || is.na(intervention$name) || 
        nchar(intervention$name)<MINIMUM.INTERVENTION.CODE.NCHAR || nchar(intervention$name)>MAXIMUM.INTERVENTION.CODE.NCHAR)
        stop(paste0(error.prefix, "Invalid intervention: 'name' must be a single, non-NA character value with between ",
                    MINIMUM.INTERVENTION.CODE.NCHAR, " and ", MAXIMUM.INTERVENTION.CODE.NCHAR, "letters"))
    
    if (is.logical(intervention$is.combinable) && length(intervention$is.combinable) && !is.na(intervention$is.combinable))
        stop(paste0("Invalid intervention: 'is.combinable' must be a single, non-NA logical value"))
          
    
    # Is something already registered?
    already.registered = get.intervention(intervention$code, throw.error.if.missing = F)
    if (!is.null(already.registered) && !already.registered$equals(intervention))
        stop(paste0("A different intervention has already been registered with the code '", intervention$code, "'"))

    # Register it    
    INTERVENTION.MANAGER[[intervention$code]] = intervention
    
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
                              code)
        {
            # Do some error checking
            
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
                       check.consistency=NULL,
                       max.run.time.seconds=Inf,
                       keep.years=start.year:end.year,
                       atol=1e-04, rtol=1e-04,
                       save.engine = F,
                       error.prefix='')
        {
            if (!is(sim, 'jheem.simulation'))
                stop(paste0(error.prefix, "'sim' must be an object of class 'jheem.simulation'"))
            
            # Get the engine
            engine = sim$get.engine()
            if (is.null(check.consistency))
                check.consistency = !engine$has.been.crunched()
            
            # Set this intervention to the engine
            engine$set.intervention(self)
            
            # Set up the engine for the intervention
            private$do.prepare.to.run(engine)
            
            # Run
            new.sim = engine$run(start.year = start.year,
                                 end.year = end.year,
                                 check.consistency = check.consistency,
                                 prior.sim = sim,
                                 max.run.time.seconds = max.run.time.seconds,
                                 keep.years = keep.years,
                                 atol = atol, rtol = rtol,
                                 save.engine = save.engine)
        },
        
        get.description = function(version)
        {
            stop("The method 'get.description' must be implemented in a descendant class of jheem.intervention")
        },
        
        equals = function(int)
        {
            if (!setequal(class(self), class(int)))
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
        }
    ),
    
    private = list(
        
        i.name = NULL,
        i.code = NULL,
        
        do.prepare.to.run = function(engine)
        {
            stop("The method 'do.prepare.to.run' must be implemented in a descendant class of jheem.intervention")
        }
    )
)

SINGLE.ITERATION.INTERVENTION = R6::R6Class(
    'single.iteration.intervention',
    inherit = JHEEM.INTERVENTION,
    
    public = list(
        
        initialize = function(code, name)
        {
            
        },
        
        # Returns a list of one or more jheem.model.foreground objects
        # engine is an optional argument, which will yield foregrounds optimized for that particular engine
        render.foregrounds = function(version, location, engine=NULL)
        {
            stop("The method 'render.foregrounds' must be implemented in a descendant class of jheem.intervention")
        },
        
        
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
        
        initialize = function(target.populations,
                              intervention.effects)
        {
            
        },
        
        resolve.dependencies = function()
        {
            stop("The method 'resolve.dependencies' must be implemented in a descendant class of jheem.standard.intervention")
        },
        
        reduce.to.standard.intervention = function()
        {
            self
        },
        
        get.description = function(model.specification)
        {
            stop("need to implement")
        },
        
        equals = function(int)
        {
            if (!setequal(class(self), class(int)))
                F
            else
            {
                
            }
        },
        
        get.target.populations.for.element = function(element.name)
        {
            private$i.target.populations.by.target.element[[element.name]]
        },
        
        get.intervention.effects.for.element = function(element.name)
        {
            private$i.intervention.effects.by.target.element[[element.name]]
        }
    ),
    
    active = list(
        
        unresolved.dependencies = function(value)
        {           
            stop("The active method 'unresolved.dependencies' must be implemented in a descendant class of jheem.standard.intervention")
        },
        
        target.element.names = function(value)
        {
            if (missing(value))
                names(private$i.target.populations.by.target.element)
            else
                stop("Cannot modify 'target.element.names' for a jheem.intervention - they are read-only")
        }
    ),
    
    private = list(
        
        # both named lists, with the same length and same names
        # The name of each list element of the target model.element
        # Each element of each list has the same length as the corresponding element in the other
        i.target.populations.by.target.element = NULL,
        i.intervention.effects.by.target.element = NULL
    )
)

JHEEM.COMBINED.INTERVENTION = R6::R6Class(
    'jheem.combined.intervention',
    inherit = JHEEM.INTERVENTION,
    
    public = list(
        
        initialize = function(name, code, ...)
        {
            super$initialize(name = name, code = code, is.combinable = T)
            
            #do some error checking
            if (!is.list(sub.interventions))
                stop("sub.interven")
            
            expanded.sub.interventions = list()
            private$sub.interventions = sub.interventions
        }
    ),
    
    private = list(
        
        i.sub.interventions = NULL
    )
)


##-------------------------------------------------------##
##-------------------------------------------------------##
##-- PUBLIC WITHIN PACKAGE, but PRIVATE TO THE OUTSIDE --##
##-------------------------------------------------------##
##-------------------------------------------------------##

