
##--------------------------##
##-- THE PUBLIC INTERFACE --##
##--------------------------##


#@Andrew need to document more
#'@export
instantiate.likelihood <- function(instructions.code,
                                   version,
                                   location,
                                   data.manager, # pass this to the child classes
                                   error.prefix = "Error instantiating likelihood from instructions: ")
{
    instr = get.likelihood.instructions(code=instructions.code, error.prefix=error.prefix)
    instr$instantiate.likelihood(version = version,
                                 location = location,
                                 data.manager = data.manager,
                                 error.prefix = error.prefix)
}



##---------------##
##-- CONSTANTS --##
##---------------##


JHEEM.LIKELIHOOD.CODE.ITERATION = '2.0'

##----------------------------------##
##-- THE LIKELIHOOD MANAGER CLASS --##
##    (and related functionality)   ##
##----------------------------------##

LIKELIHOOD.INSTRUCTIONS.MANAGER = new.env()

#'@export
register.likelihood.instructions = function(instructions,
                                            code,
                                            description,
                                            error.prefix = "Error registering likelihood instructions: ")
{
    
    # if (!R6::is.R6(instructions) || !is(instructions, 'jheem.likelihood.instructions'))
    if (!R6::is.R6(instructions))
        stop("'instructions' must be an R6 object with parent class 'jheem.likelihood.instructions'")
    instructions$register(code = code,
                          description = description,
                          error.prefix = error.prefix)
    
}

get.likelihood.instructions = function(code, error.prefix)
{
    LIKELIHOOD.INSTRUCTIONS.MANAGER[[code]]
}


##-----------------------------------------##
##-- THE LIKELIHOOD *INSTRUCTIONS* CLASS --##
##-----------------------------------------##





# An abstract class
# Needs to implement the following methods:
# - equals
# - will probably need to override the constructor BUT needs to call the superclass constructor

JHEEM.LIKELIHOOD.INSTRUCTIONS = R6::R6Class(
    class='jheem.likelihood.instructions',
    
    public = list(
        
        initialize = function(outcome.for.data,
                              outcome.for.sim,
                              from.year,
                              to.year,
                              omit.years,
                              dimensions,
                              denominator.dimensions,
                              levels.of.stratification,
                              weights,
                              equalize.weight.by.year,
                              likelihood.class.generator,
                              error.prefix='')
        {
            # Validate code: 1-5 characters, must be number, letter, . or -
            # Validate descriptions (single, non-NA, non-empty character values)
            # Validate outcomes (character vector with length >=1, no NAs or empty character values)
            # Validate years (numeric values)
            # Validate likelihood.class.generator
            # - Needs to be an R6ClassGenerator object
            # - Recurse up the inheritance tree of the classes using $get_inherit(). Something up that chain must have $classname=='jheem.likelihood'
            # Validate weights: must be a jheem.likelihood.weights object     
            # - All dimensions named in weights must be named in the dimensions of this likelihood
            
            # Store values - @Andrew finish this off
            # private$i.description = description
            
            private$i.outcome.for.data = outcome.for.data
            private$i.outcome.for.sim = outcome.for.sim
            
            # years cannot be calculated until the likelihood is instantiated because it requires checking the data manager's upper and lower bounds
            private$i.from.year = from.year
            private$i.to.year = to.year
            private$i.omit.years = omit.years
            private$i.dimensions = dimensions
            
            # Create the stratifications list
            private$i.stratifications = list()
            for (level in levels.of.stratification) {
                if (level == 0)
                    private$i.stratifications = c(private$i.stratifications, list(dimensions))
                else
                    private$i.stratifications = c(private$i.stratifications, combn(dimensions, level, simplify = F))
            }
            
            # Validate and process weights list
            private$i.weights = list()
            total.weight = 1
            for (w in weights) {
                if (is.numeric(w)) {
                    total.weight = total.weight * prod(w)
                }
                else if (is(w, 'jheem.likelihood.weights')) {
                    if (length(w$dimension.values) == 0)
                        total.weight = total.weight * w$total.weight
                    else
                        private$i.weights[[length(private$i.weights) + 1]] = w
                } else
                    stop(paste0(error.prefix, "'weights' must be a list containing only numeric vectors and jheem.likelihood.weights objects"))
            }
            private$i.weights[[length(private$i.weights) + 1]] = create.likelihood.weights(total.weight = total.weight)
            
            private$i.equalize.weight.by.year = equalize.weight.by.year
            private$i.likelihood.class.generator = likelihood.class.generator
            # browser()
            
            # # Register
            # register.likelihood.instructions(self, error.prefix=error.prefix)
        },
        
        # only need to overwrite here
        instantiate.likelihood = function(version, location, data.manager, throw.error.if.no.data = F, error.prefix = '')
        {
            # Validate location
            
            # Check that likelihood instructions are registered?
            
            # Make sure that all outcomes for the instructions are registered
            #  to the specification for the version
            private$i.likelihood.class.generator$new(instructions=self,
                                                     version=version,
                                                     location=location,
                                                     data.manager=data.manager,
                                                     throw.error.if.no.data=throw.error.if.no.data,
                                                     error.prefix=error.prefix)
        },
        
        register = function(code,
                            description,
                            error.prefix = "Error registering likelihood instructions: ")
        {
            # Add arguments: code (no more than 5 characters (numbers, letters, periods, dashes) no underscores!)
            # see function from misc helpers used in model_specification (search NUMBERS.LETTERS)
            
            #@Andrew - implement
            
            # If there is already this code in the manager
            # - if these instructions do not equal the previously registered instructions, throw an error
            
            # What if an equal instructions have been already registered under a different code?
            # - I think throw an error
            
            private$i.description = description
            private$i.code = code
            LIKELIHOOD.INSTRUCTIONS.MANAGER[[private$i.code]] = self
        },
        
        equals = function(other)
        {
            stop("The 'equals' method must be implemented at the sub-class level")
        }
    ),
    
    active = list(
        
        description = function(value)
        {
            if (missing(value))
            {
                private$i.description
            }
            else
                stop("Cannot modify a jheem.likelihood.instruction's 'description' - they are read-only")
        },
        
        outcome.for.data = function(value)
        {
            if (missing(value))
            {
                private$i.outcome.for.data
            }
            else
                stop("Cannot modify a jheem.likelihood.instruction's 'outcome.for.data' - it is read-only")
        },
        outcome.for.sim = function(value)
        {
            if (missing(value))
            {
                private$i.outcome.for.sim
            }
            else
                stop("Cannot modify a jheem.likelihood.instruction's 'outcome.for.sim' - it is read-only")
        },
        
        from.year = function(value)
        {
            if (missing(value))
            {
                private$i.from.year
            }
            else
                stop("Cannot modify a jheem.likelihood.instruction's 'from.year' - it is read-only")
        },
        to.year = function(value)
        {
            if (missing(value))
            {
                private$i.to.year
            }
            else
                stop("Cannot modify a jheem.likelihood.instruction's 'to.year' - it is read-only")
        },
        omit.years = function(value)
        {
            if (missing(value))
            {
                private$i.omit.years
            }
            else
                stop("Cannot modify a jheem.likelihood.instruction's 'omit.years' - they are read-only")
        },
        
        dimensions = function(value)
        {
            if (missing(value))
            {
                private$i.dimensions
            }
            else
                stop("Cannot modify a jheem.likelihood.instruction's 'dimensions' - they are read-only")
        },
        
        weights = function(value)
        {
            if (missing(value))
            {
                private$i.weights
            }
            else
                stop("Cannot modify a jheem.likelihood.instruction's 'weights' - they are read-only")
        },
        
        equalize.weight.by.year = function(value)
        {
            if (missing(value))
            {
                private$i.equalize.weight.by.year
            }
            else
                stop("Cannot modify a jheem.likelihood.instruction's 'equalize.weight.by.year' - it is read-only")
        },
        
        stratifications = function(value)
        {
            if (missing(value))
            {
                private$i.stratifications
            }
            else
                stop("Cannot modify a jheem.likelihood.instruction's 'stratifications' - it is read-only")
        },
        
        details = function(value)
        {
            if (missing(value))
            {
                data.frame(
                    outcome.for.data = private$i.outcome.for.data,
                    outcome.for.sim = private$i.outcome.for.sim,
                    from.year = private$i.from.year,
                    to.year = private$i.to.year,
                    omit.years = private$i.omit.years
                )
                
            }
            else
                stop("Cannot modify a jheem.likelihood.instruction's 'details' - they are read-only")
        }
    ),
    
    private = list(
        i.description = NULL,
        i.code = NULL,
        i.outcome.for.data = NULL,
        i.outcome.for.sim = NULL,
        i.from.year = NULL,
        i.to.year = NULL,
        i.omit.years = NULL,
        i.dimensions = NULL,
        i.stratifications = NULL,
        i.weights = NULL,
        i.equalize.weight.by.year = NULL,
        i.likelihood.class.generator = NULL
    )
)



##----------------------------------##
##-- THE LIKELIHOOD WEIGHTS CLASS --##
##----------------------------------##

#'@name Create Weights to Be Applied in Calculating a JHEEM Likelihood
#'
#'@param total.weight The weight that is applied to all observations in the
#'@param dimension.values
#'
#'@export
create.likelihood.weights <- function(total.weight=1,
                                      dimension.values=list())
{
    JHEEM.LIKELIHOOD.WEIGHTS$new(total.weight = total.weight,
                                 dimension.values = dimension.values)
}

JHEEM.LIKELIHOOD.WEIGHTS = R6::R6Class(
    'jheem.likelihood.weights',
    portable = F,
    
    public = list(
        
        initialize = function(total.weight,
                              dimension.values)
        {
            # Validate 
            
            # Total weight is a single numeric value that is not NA and >0
            
            # dimension.values is either (a) an empty list or (b) a named list...
            # we want to reorder the list to be in alphabetical order by dimension
            
            private$i.total.weight = total.weight
            private$i.dimension.values = dimension.values[sort(names(dimension.values))]
            
        }
    ),
    active = list(
        total.weight = function(value) {
            if (missing(value)) {
                private$i.total.weight
            }
            else
                stop("Cannot modify a jheem.likelihood.weights' 'total.weight' - it is read-only")
        },
        
        dimension.values = function(value) {
            if (missing(value)) {
                private$i.dimension.values
            }
            else
                stop("Cannot modify a jheem.likelihood.weights' 'dimension.values' - it is read-only")
        }
    ),
    private = list(
        i.total.weight = NULL,
        i.dimension.values = NULL
    )
)


##--------------------------##
##-- THE LIKELIHOOD CLASS --##
##--------------------------##

# An abstract class
# Subclasses need to implement the following methods:
# - do.compute
JHEEM.LIKELIHOOD = R6::R6Class(
    class ='jheem.likelihood',
    inherit= JHEEM.ENTITY,
    portable = F,
    
    public = list(
        
        initialize = function(instructions,
                              version,
                              location,
                              data.manager,
                              error.prefix)
        {
            #@Andrew implement
            # Validate instructions
            # (the superclass constructor will validate version and location)
            
            # Call the superclass (jheem.entity) constructor
            super$initialize(version = version,
                             location = location,
                             type = 'likelihood',
                             error.prefix = error.prefix)
            
            # Store instructions... this is only some of the parameters the Instructions object has
            private$i.parameters = instructions$parameters
            private$i.weights = instructions$weights
            private$i.stratifications = instructions$stratifications
            
            # Years must be determined at the subclass level
            
        },
        
        #'@param check.consistency - Whether to spend time checking to make sure everything is internally consistent. Setting to F is faster, but may generate weird error messages if there are bugs
        compute = function(sim, log=T, check.consistency)
        {
            #@Andrew implement
            
            if (check.consistency)
            {
                # Make sure that the sim has the years needed for the likelihood
            }
            
            # Call the subclass function
            private$do.compute(sim, log=log, check.consistency=check.consistency)
        },
        
        # A function-factory
        get.compute.function = function(default.log=T, check.consistency=T)
        {
            function(sim, log=default.log)
            {
                self$compute(sim, log=log, check.consistency=check.consistency)
            }
        }
    ),
    
    active = list(
        
        instructions = function(value)
        {
            
        }
        
    ),
    
    private = list(
        
        i.parameters = NULL,
        i.weights = NULL,
        i.stratifications = NULL,
        
        do.compute = function(sim, log, check.consistency)
        {
            stop("The 'do.compute' function must be overridden at the subclass-level for a jheem.likelihood")  
        },
        
        get.current.code.iteration = function()
        {
            JHEEM.LIKELIHOOD.CODE.ITERATION
        }
    )
    
)


