
##--------------------------##
##-- THE PUBLIC INTERFACE --##
##--------------------------##


#@Andrew need to document more
#'@export
instantiate.likelihood <- function(instructions.code,
                                   version,
                                   location,
                                   error.prefix = "Error instantiating likelihood from instructions: ")
{
    instr = get.likelihood.instructions(code=instructions.code, error.prefix=error.prefix)
    instr$instantiate.likelihood(version = version,
                                 location = location,
                                 instructions = instr,
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

# Internal to the package and this code file
# Invoked only by the constructor of likelihood instructions objects
register.likelihood.instructions(instructions, error.prefix)
{
    #@Andrew - implement
    
    # If there is already this code in the manager
    # - if these instructions do not equal the previously registered instructions, throw an error
    
    # What if an equal instructions have been already registered under a different code?
    # - I think throw an error
    
    
    # Register it    
    LIKELIHOOD.INSTRUCTIONS.MANAGER[[instructions$code]] = instructions
}

get.likelihood.instructions = function(code, error.prefix)
{
    #@Andrew
    # Throw an error if we can't find it
    
    LIKELIHOOD.INSTRUCTIONS.MANAGER[[code]]
}


##-----------------------------------------##
##-- THE LIKELIHOOD *INSTRUCTIONS* CLASS --##
##-----------------------------------------##

#'@name Make a single (joint) set of likelihood instructions out of two or more likelihood instructions objects
#'
#'@param ... One or more jheem.likelihood.instructions objects or lists which contain only jheem.likelihood.instructions objects
#'@param code
#'@param name
#'@param description
#'
#'@export
join.likelihood.instructions = function(..., 
                                        code,
                                        name,
                                        description)
{
    #@Andrew need to implement
}



# An abstract class
# Needs to implement the following methods:
# - equals
# - will probably need to override the constructor BUT needs to call the superclass constructor

JHEEM.LIKELIHOOD.INSTRUCTIONS = R6::R6Class(
    class='jheem.likelihood.instructions',
    
    public = list(
        
        initialize = function(code,
                              name,
                              description,
                              outcomes,
                              years,
                              dimensions,
                              weights,
                              likelihood.class.generator,
                              error.prefix)
        {
            # Validate code: 1-5 characters, must be number, letter, . or -
            # Validate name and descriptions (single, non-NA, non-empty character values)
            # Validate outcomes (character vector with length >=1, no NAs or empty character values)
            # Validate years (numeric values)
            # Validate likelihood.class.generator
            # - Needs to be an R6ClassGenerator object
            # - Recurse up the inheritance tree of the classes using $get_inherit(). Something up that chain must have $classname=='jheem.likelihood'
            # Validate weights: must be a jheem.likelihood.weights object     
            # - All dimensions named in weights must be named in the dimensions of this likelihood
            
            # Store values - @Andrew finish this off
            private$i.code = code
            private$i.name = name
            private$i.description = description
            
            private$i.outcomes = outcomes
            private$i.years = years
            private$i.dimensions = dimensions
            
            private$i.weights = weights
            
            private$i.likelihood.class.generator = likelihood.class.generator
            
            # Register
            register.likelihood.instructions(self, error.prefix=error.prefix)
        },
        
        instantiate.likelihood = function(version, location)
        {
            # Validate location
            
            # Make sure that all outcomes for the instructions are registered
            #  to the specification for the version
            
            private$i.likelihood.class.generator$new(instructions=self,
                                                      version=version,
                                                      location=location)
        },
        
        equals = function(other)
        {
            stop("The 'equals' method must be implemented at the sub-class level")
        }
    ),
    
    active = list(
        
        #@Andrew - need to fill in these getters
        
        code = function(value)
        {
            
        },
        
        name = function(value)
        {
            
        },
        
        description = function(value)
        {
            
        },
        
        outcome = function(value)
        {
            
        },
        
        years = function(value)
        {
            
        },
        
        dimensions = function(value)
        {
            
        },
        
        weights = function(value)
        {
            
        },
        
        stratification = function(value)
        {
            if (missing(value))
            {
                if(length(private$i.dimensions)==0)
                    'all'
                else
                    paste0(private$i.dimensions)
            }
            else
                stop("Cannot modify a jheem.likelihood.instruction's 'stratification' - it is read-only")
        },
        
        details = function(value)
        {
            if (missing(value))
            {
                data.frame(
                    outcome = private$i.outcomes,
                    years = collapse.numeric.range(private$i.years),
                    stratification = self$stratification
                )
                
            }
            else
                stop("Cannot modify a jheem.likelihood.instruction's 'details' - they are read-only")
        }
    ),
    
    private = list(
        
        i.code = NULL,
        i.name = NULL,
        i.description = NULL,
        
        i.outcomes = NULL,
        i.years = NULL,
        i.dimensions = NULL,
        
        i.weights = NULL,
        
        i.likelihood.class.generator = NULL
    )
)

JHEEM.JOINT.LIKELIHOOD.INSTRUCTIONS = R6::R6Class(
    class='jheem.joint.likelihood.instructions',
    inherit = JHEEM.LIKELIHOOD.INSTRUCTIONS,
    
    public = list(
        
        initialize = function(sub.instructions)
        {
            # Validate sub-instructions
            # Make sure to flatten out list of sub-instructions
            
            super$initialize(JHEEM.JOINT.LIKELIHOOD)
            private$i.sub.instructions = sub.instructions
        },
        
        equals = function(other)
        {
            # We can just test the sub-instructions odes for equality,
            #  because any sub-instructions will have already been registered to a unique code
            if (is(other, 'jheem.joint.likelihood.instructions'))
            {
                self.sub.codes = sapply(private$i.sub.instructions, function(instr){instr$code})
                other.sub.codes = sapply(other$sub.instructions, function(instr){instr$code})
                
                setequal(self.sub.codes, other.sub.codes)
            }
            else
                F
        }
    ),
    
    active = list(
        
        #@Andrew - fill in getters
        sub.instructions = function(value)
        {
            
        },
        
        details = function(value)
        {
            if (missing(value))
            {
                rv = NULL
                for (instr in private$i.sub.instructions)
                    rv = rbind(rv, instr$details)
            }
            else
                stop("Cannot modify a jheem.likelihood.instruction's 'details' - they are read-only")
        }
    ),
    
    private = list(
        
        i.sub.instructions = NULL
        
    )
)

##----------------------------------##
##-- THE LIKELIHOOD WEIGHTS CLASS --##
##----------------------------------##

#'@name Create Weights to Be Applied in Calculating a JHEEM Likelihood
#'
#'@param total.weight The weight that is applied to all observations in the likelihood
#'@param ... Zero or more named numeric vectors, each with its own name. The names of list(...) represent dimensions to which to attach weights, and the names of each element in a vector represent dimension values
#'
#'@export
create.likelihood.weights <- function(total.weight=1,
                                      ...)
{
    JHEEM.LIKELIHOOD.WEIGHTS$new(total.weight = total.weight,
                                 dimension.value.weights = list(...))
}

JHEEM.LIKELIHOOD.WEIGHTS = R6::R6Class(
    'jheem.likelihood.weights',
    portable = F,
    
    public = list(
        
        initialize = function(total.weight,
                              dimension.value.weights)
        {
            # Validate 
            
            # Total weight is a single numeric value that is not NA and >0
            
            # dimension.value.weights is either (a) an empty list or (b) a named list
            # Each element is a numeric vector with non-NULL names
            # The values of each element are all not NA and >0
        }
    )
)


##--------------------------##
##-- THE LIKELIHOOD CLASS --##
##--------------------------##

# An abstract class
# Subclasses need to implement the following methods:
# - do.compute
JHEEM.LIKELIHOOD = R6::R6Class(
    class='jheem.likelihood',
    inherit='jheem.entity',
    portable = F,
    
    public = list(
        
        initialize = function(version,
                              location,
                              instructions,
                              error.prefix)
        {
            #@Andrew implement
            # Validate instructions
            # (the superclass constructor will validate version and location)
            
            # Call the superclass (jheem.entity) constructor
            super$initialize(version = version,
                             location = location,
                             type = 'likelihood',
                             errore.prefix = error.prefix)
            
            # Store instructions
            private$i.instructions = instructions
        },
        
        #'@param check.consistency - Whether to spend time checking to make sure everything is internally consistent. Setting to F is faster, but may generate wierd error messages if there are bugs
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
        
        i.instructions = NULL,
        
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

JHEEM.JOINT.LIKELIHOOD = R6::R6Class(
    class='jheem.joint.likelihood',
    inherit = JHEEM.LIKELIHOOD,
    portable = F,
    
    public = list(
        
        initialize = function(version,
                              location,
                              instructions,
                              error.prefix)
        {
            super$initialize(version=version,
                             location=location,
                             instructions=instructions,
                             error.prefix=error.prefix)
            
            private$i.sub.likelihoods = lapply(instructions$sub.instructions, function(instr){
                instr$instantiate.likelihood(version=version,
                                             location=location,
                                             error.prefix=error.prefix)
            })
        }
    ),
    
    private = list(
        
        i.sub.likelihoods = NULL,
        
        do.compute = function(sim, log, check.consistency)
        {
            sub.values = sapply(private$i.sub.likelihoods, function(lik){
                lik$compute(sim, log=log, check.consistency=check.consistency)
            })
            
            if (log)
                sum(sub.values)
            else
                prod(sub.values)
        }
    )
)
