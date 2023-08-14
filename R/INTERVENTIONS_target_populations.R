

##----------------------------------------------------------##
##----------------------------------------------------------##
##-- THE PUBLIC INTERFACE for CREATING TARGET POPULATIONS --##
##----------------------------------------------------------##
##----------------------------------------------------------##

#'@name Create a Target Population to Which Interventions May be Applied
#'
#'@param ... Dimension values that denote the categories that comprise this target population. Each element must be namned with the name of the dimension, and can be either (1) character values, (2) integers, or (3) logical vectors
#'@param name A short (<= 20 characters) descriptive name for the target population. Should be nicely formatted for public display
#'@param description An optional extended description of the population
#'@param invert A logical indicating whether the target population should be the INVERSE of the characteristics specified in ... (ie, everyone who does NOT fall into those categories)
#'
#'@family Functions to create Target Populations
#'
#'@export
create.target.population <- function(...,
                                     name,
                                     description = NULL,
                                     invert=F)
{
    SIMPLE.TARGET.POPULATION$new(...,
                                 name = name,
                                 description = description,
                                 invert = invert)
}

# Wrappers for combination target populations

#'@name Create a Target Population that is the UNION Other Target Populations
#'
#'@inheritParams create.target.population
#'@param ... One or more target.population objects or lists of target populations
#'
#'@family Functions to create Target Populations
#'
#'@export
union.target.populations <- function(..., name, description=NULL)
{
    do.union.or.intersect.target.populations(...,
                                             combine.function = 'union',
                                             name = name,
                                             description = description)
}

#'@name Create a Target Population that is the INTERSECTION of Other Target Populations
#'
#'@inheritParams union.target.populations
#'
#'@family Functions to create Target Populations
#'
#'@export
intersect.target.populations <- function(..., name, description=NULL)
{
    do.union.or.intersect.target.populations(..., 
                                             combine.function = 'intersect',
                                             name = name,
                                             description = description)
}

#'@name Create a Target Population that is the DIFFERENCE between two other Target Populations
#'
#'@param pop1,pop2 Target Population Objects
#'
#'@details The resulting target population contains everyone who IS in pop1 but NOT in pop2
#'
#'@family Functions to create Target Populations
#'
#'@export
diff.target.populations <- function(pop1, pop2, name, description=NULL)
{
    if (!is(pop1, 'target.population') || !R6::is.R6(pop1))
        stop("'pop1' must be an R6 object of class 'target.population'")
    
    if (!is(pop2, 'target.population') || !R6::is.R6(pop2))
        stop("'pop2' must be an R6 object of class 'target.population'")
             
    COMBINATION.TARGET.POPULATION$new(subpopulations = list(pop1, pop2), 
                                      combine.function = 'diff',
                                      name = name,
                                      description = description)
}

##-------------##
##-------------##
##-- HELPERS --##
##-------------##
##-------------##

do.union.or.intersect.target.populations <- function(..., combine.function, name, description)
{
    args = list(...)
    subpopulations = list()
    
    for (i in 1:length(args))
    {
        elem = args[[i]]
        if (is.list(elem))
        {
            for (j in 1:length(sub.elem))
            {
                sub.elem = elem[[j]]
                if (!is(sub.elem, 'target.population'))
                    stop(paste0("All elements of ... must be either 'target.population' objects or lists which contain only 'target.population' objects. The ",
                                get.ordinal(i), " element of ... is a list that contains elements that are not 'target.population' objects"))
            }
            subpopulations = c(subpopulations, elem)
        }
        else if (is(elem, 'target.population'))
        {
            subpopulation = c(subpopulations, list(elem))
        }
        else
            stop(paste0("All elements of ... must be either 'target.population' objects or lists which contain only 'target.population' objects. The ",
                        get.ordinal(i), " element of ... is neither"))
    }
    
    if (length(subpopulations)==0)
        stop("Cannot ", combine.function, " target populations: ... must contain at least one 'target.population' object")
    else if (length(subpopulations)==1)
        subpopulations[[1]]
    else
        COMBINATION.TARGET.POPULATION$new(subpopulations = subpopulations, 
                                          combine.function = combine.function,
                                          name = name,
                                          description = description)
}

##-----------------------##
##-----------------------##
##-- CLASS DEFINITIONS --##
##-----------------------##
##-----------------------##

MAX.NCHAR.TARGET.POPULATION.NAME = 20

TARGET.POPULATION = R6::R6Class(
    'target.population',
    
    public = list(
        initialize = function(dimension.values,
                              name)
        {
            # Check Name
            if (!is.character(name) || length(name)!=1 || is.na(name) || nchar(name)==0)
                stop(paste0("Cannot create ", self$descriptor, 
                            ": 'name' must be a single, non-empty, non-NA character value"))
            
            if (nchar(name)>MAX.NCHAR.TARGET.POPULATION.NAME)
                stop(paste0("Cannot create ", self$descriptor, 
                            ": the name ('", name, 
                            "') cannot be longer than ", 
                            MAX.NCHAR.TARGET.POPULATION.NAME, " characters"))
            
            
            if (string.contains.invalid.characters(name, valid.characters = NUMBERS.LETTERS.DASH.PERIOD.SPACE))
            {
                invalid.characters = setdiff(unlist(strsplit(name, split='')),
                                             strsplit(NUMBERS.LETTERS.DASH.PERIOD.SPACE, split='')[[1]])
                
                stop(paste0("Cannot create ", self$descriptor, 
                            ": the name ('", name, "') cannot contain ", 
                            collapse.with.or("'", invalid.characters, "'"), 
                            " - it can only contain numbers, letters, periods, dashes, and spaces"))
            }
            
            # Assume dimension values has already been validated
            
            # Check description
            if (!is.null(description) && 
                (!is.character(description) || length(description)!=1 || is.na(description) || nchar(description)==0))
                stop(paste0("Cannot create ", self$descriptor, " '", name,
                            ": if 'description' is not NULL, it must be a single, non-empty, non-NA character value"))
                
            # Store variables
            private$i.dimension.values = dimension.values
            private$i.name = name
            private$i.description = description
        },

        render.population.mask = function(model.specification, dimensions = self$dimensions)
        {
            stop("This needs to be implemented in a sub-class")
        },
        
        equals = function(tpop)
        {
            stop("The 'equals' function needs to be implemented in a sub-class of target population")
        }
    ),
    
    active = list(
        dimension.values = function(value)
        {
            if (missing(value))
                private$i.dimension.values
            else
                stop("Cannot modify 'dimension.values' for a target.population - they are read-only")
        },
        
        dimensions = function(value)
        {
            if (missing(value))
                names(self$dimension.values)
            else
                stop("Cannot modify 'dimensions' for a target.population - they are read-only")
                
        },
        
        name = function(value)
        {
            if (missing(value))
                names(self$i.name)
            else
                stop("Cannot modify 'name' for a target.population - it is read-only")
        },
        
        description = function(value)
        {
            if (missing(value))
                names(self$i.description)
            else
                stop("Cannot modify 'description' for a target.population - it is read-only")
        }
    ),
    
    private = list(
        i.dimension.values = NULL,
        i.name=NULL
    )
)

SIMPLE.TARGET.POPULATION = R6::R6Class(
    'simple.target.population',
    
    public = list(
        initialize = function(...,
                              name,
                              description,
                              invert)
        {
            dimension.values = list(...)
            super$initialize(dimension.values = dimension.values,
                             name = name,
                             description = description)
            
            error.prefix = paste0("Cannot create ", self$descriptor, " '", name, "': ")
            
            if (length(dimension.values)>0)
                check.dimension.values.valid(dimension.values = dimension.values,
                                             variable.name.for.error = "the elements of ...",
                                             allow.empty = F,
                                             allow.duplicate.values.within.dimensions = F,
                                             error.prefix = error.prefix)
            
            
            if (!is.logical(invert) || length(invert)!=1 || is.na(invert))
                stop(paste0(error.prefix, "'invert' must be a single, non-NA logical value (ie TRUE or FALSE)"))

            private$i.invert = invert
        },
        
        render.population.mask = function(ontology, error.prefix)
        {
            # Check dimensions
            invalid.dims = setdiff(self$dimensions, model.specification$all.dimensions)
            if (length(invalid.dims)>0)
                stop(paste0("The following dimension",
                            ifelse(length(invalid.dims)==1, " is", "s are"),
                            " specified in the target.population, but are NOT present in the model specification: ",
                            paste0("'", invalid.dims, "'", collapse=', ')))
            
            # Check dimension values
            sapply(self$dimensions, function(d){
                invalid.values = setdiff(self$dimension.values[[d]], 
                                         model.specification$all.dimension.names[[d]])
                
                if (length(invalid.values)>0)
                    stop(paste0("The following value",
                                ifelse(length(invalid.values)==1, " was", "s were"),
                                " specified for the target.population in the '", d, 
                                "' dimension, but ",
                                ifelse(length(invalid.values)==1, "is", "are"),
                                " not present in the model specification: ",
                                paste0("'", invalid.values, "'", collapse=', ')))
            })
            
            # Make the array
            rv.dim.names = model.specification$all.dimension.names[intersect(model.specification$dimensions, self$dimensions)]
            rv = array(private$i.invert,
                       dim = sapply(rv.dim.names, length),
                       dimnames = rv.dim.names)
            
            # Access and set
            array.access(rv, dimension.values=self$dimension.values) = !private$i.invert
            
            # Return
            rv
        },
        
        equals = function(tpop)
        {
            if (setequal(class(self), class(tpop)))
            {
                self$inverted == tpop$inverted &&
                    dimension.values.equal(self$dimension.values, tpop$dimension.values)
            }
            else
                F
        }
    ),
    
    active = list(
        
        descriptor = function(value)
        {
            if (missing(value))
                'simple target population'
            else
                stop("Cannot modify 'descriptor' for a target.population - it is read-only")
        },
        
        inverted = function(value)
        {
            if (missing(value))
                private$i.invert
            else
                stop("Cannot modify 'inverted' for a simple.target.population - it is read-only")
        }
    ),
    
    private = list(
        i.invert = NULL
    )
)

COMBINATION.TARGET.POPULATION = R6::R6Class(
    'combination.target.population',
    
    public = list(
        initialize = function(subpopulations, 
                              combine.function, 
                              name,
                              description)
        { 
            # Check argument classes
            if (any(!sapply(subpopulations, is, 'target.population')))
                stop(paste0("Cannot create ", self$descriptor, ": subpopulations must all be objects of class 'target.population'"))
            
            # Call the super-constructor
            dimension.values = union.dimension.values.list(lapply(subpopulations, function(subpop){
                subpop$dimension.values
            }))
            
            super$initialize(dimension.values = dimension.values, name = name)
            error.prefix = paste0("Cannot create ", self$descriptor, " '", name, "': ")
            
            
            # Check combine.function
            if (!is.character(combine.function) || length(combine.function)!=1 || is.na(combine.function))
                stop(paste0(error.prefix, "'combine.function' must be a single, non-NA character value"))
            
            if (combine.function=='diff')
            {
                if (length(subpopulations)!=2)
                    stop(paste0(error.prefix, "If 'combine.function' is 'diff' - there must be exactly two subpopulations"))
            }
            else if (combine.function == 'union' || combine.function=='intersect')
            {
                new.subpopulations = list()
                for (subpop in subpopulations)
                {
                    if (is(subpop, 'combination.target.population') && subpop$combine.function==combine.function)
                        new.subpopulations = c(new.subpopulations, subpop$subpopulations)
                    else
                        new.subpopulations = c(new.subpopulations, subpop)
                }
                subpopulations = new.subpopulations
            }
            else
            {
                stop(paste0(error.prefix, "Invalid 'combine.function' ('",
                            combine.function, "') - must be either 'union', 'intersect', or 'diff'"))
            }
            
            
            # Add in the subclass values
            private$i.combine.function = combine.function
            private$i.subpopulations = subpopulations
        },
        
        get.description = function(model.specification)
        {
            sub.descriptions = sapply(self$subpopulations, get.description, model.description=model.description)
            
            stop("need to finish implementing")
        },
        
        render.population.mask = function(ontology, error.prefix)
        {
            mask = private$i.subpopulations[[1]]$render.population.mask(model.specification, dimensions=dimensions)
            
            for (i in 2:length(private$i.subpopulations))
            {
                mask2 = private$i.subpopulations[[i]]$render.population.mask(model.specification, dimensions=dimensions)
                
                if (combine.function == 'union')
                    mask = mask | mask2
                else if (combine.function == 'intersect')
                    mask = mask & mask2
                else if (combine.function == 'diff')
                    mask = mask & !mask2
                else
                    stop(paste0("Invalid combine.function '", private$i.combine.function,
                                "' was given to the target.population '", self$name, "'"))
            }
            
            mask
        },
        
        equals = function(tpop)
        {
            if (setequal(class(self), class(tpop)))
            {
                if (self$combine.function == tpop$combine.function && 
                    length(self$subpopulations) == length(tpop$subpopulations))
                {
                    if (self$combine.function=='diff')
                        self$subpopulations[[1]]$equals(tpop$subpopulations[[1]]) && 
                        self$subpopulations[[2]]$equals(tpop$subpopulations[[2]])
                    else
                    {
                        already.matched.from.tpop = rep(F, length(tpop$subpopulations))
                        for (self.subpop in self$subpopulations)
                        {
                            found.match = F
                            for (i in (1:length(tpop$subpopulations))[!already.matched.from.tpop])
                            {
                                if (self.subpop$equals(tpop$subpopulations[[i]]))
                                {
                                    found.match = T
                                    already.matched.from.tpop[i] = T
                                    break
                                }
                            }
                            
                            if (!found.match)
                                return (F)
                        }
                        
                        T
                    }
                }
                else
                    F
            }
            else
                F
        }
    ),
    
    active = list(
        subpopulations = function(value)
        {
            if (missing(value))
                private$i.subpopulations
            else
                stop("Cannot modify 'subpopulations' for a target.population - it is read-only")
        },
        
        combine.function = function(value)
        {
            if (missing(value))
                private$i.combine.function
            else
                stop("Cannot modify 'combine.function' for a target.population - it is read-only")
        }
    ),
    
    private = list(
        i.subpopulations = NULL,
        i.combine.function = NULL
    )
)