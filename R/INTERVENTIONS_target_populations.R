

##----------------------------------------------------------##
##----------------------------------------------------------##
##-- THE PUBLIC INTERFACE for CREATING TARGET POPULATIONS --##
##----------------------------------------------------------##
##----------------------------------------------------------##

#'@export
create.target.population <- function(dimension.values,
                                     invert=F)
{
    new('simple.target.population',
        dim.names=dim.names,
        invert=invert)
}

# Wrappers for combination target populations

#'@export
union.target.populations <- function(..., name)
{
    do.union.or.intersect.target.populations(..., combine.function='union', name=name)
}

#'@export
intersect.target.populations <- function(..., name, description)
{
    do.union.or.intersect.target.populations(..., combine.function='intersect', name=name)
}

#'@export
diff.target.populations <- function(pop1, pop2, name)
{
    if (!is(pop1, 'target.population') || !R6::is.R6(pop1))
        stop("'pop1' must be an R6 object of class 'target.population'")
    
    if (!is(pop2, 'target.population') || !R6::is.R6(pop2))
        stop("'pop2' must be an R6 object of class 'target.population'")
             
    COMBINATION.TARGET.POPULATION$new(subpopulations=list(pop1, pop2), combine.function='diff', name=name)
}

##-------------##
##-------------##
##-- HELPERS --##
##-------------##
##-------------##

do.union.or.intersect.target.populations <- function(..., combine.function, name)
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
        stop("... must at least one 'target.population' object")
    else if (length(subpopulations)==1)
        subpopulations[[1]]
    else
        COMBINATION.TARGET.POPULATION$new(subpopulations=subpopulations, combine.function=combine.function, name=name)
}

##-----------------------##
##-----------------------##
##-- CLASS DEFINITIONS --##
##-----------------------##
##-----------------------##

TARGET.POPULATION = R6::R6Class(
    'target.population',
    
    public = list(
        initialize = function(dimension.values,
                              name)
        {
            # Check arguments
            
            private$i.dimension.values = dimension.values
            private$i.name = name
            private$i.description = description
        },
        
        get.description = function(model.specification)
        {
            stop("This needs to be implemented in a sub-class")
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
        initialize = function(dimension.values,
                              name,
                              invert)
        {
            super$initialize(dimension.values, name=name)
            private$i.invert = invert
        },
        
        get.description = function(model.specification)
        {
            stop("need to implement")
        },
        
        render.population.mask = function(model.specification, dimensions = self$dimensions)
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
        initialize = function(subpopulations, combine.function, name)
        {
            # Check argument classes
            
            # Check combine.function
            if (combine.function=='diff')
            {
                if (length(subpopulations)!=2)
                    stop("If 'combine.function' is 'diff' - there must be exactly two subpopulations")
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
            }
            else
            {
                stop(paste0("Invalid 'combine.function' ('",
                            combine.function, "') - must be one of 'union', 'intersect', or 'diff'"))
            }
            
            # Call the super-constructor
            dimension.values = union.dimension.values.list(lapply(subpopulations, function(subpop){
                subpop$dimension.values
            }))
            super$initialize(dimension.values = dimension.values, name = name)
            
            # Add in the subclass values
            private$i.combine.function = combine.function
            private$i.subpopulations = subpopulations
        },
        
        get.description = function(model.specification)
        {
            sub.descriptions = sapply(self$subpopulations, get.description, model.description=model.description)
            
            stop("need to finish implementing")
        },
        
        render.population.mask = function(model.specification, dimensions = self$dimensions)
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