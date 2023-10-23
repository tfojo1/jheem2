

##----------------------------------------------------------##
##----------------------------------------------------------##
##-- THE PUBLIC INTERFACE for CREATING TARGET POPULATIONS --##
##----------------------------------------------------------##
##----------------------------------------------------------##

#'@name Create a Target Population to Which Interventions May be Applied
#'
#'@param ... Dimension values that denote the categories that comprise this target population. Each element must be namned with the name of the dimension, and can be either (1) character values, (2) integers, or (3) logical vectors
#'@param name A short (<= 20 characters) descriptive name for the target population. Should be nicely formatted for public display
#'@param invert A logical indicating whether the target population should be the INVERSE of the characteristics specified in ... (ie, everyone who does NOT fall into those categories)
#'
#'@family Functions to create Target Populations
#'
#'@export
create.target.population <- function(...,
                                     name,
                                     invert=F)
{
    SIMPLE.TARGET.POPULATION$new(list(...),
                                 name = name,
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
union.target.populations <- function(..., name=NULL)
{
    do.union.or.intersect.target.populations(...,
                                             combine.function = 'union',
                                             name = name)
}

#'@name Create a Target Population that is the INTERSECTION of Other Target Populations
#'
#'@inheritParams union.target.populations
#'
#'@family Functions to create Target Populations
#'
#'@export
intersect.target.populations <- function(..., name=NULL)
{
    do.union.or.intersect.target.populations(..., 
                                             combine.function = 'intersect',
                                             name = name)
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
diff.target.populations <- function(pop1, pop2, name=NULL)
{
    if (!is(pop1, 'target.population') || !R6::is.R6(pop1))
        stop("'pop1' must be an R6 object of class 'target.population'")
    
    if (!is(pop2, 'target.population') || !R6::is.R6(pop2))
        stop("'pop2' must be an R6 object of class 'target.population'")
             
    COMBINATION.TARGET.POPULATION$new(subpopulations = list(pop1, pop2), 
                                      combine.function = 'diff',
                                      name = name)
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
        if (is(elem, 'target.population'))
        {
            subpopulations = c(subpopulations, list(elem))
        }
        else if (is.list(elem))
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
                                          name = name)
}

##-----------------------##
##-----------------------##
##-- CLASS DEFINITIONS --##
##-----------------------##
##-----------------------##

MAX.NCHAR.TARGET.POPULATION.NAME = 30

TARGET.POPULATION = R6::R6Class(
    'target.population',
    
    public = list(
        initialize = function(dimension.values,
                              name,
                              allow.null.name)
        {
            # Check Name
            if (is.null(name))
            {
                if (!allow.null.name)
                    stop(paste0("Cannot create ", self$descriptor, 
                                ": 'name' must be a single, non-empty, non-NA character value"))
            }
            else
            {
                if (!is.character(name) || length(name)!=1 || is.na(name) || nchar(name)==0)
                    stop(paste0("Cannot create ", self$descriptor, 
                                ": 'name' must be a single, non-empty, non-NA character value"))
                
                if (nchar(name)>MAX.NCHAR.TARGET.POPULATION.NAME)
                    stop(paste0("Cannot create ", self$descriptor, 
                                ": the name ('", name, 
                                "') cannot be longer than ", 
                                MAX.NCHAR.TARGET.POPULATION.NAME, " characters"))
                
                if (string.contains.invalid.characters(name, valid.characters = NUMBERS.LETTERS.DASH.PERIOD.COMMA.SPACE))
                {
                    invalid.characters = setdiff(unlist(strsplit(name, split='')),
                                                 strsplit(NUMBERS.LETTERS.DASH.PERIOD.COMMA.SPACE, split='')[[1]])
                    
                    stop(paste0("Cannot create ", self$descriptor, 
                                ": the name ('", name, "') cannot contain ", 
                                collapse.with.or("'", invalid.characters, "'"), 
                                " - it can only contain numbers, letters, periods, commas, dashes, and spaces"))
                }
            }
            
            # Assume dimension values has already been validated
            
                
            # Store variables
            private$i.dimension.values = dimension.values
            private$i.name = name
        },

        render.population.mask = function(specification.metadata,
                                          dim.names = specification.metadata$dim.names, 
                                          render.only.relevant.dimensions = T,
                                          error.prefix = '')
        {
            if (!is.ontology(dim.names))
                check.dim.names.valid(dim.names = dim.names,
                                      variable.name.for.error = 'dim.names',
                                      allow.duplicate.values.across.dimensions = T,
                                      error.prefix = error.prefix)

            bk = dim.names
            if (!is.null(specification.metadata))
                dim.names = specification.metadata$apply.aliases(dim.names, error.prefix=error.prefix)
            
            # Check dimensions
            invalid.dims = setdiff(self$dimensions, names(dim.names))
            if (length(invalid.dims)>0)
                stop(paste0(error.prefix,
                            "The following dimension",
                            ifelse(length(invalid.dims)==1, " is", "s are"),
                            " specified in the target.population, but are NOT present in the given dim.names: ",
                            paste0("'", invalid.dims, "'", collapse=', '),
                            " (allowed dimensions include ", collapse.with.and("'", names(dim.names), "'"), ")"))
  
            # Check dimension values
            
            if (!is.null(specification.metadata))
            {
                dimension.values = specification.metadata$apply.aliases(self$dimension.values)
                resolve.dimension.values.against.dim.names(dimension.values = dimension.values,
                                                           dim.names = dim.names,
                                                           error.prefix = error.prefix)
            }
            
            # Make the array
            if (render.only.relevant.dimensions)
                dim.names = dim.names[intersect(names(dim.names), self$dimensions)]
            
            private$do.render.population.mask(specification.metadata,
                                              dim.names = dim.names,
                                              error.prefix = error.prefix)
        },
        
        equals = function(tpop)
        {
            stop("The 'equals' function needs to be implemented in a sub-class of target population")
        },
        
        get.name = function(bracket.depth=0, bracket.order=c("()","[]","{}"))
        {
            private$i.name
        },
        
        overlaps = function(other)
        {
            if (!is(other, 'target.population') && !is.ontology(other))
            {
                if (is.list(other))
                {
                    check.dimension.values.valid(other,
                                                 variable.name.for.error = 'other',
                                                 allow.empty = T,
                                                 error.prefix = "To calculate the overlap with a target.populations, if 'other' is a list, ")
                    
                    other = as.ontology(other)
                }
                else
                    stop("To calculate the overlap between target.populations, 'other' must be an object of class 'target.population' or a valid dimension.values list")
            }
            
            any(private$get.overlap.mask(other))
        },
        
        print = function(...)
        {
            base::print(paste0("Target Population: '", self$get.name(), "'"))
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
        i.name=NULL,
        
        get.overlap.mask = function(other)
        {
            if (is(other, 'target.population'))
                other.dimension.values = other$dimension.values
            else if (is.ontology(other))
                other.dimension.values = other
            else
                return (NULL)
            
        
            dim.values = union.joined.dimension.values(self$dimension.values, other.dimension.values)
            if (is.null(dim.values))
                return (NULL) #we couldn't join the dimension values
            
            dim.names = lapply(dim.values, function(values){
                if (is.numeric(values))
                    as.character(1:max(values))
                else if (is.logical(values))
                    as.character((1:length(values)))
                else
                    values
            })
            
            mask.self = self$render.population.mask(specification.metadata = NULL,
                                                    dim.names = dim.names, 
                                                    render.only.relevant.dimensions = F,
                                                    error.prefix = paste0("Cannot calculate overlap between target populations '", 
                                                                          self$get.name(), "' and '", other$get.name(), "'"))
            
            if (is.ontology(other))
            {
                mask.other = array(F, dim=sapply(dim.names, length), dimnames=dim.names)
                array.access(mask.other, other) = T
            }
            else
            {
                mask.other = other$render.population.mask(specification.metadata = NULL,
                                                          dim.names = dim.names, 
                                                          render.only.relevant.dimensions = F,
                                                          error.prefix = paste0("Cannot calculate overlap between target populations '", 
                                                                                self$get.name(), "' and '", other$get.name(), "'"))
            }
            
            mask.self & mask.other
        },
        
        do.render.population.mask = function(dim.names, render.only.relevant.dimensions, error.prefix)
        {
            stop("This needs to be implemented in a sub-class")
        }
    )
)

SIMPLE.TARGET.POPULATION = R6::R6Class(
    'simple.target.population',
    inherit = TARGET.POPULATION,
    
    public = list(
        initialize = function(dimension.values,
                              name,
                              invert)
        {
            super$initialize(dimension.values = dimension.values,
                             name = name,
                             allow.null.name = F)
            
            error.prefix = paste0("Cannot create ", self$descriptor, " '", name, "': ")
            
            if (length(dimension.values)>0)
                check.dimension.values.valid(dimension.values = dimension.values,
                                             variable.name.for.error = "the dimension.values (in ...)",
                                             allow.empty = F,
                                             allow.duplicate.values.within.dimensions = F,
                                             error.prefix = error.prefix)
            
            
            if (!is.logical(invert) || length(invert)!=1 || is.na(invert))
                stop(paste0(error.prefix, "'invert' must be a single, non-NA logical value (ie TRUE or FALSE)"))

            private$i.invert = invert
        },
        
        equals = function(tpop)
        {
            if (setequal(class(self), class(tpop)))
            {
                self$inverted == tpop$inverted &&
                    dimension.values.equal(self$dimension.values, tpop$dimension.values,
                                           match.order.of.dimensions = F,
                                           match.order.within.dimensions = F)
            }
            else
                F
        },
        
        get.overlapping.dimension.values = function(other)
        {
            if (is(other, 'target.population'))
                other.dimension.values = other$dimension.values
            else if (is.ontology(other))
                other.dimension.values = other
            else if (is.list(other))
            {
                check.dimension.values.valid(other,
                                             variable.name.for.error = 'other',
                                             allow.empty = T,
                                             error.prefix = "To calculate the overlap between target.populations, if 'other' is a list, ")
            }
            else
                stop("To calculate the overlap between target.populations, 'other' must be an object of class 'target.population' or a valid dimension.values list")
                
            
            other.dimensions = names(other.dimension.values)
            if (length(intersect(self$dimensions, other.dimensions)) == 0)
                return (NULL)
            else if (is(other, 'simple.target.population') || !is(other, 'target.population'))
                get.dimension.values.overlap(private$i.dimension.values, other.dimension.values)
            else
                other$get.overlapping.dimension.values(self)
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
        i.invert = NULL,
        
        do.render.population.mask = function(specification.metadata, dim.names, error.prefix)
        {   
            if (length(dim.names)==0)
            {
                !private$i.invert
            }
            else
            {
                rv = array(private$i.invert,
                           dim = sapply(dim.names, length),
                           dimnames = dim.names)
                
                # Access and set
                if (is.null(specification.metadata))
                    dimension.values = self$dimension.values
                else
                    dimension.values = specification.metadata$apply.aliases(self$dimension.values)
                
                array.access(rv, dimension.values) = !private$i.invert
                
                # Return
                rv
            }
        }
    )
)

COMBINATION.TARGET.POPULATION = R6::R6Class(
    'combination.target.population',
    inherit = TARGET.POPULATION,
    
    public = list(
        initialize = function(subpopulations, 
                              combine.function, 
                              name)
        { 
            # Check argument classes
            error.prefix = paste0("Cannot create ", self$descriptor, ": ")
            if (any(!sapply(subpopulations, is, 'target.population')))
                stop(paste0(error.prefix, "subpopulations must all be objects of class 'target.population'"))
            
            # Call the super-constructor
            dimension.values = outer.join.dim.names(lapply(subpopulations, function(subpop){
                subpop$dimension.values
            }))
            
            super$initialize(dimension.values = dimension.values, name = name, allow.null.name = T)
            if (!is.null(name))
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
        },
        
        get.name = function(bracket.depth=0, bracket.order=c("()","[]","{}"))
        {
            if (is.null(private$i.name))
            {
                sub.names = sapply(private$i.subpopulations, function(sub){
                    sub$get.name(bracket.depth = bracket.depth+1, bracket.order = bracket.order)
                })
                
                if (private$i.combine.function == 'union')
                    rv = collapse.with.and(sub.names)
                else if (private$i.combine.function == 'intersect')
                    rv = paste0(sub.names[1], " who are also ", collapse.with.and(sub.names[-1]))
                else if (private$i.combine.function == 'diff')
                    rv = paste0(sub.names[1], " except for ", sub.names[2])
                else
                    stop(paste0(error.prefix, "Invalid combine.function '", private$i.combine.function,
                                "' was given to the target.population '", self$name, "'"))
                
                if (bracket.depth>0)
                {
                    if (bracket.depth>length(bracket.order))
                        brackets = bracket.order[length(bracket.order)]
                    else
                        brackets = bracket.order[bracket.depth]
                    
                    open.bracket = substr(brackets, 1,1)
                    close.bracket = substr(brackets, 2,2)
                    
                    rv = paste0(open.bracket, rv, close.bracket)
                }
                
                rv
            }
            else
                private$i.name
        },
        
        get.overlapping.dimension.values = function(other)
        {
            dim.names
            
            if (is(other, 'target.population'))
                other.dimension.values = other$dimension.values
            else if (is.ontology(other))
                other.dimension.values = other
            else if (is.list(other))
            {
                check.dimension.values.valid(other,
                                             variable.name.for.error = 'other',
                                             allow.empty = T,
                                             error.prefix = "To calculate the overlap between target.populations, if 'other' is a list, ")
            }
            else
                stop("To calculate the overlap between target.populations, 'other' must be an object of class 'target.population' or a valid dimension.values list")
            
            other.dimensions = names(other.dimension.values)
            if (length(intersect(self$dimensions, other.dimensions)) == 0)
                return (NULL)
            
            if (private$i.combine.function == 'union') # the intersection with the union is the union of the intersections
            {
                overlaps.with.subpops = lapply(private$i.subpopulations, function(subpop){
                    subpop$get.overlapping.dimension.values(other)
                })
                
                non.null.overlaps = overlaps.with.subpops[!sapply(overlaps.with.subpops, is.null)]
                if (length(non.null.overlaps)==0)
                    NULL
                else
                {
                    rv = non.null.overlaps[[1]]
                    for (other.overlap in non.null.overlaps[-1])
                    {
                        dimensions = union(names(rv), names(other.overlap))
                        rv = lapply(dimensions, function(d){
                            if (is.null(rv[[d]]))
                                other.overlap[[d]]
                            else if (is.null(other.overlap[[d]]))
                                rv[[d]]
                            else
                            {
                                val1 = rv[[d]]
                                val2 = other.overlap[[d]]
                                
                                if (is.character(val1) && is.character(val2))
                                    union(val1, val2)
                                else if (is.character(val1))
                                    val1
                                else if (is.character(val2))
                                    val2
                                # neither is a character
                                else if (is.logical(val1) && is.logical(val2))
                                {
                                    if (length(val1)==length(val2))
                                        val1 | val2
                                    else if (any(val1))
                                        val1
                                    else
                                        val2
                                }
                                else #at least one is an integer
                                {
                                    if (is.logical(val1))
                                        val1 = (1:length(val1))[val1]
                                    if (is.logical(val2))
                                        val2 = (1:length(val2))[val2]
                                    
                                    union(val1, val2)
                                }
                            }
                        })
                        names(rv) = dimensions
                    }
                    
                    rv
                }
            }
            else if (private$i.combine.function == 'intersect')
            {
                rv = private$i.subpopulations[[1]]$get.overlapping.dimension.values(other)
                for (subpop in private$i.subpopulations[-1])
                {
                    sub.rv = subpop$get.overlapping.dimension.values(other)
                    rv = get.dimension.values.overlap(rv, sub.rv)
                }
                
                rv
            }
            else if (private$i.combine.function == 'diff')
            {
                overlap.with.first = private$i.subpopulations[[1]]$get.overlapping.dimension.values(other)
                if (is.null(overlap.with.first))
                    NULL
                else
                {
                    overlap.with.second = private$i.subpopulations[[2]]$get.overlapping.dimension.values(other)
                    if (is.null(overlap.with.second))
                        overlap.with.first
                    else
                    {
                        overlap.of.the.overlaps = get.dimension.values.overlap(overlap.with.first, overlap.with.second)
                        if (is.null(overlap.of.the.overlaps))
                            overlap.with.first
                        else
                            NULL # we just can't say what the overlap is
                    }
                }
            }
            else
                stop(paste0(error.prefix, "Invalid combine.function '", private$i.combine.function,
                            "' was given to the target.population '", self$name, "'"))
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
        i.combine.function = NULL,
        
        do.render.population.mask = function(specification.metadata, dim.names, error.prefix)
        {
            mask = private$i.subpopulations[[1]]$render.population.mask(specification.metadata = specification.metadata,
                                                                        dim.names = dim.names, 
                                                                        render.only.relevant.dimensions = F,
                                                                        error.prefix=error.prefix)
            
            for (subpop in private$i.subpopulations[-1])
            {
                mask2 = subpop$render.population.mask(specification.metadata = specification.metadata,
                                                      dim.names=dim.names, 
                                                      render.only.relevant.dimensions = F,
                                                      error.prefix=error.prefix)
                
                if (private$i.combine.function == 'union')
                    mask = mask | mask2
                else if (private$i.combine.function == 'intersect')
                    mask = mask & mask2
                else if (private$i.combine.function == 'diff')
                    mask = mask & !mask2
                else
                    stop(paste0(error.prefix, "Invalid combine.function '", private$i.combine.function,
                                "' was given to the target.population '", self$name, "'"))
            }
            
            mask
        }
    )
)