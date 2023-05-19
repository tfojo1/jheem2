

## ***OVERVIEW***
##
## An *ontology* outlines the format we expect some data to take
## We define it as a named list of character vectors, where the names of list elements represent dimensions
##      and the values of the character vectors which are the elements are the values for each dimensions
## PLUS a boolean indicator for whether each dimension is "complete" or not (stored in the attributes of the list)
## Essentially, an ontology is a subset of the lists that can be assigned to the dimnames attribute for an array, 
##      with a few additional constraints:
##      (1) The dimensions must be named. These names can be used only once
##      (2) Values for a dimension cannot repeat or be NA
##      (3) We store a boolean indicator for completeness
## 
## A "complete" dimension is one for which the values represent all possible values for that dimension.
## This has a few implications:
##      (1) For a complete dimension, we can marginalize it out by summing across it. If you sum the values from
##          all values of a complete dimension, that is all the values possible.
##          We CANNOT marginalize like this for an incomplete dimension
##      (2) Complete dimensions cannot have additional values added to them. But incomplete dimensions can
##      (3) It is possible to have an empty (NULL) set of values for an incomplete dimension.
##          We are essentially saying, "We know there are values here, but we don't know what they are yet"
##          
## Ontologies are implemented as S3 objects, so that they can be used as a dimnames attribute


#'@title Create an ontology object
#'
#'@details An ontology is a set of dimnames with specific characteristics: (1) the dimnames must be named, (2) some of the dimensions may be noted as "incomplete". An incomplete dimension is one for which the given values for a dimension may not represent all possible values for that dimension
#'
#'@param ... One or more character vectors which must be NAMED
#'@param incomplete.dimensions A character vector indicating which dimensions are incomplete. Must be a subset of the names of ...
#'
#'@export
ontology <- function(..., incomplete.dimensions=NULL)
{
    rv = list(...)
    
    #-- Validate arguments --#
    if (length(rv)==0)
        names(rv) = character()
    
    #-- Validate dimension names --#
    if (is.null(names(rv)))
        stop("The arguments passed to ontology() must be named")
    
    if (any(is.na(names(rv))))
        stop("The arguments passed to ontology() cannot have NA names")
    
    if (any(nchar(names(rv))==0))
        stop("The arguments passed to ontology() cannot have empty names")
    
    tabled.names = table(names(rv))
    if (any(tabled.names>1))
        stop(paste0("Names of arguments to ontology() cannot be repeated, (",
                    paste0("'", names(tabled.names)[tabled.names>1], "'", collapse=', '),
                    ")"))
    
    dimensions = names(rv)
    
    #-- Validate dimension values --#
    rv = lapply(names(rv), function(d){
        if (is.numeric(rv[[d]]))
            values = as.character(rv[[d]])
        else
            values = rv[[d]]
        
        if (!is.null(values))
        {
            if (!is.character(values))
                stop("The values passed to ontology() must be either NULL or character vectors")
            
            if (any(is.na(values)))
                stop("The values passed to ontology() cannot contain NA")
            
            if (any(nchar(values)==0))
                stop("The values passed to ontology() cannot be empty ('')")
            
            tabled.values = table(values)
            if (any(tabled.values>1))
                stop(paste0("The arguments to ontology() cannot contain repeated values. Dimension '",
                            d, "' contains multiple instances of ",
                            paste0("'", names(tabled.values)[tabled.valued>1], "'", collapse=', '),
                            ))
        }
        
        values
    })
    names(rv) = dimensions

    #-- Set up is.complete --#
    if (is.null(incomplete.dimensions))
        is.complete = !sapply(rv, is.null)
    else
    {
        if (!is.character(incomplete.dimensions))
            stop("'incomplete.dimensions' passed to ontology() must be a character vector")
        
        if (any(is.na(incomplete.dimensions)))
            stop("'incomplete.dimensions' cannot contain NA values")
        
        extraneous.dimensions = setdiff(incomplete.dimensions, names(rv))
        if (length(extraneous.dimensions)>0)
            stop(paste0("'incomplete.dimensions' passed to ontology() must contain a subset of the names of the other arguments to ontology(). ",
                        paste0("'", extraneous.dimensions, "'", collapse=', '),
                        " were specified as incomplete.dimensions but are not names of dimensions"))
        
        is.complete = sapply(names(rv), function(d){all(d!=incomplete.dimensions)})
    }
    
    null.complete.dimension.mask = sapply(rv, is.null) & is.complete
    if (any(null.complete.dimension.mask))
        stop(paste0("The values of a dimension in an ontology can only be NULL if the dimension is incomplete. NULL was passed for ",
                    ifelse(sum(null.complete.dimension.mask)==1, "dimension ", "dimensions "),
                    collapse.with.and("'", dimensions[null.complete.dimension.mask], "'"),
                    ", ",
                    ifelse(sum(null.complete.dimension.mask)==1, "which was", "which were"),
                    " not specified as incomplete"))
    
    names(is.complete) = names(rv)
    attr(rv, 'is.complete') = is.complete

    class(rv) = c('ontology', 'list')
    rv
}

#'@title Print an ontology
#'
#'@param ontology An ontology, as created by \code\link{ontology}}
#'
#'@export
print.ontology <- function(ontology)
{
    to.print = lapply(ontology, function(val){val})
    complete.text = rep('incomplete', length(ontology))
    complete.text[attr(ontology, 'is.complete')] = 'complete'
    names(to.print) = paste0(names(ontology), " (", complete.text, ")")
    
    print(to.print)
}

#'@title Convert a named list to an ontology
#'
#'@param x A named list of character vectors
#'
#'@export
as.ontology <- function(x, incomplete.dimensions=NULL)
{
    if (is(x, 'ontology'))
        return (x)
    if (!is.list(x))
        stop("Can only convert named lists to ontologies with as.ontology()")
    if (length(x)>0 && is.null(names(x)))
        stop("Can only convert NAMED lists to ontologies with as.ontology()")
    
    if (any(!sapply(x, is.character)))
        stop("Lists to be converted to ontologies using as.ontology() must contain only character vectors")
    
    do.call(ontology, args = c(x, list(incomplete.dimensions=incomplete.dimensions)))
}

#'@title Convert an ontology back to a plain list
#'
#'@param ont An ontology as created by \code{\link{ontology}}
#'
#'@export
as.list.ontology <- function(ont)
{
    rv = lapply(1:length(ont), function(i){
        ont[[i]]
    })
    names(rv) = names(ont)
    
    rv
}

#'@title Test if an object is of type 'ontology'
#'
#'@param x Object to be tested
#'
#'@return A single logical T/F value
#'
#'@export
is.ontology <- function(x)
{
    is(x, 'ontology')
}

#'@title Get indicators of which dimensions in an ontology are complete
#'
#'@param ontology An ontology, as created by \code\link{ontology}}
#'
#'@return A named logical vector - the corresponding to names(ontology) - indicating whether each dimension is complete
#'
#'@export
is_complete.ontology <- function(x)
{
    attr(x, 'is.complete')
}

#'@title Get indicators of which dimensions in an ontology are complete
#'
#'@param ontology An ontology, as created by \code\link{ontology}}
#'
#'@return A named logical vector - the corresponding to names(ontology) - indicating whether each dimension is complete
#'
#'@export
is_complete <- function(x)
{
    UseMethod('is_complete')
}

#'@title Subset an ont
#'
#'@param ont An ontology, as created by \code\link{ontology}}
#'@param i What to subset
#'
#'@return An ontology
#'
#'@export
'[.ontology' <- function(ont, i)
{
    rv = NextMethod()
    
    invalid.mask = is.na(names(rv))
    if (any(invalid.mask))
        stop(paste0("Error subsetting ontology: ",
                    paste0("'", i[invalid.mask], "'", collapse=', '),
                    ifelse(sum(invalid.mask)==1,
                           " does not reference a valid dimension",
                           " do not reference valid dimensions")))
    
    attr(rv, 'is.complete') = attr(ont, 'is.complete')[names(rv)]
    class(rv) = c('ontology','list')
    
    rv
}

#'@title Modify an ontology
#'
#'@param ont An ontology, as created by \code\link{ontology}}
#'@param i What to subset
#'@param value New values to insert
#'
#'@return An ontology
#'
#'@export
'[<-.ontology' <- function(ont, i, value)
{
    if (is.numeric(value))
        value = as.character(value)
    else if (is.list(value))
        value = lapply(value, function(val){
            if (is.numeric(val))
                as.character(val)
            else
                val
        })
    rv = NextMethod()
    
    # Cannot add dimensions
    if (length(rv)>length(ont))
    {
        if (any(is.na(i)))
            stop("Cannot have NA indices into an ontology")
        else
            stop(paste0("Cannot add dimensions to an existing ontology"))
    }
    
    # New values must:
    # (1) be character vectors
    # (2) have at least one element
    # (3) not contain NA
    # (4) not have repeated values
    sapply(names(ont[i]), function(d){
        if (!is.character(rv[[d]]))
            stop(paste0("Elements of the ontology must be character vectors (attempted to set a non-character value for dimension '", d, "')"))
        if (length(rv[[d]])==0)
            stop(paste0("Elements of the ontology must cannot be empty vectors (attempted to set an emtpy vector for dimension '", d, "')"))
        if (any(is.na(rv[[d]])))
            stop(paste0("Elements of the ontology cannot contain NA values (attempted to set NA values for dimension '", d, "')"))
        tabled.values = table(rv[[d]])
        if (any(tabled.values>1))
            stop(paste0("Elements of the ontology cannot contain repeated values (attempted to use ",
                        ifelse(sum(tabled.values)==1, "value ", "values "),
                        paste0("'", names(tabled.values[tabled.values>1]), "'", collapse=', '),
                        " more than once)"
                        ))
    })
    
    # Cannot add to complete dimensions
    # Subsetted complete dimensions are incomplete
    # Incomplete dimensions stay incomplete
    old.is.complete = attr(ont, 'is.complete')
    new.is.complete = sapply(names(ont), function(d){
        if (!old.is.complete[d])
            F
        else
        {
            if (setequal(ont[[d]], rv[[d]]))
                T
            else if (length(setdiff(rv[[d]], ont[[d]]))>0)
                stop(paste0("Cannot add values to complete dimension '", d, "' in the ontology"))
            else
                F
        }
    })

    attr(rv, 'is.complete') = new.is.complete
    
    class(rv) = c('ontology', 'list')
    rv
}

#'@title Modify an ontology
#'
#'@param ont An ontology, as created by \code\link{ontology}}
#'@param i What to subset
#'@param value New value to insert
#'
#'@return An ontology
#'
#'@export
'[[<-.ontology' <- function(ont, i, value)
{
    ont[i] = list(value)
    ont
}

#'@title Modify an ontology
#'
#'@param ont An ontology, as created by \code\link{ontology}}
#'@param i What to subset
#'@param value New value to insert
#'
#'@return An ontology
#'
#'@export
'$<-.ontology' <- function(ont, i, value)
{
    ont[[i]] = value
    ont
}

#'@title Resolve dimension.values into a set of dimnames for an ontology
#'
#'@param ont An ontology, as created by \code\link{ontology}}
#'@param dimension.values A list of dimension.values (ie, a named list of either character, integer, or logical vectors that indexes into the dimnames given in ont)
#'@param error.prefix Text to prepend to any error messages that are thrown
#'@param throw.error.if.unresolvable A logical indicating what to do if unable to resolve against the ontology. When FALSE, will return NULL if unresolvable
#'
#'@return If the dimension.values can be resolved against ontology ont, returns a list of dimnames (ie, a named list of character vectors). If cannot resolve will either throw an error (if throw.error.if.unresolvable) or return NULL (NB, if passed dimension.values==NULL, will also return NULL)
#'
#'@export
resolve.ontology.dimension.values <- function(ont, dimension.values, error.prefix, throw.error.if.unresolvable=T)
{
    if (!is(ont, 'ontology'))
        stop(paste0(error.prefix, "'ont' must be an object of class 'ontology'"))
    
    if (is.null(dimension.values))
        return (NULL)
    if (!is.list(dimension.values))
        stop(paste0(error.prefix, "'dimension.values' must be a named list"))
    if (length(dimension.values)==0)
        return (dimension.values)
    if (is.null(names(dimension.values)))
        stop(paste0(error.prefix, "'dimension.values' must be a NAMED list"))
    
    ontology.dimensions.complete = is_complete(ont)
    
    rv = lapply(names(dimension.values), function(d){
        if (!ontology.dimensions.complete[d])
        {
            if (is.character(dimension.values[[d]]))
                dimension.values[[d]]
            else
                stop(paste0(error.prefix, "values for dimension '", d, "', which is incomplete, must be a character vector"))
        }
        else
        {
            if (is.character(dimension.values[[d]]))
            {
                invalid.values = setdiff(dimension.values[[d]], ont[[d]])
                if (length(invalid.values)>0)
                {
                    if (throw.error.if.unresolvable)
                        stop(paste0(error.prefix, "Invalid ",
                                    ifelse(length(invalid.values)==1, 'value', 'values'),
                                    " for dimension '", d, "': ",
                                    collapse.with.and("'", invalid.values, "'")))
                    else
                        return (NULL)
                }
                
                dimension.values[[d]]
            }
            else if (is.numeric(dimension.values[[d]]))
            {
                if (any(dimension.values[[d]]<1) || any(dimension.values[[d]]>length(ont[[d]])))
                {
                    if (throw.error.if.unresolvable)
                        stop(paste0(error.prefix, "If it is a numeric vector, values for dimension '", d, "' must contain values between 1 and ", length(ont[[d]])))
                    else
                        return (NULL)
                }
                
                ont[[d]][ dimension.values[[d]] ]
            }
            else if (is.logical(dimension.values[[d]]))
            {
                if (length(dimension.values[[d]]) != length(ont[[d]]))
                {
                    if (throw.error.if.unresolvable)
                        stop(paste0(error.prefix, "If it is a logical vector, values for dimension '", d, "' must be of length ", length(ont[[d]])))
                    else
                        return (NULL)
                }
                
                if (!any(dimension.values[[d]]))
                    stop(paste0(error.prefix, "If it is a logical vector, values for dimension '", d, "' must be contain at least one TRUE value"))
                
                ont[[d]][ dimension.values[[d]] ]
            }
            else
                stop(paste0(error.prefix, "values for dimension '", d, "', which is incomplete, must be either a character, numeric, or logical vector"))
        }
    })
    
    names(rv) = names(dimension.values)
    rv
}
