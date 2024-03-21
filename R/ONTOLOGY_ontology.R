

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

        #   extraneous.dimensions = setdiff(incomplete.dimensions, names(rv))
        #   if (length(extraneous.dimensions)>0)
        #       stop(paste0("'incomplete.dimensions' passed to ontology() must contain a subset of the names of the other arguments to ontology(). ",
        #                   paste0("'", extraneous.dimensions, "'", collapse=', '),
        #                   " were specified as incomplete.dimensions but are not names of dimensions"))

        is.complete = sapply(names(rv), function(d){all(d!=incomplete.dimensions)})
    }

    if (length(rv)>0)
    {
        null.complete.dimension.mask = sapply(rv, is.null) & is.complete
        if (any(null.complete.dimension.mask))
            stop(paste0("The values of a dimension in an ontology can only be NULL if the dimension is incomplete. NULL was passed for ",
                        ifelse(sum(null.complete.dimension.mask)==1, "dimension ", "dimensions "),
                        collapse.with.and("'", dimensions[null.complete.dimension.mask], "'"),
                        ", ",
                        ifelse(sum(null.complete.dimension.mask)==1, "which was", "which were"),
                        " not specified as incomplete"))
    }

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
print.ontology <- function(ont)
{
    if (length(ont)==0)
    {
        print("An empty ontology")
    }
    else
    {
        to.print = lapply(ont, function(val){val})
        complete.text = rep('incomplete', length(ont))
        complete.text[is.complete(ont)] = 'complete'
        names(to.print) = paste0(names(ont), " (", complete.text, ")")

        print(to.print)
    }
}

#'@title Convert a named list to an ontology
#'
#'@param x A named list of charactera vectors
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
    if (length(ont)==0)
        list()
    else
    {
        rv = lapply(1:length(ont), function(i){
            ont[[i]]
        })
        names(rv) = names(ont)

        rv
    }
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
    rv = attr(x, 'is.complete')
    if (length(rv) != length(x)) #this protects us against an old error that might still persist in saved ontologies
    {
        incomplete.dimensions = names(rv)[as.logical(rv)]
        new.is.complete = sapply(names(x), function(d){
            all(incomplete.dimensions != d)
        })
        names(new.is.complete) = names(x)
        new.is.complete
    }
    else
        rv
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

#'@rdname is_complete
is.complete <- function(x)
{
    UseMethod('is_complete')
}

#'@title Get an ontology's incomplete dimensions
#'
#'@param ontology An ontology, as created by \code\link{ontology}}
#'
#'@return A vector containing the names of the ontology's incomplete dimensions
#'
#'@export
incomplete.dimensions.ontology <- function(x)
{
    names(x)[!is.complete(x)]
}

#'@title Get an ontology's incomplete dimensions
#'
#'@param ontology An ontology, as created by \code\link{ontology}}
#'
#'@return A vector containing the names of the ontology's incomplete dimensions
#'
#'@export
incomplete.dimensions <- function(x)
{
    UseMethod('incomplete.dimensions')
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

    attr(rv, 'is.complete') = is.complete(ont)[names(rv)]
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
    if (is.null(i)) #if for some reason this is an overwrite of nothing
        return (ont)

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
    old.is.complete = is.complete(ont)
    new.is.complete = sapply(names(ont), function(d){
        if (is.null(ont[[d]]))
            F
        else if (!old.is.complete[d])
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
    rv = NextMethod()

    # Cannot add to complete dimensions
    # Subsetted complete dimensions are incomplete
    # Incomplete dimensions stay incomplete
    old.is.complete = is.complete(ont)
    new.is.complete = sapply(names(ont), function(d){
        if (is.null(ont[[d]]))
            F
        else if (!old.is.complete[d])
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
'$<-.ontology' <- function(ont, i, value)
{
    rv = NextMethod()

    # Cannot add to complete dimensions
    # Subsetted complete dimensions are incomplete
    # Incomplete dimensions stay incomplete
    old.is.complete = is.complete(ont)
    new.is.complete = sapply(names(ont), function(d){
        if (is.null(ont[[d]]))
            F
        else if (!old.is.complete[d])
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

    rv
}

#'@title Modify an Ontology's Names
#'
#'@export
'names<-.ontology' <- function(ont, value)
{
    if (length(value) != length(ont))
        stop("In setting the names of the ontology, every dimension must have a name (the length of the names must be the same as the length of the ontology and cannot be NULL)")
    rv = NextMethod()

    #-- Validate dimension names --#

    if (length(names(rv))==0 && length(ont)>0)
        stop("The names of an ontology cannot be set to empty")

    if (any(is.na(names(rv))))
        stop("The names of an ontology cannot be NA")

    if (any(nchar(names(rv))==0))
        stop("The names of an ontology cannot be empty")

    tabled.names = table(names(rv))
    if (any(tabled.names>1))
        stop(paste0("The names of an ontology cannot be repeated, (",
                    paste0("'", names(tabled.names)[tabled.names>1], "'", collapse=', '),
                    ")"))

    new.is.complete = is.complete(rv)
    names(new.is.complete) = names(rv)
    attr(rv, 'is.complete') = new.is.complete

    rv
}

c.ontology <- function(...)
{
    args = list(...)
    if (all(sapply(args, is, 'ontology')))
    {
        list.args = as.list(args[[1]])
        incomplete.dims = incomplete.dimensions(args[[1]])
        for (to.add in args[-1])
        {
            overlapping.dimensions = intersect(names(to.add), names(list.args))
            if (length(overlapping.dimensions)>0)
                stop(paste0("Cannot concatenate ontologies: the ",
                            ifelse(length(overlapping.dimensions)==1, 'dimension ', 'dimensions '),
                            collapse.with.and("'", overlapping.dimensions, "'"),

                            ifelse(length(overlapping.dimensions)==1, ' appears', ' appear'),
                            " in more than one of the arguments to c()"))

            list.args = c(list.args, to.add)
            incomplete.dims = c(incomplete.dims, incomplete.dimensions(to.add))
        }

        do.call(ontology, args=c(list.args, list(incomplete.dimensions=incomplete.dims)))
    }
    else
    {
        NextMethod()
    }
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


    resolve.dimension.values.against.dim.names(dimension.values = dimension.values,
                                               dim.names = ont,
                                               error.prefix = error.prefix,
                                               throw.error.if.unresolvable = throw.error.if.unresolvable,
                                               dim.names.name.for.error='ont')
}

resolve.dimension.values.against.dim.names <- function(dimension.values, dim.names, error.prefix, throw.error.if.unresolvable=T,
                                                       dim.names.name.for.error='dim.names')
{
    if (is.null(dimension.values))
        return (NULL)
    if (!is.list(dimension.values))
        stop(paste0(error.prefix, "'dimension.values' must be a named list"))
    if (length(dimension.values)==0)
        return (dimension.values)
    if (is.null(names(dimension.values)))
        stop(paste0(error.prefix, "'dimension.values' must be a NAMED list"))

    if (!is.ontology(dim.names))
        check.dim.names.valid(dim.names,
                              variable.name.for.error = 'dim.names',
                              allow.duplicate.values.across.dimensions = T,
                              error.prefix = error.prefix)

    missing.dimensions = setdiff(names(dimension.values), names(dim.names))
    if (length(missing.dimensions) > 0)
    {
        if (throw.error.if.unresolvable)
            stop(paste0(error.prefix, "'", dim.names.name.for.error, "' is missing ",
                        ifelse(length(missing.dimensions)==1, "dimension ", "dimensions "),
                        collapse.with.and("'", missing.dimensions, "'"),
                        ", which ",
                        ifelse(length(missing.dimensions)==1, "is", 'are'),
                        " present in 'dimension.values'"))
        else
            return (NULL)
    }

    if (is.ontology(dim.names))
        ontology.dimensions.complete = is_complete(dim.names)
    else
    {
        ontology.dimensions.complete = rep(T, length(dim.names))
        names(ontology.dimensions.complete) = names(dim.names)
    }

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
                invalid.values = setdiff(dimension.values[[d]], dim.names[[d]])
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
                if (any(dimension.values[[d]]<1) || any(dimension.values[[d]]>length(dim.names[[d]])))
                {
                    if (throw.error.if.unresolvable)
                        stop(paste0(error.prefix, "If it is a numeric vector, values for dimension '", d, "' must contain values between 1 and ", length(dim.names[[d]])))
                    else
                        return (NULL)
                }

                dim.names[[d]][ dimension.values[[d]] ]
            }
            else if (is.logical(dimension.values[[d]]))
            {
                if (length(dimension.values[[d]]) != length(dim.names[[d]]))
                {
                    if (throw.error.if.unresolvable)
                        stop(paste0(error.prefix, "If it is a logical vector, values for dimension '", d, "' must be of length ", length(dim.names[[d]])))
                    else
                        return (NULL)
                }

                if (!any(dimension.values[[d]]))
                    stop(paste0(error.prefix, "If it is a logical vector, values for dimension '", d, "' must be contain at least one TRUE value"))

                dim.names[[d]][ dimension.values[[d]] ]
            }
            else
                stop(paste0(error.prefix, "values for dimension '", d, "', which is incomplete, must be either a character, numeric, or logical vector"))
        }
    })

    names(rv) = names(dimension.values)
    rv
}
