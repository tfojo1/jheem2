
# Tests
if (1==2)
{
    ont = ontology(year=as.character(2000:2020),
                   age=c('young','old'), race=c('black','other'),
                   incomplete.dimensions='year')
    
    ont2 = ont
    ont2['year'] = list(2000:2030)
    ont2['age'] = list('young')
    ont2['race'] = list(c('black','hispanic','other')) #should not work
    
    ont2 = ont
    ont2[['year']] = 2000:2030
    ont2[['race']] = 'hispanic'
    ont2$race = 'hispanic'
    ont2$sex = 'male'
    ont2[c('year','sex')] = list(2000:2020, c('male','female'))
    
    arr = array(0, dim=sapply(ont, length), dimnames=ont)
    
    class(dimnames(arr))
    
    arr2 = arr[,1,drop=F]
    class(dimnames(arr2))
}

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
    rv = sapply(names(rv), function(d){
        if (is.numeric(rv[[d]]))
            values = as.character(rv[[d]])
        else
            values = rv[[d]]
        
        if (!is.character(values))
            stop("The values passed to ontology() must all be *character* or *numeric* vectors")
        
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
    
    names(is.complete) = names(rv)
    attr(rv, 'is.complete') = is.complete

    class(rv) = c('ontology', 'list')
    rv
}

print.ontology <- function(ontology)
{
    to.print = lapply(ontology, function(val){val})
    complete.text = rep('incomplete', length(ontology))
    complete.text[attr(ontology, 'is.complete')] = 'complete'
    names(to.print) = paste0(names(ontology), " (", complete.text, ")")
    
    print(to.print)
}

is_complete.ontology <- function(ontology)
{
    attr(ontology, 'is.complete')
}

'[.ontology' <- function(ontology, i)
{
    rv = NextMethod()
    
    invalid.mask = is.na(names(rv))
    if (any(invalid.mask))
        stop(paste0("Error subsetting ontology: ",
                    paste0("'", i[invalid.mask], "'", collapse=', '),
                    ifelse(sum(invalid.mask)==1,
                           " does not reference a valid dimension",
                           " do not reference valid dimensions")))
    
    attr(rv, 'is.complete') = attr(ontology, 'is.complete')[names(rv)]
    class(rv) = c('ontology','list')
    
    rv
}

'[<-.ontology' <- function(ontology, i, value)
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
    if (length(rv)>length(ontology))
    {
        if (any(is.na(i)))
            stop("Cannot have NA indices into ontology")
        else
            stop(paste0("Cannot add dimensions to existing ontology"))
    }
    
    # New values must:
    # (1) be character vectors
    # (2) have at least one element
    # (3) not contain NA
    # (4) not have repeated values
    sapply(names(ontology[i]), function(d){
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
    old.is.complete = attr(ontology, 'is.complete')
    new.is.complete = sapply(names(ontology), function(d){
        if (!old.is.complete[d])
            F
        else
        {
            if (setequal(ontology[[d]], rv[[d]]))
                T
            else if (length(setdiff(rv[[d]], ontology[[d]]))>0)
                stop(paste0("Cannot add values to complete dimension '", d, "' in the ontology"))
            else
                F
        }
    })

    attr(rv, 'is.complete') = new.is.complete
    
    class(rv) = c('ontology', 'list')
    rv
}

'[[<-.ontology' <- function(ontology, i, value)
{
    if (is.numeric(value))
        value = as.character(value)
    
    rv = NextMethod()
    
    # New values must:
    # (1) be character vectors
    # (2) have at least one element
    # (3) not contain NA
    # (4) not have repeated values
    d = names(ontology[i])
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
    
    # Cannot add to complete dimensions
    # Subsetted complete dimensions are incomplete
    # Incomplete dimensions stay incomplete
    new.is.complete = attr(ontology, 'is.complete')
    if (attr(ontology, 'is.complete')[d])
    {
        if (setequal(ontology[[d]], rv[[d]]))
        {}
        else if (length(setdiff(rv[[d]], ontology[[d]]))>0)
            stop(paste0("Cannot add values to complete dimension '", d, "' in the ontology"))
        else
            new.in.complete[d] = F
    }

    attr(rv, 'is.complete') = new.is.complete
    
    class(rv) = c('ontology', 'list')
    rv
}

'$<-.ontology' <- function(ontology, i, value)
{
    if (is.numeric(value))
        value = as.character(value)
    
    rv = NextMethod()
    
    # New values must:
    # (1) be character vectors
    # (2) have at least one element
    # (3) not contain NA
    # (4) not have repeated values
    d = names(ontology[i])
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
    
    # Cannot add to complete dimensions
    # Subsetted complete dimensions are incomplete
    # Incomplete dimensions stay incomplete
    new.is.complete = attr(ontology, 'is.complete')
    if (attr(ontology, 'is.complete')[d])
    {
        if (setequal(ontology[[d]], rv[[d]]))
        {}
        else if (length(setdiff(rv[[d]], ontology[[d]]))>0)
            stop(paste0("Cannot add values to complete dimension '", d, "' in the ontology"))
        else
            new.in.complete[d] = F
    }
    
    attr(rv, 'is.complete') = new.is.complete
    
    class(rv) = c('ontology', 'list')
    rv
}
