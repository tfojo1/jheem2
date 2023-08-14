
##----------------------------##
##-- ARRAY ACCESS/OVERWRITE --##
##----------------------------##

#'Flexibly Subset an Array
#'
#'@title Functions for subsetting an array when the number and order of dimensions is not known at the point of coding. The array.access function performs error checking on the arguments; fast.array.access is optimized for speed but does not check arguments
#'
#'@param arr The array or matrix to be subsetted. Must have named dimensions and named dimnames set
#'@param dimension.values A named list containing the values of dimensions to subset for. The names of dimension.values should correspond to the names of dimnames. The elements of dimension.values should be character, integer, or logical vectors
#'@param drop Pass-through for the drop argument for array access. If set to true, any dimensions in the return value that have length one are omitted (as with standard subsetting). If false, then the dimensions are retained in the array
#'
#'@examples
#'arr = array(1:24, dim=c(age=4, race=3, sex=2), 
#'                  dimnames=list(age=c('age1','age2','age3','age4'), 
#'                                race=c('black','hispanic','other'), 
#'                                sex=c('female','male')))
#'                                
#'array.access(arr, dimension.values=list(age=1:3, race='black'))
#'#is equivalent to
#'arr[1:3, 'black',]
#'
#'@export
array.access <- function(arr,
                         ...,
                         drop=F)
{
    # Process ... into a list of dimension values
    args = list(...)
    
    error.prefix = 'Error in array.access(): '
    dimension.values = list()
    for (elem.index in 1:length(args))
    {
        elem = args[[elem.index]]
        if (is.list(elem))
        {
            if (is.null(names(elem)) || any(is.na(names(elem))) || any(nchar(names(elem))==0))
                stop(paste0(error.prefix, "Elements of ... which are lists must be NAMED"))
            
            invalid.type.mask = !sapply(elem, is.null) & !sapply(elem, is.numeric) & !sapply(elem, is.character) & !sapply(elem, is.logical)
            if (any(invalid.type.mask))
                stop(paste0(error.prefix, "The elements of ... which are lists must contain only numeric, character, or logical vectors. ",
                            ifelse(sum(invalid.type.mask)==1, "The value for dimension ", "Values for dimensions "),
                            collapse.with.and("'", names(elem)[invalid.type.mask], "'"),
                            ifelse(sum(invalid.type.mask)==1, " is", " are"),
                            " none of these."))
            
            dimension.values = c(dimension.values, elem)
        }
        else
        {
            elem.name = names(args)[elem.index]
            if (is.null(elem.name) || is.na(elem.name) || nchar(elem.name)==0)
                stop(paste0(error.prefix, "Elements of ... which are not lists must have a NAME"))
            
            if (!is.numeric(elem) && !is.character(elem) && !is.logical(elem))
                stop(paste0(error.prefix, "The elements of ... which are not lists must be either numeric, character, or logical vectors. ",
                            "The value for dimension '", names(args)[elem.index], "' is none of these."))
            
            dimension.values = c(dimension.values, args[elem.index])
        }
    }
    
    # Check arguments
    #  @Andrew MODIFIED by removing drop=drop from args since 5/3/23
    check.array.access.arguments(arr=arr, dimension.values=dimension.values)
 
    # Check drop
    if (!is.logical(drop) || length(drop) != 1 || is.na(drop))
        stop("'drop' must be a single, non-NA logical value")
    
    # Do it
    fast.array.access(arr=arr, 
                      dimension.values=dimension.values,
                      drop = drop)    
}


#'@describeIn array.access
#'
#'@export
fast.array.access <- function(arr,
                              dimension.values,
                              drop=F)
{
    if (length(dimension.values)==0)
        return (arr)
    
    subset.values = lapply(names(dim(arr)), function(d){
        if (is.null(dimension.values[[d]]))
            1:dim(arr)[d]
        else
            dimension.values[[d]]
    })

    arr = do.call('[', args=c(list(arr), subset.values, drop=drop))
    
    arr
}

get.array.access.indices <- function(arr.dim.names,
                                     dimension.values)
{
    arr = array(1:prod(sapply(arr.dim.names, length)),
                dim=sapply(arr.dim.names, length),
                dimnames=arr.dim.names)
    
    as.integer(fast.array.access(arr, dimension.values))
}

#'@export
'array.access<-' = function(arr,
                            dimension.values,
                            value)
{
    if (is.numeric(arr))
    {
        rv = do_array_overwrite(dst_array=arr,
                                src_array=value,
                                dimension_values=dimension.values)
        if (is.null(rv))
        {
            check.array.access.arguments(arr=arr,
                                         dimension.values=dimension.values,
                                         to.write=value)
            
            stop("There was an error overwriting 'arr'")
        }
        
        rv
    }
    else
    {
        if (!is.array(arr)) #!is.numeric(arr) || 
            stop("'arr' must be an array or matrix")
        
        if (is.null(dimnames(arr)))
            stop("'arr' must have named dimnames")
        
        if (is.null(names(dimnames(arr))))
            stop("'arr' must have NAMED dimnames")
        
        indices = get.array.access.indices(arr.dim.names = dimnames(arr), dimension.values = dimension.values)
        
        if (length(value) != 1 && length(value) != length(indices))
            stop(paste0("Cannot overwrite into array: values has length ", length(values),
                        " but the dimension values refer to ", length(indices),
                        ifelse(length(indices)==1, " element", " elements"),
                        " in the array"))
        
        arr[indices] = value
        
        arr
    }
}

check.array.access.arguments <- function(arr,
                                         dimension.values,
                                         dimension.values.name="'dimension.values'",
                                         to.write=NULL)
{
    # Check arr
    if (!is.array(arr)) #!is.numeric(arr) || 
        stop("'arr' must be an array or matrix")
    
    if (is.null(dimnames(arr)))
        stop("'arr' must have named dimnames")
    
    if (is.null(names(dimnames(arr))))
        stop("'arr' must have NAMED dimnames")
    
    # Check dimension values
    if (!is.list(dimension.values))
        stop(paste0(dimension.values.name, " must be a named list"))
    
    if (is.null(names(dimension.values)))
        stop(paste0(dimension.values.name, " must be a NAMED list"))
    
    if (length(unique(names(dimension.values))) != length(dimension.values))
    {
        tabled.dimensions = table(names(dimension.values))
        repeated.dimensions = names(tabled.dimensions)[tabled.dimensions>1]
        stop(paste0("The names of dimensions in ", dimension.values.name, " must be unique.",
                    ifelse(length(repeated.dimensions)==1, "Dimension ", "Dimensions "),
                    collapse.with.and("'", repeated.dimensions, "'"),
                    ifelse(length(repeated.dimensions)==1, " appears", " appear"),
                    " more than once"))
    }
    
    # Check that all names of dimension values are valid
    invalid.dimension.names = setdiff(names(dimension.values), names(dimnames(arr)))
    if (length(invalid.dimension.names)>0)
        stop(paste0("The following ",
                    ifelse(length(invalid.dimension.names)==1, "is given as a name", "are given as names"),
                    " of ", dimension.values.name, " but ",
                    ifelse(length(invalid.dimension.names)==1, "is not the name of a dimension", "are not names of the dimensions"),
                    " of 'arr': ",
                    paste0("'", invalid.dimension.names, "'", collapse=', ')))
    
    # Check validity of dimension values
    character.dimension.values = sapply(names(dimension.values), function(d){
        if (is.null(dimension.values[[d]]))
        {}
        else if (is.numeric(dimension.values[[d]]))
        {
            if (any(dimension.values[[d]]<=0) || any(dimension.values[[d]]>dim(arr)[d]))
                stop(paste0("The values of dimension '", d,
                            "' in ", dimension.values.name, " must be between 0 and ",
                            dim(arr)[d]))
            
            dimnames(arr)[[d]][ dimension.values[[d]] ]
        }
        else if (is.character(dimension.values[[d]]))
        {
            invalid.values = setdiff(dimension.values[[d]], dimnames(arr)[[d]])
            if (length(invalid.values)>0)
                stop(paste0("Invalid ",
                            ifelse(length(invalid.values)==1, 'value', 'values'),
                            " fir dimension '", d, "' in ", dimension.values.name, ": ",
                            paste0("'", invalid.values, "'", collapse=', ')))
            
            dimension.values[[d]]
        }
        else if (is.logical(dimension.values[[d]]))
        {
            if (length(dimension.values[[d]])!=dim(arr)[d])
                stop("Invalid value for dimension '", d,
                     "' in ", dimension.values.name, ": a logical vector for '", d, "' must be of length ",
                     dim(arr)[d])
            
            if (!any(dimension.values[[d]]))
                stop("Invalid value for dimension '", d,
                     "' in ", dimension.values.name, ": all elements of the logical vector are FALSE")
            
            dimnames(arr)[[d]][ dimension.values[[d]] ]
        }
        else
            stop(paste0("The elements of ", dimension.values.name, " must be either numeric, logical, or character vectors. The value for dimension '",
                        d, "' is none of these"))
    })
    names(character.dimension.values) = names(dimension.values)
    
    # Check to.write
    if (!is.null(to.write))
    {
        if (!is.numeric(to.write))
            stop("'to.write' must be either (1) an array or matrix with named dimensions and dimnames set, (2) a scalar value, or (3) a named numeric vector")
        else if (is.array(to.write))
        {
            if (is.null(dimnames(to.write)))
                stop("If 'to.write' is an array or matrix, it must have dimnames set")
            
            if (is.null(names(dimnames(to.write))))
                stop("If 'to.write' is an array or matrix, it must have NAMED dimnames")
            
            to.write.dim.names = dimnames(to.write)
        }
        else if (length(to.write)==1)
        {
            to.write.dim.names = list()
        }
        else if (!is.null(names(to.write)))
        {
            matching.access.dim.mask = sapply(character.dimension.values, function(dim.values){
                # a match if dim.values (from target) are a subset of the names of to.expand
                setdiff(dim.values, names(to.write))==0
            })
            
            if (!any(matching.access.dim.mask))
                stop("The names of 'to.write' do not match any dimension in ", dimension.values.name,
                     " (ie, are not a superset of any element of 'dimension.values')")
            else if (sum(matching.access.dim.mask)>0)
                stop(paste0("The names of 'to.write' match multiple dimensions in ", dimension.values.name, ": ",
                            paste0("'", names(character.dimension.values)[matching.access.dim.mask], "'", collapse=', '),
                            " (ie, the names of 'to.write' are a superset of the names of MULTIPLE elements of ", dimension.values.name, ")"))
            
            to.write.dim.names = list(names(to.write))
            names(to.write.dim.names) = names(character.dimension.values)[matching.access.dim.mask]
        }
        else
        {
            stop("If 'to.write' is not an array, matrix, or scalar value, it must be NAMED")
        }
    
        #-- Check that all dimensions of to.write.dim.names are present in target.dim.names --#
        
        if (is.array(to.write))
            src.text = "the dimnames of 'to.write'"
        else
            src.text = "the names of 'to.write'"
        
        dims.missing.from.access = setdiff(names(to.write), names(character.dimension.values))
        if (length(dims.missing.from.access)==1)
            stop(paste0("The dimension '", dims.missing.from.access,
                        "' is present in ", src.text, " but missing from ", dimension.values.name))
        else if (length(dims.missing.from.access)>1)
            stop(paste0("The dimensions ",
                        paste0("'", dims.missing.from.access, "'", collapse=', '),
                        " are present in ", src.text, " but missing from ", dimension.values.name))
        
        # Check that each element of dimension.values is either
        # (1) Missing entirely from to.write.dim.names OR
        # (2) A subset of the corresponding value in to.write.dim.names
        overlapping.dimensions = intersect(names(character.dimension.values), names(to.write.dim.names))
        sapply(overlapping.dimensions, function(d){
            missing.from.src = setdiff(character.dimension.values[[d]], to.write.dim.names[[d]])
            if (length(missing.from.src)==1)
                stop(paste0("'", missing.from.src, "' is given as a value in dimensions '",
                            d, "' of ", dimension.values.name, ", but is not present in dimension '", d, "' of ", src.text))
            else if (length(missing.from.src)>1)
                stop(paste0(paste0("'", missing.from.src, "'", collapse=', '),
                            " are given as values in dimension '",
                            d, "' of ", dimension.values.name, ", but is not present in dimension '", d, "' of ", src.text))
        })
    }
}

#'@title Expand an Array to Greater Dimensions
#'
#'@param to.expand The value to be expanded. Can be either (1) an array or matrix with named dimensions and dimnames set, (2) a scalar value, or (3) a named vector whose names are a superset of one of the elements of target.dim.names
#'@param target.dim.names A named list containing the values of dimensions to subset for. The names of dimension.values should correspond to the names of dimnames. The elements of dimension.values should be character, integer, or logical vectors
#'
#'@examples
#'
#'@export
expand.array <- function(to.expand, target.dim.names)
{
    if (length(to.expand)==1)
    {
        array(as.numeric(to.expand), 
              dim=sapply(target.dim.names, length),
              dimnames = target.dim.names)
    }
    else
    {
        rv = array(0, dim=sapply(target.dim.names, length), dimnames=target.dim.names)
        rv = do_expand_array(dst_array=rv, src_array=to.expand)
        if (is.null(rv))
        {
            check.expand.arguments(to.expand=to.expand, target.dim.names=target.dim.names)
            stop("There was an error expanding the array")
        }
        rv
    }
}

#'@export
get.expand.array.indices <- function(to.expand.dim.names, target.dim.names)
{
    if (length(to.expand.dim.names)==0)
    {
        if (length(target.dim.names)==0)
            1
        else
            array(1, 
                  dim=sapply(target.dim.names, length),
                  dimnames = target.dim.names)
    }
    else
    {
        if (length(target.dim.names)==0)
        {
            if (length(to.expand.dim.names)>0 && prod(sapply(to.expand.dim.names, length))>1)
                stop("Cannot create expand indices - 'to.expand.dim.names' are non-empty but 'target.dim.names' are empty")
            else
                1
        }
        else
        {
            rv = array(0, dim=sapply(target.dim.names, length), dimnames=target.dim.names)
            rv = do_get_expand_indices(dst_array=rv, src_dim_names = to.expand.dim.names)
            if (is.null(rv))
            {
                check.expand.arguments(to.expand=to.expand, target.dim.names=target.dim.names)
                stop("There was an error creating the expand indices")
            }
            rv
        }
    }
}



check.expand.arguments <- function(to.expand=NULL,
                                   to.expand.dim.names=NULL,
                                   target.dim.names)
{
    #-- Check target.dim.names --#
    if (is.null(target.dim.names))
        stop("target.dim.names' cannot be NULL")
    
    if (!is.list(target.dim.names) || length(target.dim.names)==0)
        stop("'target.dim.names' must be a non-empty list")
    
    if (is.null(names(target.dim.names)))
        stop("'target.dim.names' must be a NAMED list")
    
    if (length(unique(names(target.dim.names))) != length(target.dim.names))
        stop("The names of 'target.dim.names' must be unique")
    
    sapply(names(target.dim.names), function(d){
        if (!is.character(target.dim.names[[d]]) || length(target.dim.names[[d]])==0)
            stop(paste0("The elements of 'target.dim.names' must all be non-empty character vectors. target.dim.names[['",
                        d, "']] is not."))
        if (length(unique(target.dim.names[[d]]))!=length(target.dim.names[[d]]))
            stop(paste0("The elements of 'target.dim.names' must have unique character values. The values of target.dim.names[['",
                        d, "']] are not unique"))
    })
    
    
    #-- Check the src dim names (either the specified list, or the dimnames of to.expand) --#
    if (is.null(to.expand))
    {
        if (is.null(to.expand.dim.names))
            stop("Either 'to.expand' or 'to.expand.dim.names' must be specified")
        
        if (!is.list(to.expand.dim.names) || length(to.expand.dim.names)==0)
            stop("'to.expand.dim.names' must be a non-empty list")
        
        if (is.null(names(to.expand.dim.names)))
            stop("'to.expand.dim.names' must be a NAMED list")
        
        if (length(unique(names(to.expand.dim.names))) != length(to.expand.dim.names))
            stop("The names of 'to.expand.dim.names' must be unique")
        
        sapply(names(to.expand.dim.names), function(d){
            if (!is.character(to.expand.dim.names[[d]]) || length(to.expand.dim.names[[d]])==0)
                stop(paste0("The elements of 'to.expand.dim.names' must all be non-empty character vectors. to.expand.dim.names[['",
                            d, "']] is not."))
            if (length(unique(to.expand.dim.names[[d]]))!=length(to.expand.dim.names[[d]]))
                stop(paste0("The elements of 'to.expand.dim.names' must have unique character values. The values of to.expand.dim.names[['",
                            d, "']] are not unique"))
        })
    }
    else
    {
        if (!is.numeric(to.expand))
            stop("'to.expand' must be either (1) an array or matrix with named dimensions and dimnames set, (2) a scalar value, or (3) a named numeric vector")
        else if (is.array(to.expand))
        {
            if (is.null(dimnames(to.expand)))
                stop("If 'to.expand' is an array or matrix, it must have dimnames set")
            
            if (is.null(names(dimnames(to.expand))))
                stop("If 'to.expand' is an array or matrix, it must have NAMED dimnames")
            
            to.expand.dim.names = dimnames(to.expand)
        }
        else if (length(to.expand)==1)
        {
            to.expand.dim.names = list()
        }
        else if (!is.null(names(to.expand)))
        {
            matching.target.dim.mask = sapply(target.dim.names, function(dim.values){
                # a match if dim.values (from target) are a subset of the names of to.expand
                setdiff(dim.values, names(to.expand))==0
            })
            
            if (!any(matching.target.dim.mask))
                stop("The names of 'to.expand' do not match any dimension in 'target.dim.names' (ie, are not a superset of any element in 'target.dim.names')")
            else if (sum(matching.target.dim.mask)>0)
                stop(paste0("The names of 'to.expand' match multiple dimensions in 'target.dim.names': ",
                            paste0("'", names(target.dim.names)[matching.target.dim.mask], "'", collapse=', '),
                            " (ie, the names of 'to.expand' are a superset of multiple elements of 'target.dim.names')"))
            
            to.expand.dim.names = list(names(to.expand))
            names(to.expand.dim.names) = names(target.dim.names)[matching.target.dim.mask]
        }
        else
        {
            stop("If 'to.expand' is not an array, matrix, or scalar value, it must be NAMED")
        }
    }
    
    #-- Check that all dimensions of to.expand.dim.names are present in target.dim.names --#
    
    if (is.null(to.expand))
        src.text = "'to.expand.dim.names'"
    else if (is.array(to.expand))
        src.text = "the dimnames of 'to.expand'"
    else
        src.text = "the names of 'to.expand'"
    
    dims.missing.from.target = setdiff(names(to.expand.dim.names), names(target.dim.names))
    if (length(dims.missing.from.target)==1)
        stop(paste0("The dimension '", dims.missing.from.target,
                    "' is present in ", src.text, " but missing from 'target.dim.names"))
    else if (length(dims.missing.from.target)>1)
        stop(paste0("The dimensions ",
                    paste0("'", dims.missing.from.target, "'", collapse=', '),
                    " are present in ", src.text, " but missing from 'target.dim.names"))
    
    # Check that each element of target.dim.names is either
    # (1) Missing entirely from to.expand.dim.names OR
    # (2) A subset of the corresponding value in to.expand.dim.names
    overlapping.dimensions = intersect(names(target.dim.names), names(to.expand.dim.names))
    sapply(overlapping.dimensions, function(d){
        missing.from.src = setdiff(target.dim.names[[d]], to.expand.dim.names[[d]])
        if (length(missing.from.src)==1)
            stop(paste0("'", missing.from.src, "' is given as a value in target.dim.names[['",
                        d, "']], but is not present in dimension '", d, "' of ", src.text))
        else if (length(missing.from.src)>1)
            stop(paste0(paste0("'", missing.from.src, "'", collapse=', '),
                        " are given as values in target.dim.names[['",
                        d, "']], but is not present in dimension '", d, "' of ", src.text))
    })
}