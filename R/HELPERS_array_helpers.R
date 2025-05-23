
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
            if (length(elem)>0 && 
                (is.null(names(elem)) || any(is.na(names(elem))) || any(nchar(names(elem))==0)))
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
                                     dimension.values,
                                     index.from = 1)
{
    if (length(arr.dim.names)==0)
    {
        if (length(dimension.values)==0)
            index.from
        else
            stop("Have asked to get.array.access.indices with empty arr.dim.names but non-empty dimension.values")
            #rep(index.from, prod(sapply(dimension.values, length))) #Is this right? I'm not usre
    }
    else
    {
        arr = array(index.from:(prod(vapply(arr.dim.names, length, FUN.VALUE = numeric(1)))-1+index.from),
                    dim=sapply(arr.dim.names, length),
                    dimnames=arr.dim.names)
        
        as.integer(fast.array.access(arr, dimension.values))
    }
}

#'@export
'array.access<-' = function(arr,
                            ...,
                            value)
{# Process ... into a list of dimension values
    args = list(...)
    
    error.prefix = 'Error in array.access(): '
    dimension.values = list()
    for (elem.index in 1:length(args))
    {
        elem = args[[elem.index]]
        if (is.list(elem))
        {
            if (length(elem)>0 && 
                (is.null(names(elem)) || any(is.na(names(elem))) || any(nchar(names(elem))==0)))
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
    
    if (length(dimension.values)>0 && is.null(names(dimension.values)))
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
                            " for dimension '", d, "' in ", dimension.values.name, ": ",
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
get.expand.array.indices <- function(to.expand.dim.names, 
                                     target.dim.names,
                                     index.from = 1)
{
    if (length(to.expand.dim.names)==0)
    {
        if (length(target.dim.names)==0)
            as.integer(index.from)
        else
            as.integer(rep(index.from, length=prod(sapply(target.dim.names, length))))
    }
    else
    {
        if (length(target.dim.names)==0)
        {
            if (length(to.expand.dim.names)>0 && prod(sapply(to.expand.dim.names, length))>1)
                stop("Cannot create expand indices - 'to.expand.dim.names' are non-empty but 'target.dim.names' are empty")
            else
                as.integer(index.from)
        }
        else
        {
            rv = array(0, dim=sapply(target.dim.names, length), dimnames=target.dim.names)
            rv = do_get_expand_indices(dst_array=rv, 
                                       src_dim_names = to.expand.dim.names,
                                       index_from = index.from)
            if (is.null(rv))
            {
                check.expand.arguments(to.expand.dim.names=to.expand.dim.names, target.dim.names=target.dim.names)
                stop("There was an error creating the expand indices")
            }
            
            as.integer(rv)
        }
    }
}

#'@title Get a Set of Indices to Collapse One Array into Another
#'
#'@return A list with four elements: $large.indices, $small.indices, $large.n, $small.n
#'  large.indices and small.indices are integer vectors of equal length, such that the value at large.indices[i] goes into the value at small.indices[i] if collapsing
#'
#'@export
get.collapse.array.indices <- function(large.arr.dim.names, small.arr.dim.names)
{
    intersected.dim.names = intersect.joined.dim.names(large.arr.dim.names, small.arr.dim.names)
    
    if (!dim.names.are.subset(sub.dim.names = intersected.dim.names,
                              super.dim.names = large.arr.dim.names))
        stop("Cannot get.collapse.array.indices(): the large.arr.dim.names are not a superset of the relevant values in small.arr.dim.names")
    
    rv = list(
        small.indices = get.expand.array.indices(to.expand.dim.names = small.arr.dim.names,
                                                 target.dim.names = intersected.dim.names),
        large.indices = get.array.access.indices(arr.dim.names = large.arr.dim.names,
                                                 dimension.values = intersected.dim.names),
        small.n = prod(vapply(small.arr.dim.names, length, FUN.VALUE = numeric(1))),
        large.n = prod(vapply(large.arr.dim.names, length, FUN.VALUE = numeric(1)))
    )
    
    if (length(rv$small.indices) != length(rv$large.indices))
        stop("Internal error in get.collapse.array.indices(): small.indices and large.indices must have the same length")
    
    if (any(rv$small.indices>rv$small.n) || any(rv$small.indices<1))
        stop(paste0("Internal error in get.collapse.array.indices(): small.indices must be between 1 and small.n (", rv$small.n, ")"))
    
    if (any(rv$large.indices>rv$large.n) || any(rv$large.indices<1))
        stop(paste0("Internal error in get.collapse.array.indices(): large.indices must be between 1 and large.n (", rv$large.n, ")"))
    
    rv$no.need.to.collapse = rv$small.n==rv$large.n && all(rv$small.indices==rv$large.indices)
    
    rv
}

get.collapse.array.indices.with.intermediate <- function(large.arr.dim.names,
                                                         intermediate.arr.dim.names,
                                                         small.arr.dim.names)
{
    indices.large.to.intermediate = get.collapse.array.indices(large.arr.dim.names = large.arr.dim.names,
                                                               small.arr.dim.names = intermediate.arr.dim.names)
    
    indices.intermediate.to.small = get.collapse.array.indices(large.arr.dim.names = intermediate.arr.dim.names,
                                                               small.arr.dim.names = small.arr.dim.names)
    
    names(indices.intermediate.to.small$small.indices) = as.character(indices.intermediate.to.small$large.indices)
    
    rv = list(
        small.indices = indices.intermediate.to.small$small.indices[as.character(indices.large.to.intermediate$small.indices)],
        large.indices = indices.large.to.intermediate$large.indices,
        small.n = indices.intermediate.to.small$small.n,
        large.n = indices.large.to.intermediate$large.n
    )
    
    keep.mask = !is.na(rv$small.indices)
    rv$small.indices = rv$small.indices[keep.mask]
    rv$large.indices = rv$large.indices[keep.mask]
    
    rv$no.need.to.collapse = rv$small.n==rv$large.n && all(rv$small.indices==rv$large.indices)
    
    rv
}

get.collapse.array.indices.with.intermediate.and.ontology.mapping <- function(large.arr.dim.names,
                                                                              intermediate.arr.dim.names,
                                                                              ontology.mapping,
                                                                              small.arr.dim.names)
{
    indices.large.to.intermediate = get.collapse.array.indices(large.arr.dim.names = large.arr.dim.names,
                                                               small.arr.dim.names = intermediate.arr.dim.names)
    indices.from.mapping = ontology.mapping$get.mapping.indices(from.dim.names = intermediate.arr.dim.names,
                                                                to.dim.names = small.arr.dim.names)
    
    indices.intermediate.to.small = list(
        small.indices = unlist(sapply(1:length(indices.from.mapping), function(i){
            rep(i, length(indices.from.mapping[[i]]))
        })),
        large.indices = unlist(indices.from.mapping),
        small.n = prod(vapply(small.arr.dim.names, length, FUN.VALUE = numeric(1)))
    )
    
    names(indices.intermediate.to.small$small.indices) = as.character(indices.intermediate.to.small$large.indices)
    
    rv = list(
        small.indices = indices.intermediate.to.small$small.indices[as.character(indices.large.to.intermediate$small.indices)],
        large.indices = indices.large.to.intermediate$large.indices,
        small.n = indices.intermediate.to.small$small.n,
        large.n = indices.large.to.intermediate$large.n
    )
    
    keep.mask = !is.na(rv$small.indices)
    rv$small.indices = rv$small.indices[keep.mask]
    rv$large.indices = rv$large.indices[keep.mask]
    
    rv$no.need.to.collapse = rv$small.n==rv$large.n && all(rv$small.indices==rv$large.indices)
    
    rv
}


#'@export
collapse.array.according.to.indices <- function(arr,
                                                small.indices,
                                                large.indices,
                                                small.n,
                                                check.consistency=T)
{
    if (check.consistency && length(small.indices) != length(large.indices))
        stop("Cannot collapse array; small.indices and large.indices must have the same length")
    
    if (check.consistency && any(small.indices>small.n) || any(small.indices<1))
        stop(paste0("Cannot collapse array; small.indices must be between 1 and small.n (", small.n, ")"))
    
    if (check.consistency && any(large.indices>length(arr)) || any(large.indices<1))
        stop(paste0("Cannot collapse array; large.indices must be between 1 and length(arr) (", length(arr), ")"))
    
    do_collapse_according_to_indices(arr = arr,
                                     small_indices = small.indices,
                                     large_indices = large.indices,
                                     small_n = small.n)
}

check.dim.names.can.expand <- function(to.expand.dim.names,
                                       target.dim.names,
                                       to.expand.name,
                                       target.name,
                                       error.prefix = '')
{
    #-- All dimensions of to.expand.dim.names are present in target.dim.names --#
    
    dims.missing.from.target = setdiff(names(to.expand.dim.names), names(target.dim.names))
    if (length(dims.missing.from.target)==1)
        stop(paste0(error.prefix, "The dimension '", dims.missing.from.target,
                    "' is present in ", to.expand.name, " but missing from ", target.name))
    else if (length(dims.missing.from.target)>1)
        stop(paste0(error.prefix, "The dimensions ",
                    paste0("'", dims.missing.from.target, "'", collapse=', '),
                    " are present in ", to.expand.name, " but missing from ", target.name))
    
    # Check that each element of target.dim.names is either
    # (1) Missing entirely from to.expand.dim.names OR
    # (2) A subset of the corresponding value in to.expand.dim.names
    overlapping.dimensions = intersect(names(target.dim.names), names(to.expand.dim.names))
    sapply(overlapping.dimensions, function(d){
        missing.from.src = setdiff(target.dim.names[[d]], to.expand.dim.names[[d]])
        if (length(missing.from.src)==1)
            stop(paste0(error.prefix, "'", missing.from.src, "' is given as a value for dimension '",
                        d, "' of ", target.name, ", but is not present in ", to.expand.name))
        else if (length(missing.from.src)>1)
            stop(paste0(error.prefix, collapse.with.and("'", missing.from.src, "'"),
                        " are given as values for dimension '",
                        d, "' of ", target.name, ", but are not present in ", to.expand.name))
    })
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
    
    
    check.dim.names.can.expand(to.expand.dim.names = to.expand.dim.names,
                               target.dim.names = target.dim.names,
                               to.expand.name = src.text,
                               target.name = "'target.dim.names'",
                               error.prefix = '')
}


##-----------------##
##-- ARRAY APPLY --##
##-----------------##

apply.robust <- function(arr,
                         MARGIN,
                         FUN,
                         ...)
{
    dimnames.after.apply = dimnames(arr)[names(dimnames(arr)) %in% MARGIN]
    dimnames.after.apply = dimnames.after.apply[MARGIN] # must preserve order!!
    array(apply(arr, MARGIN=MARGIN, FUN=FUN, ...),
          dim=sapply(dimnames.after.apply, length),
          dimnames=dimnames.after.apply)
}

##---------------------##
##-- CHANGE DIMNAMES --##
##---------------------##

set.array.dimnames <- function(arr, new.dimnames)
{
    # validate new dimnames? TO DO!!
    
    return(array(arr, sapply(new.dimnames, length), new.dimnames))
}

##-------------------------#
##-- REDISTRIBUTE VALUES --#
##-------------------------#

#' Distribute dimension values
#' @param arr An array -- no validation is performed
#' @param dimension.values.to.distribute A named list of the dimensions and their values which should be redistributed
#' @param round.distributed.dimension.values Should distributed quantities be rounded?
#' @return An array with the same number of dimensions as 'arr', but without the dimension values specified in 'dimension.values.to.distribute'.
#' @description This function redistributes certain values in a dimension across the remaining values in that dimension.
#' This is useful to remove problematic dimension values -- like race = 'unknown' -- while keeping the marginal sums and the
#' relevant ratios between the remaining dimension values the same as they were before.
#' @details If multiple dimensions should be redistributed, they will be redistributed in the order that they exist in 'dimension.values.to.distribute'.
#' This can result in different outputs for different orders.
distribute.dimension.values <- function(arr, dimension.values.to.distribute, round.distributed.dimension.values) {
    for (d in names(dimension.values.to.distribute)) {
        
        dnames.original = dimnames(arr)
        remove.vals = dimension.values.to.distribute[[d]]
        
        # Put the working dimension first so that colSums can use the rest
        dnames.reordered = dnames.original[c(d, setdiff(names(dnames.original), d))]
        arr.reordered = apply.robust(arr, c(d, setdiff(names(dnames.original), d)), function(x) {x})
        
        # Separate the values to be redistributed in this dimension from the remaining part of the array
        dnames.trimmed = dnames.reordered
        dnames.trimmed[[d]] = setdiff(dnames.reordered[[d]], remove.vals)

        arr.trimmed.args = c(list(arr.reordered),
                             list(dnames.trimmed[[d]]),
                             lapply(setdiff(names(dnames.trimmed), d), function(x) {dnames.trimmed[[x]]}),
                             drop=F)
        arr.trimmed = do.call(`[`, arr.trimmed.args)
        
        arr.to.distribute.args = c(list(arr.reordered),
                                   list(remove.vals),
                                   lapply(setdiff(names(dnames.reordered), d), function(x) {dnames.reordered[[x]]}),
                                   drop=F)
        arr.to.distribute = do.call(`[`, arr.to.distribute.args)
        if (length(dim(arr.to.distribute))>1)
            arr.to.distribute = colSums(arr.to.distribute)

        # Calculate the relative ratios of the various remaining values so that we can distribute in keeping with these ratios
        col.totals = if (length(dim(arr.trimmed))>1) colSums(arr.trimmed) else as.vector(arr.to.distribute)
        fractions = apply.robust(arr.trimmed, d, function(x) {x/col.totals}, simplify = F)
        additions = lapply(fractions, function(x) {x * arr.to.distribute})
        dnames.additions = dnames.trimmed[c(setdiff(names(dnames.trimmed), d), d)]
        
        arr.additions = array(unlist(additions), sapply(dnames.additions, length), dnames.additions)
        # Treat additions that are NA as zero so that they don't make an existing value NA when they add to it
        arr.additions[is.na(arr.additions)] = 0
        
        # Round result if requested
        if (round.distributed.dimension.values) arr.additions = round(arr.additions)
        
        # The above operations put the working dimension last -- move it to the front to match 'arr.trimmed'
        arr.additions.reordered = apply.robust(arr.additions, names(dnames.reordered), function(x) {x})
        
        # Treat additions that are NA as zero so that they don't make an existing value NA when they add to it
        arr.additions.reordered[is.na(arr.additions.reordered)]
        
        arr.result = arr.trimmed + arr.additions.reordered
        
        # Finish by restoring to input order
        arr = apply(arr.result, names(dnames.original), function(x) {x})
    }
    arr
}