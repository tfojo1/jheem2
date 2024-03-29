
# Depends on HELPERS_misc_helpers

# A GENERAL NOTE:
# 
# dim.names are taken to be named lists of *character* vectors,
#    which are used to set the dimnames attribute of arrays
# dimension.values are named lists of either
#   (1) character vectors, (2) numeric vectors, or (3) logical vectors
#   which are used to access arrays 
#   
# dim.names are thus a specific type of dimension.values
# Many, though not all, helpers dealing with these have the same 
#   underlying implementation


##---------------------------------------##
##-- UNION DIMENSION VALUES/ DIM NAMES --##
##---------------------------------------##


#'@title Join multiple dim.names lists
#'
#'@return A named list, with an element for each name that appears in any of the dim.names in ... - rv[[d]] is the union of all values in the d dimension of any dim.names in ...
#'
#'@param ... can be either (1) dim.names or (2) lists of dim.names
outer.join.dim.names <- function(..., error.prefix='')
{
    #-- Process arguments --#
    #   Flatten out lists - so that each element of dim.names.list is a named list of character vectors)
    args = list(...)
    dim.names.list = list()
    for (elem in args)
    {
        if (!is.list(elem))
            stop(paste0(error.prefix,
                        "In outer joining dim.names, ... must contain only lists"))
        
        if (length(elem)>0)
        {
            if (is.list(elem[[1]]))
            {
                if (any(!sapply(elem, is.list)))
                    stop("In outer-joining dim.names, if an element of ... contains lists of dim.names, that element can contain ONLY lists")
                
                dim.names.list = c(dim.names.list, elem)
            }
            else
                dim.names.list = c(dim.names.list,
                                    list(elem))
        }
    }
    
    #-- Make sure all dim.names are named lists
    if (any(sapply(dim.names.list, function(dv){
        is.null(names(dv))
    })))
        stop(paste0(error.prefix,
                    "In outer-joining dim.names, all arguments must be NAMED lists"))
    
    
    #-- Set up the rv and iterate through dim.names.list, merging one at a time --#
    rv = dim.names.list[[1]]
    dim.names.list = dim.names.list[-1]   
    while(length(dim.names.list)>0)
    {
        to.merge = dim.names.list[[1]]
        merged.dimensions = union(names(rv), names(to.merge))
        rv = lapply(merged.dimensions, function(d){
            union(rv[[d]], to.merge[[d]])
        })
        names(rv) = merged.dimensions
        
        dim.names.list = dim.names.list[-1]
    }
    
    rv
}


# I need to figure out what 'union' means before moving on
union.dim.names <- function(...)
{
    union.dimension.values(...)
}

#'@param ... can be either (1) dimension values or (2) lists of dimension values
union.dimension.values <- function(..., error.prefix='')
{
    # Process arguments (flatten out lists)
    args = list(...)
    dim.values.list = list()
    for (elem in args)
    {
        if (!is.list(elem))
            stop(paste0(error.prefix,
                        "In unioning dim.names or dimension.values, ... must contain only lists"))
        
        if (length(elem)>0)
        {
            if (is.list(elem[[1]]))
            {
                if (any(!sapply(elem, is.list)))
                    stop("In unioning dim.names or dimension.values, if an element of ... contains lists of dim.names/dimension.values, that element can contain ONLY lists")
                
                dim.values.list = c(dim.values.list, elem)
            }
            else
                dim.values.list = c(dim.values.list,
                                    list(elem))
        }
    }
    
    if (any(sapply(dim.values.list, function(dv){
        is.null(names(dv))
    })))
        stop(paste0(error.prefix,
                    "In unioning dim.names or dimension.values, all arguments must be NAMED lists"))
    
    
    # Set up the rv and iterate through dim.values.list
    # merging one at a time
    rv = dim.values.list[[1]]
    dim.values.list = dim.values.list[-1]
stop("NEED TO IMPLEMENT CHECKS HERE!")    
    while(length(dim.values.list)>0)
    {
        to.merge = dim.values.list[[1]]
        overlapping.dimensions = union(names(rv), names(to.merge))
        
        rv = lapply(overlapping.dimensions, function(d){
            if (is.logical(rv[[d]]))
            {
                if (!is.logical(to.merge[[d]]))
                    stop(paste0(error.prefix, 
                                "In unioning dim.names or dimension.values, all "))
                values = rv[[d]] | dim.values.list[[1]][[d]]
            }
            else
                values = union(rv[[d]], dim.values.list[[1]][[d]])
            
            if (is.numeric(values))
                values = sort(values)
            
            values
        })
        names(rv) = overlapping.dimensions
        
        dim.values.list = dim.values.list[-1]
    }
    
    rv
}

##-------------------------------------------##
##--        UNION *SHARED* DIM NAMES       --##
##-- (union the values for dimensions that --##
##--   are shared in two dim.names lists)  --##
##-------------------------------------------##

##-----------------------##
##-- LOW-LEVEL HELPERS --##
##-----------------------##


# - Any dimension that does not appear in ALL the dim.names above is dropped
# - For any dimension that does appear in ALL the dim.names above, the value is the union of the values for that dimension in each of the dim.names above
union.shared.dim.names <- function(dim.names.1, dim.names.2)
{
    if (is.null(dim.names.1))
        dim.names.2
    else if (is.null(dim.names.2))
        dim.names.1
    else
    {
        overlapping.dimensions = intersect(names(dim.names.1), names(dim.names.2))
        
        rv = lapply(overlapping.dimensions, function(d){
            union(dim.names.1[[d]], dim.names.2[[d]])
        })
        names(rv) = overlapping.dimensions
        rv
    }
}

# Returns a list with two elements
# $dim.names
# $aliases
# 
# aliases - a named character vector. The names of the vector correspond to current names of dimensions. The values correspond to the names they could be swapped for
union.shared.dim.names.with.aliases <- function(dim.names.1, aliases.1,
                                               dim.names.2, aliases.2)
{
    if (is.null(dim.names.1))
        list(dim.names=dim.names.2, aliases=aliases.2)
    else if (is.null(dim.names.2))
        list(dim.names=dim.names.1, aliases=aliases.1)
    else
    {
        # Figure out which aliases we need to use
        aliases.1.in.dim.names.2.mask = as.logical(sapply(aliases.1, function(name){
            any(name == names(dim.names.2))
        }))
        aliases.2.in.dim.names.1.mask = as.logical(sapply(aliases.2, function(name){
            any(name == names(dim.names.1))
        }))
        
        aliases.1.in.dim.names.2 = aliases.1[aliases.1.in.dim.names.2.mask]
        aliases.2.in.dim.names.1 = aliases.2[aliases.2.in.dim.names.1.mask]
        
        # Rename the dimensions in 1 and 2 using the aliases we are going to use
        names(dim.names.1) = replace.with.aliases(names(dim.names.1), aliases=aliases.1.in.dim.names.2)
        names(dim.names.2) = replace.with.aliases(names(dim.names.2), aliases=aliases.2.in.dim.names.1)
        
        # Union the (renamed to aliases) dim.names
        dim.names = union.shared.dim.names(dim.names.1, dim.names.2)
        
        # Figure out which aliases are still 'viable'
        # An alias is viable if the original (un-aliased) name is in the union-ed dim.names
        #   AND the alias is present in both 1 and 2
        viable.aliases.mask = as.logical(sapply(names(aliases.1), function(alias.name){
            any(alias.name==names(dim.names)) &&
                any(alias.name==names(aliases.2)) &&
                all(aliases.1[alias.name]==aliases.2[alias.name])
        }))
        viable.aliases = aliases.1[viable.aliases.mask]
        
        
        # Return
        list(dim.names = dim.names, aliases = viable.aliases)
    }
}

replace.with.aliases <- function(values, aliases)
{
    sapply(values, function(v){
        if (any(v==names(aliases)))
            aliases[v]
        else
            v
    })
}

##----------------------------------##
##-- TESTING FOR EQUALITY/SUBSETS --##
##----------------------------------##

dimension.values.equal <- function(dimension.values.1,
                                   dimension.values.2,
                                   match.order.of.dimensions = T,
                                   match.order.within.dimensions = T)
{
stop('need to implement')
}

dim.names.equal <- function(dim.names.1, dim.names.2,
                            match.order.of.dimensions = T,
                            match.order.within.dimensions = T)
{
    is.list(dim.names.1) && is.list(dim.names.2) &&
        length(dim.names.1) == length(dim.names.2) &&
        !is.null(names(dim.names.1)) && !is.null(dim.names.2) &&
        ( (match.order.of.dimensions && all(names(dim.names.1) == names(dim.names.2))) ||
              (!match.order.of.dimensions && setequal(names(dim.names.1), names(dim.names.2))) ) &&
        all(sapply(names(dim.names.1), function(d){
            if (match.order.within.dimensions)
                length(dim.names.1[[d]]) == length(dim.names.2[[d]]) &&
                all(dim.names.1[[d]] == dim.names.2[[d]])
            else
                length(dim.names.1[[d]]) == length(dim.names.2[[d]]) &&
                setequal(dim.names.1[[d]], dim.names.2[[d]])
        }))
}

dim.names.are.subset <- function(sub.dim.names, super.dim.names)
{
    dimension.values.are.subset(sub.dim.names, super.dim.names)
}

dimension.values.are.subset <- function(sub.dimension.values, super.dimension.values)
{
    (is.null(sub.dimension.values) || is.list(sub.dimension.values)) &&
        is.list(super.dimension.values) &&
        (length(sub.dimension.values) == 0 ||
             (!is.null(names(sub.dimension.values)) && !is.null(names(super.dimension.values)) &&
            length(setdiff(names(sub.dimension.values), names(super.dimension.values))) == 0 &&
            all(sapply(names(sub.dimension.values), function(d){
                length(setdiff(sub.dimension.values[[d]], super.dimension.values[[d]])) == 0
            }))
             ))
}


subset.of.dimension.values.are.equal <- function(sub.dimension.values, super.dimension.values,
                                                 match.order.within.dimensions = T)
{
    is.list(sub.dimension.values) && is.list(super.dimension.values) &&
        !is.null(names(sub.dimension.values)) && !is.null(names(super.dimension.values)) &&
        length(setdiff(names(sub.dimension.values), names(super.dimension.values))) == 0 &&
        dimension.values.equal(sub.dimension.values, super.dimension.values[names(sub.dimension.values)],
                               match.order.within.dimensions = match.order.within.dimensions)
}


subset.of.dim.names.are.equal <- function(sub.dim.names, super.dim.names,
                                                 match.order.within.dimensions = T)
{
    subset.of.dimension.values.are.equal(sub.dim.names, super.dim.names,
                                         match.order.within.dimensions = match.order.within.dimensions)
}

are.dim.names.compatible <- function(dim.names.1, dim.names.2, 
                                     match.order=T,
                                     dim.names.2.is.subset=F)
{
    if (is.null(dim.names.1) || is.null(dim.names.2))
        T
    else
    {
        overlapping.dimensions = intersect(names(dim.names.1), names(dim.names.2))
        if (dim.names.2.is.subset && !setequal(overlapping.dimensions, names(dim.names.2)))
            F
        else
            all(sapply(overlapping.dimensions, function(d){
                if (match.order)
                    length(dim.names.1[[d]])==length(dim.names.2[[d]]) &&
                        all(dim.names.1[[d]] == dim.names.2[[d]])
                else
                    setequal(dim.names.1[[d]], dim.names.2[[d]])
            }))
    }
}

union.matching.dim.names <- function(dim.names.1, dim.names.2)
{
    if (is.null(dim.names.1))
        dim.names.2
    else if (is.null(dim.names.2))
        dim.names.1
    else
    {
        dimensions = union(names(dim.names.1), names(dim.names.2))
        rv = lapply(dimensions, function(dim){
            if (is.null(dim.names.1[[dim]]))
                dim.names.2[[dim]]
            else
                dim.names.1[[dim]]
        })
        names(rv) = dimensions
        rv
    }
}

##-----------------------##
##-- CHECK FOR OVERLAP --##
##-----------------------##

dim.names.overlap <- function(dim.names.1, dim.names.2)
{
    dimension.values.overlap(dim.names.1, dim.names.2)
}

# two sets of dim.values overlap if either
# 1) they do not name any dimensions in common (the unspecified dimensions overlap)
# 2) all of the dimensions present in both sets of values overlap on at least one value
# dimension.values are the values of dimensions to subset (unspecified dimensions keep all values)
dimension.values.overlap <- function(dimension.values.1, dimension.values.2)
{
    overlapping.dimensions = intersect(names(dimension.values.1), names(dimension.values.2))
    length(overlapping.dimensions)==0 ||
        all(sapply(overlapping.dimensions, function(d){
            
            values1 = dimension.values.1[[d]]
            values2 = dimension.values.2[[d]]
            
            if ((is.character(values1) && is.character(values2)) ||
                is.integer(values1) && is.integer(values2))
            {
                length(intersect(values1, values2)) > 0
            }
            else if (is.logical(values1) && is.logical(values2))
            {
                len = min(length(values1), length(values2))
                any(values1[1:len] & values2[1:len])
            }
            else if (is.integer(values1) && is.logical(values2))
            {
                values2 = (1:length(values2))[values2]
                length(intersect(values1, values2))>0
            }
            else if (is.logical(values1) && is.integer(values2))
            {
                values1 = (1:length(values1))[values1]
                length(intersect(values1, values2))>0
            }
            else
                F
        }))
}



##-------------------------------------------------------##
##-- VALIDATE a DIM NAMES or DIMENSION VALUES ARGUMENT --##
##-------------------------------------------------------##

check.dim.names.valid <- function(dim.names,
                                  variable.name.for.error,
                                  refer.to.dimensions.as = 'dimension',
                                  refer.to.dimension.plural.as = paste0(refer.to.dimensions.as, 's'),
                                  allow.empty = T,
                                  allow.duplicate.values.within.dimensions = F,
                                  allow.duplicate.values.across.dimensions = F,
                                  error.prefix='')
{
    if (missing(variable.name.for.error))
        stop("Must specify 'variable.name.for.error' for check.dim.names.valid() to work")
    
    if (!is.list(dim.names))
        stop(paste0(error.prefix, "'", variable.name.for.error, "' must be a list"))
    
    if (!allow.empty && length(dim.names)==0)
        stop(paste0(error.prefix, "'", variable.name.for.error, "' cannot be an empty list"))
    
    if (length(dim.names)>0 && is.null(names(dim.names)))
        stop(paste0(error.prefix, "'", variable.name.for.error, "' must be a NAMED list"))
    
    if (any(sapply(dim.names, is.null)))
        stop(paste0(error.prefix, "'", variable.name.for.error, "' canot contain NULL values"))
    
    if (any(is.na(names(dim.names)) | nchar(names(dim.names))==0))
        stop(paste0(error.prefix, "The names of '", variable.name.for.error, "' cannot be NA or empty strings"))
    
    not.character.mask = !sapply(dim.names, is.character)
    if (any(not.character.mask))
    {
        stop(paste0(error.prefix,
                    "'The elements of '", variable.name.for.error, 
                    "' must all be character vectors. ",
                    ifelse(sum(not.character.mask)==1,
                           paste0(toupper.first(refer.to.dimensions.as), " '",
                                  names(dim.names)[not.character.mask], "' is not"),
                           paste0(toupper.first(refer.to.dimensions.plural.as), " ", 
                                  collapse.with.conjunction("'", names(dim.names)[not.character.mask], "'"),
                                  " are not"))
        ))
    }
    
    length.zero.mask = sapply(dim.names, length)==0
    if (any(length.zero.mask))
        stop(paste0(error.prefix,
                    "'The elements of '", variable.name.for.error, 
                    "' must all contain at least one value. ",
                    ifelse(sum(length.zero.mask)==1,
                           paste0(toupper.first(refer.to.dimensions.as), " '", 
                                  names(dim.names)[length.zero.mask], "' does not"),
                           paste0(toupper.first(refer.to.dimension.plural.as), " ",
                                  collapse.with.conjunction("'", names(dim.names)[length.zero.mask], "'"),
                                  " do not"))
        ))
    
    tabled.dimensions = table(names(dim.names))
    if (any(tabled.dimensions>1))
        stop(paste0(error.prefix,
                    "The names of '", variable.name.for.error,
                    "' must be unique. ",
                    collapse.with.conjunction("'", names(tabled.dimensions)[tabled.dimensions>1], "'"),
                    ifelse(sum(tabled.dimensions>1)==1, " appears", " appear"),
                    " more than once"))
    
    
    if (!allow.duplicate.values.across.dimensions)
    {
        tabled.dim.values = table(unlist(dim.names))
        if (any(tabled.dim.values>1))
        {
            repeated.dim.values = names(tabled.dim.values)[tabled.dim.values>1]
            repeated.counts = as.integer(tabled.dim.values[tabled.dim.values>1])
            dimensions.for.repeated.values = lapply(repeated.dim.values, function(val){
                rv = names(dim.names)[sapply(dim.names, function(values){
                    any(values==val)
                })]
                
                if (length(rv)==1)
                    paste0("(in ", refer.to.dimensions.as, " '", rv, "')")
                else
                    paste0("(in ", refer.to.dimension.plural.as, " ",
                           collapse.with.conjunction("'", rv, "'"), ")")
            })
            
            stop(paste0(error.prefix,
                        "Values cannot be used more than once across ",
                        refer.to.dimension.plural.as, " of '", 
                        variable.name.for.error, "', but ",
                        collapse.with.conjunction("'", repeated.dim.values, "' is used ",
                                                  repeated.counts, " times ",
                                                  dimensions.for.repeated.values)))
        }
    }
    
    if (!allow.duplicate.values.within.dimensions)
    {
        tabled.dim.values.by.dim = lapply(dim.names, table)
        if (any(sapply(tabled.dim.values.by.dim, max)>1))
        {
            dimensions.with.repeats = names(dim.names)[sapply(tabled.dim.values.by.dim, max)>1]
            repeated.dim.values = sapply(tabled.dim.values.by.dim[dimensions.with.repeats], function(counts){
                rep.dv = names(counts)[counts>1]
                rep.counts = as.integer(counts[counts>1])
                
                collapse.with.conjunction("value '", rep.dv, "' ", rep.counts, " times")
            })
            
            stop(paste0(error.prefix,
                        "Values cannot be used more than once within a ",
                        refer.to.dimensions.as, " of '", 
                        variable.name.for.error, "', but ",
                        collapse.with.conjunction(refer.to.dimensions.as, " '", 
                                                  dimensions.with.repeats, "' uses ",
                                                  repeated.dim.values)))
        }
    }
}

check.dimension.values.valid <- function(dimension.values,
                                         variable.name.for.error,
                                         refer.to.dimensions.as = 'dimension',
                                         refer.to.dimension.plural.as = paste0(refer.to.dimensions.as, 's'),
                                         allow.empty = T,
                                         allow.duplicate.values.within.dimensions = F,
                                         error.prefix='')
{
    if (missing(variable.name.for.error))
        stop("Must specify 'variable.name.for.error' for check.dimension.values.valid() to work")
    
    if (!is.list(dimension.values))
        stop(paste0(error.prefix, "'", variable.name.for.error, "' must be a list"))
    
    if (!allow.empty && length(dimension.values)==0)
        stop(paste0(error.prefix, "'", variable.name.for.error, "' cannot be an empty list"))
    
    if (length(dimension.values)>0 && is.null(names(dimension.values)))
        stop(paste0(error.prefix, "'", variable.name.for.error, "' must be a NAMED list"))
    
    if (any(sapply(dimension.values, is.null)))
        stop(paste0(error.prefix, "'", variable.name.for.error, "' canot contain NULL values"))
    
    if (any(is.na(names(dimension.values)) | nchar(names(dimension.values))==0))
        stop(paste0(error.prefix, "The names of '", variable.name.for.error, "' cannot be NA or empty strings"))
    
   
    length.zero.mask = sapply(dimension.values, length)==0
    if (any(length.zero.mask))
        stop(paste0(error.prefix,
                    "'The elements of '", variable.name.for.error, 
                    "' must all contain at least one value. ",
                    ifelse(sum(length.zero.mask)==1,
                           paste0(toupper.first(refer.to.dimensions.as), " '", 
                                  names(dimension.values)[length.zero.mask], "' does not"),
                           paste0(toupper.first(refer.to.dimension.plural.as), " ",
                                  collapse.with.conjunction("'", names(dimension.values)[length.zero.mask], "'"),
                                  " do not"))
        ))
    
    tabled.dimensions = table(names(dimension.values))
    if (any(tabled.dimensions>1))
        stop(paste0(error.prefix,
                    "The names of '", variable.name.for.error,
                    "' must be unique. ",
                    collapse.with.conjunction("'", names(tabled.dimensions)[tabled.dimensions>1], "'"),
                    ifelse(sum(tabled.dimensions>1)==1, " appears", " appear"),
                    " more than once"))
    
    sapply(names(dimension.values), function(d){
        
        elem = dimension.values[[d]]
        
        if (!is.null(dim(elem)))
            stop(paste0(error.prefix,
                        "The elements of '", variable.name.for.error, 
                        "' cannot be arrays or matrices, but ",
                        variable.name.for.error, "[['", d, "']] is"))
        else if (length(elem)==0)
            stop(paste0(error.prefix,
                        "The elements of '", variable.name.for.error, 
                        "' must contain at least one value. ",
                        variable.name.for.error, "[['", d, "']] does not"))
        else if (any(is.na(elem)))
            stop(paste0(error.prefix,
                        "The elements of '", variable.name.for.error, 
                        "' cannot contain NA values. ",
                        variable.name.for.error, "[['", d, "']] does"))
        else if (is.numeric(elem) || is.character(elem))
        {
            if (is.numeric(elem))
            {
                if (any(elem<1))
                    stop(paste0(error.prefix,
                                "If they are numeric vectors, the elements of '", variable.name.for.error, 
                                "' all be greater than 1. ",
                                variable.name.for.error, "[['", d, "']] are not"))
            }
            
            use.quote = ''
            if (is.character(elem))
                use.quote = "'"
            
            if (!allow.duplicate.values.within.dimensions)
            {
                tabled.values = table(elem)
                if (any(tabled.values>1))
                    stop(paste0(error.prefix,
                                "If they are ", 
                                ifelse(is.character(elem), "character", "numeric"),
                                " vectors, the elements of '", variable.name.for.error, 
                                "' cannot contain repeated values. ",
                                variable.name.for.error, "[['", d, "']] does: ",
                                collapse.with.and(use.quote, names(tabled.values)[tabled.values>1], use.quote)))
            }
        }
        else if (is.logical(elem))
        {
            if (!any(elem))
                stop(paste0(error.prefix,
                            "'If they are logical vectors, the elements of '", variable.name.for.error, 
                            "' must contain at least one TRUE value. ",
                            variable.name.for.error, "[['", d, "']] does not"))
        }
        else
            stop(paste0(error.prefix,
                        "The elements of '", variable.name.for.error, 
                        "' must either be character, numeric, or logical vectors. ",
                        variable.name.for.error, "[['", d, "']] is none of these"))
    })
}
