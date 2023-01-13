
# Depends on HELPERS_misc_helpers

union.dimension.names <- function(...)
{
    union.dimension.values(...)
}

union.dimension.values <- function(...)
{
    dim.values.list = list(...)
    rv = dim.values.list[[1]]
    dim.values.list = dim.values.list[-1]
    
    while(length(dim.values.list)>0)
    {
        overlapping.dimensions = union(names(rv), names(dim.values.list[[1]]))
        rv = lapply(overlapping.dimensions, function(d){
            if (is.logical(rv[[d]]))
                values = rv[[d]] | dim.values.list[[1]][[d]]
            else
                values = union(rv[[d]], dim.values.list[[1]][[d]])
            
            if (is.numeric(values))
                values = sort(values)
            
            values
        })
    }
    
    rv
}

union.dimension.values.list <- function(dimension.values.list)
{
    if (length(dimension.values.list)==1)
        dimension.values.list[[1]]
    else
        union.dimension.values(dimension.values.list[[1]], 
                               union.dimension.values.list(dimension.values.list[-1]))
}

dim.names.equal <- function(dim.names.1, dim.names.2)
{
    #@need to implement
}

dim.names.are.subset <- function(sub.dim.names, super.dim.names)
{
    #@need to implement
}

check.dim.names.valid <- function(dim.names,
                                  variable.name.for.error,
                                  refer.to.dimensions.as = 'dimension',
                                  refer.to.dimension.plural.as = paste0(refer.to.dimensions.as, 's'),
                                  allow.empty = T,
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
    
    not.character.mask = !sapply(dim.names, is.character)
    if (any(not.character.mask))
        stop(paste0(error.prefix,
                    "'The elements of '", variable.name.for.error, 
                    "' must all be character vectors. ",
                    ifelse(sum(not.character.mask)==1,
                           paste0(toupper.first(refer.to.dimension.as), " '",
                                  names(dim.names)[not.character.mask], "' is not"),
                           paste0(toupper.first(refer.to.dimensions.plural.as), " ", 
                                  collapse.with.conjunction("'", names(dim.names)[not.character.mask], "'"),
                                  " are not"))
        ))
    
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
    
    if (allow.duplicate.values.across.dimensions)
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
    else
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
}
