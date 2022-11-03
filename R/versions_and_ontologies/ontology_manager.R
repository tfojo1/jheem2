
ONTOLOGY.MANAGER = list(
    mappings = list()
)

get.ontology.mapping <- function(from.dim.names,
                                 to.dim.names)
{
    # Figure out which dim.names are different
    missing.from.from.dimensions = setdiff(names(to.dim.names),
                                           names(from.dim.names))
    if (length(missing.from.from.dimensions)>0)
        stop(paste0("The following dimension(s) are present in 'to.dim.names', but missing from 'from.dim.names': ",
                    paste0("'", missing.from.from.dimensions, "'", collapse=', ')))
    
    missing.from.to.dimensions = setdiff(names(from.dim.names),
                                         names(to.dim.names))
    if (length(missing.from.to.dimensions)>0)
        stop(paste0("The following dimension(s) are present in 'from.dim.names', but missing from 'to.dim.names': ",
                    paste0("'", missing.from.to.dimensions, "'", collapse=', ')))
    
    
    if (!all(names(from.dim.names)==names(to.dim.names)))
        stop("Dimensions are NOT in the same order in 'from.dim.names' and 'to.dim.names")
    
    dimensions = names(from.dim.names)
    
    dimensions.equal.in.to.from = sapply(dimensions, function(dim){
        length(from.dim.names[[dim]]) == length(to.dim.names[[dim]]) &&
        all(from.dim.names[[dim]] == to.dim.names[[dim]])
    })

    if (all(dimensions.equal.in.to.from))
        return (create.trivial.ontology.mapping())

    differing.dimensions = dimensions[!dimensions.equal.in.to.from]
    
    # First, iterate through the ontology mappings we have registered, and see if any of them match
    for (mapping in ONTOLOGY.MANAGER$mappings)
    {
        if (ontology.mapping.matches.dim.names(mapping,
                                               dimensions = differing.dimensions,
                                               from.dim.names = from.dim.names,
                                               to.dim.names = from.dim.names))
            return (mapping)
    }
    
    # If no registered mapping matches, see if we can figure out what is being requested
    if (length(differing.dimensions)==1)
    {
        dim = differing.dimensions
        from.values = from.dim.names[[dim]]
        to.values = to.dim.names[[dim]]
        
        # If only age is different, get an age mapping
        if (dim=='age')
            stop("Need to implement this")
        
        # If only year is different, get a year mapping
        if (dim=='year') 
            stop("Need to implement this")
        
        # Can we plug in an 'other-catchall' mapping?
        if (any(to.values == 'other') &&
            !any(from.values == 'other'))
        {
            missing.from.from = setdiff(from.values, to.values)
            if (length(missing.from.from)==1 && missing.from.from=='other')
            {
                non.other.names.in.to = setdiff(to.values, 'other')
                invalid.to.names = setdiff(non.other.names.in.to, from.values)
                if (length(invalid.to.names)==0)
                {
                    return (create.other.catchall.ontology.mapping(dimension = dim,
                                                                   from.values = from.values,
                                                                   to.values = to.values))
                }
            }
        }
    }
}

ontology.mapping.matches.dim.names <- function(mapping,
                                               dimensions,
                                               from.dim.names,
                                               to.dim.names)
{
    length(mapping$dimensions) == length(dimensions) &&
        setequal(mapping$dimensions, dimensions) &&
        all(sapply(dimensions, function(dim){
            length(from.dim.names[[dim]]) == length(mapping$from[[dim]]) &&
                all(from.dim.names[[dim]] == mapping$from[[dim]]) &&
                length(to.dim.names[[dim]]) == length(mapping$to[[dim]]) &&
                all(to.dim.names[[dim]] == mapping$to[[dim]])
        }))
        
}