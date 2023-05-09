
## ***OVERVIEW***
##
## See ONTOLOGY_ontology.R for description and implementation of what an ontology is
## 
## An ontology.mapping comprises the information for how to convert data that accords to one ontology
##  into data that accords to a different ontology.
## For example, if ontology A partitions race as "black", "white", "hispanic", "aapi", "other"
##  and ontology B partitions race as "black", "hispanic", "other"
##  a mapping from A to B would know that "black" in B = "black" in A, "hispanic" in B = "hispanic" in A,
##  and "other" in B = "white" + "aapi" + "other" in A
## 
## We implement the mappings as R6 objects that store the information on how to convert as member variables,
##  and also bundle in methods to execute conversions
##  
## While ontology.mappings can be created and applied directly, we also include functionality to
##  "register" mappings - this means that they are globally accessible and can be searched.
## This allows some client that knows it has data in one ontology to request a mapping to a different
##  ontology without having to track those mappings itself
## Note: we may also encounter a situation where we have some data in ontology A and some other data
##  in ontology B that we would like to use together, but have no mapping from either A -> B or B -> A. 
##  However, we may have mappings from both A and B to a 3rd ontology (call it C): A -> C and B -> C,
##  that would let us use the data together. We refer to this as finding a set of mappings that
##  "aligns" data from ontology A with data from ontology B.
##  (Note that, since we can consider no change to an ontology as an "identity" ontology mapping, aligning
##   A and B could be satisfied if we have either a mapping A -> B or B -> A)

##----------------------##
##----------------------##
##-- PUBLIC INTERFACE --##
##----------------------##
##----------------------##

#'@description Create a mapping that allows transformations of arrays
#'
#'@param from.dimensions A character vector of names of the dimensions we are mapping from
#'@param to.dimensions A character vector of names of the dimensions we are mapping to
#'@param mappings A character matrix, with a number of columns == length(from.dimensions) + length(to.dimensions). The first length(from.dimensions) columns represent values for the from.dimensions (in the same order as from.dimensions), and the last length(to.dimensions) colums represent values for to.dimensions (in the order of to.dimensions). Each row represents a mapping from a set of values for from.dimension to a set of values for to.dimensions. NB: any dimnames on mappings will be ignored
#'@param error.prefix A text string to prepend to any error messages generated in executing the function
#'
#'@export
create.ontology.mapping <- function(mappings,
                                    from.dimensions,
                                    to.dimensions=from.dimensions,
                                    name = NULL,
                                    error.prefix = "Error creating ontology mapping: ")
{
    #-- Validate the error prefix --#
    if (!is.character(error.prefix) || length(error.prefix)!=1 || is.na(error.prefix) || nchar(error.prefix)==0)
        stop("Error creating ontology mapping: 'error.prefix' must be a single, non-NA, non-empty character value")
        
    #-- Validate the name --#
    if (!is.null(name) && (!is.character(name) || length(name)!=1 || is.na(name) || nchar(name)==0))
        stop(paste0(error.prefix), "If it is not NULL, 'name' must be a non-empty, non-NA single character value")
    
    #-- Validate from.dimensions --#
    if (!is.character(from.dimensions) || length(from.dimensions)==0)
        stop(paste0(error.prefix, "'from.dimensions' must be a non-empty character vector"))
    if (any(is.na(from.dimensions)))
        stop(paste0(error.prefix, "'from.dimensions' cannot contain NA values"))
    if (any(nchar(from.dimensions)==0))
        stop(paste0(error.prefix, "'from.dimensions' cannot contain empty ('') values"))
    
    n.from.dimensions = length(from.dimensions)
    
    #-- Validate to.dimensions --#
    if (!is.character(to.dimensions) || length(to.dimensions)==0)
        stop(paste0(error.prefix, "'to.dimensions' must be a non-empty character vector"))
    if (any(is.na(to.dimensions)))
        stop(paste0(error.prefix, "'to.dimensions' cannot contain NA values"))
    if (any(nchar(to.dimensions)==0))
        stop(paste0(error.prefix, "'to.dimensions' cannot contain empty ('') values"))
    
    n.to.dimensions = length(to.dimensions)
    
    
    #-- Validate dimensions on mappings --#
    
    if (is.null(dim(mappings)) || length(dim(mappings))!=2)
        stop(paste0(error.prefix, "'mappings' must be a two-dimensional data structure (eg, matrix or data frame)"))
    
    if (dim(mappings)[1]==0)
        stop(paste0(error.prefix, "'mappings' cannot be empty"))
    
    if (dim(mappings)[2] != (n.from.dimensions+n.to.dimensions))
        stop(paste0(error.prefix, "'mappings' must have ", 
                    (n.from.dimensions+n.to.dimensions),
                    " columns (one for each dimension in from.dimensions and to.dimensions)"))
    
    if (!all(apply(mappings, 2, is.character)))
        stop(paste0(error.prefix, "The columns of 'mappings' must all be character vectors"))
    
    mappings = as.matrix(mappings)
    n.values = dim(mappings)[1]
    
    from.values = mappings[,1:n.from.dimensions, drop=F]
    dimnames(from.values)[[2]] = from.dimensions
    
    to.values = mappings[,n.from.dimensions + 1:n.to.dimensions, drop=F]
    dimnames(to.values)[[2]] = to.dimensions
    
    #-- Check for rows with all NAs - not allowed --#
    all.na.rows = apply(is.na(mappings), 1, all)
    if (any(all.na.rows))
        stop(paste0(error.prefix,
                    "The ",
                    collapse.with.and(get.ordinal((1:n.values)[all.na.rows])),
                    ifelse(sum(all.na.rows)==1, ' row contains', ' rows contain'),
                    " only NA values"))
    
    
    #-- Check for NAs in the from.values - allowed ONLY if all values in a row are NA --#
    #   (And at least some values have to be non-NA)
    if (n.from.dimensions>1)
    {
        invalid.na.mask = apply(is.na(from.values), 1, any) &
            !apply(is.na(from.values), 1, all)
        
        if (any(invalid.na.mask))
            stop(paste0(error.prefix,
                        "Rows representing 'from' values in 'mappings' (in the 1st",
                        "-", get.ordinal(n.from.dimensions),
                        " columns) cannot contain NA values unless all 'from' values in the row are NA."))
    }
    
    if (all(is.na(from.values)))
        stop(paste0(error.prefix,
                    "The column",
                    ifelse(n.from.dimensions==1, '', 's'),
                    " representing 'to' values in 'mappings' (the 1st",
                    ifelse(n.from.dimensions==1, '',
                           paste0("-", get.ordinal(n.from.dimensions))),
                    ") cannot contain *ALL* NA values"))
    
    #-- Check for NAs in the to.values - allowed ONLY if all values in a row are NA --#
    if (n.to.dimensions>1)
    {
        invalid.na.mask = apply(is.na(to.values), 1, any) &
            !apply(is.na(from.values), 1, all)
        
        if (any(invalid.na.mask))
            stop(paste0(error.prefix,
                        "Rows representing 'to' values in 'mappings' (in the ",
                        get.ordinal(n.from.dimensions+1),
                        "-", get.ordinal(n.from.dimensions+n.to.dimensions),
                        " columns) cannot contain NA values unless all 'to' values in the row are NA."))
    }
    
    if (all(is.na(to.values)))
        stop(paste0(error.prefix,
                    "The column",
                    ifelse(n.to.dimensions==1, '', 's'),
                    " representing 'to' values in 'mappings' (the ",
                    get.ordinal(n.from.dimensions+1),
                    ifelse(n.to.dimensions==1, '',
                           paste0("-", get.ordinal(n.from.dimensions+n.to.dimensions))),
                    ") cannot contain *ALL* NA values"))
    
    
    
    #-- Make sure every combination of (non-NA) from values is used --#
    
    no.from.na.mask = !apply(is.na(from.values), 1, all)
    num.unique.non.na.from.combos = dim(unique(from.values[no.from.na.mask,,drop=F]))[1]
    
    unique.non.na.from.values = lapply(1:n.from.dimensions, function(i){
        unique(from.values[!is.na(from.values[,i]), i])
    })
    n.possible.from.combos = prod(sapply(unique.non.na.from.values, length))
    if (n.from.dimensions>1 &&
        num.unique.non.na.from.combos < n.possible.from.combos) 
    {
        all.from.combos = get.every.combination(unique.non.na.from.values)
        missing.from.combos = setdiff.rows(all.from.combos,
                                           from.values[no.from.na.mask,,drop=F])
        
        stop(paste0(error.prefix,
                    "Rows representing 'from' values in 'mappings' (in the 1st",
                    "-", get.ordinal(n.from.dimensions),
                    " columns) must contain every possible combination of from values. The following combination",
                    ifelse(dim(missing.from.combos)[1]==1, " is", "s are"),
                    " missing: ",
                    collapse.with.and("<",
                                      apply(missing.from.combos, 1, function(vals){
                                          paste0("'", vals, "'", collapse=',')
                                      }),
                                      ">")
        ))
    }
    
    
    #-- Pad NAs to fill out every combination of 'to' values --#
    
    no.to.na.mask = !apply(is.na(to.values), 1, all)
    num.unique.non.na.to.combos = dim(unique(to.values[no.to.na.mask,,drop=F]))[1]
    
    unique.non.na.to.values = lapply(1:n.to.dimensions, function(i){
        unique(to.values[!is.na(to.values[,i]), i])
    })
    n.possible.to.combos = prod(sapply(unique.non.na.to.values, length))
    if (n.to.dimensions>1 && 
        num.unique.non.na.to.combos < n.possible.to.combos)
    {
        all.to.combos = get.every.combination(unique.non.na.to.values)
        missing.to.combos = setdiff.rows(all.to.combos, to.values)
        
        from.values = rbind(from.values,
                            matrix(NA, nrow=dim(missing.to.combos)[1], ncol=n.from.dimensions))
        to.values = rbind(to.values, missing.to.combos)
    }
    
    #-- Make the mapping --#
    
    BASIC.ONTOLOGY.MAPPING$new(name=name,
                                         from.dimensions=from.dimensions,
                                         to.dimensions=to.dimensions,
                                         from.values=from.values,
                                         to.values=to.values)
}


#'@description Create and register a mapping that allows transformations of arrays
#'
#'@param name A descriptive single character value
#'@inheritParams create.ontology.mapping
#'
#'@details The mappings must contain every possible combination of 'from' values (ie, every combo of from values must map to something). A row of 'mappings' may have NA values in ALL the 'from' columns (indicating there is no set of 'from' values that maps to the corresponding 'to' values), or may have NA values in ALL the 'to' columns (indicating the that the given set of 'from' values will be discarded in the transformation)
#'
#'@export
register.ontology.mapping <- function(name,
                                      mappings,
                                      from.dimensions,
                                      to.dimensions=from.dimensions)
{
    #-- Validate the Name --#
    if (!is.character(name) || length(name)!=1 || is.na(name) || nchar(name)==0)
        stop("Error registering ontology mapping: 'name' must be a single, non-NA, non-empty character value")
    
    error.prefix = paste0("Error registering ontology mapping '", name, "': ")
    mapping = create.ontology.mapping(mappings=mappings,
                                      from.dimension=from.dimensions,
                                      to.dimensions=to.dimensions,
                                      name = name,
                                      error.prefix=error.prefix)
    
    ##??? Do we need to parse out subsets of mappings and register those if there are multiple dimensions? ???##
    
    #-- Register the mapping --#
    #   (Only if it has not been registered previously)
    
    equals.mapping = sapply(ONTOLOGY.MAPPING.MANAGER$mappings, function(other.mapping){
        mapping$equals(other.mapping)
    })
    
    if (any(equals.mapping))
    {
        other.name = ONTOLOGY.MAPPING.MANAGER$mappings[equals.mapping][[1]]$name
        if (other.name!=name)
            stop(paste0(error.prefix, "This ontology mapping has already been registered under a different name ('",
                        other.name, "')"))
        #else, just ignore - it's already registered with the same name
    }
    else
    {
        if (any(names(ONTOLOGY.MAPPING.MANAGER$mappings)==name))
            stop(paste0(error.prefix, "A different ontology mapping has already been registered under the name '",
                        name, "'"))
        else
            ONTOLOGY.MAPPING.MANAGER$mappings[[name]] = mapping
    }
    
    
    #-- If this is a reversible mapping, reverse it and register it --#
    
    from.values = mapping$from.values
    to.values = mapping$to.values
    
    # it's reversible if it's a one-to-one, complete mapping
    is.reversible = !any(is.na(from.values)) &&
        dim(unique(from.values))[1] == n.values &&
        dim(unique(to.values))[1] == n.values #ie, there are no duplicate to value combos
    
    if (is.reversible)
    {
        reverse.mapping = BASIC.ONTOLOGY.MAPPING$new(name=paste0(name, " (reversed)"),
                                                     from.dimensions=to.dimensions,
                                                     to.dimensions=from.dimensions,
                                                     from.values=to.values,
                                                     to.values=from.values)
        
        
        equals.reverse.mapping = sapply(ONTOLOGY.MAPPING.MANAGER$mappings, function(other.mapping){
            reverse.mapping$equals(other.mapping)
        })
        
        if (!any(equals.reverse.mapping))
        {
            if (any(names(ONTOLOGY.MAPPING.MANAGER$mappings)==reverse.mapping$name))
                stop(paste0(error.prefix, "A different ontology mapping has already been registered under the name '",
                            name, "' that we sought to use for the reverse of this mapping"))
            else
                ONTOLOGY.MAPPING.MANAGER$mappings[[reverse.mapping$name]] = reverse.mapping
        }
        #else just ignore - it's already registered 
    }
    
    #-- (Invisibly) Return the Mapping --#
    invisible(mapping)
}


#'@description Get an Ontology Mapping to Transform Data
#'
#'@param from.ontology The ontology to which the data to be transformed conform. Must be an 'ontology' object as created by \code{\link{ontology}}
#'@param to.ontology The ontology to which the transformed data should conform. Must be an 'ontology' object as created by \code{\link{ontology}}
#'
#'@return Either an object of class 'ontology.mapping' or NULL if no mapping that would bridge the differences is found
#'@export
get.ontology.mapping <- function(from.ontology,
                                 to.ontology)
{
    #-- Validate Arguments --#
    from.ontology = derive.ontology(from.ontology, var.name.for.error = "'from.ontology'", error.prefix = "Error in get.ontology.mapping(): ")
    to.ontology = derive.ontology(to.ontology, var.name.for.error = "'to.ontology'", error.prefix = "Error in get.ontology.mapping(): ")
    
    #-- Call the sub-function --#
    mappings = do.get.ontology.mapping(from.dim.names = from.ontology,
                                       to.dim.names = to.ontology,
                                       required.dimensions = names(to.ontology),
                                       required.dim.names = NULL,
                                       from.dimensions.are.complete = is_complete(from.ontology),
                                       to.dimensions.are.complete = is_complete(to.ontology),
                                       get.two.way.alignment = F)
    
    #-- Package up and return --#
    combine.ontology.mappings(mappings[[1]])
}

#'@description Get a Pair of Ontology Mappings that Aligns two Data Elements
#'
#'@param ontology.1,ontology.2 The ontologies of two data elements to be transformed
#'@param align.on.dimensions The dimensions which should match in the transformed data
#'@param include.dim.names Optional argument, specifying specific dimension values that must be present in the transformed data
#'
#'@return Either a (1) NULL - if no mappings could align the ontologies or (2) a list with two elements, each an ontology.mapping object, such that applying the first to data conforming to ontology.1 and applying the second to data conforming to ontology.2 yields two data objects with the same dim.names
#'
#'@export
get.mappings.to.align.ontologies <- function(ontology.1,
                                             ontology.2,
                                             align.on.dimensions = intersect(names(ontology.1), names(ontology.2)),
                                             include.dim.names = NULL)
{
    #-- Validate Arguments --#
    ontology.1 = derive.ontology(ontology.1, var.name.for.error = "'ontology.1'", error.prefix = "Error getting aligning ontology mappings: ")
    ontology.2 = derive.ontology(ontology.2, var.name.for.error = "'ontology.2'", error.prefix = "Error getting aligning ontology mappings: ")
    
    #-- Call the sub-function --#
    mappings = do.get.ontology.mapping(from.dim.names = ontology.1,
                                       to.dim.names = ontology.2,
                                       required.dimensions = align.on.dimensions,
                                       required.dim.names = include.dim.names,
                                       from.dimensions.are.complete = is_complete(ontology.1),
                                       to.dimensions.are.complete = is_complete(ontology.2),
                                       get.two.way.alignment = T)
    
    #-- Package up and return --#
    if (is.null(mappings))
        NULL
    else
        list(mapping.from.1 = combine.ontology.mappings(mappings[[1]]),
             mapping.from.2 = combine.ontology.mappings(mappings[[2]]) )
}

#'@description Get a Matrix that Applies an Ontology Mapping
#'
#'@inheritParams apply.ontology.mapping
#'
#'@export
get.ontology.mapping.matrix <- function(ontology.mapping,
                                        from.dim.names,
                                        to.dim.names=NULL,
                                        error.prefix='')
{
    if (!is(ontology.mapping, 'ontology.mapping'))
        stop("'ontology.mapping' must be an object of class 'ontology.mapping'")
    
    ontology.mapping$get.matrix(from.dim.names=from.dim.names,
                                to.dim.names=to.dim.names,
                                error.prefix=error.prefix)
}

#'@description Apply an Ontology Mapping to Data
#'
#'@param ontology.mapping An object of class 'ontology.mapping' - obtained either through \link{register.ontology.mapping}, \link{get.ontology.mapping}, \link{get.mappings.to.align.ontologies}
#'@param from.arr An array to transform. Can have any underlying data type, although the function is optimized for numeric data
#'@param to.dim.names The dimnames that the resulting transformed data should have (can represent a subset of the dimnames that the ontology mapping knows how to transform). If NULL, the ontology mapping will keep the dimnames in from.arr except for dimensions that are transformed
#'@param fun A function used to combine values, or the name of a function. The function must be able to accept any number of arguments (including none). The default, 'sum', is optimized for numeric arrays
#'@param na.rm Passed through to fun
#'@param error.prefix A character value to be prepended to any error messages thrown by the function
#'
#'@export
apply.ontology.mapping <- function(ontology.mapping,
                                   from.arr,
                                   to.dim.names=NULL,
                                   fun='sum',
                                   na.rm=F,
                                   error.prefix='')
{
    if (!is(ontology.mapping, 'ontology.mapping'))
        stop("'ontology.mapping' must be an object of class 'ontology.mapping'")
    
    ontology.mapping$apply(from.arr = from.arr, 
                           to.dim.names = to.dim.names,
                           fun = fun,
                           error.prefix = error.prefix)
}


##-------------##
##-------------##
##-- HELPERS --##
##-------------##
##-------------##


# To Note: 
#  A "complete" dimension means that the dimension's values comprise all possible values
#       This implies that the dimension can be summed over
#       A particular value (eg "other") may have different meanings in different complete dimensions, depending
#           on what else is in the dimension (eg, in 'black','hispanic','other' vs 'black','hispanic','white','other')
#       An ontology mapping to a complete dimension can only apply if the ontology mapping produces exactly the
#           set of values for that dimension (ie, extra values are not permitted)
#       An ontology mapping from a complete dimension can only apply if the ontology mapping takes exactly the
#           set of values for that dimension
#       Applying an ontology mapping yields complete dimensions if all the from dimensions are complete
#  An "incomplete" dimension means that the dimension's values may not be all possible values for the dimension
#       This implies that the dimension CANNOT be summed over
#       A particular value for an incomplete dimension is presumed to always have the same value
#       An ontology mapping to an incomplete dimension applies so long as the set of values it
#           produces are a superset of the values for the dimension (ie, extra values are permitted)
#       An ontology mapping from an incomplete dimension applies so long as the set of values it takes are a
#           subset of the values present
#       Applying an ontology mapping yields incomplete dimensions if any of the from dimensions are incomplete

#'@param from.dim.names,to.dim.names The dimnames of the from and to arrays
#'@param required.dimensions The dimensions that should be present in the aligned product
#'@param required.dim.names Any specific dimension values that should be present in the aligned product
#'@param get.two.way.alignment Logical indicating whether we need one mapping, that maps from.dim.names such that
#'
#'@return If get.two.way.alignment is false, returns a list of one or more ontology.mapping objects,
#'          such that applying these mappings to an array with from.dim.names ('from.arr')
#'          would yield an array with dimnames ('to.dim.names')
#'          
#'        If get.two.way.alignment is true, returns a list with two elements,
#'          each of which is itself a list of one or more ontology.mapping objects, such that
#'          applying the first set of mappings to an array with from.dimnames
#'          and applying the second set of mappings to an array with to.dimnames
#'          would both yield arrays with dimensions 'align.on.dimensions', both with
#'          the same dimnames
do.get.ontology.mapping <- function(from.dim.names,
                                    to.dim.names,
                                    required.dimensions,
                                    required.dim.names,
                                    from.dimensions.are.complete,
                                    to.dimensions.are.complete,
                                    get.two.way.alignment,
                                    mappings = c(ONTOLOGY.MAPPING.MANAGER$mappings,
                                                 list('age','other')))
{
    error.prefix = "Error getting ontology mapping: "
    
    # We are done (successfully) iff
    # 1) to.dim.names contains only required.dimensions and no others
    # 2) from.dim.names contains all required dimensions
    # 3) Any dimensions in from.dim.names beyond required.dimensions are complete
    # 4) All required.dimensions which are complete have equal values in from.dim.names and to.dim.names
    # 5) All required.dimensions which are incomplete have at least one value present in both from.dim.names and to.dim.names
    # 6) All required.dim.names are present in to.dim.names (if this is not true, give up)
  
    # check (6) - if not met, we will never succeed
    if (!dim.names.are.subset(sub.dim.names=required.dim.names, super.dim.names=to.dim.names))
        return (NULL)
    
    to.has.only.required = setequal(names(to.dim.names), required.dimensions)
    from.has.all.required = length(setdiff(required.dimensions, names(from.dim.names)))==0
    
    # satisfies condition (3)
    excess.from.dimensions = setdiff(names(from.dim.names), required.dimensions)
    excess.from.dimensions.are.complete = all(from.dimensions.are.complete[excess.from.dimensions])
    
    # below satisfies 
    from.out.of.alignment.mask = !sapply(required.dimensions, function(d){
        
        if (to.dimensions.are.complete[d])
        {
            !is.null(from.dim.names[[d]]) && # satisfies part of condition (1)
                !is.null(to.dim.names[[d]]) && # satisfies condition (2)
                setequal(from.dim.names[[d]], to.dim.names[[d]]) # satisfies condition (4)
            
            # do we want to separate for if from is incomplete - then to just has to be a subset of from?
        }
        else if (from.dimensions.are.complete[d])
        {
            is.null(to.dim.names[[d]]) || # if it's NULL, we'll take anything
                (!is.null(from.dim.names[[d]]) && 
                     length(setdiff(to.dim.names[[d]], from.dim.names[[d]]))>0)
        }
        else
        {
            is.null(to.dim.names[[d]]) || # if it's NULL, we'll take anything
                (!is.null(from.dim.names[[d]]) && 
                     length(intersect(from.dim.names[[d]], to.dim.names[[d]]))>0)
        }
    })
    
    success = to.has.only.required && from.has.all.required && excess.from.dimensions.are.complete &&
        !any(from.out.of.alignment.mask)
    
    if (success)
    {
        if (get.two.way.alignment)
            return (list(from=list(IDENTITY.MAPPING), to=list(IDENTITY.MAPPING)))
        else
            return (list(from=list(IDENTITY.MAPPING), to=NULL))
    }
    
    #we can only modify to.dim.names by reversing when get.two.way.alignment==T
    if (!get.two.way.alignment && length(setdiff(required.dimensions, names(to.dim.names)))>0) 
        return (NULL)

    dimensions.out.of.alignment = required.dimensions[from.out.of.alignment.mask]
    complete.dimensions=names(from.dimensions.are.complete)[from.dimensions.are.complete]
    
    
    for (i in 1:length(mappings))
    {
        mapping = mappings[[i]]
        mappings.to.try.next = mappings
        
        if (is.character(mapping) && mapping=='age')
        {
            if (any(dimensions.out.of.alignment == 'age') &&
                !is.null(from.dim.names[['age']]) &&
                !is.null(to.dim.names[['age']]))
            {
                mapping = create.age.ontology.mapping(from.values=from.dim.names[['age']],
                                                      to.values=to.dim.names[['age']],
                                                      allow.incomplete.span.of.infinite.age.range = T)
                
                if (is.null(mapping))
                    try.mapping = F
                else
                {
                    try.mapping=T
                    mappings.to.try.next = mappings[-i]                
                }
            }
            else
                try.mapping = F
        }
        else if (is.character(mapping) && mapping=='other')
        {
            try.mapping = F
            for (d in dimensions.out.of.alignment)
            {
                if (other.catchall.mapping.applies(from.values=from.dim.names[[d]],
                                                   to.values=to.dim.names[[d]]))
                {
                    mapping = create.other.catchall.ontology.mapping(dimension=d,
                                                                     from.values=from.dim.names[[d]],
                                                                     to.values=to.dim.names[[d]])
                    try.mapping = T
                    break;
                }
            }
        }
        else
        {
            try.mapping = mapping$can.apply.to.dim.names(from.dim.names,
                                                         complete.dimensions = complete.dimensions,
                                                         error.prefix=error.prefix)
            
            mappings.to.try.next = mappings[-i]
        }
        
        # If we can apply the mapping
        #   (ie if all the mapping$from.dim.names are in the from.dim.names)
        # Try applying it
        if (try.mapping)
        {
            # Update the dim.names if we were to apply this mapping
            post.mapping.from.dim.names = mapping$apply.to.dim.names(from.dim.names,
                                                                     complete.dimensions = complete.dimensions,
                                                                     error.prefix=error.prefix)
            
            # Update the from.dimensions.are.complete if we were to apply this mapping
            post.mapping.from.dimensions.are.complete = from.dimensions.are.complete
            post.mapping.from.dimensions.are.complete[mapping$to.dimensions] = all(from.dimensions.are.complete[mapping$from.dimensions])
                # I am not sure the above line is complete, but I can't think of a scenario in which the from
                # would not be all complete or all incomplete
            
            # Recurse
            additional.mappings = do.get.ontology.mapping(from.dim.names=post.mapping.from.dim.names,
                                                          to.dim.names=to.dim.names,
                                                          required.dimensions=required.dimensions,
                                                          required.dim.names=required.dim.names,
                                                          from.dimensions.are.complete = post.mapping.from.dimensions.are.complete,
                                                          to.dimensions.are.complete = to.dimensions.are.complete,
                                                          get.two.way.alignment=get.two.way.alignment,
                                                          mappings = mappings.to.try.next)
            
            if (!is.null(additional.mappings)) # we succeeded and we're done! Append and go home
            {
                return(list(from=c(list(mapping), additional.mappings$from),
                            to=additional.mappings$to))
            }
        }
    }
    
    # We got all the way to the the end and could not apply any mappings
    # If we need to go two ways, then try to map the other way
    # Otherwise, give up and return NULL
    
    if (get.two.way.alignment)
    {
        reverse.mappings = do.get.ontology.mapping(from.dim.names=to.dim.names,
                                                   to.dim.names=from.dim.names,
                                                   from.dimensions.are.complete = to.dimensions.are.complete,
                                                   to.dimensions.are.complete = from.dimensions.are.complete,
                                                   required.dimensions=required.dimensions,
                                                   required.dim.names=required.dim.names,
                                                   get.two.way.alignment=F) #leave off mappings to reset to the default
        
        if (is.null(reverse.mappings)) # We couldn't make it work
            NULL
        else # this works! package it up and return
            list(from=list(IDENTITY.MAPPING),
                 to=reverse.mappings$from)
    }
    else
        NULL
}

#'@description Combine multiple ontology mappings into a single mapping
#'
#'@param ... Either ontology.mapping objects or lists containing ontology.mapping objects#'
#'
#'@export
combine.ontology.mappings <- function(...)
{
    args = list(...)
    args = args[!sapply(args, is.null)]
    if (length(args)==0)
        return (NULL)
    
    sub.mappings = list()
    for (elem in args)
    {
        if (is(elem, 'combination.ontology.mapping'))
            sub.mappings = c(sub.mappings, elem$sub.mappings)
        else if (is(elem, 'ontology.mapping'))
            sub.mappings = c(sub.mappings, list(elem))
        else if (is.list(elem))
        {
            for (sub.elem in elem)
            {
                if (is(sub.elem, 'combination.ontology.mapping'))
                    sub.mappings = c(sub.mappings, sub.elem$sub.mappings)
                else if (is(sub.elem, 'ontology.mapping'))
                    sub.mappings = c(sub.mappings, list(sub.elem))
                else
                    stop("Cannot create combination ontology mapping: ... must contain ONLY ontology.mapping objects or lists of ontology.mapping objects")
            }
        }
        else
            stop("Cannot create combination ontology mapping: ... must contain ONLY ontology.mapping objects or lists of ontology.mapping objects")
    }
    
    if (length(sub.mappings)==0)
        return (NULL)
    
    identity.mapping.mask = sapply(sub.mappings, function(m){m$is.identity.mapping})
    if (all(identity.mapping.mask))
        IDENTITY.MAPPING
    else if (sum(!identity.mapping.mask)==1)
        sub.mappings[!identity.mapping.mask][[1]]
    else
        COMBINATION.ONTOLOGY.MAPPING$new(name = paste0(sum(!identity.mapping.mask), "-mapping combo"),
                                         sub.mappings=sub.mappings[!identity.mapping.mask])
}

#'@description Create an ontology mapping that lumps otherwise unspecified categories into an "other" category
#'
#'@param dimension A single character value indicating what dimension this mapping applies to
#'@param from.values The possible values for the dimension in the ontology to map from
#'@param to.values The possible values for the dimension in the ontology to map to
#'@param other.category.value What the "other" category should be called in the to ontology
#'
#'@export
create.other.catchall.ontology.mapping <- function(dimension,
                                                   from.values,
                                                   to.values,
                                                   other.category.value='other')
{
    #-- Sanitize arguments --#
    if (!is.character(dimension))
        stop("In create.other.catchall.ontology.mapping(), 'dimension' must be a single character value")
    if (length(dimension) != 1)
        stop("In create.other.catchall.ontology.mapping(), 'dimension' must be a SINGLE character value")
    if (is.na(dimension))
        stop("In create.other.catchall.ontology.mapping(), 'dimension' cannot be NA")
    
    if (!is.character(from.values))
        stop("In create.other.catchall.ontology.mapping(), 'from.values' must be a character vector")
    if (length(from.values)==0)
        stop("In create.other.catchall.ontology.mapping(), 'from.values' cannot be an empty vector")
    if (any(is.na(from.values)))
        stop("In create.other.catchall.ontology.mapping(), 'from.values' cannot contain NA values")
    
    if (!is.character(to.values))
        stop("In create.other.catchall.ontology.mapping(), 'to.values' must be a character vector")
    if (length(to.values)==0)
        stop("In create.other.catchall.ontology.mapping(), 'to.values' cannot be an empty vector")
    if (any(is.na(to.values)))
        stop("In create.other.catchall.ontology.mapping(), 'to.values' cannot contain NA values")
    
    if (!is.character(other.category.value))
        stop("In create.other.catchall.ontology.mapping(), 'other.category.value' must be a single character value")
    if (length(other.category.value) != 1)
        stop("In create.other.catchall.ontology.mapping(), 'other.category.value' must be a SINGLE character value")
    if (is.na(other.category.value))
        stop("In create.other.catchall.ontology.mapping(), 'other.category.value' cannot be NA")
    
    missing.from.from = setdiff(to.values, c(from.values, other.category.value))
    if (length(missing.from.from)>0)
        stop(paste0("In create.other.catchall.ontology.mapping(), no from.values will map to ",
                    collapse.with.or("'", missing.from.from, "'"), " (in to.values)"))
    
    
    #-- Do the work --#
    missing.from.to = setdiff(from.values, c(to.values, other.category.value))
    if (length(missing.from.to)==0)
        stop("You have tried to construct an 'other-catchall' mapping using create.other.catchall.ontology.mapping(), but none of the from.values will map to other")
    
    new.to.values = from.values
    names(new.to.values) = from.values
    new.to.values[missing.from.to] = other.category.value
    dim(new.to.values) = dim(from.values) = c(length(from.values),1)
    
    BASIC.ONTOLOGY.MAPPING$new(name=paste0(dimension, "/", other.category.value),
                               from.dimensions=dimension,
                               to.dimensions=dimension,
                               from.values=from.values,
                               to.values=new.to.values)
}

other.catchall.mapping.applies <- function(from.values, to.values)
{
    !is.null(from.values) && !is.null(to.values) &&
        any(to.values=='other') &&
        length(setdiff(to.values, c(from.values, 'other'))) == 0 &&
        length(setdiff(from.values, to.values)) > 0
}

# If x is an ontology, just returns x
# If x is a named list of character vectors, converts to an ontology
# else throws an error
derive.ontology <- function(x,
                            var.name.for.error,
                            error.prefix)
{
    if (is.ontology(x))
        x
    else if (is.list(x))
    {
        if (is.null(names(x)))
            stop(paste(error.prefix, "If ", var.name.for.error, " is a list, it must be NAMED"))
        if (any(!sapply(x, is.character)))
            stop(paste(error.prefix, "If ", var.name.for.error, " is a list, it must contain only character vectors"))
        
        as.ontology(x)
    }
    else
        stop(paste(error.prefix, var.name.for.error, " must be either (1) an object of class 'ontology' as created by the ontology() function or (2) a named list of character vectors"))
}

##------------------##
##------------------##
##-- AGE MAPPINGS --##
##------------------##
##------------------##

# Fundamentally we can do this iff
# For every 'to' value, the 'from' values within each to value completely span that to value's range
#     (unless the upper bound is infinity and allow.incomplete.span.of.infinite.age.range==T)
create.age.ontology.mapping <- function(from.values,
                                        to.values,
                                        allow.incomplete.span.of.infinite.age.range)
{
    from.bounds = parse.age.strata.names(from.values)
    to.bounds = parse.age.strata.names(to.values)
    
    if (is.null(from.bounds) || is.null(to.bounds))
        return (NULL)
    
    # Get the 'from's for each 'to'
    # NB: A 'from' may map to more than one 'to' if the 'to's are overlapping
    
    froms.for.to = lapply(1:length(to.values), function(to){
        mask = from.bounds$lower >= to.bounds$lower[to] &
            from.bounds$upper <= to.bounds$upper[to]
        
        if (sum(mask)==0)
            NULL
        else
            (1:length(mask))[mask]
    })
    
    # If a 'to' has no 'froms' - give up
    if (any(sapply(froms.for.to, is.null)))
        return (NULL)
    
    # Make sure the 'from's for each 'to' span the 'to' completely
    froms.span.tos = sapply(1:length(to.values), function(to){
        from.lowers = from.bounds$lower[froms.for.to[[to]]]
        from.uppers = from.bounds$upper[froms.for.to[[to]]]
        o = order(from.lowers)
        from.lowers = from.lowers[o]
        from.uppers = from.uppers[o]
        
        # from's span to the bottom
        from.lowers[1] == to.bounds$lower[to] && 
            # from's span to the top or the top is infinity
            ( from.uppers[length(from.uppers)] == to.bounds$upper[to] ||
                  (allow.incomplete.span.of.infinite.age.range
                   && is.infinite(to.bounds$upper[to])) ) && 
            # from's are contiguous
            all( from.lowers[-1] == from.uppers[-length(from.uppers)] ) 
    })
    
    if (all(froms.span.tos))
    {
        iterated.from.values = from.values[unlist(froms.for.to)]
        iterated.to.values = unlist(sapply(1:length(to.values), function(i){
            rep(to.values[i], length(froms.for.to[[i]]))
        }))
        
        missing.from.values = setdiff(from.values, iterated.from.values)
        iterated.from.values = c(iterated.from.values, missing.from.values)
        iterated.to.values = c(iterated.to.values, rep(NA, length(missing.from.values)))
        
        BASIC.ONTOLOGY.MAPPING$new(name = paste0('age ', length(from.values), "->", length(to.values), " strata"),
                                   from.dimensions = 'age',
                                   to.dimensions = 'age',
                                   from.values = matrix(iterated.from.values, ncol=1),
                                   to.values = matrix(iterated.to.values, ncol=1))
    }
    else
        return (NULL)
}

#'@description Make Names for a Set of Age Strata
#'
#'@param endpoints A numeric vector of at least two points. endpoints[1] is the lower bound (inclusive) of the first stratum, endpoints[2] is the upper bound (exclusive) for the first stratum and the lower bound for the second stratum, etc.
#'
#'@return A character vector with length(endpoints)-1 values
#'
#'@export
make.age.strata.names <- function(endpoints)
{
    if (!is.numeric(endpoints))
        stop("'endpoints' must be a numeric vector")
    
    if (length(endpoints)<2)
        stop("'endpoints' must contain at least two values")
    
    if (any(is.na(endpoints)))
        stop("'endpoints' cannot contain NA values")
    
    lowers = endpoints[-length(endpoints)]
    uppers = endpoints[-1]
    
    rv = paste0(lowers, "-", uppers-1, " years")
    rv[is.infinite(uppers)] = paste0(lowers[is.infinite(uppers)], "+ years")
    rv[(lowers+1)==uppers] = paste0(lowers, " years")
    rv[lowers==1 & uppers==2] = '1 year'
    
    rv
}

#'@description Convert Age Strata Names into Lower and Upper Bounds for Each Stratum
#'
#'@param strata.names Names generated in the format given by \link{make.age.strata.names}
#'
#'@return A list with two elements, $lowers and $uppers, representing the lower (inclusive) and upper (exclusive) bounds of each age stratum
#'
#'@export
parse.age.strata.names <- function(strata.names)
{
    # Validate
    if (!is.character(strata.names))
        stop("'strata.names' must be a character vector")
    
    if (length(strata.names)==0)
        stop("'strata.names' must contain at least one value")
    
    if (any(is.na(strata.names)))
        stop("'strata.names' cannot contain NA values")
    
    # Massage out text suffixes
    years.mask = grepl(" years$", strata.names) #we'll do this first, since it's the default
    strata.names[years.mask] = substr(strata.names[years.mask], 1, nchar(strata.names[years.mask])-6)
 
    if (!all(years.mask))
    {   
        one.year.mask = grepl(" year$", strata.names) #this next, since it's also used by the default
        strata.names[one.year.mask] = substr(strata.names[one.year.mask], 1, nchar(strata.names[one.year.mask])-5)
        
        if (!all(years.mask | one.year.mask))
        {
            years.old.mask = grepl(" years old$", strata.names) #this next, since it's also used by the default
            strata.names[years.old.mask] = substr(strata.names[years.old.mask], 1, nchar(strata.names[years.old.mask])-10)}
    }
    
    # Divide up the three ways to parse
    # <age>+
    # <age>-<age>
    # <age>    
    
    uppers = lowers = numeric(length(strata.names))

    dash.position = sapply(strsplit(strata.names, ''), function(chars){
        (1:length(chars))[chars=='-'][1]
    })
    infinite.upper.mask = substr(strata.names, nchar(strata.names), nchar(strata.names)) == "+"
    age.range.mask = !is.na(dash.position)
    single.age.mask = !age.range.mask & !infinite.upper.mask

    # Parse infinite upper
    lowers[infinite.upper.mask] = suppressWarnings(as.numeric(substr(strata.names[infinite.upper.mask],
                                                                     1, nchar(strata.names[infinite.upper.mask])-1)))
    uppers[infinite.upper.mask] = Inf
    
    # Parse age range
    lowers[age.range.mask] = suppressWarnings(as.numeric(substr(strata.names[age.range.mask],
                                                     1, dash.position-1)))
    uppers[age.range.mask] = 1+suppressWarnings(as.numeric(substr(strata.names[age.range.mask],
                                                                dash.position+1, nchar(strata.names[age.range.mask]))))

    
    # Parse single age
    lowers[single.age.mask] = suppressWarnings(as.numeric(strata.names[single.age.mask]))
    uppers[single.age.mask] = lowers[single.age.mask] + 1
 
    # Return
    if (any(is.na(uppers)) || any(is.na(lowers)))
        NULL
    else
        list(upper=uppers,
             lower=lowers)
}

##----------------------------------##
##----------------------------------##
##-- The ONTOLOGY MAPPING MANAGER --##
##----------------------------------##
##----------------------------------##


ONTOLOGY.MAPPING.MANAGER = new.env()
ONTOLOGY.MAPPING.MANAGER$mappings=list()


##-----------------------##
##-----------------------##
##-- CLASS DEFINITIONS --##
##-----------------------##
##-----------------------##


ONTOLOGY.MAPPING = R6::R6Class(
    'ontology.mapping',
    
    public = list(
        
        initialize = function(name)
        {
            private$i.name = name
        },
        
        print = function(...)
        {
            from.dimensions = self$from.dimensions
            to.dimensions = self$to.dimensions
            
            if (setequal(from.dimensions, to.dimensions))
                print(paste0("Ontology mapping '", self$name, "': over dimension",
                             ifelse(length(from.dimensions)==1, ' ', 's '),
                             collapse.with.and("'", from.dimensions, "'")))
            
            else
                print(paste0("Ontology mapping '", self$name, "': from dimension",
                             ifelse(length(from.dimensions)==1, '', 's'),
                             " <", collapse.with.and("'", from.dimensions, "'"), ">",
                             " to <",
                             collapse.with.and("'", to.dimensions, "'"), ">"))
        },
        
        equals = function(other)
        {
            stop("This subclass of 'ontology.mapping' is incompletely specified. The 'equals' method must be implemented at the subclass level")            
        },
        
        can.apply.to.dim.names = function(from.dim.names, 
                                          complete.dimensions=character(), 
                                          error.prefix='')
        {
            private$check.can.apply(from.dim.names=from.dim.names,
                                    to.dim.names=NULL,
                                    complete.dimensions=complete.dimensions,
                                    throw.errors=F,
                                    error.prefix=error.prefix)
        },
        
        apply.to.dim.names = function(from.dim.names, complete.dimensions=character(), error.prefix='')
        {
            private$check.can.apply(from.dim.names=from.dim.names,
                                    to.dim.names=NULL,
                                    complete.dimensions=complete.dimensions,
                                    throw.errors=T,
                                    error.prefix=error.prefix)
            
            rv = private$do.apply.to.dim.names(from.dim.names=from.dim.names,
                                               complete.dimensions=complete.dimensions,
                                               error.prefix=error.prefix)
            
            if (is(from.dim.names, 'ontology'))
            {
                incomplete.dimensions = intersect(names(rv), names(from.dim.names)[!is_complete(from.dim.names)])
                as.ontology(rv, incomplete.dimensions=incomplete.dimensions)
            }
            else
                rv
        },
        
        apply = function(from.arr, 
                         to.dim.names=NULL, 
                         fun='sum', 
                         na.rm=F,
                         error.prefix='')
        {
            # Validate dim.names
            private$check.can.apply(from.dim.names=dimnames(from.arr),
                                    to.dim.names=to.dim.names,
                                    complete.dimensions=character(),
                                    throw.errors=T,
                                    error.prefix=error.prefix)
            
            # Make to.dim.names if missing
            if (is.null(to.dim.names))
                to.dim.names = private$do.apply.to.dim.names(from.dim.names = dimnames(from.arr),
                                                             complete.dimensions = character(),
                                                             error.prefix = error.prefix)
            
            # Validate 'fun'
            if (is.character(fun))
            {
                if (length(fun)!=1 || is.na(fun))
                    stop("If 'fun' is the name of a function, it must be a single, non-NA character value")
                
                apply.sum = fun=='sum'
                
                fun = tryCatch(get(fun),
                               error=function(e){
                                   stop(paste0(error.prefix,
                                               "The value passed to fun (", fun, 
                                               "') is not the name of a valid function"))
                               })
            }
            else if (is.function(fun))
                apply.sum = identical(fun, sum)
            else
                stop(paste0(error.prefix, "The 'fun' to apply an ontology mapping must be either a function or the name of a function"))
            
            if (apply.sum && is.numeric(from.arr))
                private$do.apply.sum(from.arr, to.dim.names, na.rm=na.rm, error.prefix=error.prefix)
            else
            {
                mat = private$do.get.matrix(from.dim.names=dimnames(from.arr),
                                            to.dim.names=to.dim.names,
                                            error.prefix=error.prefix)
                
                rv = sapply(1:prod(sapply(to.dim.names, length)), function(i){
                    mask = mat[i,]==1
                    fun(from.arr[mask])
                })
                
                dim(rv) = sapply(to.dim.names, length)
                dimnames(rv) = to.dim.names
                
                rv
            }
        },
        
        get.matrix = function(from.dim.names, to.dim.names, error.prefix='')
        {
            # Validate dim.names
            private$check.can.apply(from.dim.names=from.dim.names,
                                    to.dim.names=to.dim.names,
                                    complete.dimensions=character(),
                                    throw.errors=T,
                                    error.prefix=error.prefix)
            
            if (is.null(to.dim.names))
                to.dim.names = private$do.apply.to.dim.names(from.dim.names = from.dim.names,
                                                             complete.dimensions = character(),
                                                             error.prefix = error.prefix)
            
            private$do.get.matrix(from.dim.names=from.dim.names,
                                  to.dim.names=to.dim.names,
                                  error.prefix=error.prefix)
        }
    ),
    
    active = list(
        
        name = function(value)
        {
            if (missing(value))
                private$i.name
            else
                stop("Cannot set value for 'name' in ontology.mapping - it is read-only")
        },
        
        is.identity.mapping = function(value)
        {
            if (missing(value))
                length(self$from.dimensions)==0 && length(self$to.dimensions)==0
            else
                stop("Cannot set value for 'is.identity.mapping' in ontology.mapping - it is read-only")
        },
        
        from.dimensions = function(value)
        {
            if (missing(value))
                names(self$from.dim.names)
            else
                stop("Cannot set value for 'from.dimensions' in ontology.mapping - they are read-only")
        },
        
        to.dimensions = function(value)
        {
            if (missing(value))
                names(self$to.dim.names)
            else
                stop("Cannot set value for 'to.dimensions' in ontology.mapping - they are read-only")
        },
        
        from.dim.names = function(value)
        {
            stop("This subclass of 'ontology.mapping' is incompletely specified. The 'from.dim.names' method must be implemented at the subclass level")
        },
        
        to.dim.names = function(value)
        {
            stop("This subclass of 'ontology.mapping' is incompletely specified. The 'to.dim.names' method must be implemented at the subclass level")
        }
    ),
    
    private = list(
        i.name = NULL,
        
        do.apply.sum = function(from.arr,
                                 to.dim.names,
                                 na.rm,
                                 error.prefix)
        {
            stop("This subclass of 'ontology.mapping' is incompletely specified. The private 'do.apply.sum' method must be implemented at the subclass level")
        },
        
        do.get.matrix = function(from.dim.names,
                                  to.dim.names,
                                  error.prefix)
        {
            stop("This subclass of 'ontology.mapping' is incompletely specified. The private 'do.get.matrix' method must be implemented at the subclass level")            
        },
        
        do.apply.to.dim.names = function(from.dim.names,
                                          complete.dimensions,
                                          error.prefix)
        {
            stop("This subclass of 'ontology.mapping' is incompletely specified. The private 'do.apply.to.dim.names' method must be implemented at the subclass level")            
        },
        
        check.can.apply = function(from.dim.names, 
                                    to.dim.names,
                                    complete.dimensions, 
                                    throw.errors,
                                    error.prefix)
        {
            stop("This subclass of 'ontology.mapping' is incompletely specified. The private 'check.can.apply' method must be implemented at the subclass level")            
        }
    )
)

IDENTITY.ONTOLOGY.MAPPING = R6::R6Class(
    'identity.ontology.mapping',
    inherit = ONTOLOGY.MAPPING,
    
    public = list(
        initialize = function()
        {
            super$initialize(name='No-Change Ontology Mapping')
        },
        
        print = function(...)
        {
            print("A 'no-change' ontology mapping")
        },
        
        equals = function(other)
        {
            is(other, 'ontology.mapping') && other$is.identity.mapping
        }
    ),
    
    active = list(
        
        from.dim.names = function(value)
        {
            list()
        },
        
        to.dim.names = function(value)
        {
            list()
        }
    ),
    
    private = list(
        do.apply.sum = function(from.arr,
                                 to.dim.names,
                                 na.rm,
                                 error.prefix)
        {
            array.access(from.arr, to.dim.names)
        },
        
        do.get.matrix = function(from.dim.names,
                                  to.dim.names,
                                  error.prefix)
        {
            rv = diag(rep(1, prod(sapply(from.dim.names, length))))
            indices = get.array.access.indices(arr.dim.names=from.dim.names,
                                               dimension.values=to.dim.names)
            
            rv[indices,,drop=F]
        },
        
        do.apply.to.dim.names = function(from.dim.names,
                                          complete.dimensions,
                                          error.prefix)
        {
            from.dim.names
        },
        
        check.can.apply = function(from.dim.names, 
                                    to.dim.names,
                                    complete.dimensions, 
                                    throw.errors,
                                    error.prefix)
        {
            if (is.null(to.dim.names) || dim.names.are.subset(sub.dim.names = to.dim.names,
                                                          super.dim.names = from.dim.names))
                T
            else
            {
                if (throw.errors)
                    stop(paste0(error.prefix, "'to.dim.names' must be a subset of 'from.dim.names'"))
                else
                    F
            }
        }
    )
)

BASIC.ONTOLOGY.MAPPING = R6::R6Class(
    'basic.ontology.mapping',
    inherit = ONTOLOGY.MAPPING,
    
    public = list(
        
        initialize = function(name,
                              from.dimensions,
                              to.dimensions,
                              from.values,
                              to.values)
        {
            super$initialize(name)
            
            # presume the error checking has been done by the
            # public-facing constructor
            
            if (dim(to.values)[1]==0)
                row.order = integer()
            else
            {
                order.arguments = c(lapply(1:dim(to.values)[2], function(i){to.values[,i]}),
                                    lapply(1:dim(from.values)[2], function(i){from.values[,i]}))
                row.order = do.call(order, args=order.arguments)
            }
            
            from.order = order(from.dimensions)
            to.order = order(to.dimensions)
            
            na.from.mask = apply(is.na(from.values), 1, all)[row.order]
            na.to.mask = apply(is.na(to.values), 1, all)[row.order]
            
            private$i.name = name
            private$i.from.dimensions = from.dimensions[from.order]
            private$i.to.dimensions = to.dimensions[to.order]
            private$i.from.values = from.values[row.order,from.order,drop=F]
            private$i.to.values = to.values[row.order,to.order,drop=F]
            private$i.mapped.from.values = private$i.from.values[!na.from.mask & !na.to.mask,,drop=F]
            private$i.mapped.to.values = private$i.to.values[!na.from.mask & !na.to.mask,,drop=F]
            
            dimnames(private$i.from.values)[[2]] = dimnames(private$i.mapped.from.values)[[2]] = private$i.from.dimensions
            dimnames(private$i.to.values)[[2]] = dimnames(private$i.mapped.to.values)[[2]] = private$i.to.dimensions
            
            private$i.unique.from.values = lapply(1:length(private$i.from.dimensions), function(d){
                unique(private$i.from.values[!na.from.mask,d])
            })
            names(private$i.unique.from.values) = private$i.from.dimensions
            
            private$i.unique.to.values = lapply(1:length(private$i.to.dimensions), function(d){
                unique(private$i.to.values[!na.to.mask,d])
            })
            names(private$i.unique.to.values) = private$i.to.dimensions
        },
        
        equals = function(other)
        {
            all(dim(self$from.values)==dim(other$from.values)) &&
                all(dim(self$to.values)==dim(other$to.values)) &
                all(self$from.dimensions == other$from.dimensions) &&
                all(self$to.dimensions == other$to.dimensions) &&
                all(self$from.values == other$from.values) &&
                all(self$to.values == other$to.values)
        }
    ),
    
    active = list(
        
        from.values = function(value)
        {
            if (missing(value))
                private$i.from.values
            else
                stop("Cannot set value for 'from.values' in ontology.mapping - they are read-only")
        },
        
        to.values = function(value)
        {
            if (missing(value))
                private$i.to.values
            else
                stop("Cannot set value for 'to.values' in ontology.mapping - they are read-only")
        },
        
        from.dim.names = function(value)
        {
            if (missing(value))
                private$i.unique.from.values
            else
                stop("Cannot set value for 'from.dim.names' in ontology.mapping - they are read-only")
        },
        
        to.dim.names = function(value)
        {
            if (missing(value))
                private$i.unique.to.values
            else
                stop("Cannot set value for 'to.dim.names' in ontology.mapping - they are read-only")
        }
    ),
    
    private = list(
        i.name = NULL,
        
        i.from.dimensions = NULL,
        i.to.dimensions = NULL,
        
        i.mapped.from.values = NULL,
        i.from.values  = NULL,
        i.mapped.to.values = NULL, #leaves out rows where the corresponding from.values were NA
        i.to.values = NULL,
        
        i.unique.from.values = NULL,
        i.unique.to.values = NULL,
        
        do.apply.sum = function(from.arr,
                                 to.dim.names,
                                 na.rm,
                                 error.prefix)
        {
            rv = array(as.numeric(NA), dim=sapply(to.dim.names, length), dimnames=to.dim.names)
            
            rv = apply_ontology_mapping(src = from.arr,
                                        dst = rv,
                                        from_values = private$i.mapped.from.values,
                                        to_values = private$i.mapped.to.values,
                                        na_rm = na.rm)

            if (is.null(rv)) #theoretically should not happen - should have been caught by error checking
                stop(paste0(error.prefix, "There was an error applying the ontology.mapping"))
            
            rv
        },
        
        do.get.matrix = function(from.dim.names,
                                  to.dim.names,
                                  error.prefix)
        {
            rv = matrix(0, nrow=prod(sapply(to.dim.names, length)), ncol=prod(sapply(from.dim.names, length)))
            
            rv = get_ontology_mapping_matrix(src_dim_names = from.dim.names,
                                             dst_dim_names = to.dim.names,
                                             dst = rv,
                                             from_values = private$i.mapped.from.values,
                                             to_values = private$i.mapped.to.values)
  
            if (is.null(rv)) #theoretically should not happen - should have been caught by error checking
                stop(paste0(error.prefix, "There was an error applying the ontology.mapping to get a matrix"))
            
            rv
        },
        
        do.apply.to.dim.names = function(from.dim.names,
                                          complete.dimensions,
                                          error.prefix)
        {
            given.from.values = get.every.combination(from.dim.names[self$from.dimensions])
            
            resulting.to.indices = unlist(apply(given.from.values, 1, row.indices.of, haystack = private$i.mapped.from.values))
            resulting.to.values = unique(private$i.mapped.to.values[resulting.to.indices,,drop=F])
            
            resulting.to.dim.names = lapply(self$to.dimensions, function(d){
                if (any(d==complete.dimensions))
                    self$to.dim.names[[d]]
                else
                    unique(resulting.to.values[,d])
            })
            names(resulting.to.dim.names) = self$to.dimensions
            
#            rv = from.dim.names
#            rv[self$to.dimensions] = resulting.to.dim.names
#            rv = rv[setdiff(names(rv), setdiff(self$from.dimensions, self$to.dimensions))]
            
            dimensions.to.keep = setdiff(names(from.dim.names), setdiff(self$from.dimensions, self$to.dimensions))
            rv = lapply(dimensions.to.keep, function(d){
                if (any(d==self$to.dimensions))
                    resulting.to.dim.names[[d]]
                else
                    from.dim.names[[d]]
            })
            names(rv) = dimensions.to.keep
            
            rv
        },
        
        check.can.apply = function(from.dim.names, 
                                    to.dim.names,
                                    complete.dimensions, 
                                    throw.errors,
                                    error.prefix)
        {
            if (!initial.check.can.apply(mapping=self,
                                         from.dim.names=from.dim.names,
                                         to.dim.names=to.dim.names,
                                         complete.dimensions=complete.dimensions,
                                         throw.errors=throw.errors,
                                         error.prefix=error.prefix))
                return (F)
            
           
            if (is.null(to.dim.names))
            {
                #we could skip this if all dimensions in from.dim.names are complete
                given.from.values = get.every.combination(from.dim.names[self$from.dimensions])
                
                resulting.to.indices = unlist(apply(given.from.values, 1, row.indices.of, haystack = private$i.mapped.from.values))
                resulting.to.values = unique(private$i.mapped.to.values[resulting.to.indices,,drop=F])
            }
            else
            {
                resulting.to.values = get.every.combination(to.dim.names[intersect(self$to.dimensions,
                                                                                   names(to.dim.names))])
            }
            
            required.from.indices = unlist(apply(resulting.to.values, 1, row.indices.of, haystack = private$i.mapped.to.values))
            
            missing.required.from.values = sapply(self$from.dimensions, function(d){
                missing.values = setdiff(private$i.mapped.from.values[required.from.indices,d],
                                         from.dim.names[[d]])
                if (length(missing.values)==0)
                    T
                else
                {
                    if (throw.errors)
                    {
                        stop(paste0(error.prefix, 
                                    "from.dim.names ['", d, "'] is missing ", 
                                    length(missing.values),
                                    ifelse(length(missing.values)==1, 'value', 'values'),
                                    ": ", collapse.with.and("'", missing.values, "'")))
                    }
                    else
                        F
                }
            })
            
            any(missing.required.from.values)
        }
    )
)

COMBINATION.ONTOLOGY.MAPPING = R6::R6Class(
    'combination.ontology.mapping',
    inherit = ONTOLOGY.MAPPING,
    
    public = list(
        
        initialize = function(name,
                              sub.mappings)
        {
            super$initialize(name)
            
            if (!is.list(sub.mappings))
                stop("Cannot create combination ontology mapping: sub.mappings must be a list")
                
            private$i.sub.mappings = sub.mappings
        },
        
        equals = function(other)
        {
            is(other, 'combination.ontology.mapping') &&
                length(self$sub.mappings) == length(other$sub.mappings) &&
                all(sapply(1:length(self$sub.mappings), function(i){
                    self$sub.mappings[[i]]$equals(other$sub.mappings[[i]])
                }))
        }
    ),
    
    active = list(
        
        from.dim.names = function(value)
        {
            if (missing(value))
            {
                rv = private$i.sub.mappings[[length(private$i.sub.mappings)]]$from.dim.names
                for (sub.mapping in rev(private$i.sub.mappings)[-1])
                {
                    rv[names(sub.mapping$from.dim.names)] = sub.mapping$from.dim.names
                    rv = rv[setdiff(names(rv),
                                    setdiff(names(sub.mapping$to.dim.names),
                                            names(sub.mapping$from.dim.names)))]
                }
                    
                rv
            }
            else
                stop("Cannot set value for 'from.dim.names' in ontology.mapping - they are read-only")  
        },
        
        to.dim.names = function(value)
        {
            if (missing(value))
            {
                rv = private$i.sub.mappings[[1]]$to.dim.names
                for (sub.mapping in private$i.sub.mappings[-1])
                {
                    rv[names(sub.mapping$to.dim.names)] = sub.mapping$to.dim.names
                    rv = rv[setdiff(names(rv),
                                    setdiff(names(sub.mapping$from.dim.names),
                                            names(sub.mapping$to.dim.names)))]
                }
                
                rv
            }
            else
                stop("Cannot set value for 'from.dim.names' in ontology.mapping - they are read-only")  
            
        },
        
        sub.mappings = function(value)
        {
            if (missing(value))
                private$i.sub.mappings
            else
                stop("Cannot set value for 'sub.mappings' in ontology.mapping - they are read-only")  
        }
    ),
    
    private = list(
        
        i.sub.mappings = NULL,
        
        do.apply.sum = function(from.arr,
                                 to.dim.names,
                                 na.rm,
                                 error.prefix)
        {
            rv = from.arr
            for (i in 1:length(private$i.sub.mappings))
            {
                sub.mapping = private$i.sub.mappings[[i]]
                if (i == length(private$i.sub.mappings))
                    rv = sub.mapping$apply(rv, to.dim.names)
                else
                    rv = sub.mapping$apply(rv, NULL)
            }
            
            rv
        },
        
        do.get.matrix = function(from.dim.names,
                                  to.dim.names,
                                  error.prefix)
        {
            rv = NULL
            for (i in 1:length(private$i.sub.mappings))
            {
                sub.mapping = private$i.sub.mappings[[i]]
                if (i == length(private$i.sub.mappings))
                    to.add = sub.mapping$get.matrix(from.dim.names, to.dim.names)
                else
                    to.add = sub.mapping$get.matrix(from.dim.names, NULL)
                
                if (is.null(rv))
                    rv = to.add
                else
                    rv = to.add %*% rv
                
                if (i < length(private$i.sub.mappings))
                    from.dim.names = sub.mapping$apply.to.dim.names(from.dim.names)
            }
            
            rv
        },
        
        do.apply.to.dim.names = function(from.dim.names,
                                          complete.dimensions,
                                          error.prefix)
        {
            rv = from.dim.names
            for (sub.mapping in private$i.sub.mappings)
                rv = sub.mapping$apply.to.dim.names(rv,
                                                    complete.dimensions = complete.dimensions,
                                                    error.prefix = error.prefix)
            
            rv
        },
        
        check.can.apply = function(from.dim.names, 
                                    to.dim.names,
                                    complete.dimensions, 
                                    throw.errors,
                                    error.prefix)
        {
            initial.check.can.apply(mapping=self,
                                    from.dim.names=from.dim.names,
                                    to.dim.names=to.dim.names,
                                    complete.dimensions=complete.dimensions,
                                    throw.errors=throw.errors,
                                    error.prefix=error.prefix)
        }
    )
)



IDENTITY.MAPPING = IDENTITY.ONTOLOGY.MAPPING$new()


##-- HELPER --##

initial.check.can.apply <- function(mapping,
                                    from.dim.names,
                                    to.dim.names,
                                    complete.dimensions,
                                    throw.errors,
                                    error.prefix)
{
    check.dim.names.valid(from.dim.names,
                          variable.name.for.error = 'from.dim.names',
                          allow.duplicate.values.across.dimensions = T,
                          error.prefix = error.prefix)
    
    if (!is.null(to.dim.names))
    {
        check.dim.names.valid(to.dim.names,
                              variable.name.for.error = 'to.dim.names',
                              allow.duplicate.values.across.dimensions = T,
                              error.prefix = error.prefix)
        
        if (length(intersect(mapping$to.dimensions, names(to.dim.names)))==0)
        {
            if (throw.errors)
                stop(paste0(error.prefix,
                            ifelse(length(mapping$to.dimensions)==1,
                                   paste0("The mapped 'to' dimension ('", mapping$to.dimensions, "') is"),
                                   paste0("None of the mapped 'to' dimensions (", collapse.with.and("'", mapping$to.dimensions, "'"), ") are")),
                            " contained in 'to.dim.names'"))
            else
                return (F)
        }
        
        non.mapped.from.dimensions = setdiff(names(from.dim.names), mapping$from.dimensions)
        non.mapped.to.dimensions = setdiff(names(to.dim.names), mapping$to.dimensions)
        extra.to.dimensions = setdiff(non.mapped.to.dimensions, non.mapped.from.dimensions)
        if (length(extra.to.dimensions)>0)
        {
            if (throw.errors)
                stop(paste0(error.prefix,
                            "'to.dim.names' contains ",
                            ifelse(length(extra.to.dimensions)==1, "a dimension that is", "dimensions that are"),
                            " not present in 'from.dim.names' (",
                            collapse.with.and("'", extra.to.dimensions, "'"), ")"))
            else
                return (F)
        }
        
    }
    
    required.from.values.are.present = sapply(mapping$from.dimensions, function(d){
        if (is.null(from.dim.names[[d]]))
        {
            if (throw.errors)
                stop(paste0(error.prefix,
                            "Dimension '", d, "' is missing from 'from.dim.names"))
            else
                F
        }
        else if (any(complete.dimensions==d))
        {
            if (setequal(from.dim.names[[d]], mapping$from.dim.names[[d]]))
                T
            else
            {
                if (throw.errors)
                {
                    extra.values = setdiff(from.dim.names[[d]], mapping$from.dim.names[[d]])
                    missing.values = setdiff(mapping$from.dim.names[[d]], from.dim.names[[d]])
                    
                    if (length(extra.values)>0)
                        extra.msg = paste0("contains",
                                           ifelse(length(extra.values)==1, 'an extraneous value', 'extraneous values'),
                                           " (", collapse.with.and("'", extra.values, "'"), ") that ",
                                           ifelse(length(extra.values)==1, 'is', 'are'),
                                           " not present in the mapping")
                    if (length(missing.values)>0)
                        missing.msg = paste0("is missing ", length(missing.values),
                                             ifelse(length(missing.values)==1, 'value', 'values'),
                                             ": ", collapse.with.and("'", missing.values, "'"))
                    
                    error.prefix = paste0(error.prefix,
                                          "from.dim.names['", d, "'] ")
                    if (length(extra.values)==0)
                        stop(paste0(error.prefix, missing.msg))
                    else if (length(missing.values)==0)
                        stop(paste0(error.prefix, extra.msg))
                    else
                        stop(paste0(error.prefix, missing.msg, " and ", extra.msg))
                }
                else
                    F
            }
        }
        else
        {
            if (is.subset(super=mapping$from.dim.names[[d]], sub=from.dim.names[[d]]))
                T
            else
            {
                if (throw.errors)
                {
                    missing.values = setdiff(mapping$from.dim.names[[d]], from.dim.names[[d]])
                    stop(paste0(error.prefix, 
                                "'from.dim.names' is missing ", length(missing.values),
                                ifelse(length(missing.values)==1, 'value', 'values'),
                                ": ", collapse.with.and("'", missing.values, "'")))
                }
                else
                    F
            }
        }
    })
    
    all(required.from.values.are.present)
}
