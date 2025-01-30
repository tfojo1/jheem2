


#'@title Get a Specification Metadata Object
#'
#'@param version The version of the model specification (must have been previously registered) for which to get metadata
#'@param location A single character value representing the location for the metadata
#'@param error.prefix A string to prepend to any errors generated in getting the metadata object
#'
#'@details A specification.metadata object contains metadata, such as dim.names of quantities, for a specification
#'
#'@export
get.specification.metadata <- function(version, location,
                                       error.prefix = '')
{
    specification = get.compiled.specification.for.version(version)
    do.get.specification.metadata(specification = specification,
                                  location = location,
                                  error.prefix = error.prefix)
}



# Internal to the package
# Contains all the 'smarts' to construct a new specification.metadata
#  so that the actual constructor can be dumb to facilitate copying
# also formulated to avoid name clashes with R6 methods (eg, specification.kernel also has a $get.specification.metadata method)
do.get.specification.metadata <- function(specification,
                                          location,
                                          error.prefix = '')
{
    if (is.null(error.prefix))
        error.prefix = paste0("Error deriving the specification-metadata for '", version, "' and location '", location, "': ")
    
    #-- Pull and resolve aliases --#
    resolved.function.aliases = lapply(names(specification$compartment.value.function.aliases), function(fn.name){
        fn = specification$compartment.value.function.aliases[[fn.name]]
        tryCatch({
            aliases = fn(location=location)
        },
        error = function(e){
            stop(paste0(error.prefix,
                        "There was an error evaluating the function for compartment value alias for '",
                        fn.name, "': ", e$message))
        })
        
        if (!is.character(aliases))
            stop(paste0(error.prefix,
                        "Evaluating the function for compartment value alias for '",
                        fn.name, "' did NOT yield a character value. It yielded an object of class ",
                        collapse.with.and("'", class(aliases), "'")))
        
        if (length(aliases)==0)
            stop(paste0(error.prefix,
                        "Evaluating the function for compartment value alias for '",
                        fn.name, "' yielded an empty (length-zero) character vector - aliases must have at least one value"))
        
        if (any(is.na(aliases)))
            stop(paste0(error.prefix,
                        "Evaluating the function for compartment value alias for '",
                        fn.name, "' yielded NA values"))
        
        tabled.aliases = table(aliases)
        if (any(tabled.aliases>1))
            stop(paste0(error.prefix,
                        "Evaluating the function for compartment value alias for '",
                        fn.name, "' yielded a character vector with duplicate values (values ",
                        collapse.with.and("'", names(tabled.aliases)[tabled.aliases>1], "'"),
                        " were included more than once). Alias values must be unique"))
        
        aliases
    })
    names(resolved.function.aliases) = names(specification$compartment.value.function.aliases)
    
    character.aliases = lapply(specification$compartment.value.character.aliases, substitute.aliases.into.vector, aliases = resolved.function.aliases)
    aliases = c(character.aliases, resolved.function.aliases)
    
    #-- Pull down dim names --#
    dim.names = lapply(specification$ontologies$all, substitute.aliases.into.vector, aliases = aliases)
    
    
    #-- All Done! - Call the Constructor --
    
    SPECIFICATION.METADATA$new(version = specification$version,
                               location = location,
                               specification.iteration = specification$iteration,
                               
                               aliases = aliases,
                               dim.names = dim.names,
                               
                               age.lower.bounds = specification$age.info$lowers,
                               age.upper.bounds = specification$age.info$uppers,
                               age.endpoints = specification$age.info$endpoints)
}

# Internal to the package
copy.specification.metadata <- function(to.copy)
{
    SPECIFICATION.METADATA$new(version = to.copy$versuib,
                               location = to.copy$location,
                               specification.iteration = to.copy$specification.iteration,
                               
                               aliases = to.copy$aliases,
                               dim.names = to.copy$dim.names,
                               
                               age.lower.bounds = to.copy$age.lower.bounds,
                               age.upper.bounds = to.copy$age.upper.bounds,
                               age.endpoints = to.copy$age.endpoints)
}

SPECIFICATION.METADATA = R6::R6Class(
    'specification.metadata',
    portable = F,
    
    public = list(
        
        # (Mostly) a dumb constructor
        # The smarts are in do.get.specification.metadata to allow for copy-construction
        initialize = function(version,
                              location,
                              specification.iteration,
                              
                              aliases,
                              dim.names,
                              
                              age.lower.bounds,
                              age.upper.bounds,
                              age.endpoints)
        {
            private$i.version = version
            private$i.location = location
            private$i.specification.iteration = specification.iteration
            
            private$i.aliases = aliases
            private$i.dim.names = dim.names
            
            private$i.age.lower.bounds = age.lower.bounds
            private$i.age.upper.bounds = age.upper.bounds
            private$i.age.endpoints = age.endpoints
            
            #-- Categorize aliases --#
            private$i.categorized.aliases = lapply(names(private$i.dim.names), function(d){
                aliases.apply.to.dimension.mask = sapply(private$i.aliases, function(aliases){
                    length(setdiff(aliases, private$i.dim.names[[d]]))==0
                })
                private$i.aliases[aliases.apply.to.dimension.mask]
            })
            names(private$i.categorized.aliases) = names(private$i.dim.names)
        },
        
        apply.aliases = function(dim.names, dimension=NULL, error.prefix='')
        {
            if (is.null(dim.names))
                NULL
            else if (is.character(dim.names))
            {
                if (length(dim.names)==1 && dim.names=='.')
                {
                    if (is.null(dimension))
                        stop(paste0(error.prefix, "Cannot apply dim.names to a character vector unless 'dimension' is supplied (ie, not NULL)"))
                    else if (all(names(private$i.ontologies)!=dimension))
                        stop(paste0(error.prefix, "'", dimension, "' is not a valid dimension for the '", private$i.version, "' specification"))
                    else
                        private$i.dim.names[dimension]
                }
                else
                    private$substitute.aliases.into.vector(dim.names, aliases = private$i.aliases)
            }
            else if (is.ontology(dim.names))
            {
                rv = self$apply.aliases(dim.names=as.list(dim.names), error.prefix=error.prefix)
                #as.ontology(rv, incomplete.dimensions = incomplete.dimensions(dim.names))
                
                attr(rv, 'is.complete') = attr(dim.names, 'is.complete')
                class(rv) = c('ontology', 'list')
                rv
            }
            else if (is.list(dim.names))
            {
                if (length(dim.names)==0)
                    dim.names
                else
                {
                    if (is.null(names(dim.names)))
                        stop(paste0(error.prefix, "In apply.aliases(), 'dim.names' must be a NAMED list"))
                    
                    invalid.dimensions = setdiff(names(dim.names), names(private$i.categorized.aliases))
                    if (length(invalid.dimensions)>0)
                        stop(paste0(error.prefix, "In apply.aliases(), ",
                                    ifelse(length(invalid.dimensions)==1, "dimension ", "dimensions "),
                                    #collapse.with.and("'", invalid.dimensions, "'"),
                                    paste0("'", invalid.dimensions, "'", collapse=', '),
                                    ifelse(length(invalid.dimensions)==1, " is not a valid dimension", " are not valid dimensions"),
                                    " for the specification for version '", private$i.version, "'"))
                    
                    rv = lapply(names(dim.names), function(d){
                        if (length(private$i.categorized.aliases[[d]])>0 && is.character(dim.names[[d]]))
                            private$substitute.aliases.into.vector(dim.names[[d]], aliases=private$i.categorized.aliases[[d]])
                        else
                            dim.names[[d]]
                    })
                    names(rv) = names(dim.names)
                    rv
                }
            }
            else
                stop(paste0(error.prefix, "In apply.aliases(), 'dim.names' must be either a character vector or a named list"))
        }
    ),
    
    active = list(
        
        version = function(value)
        {
            if (missing(value))
                private$i.version
            else
                stop("Cannot modify 'version' for a specification.info object - it is read-only")
        },
        
        location = function(value)
        {
            if (missing(value))
                private$i.location
            else
                stop("Cannot modify 'location' for a specification.info object - it is read-only")
        },
        
        specification.iteration = function(value)
        {
            if (missing(value))
                private$i.specification.iteration
            else
                stop("Cannot modify 'specification.iteration' for a specification.info object - it is read-only")
        },
        
        dim.names = function(value)
        {
            if (missing(value))
                private$i.dim.names
            else
                stop("Cannot modify 'dim.names' for a specification.info object - they are read-only")
        },
        
        dimensions = function(value)
        {
            if (missing(value))
                names(private$i.dim.names)
            else
                stop("Cannot modify 'dimensions' for a specification.info object - they are read-only")
        },
        
        compartment.aliases = function(value)
        {
            if (missing(value))
                private$i.aliases
            else
                stop("Cannot modify 'aliases' for a specification.info object - they are read-only")
        },
        
        n.ages = function(value)
        {
            if (missing(value))
                length(private$i.age.upper.bounds)
            else
                stop("Cannot modify 'n.ages' for a specification.info object - it is read-only")  
        },
        
        age.spans = function(value)
        {
            if (missing(value))
                private$i.age.upper.bounds - private$i.age.lower.bounds
            else
                stop("Cannot modify 'age.spans' for a specification.info object - they are read-only")
        },
        
        age.lower.bounds = function(value)
        {
            if (missing(value))
                private$i.age.lower.bounds
            else
                stop("Cannot modify 'age.lower.bounds' for a specification.info object - they are read-only")
        },
        
        age.upper.bounds = function(value)
        {
            if (missing(value))
                private$i.age.upper.bounds
            else
                stop("Cannot modify 'age.upper.bounds' for a specification.info object - they are read-only")
        },
        
        age.endpoints = function(value)
        {
            if (missing(value))
                private$i.age.endpoints
            else
                stop("Cannot modify 'age.endpoints' for a specification.info object - they are read-only")
        }
        
    ),
    
    private = list(
        
        i.version = NULL,
        i.location = NULL,
        i.specification.iteration = NULL,
        
        i.dim.names = NULL,
        i.aliases = NULL,
        i.categorized.aliases = NULL,
        
        i.age.lower.bounds = NULL,
        i.age.upper.bounds = NULL,
        i.age.endpoints = NULL,
        
        i.ontologies = NULL,
        
        #-- Alias Helpers --#
        
        # here so we don't have to pull functions from the global environment when we save
        substitute.aliases.into.vector = function(values, aliases)
        {
            unique(as.character(unlist(sapply(values, function(val){
                if (any(val==names(aliases)))
                    aliases[[val]]
                else
                    val
            }))))
        }
        
    )
)

# apply.categorized.aliases <- function(dim.names, aliases)
# {
#     rv = lapply(names(dim.names), function(d){
#         if (length(aliases)[[d]]>0)
#             substitute.aliases.into.vector(dim.names[[d]], aliases=aliases[[d]])
#         else
#             dim.names[[d]]
#     })
#     
#     names(rv) = names(dim.names)
#     rv
# }