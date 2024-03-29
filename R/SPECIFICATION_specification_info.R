



SPECIFICATION.INFO = R6::R6Class(
    'specification.info',
    inherit = JHEEM.ENTITY,
    portable = F,
    
    public = list(
        
        initialize = function(version,
                              location,
                              error.prefix)
        {
            #-- Call the superclass constructor --#
            super$initialize(version = version,
                             location = location,
                             code.iteration = JHEEM.ENGINE.CODE.ITERATION,
                             intervention.code = NA,
                             calibration.index = NA,
                             type = "Specification Info",
                             error.prefix = error.prefix)
            
            specification = get.specification.for.version(version)
            private$i.specification.iteration = specification$iteration
            
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
            private$i.aliases = c(character.aliases, resolved.function.aliases)
            
            #-- Pull down dim names --#
            private$i.dim.names = lapply(specification$ontologies$all, substitute.aliases.into.vector, aliases = private$i.aliases)
            
            #-- Categorize aliases --#
            private$i.categorized.aliases = lapply(names(private$i.dim.names), function(d){
                aliases.apply.to.dimension.mask = sapply(private$i.aliases, function(aliases){
                    length(setdiff(aliases, private$i.dim.names[[d]]))==0
                })
                private$i.aliases[aliases.apply.to.dimension.mask]
            })
            names(private$i.categorized.aliases) = names(private$i.dim.names)
            
            #-- Import age stuff --#
            private$i.age.lower.bounds = specification$age.info$lowers
            private$i.age.upper.bounds = specification$age.info$uppers
            private$i.age.endpoints = specification$age.info$endpoints
            
            #-- All Done! --#
            
        },
        
        apply.aliases = function(dim.names, error.prefix='')
        {
            if (is.null(dim.names))
                NULL
            else if (is.character(dim.names))
            {
                substitute.aliases.into.vector(dim.names, aliases = private$i.aliases)
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
                                    collapse.with.and("'", invalid.dimensions, "'"),
                                    ifelse(length(invalid.dimensions)==1, "is not a valid dimension", "are not valid dimensions"),
                                    " for the specification for version '", private$i.version, "'"))
                    
                    rv = lapply(names(dim.names), function(d){
                        if (length(private$i.categorized.aliases[[d]])>0)
                            substitute.aliases.into.vector(dim.names[[d]], aliases=private$i.categorized.aliases[[d]])
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
        
        i.specification.iteration = NULL,
        
        i.dim.names = NULL,
        i.aliases = NULL,
        i.categorized.aliases = NULL,
        
        i.age.lower.bounds = NULL,
        i.age.upper.bounds = NULL,
        i.age.endpoints = NULL
        
    )
)

apply.categorized.aliases <- function(dim.names, aliases)
{
    rv = lapply(names(dim.names), function(d){
        if (length(aliases)[[d]]>0)
            substitute.aliases.into.vector(dim.names[[d]], aliases=aliases[[d]])
        else
            dim.names[[d]]
    })
    
    names(rv) = names(dim.names)
    rv
}