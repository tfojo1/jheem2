

# Can we do this where we keep an internal ontology manager?

create.ontology.manager <- function()
{
    rv = list(dimensions=character(),
              categories.for.dimension=list())
    
    class(rv) = 'ontology.manager'
    rv
}

ONTOLOGY.MANAGER = create.ontology.manager()

register.ontology.category <- function(dimension,
                                       category,
                                       values)
{
    
    if (all(dimension != ONTOLOGY.MANAGER$dimensions))
    {
        ONTOLOGY.MANAGER$dimensions == c(ONTOLOGY.MANAGER$dimensions, dimension)
        ONTOLOGY.MANAGER$categories.for.dimension[[dimension]] = list()
    }
    
    
    if (any(category == names(ONTOLOGY.MANAGER$categories.for.dimension[[dimension]][[category]])))
        stop(paste0("ontology values for category '", category, "' in dimension '", dimension, "' have already been registered"))
    else
        ONTOLOGY.MANAGER$categories.for.dimension[[dimension]][[category]] = values
}

get.ontology.values.for.category <- function(dimension,
                                             category,
                                             throw.error.if.missing=T)
{
    if (dimension=='year' && !is.na(suppressWarnings(as.numeric(category))))
        as.numeric(category)
    else
    {
        if (all(dimension != ONTOLOGY$dimensions))
        {
            if (throw.error.if.missing)
                stop(paste0("'", dimension, "' has not been registered as a dimension for ontologies"))
            else
                NULL
        }
        else
        {
            rv = ONTOLOGY.MANAGER$categories.for.dimension[[dimension]][[category]]
            if (throw.error.if.missing && is.null(rv))
                stop(paste0("'", category, "' has not been registered as an ontology category for dimension '", dimension, "'"))
            rv
        }
    }
}

is.valid.ontology.category <- function(dimension,
                                       category)
{
    !is.null(get.ontology.values.for.category(dimension=dimension, cateogry=category, throw.error.if.missing = F))
}