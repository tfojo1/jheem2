

if (1==2)
{

    # for basic likelihood
    create.outcome.location.mapping(list(C.12580='C.12580'),
                                    'new',
                                    'ehe',
                                    'C.12580')

    # for nested prop lik    
    create.outcome.location.mapping(list(C.12580=c("MD", 'C.12580', "24510")),
                                    'new',
                                    'ehe',
                                    'C.12580')
}

#'@param location.mappings is a named list, where the names correspond to modeled locations, and the elements are vectors of observed locations
create.outcome.location.mapping <- function(location.mappings,
                                            outcome.name,
                                            version = jheem.kernel$version,
                                            location = jheem.kernel$location,
                                            sub.version = NULL,
                                            jheem.kernel = NULL)
{
    if (!is.character(outcome.name) || length(outcome.name)!=1 || is.na(outcome.name))
        stop(paste0(error.prefix, "'outcome.name' must be a single, non-NA character value"))
    
    
    if (!is.list(location.mappings))
        stop("Cannot create outcome.location.mapping: 'location.mappings' must be named LISTS")
    
    if (is.null(names(location.mappings)))
        stop("Cannot create outcome.location.mapping: 'location.mappings' must be NAMED lists")
    
    if (any(!sapply(location.mappings, is.character)) ||
        any(sapply(location.mappings, length)==0) ||
        any(sapply(location.mappings, function(loc){any(is.na(loc))})))
        stop("The elements of 'location.mappings' must be character vectors with no NA values")
        
    list.loc.mappings = list(location.mappings)
    names(list.loc.mappings) = outcome.name
    
    OUTCOME.LOCATION.MAPPING$new(
        version = version,
        location = location,
        sub.version = sub.version,
        location.mappings.for.outcomes = list.loc.mappings,
        jheem.kernel = jheem.kernel
    )
}

copy.outcome.location.mapping <- function(to.copy,
                                          jheem.kernel=to.copy$jheem.kernel)
{
    if (!is(jheem.kernel, 'jheem.kernel'))
        stop("Cannot copy.outcome.loca")
    
    OUTCOME.LOCATION.MAPPING$new(
        version = to.copy$version,
        location = to.copy$location,
        sub.version = to.copy$sub.version,
        location.mappings.for.outcomes = to.copy$location.mappings.for.outcomes,
        jheem.kernel = jheem.kernel
    )
}

create.default.outcome.location.mapping <- function(version = jheem.kernel$version,
                                                  location = jheem.kernel$location,
                                                  sub.version = NULL,
                                                  jheem.kernel = NULL)
{
    OUTCOME.LOCATION.MAPPING$new(
        version = version,
        location = location,
        sub.version = sub.version,
        location.mappings.for.outcomes = NULL,
        jheem.kernel = jheem.kernel
    )
}

#'@param mappings.to.join A list of outcome.location.mappings objects
join.outcome.location.mappings <- function(mappings.to.join)
{
    if (!is.list(mappings.to.join))
        stop("Cannot join outcome.location.mappings: 'mappings.to.join' must be a list of outcome.location.mapping objects")
    
    if (any(!sapply(mappings.to.join, is, 'outcome.location.mapping')))
        stop("Cannot join outcome.location.mappings: 'mappings.to.join' must be a list of outcome.location.mapping objects")
    
    joined.outcome.location.mappings = list()
    
    for (mapping in mappings.to.join)
    {
        joined.outcome.location.mappings[mapping$outcome.names] = mapping$location.mappings.for.outcomes
    }
    
    OUTCOME.LOCATION.MAPPING$new(
        version = mappings.to.join[[1]]$version,
        location = mappings.to.join[[1]]$location,
        sub.version = mappings.to.join[[1]]$sub.version,
        location.mappings.for.outcomes = joined.outcome.location.mappings,
        jheem.kernel = mappings.to.join[[1]]$jheem.kernel
    )
}


# a helper that we use for now to force a reload of the class with the old data
update.outcome.location.mapping <- function(mapping, jheem.kernel)
{
    if (is.null(jheem.kernel))
        jheem.kernel = mapping$jheem.kernel
    
    OUTCOME.LOCATION.MAPPING$new(
        version = mapping$version,
        location = mapping$location,
        sub.version = mapping$sub.version,
        location.mappings.for.outcomes = mapping$location.mappings.for.outcomes,
        jheem.kernel = jheem.kernel
    )
}


OUTCOME.LOCATION.MAPPING = R6::R6Class(
    'outcome.location.mapping',
    inherit = JHEEM.ENTITY,
    portable = F,
    
    public = list(
        
        initialize = function(version,
                              location,
                              sub.version,
                              location.mappings.for.outcomes,
                              jheem.kernel = NULL)
        {
            super$initialize(version = version, 
                             location = location,
                             sub.version = sub.version,
                             type = 'outcome.location.mapping',
                             error.prefix="Cannot create outcome.location.mapping: ")
            
            # Dumb constructor - error checking is done in the wrapper to the constructor
            
            private$i.location.mappings.for.outcomes = location.mappings.for.outcomes  
            private$i.jheem.kernel = jheem.kernel
        },
        
        get.observed.locations = function(outcome.name,
                                          modeled.location,
                                          data.manager = get.default.data.manager())
        {
            error.prefix = "Cannot get observed locations: "
            
            if (!is.character(outcome.name) || length(outcome.name)!=1 || is.na(outcome.name))
                stop(paste0(error.prefix, "'outcome.name' must be a single, non-NA character value"))
            
            if (!is.character(modeled.location) || length(modeled.location)!=1 || is.na(modeled.location))
                stop(paste0(error.prefix, "'modeled.location' must be a single, non-NA character value"))
            
            if (is.null(private$i.location.mappings.for.outcomes[[outcome.name]]))
            {
                modeled.location.type = locations::get.location.type(modeled.location)
                location.type = locations::get.location.type(private$i.location)
                
                if (!is.na(modeled.location.type) && modeled.location.type=='CBSA' &&
                    (is.na(location.type) || location.type=='CBSA'))
                {
                    if (is.null(private$i.jheem.kernel))
                    {
                        specification = get.compiled.specification.for.version('ehe')
                        outcome = specification$get.outcome(outcome.name)
                    }
                    else
                        outcome = private$i.jheem.kernel$get.outcome.kernel(outcome.name)
                        
                    if (is.null(outcome))
                        stop(paste0(error.prefix, "'", outcome.name, "' is not a valid outcome in the '", private$i.version, "' model specification"))
                    
                    if (outcome$scale=='number' || outcome$scale=='non.negative.number')
                        modeled.location
                    else
                    {
                        max.n.states = 3
                        max.n.counties = 3
                        
                        states = locations::get.overlapping.locations(modeled.location, "STATE")
                        counties = locations::get.contained.locations(modeled.location, "COUNTY")
                        
                        if (length(states)>max.n.states || length(counties)>max.n.counties)
                        {
                            weighting.outcome = 'diagnosed.prevalence'
                            weight.by.county = data.manager$pull(outcome = weighting.outcome,
                                                                 dimension.values = list(location=counties),
                                                                 keep.dimensions = c('year', 'location'))
                            
                            if (!is.null(weight.by.county))
                            {
                                weight.by.county = apply(weight.by.county, 'location', mean, na.rm=T)[counties]
                                
                                if (length(states)>max.n.states)
                                {
                                    weight.by.state = sapply(states, function(state){
                                        counties.in.state = intersect(counties, locations::get.contained.locations(state, 'county'))
                                        sum(weight.by.county[counties.in.state])
                                    })
                                    
                                    states = states[ order(weight.by.state, decreasing = T)[1:max.n.states] ]
                                }
                                
                                if (length(counties)>max.n.counties)
                                {
                                    counties = counties[ order(weight.by.county, decreasing = T)[1:max.n.counties] ]
                                }
                            }
                        }
                        
                        c(modeled.location, states, counties)
                    }
                }
                else
                    modeled.location
                
            }
            else if (is.null(private$i.location.mappings.for.outcomes[[outcome.name]][[modeled.location]]))
                stop(paste0(error.prefix, "'", modeled.location, "' is not a modeled location for the '",
                            private$i.version, "' at location '", private$i.location, "'"))
            else
                private$i.location.mappings.for.outcomes[[outcome.name]][[modeled.location]]
        }
        
    ),
    
    active = list(
        
        outcome.names = function(value)
        {
            if (missing(value))
                names(private$i.location.mappings.for.outcomes)
            else
                stop("Cannot modify a JHEEM's 'outcome.names' - they are read-only")
        },
        
        modeled.locations = function(value)
        {
            if (missing(value))
                unique(unlist(sapply(private$i.location.mappings.for.outcomes, function(loc.mappings){
                    names(loc.mappings)
                })))
            else
                stop("Cannot modify a JHEEM's 'modeled.locations' - they are read-only")
        },
        
        location.mappings.for.outcomes = function(value)
        {
            if (missing(value))
                private$i.location.mappings.for.outcomes
            else
                stop("Cannot modify a JHEEM's 'location.mappings.for.outcomes' - they are read-only")
        },
        
        jheem.kernel = function(value)
        {
            if (missing(value))
                private$i.jheem.kernel
            else
                stop("Cannot modify a JHEEM's 'jheem.kernel' - they are read-only")
        }
    ),
    
    private = list(
        
        i.location.mappings.for.outcomes = NULL,
        i.jheem.kernel = NULL,
        
        get.current.code.iteration = function()
        {
            JHEEM.CODE.ITERATION   
        }
    )
)

# test code
if (1==2)
{
    map = create.outcome.location.mapping(version='ehe',
                                          location='C.12580',
                                          outcome.name = 'suppression',
                                          location.mappings = list(C.12580=c('C.12580','MD','24510')))
    
    map$outcome.names
    map$modeled.locations
    
    map$get.observed.locations('suppression', 'C.12580')
    
    
    
    map1 = create.outcome.location.mapping(version='ehe',
                                          location='C.12580',
                                          outcome.name = 'suppression',
                                          location.mappings = list(C.12580=c('C.12580','MD','24510')))
    map2 = create.outcome.location.mapping(version='ehe',
                                           location='C.12580',
                                           outcome.name = 'new',
                                           location.mappings = list(C.12580=c('C.12580')))
    
    map = join.outcome.location.mappings(list(map1, map2))
}

# Sketched out modeled location mapping

get.one.to.one.modeled.location.mapping <- function(version,
                                                    location,
                                                    sub.version = NULL)
{
    one.to.one = list(location)
    names(one.to.one) = location
    
    MODELED.LOCATION.MAPPING$new(
        version = version,
        location = location,
        sub.version = sub.version,
        compartments.for.modeled.locations = one.to.one
    )
}


MODELED.LOCATION.MAPPING = R6::R6Class(
    'modeled.location.mapping',
    inherit = JHEEM.ENTITY,
    portable = F,
    
    public = list(
        
        initialize = function(version,
                              location,
                              sub.version,
                              compartments.for.modeled.locations)
        {
            super$initialize(version = version,
                             location = location,
                             sub.version = sub.version,
                             type = 'modeled.location.mapping',
                             error.prefix="Cannot create modeled.location.mapping: ")
            
            if (all(sapply(names(compartments.for.modeled.locations), function(modeled.loc){
                length(compartments.for.modeled.locations[[modeled.loc]]) == 1 &&
                    compartments.for.modeled.locations[[modeled.loc]] == modeled.loc
            })))
            {
                private$i.ontology.mapping = get.identity.ontology.mapping()
            }
            else
            {
                iterated.modeled.locations = unlist(sapply(names(compartments.for.modeled.locations)), function(modeled.loc){
                    rep(modeled.loc, length(compartments.for.modeled.locations[[modeled.loc]]))
                })
                
                private$i.ontology.mapping = create.ontology.mapping(
                    mappings = cbind(unlist(compartments.for.modeled.locations),
                                     iterated.modeled.locations),
                    from.dimensions = 'location',
                    to.dimensions = 'location'
                )
            }
            
            private$i.compartments.for.modeled.locations = compartments.for.modeled.locations
        },
        
        apply = function(from.arr, 
                         to.dim.names=NULL, 
                         fun='sum', 
                         na.rm=F,
                         error.prefix='Cannot apply modeled.location.mapping: ')
        {
            private$i.ontology.mapping$apply(from.arr = from.arr, 
                                             to.dim.names = to.dim.names, 
                                             fun = fun, 
                                             na.rm = na.rm,
                                             error.prefix = error.prefix)
        },
        
        get.compartments.for.modeled.locations = function(modeled.locations)
        {
            if (!is.character(modeled.locations) || any(is.na(modeled.locations)))
                stop("Cannot get comparments for modeled locations: 'modeled.locations' must be a character vector with no NA values")
            
            unlist(private$i.compartments.for.modeled.locations[modeled.locations])
        }
    ),
    
    active = list(
        
        modeled.locations = function(value)
        {
            if (missing(value))
                names(private$i.compartments.for.modeled.locations)
            else
                stop("Cannot modify a JHEEM's 'modeled.locations' - they are read-only")
        },
        
        compartments = function(value)
        {
            if (missing(value))
                unique(unlist(private$i.compartments.for.modeled.locations))
            else
                stop("Cannot modify a JHEEM's 'modeled.locations' - they are read-only")
        },
        
        compartments.for.modeled.locations = function(value)
        {
            if (missing(value))
                private$i.compartments.for.modeled.locations
            else
                stop("Cannot modify a JHEEM's 'compartments.for.modeled.locations' - they are read-only")
        }
    ),
    
    private = list(
        
        i.compartments.for.modeled.locations = NULL,
        i.ontology.mapping = NULL,
        
        get.current.code.iteration = function()
        {
            JHEEM.CODE.ITERATION   
        }
    )
)