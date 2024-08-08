
create.nested.proportion.likelihood.instructions.with.included.multiplier <- function(outcome.for.data,
                                                                                      denominator.outcome.for.data, # is NEVER null here because we are working with proportions
                                                                                      outcome.for.sim,
                                                                                      denominator.outcome.for.sim = NULL, # if NULL, uses denominator data within the sim data
                                                                                      outcome.for.n.multipliers = denominator.outcome.for.data,
                                                                                      
                                                                                      location.types, # test is c('county', 'state')
                                                                                      minimum.geographic.resolution.type, # test with 'county' #metalocations MUST contain these
                                                                                      maximum.locations.per.type = 3,
                                                                                      
                                                                                      dimensions = NULL, # this can be NULL because it's more intuitive to most users, but I'll change it to character(0) later
                                                                                      levels.of.stratification = 0:length(dimensions), # default 0:length(dimensions)
                                                                                      
                                                                                      from.year = -Inf,
                                                                                      to.year = Inf,
                                                                                      omit.years = NULL,
                                                                                      
                                                                                      sources.to.use = NULL,
                                                                                      exclude.denominator.ontology.names = NULL,
                                                                                      redundant.location.threshold = 5,
                                                                                      
                                                                                      included.multiplier,
                                                                                      included.multiplier.sd,
                                                                                      included.multiplier.correlation=NULL,
                                                                                      included.multiplier.correlation.structure=c('compound.symmetry', 'autoregressive.1')[1],
                                                                                      
                                                                                      p.bias.inside.location,
                                                                                      p.bias.outside.location,
                                                                                      p.bias.sd.inside.location,
                                                                                      p.bias.sd.outside.location,
                                                                                      
                                                                                      within.location.p.error.correlation = 0.5,
                                                                                      within.location.n.error.correlation = 0.5,
                                                                                      
                                                                                      correlation.different.locations = 0,
                                                                                      correlation.different.years = 0.5,
                                                                                      correlation.different.strata = 0.1,
                                                                                      correlation.different.sources = 0.3,
                                                                                      correlation.same.source.different.details = 0.3,
                                                                                      observation.correlation.form = c('compound.symmetry', 'autoregressive.1')[1],
                                                                                      n.multiplier.cv = 0.1, # keeping this one
                                                                                      
                                                                                      p.error.variance.term=NULL,
                                                                                      p.error.variance.type=NULL,
                                                                                      n.error.variance.term=0.05, # placeholder
                                                                                      n.error.variance.type='cv', # placeholder # same as old "denominator.measurement.error.cv"?
                                                                                      
                                                                                      weights,
                                                                                      equalize.weight.by.year = T,
                                                                                      
                                                                                      partitioning.function)
{
    JHEEM.NESTED.PROPORTION.LIKELIHOOD.INSTRUCTIONS$new(outcome.for.data = outcome.for.data,
                                                        denominator.outcome.for.data = denominator.outcome.for.data,
                                                        outcome.for.sim = outcome.for.sim,
                                                        denominator.outcome.for.sim = denominator.outcome.for.sim,
                                                        outcome.for.n.multipliers = outcome.for.n.multipliers,
                                                        location.types = location.types,
                                                        minimum.geographic.resolution.type = minimum.geographic.resolution.type,
                                                        maximum.locations.per.type = maximum.locations.per.type,
                                                        dimensions = dimensions,
                                                        levels.of.stratification = levels.of.stratification,
                                                        from.year = from.year,
                                                        to.year = to.year,
                                                        omit.years = omit.years,
                                                        sources.to.use = sources.to.use,
                                                        exclude.denominator.ontology.names = exclude.denominator.ontology.names,
                                                        redundant.location.threshold = redundant.location.threshold,
                                                        included.multiplier=included.multiplier,
                                                        included.multiplier.sd=included.multiplier.sd,
                                                        included.multiplier.correlation=included.multiplier.correlation,
                                                        included.multiplier.correlation.structure=included.multiplier.correlation.structure,
                                                        p.bias.inside.location = p.bias.inside.location,
                                                        p.bias.outside.location = p.bias.outside.location,
                                                        p.bias.sd.inside.location = p.bias.sd.inside.location,
                                                        p.bias.sd.outside.location = p.bias.sd.outside.location,
                                                        within.location.p.error.correlation = within.location.p.error.correlation,
                                                        within.location.n.error.correlation = within.location.n.error.correlation,
                                                        correlation.different.locations = correlation.different.locations,
                                                        correlation.different.years = correlation.different.years,
                                                        correlation.different.strata = correlation.different.strata,
                                                        correlation.different.sources = correlation.different.sources,
                                                        correlation.same.source.different.details = correlation.same.source.different.details,
                                                        observation.correlation.form = observation.correlation.form,
                                                        n.multiplier.cv = n.multiplier.cv,
                                                        
                                                        p.error.variance.term = p.error.variance.term,
                                                        p.error.variance.type = p.error.variance.type,
                                                        n.error.variance.term = n.error.variance.term,
                                                        n.error.variance.type = n.error.variance.type,
                                                        
                                                        weights = weights,
                                                        equalize.weight.by.year = equalize.weight.by.year,
                                                        partitioning.function = partitioning.function,
                                                        use.lognormal.approximation = F,
                                                        calculate.lagged.difference = F)
}

create.time.lagged.comparison.nested.proportion.likelihood.instructions <- function(outcome.for.data,
                                                                                    denominator.outcome.for.data, # is NEVER null here because we are working with proportions
                                                                                    outcome.for.sim,
                                                                                    denominator.outcome.for.sim = NULL, # if NULL, uses denominator data within the sim data
                                                                                    outcome.for.n.multipliers = denominator.outcome.for.data,
                                                                                    
                                                                                    location.types, # test is c('county', 'state')
                                                                                    minimum.geographic.resolution.type, # test with 'county' #metalocations MUST contain these
                                                                                    maximum.locations.per.type = 3,
                                                                                    
                                                                                    dimensions = NULL, # this can be NULL because it's more intuitive to most users, but I'll change it to character(0) later
                                                                                    levels.of.stratification = 0:length(dimensions), # default 0:length(dimensions)
                                                                                    
                                                                                    from.year = -Inf,
                                                                                    to.year = Inf,
                                                                                    omit.years = NULL,
                                                                                    
                                                                                    sources.to.use = NULL,
                                                                                    exclude.denominator.ontology.names = NULL,
                                                                                    redundant.location.threshold = 5,
                                                                                    
                                                                                    p.bias.inside.location,
                                                                                    p.bias.outside.location,
                                                                                    p.bias.sd.inside.location,
                                                                                    p.bias.sd.outside.location,
                                                                                    
                                                                                    within.location.p.error.correlation = 0.5,
                                                                                    within.location.n.error.correlation = 0.5,
                                                                                    
                                                                                    correlation.different.locations = 0,
                                                                                    correlation.different.years = 0.5,
                                                                                    correlation.different.strata = 0.1,
                                                                                    correlation.different.sources = 0.3,
                                                                                    correlation.same.source.different.details = 0.3,
                                                                                    observation.correlation.form = c('compound.symmetry', 'autoregressive.1')[1],
                                                                                    n.multiplier.cv = 0.1, # keeping this one
                                                                                    
                                                                                    p.error.variance.term=NULL,
                                                                                    p.error.variance.type=NULL,
                                                                                    n.error.variance.term=0.05, # placeholder
                                                                                    n.error.variance.type='cv', # placeholder # same as old "denominator.measurement.error.cv"?
                                                                                    
                                                                                    weights,
                                                                                    equalize.weight.by.year = T,
                                                                                    
                                                                                    partitioning.function,
                                                                                    use.lognormal.approximation = T) # only change compared to generic
{
    JHEEM.NESTED.PROPORTION.LIKELIHOOD.INSTRUCTIONS$new(outcome.for.data = outcome.for.data,
                                                        denominator.outcome.for.data = denominator.outcome.for.data,
                                                        outcome.for.sim = outcome.for.sim,
                                                        denominator.outcome.for.sim = denominator.outcome.for.sim,
                                                        outcome.for.n.multipliers = outcome.for.n.multipliers,
                                                        location.types = location.types,
                                                        minimum.geographic.resolution.type = minimum.geographic.resolution.type,
                                                        maximum.locations.per.type = maximum.locations.per.type,
                                                        dimensions = dimensions,
                                                        levels.of.stratification = levels.of.stratification,
                                                        from.year = from.year,
                                                        to.year = to.year,
                                                        omit.years = omit.years,
                                                        sources.to.use = sources.to.use,
                                                        exclude.denominator.ontology.names=exclude.denominator.ontology.names,
                                                        redundant.location.threshold = redundant.location.threshold,
                                                        included.multiplier=NULL,
                                                        included.multiplier.sd=NULL,
                                                        included.multiplier.correlation=NULL,
                                                        p.bias.inside.location = p.bias.inside.location,
                                                        p.bias.outside.location = p.bias.outside.location,
                                                        p.bias.sd.inside.location = p.bias.sd.inside.location,
                                                        p.bias.sd.outside.location = p.bias.sd.outside.location,
                                                        within.location.p.error.correlation = within.location.p.error.correlation,
                                                        within.location.n.error.correlation = within.location.n.error.correlation,
                                                        correlation.different.locations = correlation.different.locations,
                                                        correlation.different.years = correlation.different.years,
                                                        correlation.different.strata = correlation.different.strata,
                                                        correlation.different.sources = correlation.different.sources,
                                                        correlation.same.source.different.details = correlation.same.source.different.details,
                                                        observation.correlation.form = observation.correlation.form,
                                                        n.multiplier.cv = n.multiplier.cv,
                                                        
                                                        p.error.variance.term = p.error.variance.term,
                                                        p.error.variance.type = p.error.variance.type,
                                                        n.error.variance.term = n.error.variance.term,
                                                        n.error.variance.type = n.error.variance.type,
                                                        
                                                        weights = weights,
                                                        equalize.weight.by.year = equalize.weight.by.year,
                                                        partitioning.function = partitioning.function,
                                                        use.lognormal.approximation = use.lognormal.approximation, # changed vs generic
                                                        calculate.lagged.difference = T) # changed vs generic
}

#'@inheritParams create.nested.proportion.likelihood.instructions
#'@param location.types The types of the locations that contain or are contained by the model location.
#'@param minimum.geographic.resolution.type The type of location used to partition locations. The type of the model location AND 'location.types' types must all completely enclose regions of this type
#'@param p.bias.inside.location A single numeric value specifying the bias in the outcome proportion between locations inside the model location and the model location itself
#'@param p.bias.outside.location A single numeric value specifying the bias in the outcome proportion between locations outside the model location and the model location itself
#'@param p.bias.sd.inside.location The standard deviation associated with 'p.bias.inside.location'
#'@param p.bias.sd.outsidde.location The standard deviation associated with 'p.bias.outside.location'
#'@param within.location.p.error.correlation 
#'@param within.location.n.error.correlation
#'@param correlation.different.locations
#'@param denominator.measurement.error.cv
#'@param n.multiplier.cv
#'@param partitioning.function A function that partitions values in the data ontology into values in the model ontology. Must have two arguments, "arr" and "version" and return an array with the same dimnames as the input array.
#'
#'@export
create.nested.proportion.likelihood.instructions <- function(outcome.for.data,
                                                             denominator.outcome.for.data, # is NEVER null here because we are working with proportions
                                                             outcome.for.sim,
                                                             denominator.outcome.for.sim = NULL, # if NULL, uses denominator data within the sim data
                                                             outcome.for.n.multipliers = denominator.outcome.for.data,
                                                             
                                                             location.types, # test is c('county', 'state')
                                                             minimum.geographic.resolution.type, # test with 'county' #metalocations MUST contain these
                                                             maximum.locations.per.type = 3,
                                                             
                                                             dimensions = NULL, # this can be NULL because it's more intuitive to most users, but I'll change it to character(0) later
                                                             levels.of.stratification = 0:length(dimensions), # default 0:length(dimensions)
                                                             
                                                             from.year = -Inf,
                                                             to.year = Inf,
                                                             omit.years = NULL,
                                                             
                                                             sources.to.use = NULL,
                                                             exclude.denominator.ontology.names = NULL,
                                                             redundant.location.threshold = 5,
                                                             
                                                             p.bias.inside.location,
                                                             p.bias.outside.location,
                                                             p.bias.sd.inside.location,
                                                             p.bias.sd.outside.location,
                                                             
                                                             within.location.p.error.correlation = 0.5,
                                                             within.location.n.error.correlation = 0.5,
                                                             
                                                             correlation.different.locations = 0,
                                                             correlation.different.years = 0.5,
                                                             correlation.different.strata = 0.1,
                                                             correlation.different.sources = 0.3,
                                                             correlation.same.source.different.details = 0.3,
                                                             observation.correlation.form = c('compound.symmetry', 'autoregressive.1')[1],
                                                             n.multiplier.cv = 0.1, # keeping this one
                                                             
                                                             p.error.variance.term=NULL,
                                                             p.error.variance.type=NULL,
                                                             n.error.variance.term=0.05, # placeholder
                                                             n.error.variance.type='cv', # placeholder # same as old "denominator.measurement.error.cv"?
                                                             
                                                             weights,
                                                             equalize.weight.by.year = T,
                                                             
                                                             partitioning.function)
{
    
    JHEEM.NESTED.PROPORTION.LIKELIHOOD.INSTRUCTIONS$new(outcome.for.data = outcome.for.data,
                                                        denominator.outcome.for.data = denominator.outcome.for.data,
                                                        outcome.for.sim = outcome.for.sim,
                                                        denominator.outcome.for.sim = denominator.outcome.for.sim,
                                                        outcome.for.n.multipliers = outcome.for.n.multipliers,
                                                        location.types = location.types,
                                                        minimum.geographic.resolution.type = minimum.geographic.resolution.type,
                                                        maximum.locations.per.type = maximum.locations.per.type,
                                                        dimensions = dimensions,
                                                        levels.of.stratification = levels.of.stratification,
                                                        from.year = from.year,
                                                        to.year = to.year,
                                                        omit.years = omit.years,
                                                        sources.to.use = sources.to.use,
                                                        exclude.denominator.ontology.names = exclude.denominator.ontology.names,
                                                        redundant.location.threshold = redundant.location.threshold,
                                                        included.multiplier=NULL,
                                                        included.multiplier.sd=NULL,
                                                        included.multiplier.correlation=NULL,
                                                        p.bias.inside.location = p.bias.inside.location,
                                                        p.bias.outside.location = p.bias.outside.location,
                                                        p.bias.sd.inside.location = p.bias.sd.inside.location,
                                                        p.bias.sd.outside.location = p.bias.sd.outside.location,
                                                        within.location.p.error.correlation = within.location.p.error.correlation,
                                                        within.location.n.error.correlation = within.location.n.error.correlation,
                                                        correlation.different.locations = correlation.different.locations,
                                                        correlation.different.years = correlation.different.years,
                                                        correlation.different.strata = correlation.different.strata,
                                                        correlation.different.sources = correlation.different.sources,
                                                        correlation.same.source.different.details = correlation.same.source.different.details,
                                                        observation.correlation.form = observation.correlation.form,
                                                        n.multiplier.cv = n.multiplier.cv,
                                                        
                                                        p.error.variance.term = p.error.variance.term,
                                                        p.error.variance.type = p.error.variance.type,
                                                        n.error.variance.term = n.error.variance.term,
                                                        n.error.variance.type = n.error.variance.type,
                                                        
                                                        weights = weights,
                                                        equalize.weight.by.year = equalize.weight.by.year,
                                                        partitioning.function = partitioning.function,
                                                        use.lognormal.approximation = F,
                                                        calculate.lagged.difference = F)
    
}

JHEEM.NESTED.PROPORTION.LIKELIHOOD.INSTRUCTIONS = R6::R6Class(
    'jheem.nested.proportion.likelihood.instructions',
    inherit = JHEEM.LIKELIHOOD.INSTRUCTIONS,
    
    public = list(
        
        initialize = function(outcome.for.data,
                              denominator.outcome.for.data,
                              outcome.for.sim,
                              denominator.outcome.for.sim,
                              outcome.for.n.multipliers,
                              location.types,
                              minimum.geographic.resolution.type,
                              maximum.locations.per.type,
                              dimensions,
                              levels.of.stratification,
                              from.year,
                              to.year,
                              omit.years,
                              sources.to.use,
                              exclude.denominator.ontology.names,
                              redundant.location.threshold,
                              included.multiplier,
                              included.multiplier.sd,
                              included.multiplier.correlation,
                              included.multiplier.correlation.structure=c('compound.symmetry', 'autoregressive.1')[1],
                              p.bias.inside.location,
                              p.bias.outside.location,
                              p.bias.sd.inside.location,
                              p.bias.sd.outside.location,
                              within.location.p.error.correlation,
                              within.location.n.error.correlation,
                              correlation.different.locations,
                              correlation.different.years,
                              correlation.different.strata,
                              correlation.different.sources,
                              correlation.same.source.different.details,
                              observation.correlation.form,
                              n.multiplier.cv,
                              
                              p.error.variance.term,
                              p.error.variance.type,
                              n.error.variance.term,
                              n.error.variance.type,
                              
                              weights,
                              equalize.weight.by.year,
                              partitioning.function,
                              use.lognormal.approximation,
                              calculate.lagged.difference)
        {
            
            error.prefix = paste0('Error creating nested proportion likelihood instructions: ')
            
            # validated in the super$initialize:
            # *outcome.for.sim* (although more validation follows)
            # *dimensions*
            # *levels.of.stratification*
            # *weights*
            # *likelihood.class.generator*
            
            super$initialize(outcome.for.sim = outcome.for.sim,
                             dimensions = dimensions,
                             levels.of.stratification = levels.of.stratification,
                             weights,
                             likelihood.class.generator = JHEEM.NESTED.PROPORTION.LIKELIHOOD,
                             error.prefix = error.prefix)
            
            # outcome.for.sim *MUST* be a proportion (suppression, linkage, engagement, aware, heroine use, retention), but this is checked at instantiate time.
            
            # *denominator.outcome.for.data* is a single character vector
            if (!is.character(outcome.for.data) || length(outcome.for.data) > 1 || is.null(outcome.for.data) || is.na(outcome.for.data))
                stop(paste0(error.prefix, "'outcome.for.data' must be a character vector of length 1"))
            
            # *denominator.outcome.for.sim* is a single character vector
            if (!is.null(denominator.outcome.for.sim) && (!is.character(denominator.outcome.for.sim) || length(denominator.outcome.for.sim) > 1 || is.na(denominator.outcome.for.sim)))
                stop(paste0(error.prefix, "'denominator.outcome.for.sim' must be NULL or a character vector of length 1"))
            
            # *outcome.for.n.multipliers* is a single character vector
            if (!is.null(outcome.for.n.multipliers) && (!is.character(outcome.for.n.multipliers) || length(outcome.for.n.multipliers) > 1 || is.na(outcome.for.n.multipliers)))
                stop(paste0(error.prefix, "'outcome.for.n.multipliers' must be NULL or a character vector of length 1"))
            
            # *location.types* is a character vector containing one or more registered location types and no NAs or duplicates
            if (!is.character(location.types) || length(location.types) < 1 || any(is.na(location.types)) || any(duplicated(location.types)) || any(sapply(location.types, function(x) {!(toupper(x) %in% locations::get.location.types())})))
                stop(paste0(error.prefix, "'location.types' must be a character vector containing one or more registered location types and no NAs or duplicates"))
            
            # *minimum.geographic.resolution.type* is a character vector containing a single registered location type # convert to 
            if (!is.character(minimum.geographic.resolution.type) || length(minimum.geographic.resolution.type)!=1 || is.na(minimum.geographic.resolution.type) || !(toupper(minimum.geographic.resolution.type) %in% locations::get.location.types()))
                stop(paste0(error.prefix, "'minimum.geographic.resolution.type] must be a character containing a single registered location type"))
            
            # *maximum.locations.per.type* is a single numeric value greater than zero
            if (!is.numeric(maximum.locations.per.type) || length(maximum.locations.per.type)!=1 || is.na(maximum.locations.per.type) || maximum.locations.per.type<1)
                stop(paste0(error.prefix, "'maximum.locations.per.type' must be a single numeric value greater than zero"))
            
            # *from.year* and *to.year* are single numeric vectors. *to.year* must be larger than *from.year*
            if (from.year != -Inf && (!is.numeric(from.year) || length(from.year) > 1 || is.null(from.year) || is.na(from.year)))
                stop(paste0(error.prefix, "'from.year' must be -Inf or an integer vector of length 1"))
            if (to.year != Inf && (!is.numeric(to.year) || length(to.year) > 1 || is.null(to.year) || is.na(to.year)))
                stop(paste0(error.prefix, "'to.year' must be Inf or an integer vector of length 1"))
            if (from.year > to.year)
                stop(paste0(error.prefix, "'from.year' must be less than 'to.year'"))
            
            # *omit.years* is NULL or a numeric vector containing no NAs or duplicates.
            if (!is.null(omit.years) && (!is.numeric(omit.years) || any(is.na(omit.years)) || any(duplicated(omit.years))))
                stop(paste0(error.prefix, "'omit.years' must be NULL or an numeric vector containing no NAs or duplicates"))
            
            # *sources.to.use* is NULL or a character vector containing no NAs or duplicates
            if (!is.null(sources.to.use) && (!is.character(sources.to.use) || any(is.na(sources.to.use)) || any(duplicated(sources.to.use))))
                stop(paste0(error.prefix, "'sources.to.use' must be NULL or a character vector containing no NAs or duplicates"))
            
            # *redundant.location.threshold* is a nonnegative numeric value
            if (!is.numeric(redundant.location.threshold) || length(redundant.location.threshold)!=1 || is.na(redundant.location.threshold) || redundant.location.threshold < 0)
                stop(paste0(error.prefix, "'redundant.location.threshold' must be a single, non-negative numeric value"))
            
            # *included.multiplier* is NULL, a single numeric value, or a named numeric vector with names corresponding to years or year ranges.
            if (!is.null(included.multiplier) &&
                (!is.numeric(included.multiplier) || length(included.multiplier)!=1 || is.na(included.multiplier) || included.multiplier<=0) &&
                (!is.numeric(included.multiplier) || is.null(names(included.multiplier)) || any(is.na(included.multiplier)) || any(included.multiplier<=0) || is.null(parse.year.names(names(included.multiplier)))))
                stop(paste0(error.prefix, "'included.multiplier' must be one of: 1. NULL, 2. a single, non-NA, numeric value greater than 0, or 3. a named numeric vector with all values non-NA and greater than zero and names all corresponding to years or year ranges"))
            
            # *included.multiplier.sd* is NULL, a single numeric value, or a named numeric vector with the same names as *included.multiplier*.
            if (!is.null(included.multiplier.sd) &&
                (!is.numeric(included.multiplier.sd) || length(included.multiplier.sd)!= 1 || is.na(included.multiplier.sd) || included.multiplier.sd<=0) &&
                (!is.numeric(included.multiplier.sd) || any(is.na(included.multiplier.sd)) || any(included.multiplier.sd<=0) || !identical(names(included.multiplier.sd), names(included.multiplier))))
                stop(paste0(error.prefix, "'included.multiplier.sd' must be one of: 1. NULL, 2. a single, non-NA, numeric value greater than 0, or 3. a named numeric vector with all values non-NA and greater than zero and the same names as 'included.multiplier'"))
            # and cannot be NULL if *included.multiplier* is not NULL
            if (!is.null(included.multiplier) && is.null(included.multiplier.sd))
                stop(paste0(error.prefix, "'included.multiplier.sd' cannot be NULL if 'included.multiplier' is not also NULL"))
            
            # *included.multiplier.correlation* is NULL or a single numeric value between 0 and 1.
            if (!is.null(included.multiplier.correlation) &&
                (!is.numeric(included.multiplier.correlation) || length(included.multiplier.correlation)!=1 || included.multiplier.correlation <=0 || included.multiplier.correlation >= 1))
                stop(paste0(error.prefix, "'included.multiplier.correlation' must be NULL or a single numeric value between 0 and 1"))
            # and cannot be NULL if *included.multiplier* is not NULL
            if (!is.null(included.multiplier) && is.null(included.multiplier.correlation))
                stop(paste0(error.prefix, "''included.multiplier.correlation' cannot be NULL if 'included.multiplier' is not also NULL"))
            
            # *included.multiplier.correlation.structure* is 'compound.symmetry' or 'autoregressive.1'
            if (!is.character(included.multiplier.correlation.structure) || length(included.multiplier.correlation.structure)!=1 || !(included.multiplier.correlation.structure) %in% c('compound.symmetry', 'autoregressive.1'))
                stop(paste0(error.prefix, "'included.multiplier.correlation.structure' must be either 'compound.symmetry' or 'autoregressive.1'"))
            
            # *p.error.variance.type* must be one of 'sd', 'variance', 'cv', 'data.sd', 'data.variance', or 'data.ci'
            if (!(p.error.variance.type %in% c('sd', 'variance', 'cv', 'data.sd', 'data.variance', 'data.cv', 'data.ci')))
                stop(paste0(error.prefix, "'p.error.variance.type' must be one of 'sd', 'variance', 'cv', 'data.sd', 'data.variance', 'data.cv', or 'data.ci'"))
            
            if (p.error.variance.type %in% c('sd', 'variance', 'cv') && (!is.numeric(p.error.variance.term) || length(p.error.variance.term)!=1 || is.na(p.error.variance.term) || p.error.variance.term < 0))
                stop(paste0(error.prefix, "'p.error.variance.term' must be a single, nonnegative, numeric value if 'p.error.variance.type' is one of 'sd', 'variance', or 'cv'"))
            if (p.error.variance.type %in% c('data.sd', 'data.variance', 'data.ci') && !is.null(p.error.variance.term))
                stop(paste0(error.prefix, "'p.error.variance.term' must be NULL if 'p.error.variance.type' is one of 'data.sd', 'data.variance', or 'data.ci'"))
            
            if (p.error.variance.type %in% c('data.ci'))
                stop(paste0(error.prefix, "'data.ci' is not yet supported as a 'p.error.variance.type'"))
            
            if (p.error.variance.type %in% c('data.sd', 'data.variance', 'data.cv', 'data.ci'))
                p.error.variance.term = 1
            
            # *n.error.variance.type* must be one of 'sd', 'variance', 'cv', 'data.sd', or 'data.ci'
            if (!(n.error.variance.type %in% c('sd', 'variance', 'cv', 'data.sd', 'data.variance', 'data.cv', 'data.ci')))
                stop(paste0(error.prefix, "'n.error.variance.type' must be one of 'sd', 'variance', 'cv', 'data.sd', 'data.variance', 'data.cv', or 'data.ci'"))
            
            if (n.error.variance.type %in% c('sd', 'variance', 'cv') && (!is.numeric(n.error.variance.term) || length(n.error.variance.term)!=1 || is.na(n.error.variance.term) || n.error.variance.term < 0))
                stop(paste0(error.prefix, "'n.error.variance.term' must be a single, nonnegative, numeric value if 'n.error.variance.type' is one of 'sd', 'variance', or 'cv'"))
            if (n.error.variance.type %in% c('data.sd', 'data.variance', 'data.ci') && !is.null(n.error.variance.term))
                stop(paste0(error.prefix, "'n.error.variance.term' must be NULL if 'n.error.variance.type' is one of 'data.sd', 'data.variance', 'data.cv', or 'data.ci'"))
            
            if (n.error.variance.type %in% c('sd', 'variance', 'data.sd', 'data.variance', 'data.cv', 'data.ci'))
                stop(paste0(error.prefix, "only 'cv' is currently supported for 'n.error.variance.type'"))
            
            if (n.error.variance.type %in% c('data.sd', 'data.variance', 'data.cv', 'data.ci'))
                n.error.variance.term = 1
            
            # *p.bias* constants, *correlation.multipliers*, *within.location* error correlations, *metalocation* correlations, *measurement.error.sd*, and *n.multiplier.cv* are all single numeric values with values between 0 and 1 inclusive
            between.negative.one.and.positive.one = list(p.bias.inside.location=p.bias.inside.location, 
                                                         p.bias.outside.location=p.bias.outside.location,
                                                         correlation.different.locations=correlation.different.locations,
                                                         correlation.different.years=correlation.different.years,
                                                         correlation.different.strata=correlation.different.strata,
                                                         correlation.different.sources=correlation.different.sources,
                                                         correlation.same.source.different.details=correlation.same.source.different.details,
                                                         within.location.p.error.correlation=within.location.p.error.correlation,
                                                         within.location.n.error.correlation=within.location.n.error.correlation)
            for (i in seq_along(between.negative.one.and.positive.one)) {
                if (!is.numeric(between.negative.one.and.positive.one[[i]]) || length(between.negative.one.and.positive.one[[i]]) > 1 || is.na(between.negative.one.and.positive.one[[i]]) || between.negative.one.and.positive.one[[i]] > 1 || between.negative.one.and.positive.one[[i]] < -1)
                    stop(paste0(error.prefix, "'", names(between.negative.one.and.positive.one)[[i]], "' must be a single numeric value between -1 and 1 inclusive"))
            }
            
            non.negative.not.infinity = list(p.bias.sd.inside.location=p.bias.sd.inside.location,
                                             p.bias.sd.outside.location=p.bias.sd.outside.location,
                                             n.multiplier.cv=n.multiplier.cv,
                                             p.error.variance.term=p.error.variance.term,
                                             n.error.variance.term=n.error.variance.term)
            for (i in seq_along(non.negative.not.infinity)) {
                if (!is.numeric(non.negative.not.infinity[[i]]) || length(non.negative.not.infinity[[i]]) > 1 || is.na(non.negative.not.infinity[[i]]) || non.negative.not.infinity[[i]] < 0 || non.negative.not.infinity[[i]] == Inf)
                    stop(paste0(error.prefix, "'", names(non.negative.not.infinity)[[i]], "' must be a single non-negative, non-infinite numeric value"))
            }
            
            # *observation.correlation.form* is either 'compound.symmetry' or 'autoregressive.1'
            if (length(observation.correlation.form) > 1 || !(observation.correlation.form %in% c('compound.symmetry', 'autoregressive.1')))
                stop(paste0(error.prefix, "'observation.correlation.form' must be either 'compound.symmetry' or 'autoregressive.1'"))
            
            # *equalize.weight.by.year* is a boolean
            if (!is.logical(equalize.weight.by.year) || length(equalize.weight.by.year) > 1 || is.null(equalize.weight.by.year) || is.na(equalize.weight.by.year))
                stop(paste0(error.prefix, "'equalize.weight.by.year' must be a single logical value (T/F)"))
            
            # *partitioning.function* is a function that accepts an array as input and returns an array of the same dimnames as output. It must have three arguments called "arr", "version" and "location".
            if (!is.function(partitioning.function) || length(formals(partitioning.function)) != 3 || names(formals(partitioning.function))[[1]] != 'arr' || names(formals(partitioning.function))[[2]] != 'version' || names(formals(partitioning.function))[[3]] != 'location')
                stop(paste0(error.prefix, "'partitioning.function' must be a function with only three arguments: 'arr', 'version', and 'location'"))
            
            #use.lognormal.approximation
            if (!is.logical(use.lognormal.approximation) || length(use.lognormal.approximation)!=1 || is.na(use.lognormal.approximation))
                stop(paste0(error.prefix, "'use.lognormal.approximation' must be a single logical value (T/F)"))
            #calculate.lagged.difference
            if (!is.logical(calculate.lagged.difference) || length(calculate.lagged.difference)!=1 || is.na(calculate.lagged.difference))
                stop(paste0(error.prefix, "'calculate.lagged.difference' must be a single logical value (T/F)"))
            
            # PROCESSING #
            omit.years = as.integer(omit.years)
            
            # STORE VALUES #
            
            private$i.location.types = location.types
            private$i.minimum.geographic.resolution.type = minimum.geographic.resolution.type
            private$i.maximum.locations.per.type = maximum.locations.per.type
            
            private$i.outcome.for.data = outcome.for.data
            private$i.denominator.outcome.for.data = denominator.outcome.for.data
            private$i.denominator.outcome.for.sim = denominator.outcome.for.sim
            private$i.outcome.for.n.multipliers = outcome.for.n.multipliers
            private$i.from.year = from.year
            private$i.to.year = to.year
            private$i.omit.years = omit.years
            private$i.equalize.weight.by.year = equalize.weight.by.year
            
            private$i.sources.to.use = sources.to.use
            private$i.exclude.denominator.ontology.names = exclude.denominator.ontology.names
            private$i.redundant.location.threshold = redundant.location.threshold
            private$i.parameters = list(included.multiplier = included.multiplier,
                                        included.multiplier.sd = included.multiplier.sd,
                                        included.multiplier.correlation = included.multiplier.correlation,
                                        included.multiplier.correlation.structure = included.multiplier.correlation.structure,correlation.different.locations = correlation.different.locations,
                                        correlation.different.years = correlation.different.years,
                                        correlation.different.strata = correlation.different.strata,
                                        correlation.different.sources = correlation.different.sources,
                                        correlation.same.source.different.details = correlation.same.source.different.details,
                                        observation.correlation.form = observation.correlation.form,
                                        n.multiplier.cv = n.multiplier.cv,
                                        within.location.p.error.correlation = within.location.p.error.correlation,
                                        within.location.n.error.correlation = within.location.n.error.correlation,
                                        p.bias.inside.location = p.bias.inside.location,
                                        p.bias.outside.location = p.bias.outside.location,
                                        p.bias.sd.inside.location = p.bias.sd.inside.location,
                                        p.bias.sd.outside.location = p.bias.sd.outside.location,
                                        p.error.variance.term = p.error.variance.term,
                                        p.error.variance.type = p.error.variance.type,
                                        n.error.variance.term = n.error.variance.term,
                                        n.error.variance.type = n.error.variance.type)
            
            private$i.partitioning.function = partitioning.function
            private$i.use.lognormal.approximation = use.lognormal.approximation
            private$i.calculate.lagged.difference = calculate.lagged.difference
        }
        
    ),
    
    active = list(
        
        outcome.for.data = function(value)
        {
            if (missing(value))
            {
                private$i.outcome.for.data
            }
            else
                stop("Cannot modify a jheem.likelihood.instruction's 'outcome.for.data' - it is read-only")
        },
        denominator.outcome.for.data = function(value)
        {
            if (missing(value))
            {
                private$i.denominator.outcome.for.data
            }
            else
                stop("Cannot modify a jheem.likelihood.instruction's 'denominator.outcome.for.data' - it is read-only")
        },
        denominator.outcome.for.sim = function(value)
        {
            if (missing(value))
            {
                private$i.denominator.outcome.for.sim
            }
            else
                stop("Cannot modify a jheem.likelihood.instruction's 'denominator.outcome.for.sim' - it is read-only")
        },
        outcome.for.n.multipliers = function(value)
        {
            if (missing(value))
            {
                private$i.outcome.for.n.multipliers
            }
            else
                stop("Cannot modify a jheem.likelihood.instruction's 'outcome.for.n.multipliers' - it is read-only")
        },
        from.year = function(value)
        {
            if (missing(value))
            {
                private$i.from.year
            }
            else
                stop("Cannot modify a jheem.likelihood.instruction's 'from.year' - it is read-only")
        },
        to.year = function(value)
        {
            if (missing(value))
            {
                private$i.to.year
            }
            else
                stop("Cannot modify a jheem.likelihood.instruction's 'to.year' - it is read-only")
        },
        omit.years = function(value)
        {
            if (missing(value))
            {
                private$i.omit.years
            }
            else
                stop("Cannot modify a jheem.likelihood.instruction's 'omit.years' - they are read-only")
        },
        equalize.weight.by.year = function(value)
        {
            if (missing(value))
            {
                private$i.equalize.weight.by.year
            }
            else
                stop("Cannot modify a jheem.likelihood.instruction's 'equalize.weight.by.year' - it is read-only")
        },
        parameters = function(value)
        {
            if (missing(value)) {
                private$i.parameters
            }
            else
                stop("Cannot modify a jheem.likelihood.instruction's 'parameters' - they are read-only")
        },
        sources.to.use = function(value)
        {
            if (missing(value)) {
                private$i.sources.to.use
            }
            else
                stop("Cannot modify a jheem.likelihood.instruction's 'sources.to.use' - they are read-only")
        },
        exclude.denominator.ontology.names = function(value)
        {
            if (missing(value))
                private$i.exclude.denominator.ontology.names
            else
                stop(paste0("Cannot modify a jheem.likelihood.instruction's 'exclude.denominator.ontology.names' - they are read-only"))
        },
        redundant.location.threshold = function(value)
        {
            if (missing(value))
            {
                private$i.redundant.location.threshold
            }
            else
                stop("Cannot modify a jheem.likelihood.instruction's 'redundant.location.threshold' - it is read-only")
        },
        location.types = function(value)
        {
            if (missing(value))
            {
                private$i.location.types
            }
            else
                stop("Cannot modify a jheem.likelihood.instruction's 'location.types' - it is read-only")
        },
        minimum.geographic.resolution.type = function(value)
        {
            if (missing(value))
            {
                private$i.minimum.geographic.resolution.type
            }
            else
                stop("Cannot modify a jheem.likelihood.instruction's 'minimum.geographic.resolution.type' - it is read-only")
        },
        maximum.locations.per.type = function(value)
        {
            if (missing(value))
            {
                private$i.maximum.locations.per.type
            }
            else
                stop("Cannot modify a jheem.likelihood.instruction's 'maximum.locations.per.type' - it is read-only")
        },
        partitioning.function = function(value)
        {
            if (missing(value))
            {
                private$i.partitioning.function
            }
            else
                stop("Cannot modify a jheem.likelihood.instruction's 'partitioning.function' - it is read-only")
        },
        use.lognormal.approximation = function(value)
        {
            if (missing(value)) {
                private$i.use.lognormal.approximation
            }
            else
                stop("Cannot modify a jheem.basic.likelihood.instruction's 'use.lognormal.approximation' - it is read-only")
        },
        calculate.lagged.difference = function(value)
        {
            if (missing(value)) {
                private$i.calculate.lagged.difference
            }
            else
                stop("Cannot modify a jheem.basic.likelihood.instruction's 'calculate.lagged.difference' - it is read-only")
        }
        
        
    ),
    
    private = list(
        
        i.location.types = NULL,
        i.minimum.geographic.resolution.type = NULL,
        i.maximum.locations.per.type = NULL,
        
        i.outcome.for.data = NULL,
        i.denominator.outcome.for.data = NULL,
        i.denominator.outcome.for.sim = NULL,
        i.outcome.for.n.multipliers = NULL,
        i.from.year = NULL,
        i.to.year = NULL,
        i.omit.years = NULL,
        i.equalize.weight.by.year = NULL,
        i.get.measurement.error.sd.from.data = NULL,
        
        i.parameters = NULL,
        i.sources.to.use = NULL,
        i.exclude.denominator.ontology.names = NULL,
        i.redundant.location.threshold = NULL,
        
        i.partitioning.function = NULL,
        i.use.lognormal.approximation = NULL,
        i.calculate.lagged.difference = NULL
    )
)

JHEEM.NESTED.PROPORTION.LIKELIHOOD = R6::R6Class(
    'jheem.nested.proportion.likelihood',
    inherit = JHEEM.LIKELIHOOD,
    portable = F,
    
    public = list(
        
        initialize = function(instructions,
                              version,
                              location,
                              sub.version,
                              data.manager,
                              throw.error.if.no.data,
                              error.prefix)
        {
            super$initialize(instructions = instructions,
                             version = version,
                             sub.version = sub.version,
                             location = location,
                             error.prefix = error.prefix)
            
            post.time.checkpoint.flag = F
            
            # Validate *data.manager*, a 'jheem.data.manager' object
            if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
                stop(paste0(error.prefix, "'data.manager' must be an R6 object with class 'jheem.data.manager'"))
            
            # --- UNPACK INSTRUCTIONS --- #
            # browser()
            if (post.time.checkpoint.flag) print(paste0("Start time: ", Sys.time()))
            
            private$i.location = location
            private$i.version = version
            private$i.sub.version = sub.version
            
            private$i.parameters = instructions$parameters
            private$i.outcome.for.data = instructions$outcome.for.data
            private$i.denominator.outcome.for.data = instructions$denominator.outcome.for.data
            private$i.denominator.outcome.for.sim = instructions$denominator.outcome.for.sim
            private$i.outcome.for.n.multipliers = instructions$outcome.for.n.multipliers
            
            private$i.partitioning.function = instructions$partitioning.function
            
            private$i.within.location.p.error.correlation = private$i.parameters$within.location.p.error.correlation
            private$i.within.location.n.error.correlation = private$i.parameters$within.location.n.error.correlation
            
            private$i.use.lognormal.approximation = instructions$use.lognormal.approximation
            private$i.calculate.lagged.difference = instructions$calculate.lagged.difference
            
            private$i.sources = instructions$sources
            private$i.exclude.denominator.ontology.names = instructions$exclude.denominator.ontology.names
            
            # Find years we have data for. Note: this does not guarantee that we'll have data for *our* locations, especially since we don't know what counties they'll be made up of yet.
            # If we get to n-multipliers and don't have enough data, I'll just throw an error and the user can change the year range that's used.
            years = get.likelihood.years(from.year = instructions$from.year,
                                         to.year = instructions$to.year,
                                         omit.years = instructions$omit.years,
                                         data.manager = data.manager,
                                         outcome.for.data = private$i.outcome.for.data)
            years.n = get.likelihood.years(from.year = instructions$from.year,
                                           to.year = instructions$to.year,
                                           omit.years = instructions$omit.years,
                                           data.manager = data.manager,
                                           outcome.for.data = private$i.denominator.outcome.for.data,
                                           exclude.ontology.names = private$i.exclude.denominator.ontology.names)
            years = intersect(years, years.n)
            
            if (!is.null(private$i.outcome.for.n.multipliers)) {
                years.n.multipliers = get.likelihood.years(from.year = instructions$from.year,
                                                           to.year = instructions$to.year,
                                                           omit.years = instructions$omit.years,
                                                           data.manager = data.manager,
                                                           outcome.for.data = private$i.outcome.for.n.multipliers)
                years = intersect(years, years.n.multipliers)
            }
            
            # --- VALIDATE THAT OUTCOME.FOR.SIM IS A PROPORTION --- #
            sim.metadata = get.simulation.metadata(version=version,
                                                   location=location,
                                                   from.year = years[[1]],
                                                   to.year = years[[length(years)]])
            if (sim.metadata$outcome.metadata[[private$i.outcome.for.sim]]$scale != 'proportion')
                stop(paste0(error.prefix, "'outcome.for.sim' must have scale 'proportion' in this specification"))
            
            if(!(sim.metadata$outcome.metadata[[private$i.outcome.for.sim]]$is.cumulative))
                stop(paste0(error.prefix, "'outcome.for.sim' must be cumulative in this specification"))
            
            all.locations = private$get.all.locations(location = location,
                                                      location.types = instructions$location.types,
                                                      maximum.locations.per.type = instructions$maximum.locations.per.type,
                                                      minimum.geographic.resolution.type = instructions$minimum.geographic.resolution.type,
                                                      data.manager = data.manager,
                                                      years = years)
            # all.locations = c('24510', 'C.12580', 'MD')
            
            ## ---- PREPARE DATA STRUCTURES ---- ##
            
            private$i.sim.ontology = sim.metadata$outcome.ontologies[[private$i.outcome.for.sim]]
            private$i.sim.ontology$year = as.character(years)
            private$i.sim.ontology$location = all.locations
            
            private$i.obs.p = c()
            private$i.details = c() # will contain each observation's sorted details as a collapsed character factor
            private$i.metadata = data.frame(year = character(0),
                                            stratum = character(0),
                                            source = character(0))
            mappings.list = list()
            dimnames.list = list()
            locations.list = list()
            remove.mask.list = list()
            
            private$i.transformation.matrix = NULL
            private$i.sim.required.dimnames = list()
            remove.mask = c()
            data.keep.dimensions = c()
            
            
            ## ---- PULL DATA ---- ##
            if (post.time.checkpoint.flag) print(paste0("Begin pulling: ", Sys.time()))
            # browser()
            n.stratifications.with.data = 0
            for (strat in private$i.stratifications) {
                keep.dimensions = c('year', 'location')
                if (!identical(strat, "")) keep.dimensions = c(keep.dimensions, strat)
                data = data.manager$pull(outcome = private$i.outcome.for.data,
                                         sources = private$i.sources.to.use,
                                         keep.dimensions = keep.dimensions,
                                         dimension.values = list(year = as.character(years), location = all.locations),
                                         target.ontology = private$i.sim.ontology,
                                         allow.mapping.from.target.ontology = T,
                                         append.attributes = 'details')
                if (is.null(data)) {
                    if (throw.error.if.no.data)
                        stop(paste0(error.prefix, "no data was found for the stratification '", strat, "'"))
                    else next
                }
                
                one.mapping = attr(data, 'mapping')
                one.details = attr(data, 'details')
                
                ## Pull measurement error variance if needed
                if (instructions$parameters$p.error.variance.type %in% c('data.sd', 'data.variance', 'data.cv')) {
                    metric.map = list(data.sd='sd', data.variance='variance', data.cv='coefficient.of.variance')
                    p.error.data = data.manager$pull(outcome = private$i.outcome.for.data,
                                                     metric = metric.map[[private$i.parameters$p.error.variance.type]],
                                                     sources = private$i.sources.to.use,
                                                     keep.dimensions = keep.dimensions,
                                                     dimension.values = list(year = as.character(years), location = all.locations),
                                                     target.ontology = private$i.sim.ontology,
                                                     allow.mapping.from.target.ontology = T)
                    
                    if (is.null(p.error.data)) {
                        if (throw.error.if.no.data)
                            stop(paste0(error.prefix, "no ", metric.map[[private$i.parameters$p.error.variance.type]], ", data was found for the stratification '", strat, "'"))
                        else next
                    }
                    
                    # find overlapping dimnames, limit both, then flip data to NA if cv for that value is NA
                    common.dimnames = get.dimension.values.overlap(dimnames(data), dimnames(p.error.data))
                    p.error.data = array.access(p.error.data, common.dimnames)
                    data = array.access(data, common.dimnames)
                    
                    if (instructions$parameters$p.error.variance.type == 'data.cv')
                        p.error.data = data * p.error.data # sd = cv * mean
                    
                    data[is.na(p.error.data)] = NA
                    one.details = array.access(one.details, common.dimnames)
                    
                    one.p.error.data = as.numeric(p.error.data) # na masks will align perfectly with obs p mask
                    one.p.error.data = one.p.error.data[!is.na(one.p.error.data)]
                    
                    if (instructions$parameters$p.error.variance.type == 'data.variance')
                        one.p.error.data = sqrt(one.p.error.data)
                    
                    private$i.p.error.vector = c(private$i.p.error.vector, one.p.error.data)
                }
                
                # If we have lognormal approximation on, we should transform the observations right now, after converting zeroes to NA so that they are ignored in the same ways.
                if (private$i.use.lognormal.approximation) {
                    data[data==0]=NA
                    # We do NOT log transform here, at instantiate time, because the fancy function during compute won't like it
                }
                
                n.stratifications.with.data = n.stratifications.with.data + 1
                one.dimnames = dimnames(data)
                one.locations = dimnames(data)$location
                one.obs.p = as.numeric(data)
                
                one.remove.mask = is.na(one.obs.p)
                one.obs.p = one.obs.p[!one.remove.mask]
                one.details = one.details[!one.remove.mask]
                
                # Metadata will involve melting both arrays (data and details) as well as making "stratum"
                one.metadata = reshape2::melt(data)
                one.metadata = one.metadata[!one.remove.mask,]
                
                # Recover required dimnames from one.metadata -- note that year won't be fixed yet
                # if (identical(strat, "risk")) browser()
                one.sim.required.dimnames = one.mapping$get.required.from.dim.names(lapply(one.metadata[!(colnames(one.metadata) %in% c('location', 'source', 'value'))],
                                                                                                function(x) {sort(as.character(unique(x)))})) ## MONITOR THIS SORT FOR BUGS
                
                # Convert any year ranges in metadata to single years using their median year
                one.metadata$year = sapply(one.metadata$year, function(year.or.year.range) {
                    year.or.year.range = as.character(year.or.year.range)
                    if (is.year.range(year.or.year.range)) {
                        parsed.year.range = parse.year.ranges(year.or.year.range)
                        return(as.character(ceiling(mean(c(as.numeric(parsed.year.range$start), as.numeric(parsed.year.range$end))))))
                    }
                    else return(year.or.year.range)
                })
                new.years = sort(unique(one.metadata$year))
                
                # Do the same for the dimnames
                one.dimnames$year = sapply(one.dimnames$year, function(year.or.year.range) {
                    year.or.year.range = as.character(year.or.year.range)
                    if (is.year.range(year.or.year.range)) {
                        parsed.year.range = parse.year.ranges(year.or.year.range)
                        return(as.character(ceiling(mean(c(as.numeric(parsed.year.range$start), as.numeric(parsed.year.range$end))))))
                    }
                    else return(year.or.year.range)
                })
                names(one.dimnames$year) = NULL # a necessary step because the old year ranges were becoming names
                
                one.sim.required.dimnames$year = new.years
                
                one.metadata = one.metadata[, sort(colnames(one.metadata))]
                one.metadata['stratum'] = do.call(paste, c(subset.data.frame(one.metadata, select=-c(location, year, source, value)), sep="__"))
                one.metadata[is.na(one.metadata$stratum), 'stratum'] = ".TOTAL." # I can change this to "" instead of ".TOTAL.", can't I?
                one.metadata['dimensions'] = paste0(strat, collapse="__")
                one.metadata = subset.data.frame(one.metadata, select = c(location, year, stratum, dimensions, source))
                
                # Find the required.dimnames
                for (d in names(one.sim.required.dimnames)) {
                    if (!(d %in% names(private$i.sim.required.dimnames)))
                        private$i.sim.required.dimnames = c(private$i.sim.required.dimnames, setNames(list(one.sim.required.dimnames[[d]]), d))
                    else
                        private$i.sim.required.dimnames[[d]] = sort(union(private$i.sim.required.dimnames[[d]], one.sim.required.dimnames[[d]]))
                }
                
                # Convert one.details list of vectors to a list of characters of collapsed sorted details, then unlist to a vector
                one.details = unlist(lapply(one.details, function(v) {paste(sort(v), collapse="__")}))
                
                private$i.obs.p = c(private$i.obs.p, one.obs.p)
                private$i.details = c(private$i.details, one.details)
                private$i.metadata = rbind(private$i.metadata, one.metadata)
                mappings.list = c(mappings.list, list(one.mapping))
                dimnames.list = c(dimnames.list, list(one.dimnames))
                locations.list = c(locations.list, list(one.locations))
                remove.mask.list = c(remove.mask.list, list(one.remove.mask))
                
                # data.keep.dimensions = union(data.keep.dimensions, keep.dimensions) # Why do I do this?? Maybe I don't need a dimension?
                
            }
            private$i.n.obs = length(private$i.obs.p)
            if (private$i.n.obs==0) stop(paste0(error.prefix, "no data was found for any stratification"))
            
            if (post.time.checkpoint.flag) print(paste0("Finish pulling: ", Sys.time()))

            # Find redundant locations (have at most only a few more data points than the main location does) and remove them, only if we have data for our main location
            redundant.locations = NULL
            if (location %in% private$i.metadata$location) {
                redundant.locations = private$get.redundant.locations(location, private$i.metadata, extra.points.needed.to.keep = instructions$redundant.location.threshold)
                # Check if we even have data for more than just the main location.
                if (length(setdiff(all.locations, redundant.locations))==1)
                    stop(paste0(error.prefix, "data only found for main location after removing redundant locations"))
            }
            redundant.locations.mask = private$i.metadata$location %in% redundant.locations
            private$i.obs.p = private$i.obs.p[!redundant.locations.mask]
            private$i.metadata = private$i.metadata[!redundant.locations.mask,]
            private$i.details = private$i.details[!redundant.locations.mask]
            
            # The remove mask needs to be updated -- carefully -- because it is in reference to the state before removal and will be used later on for the transformation mapping matrix.
            # Since location is the second dimension after year in all the arrays pulled, n.years times n.locations is the size of a block repeated a certain number of times
            year.location.block.counts = sapply(dimnames.list, function(x) {prod(sapply(x, length)[!(names(x) %in% c('year', 'location'))])})
            empty.locations = setdiff(unique(unlist(locations.list)), unique(private$i.metadata$location)) # just throwing these in instead of duplicating code
            removed.locations.list = lapply(locations.list, function(x) {x %in% union(redundant.locations, empty.locations)})
            redundant.locations.full.size.mask = lapply(1:length(dimnames.list), function(i) {rep(rep(removed.locations.list[[i]], each=length(dimnames.list[[i]]$year)), year.location.block.counts[[i]])})
            
            # Now this shows which elements in each remove.mask.list element correspond to redundant locations. We remove them from the remove mask list and remove mask.
            remove.mask.list = lapply(1:length(remove.mask.list), function(i) {remove.mask.list[[i]][!redundant.locations.full.size.mask[[i]]]})
            locations.list = lapply(1:length(locations.list), function(i) {locations.list[[i]][!removed.locations.list[[i]]]})

            # Now the required dimnames may have their dimensions in the wrong order or their values in the wrong order. They may also lack some values from a complete dimension. Use the sim ontology to fix this.
            private$i.sim.ontology$location = union(as.vector(unique(private$i.metadata$location)), location) # location MUST be here to ensure it counts as an obs location, even if no obs exist for it
            corrected.sim.required.dimnames = private$i.sim.ontology[names(private$i.sim.ontology) %in% names(private$i.sim.required.dimnames)]
            corrected.sim.required.dimnames$year = sort(private$i.sim.required.dimnames$year)
            private$i.sim.required.dimnames = corrected.sim.required.dimnames
            private$i.sim.ontology$year = private$i.sim.required.dimnames$year
            
            private$i.details = as.factor(private$i.details)
            # private$i.metadata$location = as.factor(private$i.metadata$location) # already factor somehow
            if (private$i.parameters$observation.correlation.form == 'autoregressive.1') {
                private$i.metadata$year = suppressWarnings(as.numeric(private$i.metadata$year))
                if (any(is.na(private$i.metadata$year)))
                    stop(paste0(error.prefix, "'observation.correlation.form' 'autoreggresive.1' can only be used with single-year data points"))
            }
            else private$i.metadata$year = as.factor(private$i.metadata$year)
            private$i.metadata$stratum = as.factor(private$i.metadata$stratum)
            # private$i.metadata$source = as.factor(private$i.metadata$source) # already factor somehow
            
            # might remove years
            private$i.sim.dimension.values = private$i.sim.required.dimnames[sapply(names(private$i.sim.required.dimnames), function(d) {!identical(private$i.sim.required.dimnames[[d]], private$i.sim.ontology[[d]])})]
            private$i.sim.dimension.values$year = private$i.sim.required.dimnames$year
            private$i.sim.dimension.values = as.list(private$i.sim.dimension.values)
            
            # Make sure sim keep dimensions has year because the sim$get relies on it later
            private$i.sim.keep.dimensions = union('year', names(private$i.sim.required.dimnames))
            
            # Reorder -- everything should be in the order of the sim.ontology so that it aligns with the sim$get arrays later.
            private$i.sim.keep.dimensions = names(private$i.sim.ontology)[sort(sapply(private$i.sim.keep.dimensions, function(d) {which(names(private$i.sim.ontology) == d)}))]
            
            # years.with.data = get.range.robust.year.intersect(private$i.sim.required.dimnames$year, as.character(sort(unique(private$i.metadata$year))))
            years.with.data = private$i.sim.required.dimnames$year
            n.years = length(years.with.data)
            private$i.obs.year.index = sapply(private$i.metadata$year, function(y) {which(years.with.data == y)})
            
            ## ---- GENERATE LAGGED PAIRS IF REQUESTED ---- ##
            if (private$i.calculate.lagged.difference) {
                year.for.lag = suppressWarnings(as.numeric(private$i.metadata$year))
                if (any(is.na(year.for.lag)))
                    stop(paste0(error.prefix, "'calculate.lagged.difference' can only be used with single-year data points"))
                private$i.lagged.pairs = generate_lag_matrix_indices(year.for.lag,# check if valid -- no year ranges, please!
                                                                     as.integer(as.factor(private$i.metadata$location)), # location not used for basic likelihoods but is available for nested prop likelihoods
                                                                     as.integer(as.factor(private$i.metadata$stratum)),
                                                                     as.integer(as.factor(private$i.metadata$source)),
                                                                     private$i.n.obs)
                if (length(private$i.lagged.pairs)==0)
                    stop(paste0(error.prefix, "no data found for lagged-year pairs"))
                private$i.n.lagged.obs = length(private$i.lagged.pairs)/2
                
                # Keep only the latter years for each pair
                private$i.metadata.for.lag = private$i.metadata[private$i.lagged.pairs[rep(c(T,F), private$i.n.lagged.obs)] + 1, ]
            }
            
            # Generate transformation matrix
            if (post.time.checkpoint.flag) print(paste0("Generate transformation matrix: ", Sys.time()))
            private$i.transformation.matrix = generate.transformation.matrix.nested(dimnames.list, locations.list, remove.mask.list, n.stratifications.with.data, private$i.sim.required.dimnames, all.locations = private$i.sim.ontology$location)
            # browser()
            
            # data frame of model strata, unless we only have totals
            model.stratification = private$i.sim.keep.dimensions[!(private$i.sim.keep.dimensions %in% c('year', 'location'))]
            if (length(model.stratification)==0) {
                model.stratification = NULL
                model.strata = NULL
                n.strata = 1
            } else {
                model.strata = expand.grid(private$i.sim.ontology[names(private$i.sim.ontology) %in% model.stratification])
                n.strata = nrow(model.strata)
            }
            
            if (post.time.checkpoint.flag) print(paste0("Generate measurement error correlation matrix: ", Sys.time()))
            
            measurement.error.correlation.matrix = get_obs_error_correlation_matrix(rep(1, private$i.n.obs**2),
                                                                                    private$i.n.obs,
                                                                                    as.numeric(private$i.metadata$location),
                                                                                    as.numeric(private$i.metadata$year),
                                                                                    as.numeric(private$i.metadata$stratum),
                                                                                    as.numeric(private$i.metadata$source),
                                                                                    as.numeric(private$i.details),
                                                                                    private$i.parameters$correlation.different.locations,
                                                                                    private$i.parameters$correlation.different.year,
                                                                                    private$i.parameters$correlation.different.strata,
                                                                                    private$i.parameters$correlation.different.source,
                                                                                    private$i.parameters$correlation.same.source.different.details,
                                                                                    private$i.parameters$observation.correlation.form == "autoregressive.1")
            # All forms of error will be converted to sd and then we use cov = corr * sd %*% t(sd), the last part sometimes being just sd squared
            if (private$i.parameters$p.error.variance.type %in% c('data.sd', 'data.variance', 'data.cv')) {
                # all have been converted to sd earlier, including data.cv
                measurement.error.sd.matrix = private$i.p.error.vector %*% t(private$i.p.error.vector)
                private$i.obs.error = measurement.error.correlation.matrix * measurement.error.sd.matrix
            }
            else if (private$i.parameters$p.error.variance.type == 'sd')
                private$i.obs.error = measurement.error.correlation.matrix * private$i.parameters$p.error.variance.term ^ 2 # this reflects our choice to make measurement error sd constant, not scaling with level of suppression (or other p)
            else if (private$i.parameters$p.error.variance.type == 'variance')
                private$i.obs.error = measurement.error.correlation.matrix * private$i.parameters$p.error.variance.term
            else if (private$i.parameters$p.error.variance.type == 'cv') {
                measurement.error.sd = private$i.obs.p * private$i.parameters$p.error.variance.term
                private$i.obs.error = measurement.error.correlation.matrix * (measurement.error.sd %*% t(measurement.error.sd))
            }
            dim(private$i.obs.error) = c(private$i.n.obs, private$i.n.obs)

            # ------ THINGS THAT DEPEND ON OBSERVATION-LOCATIONS ------ #
            observation.locations = union(as.vector(unique(private$i.metadata$location)), location) #otherwise is factor
            n.obs.locations = length(observation.locations) # we have ensured the main location is always in here because it will be used for metalocations and therefore must be accounted for everywhere else too
            
            locations.possibly.with.n.data = dimnames(data.manager$pull(outcome = private$i.denominator.outcome.for.data,
                                                                        keep.dimensions = c('year', 'location'), # used to also include the model.stratification in keep.dimensions, a sometimes impossible ask!
                                                                        dimension.values = list(location = setdiff(observation.locations, location), year = years.with.data)))$location
            if (is.null(locations.possibly.with.n.data))
                stop(paste0(error.prefix, "'", private$i.denominator.outcome.for.data, "' could not be found for the required observation locations"))
            if (post.time.checkpoint.flag) print(paste0("Calculate obs.n: ", Sys.time()))
            
            obs.n.info = private$get.obs.n(data.manager = data.manager,
                                           stratification = model.stratification, # may be character(0)
                                           locations.with.n.data = locations.possibly.with.n.data,
                                           years.with.data = years.with.data,
                                           outcome.for.n = private$i.denominator.outcome.for.data,
                                           sim.ontology = private$i.sim.ontology,
                                           model.strata = model.strata, # may be NULL
                                           partitioning.function = private$i.partitioning.function,
                                           version = version,
                                           location = location,
                                           error.prefix = error.prefix)
            private$i.obs.n = obs.n.info$obs.n
            locations.with.n.data = obs.n.info$locations.with.n.data
            # missing stuff also stored in info
            # browser()
            obs.n.cv = NULL
            obs.n.variance.inflation.if.estimated = NULL

            # The T matrix is currently in dimensions responses x year-location-stratum
            # Now, split it into responses x year x location x stratum
            dim(private$i.transformation.matrix) = c(private$i.n.obs, n.years, n.obs.locations, n.strata)
            
            if (post.time.checkpoint.flag) print(paste0("Generate year loc stratum to obs mapping: ", Sys.time()))
            
            private$i.year.loc.stratum.to.obs.mapping = lapply(1:n.strata, function(i) {
                # drop the stratum dimension, which is last, but don't drop any other dimensions, like year, if they happen to have only one value
                stratum.transformation.array = private$i.transformation.matrix[,,,i,drop=F]
                dim(stratum.transformation.array) = dim(stratum.transformation.array)[c(T,T,T,F)]
                
                # flatten year and location to a matrix
                dim(stratum.transformation.array) = c(private$i.n.obs, n.years * n.obs.locations)
                stratum.transformation.array
            })
            
            # ------ THINGS THAT DEPEND ON METALOCATION INFO ------ #
            metalocation.info = private$get.metalocations(location = location,
                                                          observation.locations = observation.locations,
                                                          minimum.geographic.resolution.type = instructions$minimum.geographic.resolution.type)
            metalocation.type = metalocation.info$metalocation.type
            metalocation.to.minimal.component.map = metalocation.info$metalocation.to.minimal.component.map
            n.metalocations = length(metalocation.type)
            metalocation.to.obs.location.mapping = metalocation.info$metalocation.to.obs.location.mapping
            
            private$i.year.metalocation.to.year.obs.location.mapping = array(0, dim = c(n.years, n.obs.locations, n.years, n.metalocations))
            for (y in 1:n.years) {
                private$i.year.metalocation.to.year.obs.location.mapping[y,,y,] = metalocation.to.obs.location.mapping
            }
            dim(private$i.year.metalocation.to.year.obs.location.mapping) = c(n.obs.locations*n.years, n.metalocations*n.years)
            private$i.year.metalocation.to.year.obs.location.mask = apply(private$i.year.metalocation.to.year.obs.location.mapping != 0, 2, any)
            private$i.year.metalocation.to.year.obs.location.mapping =
                private$i.year.metalocation.to.year.obs.location.mapping[,private$i.year.metalocation.to.year.obs.location.mask]

            private$i.year.metalocation.to.obs.mapping = lapply(1:n.strata, function(i) {
                private$i.year.loc.stratum.to.obs.mapping[[i]] %*% private$i.year.metalocation.to.year.obs.location.mapping
            })

            year.metalocation.to.year.obs.n.mapping.per.stratum = array(0, dim = c(n.years, length(locations.with.n.data) + 1, n.years, n.metalocations))
            metalocation.info.for.conditioning = metalocation.info$metalocation.to.obs.location.mapping[c(locations.with.n.data, location),]
            metalocation.info.for.conditioning = metalocation.info.for.conditioning[, apply(metalocation.info.for.conditioning!=0, 2, any)]
            n.metalocations.for.conditioning = ncol(metalocation.info.for.conditioning)
            for (y in 1:n.years) {
                year.metalocation.to.year.obs.n.mapping.per.stratum[y,,y,] = metalocation.info.for.conditioning
            }
            dim(year.metalocation.to.year.obs.n.mapping.per.stratum) = c((length(locations.with.n.data) + 1) * n.years, n.metalocations.for.conditioning * n.years)
            private$i.year.metalocation.to.year.obs.n.mapping = rep(list(year.metalocation.to.year.obs.n.mapping.per.stratum), n.strata)
            metalocation.info.for.conditioning.without.msa = metalocation.info.for.conditioning[locations.with.n.data,]
            
            # --- N MULTIPLIERS --- #
            if (post.time.checkpoint.flag) print(paste0("Calculate n multipliers: ", Sys.time()))
            private$i.year.metalocation.n.multipliers = private$get.n.multipliers(metalocation.to.minimal.component.map = metalocation.to.minimal.component.map,
                                                                                  metalocation.type = metalocation.type,
                                                                                  main.location = location,
                                                                                  stratification = model.stratification, # can be NULL
                                                                                  sim.ontology = private$i.sim.ontology,
                                                                                  model.strata = model.strata, # can be NULL
                                                                                  data.manager = data.manager,
                                                                                  outcome = private$i.outcome.for.n.multipliers,
                                                                                  years = years.with.data,
                                                                                  error.prefix = error.prefix)
            
            private$i.year.metalocation.n.multiplier.sd = lapply(private$i.year.metalocation.n.multipliers, function(mult) {
                mult * private$i.parameters$n.multiplier.cv
            })
            
            # --- P.BIAS --- #
            if (post.time.checkpoint.flag) print(paste0("Generate p bias matrices: ", Sys.time()))
            private$i.year.metalocation.p.bias = private$get.p.bias.matrices(n.strata = n.strata,
                                                                             metalocation.type = metalocation.type,
                                                                             n.years = n.years,
                                                                             p.bias.in = instructions$parameters$p.bias.inside.location,
                                                                             p.bias.out = instructions$parameters$p.bias.outside.location)
            private$i.year.metalocation.p.sd = private$get.p.bias.matrices(n.strata = n.strata,
                                                                           metalocation.type = metalocation.type,
                                                                           n.years = n.years,
                                                                           p.bias.in = instructions$parameters$p.bias.sd.inside.location,
                                                                           p.bias.out = instructions$parameters$p.bias.sd.outside.location)
            
            # --- CONDITIONING --- #
            if (post.time.checkpoint.flag) print(paste0("Do conditioning: ", Sys.time()))
            private$i.obs.n.plus.conditioned.error.variances = lapply(1:n.strata, function(i) {
                obs.n.variance = sapply(private$i.obs.n[[i]], function(x) {x * private$i.parameters$n.error.variance.term}) # HERE
                year.mask = array(0, dim = c(n.years, length(locations.with.n.data), n.years, n.metalocations.for.conditioning))
                for (y in 1:n.years) {
                    year.mask[y,,y,] = metalocation.info.for.conditioning.without.msa
                }
                variance.arr = array(rep(obs.n.variance, n.metalocations.for.conditioning * n.years), dim = c(n.years, length(locations.with.n.data), n.years, n.metalocations.for.conditioning))
                variance.arr[!year.mask] = 0
                dim(variance.arr) = c(length(locations.with.n.data) * n.years, n.metalocations.for.conditioning * n.years)
                
                # now add a bunch of zero rows for the msa
                rbind(variance.arr, matrix(0, nrow = n.years, ncol = dim(variance.arr)[[2]]))
            })
            
            private$i.year.metalocation.to.year.condition.on.location.mask = rep(metalocation.info$metalocation.to.obs.location.mapping[location,], each=n.years)
            private$i.year.metalocation.to.year.condition.on.location.mapping = private$i.year.metalocation.to.year.obs.n.mapping[[1]][dim(private$i.year.metalocation.to.year.obs.n.mapping[[1]])[[1]] - (n.years:1) + 1, private$i.year.metalocation.to.year.condition.on.location.mask]
            if (!is.matrix(private$i.year.metalocation.to.year.condition.on.location.mapping))
                dim(private$i.year.metalocation.to.year.condition.on.location.mapping) = c(1, length(private$i.year.metalocation.to.year.condition.on.location.mapping))
            
            # --- INVERSE VARIANCE WEIGHTS MATRIX --- #
            
            ## included multiplier to make inverse multiplier matrix times covariance matrix
            if (!is.null(private$i.parameters$included.multiplier)) {
                
                if (!is.null(names(private$i.parameters$included.multiplier))) {
                    
                    if (any(!(private$i.metadata$year %in% names(private$i.parameters$included.multiplier))))
                        stop(paste0(error.prefix, "all years values in data must have a corresponding 'included.multiplier'"))
                    
                    included.multiplier.vector = sapply(private$i.metadata$year, function(obs.year) {private$i.parameters$included.multiplier[obs.year]})
                    included.multiplier.sd.vector = sapply(private$i.metadata$year, function(obs.year) {private$i.parameters$included.multiplier.sd[obs.year]})
                }
                else {
                    included.multiplier.vector = rep(private$i.parameters$included.multiplier, private$i.n.obs)
                    included.multiplier.sd.vector = rep(private$i.parameters$included.multiplier.sd, private$i.n.obs)
                }
                
                inverse.multiplier.matrix = (1/included.multiplier.vector) %*% t(1/included.multiplier.vector)
                
                # AR.1 cannot be selected if we have year ranges because year ranges do not have distance measures
                if (private$i.parameters$included.multiplier.correlation.form == "autoregressive.1" && any(is.year.range(private$i.metadata$year)))
                    stop(paste0(error.prefix, "instructions cannot use 'autoregressive.1' for 'included.multiplier.correlation.form' since observations with year ranges were found"))
                
                multiplier.correlation.matrix = get_multiplier_correlation_matrix(rep(1, private$i.n.obs**2),
                                                                                  private$i.n.obs,
                                                                                  as.numeric(private$i.metadata$year),
                                                                                  private$i.parameters$included.multiplier.correlation,
                                                                                  private$i.parameters$included.multiplier.correlation.structure == "autoregressive.1")
                multiplier.covariance.matrix = multiplier.correlation.matrix * included.multiplier.sd.vector %*% t(included.multiplier.sd.vector)
                private$i.inverse.multiplier.matrix.times.cov.mat = inverse.multiplier.matrix * multiplier.covariance.matrix
            }
            ##
            
            
            private$i.metadata$stratum = as.character(private$i.metadata$stratum)
            if (post.time.checkpoint.flag) print(paste0("Generate inverse variance weights matrix: ", Sys.time()))
            private$i.inverse.variance.weights.matrix = generate.inverse.variance.weights.matrix(obs.vector = private$i.obs.p,
                                                                                                 equalize.weight.by.year = instructions$equalize.weight.by.year,
                                                                                                 metadata = private$i.metadata,
                                                                                                 weights = private$i.weights)
            
            # --- SAVE SIM$GET INSTRUCTIONS --- #
            private$i.optimized.get.instructions = list()
            private$i.optimized.get.instructions[['sim.p.instr']] = sim.metadata$prepare.optimized.get.instructions(outcome = private$i.outcome.for.sim,
                                                                                                                    keep.dimensions = private$i.sim.keep.dimensions,
                                                                                                                    dimension.values = private$i.sim.dimension.values,
                                                                                                                    drop.single.sim.dimension = T)
            if (is.null(private$i.denominator.outcome.for.sim))
                private$i.optimized.get.instructions[['sim.n.instr']] = sim.metadata$prepare.optimized.get.instructions(outcome = private$i.outcome.for.sim,
                                                                                                                        keep.dimensions = private$i.sim.keep.dimensions,
                                                                                                                        dimension.values = private$i.sim.dimension.values,
                                                                                                                        output = 'denominator',
                                                                                                                        drop.single.sim.dimension = T)
            else
                private$i.optimized.get.instructions[['sim.n.instr']] = sim.metadata$prepare.optimized.get.instructions(outcome = private$i.denominator.outcome.for.sim,
                                                                                                                        keep.dimensions = private$i.sim.keep.dimensions,
                                                                                                                        dimension.values = private$i.sim.dimension.values,
                                                                                                                        drop.single.sim.dimension = T)
            
            ptm = Sys.time()
            if (post.time.checkpoint.flag) print(paste0("End time: ", ptm))
            
        },
        get.outcome.location.mapping = function()
        {
            create.outcome.location.mapping(location.mappings = setNames(list(unique(as.character(private$i.metadata$location))), private$i.location), # b/c is factor
                                            outcome.name = private$i.outcome.for.data,
                                            version = private$i.version,
                                            location = private$i.location,
                                            sub.version = private$i.sub.version)
        },
        check = function() {
            browser()
        }
    ),
    
    private = list(
        
        # FOR CPP ARGUMENTS
        i.year.metalocation.n.multipliers = NULL,
        i.year.metalocation.n.multiplier.sd = NULL,
        i.year.metalocation.p.bias = NULL,
        i.year.metalocation.p.sd = NULL,
        i.within.location.p.error.correlation = NULL,
        i.within.location.n.error.correlation = NULL,
        i.obs.n = NULL,
        i.year.metalocation.to.year.obs.n.mapping = NULL,
        i.obs.n.plus.conditioned.error.variances = NULL,
        i.year.metalocation.to.year.condition.on.location.mask = NULL,
        i.year.metalocation.to.year.condition.on.location.mapping = NULL,
        i.year.metalocation.to.year.obs.location.mask = NULL,
        i.year.metalocation.to.year.obs.location.mapping = NULL,
        i.year.loc.stratum.to.obs.mapping = NULL,
        i.year.metalocation.to.obs.mapping = NULL,
        i.obs.year.index = NULL,
        i.obs.p = NULL,
        i.obs.error = NULL,
        
        # OTHER
        
        i.location = NULL,
        i.version = NULL,
        i.sub.version = NULL,
        
        i.parameters = NULL,
        i.p.error.vector = NULL,
        
        i.outcome.for.data = NULL,
        i.denominator.outcome.for.data = NULL,
        i.denominator.outcome.for.sim = NULL,
        i.outcome.for.n.multipliers = NULL,
        
        i.optimized.get.instructions = NULL,
        
        i.sources = NULL,
        i.exclude.denominator.ontology.names = NULL,
        
        i.obs.vector = NULL,
        i.details = NULL,
        i.metadata = NULL,
        i.sim.ontology = NULL,
        i.sim.required.dimnames = NULL,
        i.sim.keep.dimensions = NULL,
        i.sim.dimension.values = NULL,
        i.transformation.matrix = NULL,
        i.inverse.variance.weights.matrix = NULL,
        i.inverse.multiplier.matrix.times.cov.mat = NULL,
        i.partitioning.function = NULL,
        
        i.n.obs = NULL,
        i.n.lagged.obs = NULL,
        i.use.lognormal.approximation = NULL,
        i.calculate.lagged.difference = NULL,
        i.lagged.pairs = NULL,
        i.metadata.for.lag = NULL,
        
        do.compute = function(sim, log=T, check.consistency=T, debug=F)
        {
            sim.p = sim$optimized.get(private$i.optimized.get.instructions[["sim.p.instr"]])
            if (is.null(private$i.denominator.outcome.for.sim))
                sim.n = sim$optimized.get(private$i.optimized.get.instructions[["sim.n.instr"]])
            else
                sim.n = sim$optimized.get(private$i.optimized.get.instructions[["sim.n.instr"]])
            
            flattened.dims = c(year = dim(sim.p)[['year']], stratum = prod(dim(sim.p)[names(dim(sim.p)) != 'year']))
            dim(sim.p) = flattened.dims
            dim(sim.n) = flattened.dims
            
            lik.components = get_nested_proportion_likelihood_components(p = sim.p,
                                                                         n = sim.n,
                                                                         year_metalocation_n_multipliers = private$i.year.metalocation.n.multipliers,
                                                                         year_metalocation_n_multiplier_sd = private$i.year.metalocation.n.multiplier.sd,
                                                                         year_metalocation_p_bias = private$i.year.metalocation.p.bias,
                                                                         year_metalocation_p_sd = private$i.year.metalocation.p.sd,
                                                                         
                                                                         metalocation_p_correlation = private$i.within.location.p.error.correlation,
                                                                         metalocation_n_multiplier_correlation = private$i.within.location.n.error.correlation,
                                                                         
                                                                         year_metalocation_to_year_obs_n_mapping = private$i.year.metalocation.to.year.obs.n.mapping,
                                                                         
                                                                         obs_n = private$i.obs.n,
                                                                         
                                                                         obs_n_plus_conditioned_error_variances = private$i.obs.n.plus.conditioned.error.variances,
                                                                         
                                                                         year_metalocation_to_year_condition_on_location_mask = private$i.year.metalocation.to.year.condition.on.location.mask,
                                                                         
                                                                         year_metalocation_to_year_condition_on_location_mapping = private$i.year.metalocation.to.year.condition.on.location.mapping,
                                                                         
                                                                         year_metalocation_to_year_obs_location_mask = private$i.year.metalocation.to.year.obs.location.mask,
                                                                         
                                                                         year_metalocation_to_year_obs_location_mapping = private$i.year.metalocation.to.year.obs.location.mapping,
                                                                         
                                                                         year_loc_stratum_to_obs_mapping = private$i.year.loc.stratum.to.obs.mapping,
                                                                         
                                                                         year_metalocation_to_obs_mapping = private$i.year.metalocation.to.obs.mapping,
                                                                         
                                                                         obs_year_index = private$i.obs.year.index,
                                                                         
                                                                         obs_p = private$i.obs.p,
                                                                         
                                                                         obs_error = private$i.obs.error)
            # print(Sys.time() - ptm)
            mean = lik.components$mean.v
            sigma = lik.components$cov.mat
            obs.vector = lik.components$obs.v
            
            sigma = sigma * private$i.inverse.variance.weights.matrix # inverse variance weights determined earlier...
            
            # Revise sigma if including multiplier
            if (!is.null(private$i.inverse.multiplier.matrix.times.cov.mat)) {
                sigma = sigma + private$i.inverse.multiplier.matrix.times.cov.mat * (sigma + mean %*% t(mean))
            }
            
            if (private$i.use.lognormal.approximation)
            {
                obs.vector = log(obs.vector) # this happens at instantiate time for the basic likelihood but must happen now because the fancy function doesn't eat log transformed ingredients
                mean.reciprocal = 1/mean
                sigma = log(mean.reciprocal %*% t(mean.reciprocal) * sigma + 1)
                mean = log(mean) - diag(sigma)/2
            }
            
            if (private$i.calculate.lagged.difference)
            {
                obs.vector = apply_lag_to_vector(obs.vector,
                                                 private$i.lagged.pairs,
                                                 rep(0, private$i.n.lagged.obs),
                                                 private$i.n.obs)
                mean = apply_lag_to_vector(mean,
                                           private$i.lagged.pairs,
                                           rep(0, private$i.n.lagged.obs),
                                           private$i.n.obs)
                sigma = apply_lag_to_matrix(sigma,
                                            private$i.lagged.pairs,
                                            rep(0, private$i.n.lagged.obs**2),
                                            private$i.n.obs)
                dim(sigma) = c(private$i.n.lagged.obs, private$i.n.lagged.obs)
                # mean = private$i.lag.matrix %*% mean
                # sigma = private$i.lag.matrix %*% sigma %*% private$i.transposed.lag.matrix # there is a more efficient way to do this if we know there are only two non-zero elements per row in lag matrix
                
                #to do at instantiate time: obs = private$i.lag.matrix %*% obs
            }
            
            likelihood = mvtnorm::dmvnorm(obs.vector,
                                          mean = mean,
                                          sigma = sigma,
                                          log = T,
                                          checkSymmetry = F)
            
            if (debug) {
                if (private$i.calculate.lagged.difference) {
                    obs.n = lik.components$obs.n
                    if (private$i.use.lognormal.approximation) {
                        obs.n = log(obs.n)
                    }
                    obs.n = apply_lag_to_vector(obs.n,
                                                private$i.lagged.pairs,
                                                rep(0, private$i.n.lagged.obs),
                                                private$i.n.obs)
                    if (private$i.use.lognormal.approximation)
                        lik.summary = cbind(obs.p=round(obs.vector, 3), mean.p = round(mean-obs.n, 3), sd.p = round(sqrt(diag(sigma))-obs.n, 3))
                    else
                        lik.summary = cbind(obs.p=round(obs.vector, 3), mean.p = round(mean/obs.n, 3), sd.p = round(sqrt(diag(sigma))/obs.n, 3))
                }
                else {
                    obs.n = lik.components$obs.n
                    lik.summary = cbind(private$i.metadata, obs.p =round(obs.vector/obs.n, 3), mean.p = round(mean/obs.n, 3), sd.p = round(sqrt(diag(sigma))/obs.n, 3))
                }
                browser()
            }
            likelihood 
            
        },
        
        # find all locations that we will check for data
        get.all.locations = function(location, location.types, maximum.locations.per.type, minimum.geographic.resolution.type, data.manager, years)
        {
            main.contained.locs = unlist(locations::get.location.code(locations::get.contained.locations(location, minimum.geographic.resolution.type), minimum.geographic.resolution.type))
            # This is slower than I expected
            sort(unique(unlist(lapply(location.types, function(type) {
                # An overly verbose way to get overlapping locations as codes rather than names -- should ask Jeff to improve interface
                locations.this.type = unlist(locations::get.location.code(locations::get.overlapping.locations(location, type), type))
                
                # if we have too many, compare them on the basis of how much denominator they have overlapping the main location
                if (length(locations.this.type) <= maximum.locations.per.type) return(locations.this.type)
                
                loc.denominators = sapply(locations.this.type, function(loc) {
                    contained.locs = unlist(locations::get.location.code(locations::get.contained.locations(loc, minimum.geographic.resolution.type), minimum.geographic.resolution.type))
                    if (is.null(contained.locs)) return(NULL)
                    overlapping.contained.locs = intersect(contained.locs, main.contained.locs)
                    denom.totals = data.manager$pull(outcome=private$i.denominator.outcome.for.data, keep.dimensions = 'year', dimension.values = list(location=overlapping.contained.locs))
                    if (is.null(denom.totals)) return(NULL)
                    return(mean(denom.totals, na.rm=T))
                })
                
                # Remove NULL locations, which had no data
                not.null.mask = !sapply(loc.denominators, is.null)
                locations.this.type = locations.this.type[not.null.mask]
                loc.denominators = loc.denominators[not.null.mask]
                loc.denominators = unlist(loc.denominators) # because any NULLs would have prevented the sapply from making it atomic

                return (locations.this.type[names(sort(loc.denominators, decreasing=T))][1:min(maximum.locations.per.type, length(loc.denominators))])
                
            }))))
        },
        
        get.redundant.locations = function(main.location, metadata, extra.points.needed.to.keep)
        { # have to say "main.location" instead of "location" because of subsetting by location==location
            # browser()
            redundant.locations = character(0)
            
            other.locations = setdiff(unique(metadata$location), main.location)
            if (length(other.locations)==0) return (redundant.locations)
            
            metadata = metadata[c('location', 'year', 'stratum')]
            metadata = metadata[!duplicated(metadata),]
            
            main.data.combinations = subset(metadata, subset = location==main.location)[c('year', 'stratum')]
            main.data.combinations = paste(main.data.combinations$year, main.data.combinations$stratum, sep="__")
            
            for (other.location in other.locations) {
                this.location.combinations = subset(metadata, location==other.location)[c('year', 'stratum')]
                this.location.combinations = paste(this.location.combinations$year, this.location.combinations$stratum, sep="__")
                amount.extra.data.this.location = setdiff(this.location.combinations, main.data.combinations)
                if (length(amount.extra.data.this.location) < extra.points.needed.to.keep) redundant.locations = c(redundant.locations, other.location)
            }
            return (redundant.locations)
        },
        
        generate.transformation.matrix.nested = function(dimnames.list, locations.list, remove.mask.list, n.strats, sim.dimnames, all.locations) # note: did we sort all locations when we added the msa?
        {   
            # It turns out that we NEED locations to be in different columns!
            # browser()
            transformation.matrix = NULL
            
            # put "location" as second dimension in sim.dimnames
            sim.dimnames = as.ontology(c(list(year=sim.dimnames$year, location=all.locations), sim.dimnames[names(sim.dimnames) != 'year']))
            
            for (i in 1:n.strats) {
                one.dimnames = dimnames.list[[i]]
                one.locations = locations.list[[i]]
                one.remove.mask = remove.mask.list[[i]]
                
                # "row year" means years as described by the data, which may year single years or ranges
                # "column year" means years as described by the model, which is always single years
                
                row.years.in.both.data.and.sim = get.range.robust.year.intersect(one.dimnames$year, sim.dimnames$year)
                col.years.in.both.data.and.sim = get.range.robust.year.intersect(sim.dimnames$year, one.dimnames$year)
                row.years.in.data.but.not.sim = setdiff(one.dimnames$year, row.years.in.both.data.and.sim)
                col.years.in.sim.but.not.data = setdiff(sim.dimnames$year, col.years.in.both.data.and.sim)
                
                # we must add rows that we'll soon delete so that every column year has a row year/range to map to
                row.years.for.initial.tmat = c(row.years.in.both.data.and.sim, col.years.in.sim.but.not.data)
                
                year.modified.dimnames = one.dimnames
                year.modified.dimnames$year = row.years.for.initial.tmat
                year.modified.dimnames.without.source = year.modified.dimnames[names(year.modified.dimnames) != 'source']
                year.modified.dimnames.without.source$location = union(one.locations, all.locations) # if data has locs B,A but matrix wants A,B,C, start with B,A,C and mask out C
                
                # problem: sim.dimnames doesn't have location and needs to have it as its second dimension
                one.mapping = get.ontology.mapping(from.ontology = sim.dimnames, to.ontology = year.modified.dimnames.without.source)
                one.source.transformation.matrix = one.mapping$get.matrix(from.dim.names = sim.dimnames,
                                                                          to.dim.names = year.modified.dimnames.without.source)
                
                # Remove rows for years not in this stratification
                if (length(col.years.in.sim.but.not.data) > 0) {
                    indices.for.years.not.present = get.array.access.indices(year.modified.dimnames.without.source, list(year=col.years.in.sim.but.not.data))
                    one.source.transformation.matrix = one.source.transformation.matrix[-indices.for.years.not.present,]
                    year.modified.dimnames.without.source$year = row.years.in.both.data.and.sim # now have removed the extra years
                }
                
                # Remove rows for locations not in this stratification
                locations.in.sim.but.not.stratification = setdiff(sim.dimnames$location, one.dimnames$location)
                indices.for.locations.not.present = get.array.access.indices(year.modified.dimnames.without.source,
                                                                             list(location=locations.in.sim.but.not.stratification))
                if (length(indices.for.locations.not.present) > 0)
                    one.source.transformation.matrix = one.source.transformation.matrix[-indices.for.locations.not.present,]
                
                # Repeat the matrix for each source this stratification has
                one.transformation.matrix = NULL
                for (source in 1:length(one.dimnames$source)) one.transformation.matrix = rbind(one.transformation.matrix, one.source.transformation.matrix)
                ncol.in.matrix = ncol(one.transformation.matrix)
                
                # Align the matrix rows with the one.remove.mask rows, which may have extra years, so that rows for sporadically missing data can be masked out
                if (length(row.years.in.data.but.not.sim) > 0) {
                    indices.to.omit.from.one.remove.mask = get.array.access.indices(one.dimnames, list(year=row.years.in.data.but.not.sim))
                    new.one.remove.mask = one.remove.mask[-indices.to.omit.from.one.remove.mask]
                    one.transformation.matrix = one.transformation.matrix[!new.one.remove.mask,]
                } else
                    one.transformation.matrix = one.transformation.matrix[!one.remove.mask]
                one.transformation.matrix = matrix(one.transformation.matrix, ncol=ncol.in.matrix)
                transformation.matrix = rbind(transformation.matrix, one.transformation.matrix)
            }
            transformation.matrix
        },
        
        get.metalocations = function(location, observation.locations, minimum.geographic.resolution.type)
        {
            minimum.components.list = lapply(observation.locations, function(obs.location) {
                locations::get.contained.locations(locations = obs.location, sub.type = minimum.geographic.resolution.type, return.list = F)})
            names(minimum.components.list) = observation.locations
            
            minimum.components = unique(unlist(minimum.components.list))
            
            # If any observation locations are themselves minimum components, add them as their own minimum component to create a unique context
            minimum.components.list = lapply(observation.locations, function(location.name) {
                if (location.name %in% minimum.components)
                    c(minimum.components.list[[location.name]], location.name)
                else
                    minimum.components.list[[location.name]]
            })
            names(minimum.components.list) = observation.locations
            
            # make matrix with membership of each minimum component to an observation location (state, substate region, EMA, county, MSA)
            # find unique sets of columns; these are metalocations.
            obs.to.minimal.component.map = t(matrix(unlist(lapply(seq_along(minimum.components.list),
                                                                  function(obs) {minimum.components %in% minimum.components.list[[obs]]})),
                                                    ncol =length(minimum.components.list),
                                                    nrow = length(minimum.components)))
            
            # Is logical. Todd's version is 0 and 1.
            metalocation.cols = unique(obs.to.minimal.component.map, MARGIN=2)
            
            metalocation.to.minimal.component.map = apply(metalocation.cols, MARGIN=2, function(metalocation.col) {
                minimum.components[which(apply(obs.to.minimal.component.map, MARGIN=2, function(obs.minimal.col) {identical(obs.minimal.col, metalocation.col)}))]
            })
            # only give 'metalocation.cols' dimnames *after* checking if columns are identical, because names will make them different
            dimnames(metalocation.cols) = list(obs.location = names(minimum.components.list), metalocation.number = 1:ncol(metalocation.cols))
            
            metalocation.type = sapply(metalocation.to.minimal.component.map, function(metalocation) {
                components.in.msa = intersect(metalocation, minimum.components.list[[location]])
                if (setequal(components.in.msa, minimum.components.list[[location]])) 'msa'
                else if (length(components.in.msa)==0) 'minimum-out-of-msa'
                else 'minimum-in-msa'
            })
            
            output = list(metalocation.to.minimal.component.map = metalocation.to.minimal.component.map,
                          metalocation.to.obs.location.mapping = metalocation.cols,
                          metalocation.type = metalocation.type)
        },
        
        get.p.bias.matrices = function(n.strata, metalocation.type, n.years, p.bias.in, p.bias.out)
        {
            matrix.each.stratum = matrix(unlist(lapply(metalocation.type, function(metaloc.type)
            {
                if (metaloc.type == 'minimum-in-msa') rep(p.bias.in, n.years)
                else if (metaloc.type == 'minimum-out-of-msa') rep(p.bias.out, n.years)
                else rep(0, n.years)
            })),
            nrow = n.years,
            ncol = length(metalocation.type))
            rep(list(matrix.each.stratum), n.strata)
        },
        
        get.obs.n = function(data.manager, stratification, locations.with.n.data, years.with.data, outcome.for.n, sim.ontology, model.strata, partitioning.function, version, location, error.prefix)
        {
            
            # Change stratification into what dimensions the n data ontology will need to achieve it and get mappings to align
            universal.ontology.for.n = data.manager$get.universal.ontology.for.outcome(outcome.for.n, exclude.ontology.names = private$i.exclude.denominator.ontology.names)
            initial.aligning.mappings = get.mappings.to.align.ontologies(universal.ontology.for.n, sim.ontology)
            
            if (is.null(initial.aligning.mappings))
                stop(paste0(error.prefix, "an aligning mapping could not be found between the universal ontology for n and the sim ontology"))
            
            # browser()
            if (!is.null(stratification))
                stratification.for.n = initial.aligning.mappings[[1]]$get.required.from.dimensions(stratification) # I think this always works... until there's a case where it doesn't.
            else
                stratification.for.n = stratification # NULL
            
            ### SET UP CACHE ###
            obs.n.cache = new.env()
            
            # Get obs.n.array with its missing data mask attached an attribute. Convert the mask to numeric so that at the end of partitioning, anything > 0 has a ancestral value that was missing
            obs.n.array = get.average(data.manager, stratification.for.n, locations.with.n.data, years.with.data, outcome.for.n, is.top.level = T, cache=obs.n.cache, error.prefix=error.prefix) # Note: "stratification" may be character(0) if we only have totals
            data.ontology = as.ontology(dimnames(obs.n.array)[names(dim(obs.n.array))!='location'], incomplete.dimensions = c('year'))
            obs.n.mask.array = array(as.numeric(attr(obs.n.array, 'missing.data.mask')), dim(obs.n.array), dimnames(obs.n.array))
            
            if (!is.null(stratification))
            {
                # map data to aligning ontology
                aligning.mappings = get.mappings.to.align.ontologies(data.ontology, sim.ontology[private$i.sim.keep.dimensions], allow.non.overlapping.incomplete.dimensions = T)
                
                obs.n.array.aligned = aligning.mappings[[1]]$apply(obs.n.array)
                obs.n.mask.array.aligned = aligning.mappings[[1]]$apply(obs.n.mask.array)
                
                # reverse map data and model mask to model ontology
                model.arr.dimnames = sim.ontology[names(sim.ontology) %in% c('year', 'location', stratification)]
                model.arr.dimnames$year = years.with.data
                model.arr.dimnames$location = locations.with.n.data
                # model.arr.indices = aligning.mappings[[2]]$get.reverse.mapping.indices(model.arr.dimnames, dimnames(obs.n.array.aligned))
                # model.arr = array(obs.n.array.aligned[model.arr.indices], sapply(model.arr.dimnames, length), model.arr.dimnames)
                # model.mask.arr = array(obs.n.mask.array.aligned[model.arr.indices], sapply(model.arr.dimnames, length), model.arr.dimnames)
                model.arr = aligning.mappings[[2]]$reverse.apply(obs.n.array.aligned)
                model.mask.arr = aligning.mappings[[2]]$reverse.apply(obs.n.mask.array.aligned) # @AZ does this work??? verify with a test b/c is logical, not integer
                # browser() ## ADDED FOR TODD
                
                if (any(is.na(model.arr)))
                    stop(paste0("'", outcome.for.n, "' data could not be found for all needed locations/years/dimensions."))
                
                # use the partitioning function - VALIDATE THAT YOU GET AN ARRAY BACK WITH SAME DIMNAMES
                partitioned.model.arr = partitioning.function(model.arr, version=version, location=location)
                
                partitioned.model.mask.arr = partitioning.function(model.mask.arr, version=version, location=location)
                
                # check sum against the sum of the values in the aligned obs array that are actually mapped
                obs.n.arr.indices = aligning.mappings[[2]]$get.mapping.indices(model.arr.dimnames, dimnames(obs.n.array.aligned))
                values.that.map = sapply(obs.n.arr.indices, function(x) {length(x) > 0})
                if (sum(obs.n.array.aligned[values.that.map], na.rm=T) != sum(partitioned.model.arr, na.rm=T))
                    stop("Sums not equal before and after partitioning obs-n")
            } else {
                partitioned.model.arr = obs.n.array
                partitioned.model.mask.arr = obs.n.mask.array
            }
            
            # Record which values had been missing at the highest level of stratification and needed to be estimated
            was.missing.arr = array(partitioned.model.mask.arr > 0, dim(partitioned.model.mask.arr), dimnames(partitioned.model.mask.arr))
            
            # Limit array to only locations without any NA
            location.has.all.data = apply(!is.na(partitioned.model.arr), 'location', all)
            locations.with.all.data = dimnames(partitioned.model.arr)$location[location.has.all.data]
            limited.dimnames = dimnames(partitioned.model.arr)
            limited.dimnames$location = locations.with.all.data
            limited.partitioned.model.arr = array(partitioned.model.arr[get.array.access.indices(dimnames(partitioned.model.arr), dimension.values = list(location = locations.with.all.data))],
                                                  sapply(limited.dimnames, length), limited.dimnames)

            # Return a list of matrices, one per stratum, indexed by year and location
            n.obs.locations = length(locations.with.all.data) # should equal how many locations we have in our array
            n.years = length(years.with.data)
            if (is.null(model.strata)) one.matrix.per.stratum = list(matrix(limited.partitioned.model.arr, nrow = n.years, ncol = n.obs.locations))
            else {
                one.matrix.per.stratum = lapply(1:nrow(model.strata), function(i) {
                    model.dimension.values = as.list(setNames(as.vector(model.strata[i,]), names(model.strata)))
                    stratum.matrix = matrix(limited.partitioned.model.arr[get.array.access.indices(dimnames(limited.partitioned.model.arr), dimension.values = model.dimension.values)],
                                            nrow = n.years,
                                            ncol = n.obs.locations)
                })
            }
            
            list(obs.n = one.matrix.per.stratum, locations.with.n.data = locations.with.all.data, estimated.values = was.missing.arr)
        },
        
        # hard code female msm as not needed so we can stop if it is NA? -> never implemented
        get.average = function(data.manager, stratification, locations.with.n.data, years.with.data, outcome.for.n, is.top.level = F, top.level.dimnames = NULL, cache, error.prefix)
        {
            
            ### check if this is cached. If so, use it and return. If not, set it at the end.
            if (length(stratification)==0) this.step.hash = 'NULL'
            else this.step.hash = paste0(stratification, collapse = '__')
            if (this.step.hash %in% names(cache)) return(cache[[this.step.hash]])
            
            # if (is.top.level) browser()
            # print(paste0(stratification, collapse="__"))
            data = data.manager$pull(outcome = outcome.for.n,
                                     keep.dimensions = c('year', 'location', stratification),
                                     dimension.values = list(location = locations.with.n.data, year = years.with.data),
                                     exclude.ontology.names = private$i.exclude.denominator.ontology.names,
                                     na.rm=T)
            
            # # if we are at the top level but got nothing, throw an error because these are supposed to be locations with data
            # if (is.null(data) && is.top.level)
            #     stop("Top level did not have any data for locations that were supposed to have data")
            ## NEW PLAN: MAKE AN ARRAY OF ALL NA WITH THE DIMENSIONS WE KNOW ALL THE PULLS COME OUT IN: THE UNIVERSAL ONTOLOGY FOR THIS OUTCOME
            if (is.null(data) && is.top.level) {
                universal = data.manager$get.universal.ontology.for.outcome(outcome.for.n, exclude.ontology.names = private$i.exclude.denominator.ontology.names)
                universal = universal[c('year', 'location', stratification)]
                universal$location = locations.with.n.data
                universal$year = years.with.data
                data = array(NA, sapply(universal, length), universal)
            }
            else if (!is.null(data)) {
                # set zeroes to NA to compensate for the pull turning some NAs into imaginary zeroes
                data[data==0]=NA
                
                # If data from multiple sources, take geometric mean. Then get rid of source dimension
                if (!('source' %in% names(dim(data)))) browser()
                if (dim(data)[['source']] > 1) {
                    count.not.na = apply.robust(data, MARGIN=names(dim(data))[names(dim(data)) != 'source'], FUN=function(x) {sum(!is.na(x))})
                    expanded.count.not.na = expand.array(count.not.na, dimnames(data))
                    data[is.na(data) & expanded.count.not.na != 0] = 1
                    data = apply.robust(data, MARGIN=names(dim(data))[names(dim(data)) != 'source'], FUN=prod) ^ (1 / count.not.na)
                } else {
                    new.dimnames = dimnames(data)[names(dimnames(data)) != 'source']
                    data = array(data, dim = sapply(new.dimnames, length), new.dimnames)
                }
                
                # If we are lacking certain years (or locations), expand data to include them as NA.
                if (!setequal(dimnames(data)$location, locations.with.n.data) || !setequal(dimnames(data)$year, get.range.robust.year.intersect(years.with.data, dimnames(data)$year))) {
                    complete.dimnames = dimnames(data)
                    complete.dimnames$location = locations.with.n.data
                    complete.dimnames$year = years.with.data
                    complete.array = array(NA, dim=sapply(complete.dimnames, length), complete.dimnames)
                    complete.array[get.array.access.indices(complete.dimnames, dimnames(data))] = data
                    # problem: dimnames(data) has single years but complete.array has year ranges
                    data = complete.array
                }
                
                ## NEW: If we're at top or 1-way and a stratum is missing at most 30% of its data, interpolate NAs across years
                if (length(stratification)<=1 && any(is.na(data))) {

                    if (!any(apply(is.na(data), names(dim(data))[names(dim(data))!='year'], function(x) {sum(x)/length(x) >= 0.3})))
                        data = interpolate.array(data)
                }
            }
            
            if (is.top.level) top.level.dimnames = dimnames(data)
            attr(data, 'missing.data.mask') = NULL
            
            # If we got something (not NULL), then see what we're missing. It may be nothing, in which case we can return what we got.
            if (!is.null(data)) {
                missing.data.mask = is.na(data)
                attr(data, 'missing.data.mask') = missing.data.mask
                if (!any(missing.data.mask)) {
                    cache[[this.step.hash]] = data
                    return(data)
                }
            }
            
            # If we're at a 0- or 1-way stratification, just return what we got (which may be NULL)
            k = length(stratification)
            if (k < 2) {
                cache[[this.step.hash]] = data
                return(data)   
            }
            
            # Otherwise, we're either missing something or have nothing, so we will take an average built out of k-2- and k-1-way stratifications
            # there will be k choose k-2 of the k-2-way stratifications and k choose k-1 of the k-1-way stratifications.
            k.minus.1.way.stratifications = combn(stratification, k - 1, simplify = F)
            k.minus.2.way.stratifications = combn(stratification, k - 2, simplify = F)
            
            # get each k-2-way and expand it to fit the data dimnames
            # if we lack data for certain years... could be an issue... should add years in I think
            # if (identical(stratification, c('risk', 'race'))) browser()
            k.minus.2.way.data = lapply(k.minus.2.way.stratifications, function(k.minus.2.way.stratification) {
                lower.data = get.average(data.manager, k.minus.2.way.stratification, locations.with.n.data, years.with.data, outcome.for.n, is.top.level = F, top.level.dimnames, cache=cache, error.prefix=error.prefix)
                if (is.null(lower.data)) return (NULL)
                missing.years = setdiff(top.level.dimnames$year, dimnames(lower.data)$year)
                if (length(missing.years)>0) {
                    dimnames.with.extra.years = dimnames(lower.data)
                    dimnames.with.extra.years$year = top.level.dimnames$year
                    lower.data.with.extra.years = array(NA, dim=sapply(dimnames.with.extra.years, length), dimnames.with.extra.years)
                    array.access(lower.data.with.extra.years, dimnames(lower.data)) <- lower.data
                    lower.data = lower.data.with.extra.years
                }
                expand.array(lower.data, top.level.dimnames[names(top.level.dimnames) %in% c('year', 'location', stratification)])
            })
            
            k.minus.1.way.data = lapply(k.minus.1.way.stratifications, function(k.minus.1.way.stratification) {
                lower.data = get.average(data.manager, k.minus.1.way.stratification, locations.with.n.data, years.with.data, outcome.for.n, is.top.level = F, top.level.dimnames, cache=cache, error.prefix=error.prefix)
                if (is.null(lower.data)) return (NULL)
                missing.years = setdiff(top.level.dimnames$year, dimnames(lower.data)$year)
                if (length(missing.years)>0) {
                    dimnames.with.extra.years = dimnames(lower.data)
                    dimnames.with.extra.years$year = top.level.dimnames$year
                    lower.data.with.extra.years = array(NA, dim=sapply(dimnames.with.extra.years, length), dimnames.with.extra.years)
                    array.access(lower.data.with.extra.years, dimnames(lower.data)) <- lower.data
                    lower.data = lower.data.with.extra.years
                }
                expand.array(lower.data, top.level.dimnames[names(top.level.dimnames) %in% c('year', 'location', stratification)])
            })
            
            # Now we build our average... We need to know which numerators (product of two k-1 stratifications) go with which denominators (a single k-2 stratification)
            # We will build terms out of denominators since there are no repeated denominators. The numerator for a denominator is the product of the two k-1 stratifications that include all the dimensions of the denominator.
            arrays.to.be.averaged = lapply(1:length(k.minus.2.way.stratifications), function(i) {
                k.minus.2.way.stratification = k.minus.2.way.stratifications[[i]]
                matching.k.minus.1.way.stratification.mask = sapply(k.minus.1.way.stratifications, function(k.minus.1.way.stratification) {is.subset(k.minus.2.way.stratification, k.minus.1.way.stratification)})
                Reduce('*', k.minus.1.way.data[matching.k.minus.1.way.stratification.mask]) / k.minus.2.way.data[[i]]
            })
            # if (identical(stratification, c('age', 'race'))) browser()
            arrays.to.be.averaged.na.mask = lapply(arrays.to.be.averaged, function(arr) {is.na(arr)})
            count.not.na = length(arrays.to.be.averaged) - Reduce('+', arrays.to.be.averaged.na.mask)
            arrays.to.be.averaged.replaced.with.zeroes = lapply(arrays.to.be.averaged, function(arr) {
                arr[is.na(arr) & count.not.na != 0] = 1
                arr
            })
            
            replacement.array = Reduce('+', arrays.to.be.averaged.replaced.with.zeroes) / count.not.na
            
            # either replace the missing positions with those from the replacement array, or return the whole replacement array
            if (is.null(data)) {
                cache[[this.step.hash]] = replacement.array
                return(replacement.array)
            }
            data[missing.data.mask] = replacement.array[missing.data.mask]
            cache[[this.step.hash]] = data
            # if (any(is.nan(data))) browser()
            data
        },
        
        
        get.n.multipliers = function(metalocation.to.minimal.component.map, metalocation.type, main.location, stratification, sim.ontology, model.strata, data.manager, outcome, years, error.prefix)
        {
            # browser()
            n.metalocations = length(metalocation.to.minimal.component.map)
            n.years = length(years)
            model.arr.dimnames = sim.ontology[names(sim.ontology) %in% c('year', stratification)]
            model.arr.dimnames$year = years
            model.arr.ontology = as.ontology(model.arr.dimnames, incomplete.dimensions = 'year')
            
            # Change stratification into what dimensions the n data ontology will need to achieve it and get mappings to align
            # browser()
            universal.ontology = data.manager$get.universal.ontology.for.outcome(outcome, exclude.ontology.names = if (private$i.outcome.for.n.multipliers==private$i.denominator.outcome.for.data) private$i.exclude.denominator.ontology.names else NULL)
            aligning.mappings = get.mappings.to.align.ontologies(universal.ontology, sim.ontology)
            if (!is.null(stratification))
                stratification.for.n = aligning.mappings[[1]]$get.required.from.dimensions(stratification)
            else stratification.for.n = stratification
            
            one.array.per.metalocation = lapply(1:n.metalocations, function(i) {
                ### SET UP CACHE ###
                n.mult.cache = new.env()
                
                if (metalocation.type[[i]] == 'msa') {
                    model.arr = array(1, dim=sapply(model.arr.dimnames, length), model.arr.dimnames)
                }
                else {
                    arr = get.outcome.ratios(location.1 = metalocation.to.minimal.component.map[[i]],
                                             location.2 = main.location,
                                             stratification = stratification.for.n, data.manager = data.manager, outcome = outcome, years = years, universal.ontology = universal.ontology, cache=n.mult.cache, error.prefix=error.prefix)
                    
                    if (is.null(arr)) stop("bug in get.outcome.ratios: returned NULL")
                    # Map this back to the model ontology
                    arr.ontology = as.ontology(dimnames(arr), incomplete.dimensions = 'year')
                    # sim.ontology.years.replaced = sim.ontology[names(sim.ontology) != 'location']
                    # sim.ontology.years.replaced$year = arr.ontology$year
                    aligning.mappings = get.mappings.to.align.ontologies(arr.ontology, model.arr.ontology, allow.non.overlapping.incomplete.dimensions = T) # needs to be subset of sim.ontology?
                    if (is.null(aligning.mappings)) stop(paste0("couldn't find mappings to align the metalocation data ontology and sim ontology"))
                    aligned.data = aligning.mappings[[1]]$apply(arr)
                    model.arr.indices = aligning.mappings[[2]]$get.reverse.mapping.indices(model.arr.dimnames, dimnames(aligned.data))
                    model.arr = array(aligned.data[model.arr.indices], sapply(model.arr.dimnames, length), model.arr.dimnames)
                }
                # if (any(is.na(model.arr))) browser()# stop("not enough data for n-multipliers in one or more locations")
                model.arr
            })
            
            # Return a matrix [year, metalocation] for each model stratum
            if (is.null(model.strata)) one.matrix.per.model.stratum = list(matrix(unlist(one.array.per.metalocation), nrow=n.years, ncol=n.metalocations))
            else {
                one.matrix.per.model.stratum = lapply(1:nrow(model.strata), function(i) {
                    model.dimension.values = as.list(setNames(as.vector(model.strata[i,]), names(model.strata)))
                    stratum.slice.by.metalocation = lapply(one.array.per.metalocation, function(meta.arr) {
                        slice = meta.arr[get.array.access.indices(dimnames(meta.arr), dimension.values = model.dimension.values)]
                    })
                    stratum.matrix = matrix(unlist(stratum.slice.by.metalocation), nrow = n.years, ncol = n.metalocations)
                } )
            }
            
            # check for NAs
            if (any(sapply(one.matrix.per.model.stratum, function(mat) {any(is.na(mat))})))
                stop(paste0(error.prefix, "not enough data found for n-multipliers; consider restricting likelihood years"))
            
            return(one.matrix.per.model.stratum)
        },
        
        get.outcome.ratios = function(location.1, location.2, stratification, data.manager, outcome, years, universal.ontology, cache, error.prefix)
        {
            # Pull data for each location. Later, we will map them to an aligning ontology.
            
            ### check if this is cached. If so, use it and return. If not, set it at the end.
            this.step.hash = paste0(stratification, collapse = '__')
            if (this.step.hash %in% names(cache)) return(cache[[this.step.hash]])
            
            # browser()
            keep.dimensions = c('year', stratification)
            location.data = lapply(list(location.1, location.2), function(location) {
                data = data.manager$pull(outcome = outcome,
                                         keep.dimensions = keep.dimensions,
                                         dimension.values = list(year = as.character(years), location = location),
                                         exclude.ontology.names = if (private$i.outcome.for.n.multipliers==private$i.denominator.outcome.for.data) private$i.exclude.denominator.ontology.names else NULL,
                                         na.rm = T,
                                         debug = F) # location.1 == '24035' && length(stratification)==0) #length(stratification)==1)
                output.before.replacement = data
                
                # if there is data for multiple sources, take the geometric mean of the values for each source, then remove source as a dimension.
                # Because the pull now can return imaginary zeroes that should be NA, convert all zeroes to NA before the mean
                if (!is.null(data)) {
                    data[data==0] = NA
                    if (dim(data)[['source']] > 1) {
                        data.dimnames.without.source=dimnames(data)[names(dimnames(data))!= 'source']
                        count.not.na = apply.robust(data, MARGIN=names(data.dimnames.without.source), FUN=function(x) {sum(!is.na(x))})
                        expanded.count.not.na = expand.array(count.not.na, dimnames(data))
                        data[is.na(data) & expanded.count.not.na != 0] = 1
                        output.before.replacement = apply.robust(data, MARGIN=names(dim(data))[names(dim(data)) != 'source'], FUN=prod) ^ (1 / count.not.na)
                    } else {
                        new.dimnames = dimnames(data)[names(dimnames(data)) != 'source']
                        output.before.replacement = array(data, dim = sapply(new.dimnames, length), new.dimnames)
                    }
                }
                output.before.replacement
            })
            
            # Find an aligning ontology between the two
            ratio.arr = NULL
            location.data.dimnames = lapply(location.data, function(x) {dimnames(x)})
            if (!any(sapply(location.data.dimnames, is.null)))
            {
                # take ratio and find missing value positions
                ratio.arr = location.data[[1]] / location.data[[2]]
                # if (any(is.infinite(ratio.arr))) browser()
                missing.data.mask = is.na(ratio.arr)
                if (!any(missing.data.mask) || is.null(stratification)) return (ratio.arr)
            }
            if (is.null(stratification)) return (NULL)
            
            # -- replace NAs or the entire thing recursively if possible -- #
            
            # generate stratifications that are one level lower than this
            if (length(stratification) == 1)
                recursive.stratifications.list = list(NULL)
            else
                recursive.stratifications.list = combn(stratification, length(stratification) - 1, simplify = F)
            
            # recurse
            stratification.ratios = lapply(recursive.stratifications.list, function(lower.stratification) {
                lower.ratio.arr = get.outcome.ratios(location.1, location.2, lower.stratification, data.manager, outcome, years, universal.ontology, cache=cache, error.prefix=error.prefix)
                # if (!is.null(lower.ratio.arr) && !is.null(ratio.arr)) lower.ratio.arr = expand.array(to.expand = lower.ratio.arr, target.dim.names = dimnames(ratio.arr))
                # lower.ratio.arr
            })
            
            # remove ratio arrays that are all NAs -- these won't participate in the geometric mean
            stratification.ratios = stratification.ratios[sapply(stratification.ratios, function(arr) {!all(is.na(arr))})]
            
            # If we have only one array coming back, we expand it to the what the universal ontology says our current dimnames should be
            if (length(stratification.ratios) == 1) {
                if (length(stratification) == 1) {
                    expand.to.dimnames = c(list(dimnames(stratification.ratios[[1]])$year), universal.ontology[names(universal.ontology) %in% stratification])
                    names(expand.to.dimnames) = c('year', stratification)
                    
                    stratification.ratios[[1]] = expand.array(stratification.ratios[[1]], expand.to.dimnames)
                }
                if (is.null(ratio.arr)) return (stratification.ratios[[1]])
                else ratio.arr[missing.data.mask] = stratification.ratios[[1]][missing.data.mask]
            }
            
            else if (length(stratification.ratios) > 1) {
                
                # If we're getting back arrays of different dimensions, need to expand them all to have the same size.
                expand.to.dimnames = list()
                for (arr in stratification.ratios)
                {
                    append.dimnames = dimnames(arr)[!names(dimnames(arr)) %in% names(expand.to.dimnames)]
                    expand.to.dimnames = c(expand.to.dimnames, append.dimnames)
                }
                
                # It's possible that one or more of them won't have as many years as the others, a fatal error
                missing.years = unique(unlist(sapply(stratification.ratios, function(arr) {setdiff(expand.to.dimnames$year, dimnames(arr)$year)})))
                if (length(missing.years)>0)
                    stop(paste0(error.prefix, "One or more needed stratifications of '", outcome, "' data lacked data for years ", paste0(missing.years, collapse=', '), ". Consider restricting the years for this likelihood."))
                stratification.ratios = lapply(stratification.ratios, function(arr) {expand.array(arr, expand.to.dimnames)})
                
                # take geometric mean of non-NAs only; this will be the product of non-NAs (NAs converted to 1 temporarily) raised to the power of 1 / the number of non-NAs
                stratification.ratio.na.mask = lapply(stratification.ratios, function(arr) {is.na(arr)})
                count.not.na = length(stratification.ratios) - Reduce('+', stratification.ratio.na.mask)
                stratification.ratios.nas.replaced.with.ones = lapply(stratification.ratios, function(stratification.arr) {
                    stratification.arr[is.na(stratification.arr) & count.not.na != 0] = 1 # propagate NA if data was never found at any depth of stratification (not expected)
                    stratification.arr
                })
                
                replacement.arr = Reduce('*', stratification.ratios.nas.replaced.with.ones)
                
                if (is.null(ratio.arr)) return(replacement.arr ^ (1 / count.not.na))
                else ratio.arr[missing.data.mask] = replacement.arr[missing.data.mask] ^ (1 / count.not.na[missing.data.mask])
            }
            cache[[this.step.hash]] = ratio.arr
            return(ratio.arr) # may be NULL
        },
        
        generate.inverse.variance.weights.matrix = function(obs.vector, equalize.weight.by.year, metadata, weights)
        {
            weights.vector = rep(1,length(obs.vector))

            if (equalize.weight.by.year) {
                obs.per.year = table(metadata$year)
                number.years = length(obs.per.year)
                
                for (year in names(obs.per.year)) {
                    weights.vector[metadata$year == year]  = length(obs.vector) / (obs.per.year[[year]] * number.years)
                }
            }
            
            data.dimension.values = apply(metadata, MARGIN=1, function(row) {
                stratum = unlist(strsplit(row[['stratum']], "__"))
                dimensions = unlist(strsplit(row[['dimensions']], "__"))
                rv = setNames(c(list(row[['year']]), as.list(stratum)), c('year', dimensions))
            })
            
            # Once the weights list is in the format list(weights.object1, weights.object2, ...), I'll loop over them.
            for (weight in weights) {
                
                # if no dimension.values, apply it to all observations
                if (length(weight$dimension.values) == 0) {
                    weights.vector = weights.vector * weight$total.weight
                } else {
                    weights.mask = sapply(data.dimension.values, function(row) {
                        dimensions.this.weight = names(weight$dimension.values)
                        if (!all(dimensions.this.weight %in% names(row)))
                            return (F)
                        all(sapply(dimensions.this.weight, function(dimension) {
                            row[[dimension]] %in% weight$dimension.values[[dimension]] # weight can have multiple values per dimension
                        }))
                    })
                    weights.vector[weights.mask] = weights.vector[weights.mask] * weight$total.weight
                }
            }
            
            sqrt.weights.vector = sqrt(1/weights.vector)
            sqrt.weights.vector %*% t(sqrt.weights.vector)
        }
    )
)


#'@param data.manager The name of the data manager
#'@param dimensions
#'@param levels.of.stratification
#'@param outcome.for.p
#'@param outcome.for.n
#'@param sub.location.type Can be NULL
#'@param super.location.type Can be NULL
#'@param main.location.type
#'@param minimum.sample.size
#'
#'@export
get.p.bias.estimates = function(data.manager=get.default.data.manager(), dimensions, levels.of.stratification, outcome.for.p, outcome.for.n, sub.location.type, super.location.type, main.location.type = 'CBSA', minimum.sample.size = 12, main.location.type.p.source=NULL, sub.location.type.p.source=NULL, super.location.type.p.source=NULL, main.location.type.n.source=NULL, sub.location.type.n.source=NULL, super.location.type.n.source=NULL, debug=F)
{
    error.prefix = "Error getting p bias estimates: "
    # --- VALIDATION --- #
    if (debug) browser()
    
    # *data.manager*
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop(paste0(error.prefix, "'data.manager' must be an R6 object with class 'jheem.data.manager'"))
    
    # *dimensions* is a character vector with no NAs or duplicates, post conversion if NULL
    if (is.null(dimensions)) dimensions = character(0)
    if (!is.character(dimensions) || is.null(dimensions) || any(is.na(dimensions)) || any(duplicated(dimensions)))
        stop(paste0(error.prefix, "'dimensions' must be NULL or a character vector containing no NAs or duplicates"))
    
    # *levels.of.stratification* is NULL or a numeric vector with no NAs or duplicates
    if (!is.numeric(levels.of.stratification) || any(is.na(levels.of.stratification)) || any(duplicated(levels.of.stratification)) || any(sapply(levels.of.stratification, function(x) {x<0})))
        stop(paste0(error.prefix, "'levels.of.stratification' must be NULL or an integer vector containing no NAs, duplicates, or nonnegative numbers"))
    
    # --- so that I don't have to change the code very much to accommodate not having a sub.location.type or super.location.type, I'll set a default but just skip one or other of the loops later.
    lack.sub.location.type = is.null(sub.location.type)
    lack.super.location.type = is.null(super.location.type)
    if (lack.sub.location.type) sub.location.type = 'county'
    if (lack.super.location.type) super.location.type = 'state'
    
    # 0. Prepare stratifications using the same method the likelihood code does, only NULL is better for level 0 than ""
    levels.of.stratification = as.integer(levels.of.stratification)
    if (is.null(levels.of.stratification)) levels.of.stratification = 0
    stratifications = list()
    for (level in levels.of.stratification)
    {
        if (level == 0) stratifications = c(stratifications, list(NULL))
        else stratifications = c(stratifications, combn(dimensions, level, simplify = F))
    }
    
    # 1. Find locations with data in the data manager
    locations.with.p.data = data.manager$get.locations.with.data(outcome = outcome.for.p)
    locations.with.p.and.n.data = intersect(locations.with.p.data, data.manager$get.locations.with.data(outcome = outcome.for.n))
    
    # 2. Determine which are MSAs
    location.types.p = locations::get.location.type(locations.with.p.data)
    location.types.p.and.n = locations::get.location.type(locations.with.p.and.n.data)
    msas.p = names(location.types.p[location.types.p == toupper(main.location.type)])
    msas.p.and.n = names(location.types.p.and.n[location.types.p.and.n == toupper(main.location.type)])
    if (length(msas.p)==0)
        stop(paste0(error.prefix, "no locations of type '", main.location.type, "' with '", outcome.for.p, "' data found"))
    if (length(msas.p.and.n)==0)
        stop(paste0(error.prefix, "no locations of type '", main.location.type, "' with both '", outcome.for.p, "' and '", outcome.for.n, "' data found"))
    
    # 3. Make a list of MSAs and their sub-locations (p) and super locations (n and p) that have data; scrap MSAs with nothing
    if (!lack.sub.location.type) {
        main.subs.p = lapply(msas.p, function(msa) {intersect(locations::get.contained.locations(msa, sub.location.type, return.list = F), locations.with.p.data)})
        names(main.subs.p) = msas.p
        main.subs.p = main.subs.p[lengths(main.subs.p) > 0]
        all.relevant.subs.p = unique(unlist(main.subs.p))
        if (is.null(all.relevant.subs.p))
            stop(paste0(error.prefix, "no locations of type '", sub.location.type, "' with '", outcome.for.p, "' data found"))
    }
    if (!lack.super.location.type) {
        main.supers.p.and.n = lapply(msas.p.and.n, function(msa) {intersect(locations::get.containing.locations(msa, super.location.type, return.list = F), locations.with.p.and.n.data)})
        names(main.supers.p.and.n) = msas.p.and.n
        main.supers.p.and.n = main.supers.p.and.n[lengths(main.supers.p.and.n) > 0]
        all.relevant.supers.p.and.n = unique(unlist(main.supers.p.and.n))
        if (is.null(all.relevant.supers.p.and.n))
            stop(paste0(error.prefix, "no locations of type '", super.location.type, "' with both '", outcome.for.p, "' and '", outcome.for.n, "' data found"))
    }
    
    # 4. Generate an inside-msa p-bias sample by looping across each stratification
    if (!lack.sub.location.type)
        p.bias.in.msa = unlist(lapply(stratifications, function(stratification)
        {
            # 4a. Pull msa data. Make sure only one source is present because more than one source will make it ambiguous what values will match what. We're already expecting to have different sources for main and sub data.
            main.data = data.manager$pull(outcome = outcome.for.p,
                                          sources = main.location.type.p.source,
                                          keep.dimensions = c('year', 'location', stratification),
                                          dimension.values = list(location = names(main.subs.p)),
                                          debug = F)#identical(stratification, "risk"))
            if (is.null(main.data)) return(NULL)
            
            # 4b. Use the msa data to prepare a target ontology for the sub-location data.
            if (dim(main.data)[['source']] > 1)
                stop(paste0(error.prefix, main.location.type, " '", outcome.for.p, "' data from more than one source found. Please specify a single source to use in 'main.location.type.p.source'"))
            new.dimnames = dimnames(main.data)[names(dimnames(main.data)) != 'source']
            main.data = array(main.data, sapply(new.dimnames, length), new.dimnames)
            main.data.ontology = as.ontology(dimnames(main.data), incomplete.dimensions = c('year', 'location'))
            main.data.ontology$location = union(main.data.ontology$location, all.relevant.subs.p) #used to just use all.relevant.subs.p, but then a mapping can't be found because the universal will be only MSAs due to LHD ontology determining uni's location dimension.
            main.years = main.data.ontology$year
            
            # 4c. Pull sub-location data with the main data's ontology as a target. NOTE: IF SUB-LEVEL DATA EXISTS FOR MULTIPLE ONTOLOGIES, ONLY THE FIRST ONTOLOGY'S DATA WILL BE PULLED
            sub.data = data.manager$pull(outcome=outcome.for.p,
                                         sources = sub.location.type.p.source,
                                         keep.dimensions = c('year', 'location', stratification),
                                         dimension.values = list(location = all.relevant.subs.p, year = main.years),
                                         target.ontology = main.data.ontology,
                                         allow.mapping.from.target.ontology = T,
                                         debug=F)
            if (is.null(sub.data)) return(NULL)
            mp = attr(sub.data, 'mapping')
            if (dim(sub.data)[['source']] > 1)
                stop(paste0(error.prefix, sub.location.type, " '", outcome.for.p, "' data from more than one source found. Please specify a single source to use in 'sub.location.type.p.source'"))
            new.dimnames = dimnames(sub.data)[names(dimnames(sub.data)) != 'source']
            sub.data = array(sub.data, sapply(new.dimnames, length), new.dimnames)
            
            # 4d. Determine which MSAs and their sub-locations are present in this stratification's data
            msas.with.data.this.stratification.mask = names(main.subs.p) %in% dimnames(main.data)$location
            msas.with.data.this.stratification = names(main.subs.p)[msas.with.data.this.stratification.mask]
            main.subs.this.stratification = lapply(main.subs.p[msas.with.data.this.stratification.mask], function(main.subs) {
                main.subs[main.subs %in% dimnames(sub.data)$location]
            })
            main.subs.this.stratification = main.subs.this.stratification[lengths(main.subs.this.stratification) > 0]
            if (length(main.subs.this.stratification) == 0) return(NULL)
            
            # 4e. Map the msa data to the ontology of the sub data.
            align.on.dimnames = dimnames(sub.data)
            align.on.dimnames$location = dimnames(main.data)$location
            aligned.main.data = mp$apply(main.data, to.dim.names = align.on.dimnames)
            
            # 4f. Get a p-bias vector for each MSA that has data in this stratification and return the collection
            p.bias.vector = unlist(lapply(1:length(main.subs.this.stratification), function(i) {
                
                # 4f.1 Get a slice of the data array for the MSA and another for its sub-locations.
                main.slice = aligned.main.data[get.array.access.indices(dimnames(aligned.main.data), dimension.values = list(location = names(main.subs.this.stratification)[[i]]))]
                subs.slice = sub.data[get.array.access.indices(dimnames(sub.data), dimension.values = list(location = main.subs.this.stratification[[i]]))]
                main.slice.dimnames = dimnames(aligned.main.data)[names(dimnames(aligned.main.data)) != 'location']
                subs.slice.dimnames = c(main.slice.dimnames, list(location = main.subs.this.stratification[[i]]))
                main.slice = array(main.slice, dim = sapply(main.slice.dimnames, length), dimnames = main.slice.dimnames)
                
                # 4f.2 Expand the MSA slice to be the same size as the subs slice
                expanded.main.array = expand.array(main.slice, target.dim.names = subs.slice.dimnames)
                
                # 4f.3 Take the difference at each position between sub-location and MSA and return as a vector -- this forms part of our in-msa p-bias sample
                p.bias.vector.for.this.msa = as.vector(subs.slice - expanded.main.array)
                p.bias.vector.for.this.msa[!is.na(p.bias.vector.for.this.msa)]
                
            }))
        }))
    else p.bias.in.msa = NA
    
    # 5. Generate an outside-msa p-bias sample by looping across each stratification
    if (!lack.super.location.type)
        p.bias.out.msa = unlist(lapply(stratifications, function(stratification)
        {
            # browser()
            # 5a. Pull msa *p* data. Use it to make a target ontology for the msa *n* data pull. This assumes that the different outcomes can have the same ontology, which they'd have to if they are a matching numerator/denominator pair.
            main.p.data = data.manager$pull(outcome = outcome.for.p,
                                            sources = main.location.type.p.source,
                                            keep.dimensions = c('year', 'location', stratification),
                                            dimension.values = list(location = names(main.supers.p.and.n)))
            if (is.null(main.p.data)) return(NULL)
            if (dim(main.p.data)[['source']] > 1)
                stop(paste0(error.prefix, main.location.type, " '", outcome.for.p, "' data from more than one source found. Please specify a single source to use in 'main.location.type.p.source'")) # SHOULD HAVE BEEN CAUGHT ALREADY
            new.dimnames = dimnames(main.p.data)[names(dimnames(main.p.data)) != 'source']
            main.p.data = array(main.p.data, sapply(new.dimnames, length), new.dimnames)
            main.p.data.ontology = as.ontology(dimnames(main.p.data), incomplete.dimensions = c('year', 'location'))
            main.p.years = main.p.data.ontology$year
            
            # 5b. Pull msa *n* data. Use it to make a target ontology for the super *p* data pull.
            main.n.data = data.manager$pull(outcome = outcome.for.n,
                                            sources = main.location.type.n.source,
                                            keep.dimensions = c('year', 'location', stratification),
                                            dimension.values = list(location = names(main.supers.p.and.n), year = main.p.years),
                                            target.ontology = main.p.data.ontology,
                                            allow.mapping.from.target.ontology = T)
            if (is.null(main.n.data)) return(NULL)
            mp.main.p.to.main.n = attr(main.n.data, 'mapping')
            if (dim(main.n.data)[['source']] > 1)
                stop(paste0(error.prefix, main.location.type, " '", outcome.for.n, "' data from more than one source found. Please specify a single source to use in 'main.location.type.n.source'"))
            new.dimnames = dimnames(main.n.data)[names(dimnames(main.n.data)) != 'source']
            main.n.data = array(main.n.data, sapply(new.dimnames, length), new.dimnames)
            main.n.data.ontology = as.ontology(dimnames(main.n.data), incomplete.dimensions = c('year', 'location'))
            main.n.data.ontology$location = union(main.n.data.ontology$location, all.relevant.supers.p.and.n)
            main.n.years = main.n.data.ontology$year
            
            # 5c. Pull super-location *p* data. Use it to make a target ontology for the super *n* data pull.
            super.p.data = data.manager$pull(outcome = outcome.for.p,
                                             sources = super.location.type.p.source,
                                             keep.dimensions = c('year', 'location', stratification),
                                             dimension.values = list(location = all.relevant.supers.p.and.n, year = main.n.years),
                                             target.ontology = main.n.data.ontology,
                                             allow.mapping.from.target.ontology = T)
            if (is.null(super.p.data)) return(NULL)
            if (dim(super.p.data)[['source']] > 1)
                stop(paste0(error.prefix, super.location.type, " '", outcome.for.p, "' data from more than one source found. Please specify a single source to use in 'super.location.type.p.source'"))
            mp.main.n.to.super.p = attr(super.p.data, 'mapping')
            new.dimnames = dimnames(super.p.data)[names(dimnames(super.p.data)) != 'source']
            super.p.data = array(super.p.data, sapply(new.dimnames, length), new.dimnames)
            super.p.data.ontology = as.ontology(dimnames(super.p.data), incomplete.dimensions = c('year', 'location'))
            super.p.years = super.p.data.ontology$year
            # browser()
            # 5d. Pull super-location *n* data.
            super.n.data = data.manager$pull(outcome = outcome.for.n,
                                             sources = super.location.type.n.source,
                                             keep.dimensions = c('year', 'location', stratification),
                                             dimension.values = list(location = all.relevant.supers.p.and.n, year = super.p.years),
                                             target.ontology = super.p.data.ontology,
                                             allow.mapping.from.target.ontology = T)
            if (is.null(super.n.data)) return(NULL)
            if (dim(super.n.data)[['source']] > 1)
                stop(paste0(error.prefix, super.location.type, " '", outcome.for.n, "' data from more than one source found. Please specify a single source to use in 'super.location.type.n.source'"))
            mp.super.p.to.super.n = attr(super.n.data, 'mapping')
            new.dimnames = dimnames(super.n.data)[names(dimnames(super.n.data)) != 'source']
            super.n.data = array(super.n.data, sapply(new.dimnames, length), new.dimnames)
            
            # 5e. Align all four datasets by mapping main.p.data to main.n.data, then both of them to super.p.data, then all three of them to super.n.data. Then rename them for ease of reading.
            main.p.data = mp.main.p.to.main.n$apply(main.p.data)
            main.p.data = mp.main.n.to.super.p$apply(main.p.data)
            main.p.data = mp.super.p.to.super.n$apply(main.p.data)
            
            main.n.data = mp.main.n.to.super.p$apply(main.n.data)
            main.n.data = mp.super.p.to.super.n$apply(main.n.data)
            
            super.p.data = mp.super.p.to.super.n$apply(super.p.data)
            
            # 5d. Determine which MSAs and their super-locations are present in all of this stratification's data
            mains.with.data.this.stratification.mask = names(main.supers.p.and.n) %in% intersect(dimnames(main.p.data)$location, dimnames(main.n.data)$location)
            mains.with.data.this.stratification = names(main.supers.p.and.n)[mains.with.data.this.stratification.mask]
            main.supers.this.stratification = lapply(main.supers.p.and.n[mains.with.data.this.stratification.mask], function(main.supers) {
                main.supers[main.supers %in% intersect(dimnames(super.p.data)$location, dimnames(super.n.data)$location)]
            })
            names(main.supers.this.stratification) = mains.with.data.this.stratification
            main.supers.this.stratification = main.supers.this.stratification[lengths(main.supers.this.stratification) > 0]
            if (length(main.supers.this.stratification) == 0) return(NULL)
            common.years = intersect(dimnames(main.p.data)$year, intersect(dimnames(main.n.data)$year, intersect(dimnames(super.p.data)$year, dimnames(super.n.data)$year)))
            
            # 5f. Get a p-bias vector for each MSA that has data in this stratification and return the collection
            p.bias.vector = unlist(lapply(1:length(main.supers.this.stratification), function(i) {
                # 5f.1 Get a slices of the n and p data arrays for the MSA and others for its super-locations. Make sure they have the same *years* (TO GENERALIZE, MAKE THIS ANY INCOMPLETE DIMENSIONS IN THE ONTOLOGY).
                main.p.slice = main.p.data[get.array.access.indices(dimnames(main.p.data), dimension.values = list(year=common.years, location = names(main.supers.this.stratification)[[i]]))]
                supers.p.slice = super.p.data[get.array.access.indices(dimnames(super.p.data), dimension.values = list(year=common.years, location = main.supers.this.stratification[[i]]))]
                main.n.slice = main.n.data[get.array.access.indices(dimnames(main.n.data), dimension.values = list(year=common.years, location = names(main.supers.this.stratification)[[i]]))]
                supers.n.slice = super.n.data[get.array.access.indices(dimnames(super.n.data), dimension.values = list(year=common.years, location = main.supers.this.stratification[[i]]))]
                
                main.p.slice.dimnames = dimnames(main.p.data)[names(dimnames(main.p.data)) != 'location']
                main.p.slice.dimnames$year = common.years
                supers.p.slice.dimnames = c(main.p.slice.dimnames, list(location = main.supers.this.stratification[[i]]))
                supers.p.slice.dimnames$year = common.years
                main.n.slice.dimnames = dimnames(main.n.data)[names(dimnames(main.n.data)) != 'location']
                main.n.slice.dimnames$year = common.years
                supers.n.slice.dimnames = c(main.n.slice.dimnames, list(location = main.supers.this.stratification[[i]]))
                supers.n.slice.dimnames$year = common.years
                
                main.p.slice = array(main.p.slice, dim = sapply(main.p.slice.dimnames, length), dimnames = main.p.slice.dimnames)
                main.n.slice = array(main.n.slice, dim = sapply(main.n.slice.dimnames, length), dimnames = main.n.slice.dimnames)
                
                # 5f.2 Expand the MSA slices to be the same size as the supers slices
                expanded.main.p.array = expand.array(main.p.slice, target.dim.names = supers.p.slice.dimnames)
                expanded.main.n.array = expand.array(main.n.slice, target.dim.names = supers.n.slice.dimnames)
                
                # 5f.3 Perform a calculation at each position between super-location and MSA and return as a vector -- this forms part of our out-of-msa p-bias sample
                p.bias.vector.for.this.msa = as.vector((supers.p.slice * supers.n.slice - expanded.main.p.array * expanded.main.n.array) / (supers.n.slice - expanded.main.n.array) - expanded.main.p.array) # can get divide by 0, making Inf
                
                p.bias.vector.for.this.msa[!is.na(p.bias.vector.for.this.msa) & !is.infinite(p.bias.vector.for.this.msa)]
            }))
        }))
    else p.bias.out.msa = NA
    
    # 6. Return the mean and standard deviation of the in-msa and out-of-msa samples. Note that this next needs to be processed via metalocation type and made into a list of matrices by model stratum.
    if (!lack.sub.location.type && length(p.bias.in.msa) < minimum.sample.size)
        stop(paste0(error.prefix, "not enough samples found for p.bias.in.msa"))
    if (!lack.super.location.type && length(p.bias.out.msa) < minimum.sample.size)
        stop(paste0(error.prefix, "not enough samples found for p.bias.out.msa"))
    list(in.mean = mean(p.bias.in.msa),
         out.mean = mean(p.bias.out.msa),
         in.sd = sd(p.bias.in.msa),
         out.sd = sd(p.bias.out.msa),
         n.in = ifelse(!lack.sub.location.type, length(p.bias.in.msa), NA),
         n.out = ifelse(!lack.super.location.type, length(p.bias.out.msa), NA))
}

##-- For the Hand-Off to the CPP Function --##
##   get_nested_proportion_likelihood_components()
##
## In this documentation, I'm going to use:
## - n.strata to refer to the number of strata in the simulations (135 in the original model)
## - n.years to refer to the number of years
## - n.metalocations for the number of metalocations
## - n.obs for the number of observations we pull out of the data manager
## 
## In general here,
## - a 'mask' refers to a logical vector that is used to select certain elements out of a vector
## - a 'mapping'


# Given the simulated proportion suppressed in the MSA, what is the likelihood of getting the observed proportions at the MSA, state, and county levels?
# Sense of numerators and denominators in each location and stratum
# multiplier: assume county # of black, ageX, msm in a county some multiple of the simulation MSA stratum count
# Condition. Good sense of each stratum in NJ and NY states from real life data. We know the real life values *with some uncertainty*.

#-- SET-UP ARGUMENTS --#
#
# p and n are the only ones needed to be found at compute time
# NumericMatrix p - the vector of probabilities (our outcome) from sim$get
# NumericMatrix n - the vector of denominators corresponding to each p from sim$get
#
# List year_metalocation_n_multipliers - a list of length n.strata, each element of which is an n.years x n.metalocations matrix. the value at [[s]][y,m] is the best-guess multiplier by which we multiply the denominator (n) in the sim to get the denominator for the metalocation
# List year_metalocation_n_multiplier_sd - a list of length n.strata, each element of which is an n.years x n.metalocations matrix. the value at [[s]][y,m] is the standard deviation of (uncertainy around) the above multiplier
# List year_metalocation_p_bias - a list of length n.strata, each element of which is an n.years x n.metalocations matrix. the value at [[s]][y,m] is the bias (metalocation - sim location) in the estimate of p. This implies that the best-guess estimate of p for the metalocation is bias + sim p
# List year_metalocation_p_sd - a list of length n.strata, each element of which is an n.years x n.metalocations matrix. the value at [[s]][y,m] is the variance of the bias (which is equal to the variance of the best-guess estimate of p for the metalocation)
# 
# double metalocation_p_correlation - a numeric scalar correlation (ie, bounded on [0,1]) which gives the correlation between values of p for the same metalocation and stratum in different years. Will need to be specified by the user
# double metalocation_n_multiplier_correlation - a numeric scalar correlation (ie, bounded on [0,1]) which gives the correlation between values of n (the denominator) for the same metalocation and stratum in different years. Will need to be specified by the user
# 
#-- CONDITION ON N ARGUMENTS --#
#   In computing the likelihood, we will at some point condition on the n's (denominators) being equal to some known values
#   We condition on two types of n's: 
#       (a) some (arbitrary) number of observations which are a combination of metalocations
#       (b) the metalocation n's for the MSA
#
# for (a), could be our state population
# List obs_n - a list of length n.strata, each element of which is a matrix indexed [year, locations for which we have n's]. @AZ pull data -- if NULL, give up. If got some data, ... (only a state level usually), a few holes few enough to patch (wait on Todd?)
#   - It will have n.years row
#   - It has one column for each of the locations in (a) above - obs.locations where we know the stratified population 
#   - The value at [[s]][y,l] is the observed number of people in stratum s for location l in year y
# 
# in practice, each stratum has the same mapping
# List year_metalocation_to_year_obs_n_mapping - A list of length n.strata, each element of which is a matrix indexed [obs, year x condition-on-metalocation]. #@AZ obs: means observation at a location in a year. First all locations with n data (a), then msa by year (b)
#   - Gives a map, for each stratum, from year x metalocation to the n's we will condition on (we will condition on n.years + a combo of metalocations)
#   - The columns are organized with (a) first followed by (b) - as above
#
# List obs_n_plus_conditioned_error_variances - A list of length n.strata, each element of which is a matrix indexed [obs, year x condition-on-metalocation] @AZ should be a vector of length [obs], which is a+b.
#   - Gives the variance around the estimate of each n we are conditioning on
#   - This variance will be non-zero for the (a)'s above
#   - But it must be zero for the (b)'s
#
# 
#-- CONDITION ON SIM LOCATION (eg MSA) ARGUMENTS --#
#   In computing the likelihood, we will at some point condition on the p's being equal to what is simulated to be the p for the whole sim location (MSA)
#   The number of p's we are conditioning on == n.years
#
# LogicalVector year_metalocation_to_year_condition_on_location_mask - a logical vector of length n.years x n.metalocations
#    - A mask that isolates just the year x metalocation elements in each stratum that we need to map to the msa that we will condition on
#
# NumericMatrix year_metalocation_to_year_condition_on_location_mapping - A matrix indexed [year, year x metalocation] 
#    - Maps from he metalocations to the location (the sim location) we are conditioning p on for each year
#    - The year x metalocation is AFTER applying the above mask
# 
#
#-- TRANSFORM TO OBS LOCATION --#
#
# LogicalVector year_metalocation_to_year_obs_location_mask - a logical vector of length n.years x n.metalocations
#   - A mask that isolates just the year x metalocation elements in each stratum that we need to map to the observed locations
# 
# NumericMatrix year_metalocation_to_year_obs_location_mapping - a matrix indexed [year x obs-location, year x metalocation] # these are observed p's
#   - Maps from the metalocations to the obs for each year
#   - The year x metalocation is AFTER applying the above mask
# 
#
#-- AGGREGATE (ACROSS STRATA) TO OBS --#
#
# List year_loc_stratum_to_obs_mapping - a list of length n.strata
#   - Each element is a matrix that aggregates maps from year x metalocations to observations, for that stratum
#   - Each element is indexed [obs, year x metalocation]
#   - The year x obs.location is BEFORE applying any masks
#   - @AZ typo??? Should be [obs, year x obs.location] ????
# 
# List year_metalocation_to_obs_mapping - a list of length n.strata. # redundant but saves computation time to precompute
#   - Each element is a matrix indexed [obs, year x metalocation] that gives, for each stratum, the mapping from values for that year and metalocation to the obs p vector
# 
# IntegerVector obs_year_index - an integer vector the length of obs_p, which gives the index of the year to which each observation in obs_p corresponds
# 
# NumericVector obs_p - the vector of observed proportions
# 
# NumericMatrix obs_error - the observation measurement error covariance matrix # assumes 0 correlation between locations

