
#'@inheritParams create.basic.likelihood.instructions
#'@param super.location.type The type for locations that contain the modeled location
#'@param sub.location.type The type for locations that the modeled location contains
#'@param minimum.geographic.resolution.type The type of location used to partition locations. The type of the model location AND the super and sub location types must all completely enclose regions of this type
#'
#'@export
create.nested.proportion.likelihood.instructions <- function(outcome.for.data,
                                                             outcome.for.sim,
                                                             denominator.outcome.for.sim = NULL, # If NULL (as it would be for population), will be doing the Poisson version of compute
                                                             
                                                             super.location.type, # test with 'state'
                                                             sub.location.type, # test with 'county'
                                                             minimum.geographic.resolution.type, # test with 'county'
                                                             
                                                             dimensions,
                                                             denominator.dimensions = dimensions,
                                                             levels.of.stratification = NULL,
                                                             
                                                             from.year = -Inf,
                                                             to.year = Inf,
                                                             omit.years = NULL,
                                                             
                                                             sources.to.use = NULL,
                                                             
                                                             within.location.p.error.correlation = 0.5,
                                                             within.location.n.error.correlation = 0.5,
                                                             
                                                             correlation.different.years = 0.5,
                                                             correlation.different.strata = 0.1,
                                                             correlation.different.sources = 0.3,
                                                             correlation.same.source.different.details = 0.3,
                                                             observation.correlation.form = c('compound.symmetry', 'autoregressive.1')[1],
                                                             measurement.error.sd, #this is different here - with proportions we can just do a SD
                                                             weights,
                                                             equalize.weight.by.year = F)
{
    
    JHEEM.NESTED.PROPORTION.LIKELIHOOD.INSTRUCTIONS$new(outcome.for.data = outcome.for.data,
                                                        outcome.for.sim = outcome.for.sim,
                                                        denominator.outcome.for.sim = denominator.outcome.for.sim,
                                                        super.location.type = super.location.type,
                                                        sub.location.type = sub.location.type,
                                                        minimum.geographic.resolution.type = minimum.geographic.resolution.type,
                                                        dimensions = dimensions,
                                                        denominator.dimensions = denominator.dimensions,
                                                        levels.of.stratification = levels.of.stratification,
                                                        from.year = from.year,
                                                        to.year = to.year,
                                                        omit.years = omit.years,
                                                        sources.to.use = sources.to.use,
                                                        within.location.p.error.correlation = within.location.p.error.correlation,
                                                        within.location.n.error.correlation = within.location.n.error.correlation,
                                                        orrelation.different.years = orrelation.different.years,
                                                        correlation.different.strata = correlation.different.strata,
                                                        correlation.different.sources = correlation.different.sources,
                                                        correlation.same.source.different.details = correlation.same.source.different.details,
                                                        observation.correlation.form = observation.correlation.form,
                                                        measurement.error.sd = measurement.error.sd,
                                                        weights = weights,
                                                        equalize.weight.by.year = equalize.weight.by.year)
    
}

JHEEM.NESTED.PROPORTION.LIKELIHOOD.INSTRUCTIONS = R6::R6Class(
    'jheem.nested.proportion.likelihood.instructions',
    inherit = JHEEM.LIKELIHOOD.INSTRUCTIONS,
    
    public = list(
        
        initialize = function(outcome.for.data,
                              outcome.for.sim,
                              denominator.outcome.for.sim,
                              super.location.type,
                              sub.location.type,
                              minimum.geographic.resolution.type,
                              dimensions,
                              denominator.dimensions,
                              levels.of.stratification,
                              from.year,
                              to.year,
                              omit.years,
                              sources.to.use,
                              within.location.p.error.correlation,
                              within.location.n.error.correlation,
                              orrelation.different.years,
                              correlation.different.strata,
                              correlation.different.sources,
                              correlation.same.source.different.details,
                              observation.correlation.form,
                              measurement.error.sd,
                              weights,
                              equalize.weight.by.year)
        {
            super$initialize(outcome.for.sim = outcome.for.sim,
                             dimensions = dimensions,
                             levels.of.stratification = levels.of.stratification,
                             weights)
            
            # VALIDATION
            
            private$i.super.location.type = super.location.type
            private$i.sub.location.type = sub.location.type
            private$i.minimum.geographic.resolution.type = minimum.geographic.resolution.type
            
            private$i.outcome.for.data = outcome.for.data
            private$i.denominator.outcome.for.sim = denominator.outcome.for.sim
            private$i.from.year = from.year
            private$i.to.year = to.year
            private$i.omit.years = omit.years
            private$i.denominator.dimensions = denominator.dimensions
            private$i.equalize.weight.by.year = equalize.weight.by.year
            
            private$i.sources.to.use = sources.to.use
            private$i.parameters = list(correlation.different.years = correlation.different.years,
                                        correlation.different.strata = correlation.different.strata,
                                        correlation.different.sources = correlation.different.sources,
                                        correlation.same.source.different.details = correlation.same.source.different.details,
                                        observation.correlation.form = observation.correlation.form,
                                        measurement.error.sd = measurement.error.sd,
                                        within.location.p.error.correlation = within.location.p.error.correlation,
                                        within.location.n.error.correlation = within.location.n.error.correlation)
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
        
        denominator.outcome.for.sim = function(value)
        {
            if (missing(value))
            {
                private$i.denominator.outcome.for.sim
            }
            else
                stop("Cannot modify a jheem.likelihood.instruction's 'denominator.outcome.for.sim' - it is read-only")
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
        
        
        denominator.dimensions = function(value)
        {
            if (missing(value))
            {
                private$i.denominator.dimensions
            }
            else
                stop("Cannot modify a jheem.likelihood.instruction's 'denominator.dimensions' - they are read-only")
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
        
        parameters = function(value) {
            if (missing(value)) {
                private$i.parameters
            }
            else
                stop("Cannot modify a jheem.basic.likelihood.instruction's 'parameters' - they are read-only")
        },
        sources.to.use = function(value) {
            if (missing(value)) {
                private$i.sources.to.use
            }
            else
                stop("Cannot modify a jheem.basic.likelihood.instruction's 'sources.to.use' - they are read-only")
        }
        
    ),
    
    private = list(
        
        i.super.location.type = NULL,
        i.sub.location.type = NULL,
        i.minimum.geographic.resolution.type = NULL,
        
        i.outcome.for.data = NULL,
        i.denominator.outcome.for.sim = NULL,
        i.from.year = NULL,
        i.to.year = NULL,
        i.omit.years = NULL,
        i.denominator.dimensions = NULL,
        i.equalize.weight.by.year = NULL,
        
        
        i.parameters = NULL,
        i.sources.to.use = NULL
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
                              data.manager,
                              throw.error.if.no.data,
                              error.prefix)
        {
            super$initialize(instructions = instructions,
                             version = version,
                             location = location,
                             error.prefix = error.prefix)
            
            private$i.parameters = instructions$parameters
            private$i.outcome.for.data = instructions$outcome.for.data
            private$i.denominator.outcome.for.sim = instructions$denominator.outcome.for.sim
            
            years = get.likelihood.years(from.year = instructions$from.year,
                                         to.year = instructions$to.year,
                                         omit.years = instructions$omit.years,
                                         data.manager = data.manager,
                                         outcome.for.data = private$i.outcome.for.data)
            
            ## ---- PREPARE DATA STRUCTURES ---- ##
            
            sim.metadata = get.simulation.metadata(version=version,
                                                   location=location,
                                                   from.year = years[[1]],
                                                   to.year = years[[length(years)]])
            
            # Pull data
            # determine n.strata and n.years from pulled data
            
            private$i.sim.ontology = sim.metadata$outcome.ontologies[[private$i.outcome.for.sim]]
            private$i.sim.ontology[['year']] = as.character(years)
            
            private$i.obs.vector = c()
            
            private$i.details = list() # details can't be in a data frame because its elements (character vectors) may have different lengths
            private$i.metadata = data.frame(year = character(0),
                                            stratum = character(0),
                                            source = character(0)
            )
            
            mappings.list = list()
            dimnames.list = list()
            private$i.transformation.matrix = NULL
            private$i.sim.required.dimnames = list()
            
            
            remove.mask = c()
            
            ## ---- PULL DATA ---- ##
            
            for (strat in private$i.stratifications) {
                keep.dimensions = 'year'
                if (!identical(strat, "")) keep.dimensions = c(keep.dimensions, strat)
                data = data.manager$pull(outcome = private$i.outcome.for.data,
                                         sources = private$i.sources.to.use,
                                         keep.dimensions = keep.dimensions,
                                         dimension.values = list(location=location), # leave this for now. Will get more complicated when we have multi location models
                                         target.ontology = private$i.sim.ontology,
                                         allow.mapping.from.target.ontology = T,
                                         append.attributes = 'details',
                                         debug = F)
                
                
                one.mapping = attr(data, 'mapping')
                one.dimnames = dimnames(data)
                one.obs.vector = as.numeric(data)
                one.details = attr(data, 'details')
                
                one.remove.mask = is.na(one.obs.vector)
                remove.mask = c(remove.mask, one.remove.mask)
                one.obs.vector = one.obs.vector[!one.remove.mask]
                one.details = one.details[!one.remove.mask]
                
                if (is.null(data)) {
                    if (throw.error.if.no.data)
                        stop(paste0(error.prefix, "no data was found for the stratification '", strat, "'"))
                    else {
                        # one.sim.keep.dimensions = NULL
                        one.metadata = NULL
                        one.sim.required.dimnames = list()
                    }
                }
                else {
                    # one.sim.keep.dimensions = one.mapping$get.required.from.dimensions(to.dimensions = names(dim(data))[names(dim(data)) != 'source'])
                    
                    # Metadata will involve melting both arrays (data and details) as well as making "stratum"
                    one.metadata = reshape2::melt(data)
                    
                    one.metadata = one.metadata[!one.remove.mask,]
                    
                    # Recover required dimnames from one.metadata
                    one.sim.required.dimnames = one.mapping$get.required.from.dim.names(lapply(one.metadata[!(colnames(one.metadata) %in% c('source', 'value'))],
                                                                                               function(x) {as.character(unique(x))}))
                    
                    one.metadata = one.metadata[, sort(colnames(one.metadata))]
                    one.metadata['stratum'] = do.call(paste, c(subset.data.frame(one.metadata, select=-c(year, source, value)), sep="__"))
                    one.metadata[is.na(one.metadata$stratum), 'stratum'] = ".TOTAL."
                    one.metadata = subset.data.frame(one.metadata, select = c(year, stratum, source))
                }
                
                # Find the required.dimnames
                for (d in names(one.sim.required.dimnames)) {
                    
                    if (!(d %in% names(private$i.sim.required.dimnames)))
                        private$i.sim.required.dimnames = c(private$i.sim.required.dimnames, setNames(list(one.sim.required.dimnames[[d]]), d))
                    else
                        private$i.sim.required.dimnames[[d]] = union(private$i.sim.required.dimnames[[d]], one.sim.required.dimnames[[d]])
                }
                
                private$i.obs.vector = c(private$i.obs.vector, one.obs.vector)
                private$i.details = c(private$i.details, one.details)
                private$i.metadata = rbind(private$i.metadata, one.metadata)
                
                # save all the one.mappings in a list?
                mappings.list[[length(mappings.list) + 1]] = one.mapping
                dimnames.list[[length(dimnames.list) + 1]] = one.dimnames
                
            }
            
            ## ---- FIND REQUIRED DIMENSION VALUES, ETC. ---- ##
            private$i.sim.dimension.values = private$i.sim.required.dimnames[sapply(names(private$i.sim.required.dimnames),
                                                                                    function(d) {
                                                                                        !identical(private$i.sim.required.dimnames[[d]],
                                                                                                   private$i.sim.ontology[[d]])
                                                                                    })]
            denominator.keep.dimensions = c(instructions$denominator.dimensions, 'year')[c(instructions$denominator.dimensions, 'year') %in% names(private$i.sim.required.dimnames)]
            private$i.denominator.required.dimnames = private$i.sim.required.dimnames[names(private$i.sim.required.dimnames) %in% denominator.keep.dimensions]
            private$i.denominator.dimension.values = private$i.denominator.required.dimnames[sapply(names(private$i.denominator.required.dimnames),
                                                                                                    function(d) {
                                                                                                        !identical(private$i.denominator.required.dimnames[[d]],
                                                                                                                   private$i.sim.ontology[[d]])
                                                                                                    })]
            private$i.years = private$i.sim.required.dimnames[['year']]
            
            ## ---- GENERATE TRANSFORMATION MATRIX ---- ##
            # browser()
            # Some data has year ranges
            for (s in seq_along(mappings.list)) {
                one.mapping = mappings.list[[s]]
                one.dimnames = dimnames.list[[s]]
                
                one.transformation.matrix = one.mapping$get.matrix(from.dim.names = private$i.sim.required.dimnames,
                                                                   to.dim.names = one.dimnames[names(one.dimnames) != 'source'])
                # rbind once per source
                for (source in 1:length(one.dimnames[['source']])) {
                    private$i.transformation.matrix = rbind(private$i.transformation.matrix, one.transformation.matrix)
                }
            }
            private$i.transformation.matrix = private$i.transformation.matrix[!remove.mask,]
            
            if (is.null(private$i.transformation.matrix))
                stop(paste0(error.prefix, "no mappings found to align simulation and data ontologies"))
            
            #----------#
            
            # Things needed for the n.multipliers.
            
            n.metalocations = NULL
            metalocation.type = character(0)
            n.years = length(years) # CORRECT
            n.strata = function(stratifications){'something I can write. Should usually be 135 because it is referring to the sim'}
            is.one.state.msa = function(location) {'something for Todd or Jeff to write'}
            n.multiplier.cv = 0.1 # something set as default in the original version
            sd.inflation.extra.msa.to.msa = 1 # set as default in the original, where it says it doesn't apply to "n multipliers"?
            sd.inflation = 1 # set as default in original
            in.county = list() # created in line 694 of original from a function "get.in.msa.county.p.bias.and.variance". It is a List to which "sd", "variance", and "bias" are added soon after.
            extra.county = list()
            county.to.msa.p.sd.multiplier = NULL
            state.to.msa.p.sd.multiplier = NULL
            
            
            # N.MULTIPLIERS
            n.multipliers = sapply(1:n.metalocations, function(i) {
                if (metalocation.type[i] == "msa")
                    matrix(1, nrow=n.years, ncol=n.strata)
                else if (denominator.outcome.for.sim == 'population')
                {
                    NULL
                    # the population data from the data manager
                    # for the metalocation, divided by that of the MSA.
                    # So if the MSA population is double that of the metalocation counties, the multiplier matrix will be all 2's.
                }
                else
                {
                    if (metalocation.type[i]=='county-out-of-msa' &&
                        is.one.state.msa(msa))
                    {
                        # all the states we have represented by the counties outside the MSA
                        super.locations = unique(state.for.county(counties.for.metalocation[[i]]))
                        # our data (we have to figure what is actually ASKED for by the calculate.outcome.differences function)
                        super.surv = state.surveillance
                        diff.super.to.1.locations = msa
                        diff.super.to.1.surv = msa.surveillance
                    }
                    else
                        super.locations = super.surv = diff.super.to.1.locations = diff.super.to.1.surv = NULL
                    
                    calculate.outcome.differences()
                    # I guess I just have to wait for Todd on this one. Not worth the time to figure it out on my own
                }
            })
            dim(n.multipliers) = c(n.years, n.strata, n.metalocations)
            private$i.n.multipliers = lapply(1:n.strata, function(d) {
                n.multipliers[,d,]
            })
            
            # N.MULTIPLIER.SD Find the sd using three constant. The sd is a fraction of the size of the multiplier.
            private$i.n.multiplier.sd = lapply(i.n.multipliers, function(mult) {
                mult * n.multiplier.cv * sd.inflation.extra.msa.to.msa / sd.inflation
            })
            
            # P.BIAS
            private$i.p.bias = lapply(1:n.strata, function(d) {
                sapply(metalocation.type, function(type) {
                    if (type=='county-in-msa')
                        rep(in.county$bias, n.years) # what is "in.county"?
                    else if (type=='county-out-of-msa')
                        rep(extra.county$bias, n.years)
                    else #type=='msa'
                        rep(0, n.years)
                })
            })
            
            # P.SD
            private$i.p.sd = lapply(1:n.strata, function(d) {
                sapply(metalocation.type, function(type) {
                    if (type=='county-in-msa')
                        rep(sqrt(in.county$variance)*county.to.msa.p.sd.multiplier * sd.inflation.extra.msa.to.msa / sd.inflation, n.years)
                    else if (type=='county-out-of-msa')
                        rep(sqrt(extra.county$variance)*state.to.msa.p.sd.multiplier * sd.inflation.extra.msa.to.msa / sd.inflation, n.years)
                    else #type=='msa'
                        rep(0, n.years)
                })
            })
        }
    ),
    
    private = list(
        
        # FOR CPP ARGUMENTS
        i.n.multipliers = NULL,
        i.n.multipliers.sd = NULL,
        i.p.bias = NULL,
        i.p.sd = NULL,
        
        # NORMAL
        
        i.parameters = NULL,
        
        i.outcome.for.data = NULL,
        i.denominator.outcome.for.sim = NULL,
        
        i.obs.vector = NULL,
        i.details = NULL,
        i.metadata = NULL,
        i.sim.ontology = NULL,
        i.sim.required.dimnames = NULL,
        i.denominator.required.dimnames = NULL,
        i.sim.dimension.values = NULL,
        i.denominator.dimension.values = NULL,
        i.transformation.matrix = NULL,
        i.inverse.variance.weights.matrix = NULL,
        
        do.compute = function(sim, log=T, check.consistency=T)
        {
            sim.numerator.data = sim$get(outcome = private$i.outcome.for.sim,
                                         keep.dimensions = names(private$i.sim.required.dimnames),
                                         dimension.values = private$i.sim.dimension.values)
            # include Poisson option for when outcome is 'population'? Or will that not happen?
            sim.denominator.data = sim$get(outcome = private$i.denominator.outcome.for.sim,
                                           keep.dimensions = names(private$i.denominator.required.dimnames),
                                           dimension.values = private$i.denominator.dimension.values)
            expanded.sim.denominator.data = expand.array(sim.denominator.data, dimnames(sim.numerator.data))
            
            sim.n = expanded.sim.denominator.data
            sim.p = sim.numerator.data / sim.n
            
            get_nested_proportion_likelihood_components(p,
                                                        n,
                                                        year_metalocation_n_multipliers = n.multipliers,
                                                        year_metalocation_n_multiplier_sd = n.multiplier.sd,
                                                        ...)
            
        }
        
    )
)


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
# List obs_n - a list of length n.strata, each element of which is a matrix indexed [year, locations for which we have n's]. 
#   - It will have n.years row
#   - It has one column for each of the locations in (a) above - obs.locations where we know the stratified population 
#   - The value at [[s]][y,l] is the observed number of people in stratum s for location l in year y
# 
# in practice, each stratum has the same mapping
# List year_metalocation_to_year_obs_n_mapping - A list of length n.strata, each element of which is a matrix indexed [obs, year x condition-on-metalocation].
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
# 
# List year_metalocation_to_obs_mapping - a list of length n.strata. # redundant but saves computation time to precompute
#   - Each element is a matrix indexed [obs, year x metalocation] that gives, for each stratum, the mapping from values for that year and metalocation to the obs p vector
# 
# IntegerVector obs_year_index - an integer vector the length of obs_p, which gives the index of the year to which each observation in obs_p corresponds
# 
# NumericVector obs_p - the vector of observed proportions
# 
# NumericMatrix obs_error - the observation measurement error covariance matrix # assumes 0 correlation between locations


