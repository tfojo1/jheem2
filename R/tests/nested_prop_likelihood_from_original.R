

##--------------------------------##
##--------------------------------##
##-- THE LIKELIHOOD CONSTRUCTOR --##
##--------------------------------##
##--------------------------------##

NESTED.DATA.TYPES.FOR.N = c('suppression'='prevalence',
                            'engagement'='prevalence',
                            'linkage'='new',
                            'diagnosed'='prevalence.all',
                            'retention'='prevalence',
                            'testing'='population')

ALLOWED.NESTED.DATA.TYPES = names(NESTED.DATA.TYPES.FOR.N)
BACKUP.DATA.TYPE.FOR.P.BIAS.AND.VARIANCE = c('retention'='engagement')
BACKUP.DATA.TYPES.FOR.IN.V.EXTRA.MSA.SD.RATIO = list(testing=c('suppression','engagement'))

EXCLUDE.COUNTIES.FROM.MSA = list(
    '47900' = '54037'
)

create.nested.likelihood <- function(data.type=c('suppression','engagement','linkage','diagnosed')[1],
                                     msa,
                                     msa.surveillance,
                                     state.surveillance,
                                     county.surveillance,
                                     
                                     census.totals,
                                     census.full.msm,
                                     census.collapsed,
                                     settings,
                                     idu.manager,
                                     
                                     years,
                                     
                                     include.msa = T,
                                     include.states=T,
                                     include.counties=T,
                                     
                                     observation.error.fn,
                                     pass.n.to.obs.error.fn=F,
                                     error.chunk.correlation=0.5,
                                     error.off.correlation=error.chunk.correlation,
                                     
                                     additional.obs.error.fn=NULL,
                                     additional.obs.error.correlation=0.9,
                                     
                                     within.location.p.error.correlation = 0.5,
                                     within.location.n.error.correlation = 0.5,
                                     
                                     sd.inflation = 1, #applies to everything EXCEPT state-vs-msa p, county-vs-msa p, and n multipliers
                                     sd.inflation.extra.msa.to.msa=1, #applies to state-vs-msa p, county-vs-msa p, and n multipliers
                                     
                                     n.error.function=sqrt, #takes a vector of numbers and returns the error for each
                                     n.error.inflation.if.calculated = 2,
                                     
                                     n.multiplier.cv = 0.1,
                                     
                                     state.to.msa.p.sd.multiplier = 1, #reduce or increase the sd in going from state to msa
                                     county.to.msa.p.sd.multiplier = 1,
                                     
                                     probability.decreasing.slope=0.05,
                                     
                                     by.total=T,
                                     by.sex=T,
                                     by.race=T,
                                     by.age=T,
                                     by.risk=T,
                                     by.sex.age=F,
                                     by.sex.race=F,
                                     by.sex.risk=F,
                                     by.race.risk=F,
                                     
                                     inflate.sd.by.n.obs.per.year.and.geography=T,
                                     inflate.sd.by.n.obs.per.year=F,
                                     
                                     ages=c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years'),
                                     races=c('black','hispanic','other'),
                                     sexes=c('heterosexual_male','msm','female'),
                                     risks=c('never_IDU','active_IDU','IDU_in_remission'),
                                     
                                     min.n.for.calculating.bias.and.variance = 10,
                                     
                                     cached.errors=NULL,
                                     verbose=F)
{
#-- CHECK ARGUMENTS --#
    
    if (all(ALLOWED.NESTED.DATA.TYPES!=data.type))
        stop(paste0("data.type must be one of: ",
                    paste0("'", ALLOWED.NESTED.DATA.TYPES, "'", collapse=', ')))
    
    data.type.for.n = NESTED.DATA.TYPES.FOR.N[data.type]
    
    
#-- CHECK WHAT DATA WE HAVE --#

    if (verbose)
        cat("NSTLIK: Starting nested proportion likelihood set-up...")
    
    # Pull MSA data (assume if we have any data, we will have unstratified)
    msa.data = get.surveillance.data(msa.surveillance, location.codes = msa, 
                                     years = years, data.type = data.type, throw.error.if.missing.data = F)
    have.msa.data = any(!is.na(msa.data)) && include.msa
    if (!include.msa && verbose)
        cat("\n    - NOT including msa")
    
    # Pull county data
    counties.in.msa = counties.for.msa(msa)
    counties.in.msa = setdiff(counties.in.msa, EXCLUDE.COUNTIES.FROM.MSA[[msa]])
    # If this is a one-msa county, don't use county data (it will be the same as MSA)
    if (include.counties)
    {
        if (length(counties.in.msa)==1)
        {
            if (verbose)
                print("Only one county in MSA - not including county data")
            
            include.counties = F
            counties.with.data = character()
        }
        else
        {
            county.data = lapply(counties.in.msa, get.surveillance.data,
                                 surv=county.surveillance, data.type=data.type, years=years,
                                 throw.error.if.missing.data = F)
            have.county.data = sapply(county.data, function(cd){
                !is.null(cd) && any(!is.na(cd))
            })
            counties.with.data = counties.in.msa[have.county.data]
        }
    }
    else
    {
        counties.with.data = character()
        if (verbose)
            cat("\n    - NOT including counties")
    }    
    
    # Pull state data
    #states.containing.msa = states.for.msa(msa)
    states.containing.msa = unique(state.for.county(counties.in.msa))
    if (include.states)
    {
        state.data = lapply(states.containing.msa, get.surveillance.data,
                            surv=state.surveillance, data.type=data.type, years=years,
                            throw.error.if.missing.data = F)
        have.state.data = sapply(state.data, function(st){
            !is.null(st) && any(!is.na(st))  
        })
        states.with.data = states.containing.msa[have.state.data]
    }
    else
    {
        states.with.data = character()
        if (verbose)
            cat("\n    - NOT including states")
    }
    
    
    if (verbose)
        cat("Done\n")
    
#-- SET UP OBSERVED LOCATIONS --#
    
    if (verbose)
        cat('NSTLIK: Setting up observed locations...')
    
    obs.location.type = character()
    obs.location.code = character()
    
    if (have.msa.data)
    { 
        obs.location.type = c(obs.location.type, 'msa')
        obs.location.code = c(obs.location.code, msa)
        obs.location.for.msa = length(obs.location.code)
    }
    else
        obs.location.for.msa = numeric()
    
    if (length(counties.with.data)>0)
    {
        obs.location.for.county = length(obs.location.code) + 1:length(counties.with.data)
        names(obs.location.for.county) = counties.with.data
        
        obs.location.type = c(obs.location.type, rep('county', length(counties.with.data)))
        obs.location.code = c(obs.location.code, counties.with.data)
    }
    else
        obs.location.for.county = numeric()
    
    if (length(states.with.data)>0)
    {
        obs.location.for.state = length(obs.location.code) + 1:length(states.with.data)
        names(obs.location.for.state) = states.with.data
        
        obs.location.type = c(obs.location.type, rep('state', length(states.with.data)))
        obs.location.code = c(obs.location.code, states.with.data)
    }
    else
        obs.location.for.state = numeric()
    
    
    n.obs.locations = length(obs.location.code)
    if (n.obs.locations==0)
        stop(paste0("There is no '", data.type, "' data for '",
                    msa, "' at the county, MSA, or state level."))
    
    
    obs.location.desriptions = paste0(obs.location.code, " (", obs.location.type, ")")
    
    if (verbose)
        cat("Done\n")
    
#-- SET UP META-LOCATIONS --#
    
    if (verbose)
        cat('NSTLIK: Setting up meta-locations...')
    
    # state - never a metalocation
    # msa - a metalocation only if either
    #   (a) this is a one-county MSA
    #   (b) there is no data on sub-counties AND either this is a one-state MSA or there is no data on any states
    # county - one for every county we have data for (if not a one-county MSA)
    # in-msa-in-state-not-in-other-counties - 
    #   (a) one for every state we have data for (if there are any counties without data in the MSA in that state)
    #   (b) plus one for all the states we don't have data for (if any)
    # in-state-out-of-msa - one for every state we have data for
    
    # the vector of one or more county fips codes corresponding to each meta-location
    counties.for.metalocation = list()
    aggregate.locations.for.metalocation = list()
    metalocation.type = character()
    metalocation.to.obs.location.mapping = NULL # a matrix with n.obs.location rows and n.metalocations colums
                                                # value [i,j] is 1 if metalocation i maps into obs.location j, 0 else
    metalocation.to.msa.mapping = numeric() # vector with a 1 for every metalocation that maps into the MSA, 0 else
    
    msa.is.metalocation = is.one.county.msa(msa) || 
        (length(counties.with.data)==0 && (is.one.state.msa(msa) || length(states.with.data)==0))
    
    # first, check if the MSA itself will be a metalocation
    if (msa.is.metalocation)
    {
        metalocation.type = c(metalocation.type, 'msa')
        counties.for.metalocation = c(counties.for.metalocation,
                                      list(character()))
        
        mapping.col = rep(0, n.obs.locations)
        # if we have data on the msa, then this msa will accrue to that location
        if (any(obs.location.type=='msa')) 
            mapping.col[obs.location.for.msa] = 1
        
        # if msa is metalocation and we have state data, then this must be a one-state MSA
        #   the MSA should accrue to that state
        if (any(obs.location.type=='state')) 
            mapping.col[obs.location.type=='state'] = 1
        
        metalocation.to.obs.location.mapping = cbind(metalocation.to.obs.location.mapping, mapping.col)
        
        metalocation.to.msa.mapping = c(metalocation.to.msa.mapping, 1)
    }
    
    # next, add a meta-location for each msa county we have data on
    for (county in counties.with.data)
    {
        metalocation.type = c(metalocation.type, 'county-in-msa')
        
        counties.for.metalocation = c(counties.for.metalocation, list(county))
        
        mapping.col = rep(0, n.obs.locations)
        # Accrue to the county obs location
        mapping.col[obs.location.type=='county' & obs.location.code == county] = 1
        
        # Accrue to the msa if we have data
        # (if we are within this if statement, then the msa is not a metalocation)
        if (have.msa.data)
            mapping.col[obs.location.for.msa] = 1
        
        # Accrue to state if we have data on that state
        state = state.for.county(county)
        if (any(states.with.data==state))
            mapping.col[obs.location.type=='state' & obs.location.code==state] = 1
        
        metalocation.to.obs.location.mapping = cbind(metalocation.to.obs.location.mapping, mapping.col)
        
        # accrues to the msa
        metalocation.to.msa.mapping = c(metalocation.to.msa.mapping, 1)
    }
    
    # next, do the msa counties we don't have data on:
    #  one for each state with data (if there are any counties without data in the MSA in that state)
    #  plus
    #  one for all the states without data together (if there are any states without data)
    states.without.data = setdiff(states.containing.msa, states.with.data)
    counties.without.data = setdiff(counties.in.msa, counties.with.data)
    
    if (!msa.is.metalocation)
    {
        for (state in states.with.data)
        {
            in.state.in.msa.counties = intersect(counties.in.msa,
                                                 counties.for.state(state))
            in.state.in.msa.counties.without.data = setdiff(in.state.in.msa.counties,
                                                            counties.with.data)
            if (length(in.state.in.msa.counties.without.data)>0)
            {
                metalocation.type = c(metalocation.type, 'county-in-msa')
                counties.for.metalocation = c(counties.for.metalocation,
                                              list(in.state.in.msa.counties.without.data))
                
                mapping.col = rep(0, n.obs.locations)
                
                # Accrue to the msa if we have data
                # (if we are within this if statement, then the msa is not a metalocation)
                if (have.msa.data)
                    mapping.col[obs.location.for.msa] = 1
                
                # Accrue to state 
                mapping.col[obs.location.type=='state' & obs.location.code==state] = 1
                
                metalocation.to.obs.location.mapping = cbind(metalocation.to.obs.location.mapping, mapping.col)
                
                # accrues to the msa
                metalocation.to.msa.mapping = c(metalocation.to.msa.mapping, 1)
            }
        }
        if (length(states.without.data)>1)
        {
            in.state.in.msa.counties = intersect(counties.in.msa,
                                                 counties.for.state(states.without.data))
            in.state.in.msa.counties.without.data = setdiff(in.state.in.msa.counties,
                                                            counties.with.data)
            if (length(in.state.in.msa.counties.without.data)>0)
            {
                metalocation.type = c(metalocation.type, 'county-in-msa')
                counties.for.metalocation = c(counties.for.metalocation,
                                              list(in.state.in.msa.counties.without.data))
            }
            mapping.col = rep(0, n.obs.locations)
            
            # Accrue to the msa if we have data
            # (if we are within this if statement, then the msa is not a metalocation)
            if (have.msa.data)
                mapping.col[obs.location.for.msa] = 1
            
            metalocation.to.obs.location.mapping = cbind(metalocation.to.obs.location.mapping, mapping.col)
            
            # accrues to the msa
            metalocation.to.msa.mapping = c(metalocation.to.msa.mapping, 1)
        }
    }
    
    # last, do out-of-msa-in-state for each state we have data on
    for (state in states.with.data)
    {
        in.state.outside.msa.counties = setdiff(counties.for.state(state),
                                                counties.in.msa)
        if (length(in.state.outside.msa.counties)>0)
        {
            metalocation.type = c(metalocation.type, 'county-out-of-msa')
            counties.for.metalocation = c(counties.for.metalocation,
                                          list(in.state.outside.msa.counties))
            
            mapping.col = rep(0, n.obs.locations)
            
            # Accrue to state 
            mapping.col[obs.location.type=='state' & obs.location.code==state] = 1
            
            metalocation.to.obs.location.mapping = cbind(metalocation.to.obs.location.mapping, mapping.col)
            
            # does not accrue to the msa
            metalocation.to.msa.mapping = c(metalocation.to.msa.mapping, 0)
        }
    }
    
    n.metalocations = length(counties.for.metalocation)
    
    if (verbose)
        cat("Done\n")
    
#-- SET UP RESPONSE LIKELIHOOD ELEMENTS --#    
    
    if (verbose)
        cat('NSTLIK: Setting up likelihood response elements...')
    
    # Pull the likelihood elements
    obs.loc.likelihood.elements = lapply(1:length(obs.location.code), function(i){
        if (obs.location.type[i]=='county')
            surv = county.surveillance
        else if (obs.location.type[i]=='state')
            surv = state.surveillance
        else
            surv = msa.surveillance
        
        create.likelihood.elements.for.data.type(data.type=data.type,
                                                 years=years,
                                                 surv=surv,
                                                 location=obs.location.code[i],
                                                 by.total=by.total,
                                                 by.sex=by.sex,
                                                 by.race=by.race,
                                                 by.age=by.age,
                                                 by.risk=by.risk,
                                                 by.sex.age=by.sex.age,
                                                 by.sex.race=by.sex.race,
                                                 by.sex.risk=by.sex.risk,
                                                 by.race.risk=by.race.risk,
                                                 
                                                 numerator.sd=observation.error.fn,
                                                 pass.n.to.numerator.sd=pass.n.to.obs.error.fn,
                                                 bias.fn=function(...){0},
                                                 bias.sd=function(...){0},
                                                 numerator.year.to.year.chunk.correlation=error.chunk.correlation,
                                                 numerator.year.to.year.off.correlation=error.off.correlation,
                                                 ages=ages,
                                                 races=races,
                                                 sexes=sexes,
                                                 risks=risks,
                                                 na.rm=F)
    }) 
   
    # Set up some n's for indexing
    n.years = length(years)
    n.strata = dim(obs.loc.likelihood.elements[[1]]$transformation.matrix)[2] / n.years
    n.responses = sum(sapply(obs.loc.likelihood.elements, function(lik.elem){
        length(lik.elem$response.vector)
    }))
     
    # What years do we actually have data for?
    n.data.by.year.and.obs.loc = sapply(obs.loc.likelihood.elements, function(lik.elem){
                tmat = lik.elem$transformation.matrix
                dim(tmat) = c(dim(tmat)[1], n.years, n.strata)
                apply(tmat, 2, sum)
    })
    
    if (length(obs.loc.likelihood.elements)==1)
        years.with.data.mask = n.data.by.year.and.obs.loc > 0
    else
        years.with.data.mask = rowSums(n.data.by.year.and.obs.loc) > 0

    if (any(!years.with.data.mask) && verbose)
        cat("\n        - The following years have no data, and will be dropped from the likelihood: ",
            paste0(years[!years.with.data.mask], collapse=', '),
            "\n        - ",
            sep='')
    
    # We're going to collapse active-IDU and IDU-in-remission strata across the board
    # (since we never have separate data for them)
    # Set up the masks to do this
    dim.names = list(year=as.character(years), age=ages, race=races, sex=sexes, risk=risks)
    active.idu.year.stratum.mask = idu.in.remission.year.stratum.mask =
        array(F, dim=sapply(dim.names, length), dimnames=dim.names)
    active.idu.year.stratum.mask[,,,,'active_IDU'] = T
    idu.in.remission.year.stratum.mask[,,,,'IDU_in_remission'] = T
    
    active.idu.stratum.mask = as.logical(active.idu.year.stratum.mask[1,,,,])
    idu.in.remission.stratum.mask = as.logical(idu.in.remission.year.stratum.mask[1,,,,])
    
    active.idu.year.stratum.mask = as.logical(active.idu.year.stratum.mask)
    idu.in.remission.year.stratum.mask = as.logical(idu.in.remission.year.stratum.mask)
    
    # reset the n strata
    n.strata = sum(!idu.in.remission.stratum.mask)
    idu.strata.after.collapse = (1:n.strata)[active.idu.stratum.mask[!idu.in.remission.stratum.mask]]
    
    # A mask for a stratum-major year x stratum dimension
    year.stratum.with.data.mask = rep(years.with.data.mask, n.strata)
    
    
    years = years[years.with.data.mask]
    if (length(years)==0)
        stop(paste0("There is no '", data.type, "' data for '",
                    msa, "' at the county, MSA, or state level."))
    
    n.years = length(years)
    
    # Set up the data structures to collate across locations
    response.vector = numeric(n.responses)
    response.description = character(n.responses)
    response.year = numeric(n.responses)
    response.location.type = character(n.responses)
    response.location = character(n.responses)
    transformation.array = array(0,
                                 dim=c(response=n.responses,
                                       year=n.years,
                                       loc=n.obs.locations,
                                       stratum=n.strata))
    error.cov.mat = matrix(0, nrow=n.responses, ncol=n.responses)
    
    # collate across locations
    response.index = 0
    for (loc in 1:n.obs.locations)
    {
        lik.elem = obs.loc.likelihood.elements[[loc]]
        n.responses.per = length(lik.elem$response.vector)
        
        response.vector[response.index+1:n.responses.per] = lik.elem$response.vector
        error.cov.mat[response.index+1:n.responses.per, response.index+1:n.responses.per] = lik.elem$numerator.covar.mat

        response.description[response.index+1:n.responses.per] = paste0(obs.location.desriptions[loc], ": ",
                                                                        lik.elem$descriptions)
        response.year[response.index+1:n.responses.per] = as.numeric(substr(lik.elem$descriptions, 1, 4))
        response.location.type[response.index+1:n.responses.per] = obs.location.type[loc]
        response.location[response.index+1:n.responses.per] = obs.location.code[loc]
        
        # before we fold in the transformation array, confirm that the vectors for active-IDU and 
        # IDU-in-remission are indeed identical (ie colinear)
        if (any(lik.elem$transformation.matrix[,active.idu.year.stratum.mask] !=
                lik.elem$transformation.matrix[,idu.in.remission.year.stratum.mask]))
            stop("We are presuming that active-IDU and IDU-in-remission are handeld identically in the data. However, for some observations, they are NOT")

        lik.elem$transformation.matrix = lik.elem$transformation.matrix[,!idu.in.remission.year.stratum.mask]
        transformation.array[response.index+1:n.responses.per,,loc,] = lik.elem$transformation.matrix[,year.stratum.with.data.mask]

        response.index = response.index + n.responses.per
    }
    
    response.year.index = sapply(response.year, function(y){
        rv = (1:length(years))[y==years]
        if (length(rv)!=1)
            stop("could not map year")
        
        rv
    })
    
    dim(transformation.array) = c(n.responses, n.years*n.obs.locations, n.strata)
    
    #add in additional, correlated error
    
    if (!is.null(additional.obs.error.fn))
    {
        additional.error.cov.mat = sapply(1:n.responses, function(i){
            sapply(1:n.responses, function(j){
                if (i==j)
                    1
                else if (response.year[i]==response.year[j] &&
                         response.location[i]==response.location[j])
                   additional.obs.error.correlation
                else
                    0
            })
        })
        
        additional.error.sds = additional.obs.error.fn(response.year, response.vector)
        additional.error.cov.mat = additional.error.sds %*% t(additional.error.sds) * additional.error.cov.mat
        
        error.cov.mat = error.cov.mat + additional.error.cov.mat
    }
    
    if (verbose)
        cat("Done\n")
    
#-- SET UP VARIANCE INFLATION --#
    
    if (inflate.sd.by.n.obs.per.year ||
        inflate.sd.by.n.obs.per.year.and.geography)
    {
        if (inflate.sd.by.n.obs.per.year)
            additional.sd.inflation.per.year = sqrt(sapply(response.year, function(y){
                sum(response.year==y)
            }))
        else
            additional.sd.inflation.per.year = sqrt(sapply(1:n.responses, function(i){
                sum(response.year==response.year[i] & response.location.type==response.location.type[i])
            }))
        
        var.inflation = sd.inflation^2 * additional.sd.inflation.per.year %*% t(additional.sd.inflation.per.year)
    }
    else
        var.inflation = matrix(sd.inflation^2, nrow=n.responses, ncol=n.responses)
    
#-- SET UP N-MULTIPLIERS --#    
    
    if (verbose)
        cat('NSTLIK: Setting up "n-multipliers"...')
    
    n.multipliers = sapply(1:n.metalocations, function(i){
        if (metalocation.type[i] == 'msa')
            matrix(1, nrow=n.years, ncol=n.strata)
        else if (data.type.for.n=='population')
        {
            get.population.for.likelihood(location = counties.for.metalocation[[i]],
                                          census.full.msm = census.full.msm,
                                          idu.manager = idu.manager,
                                          years = years,
                                          age.cutoffs = settings$AGE_CUTOFFS,
                                          races = races,
                                          sexes = sexes,
                                          collapse.idu = T) /
                get.population.for.likelihood(location = msa,
                                              census.full.msm = census.full.msm,
                                              idu.manager = idu.manager,
                                              years = years,
                                              age.cutoffs = settings$AGE_CUTOFFS,
                                              races = races,
                                              sexes = sexes,
                                              collapse.idu = T)
        }
        else
        {
            if (metalocation.type[i]=='county-out-of-msa' &&
                is.one.state.msa(msa))
            {
                super.locations = unique(state.for.county(counties.for.metalocation[[i]]))
                super.surv = state.surveillance
                diff.super.to.1.locations = msa
                diff.super.to.1.surv = msa.surveillance
            }
            else
                super.locations = super.surv = diff.super.to.1.locations = diff.super.to.1.surv = NULL
            
            calculate.outcome.differences(data.type=data.type.for.n,
                                               years=years,
                                               
                                               locations1=counties.for.metalocation[[i]],
                                               surv1=county.surveillance,
                                               locations2=msa,
                                               surv2=msa.surveillance,
                                               
                                               super.locations,
                                               super.surv,
                                               diff.super.to.1.locations,
                                               diff.super.to.1.surv,
                                               
                                               ages=ages,
                                               races=races,
                                               sexes=sexes,
                                               risks=risks,
                                               
                                               collapse.idu.strata = T)
        }
    })
    
    dim(n.multipliers) = c(n.years, n.strata, n.metalocations)
    
    #convert to a list, accessed by stratum
    n.multipliers = lapply(1:n.strata, function(d){
        n.multipliers[,d,]
    })
    
    n.multipliers.sd = lapply(n.multipliers, function(mult){
        mult * n.multiplier.cv * sd.inflation.extra.msa.to.msa / sd.inflation
    })
    
    if (verbose)
        cat("Done\n")
    
#-- SET UP P-BIAS --#    
    
    if (verbose)
        cat('NSTLIK: Setting up "p-multipliers"...\n')
    
    extra.county = NULL
    if (any(metalocation.type=='county-out-of-msa'))
    {
        extra.county = get.extra.msa.county.p.bias.and.variance(data.type,
                                                                data.type.for.n = data.type.for.n,
                                                                msa.surveillance = msa.surveillance,
                                                                county.surveillance = county.surveillance,
                                                                state.surveillance = state.surveillance,
                                                                census=census.collapsed)
        if (extra.county$n < min.n.for.calculating.bias.and.variance)
        {
            if (is.na(BACKUP.DATA.TYPE.FOR.P.BIAS.AND.VARIANCE[data.type]))
                stop("There are ", extra.county$n, " observation(s) from which to calculate ",
                     "out-of-msa county bias and variance in '", data.type, "'. You have specified ",
                     "a minimum of ", min.n.for.calculating.bias.and.variance, " observations.")
            else
                extra.county = get.extra.msa.county.p.bias.and.variance(data.type = BACKUP.DATA.TYPE.FOR.P.BIAS.AND.VARIANCE[data.type],
                                                                        data.type.for.n = data.type.for.n,
                                                                        msa.surveillance = msa.surveillance,
                                                                        county.surveillance = county.surveillance,
                                                                        state.surveillance = state.surveillance)
        }
        
        if (verbose)
        {
            cat("        - p SD for state-to-msa is ", 
                round(sqrt(extra.county$variance), 3), ".",
                sep='')
            
            if (state.to.msa.p.sd.multiplier != 1)
                cat(" Multiplying by ", round(state.to.msa.p.sd.multiplier* sd.inflation.extra.msa.to.msa / sd.inflation,3), 
                    " --> ", round(sqrt(extra.county$variance)*state.to.msa.p.sd.multiplier* sd.inflation.extra.msa.to.msa / sd.inflation,3), ".", sep='')
            
            cat("\n")
        }
    }
    
    if (any(metalocation.type=='county-in-msa'))
    {
        in.county = get.in.msa.county.p.bias.and.variance(data.type,
                                                          data.type.for.n = data.type.for.n,
                                                          msa.surveillance = msa.surveillance,
                                                          county.surveillance = county.surveillance,
                                                          census=census.collapsed,
                                                          fixed.bias = 0)
        
        if (in.county$n < min.n.for.calculating.bias.and.variance)
        {
            if (!is.na(BACKUP.DATA.TYPE.FOR.P.BIAS.AND.VARIANCE[data.type]))
                in.county = get.in.msa.county.p.bias.and.variance(data.type = BACKUP.DATA.TYPE.FOR.P.BIAS.AND.VARIANCE[data.type],
                                                                  data.type.for.n = data.type.for.n,
                                                                  msa.surveillance = msa.surveillance,
                                                                  county.surveillance = county.surveillance,
                                                                  census=census.collapsed)
            else if (!is.null(BACKUP.DATA.TYPES.FOR.IN.V.EXTRA.MSA.SD.RATIO[[data.type]]) &&
                     !is.null(extra.county))
            {
                backup.in.msa = lapply(BACKUP.DATA.TYPES.FOR.IN.V.EXTRA.MSA.SD.RATIO[[data.type]], function(bdt){
                    get.in.msa.county.p.bias.and.variance(data.type = bdt,
                                                          data.type.for.n = NESTED.DATA.TYPES.FOR.N[bdt],
                                                          msa.surveillance = msa.surveillance,
                                                          county.surveillance = county.surveillance,
                                                          census=ALL.DATA.MANAGERS$census.collapsed,
                                                          fixed.bias = 0)
                })
                
                backup.extra.msa = lapply(BACKUP.DATA.TYPES.FOR.IN.V.EXTRA.MSA.SD.RATIO[[data.type]], function(bdt){
                    get.extra.msa.county.p.bias.and.variance(data.type = bdt,
                                                             data.type.for.n = NESTED.DATA.TYPES.FOR.N[bdt],
                                                             msa.surveillance = msa.surveillance,
                                                             county.surveillance = county.surveillance,
                                                             state.surveillance = state.surveillance,
                                                             census=ALL.DATA.MANAGERS$census.collapsed)
                })
                
                sd.ratio = mean(sapply(1:length(backup.in.msa), function(i){
                    backup.in.msa[[i]]$sd / backup.extra.msa[[i]]$sd
                }))
                
                in.county = extra.county
                in.county$sd = in.county$sd * sd.ratio
                in.county$variance = in.county$variance * sd.ratio^2
                in.county$bias = 0
            }
            else
                stop("There are ", in.county$n, " observation(s) from which to calculate ",
                     "in-msa county bias and variance in '", data.type, "'. You have specified ",
                     "a minimum of ", min.n.for.calculating.bias.and.variance, " observations.")
        }
        
        if (verbose)
        {
            cat("        - p SD for county-to-msa is ", 
                round(sqrt(in.county$variance), 3), ".",
                sep='')
            
            if (county.to.msa.p.sd.multiplier != 1)
                cat(" Multiplying by ", round(county.to.msa.p.sd.multiplier* sd.inflation.extra.msa.to.msa / sd.inflation,3), 
                    " --> ", round(sqrt(in.county$variance)*county.to.msa.p.sd.multiplier* sd.inflation.extra.msa.to.msa / sd.inflation,3), ".", sep='')
            
            cat("\n")
        }
    }
    
    p.bias = lapply(1:n.strata, function(d){
        sapply(metalocation.type, function(type){
            if (type=='county-in-msa')
                rep(in.county$bias, n.years)
            else if (type=='county-out-of-msa')
                rep(extra.county$bias, n.years)
            else #type=='msa'
                rep(0, n.years)
        })
    })
    
    p.sd = lapply(1:n.strata, function(d){
        sapply(metalocation.type, function(type){
            if (type=='county-in-msa')
                rep(sqrt(in.county$variance)*county.to.msa.p.sd.multiplier * sd.inflation.extra.msa.to.msa / sd.inflation, n.years)
            else if (type=='county-out-of-msa')
                rep(sqrt(extra.county$variance)*state.to.msa.p.sd.multiplier * sd.inflation.extra.msa.to.msa / sd.inflation, n.years)
            else #type=='msa'
                rep(0, n.years)
        })
    })
      
    if (verbose)
        cat("        - Done\n")
    
#-- PROCESS CONDITION ON MSA MATRIX --#
    
    # A matrix indexed [year, year x metalocation] 
    # the year x metalocation is AFTER applying the above mask
    year.metalocation.to.year.condition.on.location.mapping = array(0, dim=c(n.years, n.years, n.metalocations))
    for (y in 1:n.years)
        year.metalocation.to.year.condition.on.location.mapping[y,y,] = metalocation.to.msa.mapping
    dim(year.metalocation.to.year.condition.on.location.mapping) = c(n.years, n.years*n.metalocations)
    
    # a mask that isolates just the year x metalocation elements in each stratum that we need to map
    #   to the msa that we will condition on
    # a vector of length year x metalocation
    year.metalocation.to.year.condition.on.location.mask = 
        apply(year.metalocation.to.year.condition.on.location.mapping!=0, 2, any)
    
    pre.mask.year.metalocation.to.year.condition.on.location.mapping = 
        year.metalocation.to.year.condition.on.location.mapping
    year.metalocation.to.year.condition.on.location.mapping = 
        year.metalocation.to.year.condition.on.location.mapping[,year.metalocation.to.year.condition.on.location.mask]
    
    
#-- PROCESS MATRICES and MASKS relating to meta.locations --#
    
    if (data.type.for.n=='population')
        state.stratified.populations = lapply(states.with.data, function(state){
            get.population.for.likelihood(location = state,
                                          census.full.msm = census.full.msm,
                                          idu.manager = idu.manager,
                                          years = years,
                                          age.cutoffs = settings$AGE_CUTOFFS,
                                          races = races,
                                          sexes = sexes,
                                          collapse.idu = T)
        })
    else
        state.stratified.populations = lapply(states.with.data, function(state){
            infer.state.population.from.stratifications(state=state,
                                                        data.type=data.type.for.n,
                                                        years=years,
                                                        state.surveillance=state.surveillance,
                                                        desired.dimensions = c('year','age','race','sex','risk'),
                                                        correct.to.yearly.total=T,
                                                        collapse.idu.strata = T)
        })
    names(state.stratified.populations) = states.with.data
    
    # a temporary helper:
    # indexed [year x obs.location, year x metalocation]
    
    state.obs.locations = (1:n.obs.locations)[obs.location.type=='state']
    if (any(obs.location.type=='state'))
    {
        year.metalocation.to.year.state.obs.mapping = sapply(1:n.metalocations, function(meta.index){
            sapply(1:n.years, function(y1){
                sapply(state.obs.locations, function(obs.index){
                    sapply(1:n.years, function(y2){
                        if (y1==y2)
                            metalocation.to.obs.location.mapping[obs.index, meta.index]
                        else
                            0
                    })
                })
            })
        })
        
        dim(year.metalocation.to.year.state.obs.mapping) = c(n.years * length(state.obs.locations), n.years * n.metalocations)
    }
    else
        year.metalocation.to.year.state.obs.mapping = numeric()
    
    # The next two arguments are lists with one element per stratum
    # each element is a vector indexed [year x obs]
    obs.n = lapply(1:n.strata, function(d){
        if (length(state.obs.locations)>0)
        {
            rv=sapply(state.obs.locations, function(obs.index){
                state = obs.location.code[obs.index]
                strat.pop = state.stratified.populations[[state]]
                dim(strat.pop) = c(n.years, n.strata)
                strat.pop[,d]
            })
            as.numeric(rv)
        }
        else
            numeric()
        
    })
    
    # A list with one element for each stratum
    # Each element is a matrix indexed [obs, year x metalocation] 
    year.metalocation.to.year.obs.n.mapping = lapply(1:n.strata, function(d){
        if (length(state.obs.locations)>0)
        {
            mask = !is.na(obs.n[[d]])
            rbind(year.metalocation.to.year.state.obs.mapping[mask,],
                  pre.mask.year.metalocation.to.year.condition.on.location.mapping)
        }
        else
            year.metalocation.to.year.condition.on.location.mapping
    })
    
    #also has zero's post-pended - one for each year
    # to go with the n[,d] 
    obs.n.plus.conditioned.error.variances = lapply(1:n.strata, function(d){
        if (length(state.obs.locations)>0)
        {
            rv = n.error.function(obs.n[[d]])
            if (data.type.for.n=='population')
            {
                # if this is an IDU stratum, apply the inflation
                if (any(d==idu.strata.after.collapse))
                    rv = rv * n.error.inflation.if.calculated
            }
            else
            {
                calculated=sapply(state.obs.locations, function(obs.index){
                    state = obs.location.code[obs.index]
                    strat.pop = state.stratified.populations[[state]]
                    calculated = attr(strat.pop, 'calculated')
                    dim(calculated) = c(n.years, n.strata)
                    calculated[,d]
                })
                calculated = as.numeric(calculated)
                rv[calculated] = rv[calculated] * n.error.inflation.if.calculated
            }            
            
            mask = !is.na(obs.n[[d]])
            rv = rv[mask]
        }
        else
            rv = numeric()
        
        c(as.numeric(rv),
          rep(0, n.years)) #add zero variance for when we condition on the simulated msa n
    })
    
    # remove the na observations
    obs.n = lapply(obs.n, function(o){
        o[!is.na(o)]
    })
    
    
#-- PROCESS MATRICES and MASKS relating to obs.locations --#
    
    ##-- aggregating across strata --#
    
    # in matrix form (and transpose)
    # was only used for the R version
 #   year.loc.stratum.to.obs.mapping.as.mat = transformation.array
 #   dim(year.loc.stratum.to.obs.mapping.as.mat) = c(n.responses, n.years * n.obs.locations * n.strata)
 #   t.year.loc.stratum.to.obs.mapping.as.mat = t(year.loc.stratum.to.obs.mapping.as.mat)
    
    # in list form (no mask)
    full.year.loc.stratum.to.obs.mapping = lapply(1:n.strata, function(d){
        rv = transformation.array[,,d]
        dim(rv) = c(n.responses, n.years * n.obs.locations)
        rv
    })
    
    # in list form (and mask)
    year.loc.stratum.to.obs.mask = lapply(full.year.loc.stratum.to.obs.mapping, function(mapping){
        apply(mapping!=0, 2, any)
    })
    year.loc.stratum.to.obs.mapping = lapply(1:n.strata, function(d){
        full.year.loc.stratum.to.obs.mapping[[d]][, year.loc.stratum.to.obs.mask[[d]] ]
    })
    
    #--  year x metalocation to year x obs.location --#
    year.metalocation.to.year.obs.location.mapping = array(0, dim=c(n.years, n.obs.locations, n.years, n.metalocations))
    for (y in 1:n.years)
        year.metalocation.to.year.obs.location.mapping[y,,y,] = metalocation.to.obs.location.mapping
    dim(year.metalocation.to.year.obs.location.mapping) = c(n.years*n.obs.locations, n.years*n.metalocations)
    
    year.metalocation.to.year.obs.location.mask = apply(year.metalocation.to.year.obs.location.mapping!=0, 2, any)
    pre.mask.year.metalocation.to.year.obs.location.mapping = year.metalocation.to.year.obs.location.mapping
    year.metalocation.to.year.obs.location.mapping = 
        year.metalocation.to.year.obs.location.mapping[,year.metalocation.to.year.obs.location.mask]
    
    full.year.metalocation.to.obs.mapping = lapply(1:n.strata, function(d){
        year.loc.stratum.to.obs.mapping[[d]] %*%
            pre.mask.year.metalocation.to.year.obs.location.mapping[year.loc.stratum.to.obs.mask[[d]],]
    })
    
    #-- the map from metalocations to obs --#
    
    # pre-computed:
    #        year.loc.stratum.to.obs.mapping %*% year.metalocation.to.year.obs.location.mapping
    # No mask
    #
    # *pre.mask.year.metalocation.to.year.obs.location.mapping here is taken to be the (repeated)
    #  block in a blockwise diagonal matrix
    # Was only used for R version
#    year.metalocation.stratum.to.obs.mapping = sapply(1:n.strata, function(d){
#        transformation.array[,,d] %*% pre.mask.year.metalocation.to.year.obs.location.mapping
#    })
#    dim(year.metalocation.stratum.to.obs.mapping) = c(n.responses, n.years*n.metalocations*n.strata)
    
#-- SET UP THE EXTRACT FUNCTIONS --#
    
    if (verbose)
        cat('NSTLIK: Setting up extract functions...')
    
    if (data.type=='linkage')
    {
        total.new = get.surveillance.data(msa.surveillance, location.codes = msa, data.type='new')
        total.new = unlist(interpolate.parameters(values=total.new[!is.na(total.new)],
                                                  value.times=attr(total.new, 'years')[!is.na(total.new)],
                                                  desired.times = years))
        
        extract.np.fn = function(sim){
            n = do.extract.new.diagnoses(sim, years=years, keep.dimensions=c('year','age','race','sex','risk'), 
                                         per.population=NA, use.cdc.categorizations = F)
            p = extract.linkage(sim, years=years, keep.dimensions=c('year','age','race','sex','risk'), 
                                per.population=1, use.cdc.categorizations = F)
            
            list(n=n / rowSums(n) * total.new,
                 p=p)
        }
    }
    else if (data.type=='testing')
    {
        total.population = as.numeric(get.census.totals(census.totals, location=msa, years=years, 
                                             interpolate.missing.years = 'all'))
        
        extract.np.fn = function(sim){
            denominator = do.extract.population.subset(sim, years=years, keep.dimensions=c('year','age','race','sex','risk'),
                                                       per.population = NA, use.cdc.categorizations = F)
            p = extract.testing.proportions(sim, years=years, keep.dimensions=c('year','age','race','sex','risk'),
                                            per.population = 1, use.cdc.categorizations = F)
            
            list(n=denominator / rowSums(denominator) * total.population,
                 p=p)
        }
    }
    else 
    {
        total.prevalence = get.surveillance.data(msa.surveillance, location.codes = msa, data.type='prevalence')
        total.prevalence = unlist(interpolate.parameters(values=total.prevalence[!is.na(total.prevalence)],
                                                         value.times=attr(total.prevalence, 'years')[!is.na(total.prevalence)],
                                                         desired.times = years))
        
        if (data.type=='diagnosed')
            extract.np.fn = function(sim){
                denominator = do.extract.prevalence(sim, years=years, keep.dimensions=c('year','age','race','sex','risk'), 
                                                    per.population=NA, use.cdc.categorizations = F)
                numerator = do.extract.diagnosed.hiv(sim, years=years, keep.dimensions=c('year','age','race','sex','risk'), 
                                                     per.population=NA, use.cdc.categorizations = F)
                
                p = numerator/denominator
                
                list(n=numerator / rowSums(numerator) * total.prevalence / p,
                     p=p)
            }
        else if (data.type=='suppression')
            extract.np.fn = function(sim){
                denominator = do.extract.diagnosed.hiv(sim, years=years, keep.dimensions=c('year','age','race','sex','risk'),
                                                       per.population = NA, use.cdc.categorizations = F)
                p = extract.suppression(sim, years=years, keep.dimensions=c('year','age','race','sex','risk'),
                                        continuum = sim$diagnosed.continuum.states,
                                        per.population = 1, use.cdc.categorizations = F)
                
                list(n=denominator / rowSums(denominator) * total.prevalence,
                     p=p)
            }
        else if (data.type=='engagement')
            extract.np.fn = function(sim){
                denominator = do.extract.diagnosed.hiv(sim, years=years, keep.dimensions=c('year','age','race','sex','risk'),
                                                       per.population = NA, use.cdc.categorizations = F)
                p = do.extract.engagement(sim, years=years, keep.dimensions=c('year','age','race','sex','risk'),
                                          continuum = sim$diagnosed.continuum.states,
                                          per.population = 1, use.cdc.categorizations = F)
                
                list(n=denominator / rowSums(denominator) * total.prevalence,
                     p=p)
            }
        else if (data.type=='retention')
        {
#            total.engagement = get.surveillance.data(msa.surveillance, location.codes = msa, data.type='engagement')
#            total.engagement = unlist(interpolate.parameters(values=total.engagement[!is.na(total.engagement)],
#                                                             value.times=attr(total.engagement, 'years')[!is.na(total.engagement)],
#                                                             desired.times = years))
            
#            engaged.prevalence = total.prevalence * total.engagement
#            if (any(is.na(engaged.prevalence)))
#                stop('Unable to pull engaged prevalence')
            
            
            extract.np.fn = function(sim){
                
                p.total.engaged = do.extract.engagement(sim, years=years, keep.dimensions=c('year'),
                                                  continuum = sim$diagnosed.continuum.states,
                                                  per.population = 1, use.cdc.categorizations = F)
                
                engaged.prevalence = total.prevalence * p.total.engaged
                
                
                engaged.states = attr(sim, 'components')$settings$ENGAGED_STATES
                denominator = do.extract.prevalence(sim, years=years, 
                                                    keep.dimensions=c('year','age','race','sex','risk'),
                                                    continuum = engaged.states,
                                                    per.population = NA, use.cdc.categorizations = F)
                p = extract.retention(sim, years=years, keep.dimensions=c('year','age','race','sex','risk'),
                                      continuum = engaged.states,
                                      per.population = 1, use.cdc.categorizations = F)
                
                list(n=denominator / rowSums(denominator) * engaged.prevalence,
                     p=p)
            }
        }
        else
            stop("Invalid data.type")
    }
    
    if (verbose)
        cat('DONE.\n')
    
    
#-- CREATE THE FUNCTION AND RETURN --#
    browser()
    if (verbose)
        cat("NSTLIK: All done. Packaging up a function and returning.")
    # return a list of anything I want to see
    function(sim, log=T, debug=F) {
        
        np = extract.np.fn(sim)
        p = np$p
        n = np$n
        dim(p) = dim(n) = c(n.years, length(p)/n.years)
        
        #collapse idu strata
        p[,active.idu.stratum.mask] = 
            (p[,active.idu.stratum.mask] * n[,active.idu.stratum.mask] +
                 p[,idu.in.remission.stratum.mask] * n[,idu.in.remission.stratum.mask]) /
            (n[,active.idu.stratum.mask] + n[,idu.in.remission.stratum.mask])
        p = p[,!idu.in.remission.stratum.mask]
        
        n[,active.idu.stratum.mask] = n[,active.idu.stratum.mask] + n[,idu.in.remission.stratum.mask]
        n = n[,!idu.in.remission.stratum.mask]
        
        
        # The continous likelihood
        tryCatch({
            lik.continuous = nested.proportion.likelihood.sub(
                p = p,
                n = n,
                
                year.metalocation.n.multipliers = n.multipliers, 
                year.metalocation.n.multiplier.sd = n.multipliers.sd,
                year.metalocation.p.bias = p.bias,
                year.metalocation.p.sd = p.sd,
                
                metalocation.p.correlation = within.location.p.error.correlation,
                metalocation.n.multiplier.correlation = within.location.n.error.correlation,
                
                year.metalocation.to.year.obs.n.mapping = year.metalocation.to.year.obs.n.mapping,
                obs.n = obs.n,
                obs.n.plus.conditioned.error.variances = obs.n.plus.conditioned.error.variances,
                
                year.metalocation.to.year.condition.on.location.mask=year.metalocation.to.year.condition.on.location.mask,
                year.metalocation.to.year.condition.on.location.mapping=year.metalocation.to.year.condition.on.location.mapping,
                
                year.metalocation.to.year.obs.location.mask = year.metalocation.to.year.obs.location.mask,
                year.metalocation.to.year.obs.location.mapping = year.metalocation.to.year.obs.location.mapping,
                
                full.year.loc.stratum.to.obs.mapping = full.year.loc.stratum.to.obs.mapping,
                full.year.metalocation.to.obs.mapping = full.year.metalocation.to.obs.mapping,
                
             #   year.loc.stratum.to.obs.mask = year.loc.stratum.to.obs.mask,
             #   year.loc.stratum.to.obs.mapping = year.loc.stratum.to.obs.mapping,
             #   t.year.loc.stratum.to.obs.mapping.as.mat = t.year.loc.stratum.to.obs.mapping.as.mat,
                
             #   year.metalocation.stratum.to.obs.mapping = year.metalocation.stratum.to.obs.mapping,
                
                obs.year.index = response.year.index,
                obs.p = response.vector,
                obs.error = error.cov.mat,
                
                var.inflation = var.inflation,
                
                obs.descriptions = response.description,
                log=log,
                debug=debug
            )
            },
            error=function(e){
                save(sim, data.type, file='mcmc_runs/nested_lik_error_info.Rdata')
                stop(e)
            })
        
        # The binary (is decreasing) likelihood
        if (is.na(probability.decreasing.slope))
        {
            if (log)
                lik.binary = 0
            else
                lik.binary = 1
        }
        else
        {
            is.decreasing = as.numeric(p[length(years),] < p[1,])
            
            if (log)
                lik.binary = sum(is.decreasing*log(probability.decreasing.slope) + (1-is.decreasing)*log(1-probability.decreasing.slope))
            else
                lik.binary = prod(probability.decreasing.slope ^ is.decreasing * (1-probability.decreasing.slope)^(1-is.decreasing))
        }
        
        # Put them together and return
        
        if (log)
            lik.binary + lik.continuous
        else
            lik.binary * lik.continuous
    }
}


##----------------------------------------##
##----------------------------------------##
##--     THE LIKELIHOOD SUB FUNCTION    --##
##-- (actually computes the likelihood) --##
##----------------------------------------##
##----------------------------------------##


# SOME NOTES ON LOCATION INDEXING:
#   metalocations are non-overlapping. They will correspond to one of:
#       (a) a single county
#       (b) a group of counties within the MSA (a proper subset of the MSA counties)
#       (c) a group of counties within a state but not in the MSA
#       (d) the MSA itself (if there is no county data or if the MSA comprises one county only)
#   - arrays indexed by year and metalocation have one element for every metalocation 
#     for every year under consideration
#   aggregate.metalocations - are a sum of two or more metalocations. They will correspond to one of:
#       (a) the MSA
#       (b) the set of counties in both the MSA and in a state (the intersection of counties) - for multi-state MSAs
#       (c) a state
#   expanded.metalocations - union of a subset of metalocations and aggregate.metalocations 
#   - arrays indexed by year and expanded.metalocation have an element for every expanded location only for
#     years when there is at least one observation deriving from that expanded.metalocation
#   obs.locations - aggregates of one or more expanded.metalocations. There is exactly one obs.location for each
#       real-world location (state, city, county) where we have at least one observation at some time

nested.proportion.likelihood.sub <- function(
        
    #-- SET-UP ARGUMENTS --#
    
    p, #array, indexed [year, stratum]
    n, #array, indexed [year, stratum]
    
    # Each of the next four arguments are a list
    # One element for each stratum
    # Each element is a matrix indexed [year, metalocation]
    year.metalocation.n.multipliers, 
    year.metalocation.n.multiplier.sd,
    year.metalocation.p.bias,
    year.metalocation.p.sd,
    
    # The next two are scalar arguments
    metalocation.p.correlation,
    metalocation.n.multiplier.correlation,
    
    
    #-- CONDITION ON N ARGUMENTS --#
    #   We condition on two things: 
    #       (a) the metalocation n's for the MSA
    #       (b) some (arbitrary) number of observations which are a combination of metalocations
    
    
    # A list with one element for each stratum
    # Each element is a matrix indexed [obs, year x metalocation] 
    year.metalocation.to.year.obs.n.mapping,
    
    
    # The next two arguments are lists with on element per stratum
    # each element is a vector indexed [obs]
    obs.n,
    
    obs.n.plus.conditioned.error.variances, #also has zero's post-pended - one for each year
    # to go with the n[,d] 
    
    
    #-- CONDITION ON MSA ARGUMENTS --#
    
    # a mask that isolates just the year x metalocation elements in each stratum that we need to map
    #   to the msa that we will condition on
    # a vector of length year x metalocation
    year.metalocation.to.year.condition.on.location.mask,
    
    
    # A matrix indexed [year, year x metalocation] 
    # the year x metalocation is AFTER applying the above mask
    year.metalocation.to.year.condition.on.location.mapping,
    
    
    #-- TRANSFORM TO OBS LOCATION --#
    
    # a mask that isolates just the year x metalocation elements in each stratum that we need to map
    #   to the observed locations
    # a vector of length year x metalocation
    year.metalocation.to.year.obs.location.mask,
    
    # indexed [year x obs location, year x metalocation] 
    # the year x metalocation is AFTER applying the above mask
    year.metalocation.to.year.obs.location.mapping,
    
    #-- AGGREGATE (ACROSS STRATA) TO OBS --#
    
    # a list, one element for each stratum
    # each element is a vector mask, indexed [year x obs.location]
# *NOT USING IN CPP VERSION *
#    year.loc.stratum.to.obs.mask,
    
    # An list, one element for each stratum
    # Each element is a matrix that aggregates maps from year x obs.location to observations,
    #       for that stratum
    # indexed [obs, year x obs.location]
    # year x obs.location AFTER applying the above mask
# *NOT USING IN CPP VERSION *
#    year.loc.stratum.to.obs.mapping, # used only in the R version
    
    # A transposed matrix which is the matrix version of the list above
    # indexed [year x obs.location x stratum, obs]
    # No masks applied
# *NOT USING IN CPP VERSION *
#    t.year.loc.stratum.to.obs.mapping.as.mat,
    
    # A matrix, indexed [obs, year x metalocation x stratum]
    #  (this is a pre-computed:
    #        year.loc.stratum.to.obs.mapping %*% year.metalocation.to.year.obs.location.mapping)
    # Applies AFTER year.metalocation.to.year.obs.location.mask
# *NOT USING IN CPP VERSION *
#  year.metalocation.stratum.to.obs.mapping,
    
    
    # An list, one element for each stratum
    # Each element is a matrix that aggregates maps from year x obs.location to observations,
    #       for that stratum
    # indexed [obs, year x obs.location]
    # year x obs.location BEFORE applying the above mask
    # (used for the C++ version of code only)
    full.year.loc.stratum.to.obs.mapping,
    
    # An list, one element for each stratum
    # Each element is a matrix that aggregates maps from year x metalocations to observations,
    #       for that stratum
    # indexed [obs, year x metalocation]
    # year x obs.location BEFORE applying any masks
    # (used for the C++ version of code only)
    full.year.metalocation.to.obs.mapping,
    
    # A vector of length n obs
    # Each value is the index into years for the year to which each observation corresponds
    obs.year.index,
    
    # A numeric vector with one value for each observation
    obs.p,
    
    # A covariance matrix with dimension length(obs.p) x length(obs.p)
    obs.error,
    
    # A matrix of dimension n.obs x n.obs
    #   how much to inflate the variance-covariance matrix (before factoring in obs.error)
    var.inflation=NULL,
    sd.inflation=NULL, #keeping the old one for backward compatibility for now
    
    #-- MISC ARGUMENTS --#
    obs.descriptions, #for debugging purposes
    log=T,
    debug=F
)
{
    #-- Pull from CPP code --#
        likelihood.elements = get_nested_proportion_likelihood_components(p = p,
                                                                          n = n,
                                                                          
                                                                          year_metalocation_n_multipliers = year.metalocation.n.multipliers,
                                                                          year_metalocation_n_multiplier_sd = year.metalocation.n.multiplier.sd,
                                                                          year_metalocation_p_bias = year.metalocation.p.bias,
                                                                          year_metalocation_p_sd = year.metalocation.p.sd,
                                                                          
                                                                          metalocation_p_correlation = metalocation.p.correlation,
                                                                          metalocation_n_multiplier_correlation = metalocation.n.multiplier.correlation,
                                                                          
                                                                          year_metalocation_to_year_obs_n_mapping = year.metalocation.to.year.obs.n.mapping,
                                                                          
                                                                          obs_n = obs.n,
                                                                          
                                                                          obs_n_plus_conditioned_error_variances = obs.n.plus.conditioned.error.variances,
                                                                          
                                                                          year_metalocation_to_year_condition_on_location_mask = year.metalocation.to.year.condition.on.location.mask,
                                                                          
                                                                          year_metalocation_to_year_condition_on_location_mapping = year.metalocation.to.year.condition.on.location.mapping,
                                                                          
                                                                          year_metalocation_to_year_obs_location_mask = year.metalocation.to.year.obs.location.mask,
                                                                          
                                                                          year_metalocation_to_year_obs_location_mapping = year.metalocation.to.year.obs.location.mapping,
                                                                          
                                                                          #        year_loc_stratum_to_obs_mask = year.loc.stratum.to.obs.mask,
                                                                          
                                                                          year_loc_stratum_to_obs_mapping = full.year.loc.stratum.to.obs.mapping,
                                                                          
                                                                          #          year_metalocation_stratum_to_obs_mapping = year.metalocation.stratum.to.obs.mapping,
                                                                          
                                                                          year_metalocation_to_obs_mapping = full.year.metalocation.to.obs.mapping,
                                                                          
                                                                          obs_year_index = obs.year.index,
                                                                          
                                                                          obs_p = obs.p,
                                                                          
                                                                          obs_error = obs.error,
                                                                          
                                                                          var_inflation = var.inflation
        )
        
        if (debug)
            print("Ran successfully through cpp function")
    
        
        #-- for checking/debugging --#
        
        if (debug)
        {
            summ = data.frame(obs=obs.descriptions, 
                              obs=round(obs.p,3),
                              mean=round(likelihood.elements$mean.v / likelihood.elements$obs.n,3),
                              sd=round(sqrt(diag(likelihood.elements$cov.mat)) / likelihood.elements$obs.n,3))
            print(summ)
            
            browser()
            
            round(cov2cor(likelihood.elements$cov.mat),2)
            sqrt(diag(likelihood.elements$cov.mat))/likelihood.elements$obs.n
        }
        
        if (any(is.na(likelihood.elements$obs.v)))
            stop("NAs produced in obs.v")
        if (any(is.na(likelihood.elements$mean.v)))
            stop("NAs produced in mean.v")
        if (any(is.na(likelihood.elements$cov.mat)))
            stop("NAs produced in cov.mat")
        
        
        #-- Compute the density --#
        
        dmvnorm(x = likelihood.elements$obs.v,
                mean = likelihood.elements$mean.v,
                sigma = likelihood.elements$cov.mat,
                log=log)
    
}

# This is the code that was used in running the R implementation
OLD.nested.proportion.likelihood.sub <- function(
    
    #-- SET-UP ARGUMENTS --#
    
    p, #array, indexed [year, stratum]
    n, #array, indexed [year, stratum]
    
    # Each of the next four arguments are a list
    # One element for each stratum
    # Each element is a matrix indexed [year, metalocation]
    year.metalocation.n.multipliers, 
    year.metalocation.n.multiplier.sd,
    year.metalocation.p.bias,
    year.metalocation.p.sd,
    
    # The next two are scalar arguments
    metalocation.p.correlation,
    metalocation.n.multiplier.correlation,
    
    
    #-- CONDITION ON N ARGUMENTS --#
    #   We condition on two things: 
    #       (a) the metalocation n's for the MSA
    #       (b) some (arbitrary) number of observations which are a combination of metalocations
    
    
    # A list with one element for each stratum
    # Each element is a matrix indexed [obs, year x metalocation] 
    year.metalocation.to.year.obs.n.mapping,
    
    
    # The next two arguments are lists with on element per stratum
    # each element is a vector indexed [obs]
    obs.n,
    
    obs.n.plus.conditioned.error.variances, #also has zero's post-pended - one for each year
                                            # to go with the n[,d] 
    
    
    #-- CONDITION ON MSA ARGUMENTS --#
    
    # a mask that isolates just the year x metalocation elements in each stratum that we need to map
    #   to the msa that we will condition on
    # a vector of length year x metalocation
    year.metalocation.to.year.condition.on.location.mask,
    
    
    # A matrix indexed [year, year x metalocation] 
    # the year x metalocation is AFTER applying the above mask
    year.metalocation.to.year.condition.on.location.mapping,
    
    
    #-- TRANSFORM TO OBS LOCATION --#
    
    # a mask that isolates just the year x metalocation elements in each stratum that we need to map
    #   to the observed locations
    # a vector of length year x metalocation
    year.metalocation.to.year.obs.location.mask,
    
    # indexed [year x obs location, year x metalocation] 
    # the year x metalocation is AFTER applying the above mask
    year.metalocation.to.year.obs.location.mapping,
    
    #-- AGGREGATE (ACROSS STRATA) TO OBS --#
    
    # a list, one element for each stratum
    # each element is a vector mask, indexed [year x obs.location]
    year.loc.stratum.to.obs.mask,
    
    # An list, one element for each stratum
    # Each element is a matrix that aggregates maps from year x obs.location to observations,
    #       for that stratum
    # indexed [obs, year x obs.location]
    # year x obs.location AFTER applying the above mask
    year.loc.stratum.to.obs.mapping, # used only in the R version
    
    # A transposed matrix which is the matrix version of the list above
    # indexed [year x obs.location x stratum, obs]
    # No masks applied
    t.year.loc.stratum.to.obs.mapping.as.mat,
    
    # A matrix, indexed [obs, year x metalocation x stratum]
    #  (this is a pre-computed:
    #        year.loc.stratum.to.obs.mapping %*% year.metalocation.to.year.obs.location.mapping)
    # Applies AFTER year.metalocation.to.year.obs.location.mask
    year.metalocation.stratum.to.obs.mapping,
    
    
    # An list, one element for each stratum
    # Each element is a matrix that aggregates maps from year x obs.location to observations,
    #       for that stratum
    # indexed [obs, year x obs.location]
    # year x obs.location BEFORE applying the above mask
    # (used for the C++ version of code only)
    full.year.loc.stratum.to.obs.mapping,
    
    # An list, one element for each stratum
    # Each element is a matrix that aggregates maps from year x metalocations to observations,
    #       for that stratum
    # indexed [obs, year x metalocation]
    # year x obs.location BEFORE applying any masks
    # (used for the C++ version of code only)
    full.year.metalocation.to.obs.mapping,
    
    # A vector of length n obs
    # Each value is the index into years for the year to which each observation corresponds
    obs.year.index,
    
    # A numeric vector with one value for each observation
    obs.p,
    
    # A covariance matrix with dimension length(obs.p) x length(obs.p)
    obs.error,
    
    # A matrix of dimension n.obs x n.obs
    #   how much to inflate the variance-covariance matrix (before factoring in obs.error)
    var.inflation=NULL,
    sd.inflation=NULL, #keeping the old one for backward compatibility for now
    
    #-- MISC ARGUMENTS --#
    obs.descriptions, #for debugging purposes
    log=T,
    debug=F
)
{
#-- Some general variables --#
    
    n.years = dim(p)[1]
    years = dimnames(p)[[1]]
    n.strata = dim(p)[2]
    n.obs = length(obs.p)
    n.obs.locations = length(year.loc.stratum.to.obs.mask[[1]]) / n.years
    n.metalocations = dim(year.metalocation.p.sd[[1]])[2]
 
    
    # for backward compatibility for now
    if (is.null(var.inflation))
        var.inflation = matrix(sd.inflation^2, nrow=n.obs, ncol=n.obs)
    
#-- Set up the mean and covariance for n --#
    
    # indexed [metalocation]
    theta = rep(metalocation.n.multiplier.correlation, n.metalocations)
    
    lambda.by.stratum = lapply(1:n.strata, function(d){
        as.numeric(n[,d] * year.metalocation.n.multipliers[[d]])
    })
    tau.by.stratum = lapply(1:n.strata, function(d){
        n[,d] * year.metalocation.n.multiplier.sd[[d]]
    })

    # list, one element per dimension
    # each element is a matrix, indexed [year x metalocation, year x metalocation]
    T.by.stratum = lapply(1:n.strata, function(d){
        
        tau = tau.by.stratum[[d]]
        
        rv = sapply(1:n.metalocations, function(i){
            sapply(1:n.years, function(t){
                sapply(1:n.metalocations, function(j){
                    sapply(1:n.years, function(s){
                        if (i==j)
                        {
                            if (s==t) # the variance - page 2 of equations
                                tau[t,i]^2
                            else # the covariance for same loc - page 2 of equations
                                theta[i] * tau[t,i] * tau[s,i]
                        }
                        else # the covariance across locations - page 2 of equations
                            0
                    })
                })
            })
        })
        
        #to ensure symmetry from rounding errors (this is painful to have to do this)
        dim(rv) = c(n.years*n.metalocations, n.years*n.metalocations)
        rv = sapply(1:(n.years*n.metalocations), function(i){
            sapply(1:(n.years*n.metalocations), function(j){
                if (i<j)
                    rv[i,j]
                else
                    rv[j,i]
            })
        })
        
        # return
        rv
    })
    # We could optimize this a bit because it's block diagonal
    
#-- Prepare to condition on observed n's --#
#     (join obs.n with condition.on)    
    
    obs.n = lapply(1:n.strata, function(d){
        c(obs.n[[d]], n[,d])
    })
    
#-- Condition on obs n --#
    
    gamma.by.stratum = lapply(1:n.strata, function(d){
        M = year.metalocation.to.year.obs.n.mapping[[d]]
        T = T.by.stratum[[d]]
        
        # page 4 of equations
        tau.Mt = T %*% t(M)
        tau.Mt %*% solve(M%*% tau.Mt)
    })
    
    nu.by.stratum = lapply(1:n.strata, function(d){
        # page 4 of equations
        M = year.metalocation.to.year.obs.n.mapping[[d]]
        rv = lambda.by.stratum[[d]] +
            gamma.by.stratum[[d]] %*% (obs.n[[d]] - M %*% lambda.by.stratum[[d]])
        
        dim(rv) = c(n.years, n.metalocations)
        rv
    })
    
    psi.by.stratum = lapply(1:n.strata, function(d){
        # page 4 of equations
        M = year.metalocation.to.year.obs.n.mapping[[d]]
        T = T.by.stratum[[d]]
        gamma = gamma.by.stratum[[d]]
        
        # take advantage of the fact that 'omega' is diag(obs.n.plus.conditioned.error.variances[[d]])
        gamma.omega = sapply(1:(dim(gamma)[2]), function(i){
            obs.n.plus.conditioned.error.variances[[d]][i] * gamma[,i]
        })
        
        rv = T - gamma %*% M %*% T + gamma.omega %*% t(gamma)
        
        #to ensure symmetry from rounding errors (this is painful to have to do this)
        rv = sapply(1:nrow(rv), function(i){
            sapply(1:ncol(rv), function(j){
                if (i<j)
                    rv[i,j]
                else
                    rv[j,i]
            })
        })
        
        # change up dimensions and return
        dim(rv) = c(n.years, n.metalocations, n.years, n.metalocations)
        rv
    })
    
    
#-- Set up the mean vectors and covariance matrices for metalocations --#
    
    # indexed [metalocation]
    phi = rep(metalocation.p.correlation, n.metalocations)
    
    mu.by.stratum = lapply(1:n.strata, function(d){
        p[,d] + year.metalocation.p.bias[[d]]
    })
    sigma.by.stratum = year.metalocation.p.sd
    
    #each element indexed [year, metalocation]
    alpha.by.stratum = lapply(1:n.strata, function(d){
        as.numeric(mu.by.stratum[[d]] * nu.by.stratum[[d]]) # page 5 of equations
    })
    
    #each element indexed [year, metalocation, year, metalocation]
    delta.by.stratum = lapply(1:n.strata, function(d){
        mu = mu.by.stratum[[d]]
        sigma = sigma.by.stratum[[d]]
        nu = nu.by.stratum[[d]]
        psi = psi.by.stratum[[d]]
        
        rv = sapply(1:n.metalocations, function(i){
            sapply(1:n.years, function(t){
                sapply(1:n.metalocations, function(j){
                    sapply(1:n.years, function(s){
                        if (i==j)
                        {
                            if (s==t) # the variance - page 5 of equations
                            {
                                nu[t,i] * mu[t,i] * (1-mu[t,i]) +
                                    sigma[t,i]^2 * nu[t,i] * (nu[t,i]-1) +
                                    psi[t,i,t,i] * mu[t,i]^2 +
                                    sigma[t,i]^2 * psi[t,i,t,i]
                            }
                            else # the covariance for same loc - page 6 of equations
                            {
                                mu[t,i] * mu[s,i] * psi[t,i,s,i] +
                                    phi[i] * sigma[t,i] * sigma[s,i] * 
                                    ( nu[t,i] * nu[s,i] + psi[t,i,s,i] )
                            }
                        }
                        else # the covariance across locations - page 7 of equations
                            psi[t,i,s,j] * mu[t,i] * mu[s,j]
                    })
                })
            })
        })
        
        #to ensure symmetry from rounding errors (this is painful to have to do this)
        dim(rv) = c(n.years*n.metalocations, n.years*n.metalocations)
        rv = sapply(1:(n.years*n.metalocations), function(i){
            sapply(1:(n.years*n.metalocations), function(j){
                if (i<j)
                    rv[i,j]
                else
                    rv[j,i]
            })
        })
        
        # return
        rv
    })
    
#-- Condition on MSA totals --#

    M = year.metalocation.to.year.condition.on.location.mapping
    Mt = t(M)
    mask = year.metalocation.to.year.condition.on.location.mask
    B = year.metalocation.to.year.obs.location.mapping
    Bt = t(B)
    
    gamma.by.stratum = lapply(1:n.strata, function(d){
        # page 8 of equations
        delta.Mt = delta.by.stratum[[d]][,mask] %*% Mt #there is a problem here with the masking
        B %*% delta.Mt[year.metalocation.to.year.obs.location.mask,] %*% solve(M%*% delta.Mt[mask,])
    })
    
    mean.year.obs.loc.by.stratum = lapply(1:n.strata, function(d){
        # page 9 of equations
        alpha = alpha.by.stratum[[d]]
        o = n[,d] * p[,d] # we condition on what the sim total is
        
        B %*% alpha[year.metalocation.to.year.obs.location.mask] + gamma.by.stratum[[d]] %*% (o - M %*% alpha[mask])
    })
    
    cov.mat.year.obs.loc.by.stratum = lapply(1:n.strata, function(d){
        # page 9 of equations
    
        delta = delta.by.stratum[[d]]
        gamma = gamma.by.stratum[[d]]
        o.var = n[,d] * p[,d] * (1-p[,d]) #just the binomial variance for the metalocations representing the msa
        
        # take advantage of the fact that 'omega' is diag(o.var)
        gamma.omega = sapply(1:ncol(gamma), function(i){
            o.var[i] * gamma[,i]
        })
        
        rv = B %*% delta[year.metalocation.to.year.obs.location.mask,year.metalocation.to.year.obs.location.mask] %*% Bt - 
            gamma %*% M %*% delta[mask,year.metalocation.to.year.obs.location.mask] %*% Bt + 
            gamma.omega %*% t(gamma)

        #to ensure symmetry from rounding errors (this is painful to have to do this)
        rv = sapply(1:nrow(rv), function(i){
            sapply(1:ncol(rv), function(j){
                if (i<j)
                    rv[i,j]
                else
                    rv[j,i]
            })
        })
        
        rv
    })
    
    
#-- Aggregate across strata within obs locations --#    

    mean.v = rowSums(sapply(1:n.strata, function(d){
        year.loc.stratum.to.obs.mapping[[d]] %*%
            mean.year.obs.loc.by.stratum[[d]][year.loc.stratum.to.obs.mask[[d]]]
    }))
    
    # conceive of cov.mat.year.obs.loc.by.stratum as a 
    #  block-diagonal covariance matrices, where each 
    #  element of the list is one block
    M.times.cov.mat = sapply(1:n.strata, function(d){
        mask = year.loc.stratum.to.obs.mask[[d]]
        year.loc.stratum.to.obs.mapping[[d]] %*% cov.mat.year.obs.loc.by.stratum[[d]][mask,]
    })
    dim(M.times.cov.mat) = c(n.obs, n.years * n.obs.locations * n.strata)
    
    cov.mat = M.times.cov.mat %*% t.year.loc.stratum.to.obs.mapping.as.mat
    
#-- Aggregate the nu's in to n's for each observation --#    
    
    obs.n = rowSums(sapply(1:n.strata, function(d){
        year.loc.stratum.to.obs.mapping[[d]] %*%
            (year.metalocation.to.year.obs.location.mapping[year.loc.stratum.to.obs.mask[[d]],] %*% 
                 as.numeric(nu.by.stratum[[d]])[year.metalocation.to.year.obs.location.mask])
    }))


#-- Inflate the SDs and add in the observation error --#    
    
    cov.mat = cov.mat + 
        ( obs.n %*% t(obs.n) ) * obs.error  
    
    cov.mat = cov.mat * var.inflation
    
#-- Map the obs n --#
    
 #   obs.n = year.metalocation.stratum.to.obs.mapping[,year.metalocation.to.year.obs.location.mask] %*% 
  #      unlist(nu.by.stratum)[year.metalocation.to.year.obs.location.mask]

    
#-- Compute the density --#
    
    obs.v = as.numeric(obs.p * obs.n)
    
    #for checking/debugging
    if (debug)
    {
        summ = data.frame(obs=obs.descriptions, 
                          obs=round(obs.p,3),
                          mean=round(mean.v / obs.n,3),
                          sd=round(sqrt(diag(cov.mat)) / obs.n,3))
        print(summ)
        
        browser()
        
        round(cov2cor(cov.mat),2)
        sqrt(diag(cov.mat))/obs.n
    }

    if (any(is.na(obs.v)))
        stop("NAs produced in obs.v")
    if (any(is.na(mean.v)))
        stop("NAs produced in mean.v")
    if (any(is.na(cov.mat)))
        stop("NAs produced in cov.mat")
    
    dmvnorm(x = obs.v,
            mean = mean.v,
            sigma = cov.mat,
            log=log)
}

##-------------------------------------##
##-------------------------------------##
##-- CALCULATING OUTCOME DIFFERENCES --##
##-------------------------------------##
##-------------------------------------##

get.population.for.likelihood <- function(location,
                                          census.full.msm,
                                          idu.manager,
                                          years,
                                          age.cutoffs,
                                          races,
                                          sexes,
                                          collapse.idu)
{
    if (length(location)==1 && !is.na(msa.names(location)))
        location = counties.for.msa(location)
    else if (length(location)==1 && !is.na(state.abbreviation.to.fips(location)))
        location = counties.for.state(location)
    
    if (any(is.na(county.names(location))))
        stop("Location must be one or more county FIPS")
    
    #-- Pull Population --#
    
    years.to.pull = intersect(years, census.full.msm$years)
    if (length(years)==0)
        stop("None of the requested years are present in the census data")
    
    population = get.census.data.age.aggregated(census=census.full.msm,
                                                years=years.to.pull,
                                                fips=location,
                                                age.cutoffs=age.cutoffs,
                                                aggregate.years=F,
                                                aggregate.counties=T,
                                                aggregate.ages=F,
                                                aggregate.races=F,
                                                aggregate.sexes=F)
        
    #interpolate missing years
    dim.names = dimnames(population)
    if (length(years.to.pull)<length(years))
    {
        dim.names$year = as.character(years)
        
        population = apply(population, names(dim.names)[-1], function(x){
            interpolate.parameters(values=x, 
                                   value.times=years.to.pull,
                                   desired.times=years,
                                   return.list=F)
        })
        
        dim(population) = sapply(dim.names, length)
        dimnames(population) = dim.names
    }
    
    #-- Aggregate Races --#
    population = collapse.races(population, races=races)
    
    #-- IDU Prevalence --#

    idu.ever.prevalence = get.idu.prevalence(idu.manager,
                                             census = census.full.msm, 
                                             age.cutoffs = age.cutoffs,
                                             use.ever=T,
                                             counties=location, 
                                             aggregate.counties = T)
    
    if (collapse.idu)
    {
        old.pop = population
        dim.names = c(
            dimnames(old.pop),
            list(risk=c('never_IDU','IDU'))
        )
        
        p.idu = rep(as.numeric(idu.ever.prevalence), each=length(years))
        
        population = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
        population[,,,,'never_IDU'] = old.pop * (1-p.idu)
        population[,,,,'IDU'] = old.pop * p.idu
    }
    else
    {
        idu.30d.prevalence = get.idu.prevalence(idu.manager, 
                                                census = census.full.msm, 
                                                age.cutoffs = age.cutoffs,
                                                use.30d=T, 
                                                counties=location,
                                                aggregate.counties = T)
        
        population = stratify.population.idu(population,
                                             active.idu.prevalence=idu.30d.prevalence,
                                             idu.ever.prevalence=idu.ever.prevalence)
        population = apply(population, union(names(dim.names), names(dimnames(population))), function(x){x})
    }
    
    population = population[,,races,sexes,]
    
    #-- Return --#
    population
    
}

calculate.census.ratios <- function(census,
                                    years,
                                    locations1,
                                    locations2)
{
    if (length(locations1)== 1 && !is.null(msa.names(locations1)))
        locations1 = counties.for.msa(locations1)
    if (length(locations2)== 1 && !is.null(msa.names(locations2)))
        locations2 = counties.for.msa(locations2)
    
    pop1 = get.census.data(census, 
                           fips = locations1,
                           years=years,
                           aggregate.counties = T, 
                           aggregate.ages = F,
                           aggregate.races = F,
                           aggregate.sexes = F)
    pop1 = collapse.races(pop1)
    
    pop2 = get.census.data(census, 
                           fips = locations1,
                           years=years,
                           aggregate.counties = T, 
                           aggregate.ages = F,
                           aggregate.races = F,
                           aggregate.sexes = F)
    pop2 = collapse.races(pop2)
}

#returns an array indexed [year, stratum]
# values are: reverse.scale(scale(values1)-scale(values2))
calculate.outcome.differences <- function(data.type,
                                          
                                          years,
                                          locations1,
                                          surv1,
                                          locations2,
                                          surv2,
                                          
                                          super.locations,
                                          super.surv,
                                          diff.super.to.1.locations,
                                          diff.super.to.1.surv,
                                          
                                          smooth.scale=log,
                                          reverse.scale=exp,
                                          
                                          smooth=T,
                                          
                                          ages=c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years'),
                                          races=c('black','hispanic','other'),
                                          sexes=c('heterosexual_male','msm','female'),
                                          risks=c('never_IDU','active_IDU','IDU_in_remission'),
                                          
                                          collapse.idu.strata=T
)
{
    if (data.type == 'prevalence.all')
        data.type = 'prevalence'

    dimensions = list(age=ages,
                      race=races,
                      sex=sexes,
                      risk=risks)
    
    CDC.RISK = c('msm','idu', 'msm_idu', 'heterosexual')
    CDC.SEX = c('female','male')
    cdc.dimensions = list(age=ages,
                          race=races,
                          sex=CDC.SEX,
                          risk=CDC.RISK)
    
    #-- Line up which combinations of dimensions are in each "level" (num dimensions not marginalized) --#
    combinations.by.level.dimensions = lapply(0:length(dimensions), function(depth){
        combos = combn(names(dimensions), depth)
        
        n.combos = max(1, dim(combos)[2])
        
        rv = matrix(F, nrow=length(dimensions), ncol=n.combos, dimnames=list(dimension=names(dimensions), combo=NULL))
        if (depth > 0)
        {
            for (i in 1:n.combos)
            {
                for (dim in combos[,i])
                    rv[dim, i] = T
            }
        }   
        
        rv
    })
    
    #-- Pull the values and calculate a delta (and smooth if desired) --#
    combinations.by.level.values = lapply(0:length(dimensions), function(depth){
        cbl = combinations.by.level.dimensions[[depth+1]]
        lapply(1:dim(cbl)[2], function(i){

            dims = cbl[,i]
            
            values1 = get.surveillance.data(surv=surv1, location.codes=locations1, data.type=data.type, years=years,
                                            aggregate.locations = T, aggregate.years = F,
                                            age=dims['age'], race=dims['race'], sex=dims['sex'], risk=dims['risk'],
                                            throw.error.if.missing.data = F)
            values2 = get.surveillance.data(surv=surv2, location.codes=locations2, data.type=data.type, years=years,
                                            aggregate.locations = T, aggregate.years = F,
                                            age=dims['age'], race=dims['race'], sex=dims['sex'], risk=dims['risk'],
                                            throw.error.if.missing.data = F)
            
            if (!is.null(super.surv) && !is.null(diff.super.to.1.surv) &&
                (is.null(values1) || all(is.na(values1) | values1==0)))
            {
                values1 = get.surveillance.data(surv=super.surv, 
                                                location.codes=super.locations, 
                                                data.type=data.type, 
                                                years=years,
                                                aggregate.locations = T, aggregate.years = F,
                                                age=dims['age'], race=dims['race'], sex=dims['sex'], risk=dims['risk'],
                                                throw.error.if.missing.data = F) -
                    get.surveillance.data(surv=diff.super.to.1.surv, 
                                          location.codes=diff.super.to.1.locations, 
                                          data.type=data.type, 
                                          years=years,
                                          aggregate.locations = T, aggregate.years = F,
                                          age=dims['age'], race=dims['race'], sex=dims['sex'], risk=dims['risk'],
                                          throw.error.if.missing.data = F)
            }
            
            # if we're coming up empty so far, allow na.rm to aggregate across missing data
            if (!is.null(values1) && all(is.na(values1 | values1==0)))
            {
                disaggregated = values1 = get.surveillance.data(surv=surv1, location.codes=locations1, data.type=data.type, years=years,
                                                                aggregate.locations = F, aggregate.years = F,
                                                                age=dims['age'], race=dims['race'], sex=dims['sex'], risk=dims['risk'],
                                                                throw.error.if.missing.data = F)
                if (is.null(disaggregated))
                    values1 = NULL
                else
                {
                    values1 = apply(disaggregated, setdiff(names(dimnames(disaggregated)), 'location'), sum, na.rm=T)
                    all.na = apply(is.na(disaggregated), setdiff(names(dimnames(disaggregated)), 'location'), all)
                    values1[all.na] = NA
                }
#                values1 = get.surveillance.data(surv=surv1, location.codes=locations1, data.type=data.type, years=years,
#                                                aggregate.locations = T, aggregate.years = F,
#                                                age=dims['age'], race=dims['race'], sex=dims['sex'], risk=dims['risk'],
#                                                throw.error.if.missing.data = F, na.rm = T)
            }
            
            
            if (is.null(values1) || is.null(values2) || all(is.na(values1) | values1==0 | is.na(values2)))
            {
                if (depth==0)
                    browser()
#                    stop("No matching data for both locations available")
                NULL
            }
            else
            {
                delta = suppressWarnings(smooth.scale(values1) - smooth.scale(values2))
                
                if (is.null(dim(delta)) || length(dim(delta))==1)
                    dim(delta) = c(year=length(years), all=1)
                
                smoothed.delta = apply(delta, 2:length(dim(delta)), function(z){
                    if (smooth)
                    {
                        mask = !is.na(z) & !is.infinite(z)
                        if (sum(mask)==0)
                            rep(NaN, length(years))
                        else if (sum(mask)==1)
                            rep(z[!is.na(z) & !is.infinite(z)], length(years))
                        else
                        {
                            fit = lm(z[mask]~years[mask])
                            fit$coefficients[1] + fit$coefficients[2]*years
                        }
                    }
                    else
                        interpolate.parameters(z[!is.na(z)], years[!is.na(z)], 
                                               desired.times = years, return.list = F)
                })
                
                dim(smoothed.delta) = dim(delta)
                dimnames(smoothed.delta) = dimnames(delta)
                
                smoothed.delta
            }
        })
    })
    
    #-- Infer missing values from the previous level --#
    for (depth in (1+1:length(dimensions)))
    {
        for (i in 1:length(combinations.by.level.values[[depth]]))
        {
            values = combinations.by.level.values[[depth]][[i]]
            if (is.null(values) || any(is.na(values)))
            {
                #if the difference is missing,
                # we're going to infer it as the sum of all the differences from the
                # level up which are a subset of the dimensions here
                
                dims = combinations.by.level.dimensions[[depth]][,i]
                calculate.from.mask = apply(combinations.by.level.dimensions[[depth-1]], 2, function(dims.above){
                    all(!dims.above | dims)
                })
                
                dim.names = c(list(year=as.character(years)),
                              cdc.dimensions[dims])
                diffs = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
                
                for (diffs.above in combinations.by.level.values[[depth-1]][calculate.from.mask])
                {
                    diffs = diffs + expand.population(diffs.above, target.dim.names=dim.names)
                }
                diffs = diffs / sum(calculate.from.mask)
                
                if (is.null(values))
                    values = diffs
                else
                    values[is.na(values)] = diffs[is.na(values)]
                
                combinations.by.level.values[[depth]][[i]] = values
            }
        }
    }
    
    #-- Clean up and aggregate --#
    
    if (locations1[1]=='54037')
        browser()
    rv = combinations.by.level.values[[1+length(dimensions)]][[1]]
    rv = recategorize.to.jheem.risk.strata(rv, proportion.idu.in.remission = 1, proportion.idu.active=1)
    rv = reverse.scale(rv)
    
    if (collapse.idu.strata)
        rv = collapse.idu.strata(rv, join.as='colinear')
    
    #-- Return --#
    rv
}

collapse.idu.strata <- function(arr,
                                join.as=c('colinear','sum','or')[1])
{
    if (join.as=='colinear')
    {
        if (any(arr[,,,,'active_IDU'] != arr[,,,,'IDU_in_remission']))
            stop("Cannot collapse IDU strata - values for 'active_IDU' are not always the same as for 'IDU_in_remission")
    }
    
    if (join.as == 'sum')
        arr[,,,,'active_IDU'] = arr[,,,,'active_IDU'] + arr[,,,,'IDU_in_remission']
    else if (join.as == 'or')
        arr[,,,,'active_IDU'] = arr[,,,,'active_IDU'] | arr[,,,,'IDU_in_remission']
    
    not.idu.in.remission = setdiff(dimnames(arr)$risk, 'IDU_in_remission')
    arr = arr[,,,,not.idu.in.remission]
    
    dimnames(arr)$risk[dimnames(arr)$risk=='active_IDU'] = 'IDU'
    
    arr
}

infer.state.population.from.stratifications <- function(state,
                                                        data.type,
                                                        years,
                                                        state.surveillance,
                                                        desired.dimensions = c('year','age','race','sex','risk'),
                                                        correct.to.yearly.total=T,
                                                        collapse.idu.strata = T)
{
    if (data.type == 'prevalence.all')
    {
        divide.by = as.numeric(get.surveillance.data(state.surveillance, state, data.type='diagnosed',
                                                     years=years))
        data.type = 'prevalence'
    }
    else
        divide.by = 1
    
    # Try 4-d
    rv = get.surveillance.data(state.surveillance, state, data.type=data.type, years=years,
                               sex=T, age=T, race=T, risk=T) / divide.by
    
    access(rv, sex='female', risk=c('msm','msm_idu')) = 0
    rv = apply(rv, desired.dimensions, function(x){x})
    
    calculated = array(F, dim=dim(rv), dimnames=dimnames(rv))
    
    # Try 3-d
    if (any(is.na(rv)))
    {
        sar = get.surveillance.data(state.surveillance, state, data.type=data.type, years=years,
                                   sex=T, age=T, race=T, risk=F) / divide.by
        sak = get.surveillance.data(state.surveillance, state, data.type=data.type, years=years,
                                    sex=T, age=T, race=F, risk=T) / divide.by
        srk = get.surveillance.data(state.surveillance, state, data.type=data.type, years=years,
                                    sex=T, age=F, race=T, risk=T) / divide.by
        
        i1 = distribute.n.among.shared.dimensions(sar, sak, desired.dimensions = desired.dimensions)
        i2 = distribute.n.among.shared.dimensions(sak, sar, desired.dimensions = desired.dimensions)
        
        i3 = distribute.n.among.shared.dimensions(sar, srk, desired.dimensions = desired.dimensions)
        i4 = distribute.n.among.shared.dimensions(srk, sar, desired.dimensions = desired.dimensions)
        
        i5 = distribute.n.among.shared.dimensions(sak, srk, desired.dimensions = desired.dimensions)
        i6 = distribute.n.among.shared.dimensions(srk, sak, desired.dimensions = desired.dimensions)
        
        num.non.na = as.numeric(!is.na(i1)) + as.numeric(!is.na(i2)) +
            as.numeric(!is.na(i3)) + as.numeric(!is.na(i4)) +
            as.numeric(!is.na(i5)) + as.numeric(!is.na(i6))
        
        i1[is.na(i1)] = 0
        i2[is.na(i2)] = 0
        i3[is.na(i3)] = 0
        i4[is.na(i4)] = 0
        i5[is.na(i5)] = 0
        i6[is.na(i6)] = 0
        
        replacement = (i1 + i2 + i3 + i4 + i5 + i6) / num.non.na
        replacement[num.non.na==0] = NA
        
        calculated[is.na(rv) & !is.na(replacement)] = T
        rv[is.na(rv)] = replacement[is.na(rv)]
    }
    
    # Try 2-d
    if (any(is.na(rv)))
    {
        sa = get.surveillance.data(state.surveillance, state, data.type=data.type, years=years,
                                    sex=T, age=T, race=F, risk=F) / divide.by
        rk = get.surveillance.data(state.surveillance, state, data.type=data.type, years=years,
                                        sex=F, age=F, race=T, risk=T) / divide.by
        
        i1 = distribute.n.among.shared.dimensions(sa, rk, desired.dimensions = desired.dimensions)
        i2 = distribute.n.among.shared.dimensions(rk, sa, desired.dimensions = desired.dimensions)
        
        
        ar = get.surveillance.data(state.surveillance, state, data.type=data.type, years=years,
                                   sex=F, age=T, race=T, risk=F) / divide.by
        sk = get.surveillance.data(state.surveillance, state, data.type=data.type, years=years,
                                   sex=T, age=F, race=F, risk=T) / divide.by
        
        i3 = distribute.n.among.shared.dimensions(ar, sk, desired.dimensions = desired.dimensions)
        i4 = distribute.n.among.shared.dimensions(sk, ar, desired.dimensions = desired.dimensions)
        
        num.non.na = as.numeric(!is.na(i1)) + as.numeric(!is.na(i2)) +
            as.numeric(!is.na(i3)) + as.numeric(!is.na(i4))
        
        i1[is.na(i1)] = 0
        i2[is.na(i2)] = 0
        i3[is.na(i3)] = 0
        i4[is.na(i4)] = 0
        
        replacement = (i1 + i2 + i3 + i4) / num.non.na
        replacement[num.non.na==0] = NA
        
        calculated[is.na(rv) & !is.na(replacement)] = T
        rv[is.na(rv)] = replacement[is.na(rv)]
    }
    
    if (correct.to.yearly.total)
        rv = rv / rowSums(rv) * 
                as.numeric(get.surveillance.data(state.surveillance, state, data.type, years=years) / divide.by)
    
    rv = recategorize.to.jheem.risk.strata(rv)
    calculated = recategorize.to.jheem.risk.strata(calculated) > 0
    if (collapse.idu.strata)
    {
        rv = collapse.idu.strata(rv, join.as='sum')
        calculated = collapse.idu.strata(calculated, join.as='or')
    }
    
    attr(rv, 'calculated') = calculated
    
    rv
}

distribute.n.among.shared.dimensions <- function(arr1, arr2,
                                                 desired.dimensions = c('year','age','race','sex','risk'))
{
    # Check dimensions
    arr1.dimensions = names(dimnames(arr1))
    arr2.dimensions = names(dimnames(arr2))
    
    if (length(setdiff(arr1.dimensions, desired.dimensions))>0)
        stop(paste0("These dimension(s) are present in arr1, but are not to be included in the return value: ",
                    paste0(setdiff(arr1.dimensions, desired.dimensions), collapse=', ')))
    if (length(setdiff(arr2.dimensions, desired.dimensions))>0)
        stop(paste0("These dimension(s) are present in arr2, but are not to be included in the return value: ",
                    paste0(setdiff(arr2.dimensions, desired.dimensions), collapse=', ')))
    
    missing.dimensions = setdiff(union(arr1.dimensions, arr2.dimensions), desired.dimensions)
    if (length(missing.dimensions)>0)
        stop(paste0("These dimension(s) are not present in either arr1 or arr2: ",
                    paste0(missing.dimensions, collapse=', ')))

    
    # Set any female msm to zero
    if (any(arr1.dimensions=='sex') && any(arr1.dimensions=='risk'))
        access(arr1, sex='female', risk=c('msm','msm_idu')) = 0
    if (any(arr2.dimensions=='sex') && any(arr2.dimensions=='risk'))
        access(arr2, sex='female', risk=c('msm','msm_idu')) = 0
    
    # Set up partitions of dimensions
    shared.dimensions = intersect(arr1.dimensions, arr2.dimensions)
    dimensions.in.1.not.2 = setdiff(arr1.dimensions, shared.dimensions)
    dimensions.in.2.not.1 = setdiff(arr2.dimensions, shared.dimensions)
    
    n.in.1.not.2 = prod(dim(arr1)[dimensions.in.1.not.2])
    n.in.2.not.1 = prod(dim(arr2)[dimensions.in.2.not.1])
        
    # Set up rv
    numerator = apply(arr1, shared.dimensions, function(x){
        rep(as.numeric(x), n.in.2.not.1)
    }) 
    
    fraction = apply(arr2, shared.dimensions, function(x){
            rep(as.numeric(x)/sum(x), each=n.in.1.not.2)
        }) 
    
    rv = numerator*fraction
    
    # label the dimensions in the order we actually got them out of the apply statements
    dim.names = c(dimnames(arr1)[dimensions.in.1.not.2],
                  dimnames(arr2)[dimensions.in.2.not.1],
                  dimnames(arr1)[shared.dimensions])
    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names
    
    # order the dimensions as we want them
    rv = apply(rv, desired.dimensions, function(x){x})
    dim.names = dim.names[desired.dimensions]
    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names
    
    # return
    rv
}


##-----------------------------------------------##
##-----------------------------------------------##
##-- ESTIMATING ERRORS FOR OUTCOME DIFFERENCES --##
##-----------------------------------------------##
##-----------------------------------------------##


#-- The (Three) Forward-Facing Functions --#

get.in.msa.county.p.bias.and.variance <- function(data.type,
                                                  data.type.for.n,
                                                  msa.surveillance,
                                                  county.surveillance,
                                                  census,
                                                  fixed.bias=0,
                                                  weight.by.n=F)
{
    if (data.type.for.n=='prevalence.all')
    {
        data.type.for.n = 'prevalence'
        divide.by.p = T
    }
    else
        divide.by.p = F
    
    matched.data = get.matched.msa.data(data.type1=data.type,
                                        data.type2=data.type.for.n,
                                        years=NULL,
                                        msa.surveillance=msa.surveillance,
                                        other.surveillance1=county.surveillance,
                                        msa.to.other1.code.mapping.fn=counties.for.msa,
                                        eliminate.one.to.one.mappings=T,
                                        other.surveillance2=NULL,
                                        msa.to.other2.code.mapping.fn=NULL,
                                        census=census,
                                        by.total=T,
                                        by.age=T,
                                        by.race=T,
                                        by.sex=T,
                                        by.risk=T,
                                        require.msa.data = T)
    
    if (divide.by.p)
        matched.data$location1.value2 = matched.data$location1.value2 / matched.data$location1.value1
    
    if (weight.by.n)
        weights = matched.data$location1.value2
    else
        weights = rep(1, length(matched.data$msa.value1))
    
    mask = !is.na(matched.data$location1.value1) & !is.na(matched.data$msa.value1) &
        !is.infinite(matched.data$location1.value1) & !is.infinite(matched.data$msa.value1) & 
        matched.data$location1.value1 >= 0 & matched.data$location1.value1 <= 1 &
        matched.data$msa.value1 >= 0 & matched.data$msa.value1 <= 1 &
        !is.na(weights)
    
#    print(qplot(matched.data$msa.value1[mask],matched.data$location1.value1[mask],
#          color =matched.data$stratification[mask], size=matched.data$location1.value2[mask]) +
#        ylim(0,1) + xlim(0,1) +
#        xlab("MSA p") + ylab("County p") + geom_abline(intercept = 0, slope=1) +
#        ggtitle(data.type))
    
    
    do.get.bias.and.variance(matched.data$location1.value1[mask], 
                             matched.data$msa.value1[mask],
                             fixed.bias=fixed.bias,
                             weights=weights[mask])
}


get.extra.msa.county.p.bias.and.variance <- function(data.type,
                                                     data.type.for.n,
                                                     msa.surveillance,
                                                     county.surveillance,
                                                     state.surveillance,
                                                     census,
                                                     fixed.bias=NA,
                                                     weight.by.n=F,
                                                     use.county.p.if.missing.msa=T)
{
    if (data.type.for.n=='prevalence.all')
    {
        data.type.for.n = 'prevalence'
        divide.by.p = T
    }
    else
        divide.by.p = F
    
    matched.data = get.matched.msa.data(data.type1=data.type,
                                     data.type2=data.type.for.n,
                                     years=NULL,
                                     msa.surveillance=msa.surveillance,
                                     other.surveillance1=state.surveillance,
                                     msa.to.other1.code.mapping.fn=states.for.msa,
                                     eliminate.one.to.one.mappings=F,
                                     other.surveillance2=county.surveillance,
                                     msa.to.other2.code.mapping.fn=get.in.state.in.msa.counties,
                                     census=census,
                                     by.total=T,
                                     by.age=T,
                                     by.race=T,
                                     by.sex=T,
                                     by.risk=T,
                                     require.msa.data = !use.county.p.if.missing.msa)


    p.msa = matched.data$msa.value1
    p.msa[is.na(p.msa)] = matched.data$location2.value1[is.na(p.msa)]
    p.state = matched.data$location1.value1
    n.msa = matched.data$msa.value2
    one.state.msa = sapply(matched.data$msa, is.one.state.msa)
    n.msa[!one.state.msa] = matched.data$location2.value2[!one.state.msa]
    n.state = matched.data$location1.value2
    
    if (divide.by.p)
    {
        np.extra.msa = n.state + n.msa
        
        n.msa = n.msa / p.msa
        n.state = n.state / p.state
    }
    else
        np.extra.msa = n.state * p.state - n.msa * p.msa
    
    n.extra.msa = n.state - n.msa
    p.extra.msa = np.extra.msa / n.extra.msa
    
    if (weight.by.n)
        weights = n.extra.msa
    else
        weights = rep(1, length(p.extra.msa))
    
    mask = !is.na(p.extra.msa) & !is.na(p.msa) & !is.infinite(p.extra.msa) & !is.infinite(p.msa) & 
        p.state >= 0 & p.state <=1 &
        p.extra.msa >= 0 & p.extra.msa <= 1 & 
        p.msa >= 0 & p.msa <= 1 &
        !is.na(weights)
  
    do.get.bias.and.variance(p.extra.msa[mask], 
                             p.msa[mask], 
                             fixed.bias=fixed.bias,
                             weights=weights[mask])
}

# we don't actually use this, but keeping it here for debugging/checking things out
get.state.p.bias.and.variance <- function(data.type,
                                          data.type.for.n,
                                          msa.surveillance,
                                          state.surveillance,
                                          census,
                                          fixed.bias=NA,
                                          weight.by.n=F)
{
    if (data.type.for.n=='prevalence.all')
    {
        data.type.for.n = 'prevalence'
        divide.by.p = T
    }
    else
        divide.by.p = F
    
    matched.data = get.matched.msa.data(data.type1=data.type,
                                        data.type2=data.type.for.n,
                                        years=NULL,
                                        msa.surveillance=msa.surveillance,
                                        other.surveillance1=state.surveillance,
                                        msa.to.other1.code.mapping.fn=states.for.msa,
                                        eliminate.one.to.one.mappings=F,
                                        other.surveillance2=NULL,
                                        msa.to.other2.code.mapping.fn=NULL,
                                        census=census,
                                        by.total=T,
                                        by.age=T,
                                        by.race=T,
                                        by.sex=T,
                                        by.risk=T,
                                        require.msa.data = T)
    
    
    if (divide.by.p)
        matched.data$location1.value2 = matched.data$location1.value2 / matched.data$location1.value1
    
    if (weight.by.n)
        weights = matched.data$location1.value2
    else
        weights = rep(1, length(matched.data$msa.value1))
    
    mask = !is.na(matched.data$location1.value1) & !is.na(matched.data$msa.value1) &
        !is.infinite(matched.data$location1.value1) & !is.infinite(matched.data$msa.value1) & 
        matched.data$location1.value1 >= 0 & matched.data$location1.value1 <= 1 &
        matched.data$msa.value1 >= 0 & matched.data$msa.value1 <= 1 &
        !is.na(weights)
    
#    print(qplot(matched.data$msa.value1[mask],matched.data$location1.value1[mask],
#                color =matched.data$stratification[mask], size=matched.data$location1.value2[mask]) +
#              ylim(0,1) + xlim(0,1) +
#              xlab("MSA p") + ylab("County p") + geom_abline(intercept = 0, slope=1) +
#              ggtitle(data.type))
    
 
    do.get.bias.and.variance(matched.data$location1.value1[mask], 
                             matched.data$msa.value1[mask],
                             fixed.bias=fixed.bias,
                             weights=weights[mask])
}


#-- Internal Helpers --#


get.in.state.out.of.msa.counties <- function(msa, state)
{
    setdiff(counties.for.state(state),
            counties.for.msa(msa))
}

get.in.state.in.msa.counties <- function(msa, state)
{
    intersect(counties.for.state(state),
            counties.for.msa(msa))
}

is.one.county.msa <- function(msas)
{
    sapply(msas, function(msa){
        length(counties.for.msa(msa))==1
    })
}

is.one.state.msa <- function(msas)
{
    sapply(msas, function(msa){
        length(states.for.msa(msa))==1
    })
}

# the bias = values1 - values2
# ie, values1 = values2 + bias
do.get.bias.and.variance <- function(values1, values2, fixed.bias,
                                     weights = rep(1, length(values1)))
{
    rv = list()
    if (is.na(fixed.bias))
        rv$bias = sum(weights * (values1-values2)) / sum(weights)
    else
        rv$bias = fixed.bias
    
    errors = values1 - (values2 + rv$bias)
    rv$variance = sum(weights * errors^2)/sum(weights) * length(errors) / (length(errors)-1)
    rv$sd = sqrt(rv$variance)
    rv$n = length(values1)
    
    rv
}

get.matched.msa.data <- function(data.type1,
                                 data.type2=NULL,
                                 years=NULL,
                                 msa.surveillance,
                                 other.surveillance1,
                                 msa.to.other1.code.mapping.fn,
                                 eliminate.one.to.one.mappings,
                                 other.surveillance2=NULL,
                                 msa.to.other2.code.mapping.fn=NULL,
                                 census,
                                 by.total=T,
                                 by.age=T,
                                 by.race=T,
                                 by.sex=T,
                                 by.risk=T,
                                 require.msa.data=T)
{
    rv = list(
        msa = character(),
        msa.value1 = numeric(),
        msa.value2 = numeric(),
        location1 = character(),
        location1.value1 = numeric(),
        location1.value2 = numeric(),
        location2 = character(),
        location2.value1 = numeric(),
        location2.value2 = numeric(),
        stratification = character()
    )
    
    if (by.total)
    {
        sub.rv = get.matched.msa.data.one.stratification(data.type1=data.type1,
                                                         data.type2=data.type2,
                                                         years=years,
                                                         msa.surveillance=msa.surveillance,
                                                         other.surveillance1=other.surveillance1,
                                                         msa.to.other1.code.mapping.fn=msa.to.other1.code.mapping.fn,
                                                         eliminate.one.to.one.mappings=eliminate.one.to.one.mappings,
                                                         other.surveillance2=other.surveillance2,
                                                         msa.to.other2.code.mapping.fn=msa.to.other2.code.mapping.fn,
                                                         census=census,
                                                         age=F,
                                                         race=F,
                                                         sex=F,
                                                         risk=F,
                                                         require.msa.data=require.msa.data)
        
        rv = c.list.elements(rv, sub.rv)
    }
    
    
    if (by.age)
    {
        sub.rv = get.matched.msa.data.one.stratification(data.type1=data.type1,
                                                        data.type2=data.type2,
                                                        years=years,
                                                        msa.surveillance=msa.surveillance,
                                                        other.surveillance1=other.surveillance1,
                                                        msa.to.other1.code.mapping.fn=msa.to.other1.code.mapping.fn,
                                                        eliminate.one.to.one.mappings=eliminate.one.to.one.mappings,
                                                        other.surveillance2=other.surveillance2,
                                                        msa.to.other2.code.mapping.fn=msa.to.other2.code.mapping.fn,
                                                        census=census,
                                                        age=T,
                                                        race=F,
                                                        sex=F,
                                                        risk=F,
                                                        require.msa.data=require.msa.data)
        
        rv = c.list.elements(rv, sub.rv)
    }
    
    
    if (by.race)
    {
        sub.rv = get.matched.msa.data.one.stratification(data.type1=data.type1,
                                                        data.type2=data.type2,
                                                        years=years,
                                                        msa.surveillance=msa.surveillance,
                                                        other.surveillance1=other.surveillance1,
                                                        msa.to.other1.code.mapping.fn=msa.to.other1.code.mapping.fn,
                                                        eliminate.one.to.one.mappings=eliminate.one.to.one.mappings,
                                                        other.surveillance2=other.surveillance2,
                                                        msa.to.other2.code.mapping.fn=msa.to.other2.code.mapping.fn,
                                                        census=census,
                                                        age=F,
                                                        race=T,
                                                        sex=F,
                                                        risk=F,
                                                        require.msa.data=require.msa.data)
        
        rv = c.list.elements(rv, sub.rv)
    }
    
    
    if (by.sex)
    {
        sub.rv = get.matched.msa.data.one.stratification(data.type1=data.type1,
                                                        data.type2=data.type2,
                                                        years=years,
                                                        msa.surveillance=msa.surveillance,
                                                        other.surveillance1=other.surveillance1,
                                                        msa.to.other1.code.mapping.fn=msa.to.other1.code.mapping.fn,
                                                        eliminate.one.to.one.mappings=eliminate.one.to.one.mappings,
                                                        other.surveillance2=other.surveillance2,
                                                        msa.to.other2.code.mapping.fn=msa.to.other2.code.mapping.fn,
                                                        census=census,
                                                        age=F,
                                                        race=F,
                                                        sex=T,
                                                        risk=F,
                                                        require.msa.data=require.msa.data)
        
        rv = c.list.elements(rv, sub.rv)
    }
    
    
    if (by.risk)
    {
        sub.rv = get.matched.msa.data.one.stratification(data.type1=data.type1,
                                                        data.type2=data.type2,
                                                        years=years,
                                                        msa.surveillance=msa.surveillance,
                                                        other.surveillance1=other.surveillance1,
                                                        msa.to.other1.code.mapping.fn=msa.to.other1.code.mapping.fn,
                                                        eliminate.one.to.one.mappings=eliminate.one.to.one.mappings,
                                                        other.surveillance2=other.surveillance2,
                                                        msa.to.other2.code.mapping.fn=msa.to.other2.code.mapping.fn,
                                                        census=census,
                                                        age=F,
                                                        race=F,
                                                        sex=F,
                                                        risk=T,
                                                        require.msa.data=require.msa.data)
        
        rv = c.list.elements(rv, sub.rv)
    }
    
 
    # Return
    rv
}

get.matched.msa.data.one.stratification <- function(data.type1,
                                                    data.type2,
                                                    years=NULL,
                                                    msa.surveillance,
                                                    other.surveillance1,
                                                    msa.to.other1.code.mapping.fn,
                                                    eliminate.one.to.one.mappings,
                                                    other.surveillance2,
                                                    msa.to.other2.code.mapping.fn,
                                                    census,
                                                    age=F,
                                                    race=F,
                                                    sex=F,
                                                    risk=F,
                                                    require.msa.data=F)
{
    if (is.null(years))
    {
        if (require.msa.data || is.null(other.surveillance2))
            years = msa.surveillance$DIMENSION.VALUES$year
        else
            years = union(msa.surveillance$DIMENSION.VALUES$year,
                          other.surveillance2$DIMENSION.VALUES$year)
        
        years = sort(intersect(years, other.surveillance1$DIMENSION.VALUES$year))
    }
    
    if (require.msa.data)
        msas = get.locations.for.surveillance.data(msa.surveillance, data.type=data.type1,
                                                   sex=sex, age=age, race=race, risk=risk)
    else
        msas = msa.surveillance$DIMENSION.VALUES$location

    data1.for.msas = lapply(msas, get.surveillance.data,
                            surv=msa.surveillance,
                            data.type=data.type1,
                            age=age, race=race, sex=sex, risk=risk,
                            years=years,
                            throw.error.if.missing.data=F)
    
    if (require.msa.data)
    {
        missing.all.mask = sapply(data1.for.msas, function(data){
            is.null(data) || all(is.na(data))
        })
        
        msas = msas[!missing.all.mask]
        data1.for.msas = data1.for.msas[!missing.all.mask]
    }
    
    rv = list(
        msa = character(),
        msa.value1 = numeric(),
        msa.value2 = numeric(),
        location1 = character(),
        location1.value1 = numeric(),
        location1.value2 = numeric(),
        location2 = character(),
        location2.value1 = numeric(),
        location2.value2 = numeric()
    )
    
    if (length(msas)>0)
    {
        for (i in 1:length(msas))
        {
            msa = msas[[i]]
            msa.data1 = data1.for.msas[[i]]    
            if (!is.null(data.type2))
            {
                if (data.type2=='population')
                {
                    if (risk)
                        msa.data2 = NULL
                    else
                    {
                        years.to.pull = intersect(years, census$years)
                        if (length(years.to.pull)==0)
                            msa.data2 = NULL
                        else
                        {
                            msa.data2 = pad.data.for.years(
                                data=get.census.data(census,
                                                     fips=msa,
                                                     years=years.to.pull,
                                                     ages=msa.surveillance$DIMENSION.VALUES$age,
                                                     races=msa.surveillance$DIMENSION.VALUES$race,
                                                     sexes=msa.surveillance$DIMENSION.VALUES$sex,
                                                     aggregate.years=F,
                                                     aggregate.counties=T,
                                                     aggregate.ages=!age,
                                                     aggregate.races=!race,
                                                     aggregate.sexes=!sex,
                                                     collapse.races=T,
                                                     throw.error.if.location.missing = F),
                                years=years)
                        }
                    }
                }
                else
                    msa.data2 = get.surveillance.data(msa,
                                                      surv=msa.surveillance,
                                                      data.type=data.type2,
                                                      years=years,
                                                      age=age, race=race, sex=sex, risk=risk,
                                                      throw.error.if.missing.data=F)
            }
            else
                msa.data2 = NULL
            
            
            other.location.codes = msa.to.other1.code.mapping.fn(msa)
            
            if (length(other.location.codes)>1 || !eliminate.one.to.one.mappings)
            {
                for (other.code1 in other.location.codes)
                {
                    other1.data1 = get.surveillance.data(other.surveillance1, 
                                                       location.codes = other.code1,
                                                       data.type=data.type1,
                                                       years=years,
                                                       age=age, race=race, sex=sex, risk=risk,
                                                       throw.error.if.missing.data = F)
                    
                    if (!is.null(other1.data1) && any(!is.na(other1.data1)))
                    {
                        if (!is.null(data.type2))
                        {
                            if (data.type2=='population')
                            {
                                if (risk)
                                    other1.data2 = NULL
                                else
                                {
                                    years.to.pull = intersect(years, census$years)
                                    if (length(years.to.pull)==0)
                                        other1.data2 = NULL
                                    else
                                    {
                                        other1.data2 = pad.data.for.years(
                                            data=get.census.data(census,
                                                                 fips=other.code1,
                                                                 years=years.to.pull,
                                                                 ages=msa.surveillance$DIMENSION.VALUES$age,
                                                                 races=msa.surveillance$DIMENSION.VALUES$race,
                                                                 sexes=msa.surveillance$DIMENSION.VALUES$sex,
                                                                 aggregate.years=F,
                                                                 aggregate.counties=T,
                                                                 aggregate.ages=!age,
                                                                 aggregate.races=!race,
                                                                 aggregate.sexes=!sex,
                                                                 collapse.races=T,
                                                                 throw.error.if.location.missing = F),
                                            years=years)
                                    }
                                }
                            }
                            else
                                other1.data2 = get.surveillance.data(other.surveillance1, 
                                                                    location.codes = other.code1,
                                                                    data.type=data.type2,
                                                                    years=years,
                                                                    age=age, race=race, sex=sex, risk=risk,
                                                                    throw.error.if.missing.data = F)
                        }
                        else
                            other1.data2 = NULL
                        
                        other2.data1 = NULL
                        other2.data2 = NULL
                        if (!is.null(msa.to.other2.code.mapping.fn))
                        {
                            other.code2 = msa.to.other2.code.mapping.fn(msa, other.code1)
                            if (length(other.code2)>0)
                            {
                                
                                other2.data1 = get.surveillance.data(other.surveillance2, 
                                                                     location.codes = other.code2,
                                                                     data.type=data.type1,
                                                                     years=years,
                                                                     age=age, race=race, sex=sex, risk=risk,
                                                                     throw.error.if.missing.data = F,
                                                                     aggregate.by='weight',
                                                                     aggregate.weight.data.type = data.type2,
                                                                     aggregate.locations=T,
                                                                     na.rm=T)
                                
                                if (!is.null(data.type2))
                                {
                                    if (data.type2=='population')
                                    {
                                        if (risk)
                                            other2.data2 = NULL
                                        else
                                        {
                                            years.to.pull = intersect(years, census$years)
                                            if (length(years.to.pull)==0)
                                                other2.data2 = NULL
                                            else
                                            {
                                                other2.data2 = pad.data.for.years(
                                                    data=get.census.data(census,
                                                                         fips=other.code2,
                                                                         years=years.to.pull,
                                                                         ages=msa.surveillance$DIMENSION.VALUES$age,
                                                                         races=msa.surveillance$DIMENSION.VALUES$race,
                                                                         sexes=msa.surveillance$DIMENSION.VALUES$sex,
                                                                         aggregate.years=F,
                                                                         aggregate.counties=T,
                                                                         aggregate.ages=!age,
                                                                         aggregate.races=!race,
                                                                         aggregate.sexes=!sex,
                                                                         collapse.races=T,
                                                                         throw.error.if.location.missing = F),
                                                    years=years)
                                            }
                                        }
                                    }
                                    else
                                        other2.data2 = get.surveillance.data(other.surveillance2, 
                                                                             location.codes = other.code2,
                                                                             data.type=data.type2,
                                                                             years=years,
                                                                             age=age, race=race, sex=sex, risk=risk,
                                                                             throw.error.if.missing.data = F,
                                                                             aggregate.locations = T)
                                }
                                else
                                    other2.data2 = NULL
                            }
                        }
                        else 
                            other.code2 = NA
                        
                        if (is.null(msa.data1) || length(msa.data1)==0)
                        {
                            if (is.null(other1.data1))
                                n.data = length(other2.data1)
                            else
                                n.data = length(other1.data1)
                        }
                        else
                            n.data = length(msa.data1)
                        
                        
                        if (n.data > 0)
                        {
                            if (is.null(msa.data1) || length(msa.data1)==0)
                                msa.data1 = rep(NA, n.data)
                            if (is.null(msa.data2))
                                msa.data2 = rep(NA, n.data)
                            if (is.null(other1.data1))
                                other1.data1 = rep(NA, n.data)
                            if (is.null(other1.data2))
                                other1.data2 = rep(NA, n.data)
                            if (is.null(other2.data1))
                                other2.data1 = rep(NA, n.data)
                            if (is.null(other2.data2))
                                other2.data2 = rep(NA, n.data)
                            
                            if (require.msa.data)
                                both.not.missing = as.logical(!is.na(msa.data1) & !is.na(other1.data1))
                            else
                                both.not.missing = as.logical( (!is.na(msa.data1) | !is.na(other2.data1)) &
                                                                   !is.na(other1.data1))
                            
                            rv$msa = c(rv$msa,
                                       rep(msa, sum(both.not.missing)))
                            rv$msa.value1 = c(rv$msa.value1,
                                              as.numeric(msa.data1)[both.not.missing])
                            rv$msa.value2 = c(rv$msa.value2,
                                              as.numeric(msa.data2)[both.not.missing])
                            
                            rv$location1 = c(rv$location1,
                                             rep(other.code1, sum(both.not.missing)))
                            rv$location1.value1 = c(rv$location1.value1,
                                                    as.numeric(other1.data1)[both.not.missing])
                            rv$location1.value2 = c(rv$location1.value2,
                                                    as.numeric(other1.data2)[both.not.missing])
                            
                            rv$location2 = c(rv$location2,
                                             rep(paste0(other.code2, collapse=';'), sum(both.not.missing)))
                            rv$location2.value1 = c(rv$location2.value1,
                                                    as.numeric(other2.data1)[both.not.missing])
                            rv$location2.value2 = c(rv$location2.value2,
                                                    as.numeric(other2.data2)[both.not.missing])
                            
                            lens = sapply(rv, length)
                            if (length(unique(lens))>1)
                                stop('mismatched lengths in data - this means an error here')
                        }
                    }
                }
            }
        }
    }
    
    strat = c('age','race','sex','risk')[c(age,race,sex,risk)]
    if (length(strat)==0)
        strat = 'all'
    else
        strat = paste0(strat, collapse='.')
    
    rv$stratification = rep(strat, length(rv$msa))
    
    rv
}

c.list.elements <- function(l1, l2)
{
    for (elem.name in names(l1))
    {
        l1[[elem.name]] = c(l1[[elem.name]], l2[[elem.name]])
    }
    
    l1
}

pad.data.for.years <- function(data, years)
{
    if (is.null(data))
        return (NULL)
    
    if (is.null(dim(data)))
    {
        dim.names = list(year=names(data))
        dim(data) = sapply(dim.names, length)
        dimnames(data) = dim.names
    }
    
    if (names(dimnames(data))[1] != 'year')
        stop("'year' must be the first dimension of data")
    present.years = dimnames(data)$year
    if (setequal(present.years, as.character(years)))
        return (data)
    
    dim.names = dimnames(data)
    dim.names$year = as.character(years)
    
    if (length(dim(data))==1)
    {
        rv = rep(NaN, length(years))
        names(rv) = as.character(years)
        rv[present.years] = data
    }
    else
    {
        rv = apply(data, setdiff(names(dimnames(data)), 'year'), function(x){
            z = rep(NaN, length(years))
            names(z) = as.character(years)
            z[present.years] = x
            z
        })
    }
    
    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names
    
    rv
}


# for seeing the relationship between in-msa and out-of-msa variance
if (1==2)
{
    data.types = setdiff(ALLOWED.DATA.TYPES, c('testing','retention'))
    data.types.for.n = rep('prevalence', length(data.types))
    data.types.for.n[data.types=='linkage'] = 'new'
    
    in.msa = lapply(1:length(data.types), function(i){
        get.in.msa.county.p.bias.and.variance(data.type = data.types[i],
                                              data.type.for.n = data.types.for.n[i],
                                              msa.surveillance = msa.surveillance,
                                              county.surveillance = county.surveillance,
                                              census=ALL.DATA.MANAGERS$census.collapsed,
                                              fixed.bias = 0)
    })
    
    extra.msa = lapply(1:length(data.types), function(i){
        get.extra.msa.county.p.bias.and.variance(data.type = data.types[i],
                                                 data.type.for.n = data.types.for.n[i],
                                                 msa.surveillance = msa.surveillance,
                                                 county.surveillance = county.surveillance,
                                                 state.surveillance = state.surveillance,
                                                 census=ALL.DATA.MANAGERS$census.collapsed)
    })
    
    sds = sapply(1:length(data.types), function(i){
        c(in.msa=in.msa[[i]]$sd,
          extra.msa=extra.msa[[i]]$sd)
    }); dimnames(sds)[[2]] = data.types
    
    sds[1,] / sds[2,]
    sds[1,]^2 / sds[2,]^2
    
    
    get.extra.msa.county.p.bias.and.variance(data.type = 'testing',
                                             data.type.for.n = 'population',
                                             msa.surveillance = msa.surveillance,
                                             county.surveillance = county.surveillance,
                                             state.surveillance = state.surveillance,
                                             census=ALL.DATA.MANAGERS$census.collapsed)
}