# FIND P-BIAS

get.p.bias.estimates = function(data.manager, stratifications, outcome.for.p, outcome.for.n, sub.geographic.type, super.geographic.type, whatever.were.comparing.to = 'msa', minimum.sample.size = 12)#not always msa
{
    error.prefix = "Error getting p bias estimates: "
    
    # 1. Find locations with data in the data manager
    locations.with.p.data = data.manager$get.locations.with.data(outcome = outcome.for.p)
    locations.with.p.and.n.data = intersect(locations.with.p.data, data.manager$get.locations.with.data(outcome = outcome.for.n))
    
    # 2. Determine which are MSAs
    location.types.p = locations::get.location.type(locations.with.p.data)
    location.types.p.and.n = locations::get.location.type(locations.with.p.and.n.data)
    msas.p = names(location.types.p[location.types.p == 'CBSA'])
    msas.p.and.n = names(location.types.p.and.n[location.types.p.and.n == 'CBSA'])
    
    # 3. Make a list of MSAs and their sub-locations (p) and super locations (n and p) that have data; scrap MSAs with nothing
    msa.subs.p = lapply(msas.p, function(msa) {
        # browser()
        intersect(locations::get.sub.locations(msa, sub.type = sub.geographic.type, limit.to.completely.enclosing = T), locations.with.p.data)
    })
    msa.supers.p.and.n = lapply(msas.p.and.n, function(msa) {
        intersect(locations::get.super.locations(msa, super.type = super.geographic.type, limit.to.completely.enclosing = T), locations.with.p.and.n.data)
    })
    names(msa.subs.p) = msas.p
    names(msa.supers.p.and.n) = msas.p.and.n
    msa.subs.p = msa.subs.p[lengths(msa.subs.p) > 0]
    msa.supers.p.and.n = msa.supers.p.and.n[lengths(msa.supers.p.and.n) > 0]
    all.relevant.subs.p = unique(unlist(msa.subs.p))
    all.relevant.supers.p.and.n = unique(unlist(msa.supers.p.and.n))
    
    # 4. Generate an inside-msa p-bias sample by looping across each stratification
    p.bias.in.msa = unlist(lapply(stratifications, function(stratification)
        {
        # browser()
        # 4a. Pull data using the common ontology
        stratification.data = data.manager$pull(outcome = outcome.for.p,
                                                keep.dimensions = c('year', 'location', stratification),
                                                dimension.values = list(location = c(names(msa.subs.p), all.relevant.subs.p)))
        
        # 4b. Determine which MSAs and their sub-locations are present in this stratifications' data
        msas.with.data.this.stratification.mask = names(msa.subs.p) %in% dimnames(stratification.data)$location
        msas.with.data.this.stratification = names(msa.subs.p)[msas.with.data.this.stratification.mask]
        msa.subs.this.stratification = lapply(msa.subs.p[msas.with.data.this.stratification.mask], function(msa.subs) {
            msa.subs[msa.subs %in% dimnames(stratification.data)[['location']]]
        })
        names(msa.subs.this.stratification) = msas.with.data.this.stratification
        msa.subs.this.stratification = msa.subs.this.stratification[lengths(msa.subs.this.stratification) > 0]
        if (length(msa.subs.this.stratification) == 0) return(NULL)
        # browser()
        # 4c. Get a p-bias vector for each MSA that has data in this stratification, and return the collection
        p.bias.vector = unlist(lapply(1:length(msa.subs.this.stratification), function(i) {
            
            # 4c.1 Get a slice of the data array for the MSA and another for its sub-locations.
            msa.slice = stratification.data[get.array.access.indices(dimnames(stratification.data), dimension.values = list(location = names(msa.subs.this.stratification)[[i]]))]
            subs.slice = stratification.data[get.array.access.indices(dimnames(stratification.data), dimension.values = list(location = msa.subs.this.stratification[[i]]))]
            msa.slice.dimnames = dimnames(stratification.data)[names(dimnames(stratification.data)) != 'location']
            subs.slice.dimnames = c(msa.slice.dimnames, list(location = msa.subs.this.stratification[[i]]))
            msa.slice = array(msa.slice, dim = sapply(msa.slice.dimnames, length), dimnames = msa.slice.dimnames)
            # subs.slice = array(subs.slice, dim = sapply(subs.slice.dimnames, length), dimnames = subs.slice.dimnames) # technically unnecessary
            
            # 4c.2 Expand the MSA slice to be the same size as the subs slice
            expanded.msa.array = expand.array(msa.slice, target.dim.names = subs.slice.dimnames)
            
            # 4c.3 Take the difference at each position between sub-location and MSA and return as a vector -- this forms part of our in-msa p-bias sample
            p.bias.vector.for.this.msa = as.vector(subs.slice - expanded.msa.array)
            p.bias.vector.for.this.msa[!is.na(p.bias.vector.for.this.msa)]
            
        }))
        
    }))
    # browser()
    
    # 5. Generate an outside-msa p-bias sample by looping across each stratification
    p.bias.out.msa = unlist(lapply(stratifications, function(stratification)
        {
        # browser()
        # 5a. Pull *p* and *n* data using the common ontology. Note that these may have different length dimensions (i.e. number of years) from each other
        stratification.p.data = data.manager$pull(outcome = outcome.for.p,
                                                keep.dimensions = c('year', 'location', stratification),
                                                dimension.values = list(location = c(names(msa.supers.p.and.n), all.relevant.supers.p.and.n)))
        stratification.n.data = data.manager$pull(outcome = outcome.for.n,
                                                  keep.dimensions = c('year', 'location', stratification),
                                                  dimension.values = list(location = c(names(msa.supers.p.and.n), all.relevant.supers.p.and.n)))
        years.in.common = intersect(dimnames(stratification.p.data)$year, dimnames(stratification.n.data)$year)
        
        # 5b. Determine which MSAs and their super-locations are present in this stratification's p and n data both
        msas.with.data.this.stratification.mask = names(msa.supers.p.and.n) %in% intersect(dimnames(stratification.p.data)$location, dimnames(stratification.n.data)$location)
        msas.with.data.this.stratification = names(msa.supers.p.and.n)[msas.with.data.this.stratification.mask]
        msa.supers.this.stratification = lapply(msa.supers.p.and.n[msas.with.data.this.stratification.mask], function(msa.supers) {
            msa.supers[msa.supers %in% intersect(dimnames(stratification.p.data)$location, dimnames(stratification.n.data)$location)]
        })
        names(msa.supers.this.stratification) = msas.with.data.this.stratification
        msa.supers.this.stratification = msa.supers.this.stratification[lengths(msa.supers.this.stratification) > 0]
        if (length(msa.supers.this.stratification) == 0) return(NULL)
        
        # 5c. Get a p-bias vector for each MSA that has data in this stratification, and return the collection
        p.bias.vector = unlist(lapply(1:length(msa.supers.this.stratification), function(i) {
            
            # 5c.1 Get a slices of the n and p data arrays for the MSA and others for its super-locations. Make sure they have the same *years* (TO GENERALIZE, MAKE THIS ANY INCOMPLETE DIMENSIONS IN THE ONTOLOGY).
            
            msa.p.slice = stratification.p.data[get.array.access.indices(dimnames(stratification.p.data), dimension.values = list(location = names(msa.supers.this.stratification)[[i]], year = years.in.common))]
            supers.p.slice = stratification.p.data[get.array.access.indices(dimnames(stratification.p.data), dimension.values = list(location = msa.supers.this.stratification[[i]], year = years.in.common))]
            msa.n.slice = stratification.n.data[get.array.access.indices(dimnames(stratification.n.data), dimension.values = list(location = names(msa.supers.this.stratification)[[i]], year = years.in.common))]
            supers.n.slice = stratification.n.data[get.array.access.indices(dimnames(stratification.n.data), dimension.values = list(location = msa.supers.this.stratification[[i]], year = years.in.common))]
            
            msa.p.slice.dimnames = dimnames(stratification.p.data)[names(dimnames(stratification.p.data)) != 'location']
            msa.p.slice.dimnames[['year']] = years.in.common
            supers.p.slice.dimnames = c(msa.p.slice.dimnames, list(location = msa.supers.this.stratification[[i]]))
            msa.n.slice.dimnames = dimnames(stratification.n.data)[names(dimnames(stratification.n.data)) != 'location']
            msa.n.slice.dimnames[['year']] = years.in.common
            supers.n.slice.dimnames = c(msa.n.slice.dimnames, list(location = msa.supers.this.stratification[[i]]))
            
            msa.p.slice = array(msa.p.slice, dim = sapply(msa.p.slice.dimnames, length), dimnames = msa.p.slice.dimnames)
            # supers.p.slice = array(supers.p.slice, dim = sapply(supers.p.slice.dimnames, length), dimnames = supers.p.slice.dimnames) # technically unnecessary
            msa.n.slice = array(msa.n.slice, dim = sapply(msa.n.slice.dimnames, length), dimnames = msa.n.slice.dimnames)
            # supers.n.slice = array(supers.n.slice, dim = sapply(supers.n.slice.dimnames, length), dimnames = supers.n.slice.dimnames) # technically unnecessary
            
            # 5c.2 Expand the MSA slices to be the same size as the supers slices
            expanded.msa.p.array = expand.array(msa.p.slice, target.dim.names = supers.p.slice.dimnames)
            expanded.msa.n.array = expand.array(msa.n.slice, target.dim.names = supers.n.slice.dimnames)
            # browser()
            # 5c.3 Perform a calculation at each position between super-location and MSA and return as a vector -- this forms part of our out-of-msa p-bias sample
            p.bias.vector.for.this.msa = as.vector((supers.p.slice * supers.n.slice - msa.p.slice * msa.n.slice) / (supers.n.slice - msa.n.slice) - msa.p.slice) # can get divide by 0, making Inf
            p.bias.vector.for.this.msa[!is.na(p.bias.vector.for.this.msa) & !is.infinite(p.bias.vector.for.this.msa)]
            
        }))
        
    }))
    
    # Check sample size. 12? "Minumum sample size" defaults to 12
    if (length(p.bias.in.msa) < minimum.sample.size || length(p.bias.out.msa) < minimum.sample.size)
        stop(paste0(error.prefix, "not enough samples found"))
    
    # 6. Return the mean and standard deviation of the in-msa and out-of-msa samples. Note that this next needs to be processed via metalocation type and made into a list of matrices by model stratum.
    list(in.mean = mean(p.bias.in.msa),
         out.mean = mean(p.bias.out.msa),
         in.sd = sd(p.bias.in.msa),
         out.sd = sd(p.bias.out.msa))
}

format.p.bias.estimates = function(p.bias.estimates, metalocation.types, n.years, n.model.strata) {
    
    # Each model stratum gets a matrix with nrow = n.years and ncol = number of metalocations.
    # Whether the in-msa or out-of-msa values are used depends on the metalocation type.
    
    p.bias.each.stratum = lapply(metalocation.types, function(metaloc.type) {
        if (metaloc.type == 'county-in-msa') rep(p.bias.estimates$in.mean, n.years)
        else if (metaloc.type == 'county-out-msa') rep(p.bias.estimates$out.mean, n.years)
        else rep(0, n.years)
    })
    p.bias.each.stratum = matrix(unlist(p.bias.each.stratum), nrow = n.years, ncol = length(metalocation.types))
    p.bias = rep(list(p.bias.each.stratum), n.model.strata)
    
    p.bias.sd.each.stratum = lapply(metalocation.types, function(metaloc.type) {
        if (metaloc.type == 'county-in-msa') rep(p.bias.estimates$in.sd, n.years)
        else if (metaloc.type == 'county-out-msa') rep(p.bias.estimates$out.sd, n.years)
        else rep(0, n.years)
    })
    p.bias.sd.each.stratum = matrix(unlist(p.bias.sd.each.stratum), nrow = n.years, ncol = length(metalocation.types))
    p.bias.sd = rep(list(p.bias.sd.each.stratum), n.model.strata)
    
    list(p.bias = p.bias, p.bias.sd = p.bias.sd)
}

# testing
stratifications = list(NULL, c('age', 'sex'), c('age', 'sex', 'risk'), c('age', 'risk'), 'risk')
p.bias = get.p.bias.estimates(data.manager = surveillance.manager,
                              stratifications = stratifications,
                              outcome.for.p = 'diagnoses',
                              outcome.for.n = 'prevalence',
                              sub.geographic.type = 'COUNTY',
                              super.geographic.type = 'STATE')
p.bias.formatted = format.p.bias.estimates(p.bias,
                                           metalocation.types = c('county-in-msa', 'county-in-msa', 'county-out-msa'),
                                           n.years = 4,
                                           n.model.strata = 3)
