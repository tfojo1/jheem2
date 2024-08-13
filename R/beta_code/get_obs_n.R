# get obs_n

# To get obs_n, I'll call "get.average" once with the model stratification as the stratification, without a target ontology.
# This will search for *n* data and we'll have a problem if data for more than one ontology is found and the data for the second ontology can't be mapped to the first one. We'll deal with that if it happens.
# We'll have found which locations have *n* data already (probably just states? Or also MSA?)
# We'll pull data and see what holes we have. Then we'll go about plugging those holes.
# We'll return this array of obs_n, still stratified by location and not reverse mapped to the model ontology yet.
# We can also return the missing mask so that we can later know what had been replaced.
# We'll get an aligning mapping with the sim ontology now and map the data to the common ontology.
# Then we'll take the sum.
# We can map the missing mask as well.
# Then we'll reverse map both the data and the mask to the model ontology.
# We'll use the partitioning function to partition as necessary.
# We'll take the sum and compare it to the original sum -- should be equal.
# Then we'll convert this array to a list by model stratum, as well as the mask.

get.obs.n = function(data.manager, stratification, locations.with.n.data, years.with.data, outcome.for.n, sim.ontology, model.strata, partitioning.function) {
    
    # Get obs.n.array with its missing data mask attached an attribute. It is in its own ontology.
    obs.n.array = get.average(data.manager, stratification, locations.with.n.data, years.with.data, outcome.for.n, is.top.level = T)
    data.ontology = as.ontology(dimnames(obs.n.array), incomplete.dimensions = c('year', 'location'))
    
    # get mapping to align
    aligning.mappings = get.mappings.to.align.ontologies(data.ontology, sim.ontology)
    obs.n.array.aligned = aligning.mappings[[1]]$apply(obs.n.array)
    # obs.n.mask.aligned = aligning.mappings[[1]]$apply(attr(obs.n.array, 'missing.data.mask'), fun = &) # QUESTION FOR TODD: HOW TO MAKE MAPPING$APPLY HAVE 'OR' AS A FUNCTION?
    
    # reverse map data and model mask to model ontology
    model.arr.dimnames = sim.ontology[names(sim.ontology) %in% c('year', 'location', stratification)]
    model.arr.dimnames$year = years.with.data
    model.arr.dimnames$location = locations.with.n.data
    model.arr.indices = aligning.mappings[[2]]$get.reverse.mapping.indices(model.arr.dimnames, dimnames(obs.n.array.aligned))
    model.arr = array(obs.n.array.aligned[model.arr.indices], sapply(model.arr.dimnames, length), model.arr.dimnames)
    # model.mask = array(obs.n.mask.aligned[model.arr.indices], sapply(model.arr.dimnames, length), model.arr.dimnames)


    # use the partitioning function
    partitioned.model.arr = partitioning.function(model.arr)
    
    # check sum against the sum of the values in the aligned obs array that are actually mapped
    obs.n.arr.indices = aligning.mappings[[2]]$get.mapping.indices(model.arr.dimnames, dimnames(obs.n.array.aligned))
    values.that.map = sapply(obs.n.arr.indices, function(x) {length(x) > 0})
    if (sum(obs.n.array.aligned[values.that.map], na.rm=T) != sum(partitioned.model.arr, na.rm=T))
        stop("Sums not equal before and after partitioning obs-n")
    
    # Return a list of matrices, one per stratum, indexed by year and location
    n.obs.locations = length(locations.with.n.data) # should equal how many locations we have in our array
    n.years = length(years.with.data)

    one.matrix.per.stratum = lapply(1:nrow(model.strata), function(i) {
        model.dimension.values = setNames(model.strata[i,], names(model.strata[i,]))
        stratum.matrix = matrix(partitioned.model.arr[get.array.access.indices(dimnames(partitioned.model.arr), dimension.values = model.dimension.values)],
                                nrow = n.years,
                                ncol = n.obs.locations)
    })
}


# hard code female msm as not needed so we can stop if it is NA?
get.average = function(data.manager, stratification, locations.with.n.data, years.with.data, outcome.for.n, is.top.level = F, top.level.dimnames = NULL)
{
    # print(paste0("calling get.average for stratification '", paste0(stratification, collapse = "__"), "'"))
    data = data.manager$pull(outcome = outcome.for.n,
                             keep.dimensions = c('year', 'location', stratification),
                             dimension.values = list(location = locations.with.n.data, year = years.with.data))

    # if we are at the top level but go nothing, throw an error because these are supposed to be locations with data
    if (is.null(data) && is.top.level)
        stop("Top level did not have any data for locations that were supposed to have data")

    # If data from multiple sources, take geometric mean. Then get rid of source dimension
    if (!is.null(data)) {
        if (dim(data)[['source']] > 1) {
            count.not.na = apply(data, MARGIN=names(dim(data))[names(dim(data)) != 'source'], FUN=function(x) {sum(!is.na(x))})
            expanded.count.not.na = expand.array(count.not.na, dimnames(data))
            data[is.na(data) & expanded.count.not.na != 0] = 1
            data = apply(data, MARGIN=names(dim(data))[names(dim(data)) != 'source'], FUN=prod) ^ (1 / count.not.na)
        } else {
            new.dimnames = dimnames(data)[names(dimnames(data)) != 'source']
            data = array(data, dim = sapply(new.dimnames, length), new.dimnames)
        }
    }
    
    # If we are lacking certain years (or locations), expand data to include them.
    if (!is.null(data)) {
        if (!setequal(dimnames(data)$location, locations.with.n.data) || !setequal(dimnames(data)$year, years.with.data)) {
            complete.dimnames = dimnames(data)
            complete.dimnames$location = locations.with.n.data
            complete.dimnames$year = years.with.data
            complete.array = array(NA, dim=sapply(complete.dimnames, length), complete.dimnames)
            complete.array[get.array.access.indices(complete.array, dimnames(data))] = data
            data = complete.array
        }
    }
    
    if (is.top.level) top.level.dimnames = dimnames(data)
    attr(data, 'missing.data.mask') = NULL # ??
    
    # If we're at a 0- or 1-way stratification, just return what we got (which may be NULL)
    k = length(stratification)
    if (k < 2) return(data)
    
    # If we got something (not NULL), then see what we're missing. It may be nothing, in which case we can return what we got.
    if (!is.null(data)) {
        missing.data.mask = is.na(data)
        attr(data, 'missing.data.mask') = missing.data.mask
        if (!any(missing.data.mask)) return(data)
    }

    # Otherwise, we're either missing something or have nothing, so we will take an average built out of k-2- and k-1-way stratifications
    # there will be k choose k-2 of the k-2-way stratifications and k choose k-1 of the k-1-way stratifications.
    k.minus.1.way.stratifications = combn(stratification, k - 1, simplify = F)
    k.minus.2.way.stratifications = combn(stratification, k - 2, simplify = F)
    
    # get each k-2-way and expand it to fit the data dimnames
    # if we lack data for certain years... could be an issue... should add years in I think
    
    k.minus.2.way.data = lapply(k.minus.2.way.stratifications, function(k.minus.2.way.stratification) {
        lower.data = get.average(data.manager, k.minus.2.way.stratification, locations.with.n.data, years.with.data, outcome.for.n, is.top.level = F, top.level.dimnames)
        if (is.null(lower.data)) return (NULL)
        if (!is.array(lower.data)) browser()
        expand.array(lower.data, top.level.dimnames[names(top.level.dimnames) %in% c('year', 'location', stratification)])
    })
    
    k.minus.1.way.data = lapply(k.minus.1.way.stratifications, function(k.minus.1.way.stratification) {
        lower.data = get.average(data.manager, k.minus.1.way.stratification, locations.with.n.data, years.with.data, outcome.for.n, is.top.level = F, top.level.dimnames)
        if (is.null(lower.data)) return (NULL)
        if (!is.array(lower.data)) browser()
        expand.array(lower.data, top.level.dimnames[names(top.level.dimnames) %in% c('year', 'location', stratification)])
    })
    
    # Now we build our average... We need to know which numerators (product of two k-1 stratifications) go with which denominators (a single k-2 stratification)
    # We will build terms out of denominators since there are no repeated denominators. The numerator for a denominator is the product of the two k-1 stratifications that include all the dimensions of the denominator.
    arrays.to.be.averaged = lapply(1:length(k.minus.2.way.stratifications), function(i) {
        k.minus.2.way.stratification = k.minus.2.way.stratifications[[i]]
        matching.k.minus.1.way.stratification.mask = sapply(k.minus.1.way.stratifications, function(k.minus.1.way.stratification) {is.subset(k.minus.2.way.stratification, k.minus.1.way.stratification)})
        Reduce('*', k.minus.1.way.data[matching.k.minus.1.way.stratification.mask]) / k.minus.2.way.data[[i]]
    })
    
    arrays.to.be.averaged.na.mask = lapply(arrays.to.be.averaged, function(arr) {is.na(arr)})
    count.not.na = length(arrays.to.be.averaged) - Reduce('+', arrays.to.be.averaged.na.mask)
    arrays.to.be.averaged.replaced.with.zeroes = lapply(arrays.to.be.averaged, function(arr) {
        arr[is.na(arr) & count.not.na != 0] = 1
        arr
    })
    
    replacement.array = Reduce('+', arrays.to.be.averaged.replaced.with.zeroes) / count.not.na
    
    # either replace the missing positions with those from the replacement array, or return the whole replacement array
    if (is.null(data)) return(replacement.array)
    data[missing.data.mask] = replacement.array[missing.data.mask]
    data
}

# young_black_male estimate = young_black * fraction of black who are male = (young_black) * (black_male) / black = (black_male) * (young_black) / black

# testing
# for test

adjust.array = function(arr, version)
{
    # return an array that partitions IDU into 25% active and 75% remission
    partitioning.arr.dimnames = list(risk = c('active_IDU', 'IDU_in_remission'))
    partitioning.arr = array(c(0.25, 0.75), dim =sapply(partitioning.arr.dimnames, length), partitioning.arr.dimnames)
    
    modified = array.access(arr, dimnames(partitioning.arr))
    modified = modified * expand.array(partitioning.arr, dimnames(modified))
    array.access(arr, dimnames(modified)) = modified
    arr
    
}
partition.array = function(arr, version) {adjust.array(arr, version)}

sim.ontology = ontology(year = as.character(2002:2022), # I changed it since Baltimore MSA lacks data before 2019
                        location = c('C.12580', 'MD', '24003', '24005', '24013', '24025', '24027', '24035', '24510'),
                        age = c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years'),
                        race = c('black', 'hispanic', 'other'),
                        sex = c('heterosexual_male', 'female', 'msm'),
                        risk = c('active_IDU', 'never_IDU', 'IDU_in_remission'),
                        incomplete.dimensions = c('year', 'location'))

model.stratification = c('sex', 'risk', 'race')
years.with.data = as.character(2014:2021)
locations.with.n.data = c("24510", "MD")
sim.ontology$location = locations.with.n.data
model.strata = expand.grid(sim.ontology[names(sim.ontology) %in% model.stratification])

obs_n = get.obs.n(surveillance.manager,
                  model.stratification,
                  locations.with.n.data,
                  years.with.data,
                  outcome.for.n = 'diagnosed.prevalence',
                  sim.ontology,
                  model.strata,
                  partitioning.function = partition.array)


