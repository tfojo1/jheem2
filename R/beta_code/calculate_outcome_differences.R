### NEW ALGORITHM

# if NA propagated all the way throw an error
# n for linkage is new diagnoses, for prevalence etc. is population, and for suppression, is "diagnosed.prevalence"
# proportion aware of diagnosis (status), CDC guesses based on people's CD4 count which reveals how long before diagnosis infection occurred. n would be people with diagnosed and undiagnosed HIV. Calibrate to percentage. Handled as special case? Zoe could add in the denominator to data manager?
# won't work because we never have MSA-level data. Let someone pick a different denominator, in this case could be diagnoses. If state has 2x diagnoses compared to MSA, it likely has 2x the people unaware of diagnosis. Could be wrong if certain groups more likely to be diagnosed/aware.
# Also if state is less likely than city to diagnosis certain people

# So actually, this just takes in the single stratification that is the sim keep dimensions, or rather, the common ontology version of it.

get.n.multipliers = function(metalocations.to.minimal.component.map, metalocation.type, main.location, stratification, sim.ontology, model.strata, data.manager, outcome, years)
{
    n.metalocations = length(metalocations.to.minimal.component.map)
    n.years = length(years)
    
    one.array.per.metalocation = lapply(1:n.metalocations, function(i) {
        if (metalocation.type[[i]] == 'msa') {
            output.dimnames = sim.ontology[names(sim.ontology) %in% c('year', stratification)]
            arr = array(1, dim=sapply(output.dimnames, length), output.dimnames)
        }
        else {
            arr = returns_ratios(location.1 = metalocations.to.minimal.component.map[[i]],
                                 location.2 = main.location,
                                 stratification = stratification, data.manager = data.manager, outcome = outcome, years = years)
        }
        # Map this back to the model ontology
        arr.ontology = as.ontology(dimnames(arr), incomplete.dimensions = 'year')
        aligning.mappings = get.mappings.to.align.ontologies(arr.ontology, sim.ontology) # needs to be subset of sim.ontology?
        if (is.null(aligning.mappings)) stop(paste0("couldn't find mappings to align the metalocation data ontology and sim ontology"))
        aligned.data = aligning.mappings[[1]]$apply(arr)
        attr(aligned.data, 'mapping') = aligning.mappings[[2]]
        
        # return aligned data with its mapping attached
        aligned.data
    })
    
    # Return a matrix [year, metalocation] for each model stratum
    one.matrix.per.model.stratum = lapply(1:nrow(model.strata), function(i) {
        model.dimension.values = as.list(setNames(model.strata[i,], names(model.strata)[i,]))
        stratum.slice.by.metalocation = lapply(one.array.per.metalocation, function(j) {
            obs.dimension.values = attr(j, 'mapping')$apply.to.dim.names(model.dimension.values)
            slice = j[get.array.access.indices(dimnames(j), dimension.values = obs.dimension.values)]
        })
        stratum.matrix = matrix(unlist(stratum.slice.by.metalocation), nrow = n.years, ncol = n.metalocations)
    } )
}


returns_ratios = function(location.1, location.2, stratification, data.manager, outcome, years)
{
    print(paste0("called returns_ratios with strat = '", paste(stratification, collapse = "__", '"')))
    
    # Pull data for each location. Later, we will map them to an aligning ontology.
    keep.dimensions = c('year', stratification)
    location.data = lapply(list(location.1, location.2), function(location) {
        data = data.manager$pull(outcome = outcome,
                                 keep.dimensions = keep.dimensions,
                                 dimension.values = list(year = years, location = location))
        output.before.replacement = data
        
        # if there is data for multiple sources, take the geometric mean of the values for each source, then remove source as a dimension.
        if (!is.null(data)) {
            if (dim(data)[['source']] > 1) {
                count.not.na = apply(data, MARGIN=names(dim(data))[names(dim(data)) != 'source'], FUN=function(x) {sum(!is.na(x))})
                expanded.count.not.na = expand.array(count.not.na, dimnames(data))
                data[is.na(data) & expanded.count.not.na != 0] = 1
                output.before.replacement = apply(data, MARGIN=names(dim(data))[names(dim(data)) != 'source'], FUN=prod) ^ (1 / count.not.na)
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
        # align them
        location.data.ontologies = lapply(location.data.dimnames, function(dn) {as.ontology(dn, incomplete.dimensions = 'year')})
        aligning.mappings = get.mappings.to.align.ontologies(location.data.ontologies[[1]], location.data.ontologies[[2]])
        if (is.null(aligning.mappings))
            stop("error calculating n-multipliers: data ontologies cannot be reconciled")
        aligning.dimnames = aligning.mappings[[1]]$apply.to.dim.names(from.dim.names = location.data.dimnames[[1]])
        location.data = lapply(1:2, function(i) {aligning.mappings[[i]]$apply(location.data[[i]], to.dim.names = aligning.dimnames)})
        ratio.arr = location.data[[1]] / location.data[[2]]
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
        lower.ratio.arr = returns_ratios(location.1, location.2, lower.stratification, data.manager, outcome, years)
        if (!is.null(lower.ratio.arr) && !is.null(ratio.arr)) lower.ratio.arr = expand.array(to.expand = lower.ratio.arr, target.dim.names = dimnames(ratio.arr))
        lower.ratio.arr
    })
    
    # remove ratio arrays that are all NAs -- these won't participate in the geometric mean
    stratification.ratios = stratification.ratios[sapply(stratification.ratios, function(arr) {!all(is.na(arr))})]
    
    # If we have only one array coming back, we can use it be AND done
    if (length(stratification.ratios) == 1) {
        if (is.null(ratio.arr)) return (stratification.ratios[[1]])
        else ratio.arr[missing.data.mask] = stratification.ratios[[1]][missing.data.mask]
    }

    else if (length(stratification.ratios) > 1) {
        
        # If we're getting back arrays of different dimensions, need to expand them all to have the same size. THIS IS WHERE WE ASSUME THAT EACH STRATUM OUTPUT HAS THE SAME ONTOLOGY FOR THESE SHARED DIMENSIONS.
        expand.to.dimnames = list()
        for (arr in stratification.ratios)
        {
            append.dimnames = dimnames(arr)[!names(dimnames(arr)) %in% names(expand.to.dimnames)]
            expand.to.dimnames = c(expand.to.dimnames, append.dimnames)
        }
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
    ratio.arr # may be NULL
}

# restructure.n.multipliers = function(n.multipliers, n.years, model.strata, model.to.common.mapping)
# {
#     
#     # takes in a list of arrays, one per metalocation ('n.multipliers')
#     # returns a list of matrices, one per model stratum
#     n.metalocations = length(n.multipliers)
#     lapply(1:nrow(model.strata), function(i) {
#         model.dimension.values = model.strata[i,]
#         obs.dimension.values = model.to.common.mapping$apply.to.dimnames(model.dimension.values) # proper usage??
#         stratum.slice.by.metalocation = lapply(n.multipliers, function(metaloc.multipliers) {
#             slice = metaloc.multipliers[get.array.access.indices(dimnames(metaloc.multipliers), dimension.values = obs.dimension.values)]
#         })
#         stratum.matrix = matrix(unlist(stratum.slice.by.metalocation), nrow = n.years, ncol = n.metalocations)
#     })
# }


# for test
# common.ontology = ontology(year = as.character(2019:2022), # I changed it since Baltimore MSA lacks data before 2019
#                            location = c('C.12580', 'MD', '24003', '24005', '24013', '24025', '24027', '24035', '24510'),
#                            age = c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years'),
#                            race = c('black', 'hispanic', 'other'),
#                            sex = c('male', 'female'),
#                            risk = c('idu', 'msm', 'msm_idu', 'heterosexual'),
#                            incomplete.dimensions = 'year')
# common.ontology = common.ontology[names(common.ontology) %in% c('year', 'location', 'age', 'sex', 'risk')] # corresponds to our model's single stratification


# test
stratification = c('age', 'sex', 'risk')
# substrata.dimvals = list(list(age='13-24 years', sex='male'), list(age='25-34 years', sex='male'), list(age='13-24 years', sex='female'), list(age='25-34 years', sex='female'), list()) # generate somehow
# corresponding.strata = c(rep(1, 4), 2) # which stratification substrat.dimvals come from by index
metalocations = list(c('24003', '24005', '24013'),
                     c('24025', '24027', '24035', '24510'))
n.metalocations = length(metalocations)
metalocation.types = c('minimum-out-of-msa', 'minimum-in-msa')
n.multipliers = lapply(1:n.metalocations, function(i) {
    calculate.outcome.differences(location.1 = metalocations[[i]],
                                  location.2 = "C.12580",
                                  stratification = stratification,
                                  data.manager = surveillance.manager, #make sure you've got the data manager loaded... and the mappings registered!
                                  outcome = 'diagnosed.prevalence',
                                  years = as.character(2018:2021),
                                  metalocation.is.msa = metalocation.types[[i]] == 'msa')
})

# restructured = restructure.n.multipliers(n.multipliers,
#                                          n.years = length(2018:2021),
#                                          model.strata = model.strata, # don't have here
#                                          model.to.common.mapping = model.to.common.mapping # also don't have
#                                          )
