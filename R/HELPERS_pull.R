
#' @export
do.aggregation <- function(data.manager,
                           pre.agg.dimnames,
                           data.to.aggregate,
                           keep.dimensions,
                           data.type,
                           metric,
                           scale,
                           source.name,
                           outcome,
                           dimension.values,
                           target.ontology,
                           outcome.info,
                           na.rm) {
    
    default.failure.return <- NULL
    
    post.agg.dimensions = intersect(names(pre.agg.dimnames), keep.dimensions)
    post.agg.dimnames = pre.agg.dimnames[post.agg.dimensions]
    
    if (metric %in% c("coefficient.of.variance", "standard.deviation")) {
        # browser()
        converted.and.cv.estimate <- do.conversion.to.variance(data.manager=data.manager,
                                                               metric=metric,
                                                               data.to.process=data.to.aggregate,
                                                               outcome,
                                                               source.name,
                                                               keep.dimensions.for.pull = names(pre.agg.dimnames),
                                                               dimension.values.for.pull = dimension.values[names(dimension.values) %in% names(pre.agg.dimnames)],
                                                               strat.dimnames=strat.dimnames,
                                                               target.ontology=target.ontology) # Difference, here want target but before don't
        # Other difference, use special dimension values and keep dimensions for the recursive pull
        data.to.aggregate <- converted.and.cv.estimate$converted.data
        if (is.null(data.to.aggregate)) return(default.failure.return)
    }
    if (scale %in% c('non.negative.number', 'number')) {
        aggregated.data = apply.robust(data.to.aggregate, post.agg.dimensions, sum, na.rm=na.rm)
    }

    else if (scale %in%  c('rate', 'time', 'proportion', 'ratio')) {
        aggregated.data = do.mapping.or.aggregation.of.fraction(is.aggregation=T,
                                                                data.manager=data.manager,
                                                                data.to.process = data.to.aggregate,
                                                                denom.dim.vals=pre.agg.dimnames,
                                                                source.name = source.name,
                                                                denominator.outcome = outcome.info$denominator.outcome,
                                                                denominator.sources=NULL,
                                                                denominator.lags.by.one.year = outcome.info$denominator.lags.by.one.year,
                                                                target.ontology.for.pull=target.ontology, ## for mapping would be strat.dimnames
                                                                na.rm=na.rm,
                                                                square.denominator= metric %in% c("coefficient.of.variance", "standard.deviation", "variance"),
                                                                mapping.to.apply=NULL,
                                                                dimnames.for.apply=NULL,
                                                                post.agg.dimnames=post.agg.dimnames)
    }
    else stop(paste0(error.prefix, 'aggregating ', scale, ' data is not yet implemented'))
    
    ## PROCESS METRICS
    
    aggregated.data
    
}

#' @export
do.pre.aggregation.processing <- function(data.manager,
                                          append.attributes,
                                          mapping.to.apply,
                                          strat.dimnames,
                                          dimnames.for.apply,
                                          strat.data,
                                          metric,
                                          outcome,
                                          source.name,
                                          keep.dimensions,
                                          dv.names,
                                          outcome.info,
                                          na.rm,
                                          append.attributes.data=NULL) {
    # Return all NULL if failure due to being unable to map or lacking a needed denominator
    default.failure.return <- lapply(c("data", append.attributes), function(data.type) {NULL})
    
    if (!mapping.to.apply$can.apply.to.dim.names(from.dim.names = strat.dimnames,
                                                 to.dim.names = dimnames.for.apply,
                                                 throw.errors = F))
        return(default.failure.return)
    
    data.to.process <- strat.data
    
    if (metric %in% c("coefficient.of.variance", "standard.deviation")) {
        converted.and.cv.estimate <- do.conversion.to.variance(data.manager=data.manager,
                                                               metric=metric,
                                                               data.to.process=data.to.process,
                                                               outcome=outcome,
                                                               source.name=source.name,
                                                               keep.dimensions.for.pull = union(keep.dimensions, dv.names),
                                                               dimension.values.for.pull = strat.dimnames,
                                                               target.ontology=NULL)
        data.to.process <- converted.and.cv.estimate$converted.data
        if (is.null(data.to.process)) return(default.failure.return)
    }
    
    # Mappings inherently perform sum operations, but that is invalid for these scales. We therefore can only map the counts and then reproduce the rate/time/proportions/ratios afterwards.
    # If we have an identity mapping, then we can skip this
    if (outcome.info[['metadata']][['scale']] %in% c('rate', 'time', 'proportion', 'ratio') && !mapping.to.apply$is.identity.mapping) {
        mapped.data <- do.mapping.or.aggregation.of.fraction(is.aggregation=F,
                                                             data.manager=data.manager,
                                                             data.to.process=data.to.process,
                                                             denom.dim.vals=strat.dimnames,
                                                             source.name=source.name,
                                                             denominator.outcome=outcome.info$denominator.outcome,
                                                             denominator.sources=NULL,
                                                             denominator.lags.by.one.year = outcome.info$denominator.lags.by.one.year,
                                                             target.ontology.for.pull = strat.dimnames,
                                                             na.rm=na.rm,
                                                             square.denominator= metric %in% c("coefficient.of.variance", "standard.deviation", "variance"),
                                                             mapping.to.apply=mapping.to.apply,
                                                             dimnames.for.apply=dimnames.for.apply,
                                                             post.agg.dimnames=NULL)
        if (is.null(mapped.data)) return(default.failure.return)
    }
    
    else
        mapped.data <- mapping.to.apply$apply(data.to.process,
                                              na.rm = na.rm,
                                              to.dim.names = dimnames.for.apply,
                                              fun = "sum")
    
    if (metric %in% c("coefficient.of.variance", "standard.deviation"))
        mapped.data <- do.conversion.from.variance(metric,
                                                   mapped.data,
                                                   converted.and.cv.estimate$estimate.data.for.cv,
                                                   mapping.to.apply,
                                                   dimnames.for.apply,
                                                   na.rm)
    
    mapped.metadata <- lapply(append.attributes.data, function(metadata.to.process) {
        function.to.apply = function(x) {list(unique(unlist(x)))}
        mapped.metadata.this.type <- mapping.to.apply$apply(metadata.to.process,
                                                            na.rm=na.rm,
                                                            to.dim.names=dimnames.for.apply,
                                                            fun = function.to.apply)
        
        # We might need to subset details or url if the 'data' was unexpectedly subset due to denominator data for a proportion having fewer years or locations
        if (outcome.info$metadata$scale %in% c("rate", "time", "proportion", "ratio") || metric =="coefficient.of.variance")
            mapped.metadata.this.type <- array.access(mapped.metadata.this.type, dimnames(mapped.data))
        mapped.metadata.this.type
    })
    
    mapped.data.by.type <- setNames(c(list(mapped.data), mapped.metadata), c("data", append.attributes))
    
}

#' @export
do.conversion.to.variance <- function(data.manager,
                                      metric,
                                      data.to.process,
                                      outcome,
                                      source.name,
                                      keep.dimensions.for.pull, #union(keep.dimensions, dv.names) vs. names(pre.agg.dimnames)
                                      dimension.values.for.pull, #strat.dimnames vs. dimension.values[names(dimension.values) %in% names(pre.agg.dimnames)]
                                      strat.dimnames,
                                      target.ontology) {
    # Convert to standard.deviation
    estimate.data.for.cv = NULL
    if (metric == "coefficient.of.variance") {
        
        # Check if this source has data, otherwise use all available sources and apply mean over result, or give up.
        if (source.name %in% names(data.manager$data[[outcome]][['estimate']]))
            estimate.source = source.name
        else if (length(names(data.manager$data[[outcome]][['estimate']]))>0)
            estimate.source = names(data.manager$data[[outcome]][['estimate']])
        else return(NULL)
        
        # pull with no target ontology because this has to mesh the strat dimnames as they are here, and we know they will
        estimate.data.for.cv = data.manager$pull(outcome = outcome,
                                                 metric = 'estimate',
                                                 source = estimate.source,
                                                 keep.dimensions = union(keep.dimensions.for.pull, names(dimension.values.for.pull)),
                                                 dimension.values = dimension.values.for.pull)
        
        if (is.null(estimate.data.for.cv)) return(NULL)
        
        estimate.data.for.cv = do.strip.source.dimension(estimate.data.for.cv)
        
        # Intersect to achieve overlap
        dimnames.in.common = get.dimension.values.overlap(dimnames(data.to.process), dimnames(estimate.data.for.cv))
        data.to.process = array.access(data.to.process, dimnames.in.common)
        estimate.data.for.cv = array.access(estimate.data.for.cv, dimnames.in.common)
        
        # sd = cv * mean
        data.to.process = data.to.process * estimate.data.for.cv
    }
    
    # Convert to variance
    if (metric %in% c("coefficient.of.variance", "standard.deviation"))
        data.to.process = data.to.process ** 2
    
    # Return both data.to.process and estimate.data.for.cv, which will be needed later for re-conversion
    list(converted.data=data.to.process, estimate.data.for.cv=estimate.data.for.cv)
}

#' @export
do.conversion.from.variance <- function(metric,
                                        data.to.process,
                                        estimate.data.for.cv,
                                        mapping.to.apply,
                                        dimnames.for.apply,
                                        na.rm) {
    
    # Convert back to standard deviation from variance
    if (metric %in% c("standard.deviation", "coefficient.of.variance")) data.to.process <- sqrt(data.to.process)
    
    # Convert back to coefficient of variance from standard deviation
    if (metric == "coefficient.of.variance") {
        # map the estimate data for cv
        estimate.data.for.cv <- mapping.to.apply$apply(estimate.data.for.cv,
                                                       na.rm=na.rm,
                                                       to.dim.names = dimnames.for.apply,
                                                       fun = "sum")
        
        # We may have lost dimension values compared to what we started with
        if (!dim.names.equal(dimnames(data.to.process), dimnames(estimate.data.for.cv))) {
            dimnames.in.common = get.dimension.values.overlap(dimnames(data.to.process), dimnames(estimate.data.for.cv))
            data.to.process = array.access(data.to.process, dimnames.in.common)
            estimate.data.for.cv = array.access(estimate.data.for.cv, dimnames.in.common)
        }
        
        # cv = sd / mean
        data.to.process = data.to.process / estimate.data.for.cv
    }
    
    data.to.process
}

#' @param is.aggregation If F, then is considered a mapping step.
#' @param denom.dim.vals Used to subset the denominator the same way the proportion is.
#' @param square.denominator For metrics that have been converted to variance, so that the denominator data is squared during the weighted average.
#' @export
do.mapping.or.aggregation.of.fraction <- function(is.aggregation,
                                                  data.manager,
                                                  data.to.process,
                                                  denom.dim.vals,
                                                  source.name,
                                                  denominator.outcome,
                                                  denominator.sources,
                                                  denominator.lags.by.one.year,
                                                  target.ontology.for.pull,
                                                  na.rm=na.rm,
                                                  square.denominator=F,
                                                  mapping.to.apply=NULL,
                                                  dimnames.for.apply=NULL,
                                                  post.agg.dimnames=NULL,
                                                  debug=F) {
    if (debug) browser()
    # The dimension.values for this pull are usually the strat.dimnames, but if we have a denominator offset,
    # then we need the year part of it to be changed by <offset>, like 2020 -> 2019.
    # denom.dim.vals = strat.dimnames
    if (!is.null(denominator.lags.by.one.year))
        denom.dim.vals$year = as.character(as.numeric(denom.dim.vals$year) - denominator.lags.by.one.year)
    
    # Now we'll just do all the denominator sources
    
    
    
    # The same source might not be the right one if we aggregated locations into another source.
    # We could try each of our sources until we get one, starting with whichever matches, but trying others.
    denominator.sources = names(data.manager$data[[denominator.outcome]][['estimate']]) ############
    
    denominator.array = NULL
    
    
    denominator.array = data.manager$pull(outcome = denominator.outcome,
                                          metric = 'estimate',
                                          keep.dimensions = names(denom.dim.vals),
                                          dimension.values = denom.dim.vals,
                                          sources = denominator.sources,
                                          target.ontology = target.ontology.for.pull,
                                          allow.mapping.from.target.ontology = F,
                                          from.ontology.names = NULL, # I suppose we have no choice since the same source could use different ontologies for its denominator
                                          na.rm = na.rm,
                                          check.arguments = T) ##
    if (is.null(denominator.array)) return(NULL)
    
    denominator.array <- do.strip.source.dimension(denominator.array)
    
    # If we had an offset, rename the year dimension names to match the main data
    if (!is.null(denominator.lags.by.one.year))
        dimnames(denominator.array)$year = as.character(as.numeric(dimnames(denominator.array)$year) + denominator.lags.by.one.year)
    
    # So apparently it's possible that the ontology we get denominator data from can have the same dimension values but in a different order from those in our main data
    denom.to.data.mapping = get.ontology.mapping(dimnames(denominator.array), as.ontology(dimnames(data.to.process), incomplete.dimensions=c('year', 'location'))) # made it as.ontology b/c couldn't map year otherwise
    if (is.null(denom.to.data.mapping))
        stop(paste0(error.prefix, 'bug in aggregation code: denominator array dimensions cannot be mapped to main data dimensions'))
    
    # It's possible that we didn't find as many years or locations in the denominator as we did in the data.to.process
    # In fact, we might have lost years/locations that we needed to have according to our dimension.values
    if (!setequal(dimnames(denominator.array)$year, dimnames(data.to.process)$year) || !setequal(dimnames(denominator.array)$location, dimnames(data.to.process)$location))
        data.to.process = array.access(data.to.process, year=dimnames(denominator.array)$year, location=dimnames(denominator.array)$location)
    
    denominator.array = denom.to.data.mapping$apply(denominator.array, to.dim.names = dimnames(data.to.process))
    
    # Note that if some of data.to.process is NA when the denominator array isn't, we don't want the denominator to have contributions the numerator won't have
    denominator.array[is.na(data.to.process)]=NA
    
    # This is used for variance-type metrics where the denominator must be squared
    if (square.denominator)
        denominator.array = denominator.array ** 2
    
    # Perform weighted average
    unmapped.numerator = weighted.value.array = data.to.process * denominator.array
    
    ## I'D LIKE TO HAVE A CHECK FOR WHETHER THIS MAPPING CAN BE APPLIED, NOW THAT WE HAVE DIFFERENT DIMENSION VALUES, LIKE MISSING LOCATIONS
    ## BUT CHECK.CAN.APPLY.TO.DIM.NAMES METHOD OF MAPPINGS DOESN'T WORK YET
    # Context: Asking for Baltimore in dimnames.for.apply, but denominator data doesn't have it. The mapping will fail.
    
    ## IF AGGREGATING
    if (is.aggregation) {
        numerator.totals.array = apply.robust(weighted.value.array, names(post.agg.dimnames), sum, na.rm=na.rm)
        denominator.totals.array = apply.robust(denominator.array, names(post.agg.dimnames), sum, na.rm=na.rm)
        numerator.totals.array/denominator.totals.array
    }
    
    ## IF MAPPING
    else {
        mapped.numerator = mapping.to.apply$apply(unmapped.numerator,
                                                  na.rm = na.rm,
                                                  to.dim.names = dimnames.for.apply,
                                                  fun = "sum")
        mapped.denominator = mapping.to.apply$apply(denominator.array,
                                                    na.rm = na.rm,
                                                    to.dim.names = dimnames.for.apply,
                                                    fun = "sum")
        mapped.numerator/mapped.denominator
    }
}


#' @param arr An array. If it has no source dimension, this function does nothing.
#' @param allow.mean.across.multiple.sources Should multiple sources be condensed by taking their mean?
#' @return The input array but without a source dimension
#' @export
do.strip.source.dimension <- function(arr, allow.mean.across.multiple.sources=T) {
    non.source.dimensions = setdiff(names(dim(arr)), "source")
    if (!("source" %in% names(dim(arr))))
        arr
    else if (dim(arr)["source"]==1)
        array(arr, dim(arr)[non.source.dimensions], dimnames(arr)[non.source.dimensions])
    else if (allow.mean.across.multiple.sources)
        apply.robust(arr, non.source.dimensions, mean, na.rm=T)
    else
        stop("Cannot strip source dimension since there are multiple sources yet 'allow.mean.across.multiple.sources' is FALSE")
}