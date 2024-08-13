# DATA MANAGER PULL RECODE

# NEW STRUCTURE:

# 1. TAKE OUTCOME AND FIND A UNIVERSAL ONTOLOGY WITH THE OUTCOME ONTOLOGIES AND THE TARGET, IF THERE IS ONE.
# 1.a PARE IT DOWN TO THE UNION OF KEEP AND DIMENSION VALUES
# 1.b FOR THE FUTURE: DO A PRETEND PULL WHERE IT IS SEEN WHAT ONTOLOGIES YOU'LL ACTUALLY GET DATA FOR, THEN REMAKE THE UNIVERSAL ONTOLOGY WITH ONLY THOSE ONTOLOGIES
# 2. LOOP ACROSS SOURCES AND ONTOLOGIES
# 2.a MAP DATA DIRECTLY TO THE UNIVERSAL ONTOLOGY
# 2.b

# issue: need to have incomplete dimensions filled in with ALL possible values from all ontologies
get.universal.ontology = function(outcome, sources = NULL, ontologies = NULL, target = NULL, return.target.to.universal.mapping = T) {
    onts = self$get.ontologies.for.outcome(outcome, sources)
    if (!is.null(ontologies)) onts = onts[names(onts) %in% ontologies]
    uni = onts[[1]]
    if (length(onts) > 1) {
        for (i in 2:length(onts)) {
            mps = get.mappings.to.align.ontologies(onts[[i]], uni)
            uni = mps[[2]]$apply.to.ontology(uni)
            
            # for any incomplete dimensions, union with newly found values
            for (d in incomplete.dimensions(uni)) {
                uni[[d]] = union(uni[[d]], mps[[1]]$apply.to.ontology(onts[[i]])[[d]])
            }
            
        }
    }
    if (!is.null(target)) {
        mps = get.mappings.to.align.ontologies(target, uni)
        uni = mps[[2]]$apply.to.ontology(uni)
        if (return.target.to.universal.mapping) attr(uni, 'target.to.universal.mapping') = mps[[1]]
    }
    uni
}

get.ontologies.for.outcome = function(outcome, sources = NULL) {
    ont.names = unique(unlist(lapply(names(private$i.data[[outcome]]), function(source.name) {
        source.ontologies = names(private$i.data[[outcome]][[source.name]])
        if (is.null(sources) || source.name %in% sources) source.ontologies
        else NULL
    })))
    onts = lapply(ont.names, function(n) {self$get.registered.ontology(n)})
    names(onts) = ont.names
    onts
}

pull = function(data.manager,
                outcome,
                keep.dimensions = NULL,
                dimension.values = NULL,
                sources = NULL,
                from.ontology.names = NULL,
                target.ontology = NULL,
                allow.mapping.from.target.ontology = T,
                append.attributes = NULL,
                na.rm = F,
                check.arguments = T,
                debug = F,
                ...)
{
    error.prefix = paste0("Cannot pull '", outcome, "' data from the data manager: ")
    # *extra dimensions* are an alternative to 'dimension.values' and must pass the same checks if used
    extra.dimension.values = list(...)
    if (length(extra.dimension.values)>0) {
        dimension.values = extra.dimension.values
        check.dimension.values.valid.error.name = "pull function extra arguments"
    }
    # If *keep.dimensions* are NULL and there is a target ontology, set keep.dimensions to empty char vector
    if (!is.null(target.ontology) && is.null(keep.dimensions))
        keep.dimensions = character(0)
    
    if (check.arguments) {
        
        # *outcome* is a single, non-NA character value
        #  that has been previously registered as an outcome for this data manager
        if (!is.character(outcome) || length(outcome)!=1 || is.na(outcome) || nchar(outcome)==0)
            stop(paste0(error.prefix, "'outcome' must be a single, non-empty, non-NA character value"))
        
        outcome.info = private$i.outcome.info[[outcome]]
        if (is.null(outcome.info))
            stop(paste0(error.prefix, "'", outcome, "' is not a registered outcome."))
        
        # *keep.dimensions* is either NULL or a character vector with no NA values or repeats
        if (!is.null(keep.dimensions) && (!is.character(keep.dimensions) || any(duplicated(keep.dimensions)) || anyNA(keep.dimensions)))
            stop(paste0(error.prefix, "'keep.dimensions' must be either NULL or a character vector with no NA values or repeats"))
        
        # make a variable name for error to pass to check.dimension.values.valid
        check.dimension.values.valid.error.name = "dimension.values"
        
        # check that either this or dimension.values is len(0)
        if (length(extra.dimension.values)>0 && length(dimension.values)>0)
            stop(paste0(error.prefix, "'dimension.values' must be specified in either the 'dimension.values' argument or as additional arguments to the function"))
        
        # *dimension.values* are valid
        #   - check.dimension.values.valid() doesn't accept NULL because it wants a list
        if (!is.null(dimension.values))
            check.dimension.values.valid(dimension.values, "dimension.values")
        
        # *sources* is either NULL or a character vector with at least one element and no NA or empty values
        #  that have all been registered previously as sources for this outcome with this data manager
        if (!is.null(sources) && (!is.character(sources) || !length(sources)>0 || anyNA(sources) || any(nchar(sources)==0)))
            stop(paste0(error.prefix, "'sources' must be NULL or a character vector with at least one element and no NA or empty values"))
        
        unregistered.sources = sapply(sources, function(x){!(x %in% names(private$i.data[[outcome]]))})
        if (any(unregistered.sources))
            stop(paste0(error.prefix, "all sources must be registered for this outcome with this data manager"))
        
        # # *include.sources.without.data.in.output* is a boolean
        # if (!is.logical(include.sources.without.data.in.output) || length(include.sources.without.data.in.output) > 1 || is.na(include.sources.without.data.in.output) || is.null(include.sources.without.data.in.output))
        #     stop(paste0(error.prefix, "'include.sources.without.data.in.output' must be a single, non-NA logical value"))
        
        # # *include.sources.without.data.in.output* must be FALSE if 'sources' is NULL
        # if (is.null(sources) && include.sources.without.data.in.output)
        #     stop(paste0(error.prefix, "'sources' must not be NULL if 'include.sources.without.data.in.output' is TRUE"))
        
        # *target.ontology* is either NULL or an ontology object
        if (!is.null(target.ontology) && !is.ontology(target.ontology))
            stop(paste0(error.prefix, "'target.ontology' must be either NULL or an ontology object"))
        
        # # The target ontology also needs to contain the keep dimensions if any
        # # @AZ IS THIS STILL TRUE???
        # if (!is.null(target.ontology) && !is.null(keep.dimensions)) {
        #     if (!any(keep.dimensions %in% names(target.ontology)))
        #         stop(paste0(error.prefix, "'keep.dimensions' must be contained in 'target.ontology'"))
        # }
        
        # The target ontology cannot have any NULL dimensions (ones that do have never had any data put to them)
        if (any(sapply(target.ontology, is.null)))
            stop(paste0(error.prefix, "'target.ontology' cannot have any NULL dimensions"))
        
        # *allow.mapping.from.target.ontology* is a single, non-NA logical value
        if (!is.logical(allow.mapping.from.target.ontology) || length(allow.mapping.from.target.ontology)!=1 || is.na(allow.mapping.from.target.ontology))
            stop(paste0(error.prefix, "'allow.mapping.from.target.ontology' must be a single, non-NA logical value"))
        
        # *from.ontology.names* is either NULL or a character vector with no NA or empty values
        if (!is.null(from.ontology.names) && (!is.character(from.ontology.names) || anyNA(from.ontology.names) || any(nchar(from.ontology.names)==0)))
            stop(paste0(error.prefix, "from.ontology.names must be either NULL or a character vector with no NA or empty values"))
        
        #  all of which have been previously registered with this data manager (if not NULL)
        unregistered.ontologies = sapply(from.ontology.names, function(x){is.null(private$i.ontologies[[x]])})
        if (is.null(from.ontology.names) && any(unregistered.ontologies))
            stop(paste0(error.prefix, "all ontologies in from.ontology.names must be registered with this data manager"))
        
        # *append.attributes* is either NULL or a character vector with no NA values that
        #  contains only "details" or "url" or both
        if (!is.null(append.attributes) && (!is.character(append.attributes) || anyNA(append.attributes) || !all(append.attributes %in% c("details", "url"))))
            stop(paste0(error.prefix, "append.attributes' must be either NULL or a character vector with no NA values that contains only 'details' or 'url' or both"))
        
        # *na.rm* is a single, non-NA logical value
        if (!is.logical(na.rm) || length(na.rm)!=1 || is.na(na.rm))
            stop(paste0(error.prefix, "na.rm must be a single, non-NA, logical value"))
    }
    
    if (is.null(target.ontology) || allow.mapping.from.target.ontology) target.ontology = private$get.universal.ontology(outcome, sources, from.ontology.names, target.ontology, return.target.to.universal.mapping = allow.mapping.from.target.ontology)
    
    need.to.set.keep.dimensions.flag = is.null(keep.dimensions)
    if (!need.to.set.keep.dimensions.flag) target.ontology = target.ontology[names(target.ontology) %in% union(keep.dimensions, names(dimension.values))]
    # If sources is NULL, use all the sources from the outcome
    if (is.null(sources))
        sources.used.names = names(private$i.data[[outcome]])
    else
        sources.used.names = sources
    sources.successful.names = c()
    
    dv.names = names(dimension.values)
    
    
    ## FOR THE FUTURE: DO A PRETEND PULL TO SEE WHAT ONTOLOGIES WE NEED, THEN POTENTIALLY REMAKE THE UNIVERSAL WITH ONLY THOSE
    # if (debug) browser()
    pre.processed.data = lapply(sources.used.names, function(source.name) {
        
        source.ontology.names = names(private$i.data[[outcome]][[source.name]])
        ontologies.used.names = ifelse(is.null(from.ontology.names), source.ontology.names, intersect(source.ontology.names, from.ontology.names))
        pulled.source.data = NULL
        source.lacks.denominator.data.flag = FALSE
        
        for (ont.name in ontologies.used.names) {
            
            ont = private$i.ontologies[[ont.name]]
            stratification.names = names(private$i.data[[outcome]][[source.name]][[ont.name]])
            if (debug) browser()
            for (strat in stratification.names) {
                
                strat.data = private$i.data[[outcome]][[source.name]][[ont.name]][[strat]]
                strat.dimensions = names(dim(strat.data))
                strat.dimnames = as.ontology(dimnames(strat.data), incomplete.dimensions = intersect(incomplete.dimensions(ont), strat.dimensions))
                
                # IF WE DON'T HAVE KEEP DIMENSIONS YET, SET IT TEMPORARILY TO WHAT WE FIND HERE. ALL INCOMPLETE DIMENSIONS AND DIMENSION.VALUES DIMENSIONS OF LENGTH > 1
                if (is.null(keep.dimensions) || need.to.set.keep.dimensions.flag) {
                    dimension.values.dimensions.longer.than.one = dv.names[sapply(dimension.values, length)>1]
                    keep.dimensions = setdiff(incomplete.dimensions(strat.dimnames), dimension.values.dimensions.longer.than.one)
                }
                
                mapping.to.apply = get.ontology.mapping(strat.dimnames, target.ontology)
                if (is.null(mapping.to.apply)) next
                
                dimnames.for.apply = mapping.to.apply$apply.to.dim.names(strat.dimnames)
                if (!setequal(names(dimnames.for.apply), union(keep.dimensions, dv.names))) next
                
                # Check that the mapped stratification won't aggregate illegally due to missing some values in an incomplete dimension that will be aggregated (since it is in dimension.values but not keep.dimensions)
                missing.dimension.values.for.aggregated.dimension = F
                aggregated.dimensions = setdiff(incomplete.dimensions(dimnames.for.apply), keep.dimensions)
                for (d in aggregated.dimensions) {
                    if (d %in% dv.names) {
                        if (length(setdiff(dimension.values[[d]], dimnames.for.apply[[d]])) > 0) {
                            missing.dimension.values.for.aggregated.dimension = T
                            break
                        }
                    }
                }
                if (missing.dimension.values.for.aggregated.dimension) next
                
                ### ... hmmm
                
                # Insert dimension.values
                
                resolved.dimension.values = resolve.ontology.dimension.values(target.ontology, dimension.values, error.prefix = error.prefix, throw.error.if.unresolvable = F)
                if (is.null(resolved.dimension.values) && !is.null(dimension.values)) next
                for (d in dv.names) dimnames.for.apply[[d]] = intersect(dimnames.for.apply[[d]], resolved.dimension.values[[d]])
                
                # Apply mapping
                
                incompatible.mapped.stratification = F
                data.types = union('data', append.attributes)
                
                pulled.source.data = lapply(data.types, function(data.type) {
                    if (incompatible.mapped.stratification) return (NULL)
                    if (data.type == 'data') {
                        data.to.process = strat.data
                        function.to.apply = 'sum'
                    }
                    else {
                        data.to.process = private[[paste0('i.', data.type)]][[outcome]][[source.name]][[ont.name]][[strat]]
                        function.to.apply = function(x) {list(unique(unlist(x)))}
                    }
                    if (!mapping.to.apply$can.apply.to.dim.names(from.dim.names = strat.dimnames,
                                                                 to.dim.names = dimnames.for.apply,
                                                                 throw.errors = F)) {
                        incompatible.mapped.stratification <<- TRUE
                        return (NULL)
                    }
                    mapped.data.by.type = mapping.to.apply$apply(data.to.process,
                                                                 na.rm = na.rm,
                                                                 to.dim.names = dimnames.for.apply,
                                                                 fun = function.to.apply)
                    if (data.type != 'data') {
                        mapped.data.by.type = lapply(mapped.data.by.type, function(x) {x[[1]]})
                        dim(mapped.data.by.type) = sapply(dimnames.for.apply, length)
                        dimnames(mapped.data.by.type) = dimnames.for.apply
                    }
                    mapped.data.by.type
                })
                if (incompatible.mapped.stratification) {
                    pulled.source.data = NULL
                    next
                }
                names(pulled.source.data) = data.types
                
                # Aggregate if needed
                initial.dimnames = dimnames.for.apply
                dimensions.to.drop = intersect(which(length(initial.dimnames) == 1), which(!(names(initial.dimnames) %in% keep.dimensions)))
                
                pulled.source.data = lapply(data.types, function(data.type) {
                    # browser()
                    if (source.lacks.denominator.data.flag) return (NULL)
                    data.by.data.type = pulled.source.data[[data.type]]
                    pre.agg.dimnames = initial.dimnames
                    if (length(dimensions.to.drop) > 0) {
                        pre.agg.dimnames = pre.agg.dimnames[-dimensions.to.drop]
                        data.by.data.type = array(data.by.data.type,
                                                  dim = sapply(pre.agg.dimnames, length),
                                                  dimnames = pre.agg.dimnames)
                    }
                    if (length(pre.agg.dimnames) > length(keep.dimensions)) {
                        post.agg.dimnames = pre.agg.dimnames[names(pre.agg.dimnames) %in% keep.dimensions]
                        if (data.type == 'data') {
                            scale = outcome.info[['metadata']][['scale']]
                            if (scale %in% c('non.negative.number', 'number')) {
                                data.by.data.type = apply(data.by.data.type, keep.dimensions, FUN = sum, na.rm = na.rm)
                                dim(data.by.data.type) = sapply(post.agg.dimnames, length)
                                dimnames(data.by.data.type) = post.agg.dimnames
                            } else if (scale %in% c('rate', 'time', 'proportion')) {
                                denominator.outcome = outcome.info[['denominator.outcome']] ## NOTE: I MUST TELL ZOE TO ADD THIS AFTER I INTRODUCE THE REQUIREMENT TO HAVE IT
                                denominator.ontology = target.ontology ## NOTE: DO WE NEED TO HAVE THE UNIVERSAL ALIGN TO THE DENOMINATOR ONTOLOGIES TOO, WHEN WE KNOW WE'LL NEED IT?
                                
                                denominator.array = self$pull(outcome = denominator.outcome,
                                                              keep.dimensions = names(pre.agg.dimnames),
                                                              dimension.values = dimension.values,
                                                              sources = source.name,
                                                              target.ontology = denominator.ontology,
                                                              allow.mapping.from.target.ontology = F,
                                                              from.ontology.names = NULL,
                                                              append.attributes = NULL,
                                                              na.rm = na.rm)
                                if (is.null(denominator.array)) {
                                    source.lacks.denominator.data.flag <<- TRUE
                                    return (NULL)
                                }
                                # Since the denominator.array came from only one source, we can remove the source so that it will match size of data
                                denominator.array = array(denominator.array,
                                                          dim = dim(denominator.array)[names(dim(denominator.array)) != 'source'],
                                                          dimnames = dimnames(denominator.array)[names(dimnames(denominator.array)) != 'source']
                                )
                                
                                # Catch an otherwise invisible bug if denominator.array somehow doesn't have the same shape/order as the data
                                if (!identical(dimnames(denominator.array), dimnames(data.by.data.type)))
                                    stop(paste0(error.prefix, 'bug in aggregation code: denominator array has incorrect dimensions'))
                                
                                # We should find totals by aggregating the denominator.array rather than pulling less stratified data
                                # because less stratified data might not equal the sum of the more stratified data in denominator.array
                                denominator.totals.array = apply(denominator.array, keep.dimensions, FUN = sum, na.rm=na.rm)
                                dim(denominator.totals.array) = sapply(post.agg.dimnames, length)
                                dimnames(denominator.totals.array) = post.agg.dimnames
                                
                                weighted.value.array = data.by.data.type * denominator.array
                                
                                data.by.data.type = apply(weighted.value.array, keep.dimensions, FUN = sum, na.rm=na.rm)
                                dim(data.by.data.type) = sapply(post.agg.dimnames, length)
                                dimnames(data.by.data.type) = post.agg.dimnames
                                
                                data.by.data.type = data.by.data.type / denominator.totals.array
                            } else if (scale == 'ratio') stop(paste0(error.prefix, scale, ' data cannot be aggregated'))
                            else stop(paste0(error.prefix, 'aggregating ', scale, ' data is not yet implemented'))
                        } else {
                            data.by.data.type = apply(data.by.data.type, keep.dimensions, function(x) {list(unique(unlist(x)))})
                            dim(data.by.data.type) = sapply(post.agg.dimnames, length)
                            dimnames(data.by.data.type) = post.agg.dimnames
                            data.by.data.type = lapply(data.by.data.type, function(x) {x[[1]]})
                            dim(data.by.data.type) = sapply(post.agg.dimnames, length)
                            dimnames(data.by.data.type) = post.agg.dimnames
                        }
                    }
                    dimnames(data.by.data.type) = as.list(dimnames(data.by.data.type))
                    data.by.data.type
                }) # end of lapply for data.types
                
                names(pulled.source.data) = data.types
                
                # SUCCESS FOR THIS ONTOLOGY AND THEREFORE SOURCE
                sources.successful.names <<- c(sources.successful.names, source.name)
                keep.dimensions <<- keep.dimensions
                need.to.set.keep.dimensions.flag <<- FALSE
                break
                
            } # end of loop for stratification
            
            if (!is.null(pulled.source.data) || source.lacks.denominator.data.flag) break
        }  # end of loop for ontology
        pulled.source.data
        
    }) # end of lapply for sources
    
    # we have a list (one element per source) of lists (one element per data type, i.e. 'data', 'url', or 'details')
    # repackage this to be a data array with 'url', 'details' and possibly a mapping as attributes
    post.processed.data = NULL
    # Some sources may have returned NULL above and should be removed.
    # browser()
    
    pre.processed.data = pre.processed.data[!unlist(lapply(pre.processed.data, is.null))]
    
    # Extract data for data, url, and details out of what lapply returned above
    if (length(pre.processed.data) > 0) {
        for (data.type in c('data', append.attributes)) {
            
            # make a list of the data from the sources
            data.by.source = lapply(pre.processed.data, function(x) {x[[data.type]]})
            names(data.by.source) = sources.successful.names
            overall.dim.names = c(dimnames(data.by.source[[1]]), list(source=sources.successful.names))
            
            return.for.this.data.type = NULL
            
            if (data.type == 'data') {
                return.for.this.data.type = sapply(data.by.source, function(x) {x})
                
            } else {
                for (source.data in data.by.source) {
                    return.for.this.data.type = append(return.for.this.data.type, source.data)
                }
            }
            
            dim(return.for.this.data.type) = sapply(overall.dim.names, length)
            dimnames(return.for.this.data.type) = overall.dim.names
            
            # incorporate it into final.return
            if (data.type == 'data') {
                post.processed.data = return.for.this.data.type
            } else if (data.type == 'url') {
                attr(post.processed.data, 'url') = return.for.this.data.type
            } else if (data.type == 'details') {
                attr(post.processed.data, 'details') = return.for.this.data.type
            }
            
        }
    }
    
    if (allow.mapping.from.target.ontology)
        attr(post.processed.data, 'mapping') = attr(target.ontology, 'target.to.universal.mapping')
    post.processed.data
},


### ------------ OLD VERSION -------------- ###

pull = function(data.manager,
                outcome,
                keep.dimensions = NULL,
                dimension.values = NULL,
                sources = NULL,
                include.sources.without.data.in.output = F,
                target.ontology = NULL,
                allow.mapping.from.target.ontology = T,
                from.ontology.names = NULL,
                append.attributes = NULL,
                na.rm = F,
                debug = F,
                ...)
{
    #-- Validate arguments --#
    # browser()
    error.prefix = paste0("Cannot pull '", outcome, "' data from the data manager: ")
    
    # *outcome* is a single, non-NA character value
    #  that has been previously registered as an outcome for this data manager
    if (!is.character(outcome) || length(outcome)!=1 || is.na(outcome) || nchar(outcome)==0)
        stop(paste0(error.prefix, "'outcome' must be a single, non-empty, non-NA character value"))
    
    outcome.info = private$i.outcome.info[[outcome]]
    if (is.null(outcome.info))
        stop(paste0(error.prefix, "'", outcome, "' is not a registered outcome."))
    
    # *keep.dimensions* is either NULL or a character vector with no NA values or repeats
    if (!is.null(keep.dimensions) && (!is.character(keep.dimensions) || any(duplicated(keep.dimensions)) || anyNA(keep.dimensions)))
        stop(paste0(error.prefix, "'keep.dimensions' must be either NULL or a character vector with no NA values or repeats"))
    
    # *extra dimensions* are an alternative to 'dimension.values' and must pass the same checks if used
    extra.dimension.values = list(...)
    
    # make a variable name for error to pass to check.dimension.values.valid
    check.dimension.values.valid.error.name = "dimension.values"
    
    # check that either this or dimension.values is len(0)
    if (length(extra.dimension.values)>0 && length(dimension.values)>0)
        stop(paste0(error.prefix, "'dimension.values' must be specified in either the 'dimension.values' argument or as additional arguments to the function"))
    if (length(extra.dimension.values)>0) {
        dimension.values = extra.dimension.values
        check.dimension.values.valid.error.name = "pull function extra arguments"
    }
    
    # *dimension.values* are valid
    #   - check.dimension.values.valid() doesn't accept NULL because it wants a list
    if (!is.null(dimension.values))
        check.dimension.values.valid(dimension.values, "dimension.values")
    
    # *sources* is either NULL or a character vector with at least one element and no NA or empty values
    #  that have all been registered previously as sources for this outcome with this data manager
    if (!is.null(sources) && (!is.character(sources) || !length(sources)>0 || anyNA(sources) || any(nchar(sources)==0)))
        stop(paste0(error.prefix, "'sources' must be a character vector with at least one element and no NA or empty values"))
    
    unregistered.sources = sapply(sources, function(x){!(x %in% names(private$i.data[[outcome]]))})
    if (any(unregistered.sources))
        stop(paste0(error.prefix, "all sources must be registered for this outcome with this data manager"))
    
    # *include.sources.without.data.in.output* is a boolean
    if (!is.logical(include.sources.without.data.in.output) || length(include.sources.without.data.in.output) > 1 || is.na(include.sources.without.data.in.output) || is.null(include.sources.without.data.in.output))
        stop(paste0(error.prefix, "'include.sources.without.data.in.output' must be a single, non-NA logical value"))
    
    # # *include.sources.without.data.in.output* must be FALSE if 'sources' is NULL
    # if (is.null(sources) && include.sources.without.data.in.output)
    #     stop(paste0(error.prefix, "'sources' must not be NULL if 'include.sources.without.data.in.output' is TRUE"))
    
    # *target.ontology* is either NULL or an ontology object
    if (!is.null(target.ontology) && !is.ontology(target.ontology))
        stop(paste0(error.prefix, "'target.ontology' must be either NULL or an ontology object"))
    
    # # The target ontology also needs to contain the keep dimensions if any
    # # @AZ IS THIS STILL TRUE???
    # if (!is.null(target.ontology) && !is.null(keep.dimensions)) {
    #     if (!any(keep.dimensions %in% names(target.ontology)))
    #         stop(paste0(error.prefix, "'keep.dimensions' must be contained in 'target.ontology'"))
    # }
    
    # The target ontology cannot have any NULL dimensions (ones that do have never had any data put to them)
    if (any(sapply(target.ontology, is.null)))
        stop(paste0(error.prefix, "'target.ontology' cannot have any NULL dimensions"))
    
    # If *keep.dimensions* are NULL and there is a target ontology, set keep.dimensions to empty char vector
    if (!is.null(target.ontology) && is.null(keep.dimensions))
        keep.dimensions = character(0)
    
    # *allow.mapping.from.target.ontology* is a single, non-NA logical value
    if (!is.logical(allow.mapping.from.target.ontology) || length(allow.mapping.from.target.ontology)!=1 || is.na(allow.mapping.from.target.ontology))
        stop(paste0(error.prefix, "'allow.mapping.from.target.ontology' must be a single, non-NA logical value"))
    
    # *from.ontology.names* is either NULL or a character vector with no NA or empty values
    if (!is.null(from.ontology.names) && (!is.character(from.ontology.names) || anyNA(from.ontology.names) || any(nchar(from.ontology.names)==0)))
        stop(paste0(error.prefix, "from.ontology.names must be either NULL or a character vector with no NA or empty values"))
    
    #  all of which have been previously registered with this data manager (if not NULL)
    unregistered.ontologies = sapply(from.ontology.names, function(x){is.null(private$i.ontologies[[x]])})
    if (is.null(from.ontology.names) && any(unregistered.ontologies))
        stop(paste0(error.prefix, "all ontologies in from.ontology.names must be registered with this data manager"))
    
    # *append.attributes* is either NULL or a character vector with no NA values that
    #  contains only "details" or "url" or both
    if (!is.null(append.attributes) && (!is.character(append.attributes) || anyNA(append.attributes) || !all(append.attributes %in% c("details", "url"))))
        stop(paste0(error.prefix, "append.attributes' must be either NULL or a character vector with no NA values that contains only 'details' or 'url' or both"))
    
    # *na.rm* is a single, non-NA logical value
    if (!is.logical(na.rm) || length(na.rm)!=1 || is.na(na.rm))
        stop(paste0(error.prefix, "na.rm must be a single, non-NA, logical value"))
    
    # --- PULL DATA --- #
    
    # These must be saved if applicable
    target.to.common.mapping = NULL
    common.ontology = NULL
    
    # If we have no target.ontology, the ontology of the first successful stratification will serve as the target.ontology for subsequent sources
    target.represents.successful.stratification.flag = FALSE
    
    # If we have no keep.dimensions, we will need to find them. Once we have a success, we won't need to look during subsequent sources.
    need.to.set.keep.dimensions.flag = is.null(keep.dimensions)
    
    # If sources is NULL, use all the sources from the outcome
    if (is.null(sources))
        sources.used.names = names(private$i.data[[outcome]])
    else
        sources.used.names = sources
    
    # Keep track of which sources found data that is going to be returned from the pull
    sources.successful.names = c()
    if (debug) browser()
    
    
    return.data = lapply(sources.used.names, function(x) {
        
        # Helps us return NULL data if we later find we need denominator data for aggregation and can't find it
        source.lacks.denominator.data.flag = FALSE
        
        # Use all ontologies in the source or only those also in from.ontology.names
        source.ontology.names = names(private$i.data[[outcome]][[x]])
        ontologies.used.names = ifelse(is.null(from.ontology.names),
                                       source.ontology.names,
                                       intersect(source.ontology.names, from.ontology.names))
        
        data.to.return = NULL
        
        for (y in ontologies.used.names) {
            
            ont = private$i.ontologies[[y]]
            
            stratification.names = names(private$i.data[[outcome]][[x]][[y]])
            # if (debug) browser()
            
            for (strat in stratification.names) {
                
                # -- GET STRAT DATA -- #
                
                
                strat.data = private$i.data[[outcome]][[x]][[y]][[strat]]
                strat.dimensions = names(dim(strat.data))
                strat.dimnames = as.ontology(dimnames(strat.data),incomplete.dimensions = intersect(incomplete.dimensions(ont), strat.dimensions))
                
                # # Check that there are data (not all NA) before any mapping is applied ?????
                # if (all(is.na(strat.data))) next
                
                # -- GET KEEP.DIMENSIONS IF APPLICABLE -- #
                
                # Only for when there is no target.ontology or the need.to.set.keep.dimensions flag is on.
                # Determine keep.dimensions if keep.dimensions is NULL. Will be all incomplete dimensions and dimension.values dimensions that are greater than length 1
                # We use the flag because a previous, failed stratification could have set keep.dimensions in this scope but we don't want to use it
                if (is.null(keep.dimensions) || need.to.set.keep.dimensions.flag) {
                    dimension.values.dimensions.longer.than.one = names(dimension.values)[sapply(dimension.values, length)>1]
                    keep.dimensions = setdiff(incomplete.dimensions(strat.dimnames), dimension.values.dimensions.longer.than.one)
                }
                
                # -- GET MAPPINGS -- #
                
                target.to.common.mapping.placeholder = NULL
                common.ontology.placeholder = NULL
                mapping.to.apply = NULL
                
                if (is.null(target.ontology))
                    mapping.to.apply = get.identity.ontology.mapping()
                else {
                    if (allow.mapping.from.target.ontology) {
                        if (!is.null(common.ontology))
                            mapping.to.apply = get.ontology.mapping(strat.dimnames, common.ontology)
                        
                        else {
                            
                            # get an aligning mapping
                            aligning.mappings.list = get.mappings.to.align.ontologies(strat.dimnames, target.ontology)
                            if (!is.null(aligning.mappings.list)) {
                                mapping.to.apply = aligning.mappings.list[[1]]
                                target.to.common.mapping.placeholder = aligning.mappings.list[[2]]
                                common.ontology.placeholder = mapping.to.apply$apply.to.ontology(strat.dimnames)
                            }
                        }
                    } else
                        mapping.to.apply = get.ontology.mapping(strat.dimnames, target.ontology)
                }
                
                # Skip this stratification if mappings were needed and couldn't be found
                if (is.null(mapping.to.apply)) next
                
                # -- MAP THE STRAT DIMNAMES -- #
                
                # Figure out the to.dimnames for when we apply a mapping/subset
                dimnames.for.apply = mapping.to.apply$apply.to.dim.names(strat.dimnames)
                
                # Check that the mapped stratification has exactly keep.dimensions + dimension.values dimensions
                if (!setequal(names(dimnames.for.apply), union(keep.dimensions, names(dimension.values))))
                    next
                
                # Check that the mapped stratification won't aggregate illegally due to missing some values in an incomplete dimension that will be aggregated (since it is in dimension.values but not keep.dimensions)
                missing.dimension.values.for.aggregated.dimension = F
                aggregated.dimensions = setdiff(incomplete.dimensions(dimnames.for.apply), keep.dimensions)
                for (d in aggregated.dimensions) {
                    if (d %in% names(dimension.values)) {
                        if (length(setdiff(dimension.values[[d]], dimnames.for.apply[[d]])) > 0) {
                            missing.dimension.values.for.aggregated.dimension = T
                            break
                        }
                    }
                }
                if (missing.dimension.values.for.aggregated.dimension) next
                
                
                # -- MAP THE TARGET ONTOLOGY DIMNAMES TO LIMIT THE DIMNAMES.FOR.APPLY INCOMPLETE DIMENSION VALUES -- #
                
                if (!is.null(target.ontology)) {
                    mapped.target.dimnames = NULL
                    if (allow.mapping.from.target.ontology) {
                        if (!is.null(target.to.common.mapping))
                            mapped.target.dimnames = target.to.common.mapping$apply.to.dim.names(target.ontology)
                        else
                            mapped.target.dimnames = target.to.common.mapping.placeholder$apply.to.dim.names(target.ontology)
                    }
                    else
                        mapped.target.dimnames = target.ontology
                    
                    mapped.target.incomplete.dimensions = incomplete.dimensions(mapped.target.dimnames)
                    dimnames.for.apply.incomplete.dimensions = incomplete.dimensions(dimnames.for.apply)
                    for (d in intersect(mapped.target.incomplete.dimensions, dimnames.for.apply.incomplete.dimensions)) {
                        dimnames.for.apply[[d]] = intersect(mapped.target.dimnames[[d]], dimnames.for.apply[[d]])
                    }
                }
                
                
                # -- INSERT DIMENSION.VALUES -- #
                
                ontology.to.resolve.against = NULL
                if (is.null(target.ontology))
                    ontology.to.resolve.against = ont
                else {
                    if (allow.mapping.from.target.ontology)
                        ontology.to.resolve.against = common.ontology.placeholder
                    else
                        ontology.to.resolve.against = target.ontology
                }  
                resolved.dimension.values = resolve.ontology.dimension.values(ontology.to.resolve.against, dimension.values, error.prefix = error.prefix, throw.error.if.unresolvable = FALSE)
                if (is.null(resolved.dimension.values) && !is.null(dimension.values)) next
                # dimnames.for.apply[names(dimension.values)] = resolved.dimension.values # there can be an issue where something in dimension.values does not exist in what the target ontology can produce from mapping
                # if (debug) browser()
                for (d in names(dimension.values))
                {
                    dimnames.for.apply[[d]] = intersect(dimnames.for.apply[[d]], resolved.dimension.values[[d]])
                }
                
                
                # -- MAP THE STRAT DATA -- #
                
                data.elements.accessors = 'data'
                if ('url' %in% append.attributes)
                    data.elements.accessors = append(data.elements.accessors, 'url')
                if ('details' %in% append.attributes)
                    data.elements.accessors = append(data.elements.accessors, 'details')
                
                incompatible.mapped.strat = FALSE
                
                # Apply mapping to data and subset in one step
                data.to.return = lapply(data.elements.accessors, function(a) {
                    
                    if (incompatible.mapped.strat) NULL
                    else {
                        # default
                        function.to.apply = 'sum'
                        
                        if (is.null(target.ontology)) data.to.process = private[[paste0('i.', a)]][[outcome]][[x]][[y]][[strat]]
                        else {
                            if (a == 'data') data.to.process = strat.data
                            else {
                                data.to.process = private[[paste0('i.', a)]][[outcome]][[x]][[y]][[strat]]
                                function.to.apply = function(b) {list(unique(unlist(b)))}
                            }
                        }
                        # if (debug) browser()
                        
                        if (!mapping.to.apply$can.apply.to.dim.names(from.dim.names = strat.dimnames,
                                                                     to.dim.names = dimnames.for.apply,
                                                                     throw.errors = F))
                            incompatible.mapped.strat <<- TRUE
                        if (incompatible.mapped.strat) NULL
                        else {
                            
                            data.temp = mapping.to.apply$apply(
                                data.to.process,
                                na.rm = na.rm,
                                to.dim.names = dimnames.for.apply,
                                fun = function.to.apply
                            )
                            if (!is.null(target.ontology) && a != 'data') {
                                data.temp = lapply(data.temp, function(b) {b[[1]]})
                                dim(data.temp) = sapply(dimnames.for.apply, length)
                                dimnames(data.temp) = dimnames.for.apply
                            }
                            data.temp
                        }
                    }
                })
                
                if (incompatible.mapped.strat) {
                    data.to.return = NULL
                    next
                }
                
                names(data.to.return) = data.elements.accessors
                
                # browser()
                # -- AGGREGATE IF NEEDED -- #
                
                # Dims and dimnames before the following transformations
                initial.dim = sapply(dimnames.for.apply, length)
                initial.dimnames = dimnames.for.apply
                
                for (d in seq_along(data.to.return)) {
                    
                    # Drop length 1 dimensions that aren't in keep.dimensions
                    dimensions.to.drop = intersect(which(initial.dim == 1),
                                                   which(!(names(initial.dim) %in% keep.dimensions)))
                    
                    # Dim and dimnames before possible aggregation
                    pre.agg.dim = initial.dim
                    pre.agg.dimnames = initial.dimnames
                    
                    if (length(dimensions.to.drop) > 0) {
                        pre.agg.dim = pre.agg.dim[-dimensions.to.drop]
                        pre.agg.dimnames = pre.agg.dimnames[-dimensions.to.drop]
                        
                        data.to.return[[d]] = array(data.to.return[[d]],
                                                    dim = pre.agg.dim,
                                                    dimnames = pre.agg.dimnames)
                    }
                    
                    # check if NEED to aggregate (i.e., any dimensions not in keep.dimensions)
                    if (length(pre.agg.dim) > length(keep.dimensions)) {
                        
                        # determine post-aggregation dim and dimnames
                        post.agg.dim = pre.agg.dim[names(pre.agg.dim) %in% keep.dimensions]
                        post.agg.dimnames = pre.agg.dimnames[names(pre.agg.dimnames) %in% keep.dimensions]
                        
                        if (names(data.to.return)[[d]] == 'data') {
                            scale = outcome.info[['metadata']][['scale']]
                            
                            if (scale %in% c('non.negative.number', 'number')) {
                                
                                data.to.return[[d]] = apply(data.to.return[[d]], keep.dimensions, FUN = sum, na.rm=na.rm)
                                dim(data.to.return[[d]]) = post.agg.dim
                                dimnames(data.to.return[[d]]) = post.agg.dimnames
                                
                            } else if (scale %in% c('rate', 'time', 'proportion')) {
                                
                                # We'll do weighted averages with a denominator value as weight.
                                denominator.outcome = outcome.info[['denominator.outcome']]
                                
                                if (is.null(target.ontology))
                                    denominator.ontology = strat.dimnames
                                else {
                                    if (allow.mapping.from.target.ontology)
                                        denominator.ontology = common.ontology
                                    else
                                        denominator.ontology = target.ontology
                                }
                                
                                # Recursive call to pull -- might not find anything
                                denominator.array = self$pull(outcome = denominator.outcome,
                                                              keep.dimensions = names(pre.agg.dimnames),
                                                              dimension.values = dimension.values,
                                                              sources = x,
                                                              target.ontology = denominator.ontology,
                                                              allow.mapping.from.target.ontology = FALSE,
                                                              from.ontology.names = NULL,
                                                              append.attributes = NULL,
                                                              na.rm = na.rm)
                                
                                # If no denominator data found, break from the loops for data type, stratification, and ontology and return NULL for the whole source
                                if (is.null(denominator.array)) {
                                    source.lacks.denominator.data.flag = TRUE
                                    data.to.return = NULL
                                    break
                                }
                                
                                # Since the denominator.array came from only one source, we can remove the source so that it will match size of data
                                denominator.array = array(denominator.array,
                                                          dim = dim(denominator.array)[names(dim(denominator.array)) != 'source'],
                                                          dimnames = dimnames(denominator.array)[names(dimnames(denominator.array)) != 'source']
                                )
                                
                                # Catch an otherwise invisible bug if denominator.array somehow doesn't have the same shape/order as the data
                                if (!identical(dimnames(denominator.array), dimnames(data.to.return[[d]])))
                                    stop(paste0(error.prefix, 'bug in aggregation code: denominator array has incorrect dimensions'))
                                
                                # We should find totals by aggregating the denominator.array rather than pulling less stratified data
                                # because less stratified data might not equal the sum of the more stratified data in denominator.array
                                denominator.totals.array = apply(denominator.array, keep.dimensions, FUN = sum, na.rm=na.rm)
                                dim(denominator.totals.array) = post.agg.dim
                                dimnames(denominator.totals.array) = post.agg.dimnames
                                
                                # Generate an array that multiplies every cell in data.to.return[[d]] by its weight from denominator.array
                                weighted.value.array = data.to.return[[d]] * denominator.array
                                
                                # Take the sum of the weighted values
                                data.to.return[[d]] = apply(weighted.value.array, keep.dimensions, FUN = sum, na.rm=na.rm)
                                dim(data.to.return[[d]]) = post.agg.dim
                                dimnames(data.to.return[[d]]) = post.agg.dimnames
                                
                                # Divide by the denominator.totals.array values to finish the weighted average
                                data.to.return[[d]] = data.to.return[[d]] / denominator.totals.array
                                
                            } else if (scale == 'ratio') {
                                stop(paste0(error.prefix, scale, ' data cannot be aggregated'))
                            }
                            
                            else {
                                
                                stop(paste0(error.prefix, 'aggregating with the ', scale, ' scale is not yet implemented'))
                                
                            }
                            
                        } else {
                            
                            data.to.return[[d]] = apply(data.to.return[[d]],
                                                        keep.dimensions,
                                                        function(x) {
                                                            list(unique(unlist(x)))
                                                        })
                            
                            dim(data.to.return[[d]]) = post.agg.dim
                            dimnames(data.to.return[[d]]) = post.agg.dimnames
                            
                            # fix apply's annoying behavior
                            data.data = lapply(data.to.return[[d]],
                                               function(x) {x[[1]]})
                            
                            data.to.return[[d]] = array(data.data,
                                                        dim = post.agg.dim,
                                                        dimnames = post.agg.dimnames)
                        }
                    }
                    
                    # There are too many places where the data's dimnames could be flipped between being an ontology or just a list. So right here, I should enforce that it's forevermore just a list because ontologies cause issues, such as with concatenating an ontology with a list to get new dimnames.
                    dimnames(data.to.return[[d]]) = as.list(dimnames(data.to.return[[d]]))
                    
                    
                } # end of loop for data types
                
                # # If we end up with only NA, erase data.to.return and try the next stratification
                # if (all(is.na(data.to.return[['data']]))) {
                #     data.to.return = NULL
                #     next
                # }
                
                ### SUCCESS FOR THIS ONTOLOGY ### -- only now that we still have data after mapping and aggregation can we save what we found
                
                # Save the target.to.common.mapping if we discovered one, and the mapped ontology as the common ontology
                if (!is.null(target.to.common.mapping.placeholder)) {
                    target.to.common.mapping <<- target.to.common.mapping.placeholder
                    common.ontology <<- common.ontology.placeholder
                }
                
                # If we don't have a target.ontology, save this stratification's dimnames (as an ontology) as target.ontology so that subsequent sources must conform to it
                if (is.null(target.ontology)) {
                    target.ontology <<- strat.dimnames
                    allow.mapping.from.target.ontology <<- FALSE
                    target.represents.successful.stratification.flag <<- TRUE
                }
                
                # Mark this source as a success so that we can subset easier later (when some sources will have return data and others NULL)
                sources.successful.names <<- c(sources.successful.names, x)
                
                # If the pull() was called without keep.dimensions defined, use the keep.dimensions of this successful pull as keep.dimensions for other sources
                keep.dimensions <<- keep.dimensions
                need.to.set.keep.dimensions.flag <<- FALSE
                
                # If we found a match, or lack denominator data, then we won't search any more stratifications for this ontology
                break
                
            } # end of loop for stratification
            
            # If we found a match, or lack denominator data, then we won't search any more ontologies for this source
            if (!is.null(data.to.return) || source.lacks.denominator.data.flag) break
            
        } # end of loop for ontology
        data.to.return
        
        
    }) # end of lapply for sources
    
    # we have a list (one element per source) of lists (one element per data type, i.e. 'data', 'url', or 'details')
    # repackage this to be a data array with 'url', 'details' and possibly a mapping as attributes
    final.return = NULL
    # Some sources may have returned NULL above and should be removed.
    return.data = return.data[!unlist(lapply(return.data, is.null))]
    
    # Extract data for data, url, and details out of what lapply returned above
    if (length(return.data) > 0) {
        for (data.type in names(return.data[[1]])) {
            
            # make a list of the data from the sources
            pull.return.data.list = lapply(return.data, function(x) {x[[data.type]]})
            names(pull.return.data.list) = sources.successful.names
            dim.names.pull = c(dimnames(pull.return.data.list[[1]]), list(source=sources.successful.names))
            
            pull.return.data = NULL
            
            if (data.type == 'data') {
                pull.return.data = sapply(pull.return.data.list, function(x) {x})
                
            } else {
                for (src.data in pull.return.data.list) {
                    pull.return.data = append(pull.return.data, src.data)
                }
            }
            
            dim(pull.return.data) = sapply(dim.names.pull, length)
            dimnames(pull.return.data) = dim.names.pull
            
            # incorporate it into final.return
            if (data.type == 'data') {
                final.return = pull.return.data
            } else if (data.type == 'url') {
                attr(final.return, 'url') = pull.return.data
            } else if (data.type == 'details') {
                attr(final.return, 'details') = pull.return.data
            }
            
        }
    }
    
    ###     WARNING: UNTESTED ####
    ## New feature: add NA data for unsuccessful sources if include.sources.without.data.in.output flag is TRUE
    # if (include.sources.without.data.in.output) {
    #     replacement.arr.dimnames = dimnames(final.return)
    #     replacement.arr.dimnames[['source']] = sources.used.names
    #     replacement.arr.vector = sapply(sources.used.names, function(source.name) {
    #         if (source.name %in% sources.successful.names)
    #             final.return[get.array.access.indices(dimnames(final.return), dimension.values = list(source = source.name))]
    #         else
    #             rep(NA, prod(sapply(replacement.arr.dimnames)) / length(sources.used.names))
    #     })
    #     final.return = array(replacement.arr.vector, dim = sapply(replacement.arr.dimnames, length), dimnames = replacement.arr.dimnames)
    # }
    
    
    # add mapping
    if (!is.null(target.to.common.mapping))
        attr(final.return, 'mapping') = target.to.common.mapping
    
    #-- Return --#
    # if (is.null(final.return)) {stop("I failed to pull!!")}
    final.return
}
    
