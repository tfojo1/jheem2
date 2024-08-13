pull = function(data.manager,
                outcome,
                keep.dimensions,
                dimension.values = NULL,
                target.ontology,
                debug = F)
{
    
    # ASSUMPTIONS:
    # 1) always have a valid target ontology
    # 2) always have keep.dimensions
    # 3) never needs aggregation, meaning dimension.values dimensions are always included in keep.dimensions
    # 4) a common mapping is desired
    
    if (!(names(dimensions.values) %in% keep.dimensions))
        stop("This pull doesn't have aggregation implemented")
    
    if (debug) browser()
    
    target.to.common.mapping = NULL
    common.ontology = NULL
    
    sources.used.names = names(private$i.data[[outcome]])
    
    sources.successful.names = c()
    
    return.data = lapply(sources.used.names, function(x) {
        
        ontologies.used.names = names(private$i.data[[outcome]][[x]])
        
        data.to.return = NULL
        
        for (y in ontologies.used.names) {
            
            ont = private$i.ontologies[[y]]
            
            if (is.null(resolved.dimension.values) && !is.null(dimension.values)) next
            
            stratification.names = names(private$i.data[[outcome]][[x]][[y]])
            
            for (strat in stratification.names) {
                
                strat.data = private$i.data[[outcome]][[x]][[y]][[strat]]
                strat.dimensions = names(dim(strat.data))
                strat.dimnames = as.ontology(dimnames(strat.data), incomplete.dimensions = intersect(incomplete.dimensions(ont), strat.dimensions))
                
                if (all(is.na(strat.data))) next
                
                strat.to.common.mapping = NULL
                target.to.common.mapping.placeholder = NULL
                
                
                if (!is.null(common.ontology)) {
                    strat.to.common.mapping = get.ontology.mapping(strat.dimnames, common.ontology)
                } else {
                    aligning.mappings.list = get.mappings.to.align.ontologies(strat.dimnames, target.ontology)
                    if (!is.null(aligning.mappings.list)) {
                        strat.to.common.mapping = aligning.mappings.list[[1]]
                        target.to.common.mapping.placeholder = aligning.mappings.list[[2]]
                    }
                }
                
                if (is.null(strat.to.common.mapping)) next
                
                dimnames.for.apply = NULL
                dimnames.for.apply = target.ontology
                dimnames.for.apply[names(dimension.values)] = dimension.values
                dimnames.for.apply = dimnames.for.apply[names(dimnames.for.apply) %in% union(keep.dimensions, names(dimension.values))]
                
                if (is.null(target.to.common.mapping)) {
                    if (!target.to.common.mapping.placeholder$can.apply.to.dim.names(dimnames.for.apply)) next
                    else dimnames.for.apply = target.to.common.mapping.placeholder$apply.to.dim.names(dimnames.for.apply)
                } else {
                    if (!target.to.common.mapping$can.apply.to.dim.names(dimnames.for.apply)) next
                    else dimnames.for.apply = target.to.common.mapping$apply.to.dim.names(dimnames.for.apply)
                }
                
                if (!setequal(strat.dimensions, names(dimnames.for.apply))) next
                
                strat.missing.incomplete.dimensions.values = FALSE
                
                data.to.return = lapply(c('data'), function(a) {
                    
                    if (strat.missing.incomplete.dimensions.values) NULL
                    else {
                        
                        data.to.process = strat.data
                        
                        mapping.to.apply = strat.to.common.mapping
                        
                        mapped.dimnames = mapping.to.apply$apply.to.dim.names(strat.dimnames)
                        
                        incomplete.dimension.values = dimension.values[names(dimension.values) %in% incomplete.dimensions(target.ontology)]
                        
                        for (d in names(incomplete.dimension.values)) {
                            if (!all(dimension.values[[d]] %in% mapped.dimnames[[d]])) { # We should check against the mapped dimension.values instead
                                strat.missing.incomplete.dimensions.values <<- TRUE
                                break
                            }
                        }
                        
                        if (!mapping.to.apply$can.apply.to.dim.names(from.dim.names = data.to.process,
                                                                     to.dim.names = dimnames.for.apply,
                                                                     throw.errors = F))
                            strat.missing.incomplete.dimensions.values <<- TRUE
                        
                        if (strat.missing.incomplete.dimensions.values)
                            NULL
                        else {
                            
                            data.temp = mapping.to.apply$apply(data.to.process,
                                                               na.rm = na.rm,
                                                               to.dim.names = dimnames.for.apply,
                                                               fun = 'sum')
                        }
                        
                    }
                    
                })
                
                if (strat.missing.incomplete.dimensions.values) {
                    data.to.return = NULL
                    next
                }
                
                names(data.to.return) = 'data'
                
                if (all(is.na(data.to.return[['data']]))) {
                    data.to.return = NULL
                    next
                }
                
                if (!is.null(target.to.common.mapping.placeholder)) {
                    target.to.common.mapping <<- target.to.common.mapping.placeholder
                    common.ontology <<- strat.to.common.mapping$apply.to.ontology(strat.dimnames)
                }
                
                sources.successful.names <<- c(sources.successful.names, x)
                
                break
                
            }
            
            if (!is.null(data.to.return)) break
            
        }
        data.to.return
        
    })
    
    final.return = NULL
    
    return.data = return.data[!unlist(lapply(return.data, is.null))]
    
    if (length(return.data) > 0) {
        pull.return.data.list = lapply(return.data, function(x) {x[['data']]})
        names(pull.return.data.list) = sources.successful.names
        dim.names.pull = c(dimnames(pull.return.data.list[[1]]), list(source=sources.successful.names))
        
        pull.return.data = NULL
        pull.return.data = sapply(pull.return.data.list, function(x) {x})
        dim(pull.return.data) = sapply(dim.names.pull, length)
        dimnames(pull.return.data) = dim.names.pull
        
        final.return = pull.return.data
    }
    attr(final.return, 'mapping') = target.to.common.mapping
    final.return
    
}