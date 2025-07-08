# Status: right now, this is finding a lot of percent diff of -1 due to the sub strat marginals being 0. How to handle?
inspect_marginals = function(threshold.percent = 0.1,
                             outcome=NULL,
                             source=NULL,
                             ontology=NULL,
                             sub.stratification=NULL,
                             super.stratification=NULL) {
    
    # For each outcome/metric/source/ontology, check each stratification against the others for correct marginals
    # Only for strats that are subsidiary to others, though (like "age" vs. "age__sex")
    # I call the more stratified strat the "sub" and the aggregate the "super" in a given pair
    error.prefix = "Error inspecting marginals: "
    if (!is.numeric(threshold.percent) || length(threshold.percent)!=1 || is.na(threshold.percent) || threshold.percent<0 || threshold.percent>1)
        stop(paste0(error.prefix, "'threshold.percent' must be a single, non-NA numeric value"))
    
    usable_outcomes <- names(private$i.outcome.info)[sapply(private$i.outcome.info, function(outcome) {outcome$metadata$scale == "non.negative.number"})]
    usable_outcomes <- intersect(names(private$i.data), usable_outcomes)
    
    ## CASE 1: Inspect marginals for all outcomes
    if (is.null(outcome) && is.null(source) && is.null(ontology) && is.null(sub.stratification) && is.null(super.stratification)) {
        rv = private$do_inspect_all_marginals(threshold.percent=threshold.percent,
                                              usable.outcomes = usable_outcomes,
                                              error.prefix=error.prefix)
    }
    ## CASE 2: Inspect marginals between two specified arrays
    else {
        if (!is.character(outcome) || length(outcome) > 1 || is.na(outcome))
            stop(paste0(error.prefix, "'outcome' must be a single, non-NA character value"))
        if (!(outcome %in% names(private$i.outcome.info)))
            stop(paste0(error.prefix, "'", outcome, "' is not a registered outcome."))
        if (!(outcome %in% usable_outcomes))
            stop(paste0(error.prefix, "'outcome' must have scale 'non.negative.number'"))
        if (is.null(private$i.data[[outcome]][['estimate']][[source]][[ontology]][[sub.stratification]]))
            stop(paste0(error.prefix, "data must exist in the data manager for this 'outcome', 'source', 'ontology' and 'sub.stratification'"))
        if (is.null(private$i.data[[outcome]][['estimate']][[source]][[ontology]][[super.stratification]]))
            stop(paste0(error.prefix, "data must exist in the data manager for this 'outcome', 'source', 'ontology' and 'super.stratification'"))
        rv = private$do_inspect_particular_marginals(outcome=outcome,
                                                     source=source,
                                                     ontology=ontology,
                                                     sub.stratification=sub.stratification,
                                                     super.stratification = super.stratification,
                                                     error.prefix=error.prefix)
    }
    rv
    
}

do_inspect_all_marginals = function(threshold.percent,
                                    usable.outcomes,
                                    error.prefix) {
    return_all_outcomes <- lapply(usable.outcomes, function(outcome) {
        return_this_outcome <- lapply(names(private$i.data[[outcome]][["estimate"]]), function(source) {
            return_this_source <- lapply(names(private$i.data[[outcome]][["estimate"]][[source]]), function(ontology) {
                # browser()
                strat_names <- names(private$i.data[[outcome]][["estimate"]][[source]][[ontology]])
                return_this_ontology <- sapply(strat_names, function(super_strat) {
                    sapply(strat_names, function(sub_strat) {
                        
                        percent_discrepancies <- private$do_inspect_particular_marginals(outcome=outcome,
                                                                                         source=source,
                                                                                         ontology=ontology,
                                                                                         sub.stratification = sub_strat,
                                                                                         super.stratification = super_strat,
                                                                                         error.prefix=error.prefix)
                        if (is.null(percent_discrepancies))
                            return(NA)
                        if (max(percent_discrepancies, na.rm=T) <= threshold.percent && abs(min(percent_discrepancies, na.rm=T)) <= threshold.percent)
                            return(NA)
                        
                        if (max(percent_discrepancies, na.rm=T) > abs(min(percent_discrepancies, na.rm=T)))
                            max(percent_discrepancies, na.rm=T)
                        else
                            min(percent_discrepancies, na.rm=T)
                    })
                })
                return_this_ontology <- as.matrix(return_this_ontology)
                dimnames(return_this_ontology) <- list(sub_strat=strat_names, super_strat=strat_names)
                
                # Remove rows and columns that are all NA
                keep_rows <- apply(return_this_ontology, 1, function(row) {!all(is.na(row))})
                keep_cols <- apply(return_this_ontology, 2, function(col) {!all(is.na(col))})
                return_this_ontology <- return_this_ontology[keep_rows, keep_cols, drop=F]
                if (length(return_this_ontology)==0)
                    return_this_ontology <- NA
                
                return_this_ontology
                
            })
            names(return_this_source) <- names(private$i.data[[outcome]][["estimate"]][[source]])
            return_this_source <- return_this_source[!is.na(return_this_source)]
            if (length(return_this_source)==0)
                return_this_source <- NA
            
            return_this_source
            
        })
        names(return_this_outcome) <- names(private$i.data[[outcome]][["estimate"]])
        return_this_outcome <- return_this_outcome[!is.na(return_this_outcome)]
        if (length(return_this_outcome)==0)
            return_this_outcome <- NA
        
        return_this_outcome
    })
    names(return_all_outcomes) <- usable.outcomes
    return_all_outcomes <- return_all_outcomes[!is.na(return_all_outcomes)]
    if (length(return_all_outcomes)==0)
        return_all_outcomes <- NA
    
    return_all_outcomes
},

do_inspect_particular_marginals = function(outcome,
                                           source,
                                           ontology,
                                           sub.stratification,
                                           super.stratification,
                                           error.prefix) {
    # If the same, return NA
    if (super.stratification == sub.stratification)
        return(NULL)
    
    # Break the strats into their dimensions
    super_dimensions <- unlist(strsplit(super.stratification, "__"))
    sub_dimensions <- unlist(strsplit(sub.stratification, "__"))
    
    # If the super is not actually super to the sub, return NA
    if (length(setdiff(sub_dimensions, super_dimensions))==0 || length(setdiff(super_dimensions, sub_dimensions)!=0))
        return(NULL)
    
    # If there are not any values to compare, return NA
    shared_dimnames <- intersect.shared.dim.names(dimnames(private$i.data[[outcome]][["estimate"]][[source]][[ontology]][[sub.stratification]]),
                                                  dimnames(private$i.data[[outcome]][["estimate"]][[source]][[ontology]][[super.stratification]]))
    if (any(sapply(shared_dimnames, length)==0))
        return(NULL)
    
    # Else, find marginals of sub and compare to super
    sub_data <- array.access(private$i.data[[outcome]][["estimate"]][[source]][[ontology]][[sub.stratification]], shared_dimnames)
    super_data <- array.access(private$i.data[[outcome]][["estimate"]][[source]][[ontology]][[super.stratification]], shared_dimnames)
    sub_marginals <- apply(sub_data, MARGIN=names(dim(super_data)), FUN=sum, na.rm=T)
    
    # Verify that the dimnames are the same now (values should be ordered within a dimension, but you never know...)
    if (!identical(dimnames(super_data), dimnames(sub_marginals)))
        stop(paste0("Error inspecting totals: dimension value order assumption broken! Contact Andrew"))
    
    # Return the percentage discrepancy
    percent_discrepancies <- (sub_marginals - super_data)/super_data
}