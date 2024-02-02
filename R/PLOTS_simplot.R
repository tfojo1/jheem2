

#'@param ... One or more of either (1) jheem.simulation objects or (2) jheem.simset objects or (3) lists containing only jheem.simulation or jheem.simset objects
#'@param outcomes A character vector of which simulation outcomes to plot
#'@param split.by AZ: at most one dimension
#'@param facet.by AZ: any number of dimensions but cannot include the split.by dimension
#'@param dimension.values
#'@param plot.which Should only simulation data or only calibration data be plotted, or both
#'@param data.manager The data.manager from which to draw real-world data for the plots
#'@param style.manager We are going to have to define this down the road. It's going to govern how we do lines and sizes and colors. For now, just hard code those in, and we'll circle back to it
#'
#'@details Returns a ggplot object:
#'  - With one panel for each combination of outcome x facet.by
#'  - x-axis is year
#'  - y-axis is outcome
#'
#'@export
simplot <- function(...,
                    outcomes = NULL,
                    corresponding.data.outcomes = NULL,
                    split.by = NULL,
                    facet.by = NULL,
                    dimension.values = list(),
                    plot.which = c('sim.only', 'data.only', 'both')['both'],
                    data.manager = get.default.data.manager(),
                    style.manager) # will be an R6 object. will be mine!
{
    ### --- FEATURES TO ADD --- ###
    # - change y-axis from 'value' to the name of the outcome? Maybe not, since "value" is shared by all plots
    # - allow changing color scheme for lines and points
    # - replace outcome names on plot with the names from outcome metadata
    # - y labels, scale may be percentage or number with commas
    # - for color schemes look at ggsci package palettes
    
    # -- VALIDATION -- #
    
    error.prefix = "Cannot generate simplot: "
    
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
    stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    # *split.by* is NULL or a single, non-NA character vector
    if (!is.null(split.by) && (!is.character(split.by) || length(split.by) > 1 || is.na(split.by)))
        stop(paste0(error.prefix, "'split.by' must be NULL or a length one, non-NA character vector"))
    
    # *facet.by* is NULL or a character vector of length > 0 with no NAs or duplicates
    if (!is.null(facet.by) && (!is.character(facet.by) || length(facet.by) < 1 || any(is.na(facet.by)) || any(duplicated(facet.by))))
        stop(paste0(error.prefix, "'facet.by' must be NULL or a character vector with at least one element and no NAs or duplicates"))
    
    if (!is.null(split.by) && split.by %in% facet.by)
        stop(paste0(error.prefix, "'facet.by' must not contain the dimension in 'split.by'"))
    
    if (!is.null(split.by) && split.by == 'year')
        stop(paste0(error.prefix, "'split.by' cannot equal 'year'"))
    
    if (!is.null(facet.by) && 'year' %in% facet.by)
        stop(paste0(error.prefix, "'facet.by' cannot contain 'year'"))
    
    if (!(identical(plot.which, 'sim.only') || identical(plot.which, 'data.only') || identical(plot.which, 'both')))
        stop(paste0(error.prefix, "'plot.which' must be one of 'sim.only', 'data.only', or 'both'"))
    
    #-- STEP 1: PRE-PROCESSING --#
    # Get a list out of ... where each element is one simset (or sim for now)
    
    simset.args = list(...) # will later be SIMSETS
    
    outcomes.found.in.simset.args = F
    # each element of 'sim.list' should be either a sim or list containing only sims.
    for (element in simset.args) {
        if (!R6::is.R6(element) || !is(element, 'jheem.simulation.set')) {
            if (is.list(element)) {
                if (any(sapply(element, function(sub.element) {!R6::is.R6(sub.element) || !is(sub.element, 'jheem.simulation.set')}))) {
                    stop(paste0(error.prefix, "arguments supplied in '...' must either be jheem.simulation.set objects or lists containing only jheem.simulation.set objects"))
                }
            } else if (is.null(outcomes) && is.character(element)) {
                outcomes = element
                outcomes.found.in.simset.args = T
            }
            else
                stop(paste0(error.prefix, "arguments supplied in '...' must either be jheem.simulation.set objects or lists containing only jheem.simulation.set objects"))
        }
    }
    
    if (!is.character(outcomes) || is.null(outcomes) || any(is.na(outcomes)) || any(duplicated(outcomes))) {
        if (outcomes.found.in.simset.args)
            stop(paste0(error.prefix, "'outcomes' found as unnamed argument in '...' must be a character vector with no NAs or duplicates"))
        else
            stop(paste0(error.prefix, "'outcomes' must be a character vector with no NAs or duplicates"))
    }
    
    if (outcomes.found.in.simset.args) {
        if (length(simset.args) < 2)
            stop(paste0(error.prefix, "one or more jheem.simulation.set objects or lists containing only jheem.simulation.set objects must be supplied"))
        else
            simset.list = simset.args[1:(length(simset.args)-1)]
    }
    else {
        if (length(simset.args) < 1)
            stop(paste0(error.prefix, "one or more jheem.simulation.set objects or lists containing only jheem.simulation.set objects must be supplied"))
        else
            simset.list = simset.args
    }

    # Now simset.list contains only simsets and lists containing only simsets. It needs to be just a single-level list of simsets now
    simset.list = unlist(simset.list, recursive = F)
    
    # - make sure they are all the same version and the location
    if (length(unique(sapply(simset.list, function(simset) {simset$version}))) > 1)
        stop(paste0(error.prefix, "all simulation sets must have the same version"))
    if (length(unique(sapply(simset.list, function(simset) {simset$location}))) > 1)
        stop(paste0(error.prefix, "all simulation sets must have the same location"))
    
    # Check outcomes
    # - make sure each outcome is present in sim$outcomes for at least one sim/simset
    if (any(sapply(outcomes, function(outcome) {!any(sapply(simset.list, function(simset) {outcome %in% simset$outcomes}))})))
        stop(paste0("There weren't any simulation sets for one or more outcomes. Should this be an error?"))
    
    # Get the real-world outcome names
    # - eventually we're going to want to pull this from info about the likelihood if the sim notes which likelihood was used on it
    # - what we'll do now will be the back-up to above
    #   sim$outcome.metadata[[outcome]]$corresponding.observed.outcome
    # sims do not all have each outcome because of sub-versions
    
    # likelihoods need to share their outcome for sim and data, and think about what joint likelihoods. One simulation has one (usually joint) likelihood (instructions)
    # browser()
    outcomes.for.data = sapply(outcomes, function(outcome) {
        if (outcome %in% names(corresponding.data.outcomes))
            return(corresponding.data.outcomes[[outcome]])
        corresponding.observed.outcome = NULL
        i = 1
        while (i <= length(simset.list)) {
            if (outcome %in% names(simset.list[[i]]$outcome.metadata)) {
                corresponding.observed.outcome = simset.list[[i]]$outcome.metadata[[outcome]]$corresponding.observed.outcome
                break
            } else i = i + 1
        }
        corresponding.observed.outcome
    })
    
    outcome.ontologies = lapply(outcomes, function(outcome) {
        outcome.ontology = NULL
        i = 1
        while (i <= length(simset.list)) {
            if (outcome %in% names(simset.list[[i]]$outcome.ontologies)) {
                outcome.ontology = simset.list[[i]]$outcome.ontologies[[outcome]]
                break
            } else i = i + 1
        }
        if (is.null(outcome.ontology))
            stop(paste0("No outcome ontology found for outcome '", outcome, "'")) # Shouldn't happen
        outcome.ontology
    })
    
    # Get the locations to pull data for for each outcome
    # - for now, just use the sim$location
    location = simset.list[[1]]$location
    
    #-- STEP 2: MAKE A DATA FRAME WITH ALL THE REAL-WORLD DATA --#

    outcome.mappings = list() # note: not all outcomes will have corresponding data outcomes
    
    df.truth = NULL
    for (i in seq_along(outcomes.for.data))
    {
        if (plot.which != 'sim.only' && !is.null(outcomes.for.data[[i]]))
        {
            outcome.data = tryCatch(
                {
                    result = data.manager$pull(outcome = outcomes.for.data[[i]],
                                               dimension.values = c(dimension.values, list(location = location)),
                                               keep.dimensions = c('year', facet.by, split.by), #'year' can never be in facet.by
                                               target.ontology = outcome.ontologies[[i]],
                                               allow.mapping.from.target.ontology = T,
                                               na.rm=T,
                                               debug=F)
                },
                error = function(e) {
                    NULL
                }
            )
            outcome.mappings = c(outcome.mappings, list(attr(outcome.data, 'mapping')))
            if (!is.null(outcome.data)) {
                
                # If we have multiple outcomes that may map differently (for example, with years), the factor levels unavoidably determined by the first outcome for reshape2::melt may not be valid for subsequent outcomes
                one.df.outcome = reshape2::melt(outcome.data, na.rm = T)
                one.df.outcome = as.data.frame(lapply(one.df.outcome, function(col) {
                    if (is.factor(col)) as.character(col)
                    else col
                }))
                
                corresponding.outcome = names(outcomes.for.data)[[i]]
                one.df.outcome['outcome'] = corresponding.outcome
                df.truth = rbind(df.truth, one.df.outcome)
            }
        }
        else
        {
            outcome.mappings = c(outcome.mappings, list(NULL))
        }
    }

    names(outcome.mappings) = outcomes

    #-- STEP 3: MAKE A DATA FRAME WITH ALL THE SIMULATIONS' DATA --#

    df.sim = NULL
    if (plot.which != 'data.only') {
        for (outcome in outcomes) {
            
            # Determine the keep.dimensions we need from the sim$gets
            extra.dimensions.needed.for.mapping = c()
            if (plot.which != 'sim.only' && !is.null(outcome.mappings[[outcome]])) {
                extra.dimensions.needed.for.mapping = setdiff(outcome.mappings[[outcome]]$from.dimensions, c(facet.by, split.by))
                keep = union(c('year', facet.by, split.by), extra.dimensions.needed.for.mapping)
            }
            else keep = c('year', facet.by, split.by)
            
            for (i in seq_along(simset.list)) {
                
                simset = simset.list[[i]]
                simset.data.this.outcome = simset$get(outcomes = outcome,
                                                      dimension.values = dimension.values,
                                                      keep.dimensions = keep,
                                                      drop.single.outcome.dimension = T)
                if (plot.which != 'sim.only' && !is.null(outcome.mappings[[outcome]])) {
                    simset.data.mapped.this.outcome = tryCatch(
                        {outcome.mappings[[outcome]]$apply(simset.data.this.outcome)},
                        error = function(e) {NULL}
                    )
                } else simset.data.mapped.this.outcome = simset.data.this.outcome
                
                if (is.null(simset.data.mapped.this.outcome)) next
                
                # Aggregate out the extra dimensions we may have gotten for the mapping
                simset.data.mapped.this.outcome = apply(simset.data.mapped.this.outcome, setdiff(names(dim(simset.data.mapped.this.outcome)), extra.dimensions.needed.for.mapping), sum, na.rm=T)
                
                # If we have multiple outcomes that may map differently (for example, with years), the factor levels unavoidably determined by the first outcome for reshape2::melt may not be valid for subsequent outcomes
                one.df.sim.this.outcome = reshape2::melt(simset.data.mapped.this.outcome, na.rm = T)
                one.df.sim.this.outcome = as.data.frame(lapply(one.df.sim.this.outcome, function(col) {
                    if (is.factor(col)) as.character(col)
                    else col
                }))
                
                one.df.sim.this.outcome['simset'] = i
                one.df.sim.this.outcome['outcome'] = outcome
                one.df.sim.this.outcome['linewidth'] = 1/sqrt(simset$n.sim) # have style manager create this later?
                one.df.sim.this.outcome['alpha'] = 20 * one.df.sim.this.outcome['linewidth'] # same comment as above
                
                df.sim = rbind(df.sim, one.df.sim.this.outcome)
            }
            
        }
    }
    
    # browser()
    if (!is.null(df.sim)) {
        df.sim$simset = factor(df.sim$simset)
        df.sim$sim = factor(df.sim$sim)
        df.sim$groupid = paste0(df.sim$outcome, '_', df.sim$simset, '_', df.sim$sim, '_', df.sim[,split.by])
        
        # break df.sim into two data frames, one for outcomes where the sim will be lines and the other for where it will be points
        groupids.with.one.member = setdiff(unique(df.sim$groupid), df.sim$groupid[which(duplicated(df.sim$groupid))])
        df.sim$groupid_has_one_member = with(df.sim, groupid %in% groupids.with.one.member)
        df.sim.groupids.one.member = subset(df.sim, groupid_has_one_member)
        df.sim.groupids.many.members = subset(df.sim, !groupid_has_one_member)
    }
    
    
    #- STEP 4: MAKE THE PLOT --#
    
    y.label = paste0(sapply(outcomes, function(outcome) {simset.list[[1]][['outcome.metadata']][[outcome]][['units']]}), collapse='/')
    
    facet.formula = as.formula(paste0("~",
                                      paste0(c('outcome', facet.by), collapse='+')))
    
    rv = ggplot() + scale_y_continuous(limits=c(0, NA), labels = scales::comma) + scale_color_discrete()

    # how data points are plotted is conditional on 'split.by', but the facet_wrap is not
    if (!is.null(split.by)) {
        if (!is.null(df.sim)) {
            rv = rv + geom_line(data=df.sim.groupids.many.members, aes(x=year, y=value, linetype=simset, group=groupid, color=!!sym(split.by), alpha=alpha, linewidth=linewidth)) +
                geom_point(data=df.sim.groupids.one.member, size=2, aes(x=year, y=value, shape=simset, color=!!sym(split.by)))
        }
        if (!is.null(df.truth))
            rv = rv + geom_point(data=df.truth, aes(x=year, y=value, color=!!sym(split.by)))
    } else {
        if (!is.null(df.sim)) {
            rv = rv + geom_line(data=df.sim.groupids.many.members, aes(x=year, y=value, linetype=simset, group=groupid, alpha=alpha, linewidth=linewidth)) +
                geom_point(data=df.sim.groupids.one.member, size=2, aes(x=year, y=value, shape=simset))
        }
        if (!is.null(df.truth))
            rv = rv + geom_point(data=df.truth, aes(x=year, y=value))
    }
    
    if (!is.null(facet.by) || length(outcomes) > 1) {
        rv = rv + facet_wrap(facet.formula, scales = 'free_y', )
    }
    
    rv = rv +
        scale_alpha(guide='none') +
        labs(y=y.label)
    if (!is.null(df.sim)) rv = rv + scale_linewidth(NULL, range=c(min(df.sim$linewidth), 1), guide = 'none')
    
    rv
}