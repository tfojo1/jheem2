
#'@param ... One or more of either (1) jheem.simulation objects or (2) jheem.simset objects or (3) lists containing only jheem.simulation or jheem.simset objects
#'@param outcomes A character vector of which simulation outcomes to plot
#'@param split.by AZ: at most one dimension
#'@param facet.by AZ: any number of dimensions but cannot include the split.by dimension
#'@param dimension.values
#'@param plot.which Should only simulation data or only calibration data be plotted, or both
#'@param title NULL or a single, non-NA character value. If "location", the location of the first provided simset (if any) will be used for the title.
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
                    outcomes=NULL,
                    corresponding.data.outcomes = NULL,
                    split.by = NULL,
                    facet.by = NULL,
                    dimension.values = list(),
                    target.ontology = NULL,
                    plot.which = c('both', 'sim.only', 'data.only')[1],
                    summary.type = c('individual.simulation', 'mean.and.interval', 'median.and.interval')[1],
                    plot.year.lag.ratio = F,
                    title = NULL,
                    n.facet.rows = NULL,
                    data.manager = get.default.data.manager(),
                    style.manager = get.default.style.manager(),
                    debug = F)
{
    prepared.plot.data = prepare.plot(...,
                                      outcomes=outcomes,
                                      corresponding.data.outcomes = corresponding.data.outcomes,
                                      split.by=split.by,
                                      facet.by=facet.by,
                                      dimension.values=dimension.values,
                                      target.ontology=target.ontology,
                                      plot.which=plot.which,
                                      summary.type=summary.type,
                                      plot.year.lag.ratio=plot.year.lag.ratio,
                                      title=title,
                                      data.manager=data.manager,
                                      debug=debug)
    execute.simplot(prepared.plot.data,
                    outcomes=outcomes,
                    split.by=split.by,
                    facet.by=facet.by,
                    plot.which=plot.which,
                    summary.type=summary.type,
                    plot.year.lag.ratio=plot.year.lag.ratio,
                    n.facet.rows=n.facet.rows,
                    style.manager=style.manager,
                    debug=debug)
}

prepare.plot <- function(...,
                         outcomes=NULL,
                         corresponding.data.outcomes = NULL,
                         split.by = NULL,
                         facet.by = NULL,
                         dimension.values = list(),
                         target.ontology = NULL,
                         plot.which = c('both', 'sim.only', 'data.only')[1],
                         summary.type = c('individual.simulation', 'mean.and.interval', 'median.and.interval')[1],
                         plot.year.lag.ratio = F,
                         title=NULL,
                         data.manager = get.default.data.manager(),
                         debug = F)
{
    # -- VALIDATION -- #
    if (debug) browser()
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

    if (!is.null(target.ontology) &&
        !is.ontology(target.ontology) &&
        !(is.list(target.ontology) && all(sapply(target.ontology, function(x) {is.ontology((x))})) && !is.null(names(target.ontology))))
        stop(paste0(error.prefix, "'target.ontology' must be NULL, an ontology, or a list of ontologies with outcomes as names"))
    
    if (!(identical(plot.which, 'sim.only') || identical(plot.which, 'data.only') || identical(plot.which, 'both')))
        stop(paste0(error.prefix, "'plot.which' must be one of 'sim.only', 'data.only', or 'both'"))
    
    if (!(identical(summary.type, 'individual.simulation') || identical(summary.type, 'mean.and.interval') || identical(summary.type, 'median.and.interval')))
        stop(paste0(error.prefix, "'summary.type' must be one of 'individual.simulation', 'mean.and.interval', or 'median.and.interval'"))
    
    if (!identical(plot.year.lag.ratio, T) && !identical(plot.year.lag.ratio, F))
        stop(paste0(error.prefix, "'plot.year.lag.ratio' must be either T or F"))
    
    if (!is.null(title) && (!is.character(title) || length(title)!=1 || is.na(title)))
        stop(paste0(error.prefix, "'title' must be NULL or a single, non-NA character value"))
    
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
    
    # *corresponding.data.outcomes' is NULL or a vector with outcomes as names
    if (!is.null(corresponding.data.outcomes) && (!is.character(corresponding.data.outcomes) || any(is.na(corresponding.data.outcomes)) || is.null(names(corresponding.data.outcomes)) || !all(names(corresponding.data.outcomes) %in% outcomes)))
        stop(paste0(error.prefix, "'corresponding.data.outcomes' must be NULL or a character vector with outcomes as names and all of those outcomes specified in either the 'outcomes' argument or in '...'"))
    
    # if *plot.year.lag.ratio* is true, we can have only one outcome
    if (plot.year.lag.ratio && length(outcomes)>1)
        stop(paste0(error.prefix, "only one outcome can be used with 'plot.year.lag.ratio'"))
    
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
    # browser()
    outcome.display.names = list()
    for (outcome in outcomes) {
        display.name = outcome
        i = 1
        while (i <= length(simset.list)) {
            if (outcome %in% names(simset.list[[i]]$outcome.metadata)) {
                display.name = simset.list[[i]]$outcome.metadata[[outcome]]$display.name
                break
            } else i = i + 1
        }
        outcome.display.names[outcome] = display.name
    }
    
    outcome.ontologies = lapply(outcomes, function(outcome) {
        if (is.null(target.ontology) || FALSE)
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
    
    outcome.locations = lapply(outcomes, function(outcome) {
        locations.this.outcome = unique(unlist(lapply(simset.list, function(simset) {
            simset$outcome.location.mapping$get.observed.locations(outcome, simset$location)
        })))
    })
    names(outcome.locations) = outcomes.for.data
    # browser()
    
    #-- STEP 2: MAKE A DATA FRAME WITH ALL THE REAL-WORLD DATA --#
    
    outcome.mappings = list() # note: not all outcomes will have corresponding data outcomes
    # browser()
    df.truth = NULL
    for (i in seq_along(outcomes.for.data))
    {
        if (plot.which != 'sim.only' && !is.null(outcomes.for.data[[i]]))
        {
            outcome.data = tryCatch(
                {
                    if (!is.null(target.ontology) && !is.list(target.ontology))
                        result = data.manager$pull(outcome = outcomes.for.data[[i]],
                                                   dimension.values = c(dimension.values, list(location = outcome.locations[[i]])),
                                                   keep.dimensions = c('year', 'location', facet.by, split.by), #'year' can never be in facet.by
                                                   target.ontology = target.ontology,
                                                   allow.mapping.from.target.ontology = F,
                                                   na.rm=T,
                                                   debug=F)
                    else if (is.list(target.ontology) && outcomes.for.data[[i]] %in% names(target.ontology))
                        result = data.manager$pull(outcome = outcomes.for.data[[i]],
                                                   dimension.values = c(dimension.values, list(location = outcome.locations[[i]])),
                                                   keep.dimensions = c('year', 'location', facet.by, split.by), #'year' can never be in facet.by
                                                   target.ontology = target.ontology[[outcomes.for.data[[i]]]],
                                                   allow.mapping.from.target.ontology = F,
                                                   na.rm=T,
                                                   debug=F)
                    else
                        result = data.manager$pull(outcome = outcomes.for.data[[i]],
                                                   dimension.values = c(dimension.values, list(location = outcome.locations[[i]])),
                                                   keep.dimensions = c('year', 'location', facet.by, split.by), #'year' can never be in facet.by
                                                   target.ontology = outcome.ontologies[[i]],
                                                   allow.mapping.from.target.ontology = T,
                                                   na.rm=T,
                                                   debug=F)
                    
                },
                error = function(e) {
                    NULL
                }
            )
            if (!is.null(attr(outcome.data, 'mapping')))
                outcome.mappings = c(outcome.mappings, list(attr(outcome.data, 'mapping')))
            else
                outcome.mappings = c(outcome.mappings, list(NULL))
            if (!is.null(outcome.data)) {
                
                # If the scale is proportion, multiply data by 100 to match the "%" symbol the label will have
                if (data.manager$outcome.info[[outcomes.for.data[[i]]]]$metadata$display.as.percent)
                    outcome.data = outcome.data * 100
                
                # If we have multiple outcomes that may map differently (for example, with years), the factor levels unavoidably determined by the first outcome for reshape2::melt may not be valid for subsequent outcomes
                one.df.outcome = reshape2::melt(outcome.data, na.rm = T)
                one.df.outcome = as.data.frame(lapply(one.df.outcome, function(col) {
                    if (is.factor(col)) as.character(col)
                    else col
                }))
                
                corresponding.outcome = names(outcomes.for.data)[[i]]
                one.df.outcome['outcome'] = corresponding.outcome
                one.df.outcome['outcome.display.name'] = outcome.display.names[corresponding.outcome]
                df.truth = rbind(df.truth, one.df.outcome)
            }
        }
        else
        {
            outcome.mappings = c(outcome.mappings, list(NULL))
        }
    }
    if (!is.null(df.truth)) {
        # make whatever column corresponds to split by actually be called "stratum" and same for facet.by.
        if (!is.null(split.by)) names(df.truth)[names(df.truth)==split.by] = "stratum"
        if (!is.null(facet.by)) names(df.truth)[names(df.truth)==facet.by] = "facet.by"
        
        # if there is no 'stratum' because no split, then we should fill it with ""
        if (!('stratum' %in% names(df.truth))) df.truth['stratum'] = rep('', nrow(df.truth))
        
        # sort the split.by column alphabetically so that when we assign colors, it will be the same for sim.
        if (!is.null(split.by))
            df.truth = df.truth[order(df.truth$stratum),]
    }
    names(outcome.mappings) = outcomes
    
    #-- STEP 3: MAKE A DATA FRAME WITH THE SIMULATION DATA --#
    
    df.sim = NULL
    if (plot.which != 'data.only') {
        for (outcome in outcomes) {
            
            keep.dimensions = c('year', facet.by, split.by)
            for (i in seq_along(simset.list)) {
                
                simset = simset.list[[i]]
                if (!is.null(outcome.mappings[[outcome]])) mapping.this.outcome = outcome.mappings[[outcome]] # case when target ontology is NULL or is a list but doesn't include this outcome
                else if (is.list(target.ontology)) mapping.this.outcome = get.ontology.mapping(outcome.ontologies[[i]], target.ontology[[outcome]])
                else if (!is.null(target.ontology)) mapping.this.outcome = get.ontology.mapping(outcome.ontologies[[i]], target.ont)
                else mapping.this.outcome = NULL
                # browser()
                simset.data.this.outcome = simset$get(outcomes = outcome,
                                                      dimension.values = dimension.values,
                                                      keep.dimensions = keep.dimensions,
                                                      drop.single.outcome.dimension = T,
                                                      mapping=mapping.this.outcome,
                                                      summary.type = summary.type)
                
                if (is.null(simset.data.this.outcome)) next
                
                # If the scale is proportion, multiply data by 100 to match the "%" symbol the label will have
                if (simset.list[[i]][['outcome.metadata']][[outcome]]$display.as.percent)
                    simset.data.this.outcome = simset.data.this.outcome * 100
                
                # If we have multiple outcomes that may map differently (for example, with years), the factor levels unavoidably determined by the first outcome for reshape2::melt may not be valid for subsequent outcomes
                one.df.sim.this.outcome = reshape2::melt(simset.data.this.outcome, na.rm = T)
                one.df.sim.this.outcome = as.data.frame(lapply(one.df.sim.this.outcome, function(col) {
                    if (is.factor(col)) as.character(col)
                    else col
                }))
                
                one.df.sim.this.outcome['simset'] = i
                one.df.sim.this.outcome['outcome'] = outcome
                one.df.sim.this.outcome['linewidth'] = 1/sqrt(simset$n.sim) # have style manager create this later?
                one.df.sim.this.outcome['alpha'] = one.df.sim.this.outcome['linewidth'] # same comment as above; USED to be 20 * this
                
                # Make a "outcome.long.name" column so that the facet.by can present it instead of the short name
                one.df.sim.this.outcome['outcome.display.name'] = simset.list[[i]]$outcome.metadata[[outcome]]$display.name
                
                df.sim = rbind(df.sim, one.df.sim.this.outcome)
            }
        }
        
        # Pivot wider to convert column "metric" to columns "value.mean", "value.lower", "value.upper" or such
        if (summary.type != 'individual.simulation') {
            df.sim = reshape(df.sim, direction='wide', idvar=names(df.sim)[!(names(df.sim) %in% c('metric', 'value'))], timevar='metric')
            if (!is.null(df.sim[['value.mean']])) df.sim$value = df.sim$value.mean
            if (!is.null(df.sim[['value.median']])) df.sim$value = df.sim$value.median
        }
        
        # make whatever column corresponds to split by actually be called "stratum" and same for facet.by.
        if (!is.null(split.by)) df.sim["stratum"] = df.sim[split.by]
        if (!is.null(facet.by)) df.sim["facet.by"] = df.sim[facet.by]
        
        # if we don't have a 'stratum' col because no split, make an empty one
        if (!('stratum' %in% names(df.sim))) df.sim['stratum'] = rep('', nrow(df.sim))
        
        df.sim$simset = factor(df.sim$simset)
        df.sim$sim = factor(df.sim$sim)
        df.sim$groupid = paste0(df.sim$outcome, '_', df.sim$simset, '_', df.sim$sim, '_', df.sim$stratum)
        
        # sort split by alphabetically to line it up with df.truth when colors are picked
        if (!is.null(split.by))
            df.sim = df.sim[order(df.sim$stratum),]
    }
    
    ## YEAR LAG RATIO
    if (plot.year.lag.ratio) {
        ## We will take log of values, then difference, then exponentiate result
        if (!is.null(df.truth)) {
            df.truth$value = log(df.truth$value)
            if (!is.null(split.by)) {
                if (!is.null(facet.by))
                    df.truth[['stratum']] = do.call(paste, list(df.truth$stratum, df.truth$facet.by, sep="__"))
            }
            else if (!is.null(facet.by))
                df.truth[['stratum']] = df.truth$facet.by
            else df.truth[['stratum']] = rep(0, nrow(df.truth))
            truth.lag.indices = generate_lag_matrix_indices(as.integer(as.factor(df.truth$year)),
                                                            as.integer(as.factor(df.truth$location)),
                                                            as.integer(as.factor(df.truth$stratum)),
                                                            as.integer(as.factor(df.truth$source)),
                                                            nrow(df.truth))
            truth.n.lag.pairs = length(truth.lag.indices)/2
            
            truth.lag.values = apply_lag_to_vector(df.truth$value, truth.lag.indices, rep(0, truth.n.lag.pairs), truth.n.lag.pairs)
            truth.rows.to.keep = truth.lag.indices[rep(c(T,F), truth.n.lag.pairs)] + 1 # add one because CPP is zero-indexed
            df.truth = df.truth[truth.rows.to.keep,]
            df.truth$value = exp(truth.lag.values)
            
            # Remove NAs or Infs generated in this process
            df.truth = df.truth[!is.na(df.truth$value) & !is.infinite(df.truth$value),]

            #If we end up with 0 rows, we need to consider the df.truth to be NULL
            if (nrow(df.truth)==0) df.truth=NULL
        }
        if (!is.null(df.sim)) {
            df.sim$value = log(df.sim$value)
            if (!is.null(split.by)) {
                if (!is.null(facet.by))
                    df.sim[['stratum']] = do.call(paste, list(df.sim$stratum, df.sim$facet.by, sep="__"))
            }
            else if (!is.null(facet.by))
                df.sim[['stratum']] = df.sim$facet.by
            else df.sim[['stratum']] = rep(0, nrow(df.sim))
            # browser()
            sim.lag.indices = generate_lag_matrix_indices(as.integer(as.factor(df.sim$year)),
                                                          as.integer(as.factor(df.sim$sim)),
                                                          as.integer(as.factor(df.sim$stratum)),
                                                          as.integer(as.factor(df.sim$simset)),
                                                          nrow(df.sim))
            sim.n.lag.pairs = length(sim.lag.indices)/2
            
            sim.lag.values = apply_lag_to_vector(df.sim$value, sim.lag.indices, rep(0, sim.n.lag.pairs), sim.n.lag.pairs)
            sim.rows.to.keep = sim.lag.indices[rep(c(T,F), sim.n.lag.pairs)] # add one because CPP is zero-indexed
            df.sim = df.sim[sim.rows.to.keep,]
            df.sim$value = exp(sim.lag.values)
            
            # Remove NAs or Infs generated in this process
            df.sim = df.sim[!is.na(df.sim$value) & !is.infinite(df.sim$value),]
            
            #If we end up with 0 rows, we need to consider the df.sim to be NULL
            if (length(df.sim)==0) df.sim=NULL
        }
    }
    
    #-- PACKAGE AND RETURN --#
    y.label = paste0(sapply(outcomes, function(outcome) {simset.list[[1]][['outcome.metadata']][[outcome]][['units']]}), collapse='/')
    if (title=="location" && length(simset.list)>0) {
        plot.title = paste0(get.location.name(simset.list[[1]]$location), " (", simset.list[[1]]$location, ")")
    }
    else plot.title = title

    return(list(df.sim=df.sim, df.truth=df.truth, details=list(y.label=y.label, plot.title=plot.title)))
}

execute.simplot <- function(prepared.plot.data,
                            outcomes=NULL,
                            split.by=NULL,
                            facet.by=NULL,
                            plot.which=c('both', 'sim.only', 'data.only')[1],
                            summary.type=c('individual.simulation', 'mean.and.interval', 'median.and.interval')[1],
                            plot.year.lag.ratio=F,
                            n.facet.rows=NULL,
                            style.manager=get.default.style.manager(),
                            debug=F)
{
    if (debug) browser()

    #-- UNPACK DATA --#
    df.sim=prepared.plot.data$df.sim
    df.truth=prepared.plot.data$df.truth
    y.label = prepared.plot.data$details$y.label
    plot.title = prepared.plot.data$details$plot.title
    
    #-- PREPARE PLOT COLORS, SHADES, SHAPES, ETC. --#
    
    if (!is.null(df.sim)) {
        df.sim['linetype.sim.by'] = df.sim[style.manager$linetype.sim.by]
        df.sim['shape.sim.by'] = df.sim[style.manager$shape.sim.by]
        df.sim['color.sim.by'] = df.sim[style.manager$color.sim.by]
    }
    
    if (!is.null(df.truth)) {
        # make some other columns
        df.truth['location.type'] = locations::get.location.type(df.truth$location)
        df.truth['shape.data.by'] = df.truth[style.manager$shape.data.by]
        df.truth['color.data.by'] = df.truth[style.manager$color.data.by]
        df.truth['shade.data.by'] = df.truth[style.manager$shade.data.by]
        df.truth['color.and.shade.data.by'] = do.call(paste, c(df.truth['shade.data.by'], df.truth['color.data.by'], list(sep="__")))
    }
    
    ## COLORS
    color.sim.by = NULL
    color.data.primary.colors = NULL
    
    sim.color.groups = sort(unique(df.sim$color.sim.by))
    data.color.groups = sort(unique(df.truth$color.data.by))
    
    # if coloring by the same thing, use the same palette (defaulting to SIM's palette) unless one is missing
    if (style.manager$color.sim.by == style.manager$color.data.by) {
        all.color.groups = sort(union(sim.color.groups, data.color.groups))
        
        if (!is.null(df.sim))
            all.colors = style.manager$get.sim.colors(length(all.color.groups))
        else if (!is.null(df.truth))
            all.colors = style.manager$get.data.colors(length(all.color.groups))
        else
            all.colors = NULL # doesn't matter?
        
        names(all.colors) = all.color.groups
        color.sim.by = all.colors[sim.color.groups]
        color.data.primary.colors = all.colors[data.color.groups]
    }
    
    # otherwise, assign colors individually
    else {
        if (!is.null(df.sim)) {
            color.sim.by = style.manager$get.sim.colors(length(sim.color.groups))
            names(color.sim.by) = sim.color.groups
        }
        if (!is.null(df.truth)) {
            color.data.primary.colors = style.manager$get.data.colors(length(data.color.groups))
            names(color.data.primary.colors) = data.color.groups
        }
    }
    
    ## RIBBON COLOR
    color.ribbon.by = NULL
    if (!is.null(df.sim)) {
        color.ribbon.by = ggplot2::alpha(color.sim.by, style.manager$alpha.ribbon)
    }
    
    ## SHADES FOR DATA
    color.data.shaded.colors = NULL
    if (!is.null(df.truth)) {
        color.data.shaded.colors = unlist(lapply(color.data.primary.colors, function(prim.color) {style.manager$get.shades(base.color=prim.color, length(unique(df.truth$shade.data.by)))}))
        names(color.data.shaded.colors) = do.call(paste, c(expand.grid(unique(df.truth$shade.data.by), unique(df.truth$color.data.by)), list(sep="__")))
    }
    
    ## SHAPES
    shapes.for.data = NULL
    shapes.for.sim = NULL
    if (!is.null(df.truth)) {
        shapes.for.data = style.manager$get.shapes(length(unique(df.truth$shape.data.by)))
        names(shapes.for.data) = unique(df.truth$shape.data.by)
    }
    if (!is.null(df.sim)) {
        shapes.for.sim = style.manager$get.shapes(length(unique(df.sim$shape.sim.by)))
        names(shapes.for.sim) = unique(df.sim$shape.sim.by)
    }
    all.shapes.for.scale = c(shapes.for.data, shapes.for.sim)
    
    ## GROUPS
    # break df.sim into two data frames, one for outcomes where the sim will be lines and the other for where it will be points
    if (!is.null(df.sim)) {
        groupids.with.one.member = setdiff(unique(df.sim$groupid), df.sim$groupid[which(duplicated(df.sim$groupid))])
        df.sim$groupid_has_one_member = with(df.sim, groupid %in% groupids.with.one.member)
        df.sim.groupids.one.member = subset(df.sim, groupid_has_one_member)
        df.sim.groupids.many.members = subset(df.sim, !groupid_has_one_member)
    }
    
    #-- MAKE THE PLOT --#
    
    rv = ggplot2::ggplot()
    if (!is.null(df.sim)) {
        rv = rv + ggplot2::scale_color_manual(name = "sim color", values = color.sim.by)
        rv = rv + ggplot2::scale_fill_manual(name = "sim color", values = color.sim.by)
        rv = rv + ggplot2::scale_linetype(name="sim linetype")
    }
    if (!is.null(df.truth)) {
        rv = rv + ggplot2::scale_shape_manual(name = "data shape", values = all.shapes.for.scale)
    }
    
    if (!plot.year.lag.ratio) rv = rv + ggplot2::scale_y_continuous(limits=c(0, NA), labels = scales::comma)
    else
        rv = rv + ggplot2::scale_y_continuous(labels = scales::comma)
    # browser()
    # how data points are plotted is conditional on 'split.by', but the facet_wrap is not
    if (!is.null(split.by)) {
        if (!is.null(df.sim)) {
            rv = rv + ggplot2::geom_line(data=df.sim.groupids.many.members, ggplot2::aes(x=year,y=value,group=groupid,
                                                                                         linetype = linetype.sim.by,
                                                                                         color = color.sim.by,
                                                                                         alpha = alpha,
                                                                                         linewidth = linewidth)) +
                ggplot2::geom_point(data=df.sim.groupids.one.member, size=2, ggplot2::aes(x=year, y=value,
                                                                                          color = color.sim.by,
                                                                                          shape = shape.sim.by))
            if (summary.type != 'individual.simulation')
                rv = rv + ggplot2::geom_ribbon(data=df.sim.groupids.many.members, ggplot2::aes(x=year, y=value,group=groupid,
                                                                                               fill = color.sim.by,
                                                                                               ymin = value.lower,
                                                                                               ymax = value.upper),
                                               alpha = style.manager$alpha.ribbon,
                                               outline.type = 'full')
        }
        if (!is.null(df.truth)) {
            rv = rv + ggnewscale::new_scale_fill() + ggplot2::scale_fill_manual(values = color.data.shaded.colors)
            rv = rv + ggplot2::guides(fill = ggplot2::guide_legend("data color", override.aes = list(shape = 21)))
            rv = rv + ggplot2::geom_point(data=df.truth, ggplot2::aes(x=year, y=value,
                                                                      fill=color.and.shade.data.by, # fill
                                                                      shape=shape.data.by))
        }
        
    } else {
        if (!is.null(df.sim)) {
            rv = rv + ggplot2::geom_line(data=df.sim.groupids.many.members, ggplot2::aes(x=year, y=value, group=groupid,
                                                                                         linetype = linetype.sim.by,
                                                                                         alpha = alpha,
                                                                                         linewidth = linewidth)) +
                ggplot2::geom_point(data=df.sim.groupids.one.member, size=2, ggplot2::aes(x=year, y=value,
                                                                                          color = color.sim.by,
                                                                                          shape=shape.sim.by))
            if (summary.type != 'individual.simulation') {
                rv = rv + ggplot2::geom_ribbon(data=df.sim.groupids.many.members, ggplot2::aes(x=year, y=value,group=groupid,
                                                                                               fill = color.sim.by,
                                                                                               ymin = value.lower,
                                                                                               ymax = value.upper),
                                               alpha = style.manager$alpha.ribbon,
                                               outline.type = 'full')
                # Remove the fill scale since we don't have more than one sim ribbon color
                if (style.manager$color.sim.by == "stratum")
                    rv = rv + ggplot2::guides(fill = "none")
            }
        }
        if (!is.null(df.truth)) {
            rv = rv + ggnewscale::new_scale_fill() + ggplot2::scale_fill_manual(values = color.data.shaded.colors)
            rv = rv + ggplot2::guides(fill = ggplot2::guide_legend("data color", override.aes = list(shape = 21)))
            rv = rv + ggplot2::geom_point(data=df.truth, size=2, ggplot2::aes(x=year, y=value, fill=color.and.shade.data.by, shape = shape.data.by))
        } 
    }
    # If don't have a split.by, and thus only 1 color for sim, probably, then remove legend for it.
    if (style.manager$color.sim.by == 'stratum' && is.null(split.by))
        rv = rv + ggplot2::guides(color = "none")
    
    #-- FACET --#
    if (is.null(facet.by))
        facet.formula = as.formula("~outcome.display.name")
    else
        facet.formula = as.formula("~outcome.display.name + facet.by")
    if (!is.null(n.facet.rows))
        rv = rv + ggplot2::facet_wrap(facet.formula, scales = 'free_y', nrow=n.facet.rows)
    else
        rv = rv + ggplot2::facet_wrap(facet.formula, scales = 'free_y')
    
    rv = rv +
        ggplot2::scale_alpha(guide='none') +
        ggplot2::labs(y=y.label) +
        ggplot2::ggtitle(plot.title)
    if (!is.null(df.sim))
        rv = rv + ggplot2::scale_linewidth(NULL, range=c(min(df.sim$linewidth), 1), guide = 'none')
    
    if (plot.year.lag.ratio) rv = rv + ggplot2::xlab("latter year")

    rv
}