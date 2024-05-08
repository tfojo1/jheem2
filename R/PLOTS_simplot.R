

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
                    plot.which = c('both', 'sim.only', 'data.only')[1],
                    plot.year.lag.ratio = F,
                    data.manager = get.default.data.manager(),
                    style.manager = get.default.style.manager(),
                    debug = F)
{
    ### --- FEATURES TO ADD --- ###
    # - change y-axis from 'value' to the name of the outcome? Maybe not, since "value" is shared by all plots
    # - y labels, scale may be percentage or number with commas
    
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
    
    if (!(identical(plot.which, 'sim.only') || identical(plot.which, 'data.only') || identical(plot.which, 'both')))
        stop(paste0(error.prefix, "'plot.which' must be one of 'sim.only', 'data.only', or 'both'"))
    
    if (!identical(plot.year.lag.ratio, T) && !identical(plot.year.lag.ratio, F))
        stop(paste0(error.prefix, "'plot.year.lag.ratio' must be either T or F"))
    
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
    if (!is.null(df.truth)) {
        # make whatever column corresponds to split by actually be called "split.by" and same for facet.by.
        if (!is.null(split.by)) names(df.truth)[names(df.truth)==split.by] = "split.by"
        if (!is.null(facet.by)) names(df.truth)[names(df.truth)==facet.by] = "facet.by"
    }
    # browser()
    names(outcome.mappings) = outcomes

    #-- STEP 3: MAKE A DATA FRAME WITH ALL THE SIMULATIONS' DATA --#

    df.sim = NULL
    if (plot.which != 'data.only') {
        for (outcome in outcomes) {
            
            # Determine the keep.dimensions we need from the sim$gets
            # extra.dimensions.needed.for.mapping = c()
            # if (plot.which != 'sim.only' && !is.null(outcome.mappings[[outcome]])) {
            #     extra.dimensions.needed.for.mapping = setdiff(outcome.mappings[[outcome]]$from.dimensions, c(facet.by, split.by, 'year'))
            #     keep = union(c('year', facet.by, split.by), extra.dimensions.needed.for.mapping)
            # }
            # else keep = c('year', facet.by, split.by)
            keep.dimensions = c('year', facet.by, split.by)
            for (i in seq_along(simset.list)) {
                
                simset = simset.list[[i]]
                if (!is.null(outcome.mappings[[outcome]])) mapping.this.outcome = outcome.mappings[[outcome]]
                else mapping.this.outcome = NULL
                # browser()
                simset.data.this.outcome = simset$get(outcomes = outcome,
                                                      dimension.values = dimension.values,
                                                      keep.dimensions = keep.dimensions,
                                                      drop.single.outcome.dimension = T,
                                                      mapping=mapping.this.outcome)
                # if (plot.which != 'sim.only' && !is.null(outcome.mappings[[outcome]])) {
                #     simset.data.mapped.this.outcome = tryCatch(
                #         {outcome.mappings[[outcome]]$apply(simset.data.this.outcome)},
                #         error = function(e) {NULL}
                #     )
                # } else simset.data.mapped.this.outcome = simset.data.this.outcome
                
                # if (is.null(simset.data.mapped.this.outcome)) next
                if (is.null(simset.data.this.outcome)) next
                
                # Aggregate out the extra dimensions we may have gotten for the mapping
                # simset.data.mapped.this.outcome = apply(simset.data.mapped.this.outcome, setdiff(names(dim(simset.data.mapped.this.outcome)), extra.dimensions.needed.for.mapping), sum, na.rm=T)
                
                # If we have multiple outcomes that may map differently (for example, with years), the factor levels unavoidably determined by the first outcome for reshape2::melt may not be valid for subsequent outcomes
                one.df.sim.this.outcome = reshape2::melt(simset.data.this.outcome, na.rm = T)
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
        # make whatever column corresponds to split by actually be called "split.by" and same for facet.by.
        if (!is.null(split.by)) df.sim["split.by"] = df.sim[split.by]
        if (!is.null(facet.by)) df.sim["facet.by"] = df.sim[facet.by]
        
        df.sim$simset = factor(df.sim$simset)
        df.sim$sim = factor(df.sim$sim)
        df.sim$groupid = paste0(df.sim$outcome, '_', df.sim$simset, '_', df.sim$sim, '_', df.sim$split.by)
        
        df.sim['linetype.sim.by'] = df.sim[style.manager$linetype.sim.by]
        df.sim['shape.sim.by'] = df.sim[style.manager$shape.sim.by]
        if (style.manager$color.sim.by == 'stratum' && !is.null(split.by))
            df.sim['color.sim.by'] = df.sim$split.by
        else df.sim['color.sim.by'] = rep('none', nrow=df.sim)
    }
    
    if (plot.year.lag.ratio) {
        ## We will take log of values, then difference, then exponentiate result
        if (!is.null(df.truth)) {
            df.truth$value = log(df.truth$value)
            if (!is.null(split.by)) {
                if (!is.null(facet.by))
                    df.truth[['stratum']] = do.call(paste, list(df.truth$split.by, df.truth$facet.by, sep="__"))
                else
                    df.truth[['stratum']] = df.truth$split.by
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
            truth.rows.to.keep = truth.lag.indices[rep(c(T,F), truth.n.lag.pairs/2)]
            df.truth = df.truth[truth.rows.to.keep,]
            df.truth$value = exp(truth.lag.values)
            
            # Remove NAs or Infs generated in this process
            df.truth = df.truth[!is.na(df.truth$value) & !is.infinite(df.truth$value),]
        }
        if (!is.null(df.sim)) {
            df.sim$value = log(df.sim$value)
            if (!is.null(split.by)) {
                if (!is.null(facet.by))
                    df.sim[['stratum']] = do.call(paste, list(df.sim$split.by, df.sim$facet.by, sep="__"))
                else
                    df.sim[['stratum']] = df.sim$split.by
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
            sim.rows.to.keep = sim.lag.indices[rep(c(T,F), sim.n.lag.pairs/2)]
            df.sim = df.sim[sim.rows.to.keep,]
            df.sim$value = exp(sim.lag.values)
            
            # Remove NAs or Infs generated in this process
            df.sim = df.sim[!is.na(df.sim$value) & !is.infinite(df.sim$value),]
        }
    }
    
    if (!is.null(df.sim)) {
        
        
        # break df.sim into two data frames, one for outcomes where the sim will be lines and the other for where it will be points
        groupids.with.one.member = setdiff(unique(df.sim$groupid), df.sim$groupid[which(duplicated(df.sim$groupid))])
        df.sim$groupid_has_one_member = with(df.sim, groupid %in% groupids.with.one.member)
        df.sim.groupids.one.member = subset(df.sim, groupid_has_one_member)
        df.sim.groupids.many.members = subset(df.sim, !groupid_has_one_member)
    }
    
    if (!is.null(df.truth)) {
        df.truth['location.type'] = locations::get.location.type(df.truth$location)
        
        df.truth['shape.data.by'] = df.truth[style.manager$shape.data.by]
        if (style.manager$color.data.by == 'stratum' && !is.null(split.by))
            df.truth['color.data.by'] = df.truth$split.by
        else if (style.manager$color.data.by == 'location.type')
            df.truth['color.data.by'] = df.truth['location.type']
        else df.truth['color.data.by'] = rep('none', nrow(df.truth))
        
        # modify colors with shades
        # for every color we have, we will then have as many shades of it as we have shade.by features
        df.truth['shade.data.by'] = df.truth[style.manager$shade.data.by]
        df.truth['color.and.shade.data.by'] = do.call(paste, c(df.truth['shade.data.by'], df.truth['color.data.by'], list(sep="__")))
    }
    
    #-- STEP 4: MAKE THE PLOT --#
    # browser()
    y.label = paste0(sapply(outcomes, function(outcome) {simset.list[[1]][['outcome.metadata']][[outcome]][['units']]}), collapse='/')
    
    if (is.null(facet.by))
        facet.formula = as.formula("~outcome")
    else
        facet.formula = as.formula("~outcome + facet.by")
    # browser()
    # determine colors as a named vector
    # need one color per unique value in color "color.and.shade.data.by"
    # first get one color per color.data.by
    color.data.shaded.colors = NULL
    color.sim.by = NULL
    shapes.for.data = NULL
    shapes.for.sim = NULL
    if (!is.null(df.truth)) {
        color.data.primary.colors = style.manager$get.data.colors(length(unique(df.truth$color.data.by)))
        color.data.shaded.colors = unlist(lapply(color.data.primary.colors, function(prim.color) {style.manager$get.shades(base.color=prim.color, length(unique(df.truth$shade.data.by)))}))
        names(color.data.shaded.colors) = do.call(paste, c(expand.grid(unique(df.truth$shade.data.by), unique(df.truth$color.data.by)), list(sep="__")))
        shapes.for.data = style.manager$get.shapes(length(unique(df.truth$shape.data.by)))
        names(shapes.for.data) = unique(df.truth$shape.data.by)
    }
    
    if (!is.null(df.sim)) {
        color.sim.by = style.manager$get.sim.colors(length(unique(df.sim$color.sim.by)))
        names(color.sim.by) = unique(df.sim$color.sim.by)
        shapes.for.sim = style.manager$get.shapes(length(unique(df.sim$shape.sim.by)))
        names(shapes.for.sim) = unique(df.sim$shape.sim.by)
    }
    
    all.colors.for.scale = c(color.data.shaded.colors, color.sim.by)
    all.shapes.for.scale = c(shapes.for.data, shapes.for.sim)
    
    # browser()
    rv = ggplot2::ggplot()
    rv = rv + ggplot2::scale_color_manual(values = all.colors.for.scale)
    rv = rv + ggplot2::scale_shape_manual(values = all.shapes.for.scale)
    rv = rv + ggplot2::scale_fill_manual(values = color.data.shaded.colors)
    rv = rv + ggplot2::guides(fill = ggplot2::guide_legend("Shade legend", override.aes = list(shape = 21)))
    
    if (!plot.year.lag.ratio) rv = rv + ggplot2::scale_y_continuous(limits=c(0, NA), labels = scales::comma)
    else rv = rv + ggplot2::scale_y_continuous(labels = scales::comma)

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
            
            
            # rv = rv + ggplot2::geom_line(data=df.sim.groupids.many.members, ggplot2::aes(x=year, y=value, linetype=style.manager$linetype.sim.by, group=groupid, color=split.by, alpha=alpha, linewidth=linewidth)) +
            #     ggplot2::geom_point(data=df.sim.groupids.one.member, size=2, ggplot2::aes(x=year, y=value, shape=style.manager$shape.data.by, color=split.by))
        }
        if (!is.null(df.truth)) {
            rv = rv + ggplot2::geom_point(data=df.truth, ggplot2::aes(x=year, y=value,
                                                                      fill=color.and.shade.data.by, # fill
                                                                      shape=shape.data.by))
            # scale fill manual instead
            # rv = rv + ggplot2::geom_point(data=df.truth, ggplot2::aes(x=year, y=value,color=split.by, shape=ifelse(length(unique(location))==1, source, location)))
        }
            
    } else {
        if (!is.null(df.sim)) {
            rv = rv + ggplot2::geom_line(data=df.sim.groupids.many.members, ggplot2::aes(x=year, y=value, group=groupid,
                                                                                         linetype = linetype.sim.by,
                                                                                         alpha = alpha,
                                                                                         linewidth = linewidth)) +
                ggplot2::geom_point(data=df.sim.groupids.one.member, size=2, ggplot2::aes(x=year, y=value, shape=shape.sim.by))
            # rv = rv + ggplot2::geom_line(data=df.sim.groupids.many.members, ggplot2::aes(x=year, y=value, linetype=simset, group=groupid, alpha=alpha, linewidth=linewidth)) +
            #     ggplot2::geom_point(data=df.sim.groupids.one.member, size=2, ggplot2::aes(x=year, y=value, shape=simset))
        }
        if (!is.null(df.truth))
            rv = rv + ggplot2::geom_point(data=df.truth, size=2, ggplot2::aes(x=year, y=value, fill=color.and.shade.data.by, shape = shape.data.by))
            # rv = rv + ggplot2::geom_point(data=df.truth, ggplot2::aes(x=year, y=value, shape=ifelse(length(unique(location))==1, source, location)))
    }
    
    # rv = rv + ggplot2::scale_color_manual(values = color.values)
    
    rv = rv + ggplot2::facet_wrap(facet.formula, scales = 'free_y', )
    
    rv = rv +
        ggplot2::scale_alpha(guide='none') +
        ggplot2::labs(y=y.label)
    if (!is.null(df.sim)) rv = rv + ggplot2::scale_linewidth(NULL, range=c(min(df.sim$linewidth), 1), guide = 'none')
    
    if (plot.year.lag.ratio) rv = rv + xlab("latter year")
    
    rv
    }
