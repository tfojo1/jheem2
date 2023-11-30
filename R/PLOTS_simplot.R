

#'@param ... One or more of either (1) jheem.simulation objects or (2) jheem.simset objects or (3) lists containing only jheem.simulation or jheem.simset objects
#'@param outcomes A character vector of which simulation outcomes to plot
#'@param split.by AZ: at most one dimension
#'@param facet.by AZ: any number of dimensions but cannot include the split.by dimension
#'@param dimension.values
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
                    split.by = NULL,
                    facet.by = NULL,
                    dimension.values = list(),
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
        stop(patse0(error.prefix, "'facet.by' cannot contain 'year'"))
    
    #-- STEP 1: PRE-PROCESSING --#
    # Get a list out of ... where each element is one simset (or sim for now)
    sim.args = list(...) # will later be SIMSETS
    
    outcomes.found.in.sim.args = F
    # each element of 'sim.list' should be either a sim or list containing only sims.
    for (element in sim.args) {
        if (!R6::is.R6(element) || !is(element, 'jheem.simulation')) {
            if (is.list(element)) {
                if (any(sapply(element, function(sub.element) {!R6::isR6(sub.element) || !is(sub.element, 'jheem.simulation')}))) {
                    stop(paste0(error.prefix, "arguments supplied in '...' must either be jheem.simulation objects or lists containing only jheem.simulation objects"))
                }
            } else if (is.null(outcomes) && is.character(element)) {
                outcomes = element
                outcomes.found.in.sim.args = T
            }
            else
                stop(paste0(error.prefix, "arguments supplied in '...' must either be jheem.simulation objects or lists containing only jheem.simulation objects"))
        }
    }
    
    if (!is.character(outcomes) || is.null(outcomes) || any(is.na(outcomes)) || any(duplicated(outcomes))) {
        if (outcomes.found.in.sim.args)
            stop(paste0(error.prefix, "'outcomes' found as unnamed argument in '...' must be a character vector with no NAs or duplicates"))
        else
            stop(paste0(error.prefix, "'outcomes' must be a character vector with no NAs or duplicates"))
    }
    
    if (outcomes.found.in.sim.args) {
        if (length(sim.args) < 2)
            stop(paste0(error.prefix, "one or more jheem.simulation objects or lists containing only jheem.simulation objects must be supplied"))
        else
            sim.list = sim.args[1:(length(sim.args)-1)]
    }
    else {
        if (length(sim.args) < 1)
            stop(paste0(error.prefix, "one or more jheem.simulation objects or lists containing only jheem.simulation objects must be supplied"))
        else
            sim.list = sim.args
    }
    
    # - make sure they are all the same version and the location
    if (length(unique(sapply(sim.list, function(sim) {sim$version}))) > 1)
        stop(paste0(error.prefix, "all simulations must have the same version"))
    if (length(unique(sapply(sim.list, function(sim) {sim$location}))) > 1)
        stop(paste0(error.prefix, "all simulations must have the same location"))
    
    # Check outcomes
    # - make sure each outcome is present in sim$outcomes for at least one sim/simset
    if (any(sapply(outcomes, function(outcome) {!any(sapply(sim.list, function(sim) {outcome %in% sim$outcomes}))})))
        stop(paste0("There weren't any simulations for one or more outcomes. Should this be an error?"))
    
    # Get the real-world outcome names
    # - eventually we're going to want to pull this from info about the likelihood if the sim notes which likelihood was used on it
    # - what we'll do now will be the back-up to above
    #   sim$outcome.metadata[[outcome]]$corresponding.observed.outcome
    # sims do not all have each outcome because of sub-versions
    
    # likelihoods need to share their outcome for sim and data, and think about what joint likelihoods. One simulation has one (usually joint) likelihood (instructions)

    outcomes.for.data = sapply(outcomes, function(outcome) {
        corresponding.observed.outcome = NULL
        i = 1
        while (i <= length(sim.list)) {
            if (outcome %in% names(sim.list[[i]]$outcome.metadata)) {
                corresponding.observed.outcome = sim.list[[i]]$outcome.metadata[[outcome]]$corresponding.observed.outcome
                break
            } else i = i + 1
        }
        # if (is.null(corresponding.observed.outcome))
        #     stop(paste0("No corresponding observed outcome found for outcome '", outcome, "'")) # Shouldn't happen
        corresponding.observed.outcome
    })
    
    outcome.ontologies = lapply(outcomes, function(outcome) {
        outcome.ontology = NULL
        i = 1
        while (i <= length(sim.list)) {
            if (outcome %in% names(sim.list[[i]]$outcome.ontologies)) {
                outcome.ontology = sim.list[[i]]$outcome.ontologies[[outcome]]
                break
            } else i = i + 1
        }
        if (is.null(outcome.ontology))
            stop(paste0("No outcome ontology found for outcome '", outcome, "'")) # Shouldn't happen
        outcome.ontology
    })
    
    # Get the locations to pull data for for each outcome
    # - for now, just use the sim$location
    location = sim.list[[1]]$location
    
    #-- STEP 2: MAKE A DATA FRAME WITH ALL THE REAL-WORLD DATA --#

    outcome.mappings = list() # note: not all outcomes will have corresponding data outcomes
    
    df.truth = NULL
    for (i in seq_along(outcomes.for.data))
    {
        if (!is.null(outcomes.for.data[[i]]))
        {
            # print(i)
            outcome.data = data.manager$pull(outcome = outcomes.for.data[[i]],
                                             dimension.values = c(dimension.values, list(location = location)),
                                             keep.dimensions = c('year', facet.by, split.by), #'year' can never be in facet.by
                                             target.ontology = outcome.ontologies[[i]],
                                             allow.mapping.from.target.ontology = T,
                                             debug=F)
            outcome.mappings = c(outcome.mappings, list(attr(outcome.data, 'mapping')))
    
            one.df.outcome = reshape2::melt(outcome.data, na.rm = T)
            corresponding.outcome = names(outcomes.for.data)[[i]]
            one.df.outcome['outcome'] = corresponding.outcome
            df.truth = rbind(df.truth, one.df.outcome)
            # browser()
        }
        else
        {
            outcome.mappings = c(outcome.mappings, list(NULL))
        }
    }

    names(outcome.mappings) = outcomes

    #-- STEP 3: MAKE A DATA FRAME WITH ALL THE SIMULATIONS' DATA --#
    
    df.sim = NULL
    for (i in seq_along(sim.list))
    {
        sim = sim.list[[i]]
        sim.data = sim$get(outcomes = outcomes,
                           dimension.values = dimension.values,
                           keep.dimensions = c('year', facet.by, split.by),
                           drop.single.outcome.dimension = F)

        dim.names.without.outcome = dimnames(sim.data)[names(dimnames(sim.data)) != 'outcome']
        outcome.arrays = lapply(outcomes, function(outcome) {
            arr = array(sim.data[get.array.access.indices(dimnames(sim.data),
                                                          dimension.values = list(outcome = outcome))],
                        dim = sapply(dim.names.without.outcome, length),
                        dimnames = dim.names.without.outcome)
            if (!is.null(outcome.mappings[[outcome]]))
                outcome.mappings[[outcome]]$apply(arr)
            else
                arr
        })
        mapped.dimnames = c(dimnames(outcome.arrays[[1]]), list(outcome = outcomes))
        mapped.sim.data = array(unlist(outcome.arrays), dim=sapply(mapped.dimnames, length), dimnames = mapped.dimnames)
        
        one.df.sim = reshape2::melt(mapped.sim.data, na.rm = T)
        one.df.sim['sim.name'] = i
        
        df.sim = rbind(df.sim, one.df.sim)
    }
    
    #- STEP 4: MAKE THE PLOT --#

    facet.formula = as.formula(paste0("~",
                                      paste0(c('outcome', facet.by), collapse='+')))

    if (is.null(split.by)) {
        if (is.null(facet.by)) {
            rv = ggplot() +
                geom_line(data=df.sim, aes(x=year, y=value, color=sim.name))
            if (!is.null(df.truth))
                rv = rv + geom_point(data=df.truth, aes(x=year, y=value))
        } else {
            rv = ggplot() +
                geom_line(data=df.sim, aes(x=year, y=value, color=sim.name)) +
                facet_wrap(facet.formula, scales = 'free_y')
            if (!is.null(df.truth))
                rv = rv + geom_point(data=df.truth, aes(x=year, y=value))
        }
    } else {
        if (is.null(facet.by)) {
            rv = ggplot() +
                geom_line(data=df.sim, aes(x=year, y=value, color=!!sym(split.by)))
            if (!is.null(df.truth))
                rv = rv + geom_point(data=df.truth, aes(x=year, y=value, color=!!sym(split.by)))
        } else {
            rv = ggplot() +
                geom_line(data=df.sim, aes(x=year, y=value, color=!!sym(split.by))) +
                facet_wrap(facet.formula, scales = 'free_y')
            if (!is.null(df.truth))
                rv = rv + geom_point(data=df.truth, aes(x=year, y=value, color=!!sym(split.by)))
        }
        
    }
    
    rv = rv + ylim(0,NA)

    rv
}