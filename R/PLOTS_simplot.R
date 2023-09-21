
sim = make.dummy.sim()

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
                    outcomes,
                    split.by = NULL,
                    facet.by = NULL,
                    dimension.values = list(),
                    data.manager,
                    style.manager)
{
    ### --- FEATURES TO ADD --- ###
    # - change y-axis from 'value' to the name of the outcome? Maybe not, since "value" is shared by all plots
    # - allow changing color scheme for lines and points
    
    # -- VALIDATION -- #
    
    error.prefix = "Cannot generate simplot: "
    
    # *split.by* is NULL or a single, non-NA character vector
    if (!is.null(split.by) && (!is.character(split.by) || length(split.by) > 1 || is.na(split.by)))
        stop(paste0(error.prefix, "'split.by' must be NULL or a length one, non-NA character vector"))
    
    # *facet.by* is NULL or a character vector of length > 0 with no NAs or duplicates
    if (!is.null(facet.by) && (!is.character(facet.by) || length(facet.by) < 1 || any(is.na(facet.by)) || any(duplicated(facet.by))))
        stop(paste0(error.prefix, "'facet.by' must be NULL or a character vector with at least one element and no NAs or duplicates"))
    
    #-- STEP 1: PRE-PROCESSING --#
    # Get a list out of ... where each element is one simset (or sim for now)
    sim.list = list(...) # will later be SIMSETS
    # - make sure they are all the same version and the location
    if (length(unique(sapply(sim.list, function(sim) {sim$version}))) > 1)
        stop(paste0(error.prefix, "all simulations must have the same version"))
    if (length(unique(sapply(sim.list, function(sim) {sim$location}))) > 1)
        stop(paste0(error.prefix, "all simulations must have the same location"))
    
    # Check outcomes
    # - make sure it's a non-empty character vector, no NAs
    # - make sure each outcome is present in sim$outcomes for at least one sim/simset
    if (!is.character(outcomes) || is.null(outcomes) || any(sapply(outcomes, function(outcome) {
        if (is.na(outcome)) T
        else
            !any(sapply(sim.list, function(sim) {
                outcome %in% sim$outcomes
            }))
    })))
        stop(paste0("There weren't any simulations for one or more outcomes. Should this be an error?"))
    
    # Get the real-world outcome names
    # - eventually we're going to want to pull this from info about the likelihood if the sim notes which likelihood was used on it
    # - what we'll do now will be the back-up to above
    #   sim$outcome.metadata[[outcome]]$corresponding.observed.outcome
    # sims do not all have each outcome because of sub-versions

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
    
    ## HARDCODE BECAUSE CORRESPONDING.OBSERVED.OUTCOME INFORMATION IS MISSING
    outcomes.for.data = setNames(c('diagnoses', 'prevalence'), outcomes)
    
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

    outcome.mappings = list()
    
    df.truth = NULL
    for (i in seq_along(outcomes.for.data))
    {
        outcome.data = data.manager$pull(outcome = outcomes.for.data[[i]],
                                         dimension.values = c(dimension.values, list(location = location)),
                                         keep.dimensions = c('year', facet.by, split.by), #'year' can never be in facet.by
                                         target.ontology = outcome.ontologies[[i]],
                                         allow.mapping.from.target.ontology = T)
        outcome.mappings = c(outcome.mappings, list(attr(outcome.data, 'mapping')))
        one.df.outcome = reshape2::melt(outcome.data, na.rm = T)
        corresponding.outcome = names(outcomes.for.data)[[i]]
        one.df.outcome['outcome'] = corresponding.outcome
        df.truth = rbind(df.truth, one.df.outcome)
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
            outcome.mappings[[outcome]]$apply(arr)
        })
        mapped.dimnames = c(dimnames(outcome.arrays[[1]]), list(outcome = outcomes))
        mapped.sim.data = array(unlist(outcome.arrays), dim=sapply(mapped.dimnames, length), dimnames = mapped.dimnames)
        
        one.df.sim = reshape2::melt(mapped.sim.data, na.rm = T)
        one.df.sim['sim.name'] = i
        
        df.sim = rbind(df.sim, one.df.sim)
    }
    
    #- STEP 4: MAKE THE PLOT --#
    
    facet.formula = as.formula(paste0("~outcome",
                                      paste0(" + ", facet.by, collapse='')))
    
    if (is.null(split.by)) {
        if (is.null(facet.by)) {
            ggplot() +
                geom_line(data=df.sim, aes(x=year, y=value, color=sim.name)) +
                geom_point(data=df.tuth, aes(x=year, y=value))
        } else {
            ggplot() +
                geom_line(data=df.sim, aes(x=year, y=value, color=sim.name)) +
                geom_point(data=df.truth, aes(x=year, y=value)) +
                facet_wrap(facet.formula, scales = 'free_y')
        }
    } else {
        if (is.null(facet.by)) {
            ggplot() +
                geom_line(data=df.sim, aes(x=year, y=value, color=!!sym(split.by))) +
                geom_point(data=df.truth, aes(x=year, y=value, color=!!sym(split.by)))
        } else {
            ggplot() +
                geom_line(data=df.sim, aes(x=year, y=value, color=!!sym(split.by))) +
                geom_point(data=df.truth, aes(x=year, y=value, color=!!sym(split.by))) +
                facet_wrap(facet.formula, scales = 'free_y')
        }
        
    }
}