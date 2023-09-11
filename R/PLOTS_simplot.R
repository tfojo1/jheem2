
sim = make.dummy.sim()

#'@param ... One or more of either (1) jheem.simulation objects or (2) jheem.simset objects or (3) lists containing only jheem.simulation or jheem.simset objects
#'@param outcomes A character vector of which simulation outcomes to plot
#'@param split.by
#'@param facet.by
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
                    split.by,
                    facet.by = character(),
                    dimension.values,
                    data.manager,
                    style.manager)
{
    #-- STEP 1: PRE-PROCESSING --#
    # Get a list out of ... where each element is one simset (or sim for now)
    
    simset.list = list(...)
    # @Andrew - fill in
    # - make sure they are all the same version and the location
    
    # Check outcomes
    # - make sure it's a non-empty character vector, no NAs
    # - make sure each outcome is present in sim$outcomes for at least one sim/simset
    
    # Get the real-world outcome names
    # - eventually we're going to want to pull this from info about the likelihood if the sim notes which likelihood was used on it
    # - what we'll do now will be the back-up to above
    #   sim$outcome.metadata[[outcome]]$corresponding.observed.outcome
    sim.one = simset.list[[1]] #if a list of sims
    sim.outcome.metadata = sim.one$outcome.metadata
    outcomes.for.data = sapply(outcomes, function(outcome) {
        sim.outcome.metadata[[outcome]]$corresponding.observed.outcome
    })
    outcome.mapping = setNames(outcomes, outcomes.for.data)
    
    # Get the locations to pull data for for each outcome
    # - for now, just use the sim$location
    
    #-- STEP 2: MAKE A DATA FRAME WITH ALL THE REAL-WORLD DATA --#
    # columns:
    # $year
    # $value
    # $outcome
    # $split
    # $sim.name - name for this simulation
    # <a column for each element of facet.by>
    
    df.truth = NULL
    # I probably want to make sure this data frame is still indexed by simulation outcome
    for (outcome in outcomes.for.data)
    {
        outcome.data = data.manager$pull(outcome = outcome,
                                         dimension.values = dimension.values,
                                         keep.dimensions = union('year', facet.by))
        one.df.outcome = reshape2::melt(outcome.data) # I need to add an outcome column, don't I?
        df.truth = rbind(df.truth, one.df.outcome)
    }
    df.truth[[outcome]] = lapply(df.truth[[outcome]], function(x){outcome.mapping[[x]]}) # probably incorrect syntax
    
    
    #-- STEP 3: MAKE A DATA FRAME WITH ALL THE SIMULATIONS' DATA --#
    # columns:
    # $year
    # $value
    # $outcome
    # $split
    # $sim.name - name for the simulation
    # <a column for each element of facet.by>
    
    
    df.sim = NULL
    for (sim in sims)
    {
        sim.data = sim$get(outcomes = outcomes,
                           dimension.values = dimension.values,
                           keep.dimensions = union('year', facet.by),
                           drop.single.outcome.dimension = F)
        
        one.df.sim = reshape2::melt(sim.data)
        
        df.sim = rbind(df.sim, one.df.sim)
    }
    
    
    #- STEP 4: MAKE THE PLOT --#
    
    facet.formula = as.formula(paste0("~outcome",
                                      paste0(" + ", facet.by, collapse='')))
    
    ggplot() +
        geom_line(data=df.sim, aes(x=year, y=value, color=sim.name)) +
        geom_point(data=df.truth, aes(x=year, y=value)) +
        facet_wrap(facet.formula, scales = 'free_y')
}