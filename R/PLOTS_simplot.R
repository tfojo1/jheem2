
sim = make.dummy.sim()

#'@param ... One or more of either (1) jheem.simulation objects or (2) jheem.simset objects or 93) lists containing only jheem.simulation or jheem.simset objects
#'@param outcomes A character vector of which outcomes to plot
#'
#'@details A ggplot object:
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
                    palette)
{
    # Get the sims list out of ...
    
    #-- Make df.sim --#
    # columns:
    # $year
    # $value
    # $outcome
    # $sim.name - name for the simulation
    # <a column for each element of facet.by>
    
    df.truth = NULL
    for (outcome in outcomes)
    {
        data.manager$pull()
    }
    
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
    
    facet.formula = as.formula(paste0("~outcome",
                                      paste0(" + ", facet.by, collapse='')))
    
    ggplot() +
        geom_line(data=df.sim, aes(x=year, y=value, color=sim.name)) +
        geom_point(data=df.truth, aes(x=year, y=value)) +
        facet_wrap(facet.formula, scales = 'free_y')
}