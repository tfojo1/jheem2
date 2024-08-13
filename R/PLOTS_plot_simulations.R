
#'@param ... One or more of either (1) jheem.simulation.set objects or (2) lists containing only jheem.simulation or jheem.simset objects
#'@param outcomes A character vector of which simulation outcomes to plot
#'@param split.by A character vector of dimensions for which to make different lines
#'@param facet.by A character vector of dimensions for which to make different panels
#'@param dimension.values
#'@param data.manager The data.manager from which to draw real-world data for the plots
#'@param style.manager An object of class 'jheem.style.manager' specifying style attributes

plot.simulations <- function(...,
                            outcomes,
                            split.by = character(),
                            facet.by = character(),
                            dimension.values = list(),
                            statistic = c('individual.simulations','mean.and.interval','median.and.interval')[1],
                            interval.coverate = 0.95,
                            data.manager,
                            style.manager = get.default.style.manager('plotly'))
{
    
}