# We need access to the prepare.plot function from PLOTS_simplot.R

source("R/PLOTS_simplot.R")

# Need access to the Data Manager

source("R/DATA_MANAGER_data_manager.R")

#'@param ... One or more of either (1) jheem.simulation.set objects or (2) lists containing only jheem.simulation or jheem.simset objects
#'@param outcomes A character vector of which simulation outcomes to plot
#'@param split.by A character vector of dimensions for which to make different lines
#'@param facet.by A character vector of dimensions for which to make different panels
#'@param dimension.values
#'@param data.manager The data.manager from which to draw real-world data for the plots
#'@param style.manager An object of class 'jheem.style.manager' specifying style attributes

# Questions for Todd and Andrew: October 7th, 2024

# - Best way to setup the environment to test/work on plot.simulations
#    - how to properly reference prepare.plot
        # No source() commands in any files as will be used as
        # package
        # source_jheem2_package is all we need in the test
        # 
#    - how to get access to data.manager / style.manager functions
#    - loading up the simset data

# - How to deal with parameters that are not available for plot.simulations
#    - prepare.plot
#      - corresponding.data.outcomes
#      - target.ontology
#      - plot.which
#      - summary.type
#      - plot.year.lag.ratio
#    - execute.simplot:
#      - n.facet.rows

#  Add these parameters to the plot.simulations function, with
#  the exception of plot.which, which is "both"

# Questions for Todd and Andrew: October 22nd, 2024

# Andrew has some cleaning functionality at the beginning of sim plot;
# validation, etc.  Would it be ok if I extracted this functionality
# into another function and returned a list with the modified values?

# Which are the correct values for plot.which?

# Where should I setup the project for the new hierarchy to work?
# I have currently:
# JHEEM/code
#     -> jheem2
#     -> jheem_analyses
# JHEEM/cached
#     -> (cached objects)
# and my project is setup in the JHEEM directory

# simplot <- function(...,
#                     outcomes=NULL,
#                     corresponding.data.outcomes = NULL,
#                     split.by = NULL,
#                     facet.by = NULL,
#                     dimension.values = list(),
#                     target.ontology = NULL,
#                     plot.which = c('sim.and.data', 'sim.only')[1],
#                     summary.type = c('individual.simulation', 'mean.and.interval', 'median.and.interval')[1],
#                     plot.year.lag.ratio = F,
#                     title = "location",
#                     n.facet.rows = NULL,
#                     data.manager = get.default.data.manager(),
#                     style.manager = get.default.style.manager(),
#                     debug = F)


plot.simulations <- function(...,
                            outcomes,
                            corresponding.data.outcomes = NULL,
                            split.by = NULL,
                            facet.by = NULL,
                            dimension.values = list(),
                            target.ontology = NULL,
                            summary.type = c('individual.simulation','mean.and.interval','median.and.interval')[1],
                            plot.year.lag.ratio = F,
                            title = "location",
                            n.facet.rows = NULL,
                            interval.coverate = 0.95,
                            data.manager = get.default.data.manager(),
                            style.manager = get.default.style.manager('plotly'))
{
  
    plot.which = "sim.and.data"

    plot.data = plot.data.validation(list(...), 
                                     match.call(expand.dots = F)$...,
                                     outcomes, 
                                     corresponding.data.outcomes, 
                                     plot.which, 
                                     summary.type)
    
    # These values are possibly modified by the plot.data.validation call, so 
    # they need to be extracted from the returned list.
    simset.list = plot.data$simset.list
    outcomes = plot.data$outcomes
    
    prepared.plot.data = prepare.plot(simset.list,
                                      outcomes=outcomes,
                                      locations = NULL,
                                      corresponding.data.outcomes = corresponding.data.outcomes,
                                      split.by=split.by,
                                      facet.by=facet.by,
                                      dimension.values=dimension.values,
                                      target.ontology=target.ontology,
                                      plot.which=plot.which,
                                      summary.type=summary.type,
                                      plot.year.lag.ratio=plot.year.lag.ratio,
                                      title = title,
                                      data.manager=data.manager,
                                      debug=F)
   
    execute.plotly.plot(prepared.plot.data,
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
execute.plotly.plot <- function(prepared.plot.data,
                            outcomes=NULL,
                            split.by=NULL,
                            facet.by=NULL,
                            plot.which = 'sim.and.data',
                            summary.type=c('individual.simulation', 'mean.and.interval', 'median.and.interval')[1],
                            plot.year.lag.ratio=F,
                            n.facet.rows=NULL,
                            style.manager=get.default.style.manager(),
                            debug=F)
{
  browser()
  
  # print(prepared.plot.data)
    
}

# plot.simulations(simset, "Incidence", data.manager = get.default.data.manager())