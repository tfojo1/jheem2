# We need access to the prepare.plot function from PLOTS_simplot.R

library(plotly)

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

  #-- UNPACK DATA --#
  df.sim=prepared.plot.data$df.sim
  df.truth=prepared.plot.data$df.truth
  y.label = prepared.plot.data$details$y.label
  plot.title = prepared.plot.data$details$plot.title
  
  #-- PREPARE PLOT COLORS, SHADES, SHAPES, ETC. --#
  
  # Here we can assume that we always have sim data and truth data
  # This is heavily borrowed and only slightly modified code from execute_simplot()
  
  # Here we are setting linetype, shape and colour for the simulation data
  df.sim['linetype.sim.by'] = df.sim[style.manager$linetype.sim.by]
  df.sim['shape.sim.by'] = df.sim[style.manager$shape.sim.by]
  df.sim['color.sim.by'] = df.sim[style.manager$color.sim.by]
  
  # make some other columns
  # Her we are setting attribute for the truth
  df.truth['location.type'] = locations::get.location.type(df.truth$location)
  df.truth['shape.data.by'] = df.truth[style.manager$shape.data.by]
  df.truth['color.data.by'] = df.truth[style.manager$color.data.by]
  df.truth['shade.data.by'] = df.truth[style.manager$shade.data.by]
  
  if (style.manager$color.data.by == 'stratum' && !is.null(df.truth$stratum) && all(df.truth$stratum=="")) {
    df.truth['color.and.shade.data.by'] = df.truth['shade.data.by']
  } else if (style.manager$shade.data.by == 'stratum' && !is.null(df.truth$stratum) && all(df.truth$stratum=="")) {
    df.truth['color.and.shade.data.by'] = df.truth['color.data.by']
  } else {
    df.truth['color.and.shade.data.by'] = do.call(paste, c(df.truth['shade.data.by'], df.truth['color.data.by'], list(sep="__")))
  }
  
  ## COLORS
  colors.for.sim = NULL
  color.data.primary.colors = NULL
  
  sim.color.groups = sort(unique(df.sim$color.sim.by))
  data.color.groups = sort(unique(df.truth$color.data.by))
  
  # if coloring by the same thing, use the same palette (defaulting to SIM's palette) unless one is missing
  if (style.manager$color.sim.by == style.manager$color.data.by) {
    all.color.groups = sort(union(sim.color.groups, data.color.groups))
      
    # Both df.sim and df.truth will be non-NULL here; I suspect that means
    # I should do the get.sim.colors() call (as that is how the code would
    # got in the original) but will leave it as-is for now
    
    if (!is.null(df.sim)) {
      all.colors = style.manager$get.sim.colors(length(all.color.groups))
    } else if (!is.null(df.truth)) {
      all.colors = style.manager$get.data.colors(length(all.color.groups))
    } else {
      all.colors = NULL # doesn't matter?
    }
      
    names(all.colors) = all.color.groups
    colors.for.sim = all.colors[sim.color.groups]
    color.data.primary.colors = all.colors[data.color.groups]
  } else {
    # otherwise, assign colors individually
    colors.for.sim = style.manager$get.sim.colors(length(sim.color.groups))
    names(colors.for.sim) = sim.color.groups
    color.data.primary.colors = style.manager$get.data.colors(length(data.color.groups))
    names(color.data.primary.colors) = data.color.groups
  }
  
  ## RIBBON COLOR
  color.ribbon.by = NULL
  # For now I'm going to leave the ggplot2 call in; we will have the library loaded and 
  # I can use it until I find a suitable plotly replacement.
  color.ribbon.by = ggplot2::alpha(colors.for.sim, style.manager$alpha.ribbon)
  
  ## SHADES FOR DATA
  color.data.shaded.colors = NULL
  color.data.shaded.colors = unlist(lapply(color.data.primary.colors, function(prim.color) {style.manager$get.shades(base.color=prim.color, length(unique(df.truth$shade.data.by)))}))
  names(color.data.shaded.colors) = do.call(paste, c(expand.grid(unique(df.truth$shade.data.by), unique(df.truth$color.data.by)), list(sep="__")))
  
  ## SHAPES
  shapes.for.data = NULL
  shapes.for.sim = NULL
  shapes.for.data = style.manager$get.shapes(length(unique(df.truth$shape.data.by)))
  names(shapes.for.data) = unique(df.truth$shape.data.by)
  shapes.for.sim = style.manager$get.shapes(length(unique(df.sim$shape.sim.by)))
  names(shapes.for.sim) = unique(df.sim$shape.sim.by)
  all.shapes.for.scale = c(shapes.for.data, shapes.for.sim)
  
  ## LINETYPES
  linetypes.for.sim = NULL
  linetypes.for.sim = style.manager$get.linetypes(length(unique(df.sim$linetype.sim.by)))
  names(linetypes.for.sim) = unique(df.sim$linetype.sim.by)
  
  ## GROUPS
  # break df.sim into two data frames, one for outcomes where the sim will be lines and the other for where it will be points
  groupids.with.one.member = setdiff(unique(df.sim$groupid), df.sim$groupid[which(duplicated(df.sim$groupid))])
  df.sim$groupid_has_one_member = with(df.sim, groupid %in% groupids.with.one.member)
  df.sim.groupids.one.member = subset(df.sim, groupid_has_one_member)
  df.sim.groupids.many.members = subset(df.sim, !groupid_has_one_member)

  # Draw the plots
  
  fig = plot_ly()
  # We plot the sim Data first.
  
  if (!is.null(split.by)) {
    # Split.by is per-plot; these are the groups that are being varied within the plot
    # For example, a plot with split.by = "race" would have multiple races in each plot
      
    #In this example plot, we have a df.sim.groupids.man.members > 0
    if (nrow(df.sim.groupids.many.members) > 0) {
          
      fig = plot_ly(
              type="scatter",
              x = df.sim.groupids.many.members$year,
              y = df.sim.groupids.many.members$value,
              # color = df.sim.groupids.many.members$groupid,
              color = df.sim.groupids.many.members$color.sim.by,
              color_continuous_scale = colors.for.sim,
              # transforms = list(
              #   list(
              #     type = "groupby",
              #     groups = df.sim.groupids.many.members$groupid,
              #     styles = list (
              #       list(target = 4, value = list(marker = list(color = 'blue'))),
              #       list(target = 2, value = list(marker = list(color = 'red'))),
              #       list(target = 6, value = list(marker = list(color = 'green')))
              #     )
              #
              #   ),
              mode = "lines+markers"
              )
        
      #       %>% 
      #       group_by (data = df.sim.groupids.many.members, age) %>%
      #     
      #       do(p=plot_ly(.)) %>%
      #       subplot(nrows = 2, shareX = TRUE, shareY = TRUE)
        
      # df.sim.groupids.many.members %>%
      #       group_by (age) %>%
      #       do(p = plot_ly(., x = ~year, y = ~value, color = ~color.sim.by, type = "scatter", mode="lines+markers")) %>%
      #       subplot(nrows = 2, shareX = TRUE, shareY = TRUE)
      
      # df.sim.groupids.many.members %>%
      #       group_by (age) %>%
      #       group_map(~ plot_ly(data=., x = ~year, y = ~value, color = ~color.sim.by, type = "scatter", mode="lines+markers"), .keep = TRUE) %>% 
      #       subplot(nrows = 2, shareX = TRUE, shareY = TRUE)
              
      
      # one_plot = function(d) {
      #     return (plot_ly(data = d, x = ~year, y = ~value, color = ~color.sim.by, type = "scatter", mode="lines") )
      # }
      # df.sim.groupids.many.members <- df.sim.groupids.many.members %>% group_by(age)
      # df.sim.groupids.many.members <- df.sim.groupids.many.members %>% do(mafig = one_plot(.))
      # fig <- df.sim.groupids.many.members %>% subplot(nrows = 2)
      
      one_plot = function(sim_data, world_data) {
          # print(world_data)
          fig = plot_ly()
          # Add the Simulation Trace
          fig <- fig %>% add_trace(data = sim_data, x = ~year, y = ~value, color = ~color.sim.by, type = "scatter", mode = "markers")
          # Add the Data Trace
          fig <- fig %>% add_trace(data = world_data, x = ~year, y = ~value, color = ~color.data.by, type = "scatter", mode = "lines")
          return (fig)
          # return (plot_ly(data = d, x = ~year, y = ~value, color = ~color.sim.by, type = "scatter", mode="lines") )
      }
      df.sim.groupids.many.members <- df.sim.groupids.many.members %>% group_by(age)
      df.sim.groupids.many.members <- df.sim.groupids.many.members %>% do(mafig = one_plot(., df.truth))
      fig <- df.sim.groupids.many.members %>% subplot(nrows = 2)
      
      jfig = plotly_json(fig, pretty = T)
      
      # browser()
      
      # Looking to use the list build:
      
      # Using the description retrieved from the add_trace() calls above, to get more than one plot
      # Seems to not be working, will try the simpler way found online 
      
      # fig = list (
      #   x = list (
      #     data = list (
      #         list (
      #             x = seq(2000,2025),
      #             y = c(150703.0,150703.0,150703.0,150703.0,150703.0,150703.0,150701.1,151067.1,152113.7,153732.0,155840.6,158379.5,161295.3,164538.1,168060.5,171816.7,175761.3,179848.9,184033.5,188268.1,192503.7,196693.0,200794.1,204774.1,208608.6,212281.6),
      #             type = "scatter",
      #             mode = "markers",
      #             marker = list(
      #                 color = "rgba(255,0,0,1)",
      #                 line = list (
      #                     color = "rgba(255,0,0,1)"
      #                 )
      #             )
      #         ),
      #         list (
      #             x = seq(2000,2025),
      #             y = c(21222.00,21222.00,21222.00,21222.00,21222.00,21222.00,21226.45,20675.77,19591.21,18583.49,17672.87,16849.59,16104.66,15429.89,14817.91,14262.17,13756.82,13296.71,12877.26,12494.43,12144.65,11824.71,11531.72,11263.05,11016.31,10789.32),
      #             type = "scatter",
      #             mode = "markers",
      #             marker = list(
      #                 color = "rgba(0,255,0,1)",
      #                 line = list (
      #                     color = "rgba(0,255,0,1)"
      #                 )
      #             )
      #         )
      #     )
      #   )
      # )
      
      fig = list(
          data = list (
              list (
                  x = seq(2000,2025),
                  y = c(21222.00,21222.00,21222.00,21222.00,21222.00,21222.00,21226.45,20675.77,19591.21,18583.49,17672.87,16849.59,16104.66,15429.89,14817.91,14262.17,13756.82,13296.71,12877.26,12494.43,12144.65,11824.71,11531.72,11263.05,11016.31,10789.32),
                  type = "scatter"
              ),
              list (
                  x = seq(2000,2025),
                  y = c(150703.0,150703.0,150703.0,150703.0,150703.0,150703.0,150701.1,151067.1,152113.7,153732.0,155840.6,158379.5,161295.3,164538.1,168060.5,171816.7,175761.3,179848.9,184033.5,188268.1,192503.7,196693.0,200794.1,204774.1,208608.6,212281.6),
                  type = "scatter"
              )
          ),
          layout = list (
                      title = 'A Figure Specified By R List',
                      plot_bgcolor='#e5ecf6', 
                      xaxis = list( 
                          zerolinecolor = '#ffff', 
                          zerolinewidth = 2, 
                          gridcolor = 'ffff'), 
                      yaxis = list( 
                          zerolinecolor = '#ffff', 
                          zerolinewidth = 2, 
                          gridcolor = 'ffff')
              # ), list (
              #         title = 'A Second Figure',
              #         plot_bgcolor='#e5acf6', 
              #         xaxis = list( 
              #             zerolinecolor = '#ffff', 
              #             zerolinewidth = 2, 
              #             gridcolor = 'ffff'), 
              #         yaxis = list( 
              #             zerolinecolor = '#ffff', 
              #             zerolinewidth = 2, 
              #             gridcolor = 'ffff')
              )
          )
            
      plotly_build(fig)
      
      # Define the traces
      trace1 <- list(
          x = rnorm(100),
          type = "histogram",
          name = "Histogram 1",
          xaxis = "x1",
          yaxis = "y1"
      )
      
      trace2 <- list(
          x = rnorm(100, mean = 5),
          type = "histogram",
          name = "Histogram 2",
          xaxis = "x1",
          yaxis = "y1"
      )
      
      trace3 <- list(
          x = 1:10,
          y = (1:10)^2,
          type = "scatter",
          mode = "lines",
          name = "Line 1",
          xaxis = "x2",
          yaxis = "y2"
      )
      
      trace4 <- list(
          x = 1:10,
          y = (1:10)^3,
          type = "scatter",
          mode = "lines",
          name = "Line 2",
          xaxis = "x2",
          yaxis = "y2"
      )
      
      trace5 <- list(
          z = volcano,
          type = "heatmap",
          name = "Heatmap 1",
          xaxis = "x3",
          yaxis = "y3"
      )
      
      trace6 <- list(
          z = matrix(runif(100, min = -2, max = 2), nrow = 10),
          type = "heatmap",
          name = "Heatmap 2",
          xaxis = "x4",
          yaxis = "y4"
      )
      
      # Define the layout
      layout <- list(
          title = "Multiple Figures with Multiple Traces",
          grid = list(rows = 2, columns = 2, pattern = "independent"),
          xaxis = list(title = "Histogram X-Axis", domain = c(0, 0.45), anchor = "y1"),
          yaxis = list(title = "Histogram Y-Axis", domain = c(0.55, 1), anchor = "x1"),
          xaxis2 = list(title = "Scatter X-Axis", domain = c(0.55, 1), anchor = "y2"),
          yaxis2 = list(title = "Scatter Y-Axis", domain = c(0.55, 1), anchor = "x2"),
          xaxis3 = list(title = "Heatmap 1 X-Axis", domain = c(0, 0.45), anchor = "y3"),
          yaxis3 = list(title = "Heatmap 1 Y-Axis", domain = c(0, 0.45), anchor = "x3"),
          xaxis4 = list(title = "Heatmap 2 X-Axis", domain = c(0.55, 1), anchor = "y4"),
          yaxis4 = list(title = "Heatmap 2 Y-Axis", domain = c(0, 0.45), anchor = "x4")
      )
      
      # Combine traces and layout into a single object
      plot_data <- list(trace1, trace2, trace3, trace4, trace5, trace6)
      plot_object <- list(data = plot_data, layout = layout)
      
      # Build the plotly object
      final_plot <- plotly_build(plot_object)
      
      # Render the plot
      final_plot
      
      # print (fig)
    }
     
    browser()
  }

  
}