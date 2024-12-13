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

  # Plotly uses 'dash' instead of 'dashed' for dashed lines, so 
  # convert the value in linetypes.for.sim
  linetypes.for.sim = gsub ("dashed", "dash", linetypes.for.sim)
  
  # Helper function definition
  collect.traces.for.facet = function (split.categories, 
                                      data.for.this.facet, 
                                      local.split.by, 
                                      trace.column, 
                                      marker.type, 
                                      current.facet) {
    rv = list()
    for (spl.cat in split.categories) {
      data.for.this.trace = subset(data.for.this.facet,
                                     data.for.this.facet[[local.split.by]] == spl.cat)
      # One trace for each category
        category.list = unique(data.for.this.trace[[trace.column]])
        traces = lapply (category.list, function(trace_id) {
          trace.data = subset(data.for.this.trace,
                              data.for.this.trace[[trace.column]] == trace_id)
              
          clean.group.id = gsub("^population_|_1_.*$", "", trace_id) #This will do nothing if the pattern isn't found
          d = list (
            type = "scatter",
            mode = paste0(marker.type, "s"),
            name = clean.group.id,
            x = trace.data$year,
            y = trace.data$value,
            xaxis = paste0("x", current.facet),
            yaxis = paste0("y", current.facet)
          )
          if (marker.type == "line") {
            d[[marker.type]] = list (
              dash = linetypes.for.sim[[clean.group.id]],
              color = colors.for.sim[[spl.cat]]
            )
          } else if (marker.type == "marker") {
            d[[marker.type]] = list (
              color = colors.for.sim[[spl.cat]]
              #  Add additional information here; shape of marker, size
            )
          }
          d    
        })
        
        rv = append(rv, traces)
    } # End of splits
    rv
  }
  # Draw the plots
      
  # Define the list structure
  # Initialize the plotly object as a list
  fig <- list(data = list(), layout = list())
  
  
  # Add labels for the y-axis and a title for the plot
  fig$layout$title <- list(text = plot.title)
  
  
  # Remove alpha guide (no direct equivalent in Plotly)
  # Nothing to do for alpha guides since they don’t exist in Plotly
  
  # if (!plot.year.lag.ratio) {
  #     # Set y-axis limits and format labels with commas
  #     rv$layout$yaxis <- modifyList(rv$layout$yaxis, list(range = c(0, NULL), tickformat = ","))
  # } else {
  #     # Format y-axis labels with commas
  #     rv$layout$yaxis <- modifyList(rv$layout$yaxis, list(tickformat = ","))
  # }
  
  # This will be changed if facet.by is set, but set it to 1 initially
  figure.count = 1
  facet.categories = NULL
  all.traces = list ()
  figures.per.row = 3
  
  # SIMULATION ELEMENTS
  if (!is.null(df.sim)) {
      if (!is.null(split.by)) {
          if (nrow(df.sim.groupids.many.members) > 0) {
              # Add lines for multiple simulation groups
              # we know split.by has a value
              split.categories = unique (df.sim.groupids.many.members[[split.by]])
              # At this point we don't know if facet.by is non-null
              if (is.null(facet.by)) {
                print("facet.by is null")
                # TODO
                # If it is null, we want only one figure
                # Add as many traces to the figure as we have split.categories
                  
              } else {
                current.facet = 1
                # If it is non null, we want multiple figures within this plot
                facet.categories = unique (df.sim.groupids.many.members[[facet.by]])
                figure.count = length(facet.categories)
                # For each figure, assign the split.by traces
                for (fac.cat in facet.categories) {
                    data.for.this.facet = subset(df.sim.groupids.many.members,
                                                 df.sim.groupids.many.members[[facet.by]] == fac.cat)
                    # For each facet, we need to collect the trace for each split.by category
                    traces = collect.traces.for.facet(split.categories, data.for.this.facet, split.by, "groupid", "line",current.facet)
                    current.facet = current.facet + 1
                    fig$data = append(fig$data, traces)
                } # End of facets
              }
          }
          
          if (nrow(df.sim.groupids.one.member) > 0) {
              # Add points for single-member groups
              print ("df.sim.groupids.one.member > 0")
              # TODO - incomplete example :
              # trace <- list(
              #     type = "scatter",
              #     mode = "markers",
              #     x = df.sim.groupids.one.member$year,
              #     y = df.sim.groupids.one.member$value,
              #     marker = list(
              #         color = df.sim.groupids.one.member$color.sim.by,
              #         symbol = df.sim.groupids.one.member$shape.sim.by,
              #         size = 8
              #     )
              # )
              # rv$data <- append(rv$data, list(trace))
          }
          
          if (summary.type != 'individual.simulation') {
              print ("summary.type != 'individual.simulations")
              # TODO - incomplete example :
              # # Add ribbons for simulation confidence intervals
              # trace <- list(
              #     type = "scatter",
              #     mode = "lines",
              #     x = c(df.sim.groupids.many.members$year, rev(df.sim.groupids.many.members$year)),
              #     y = c(df.sim.groupids.many.members$value.upper, rev(df.sim.groupids.many.members$value.lower)),
              #     fill = "tonexty",
              #     fillcolor = df.sim.groupids.many.members$color.sim.by,
              #     line = list(color = "transparent"),
              #     opacity = style.manager$alpha.ribbon
              # )
              # rv$data <- append(rv$data, list(trace))
          }
      } else {
          # Split.by is null; no splits on the plots
          print ("Split.by is null")
          if (nrow(df.sim.groupids.many.members) > 0) {
              print ("nrow(df.sim.groupids.many.members) > 0")
              # TODO
              # Add lines for grouped simulation data
              # trace <- list(
              #     type = "scatter",
              #     mode = "lines",
              #     x = df.sim.groupids.many.members$year,
              #     y = df.sim.groupids.many.members$value,
              #     line = list(color = df.sim.groupids.many.members$color.sim.by),
              #     opacity = df.sim.groupids.many.members$alpha
              # )
              # rv$data <- append(rv$data, list(trace))
          }
          
          if (nrow(df.sim.groupids.one.member) > 0) {
              print ("nrow(df.sim.groupids.one.member) > 0")
              # TODO
              # Add points for single-member simulation groups
              # trace <- list(
              #     type = "scatter",
              #     mode = "markers",
              #     x = df.sim.groupids.one.member$year,
              #     y = df.sim.groupids.one.member$value,
              #     marker = list(
              #         color = df.sim.groupids.one.member$color.sim.by,
              #         symbol = df.sim.groupids.one.member$shape.sim.by,
              #         size = 8
              #     )
              # )
              # rv$data <- append(rv$data, list(trace))
          }
          
          if (summary.type != 'individual.simulation') {
              print ("summary.type != 'individual.simulations'")
              # TODO
              # Add ribbons for simulation confidence intervals
              # trace <- list(
              #     type = "scatter",
              #     mode = "lines",
              #     x = c(df.sim.groupids.many.members$year, rev(df.sim.groupids.many.members$year)),
              #     y = c(df.sim.groupids.many.members$value.upper, rev(df.sim.groupids.many.members$value.lower)),
              #     fill = "tonexty",
              #     fillcolor = df.sim.groupids.many.members$color.sim.by,
              #     line = list(color = "transparent"),
              #     opacity = style.manager$alpha.ribbon
              # )
              # rv$data <- append(rv$data, list(trace))
          }
      }
  } #End of df.sim traces
  
  # DATA ELEMENTS
  if (!is.null(df.truth)) {
      if (!is.null(split.by)) {
          # Add points for truth data with split groups
          # Add lines for multiple simulation groups
          # we know split.by has a value
          split.categories = unique (df.truth$stratum)
          # At this point we don't know if facet.by is non-null
          if (is.null(facet.by)) {
            print("facet.by is null")
            # TODO
            # If it is null, we want only one figure
            # Add as many traces to the figure as we have split.categories
              
          } else {
            current.facet = 1
            # If it is non null, we want multiple figures within this plot
            facet.categories = unique (df.sim.groupids.many.members$facet.by1)
            # For each figure, assign the split.by traces
            for (fac.cat in facet.categories) {
                data.for.this.facet = subset(df.truth,
                                             df.truth$facet.by1 == fac.cat)
                # For each facet, we need to collect the trace for each split.by category
                traces = collect.traces.for.facet(split.categories, data.for.this.facet, "stratum", "stratum", "marker",current.facet)
                current.facet = current.facet + 1
                fig$data = append(fig$data, traces)
            } # End of facets
          }
          
          # trace <- list(
          #     type = "scatter",
          #     mode = "markers",
          #     x = df.truth$year,
          #     y = df.truth$value,
          #     marker = list(
          #         color = df.truth$color.and.shade.data.by,
          #         symbol = df.truth$shape.data.by,
          #         size = 8
          #     )
          # )
          # rv$data <- append(rv$data, list(trace))
      } else {
          # Add points for truth data without split groups
          print ("Truth no Split.by")
          # trace <- list(
          #     type = "scatter",
          #     mode = "markers",
          #     x = df.truth$year,
          #     y = df.truth$value,
          #     marker = list(
          #         color = df.truth$color.and.shade.data.by,
          #         symbol = df.truth$shape.data.by,
          #         size = 8
          #     )
          # )
          # rv$data <- append(rv$data, list(trace))
      }
  }
  
  # At this point we have processed all the traces and now need to lay them out
  
  # How many figures do we need? One for each facet.
  if (figure.count > 1) {
    # How many full rows of figures do we have?
    plot.rows = ceiling(figure.count / figures.per.row)
    
    fig$layout$grid = list(rows = plot.rows, columns = figures.per.row, pattern = "independent")
    fig$layout$annotations = list()
    
    # Layout constants
    x_i = 1
    y_i = 1
    buffer = 0.05
    xdelta = 1 / figures.per.row
    ydelta = 1 / plot.rows
    
    for (i in 1:figure.count) {
        # Start at the beginning
        x_left = ((x_i - 1) * xdelta) + buffer
        x_right = (x_i * xdelta) - buffer
        y_left = (1-((y_i - 1) * ydelta))+ buffer
        y_right = (1-(y_i * ydelta)) - buffer
        
        fig$layout[[paste0("xaxis", i)]] = list(title = "years", domain = c(x_left,x_right), anchor = paste0("y",i))
        fig$layout[[paste0("yaxis", i)]] = list(title = y.label, domain = c(y_left,y_right), anchor = paste0("x",i))
        fig$layout$annotations = append ( fig$layout$annotations, list(list ( 
            text = facet.categories[i],
            showarrow = F,
            xref = "paper",
            yref = "paper"
            # x = #halfway between the right and the left side of the figure
            # y = #A buffer-space away from the top 
        ))) 
        
        if (x_i == figures.per.row) {
            x_i = 1
            y_i = y_i + 1
        } else {
            x_i = x_i + 1
        }
    }
    
    
    
  } else {
      print("Layout for a single figure")
      # TODO
  }
  
  # 
  # # FACETING
  # if (is.null(facet.by)) {
  #     # Define faceting by outcome display name
  #     rv$layout$facet <- list(row = ~outcome.display.name)
  # } else {
  #     # Define faceting by outcome display name and additional variables
  #     rv$layout$facet <- list(row = ~outcome.display.name, col = paste(facet.by, collapse = " + "))
  # }
  # 
  # if (plot.year.lag.ratio) {
  #     # Set the x-axis label to "latter year"
  #     rv$layout$xaxis <- list(title = "latter year")
  # }
  
  # Return the final plot object
  plotly_build(fig)
  
  browser()
  # Start with the sim traces
  
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
 
    browser()

  
}
