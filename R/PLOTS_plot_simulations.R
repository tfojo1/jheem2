# We need access to the prepare.plot function from PLOTS_simplot.R

#library(plotly)

#source("R/PLOTS_simplot.R")

# Need access to the Data Manager

#source("R/DATA_MANAGER_data_manager.R")

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
                            style.manager = get.default.style.manager('plotly'),
                            hide.legend = FALSE)
{
  
    plot.which = "sim.and.data"
    
    simset = list(...)[[1]]

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
                        debug=debug,
                        hide.legend = hide.legend)
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
                            debug=F,
                            hide.legend = FALSE)
{

  #-- UNPACK DATA --#
  df.sim=prepared.plot.data$df.sim
  df.truth=prepared.plot.data$df.truth
  # browser()
  y.label = prepared.plot.data$details$y.label
  plot.title = prepared.plot.data$details$plot.title
  outcome.metadata = prepared.plot.data$details$outcome.metadata.list
  
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

  # PLOTLY PLOTS
  
  # browser()
  plotly.debug = TRUE
  # Plotly uses 'dash' instead of 'dashed' for dashed lines, so 
  # convert the value in linetypes.for.sim
  linetypes.for.sim = gsub ("dashed", "dash", linetypes.for.sim)
  # Mapping the ggplot marker shapes into plotly
  marker.mappings = unlist(lapply(shapes.for.data, function(gg_shape) {
    if (gg_shape == 21) return ('circle') # Circle
    if (gg_shape == 22) return ('square') # Square
    if (gg_shape == 23) return ('diamond') # Diamond
    if (gg_shape == 24) return ('triangleup') # Triangle UP
    if (gg_shape == 25) return ('triangledown') # Triangle DOWN
  }))
  
  sim.trace.count = 0
  trace.in.legend = list()
  
  # Helper function definition
  inner.collector = function(cat.list, 
                             data.for.this.facet, 
                             trace.column, 
                             marker.type, 
                             current.facet) {
    if (plotly.debug) {
      cat(paste0("cat.list: ",cat.list,"\ntrace.column: ", trace.column, "\ncurrent.facet: ",current.facet,"\n"))
    }
    
    lapply( cat.list, function (trace_id) {
      trace.data = subset(data.for.this.facet,
                          data.for.this.facet[[trace.column]] == trace_id)
          
      # Don't pull from markers
      clean.group.id = trace_id #gsub("^[a-zA-Z\\.]+_|_1_.*$", "", trace_id) #This will do nothing if the pattern isn't found
      if (marker.type == "marker") {
        # browser()
      }
      
      base.trace = list (
        type = "scatter",
        mode = paste0(marker.type, "s"),
        name = clean.group.id,
        x = trace.data$year,
        y = trace.data$value,
        xaxis = paste0("x", current.facet),
        yaxis = paste0("y", current.facet)
      )
      
      if (marker.type == "line") {
        sim.trace.count <<- sim.trace.count + 1
        col = if(is.null(trace.data$line.color[1])) {
          colors.for.sim
        } else {
          trace.data$line.color[1]
        }
        
        mark = if(is.null(trace.data$line.shape[1])) {
          linetypes.for.sim[[clean.group.id]]
        } else {
          trace.data$line.shape[1]
        }
        # Does this combination of color and style already exist in the legend?
        if (!hide.legend) {
          trace.key = paste0(col, mark)
          if (!is.null(trace.in.legend[[trace.key]])) {
            base.trace$showlegend = FALSE
          } else {
            trace.in.legend[[trace.key]] <<- TRUE
          }
        } else {
          base.trace$showlegend = FALSE
        }
        
        base.trace[["line"]] = list (
          dash = mark,
          color = col
        )
        return (base.trace)
        
      } else if (marker.type == "marker") {
        unique.shapes = unique(trace.data$marker.shapes)
        
        
        marker.traces = lapply(unique.shapes, function(shape) {
          
          shape.data = subset(trace.data, marker.shapes == shape)
          
          col = if (is.null(shape.data$marker.color[1])) {
            colors.for.sim
          } else {
            shape.data$marker.color[1]
          }
          sym = if(is.null(shape.data$marker.shapes[1])) {
            'circle'
          } else {
            shape.data$marker.shapes[1]
          }
          
          trace = base.trace
          trace$showlegend = FALSE
          
          # Markers are unique because we need to enter the shape
          # and the color separately on the legend.  We need a white-filled
          # entry to denote the shape and another plotly shape to denote the color
          # something not used already by the library
          
          
          # What we can do is add a trace with no data.
          # This first one, we'll take out the color and add just the shape.
          # It's name will be the 'source' column.  The next one we'll 
          
          trace$x = shape.data$year
          trace$y = shape.data$value
          
          # Does this combination of color and style already exist in the legend?
          # trace.key = paste0(col, sym)
          # if (!is.null(trace.in.legend[[trace.key]])) {
          #   trace$showlegend = FALSE
          # } else {
          #   trace.in.legend[[trace.key]] <<- TRUE
          # }
          
          trace[["marker"]] = list (
            color = col,
            symbol = sym,
            line = list (
              color = "#202020",
              width = 1
            )
          )
          return (trace)
        })
        
        # Here we want to add one empty trace for the shape and color of the
        # marker, provided they are not already on display.  For the sims we
        # use a combination of shape and color to denote the key into the
        # trace.in.legend object, but here we should use something else as 
        # we are adding 'shape' and 'color' separately.  So instead we'll use
        # paste0("marker",color) and paste0("marker",shape)
        
        # browser()
        shape.traces = lapply(marker.traces, function(entry) {
                         working.symbol = entry$marker$symbol
                         shape.key.val = paste0("marker", working.symbol)
                         # Is there already an entry for this combo in the 
                         if (is.null(trace.in.legend[[shape.key.val]])) {
                          # Put the entry in
                           shape.trace = base.trace
                           shape.trace$x = c()
                           shape.trace$y = c()
                           shape.trace$name = "SHAPE"
                           shape.trace[["marker"]] = list (
                             color = "white",
                             symbol = working.symbol,
                             line = list (
                               color = "#202020",
                               width = 1
                             )
                           )
                           if (!hide.legend) {
                             trace.in.legend[[shape.key.val]] <<- TRUE
                           } else {
                             trace.in.legend[[shape.key.val]] <<- FALSE
                           }
                           return (shape.trace)
                         } 
                       })
        
        color.traces = lapply(marker.traces, function(entry) {
                         working.color = entry$marker$color
                         color.key.val = paste0("marker", working.color)
                         # Is there already an entry for this combo in the 
                         if (is.null(trace.in.legend[[color.key.val]])) {
                          # Put the entry in
                           color.trace = base.trace
                           color.trace$x = c()
                           color.trace$y = c()
                           color.trace$name = "COLOR"
                           color.trace[["marker"]] = list (
                             color = working.color,
                             symbol = "pentagon",
                             line = list (
                               color = "#202020",
                               width = 1
                             )
                           )
                           if (!hide.legend) {
                             trace.in.legend[[color.key.val]] <<- TRUE
                           } else {
                             trace.in.legend[[color.key.val]] <<- FALSE
                           }
                           return (color.trace)
                         } 
                       })
        
        # browser()
        merged.traces = append(append(marker.traces,shape.traces),color.traces)
        clean.traces = list()
        for (i in seq_along(merged.traces)) {
          if (!is.null(merged.traces[[i]])) {
            clean.traces = append(clean.traces, merged.traces[i])
          }
        }
        return (clean.traces)    
      }
    })
  }

  
  
  collect.traces.for.facet = function (split.categories, 
                                      data.for.this.facet, 
                                      local.split.by, 
                                      trace.column, 
                                      marker.type, 
                                      current.facet) {
    rv = list()
    if (is.null(split.categories)) {
      # No splits
      category.list = unique(data.for.this.facet[[trace.column]])
      raw.traces = inner.collector(category.list, 
                               data.for.this.facet, 
                               trace.column,
                               marker.type,
                               current.facet)
      # traces = unlist(raw.traces, recursive = FALSE)
      rv = append(rv, raw.traces)
    } else {
        # There are splits to collect
      for (spl.cat in split.categories) {
        data.for.this.trace = subset(data.for.this.facet,
                                     data.for.this.facet[[local.split.by]] == spl.cat)
        # One trace for each category
        category.list = unique(data.for.this.trace[[trace.column]])
        raw.traces = inner.collector(category.list, 
                                 data.for.this.facet, 
                                 trace.column,marker.type,
                                 current.facet)
        # traces = unlist(raw.traces, recursive = FALSE)
        rv = append(rv, raw.traces)
      } # End of splits
    }
    rv
  }
  
  #Helper for properly creating the y.axis labels for the figures
  y.axis.label.helper = function( outcome.metadata, outcome ) {
    label = outcome.metadata[[outcome]]$axis.name
    unit = outcome.metadata[[outcome]]$units
    # We want to prevent a 'Cases (cases)' situation here
    if (tolower(label) == tolower(unit)) {
      return (label)
    }
    return (paste0(label," (",unit,")"))
  }
  # Draw the plots
      
  # Define the list structure
  # Initialize the plotly object as a list
  fig <- list(data = list(), layout = list())
  
  
  # Add labels for the y-axis and a title for the plot
  fig$layout$title <- list(text = plot.title)
  
  # Each figure will need a y axis label, but that will be determined by the outcome,
  # So we should have a vector of y axis labels that the layout can use when laying
  # out the plot
  y.axis.labels = c()
  
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
  # figures.per.row = 3
  
  # SIMULATION ELEMENTS
  if (!is.null(df.sim)) {
    # Since this is simulation data, the marker type is "line"
    marker.type = "line"
    
    if (plotly.debug) {
      cat("SIM\n\n")
    }
    
    # browser()
    # Creating a new column in the df.sim.groupids.many.members datafrae called 
    # line.color, and assigning it the value of the color associated with the row
    df.sim.groupids.many.members$line.color = 
      unlist(lapply(df.sim.groupids.many.members$color.sim.by, function(val) {
        if (all(names(colors.for.sim) == "")) {
          return(colors.for.sim)
        }
        colors.for.sim[val]
      }))
    # Doing the same for line.shape (dashed vs solid)
    df.sim.groupids.many.members$line.shape = 
      unlist(lapply(df.sim.groupids.many.members$linetype.sim.by, function(val) {linetypes.for.sim[val]}))
    
    # Doing the same as above, but for the df.sim.groupids.one.member dataframe
    df.sim.groupids.one.member$line.color = 
      unlist(lapply(df.sim.groupids.one.member$color.sim.by, function(val) {
        if (all(names(colors.for.sim) == "")) {
          return(colors.for.sim)
        }
        colors.for.sim[val]
      }))
    
    df.sim.groupids.one.member$line.shape = 
      unlist(lapply(df.sim.groupids.one.member$linetype.sim.by, function(val) {linetypes.for.sim[val]}))
    
    if (!is.null(split.by)) {
      if (plotly.debug) {
        cat("split.by\n")
      }
      if (nrow(df.sim.groupids.many.members) > 0) {
        if (plotly.debug) {
          cat("  many members\n")
        }
        # Add lines for multiple simulation groups
        # we know split.by has a value
        split.categories = unique (df.sim.groupids.many.members[[split.by]])
        # At this point we don't know if facet.by is non-null
        if (is.null(facet.by)) {
          # print("facet.by is null")
          # TODO
          # If it is null, we want only one figure
          # Add as many traces to the figure as we have split.categories
            
        } else {
          if (plotly.debug) {
            cat("    facet by\n")
          }        
          current.facet = 1
          # If it is non null, we want multiple figures within this plot
          facet.categories = unique (df.sim.groupids.many.members[[facet.by]])
          fig$y.axis.labels = rep(y.axis.label.helper(outcome.metadata, outcomes[1]), length(facet.categories))
          figure.count = length(facet.categories)
          # For each figure, assign the split.by traces
          for (fac.cat in facet.categories) {
            data.for.this.facet = subset(df.sim.groupids.many.members,
                                         df.sim.groupids.many.members[[facet.by]] == fac.cat)
            # For each facet, we need to collect the trace for each split.by category
            traces = collect.traces.for.facet(split.categories, data.for.this.facet, split.by, "groupid", marker.type,current.facet)
            current.facet = current.facet + 1
            fig$data = append(fig$data, traces)
          } # End of facets
        }
      }
      
      if (nrow(df.sim.groupids.one.member) > 0) {
        if (plotly.debug) {
          cat("  one member\n")
        }        
        # Add points for single-member groups
        # print ("df.sim.groupids.one.member > 0")
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
        # RIBBONS
        if (plotly.debug) {
          cat("  not individual simulation\n")
        }        
        # print ("summary.type != 'individual.simulations")
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
      if (plotly.debug) {
        cat ("no split by\n")
      }        
      # Split.by is null; no splits on the plots
      if (nrow(df.sim.groupids.many.members) > 0) {
        if (plotly.debug) {
          cat ("  many members\n")
        }        
        # Collect the single trace
        if (is.null(facet.by)) {
          if (plotly.debug) {
            cat ("    no facet by\n")
          }        
          # No Faceting
          # single outcome vs multiple outcome
          # In multiple outcomes we have multiple "facets" (one
          # for each outcome)
          current.facet = 1
          
          # Add the proper y.axis.labels to the fig structure
          fig$y.axis.labels = unlist(lapply(outcomes, function(category) {
            y.axis.label.helper(outcome.metadata, category) 
          }))
          
          # browser()
          
          if (length(outcomes) > 1) {
            # Collect the trace for each outcome, treating each as a different facet    
            facet.categories = outcomes
            figure.count = length(facet.categories)
            
            # # Add the proper y.axis.labels to the fig structure
            # fig$y.axis.labels = unlist(lapply(facet.categories, function(category) {
            #   paste0(outcome.metadata[[category]]$axis.name," (",outcome.metadata[[category]]$units,")") 
            # }))
            # browser()
            
            # For each figure, assign the split.by traces
            for (fac.cat in facet.categories) {
              data.for.this.facet = subset(df.sim.groupids.many.members,
                                           df.sim.groupids.many.members[["outcome"]] == fac.cat)
              # For each facet, we need to collect the trace for each split.by category
              traces = collect.traces.for.facet(NULL, data.for.this.facet, split.by, "groupid", marker.type,current.facet)
              current.facet = current.facet + 1
              fig$data = append(fig$data, traces)
            } # End of facets
          } else {
            
            category.list = unique(df.sim.groupids.many.members[["groupid"]])
            
            
            raw.traces = inner.collector(category.list,
                            df.sim.groupids.many.members, 
                            "groupid",
                            marker.type,
                            current.facet)
            # traces = unlist(raw.traces, recursive = FALSE)
            fig$data = append(fig$data, raw.traces)
          }
        } else {
          if (plotly.debug) {
            cat ("    facet\n")
          }        
          # Faceting but no split by
          current.facet = 1
          # If it is non null, we want multiple figures within this plot
          facet.categories = unique (df.sim.groupids.many.members[[facet.by]])
          figure.count = length(facet.categories)
          fig$y.axis.labels = rep(y.axis.label.helper(outcome.metadata, outcomes[1]), figure.count)
          # For each figure, assign the split.by traces
          for (fac.cat in facet.categories) {
            data.for.this.facet = subset(df.sim.groupids.many.members,
                                         df.sim.groupids.many.members[[facet.by]] == fac.cat)
            # For each facet, we need to collect the trace for each split.by category
            traces = collect.traces.for.facet(NULL, data.for.this.facet, split.by, "groupid", marker.type,current.facet)
            current.facet = current.facet + 1
            fig$data = append(fig$data, traces)
          } # End of facets
        }
      }
      
      if (nrow(df.sim.groupids.one.member) > 0) {
        # print ("nrow(df.sim.groupids.one.member) > 0")
        # TODO
        # Add points for single-member simulation groups
        # trace = list(
        #   type = "scatter",
        #   mode = "markers",
        #   x = df.sim.groupids.one.member$year,
        #   y = df.sim.groupids.one.member$value,
        #   marker = list(
        #     color = df.sim.groupids.one.member$color.sim.by,
        #     symbol = df.sim.groupids.one.member$shape.sim.by,
        #     size = 8
        #   )
        # )
        # rv$data = append(rv$data, list(trace))
      }
      
      if (summary.type != 'individual.simulation') {
        # print ("summary.type != 'individual.simulations'")
        # TODO
        # Add ribbons for simulation confidence intervals
        # trace = list(
        #   type = "scatter",
        #   mode = "lines",
        #   x = c(df.sim.groupids.many.members$year, rev(df.sim.groupids.many.members$year)),
        #   y = c(df.sim.groupids.many.members$value.upper, rev(df.sim.groupids.many.members$value.lower)),
        #   fill = "tonexty",
        #   fillcolor = df.sim.groupids.many.members$color.sim.by,
        #   line = list(color = "transparent"),
        #   opacity = style.manager$alpha.ribbon
        # )
        # rv$data = append(rv$data, list(trace))
      }
    }
  } #End of df.sim traces
  
  if (plotly.debug) {
    cat("\nDATA\n\n")
    # browser()
  }        
  
  # DATA ELEMENTS
  if (!is.null(df.truth)) {
    marker.type = "marker"
    
    df.truth$marker.shapes = unlist(lapply(df.truth$shape.data.by, function(val) { marker.mappings[[val]] }))
    df.truth$marker.colors = unlist(lapply(df.truth$color.data.by, function(val) { color.data.primary.colors[val] }))
    
    # TODO Add reasoning for this assert or remove
    if (length(df.truth$marker.colors) != length(df.truth$marker.shapes)) {
      stop("df.truth: We cannot have different numbers of shapes and colors")
    }
    
    if (!is.null(split.by)) {
      if (plotly.debug) {
        cat("split by\n")
      }        
      # Add points for truth data with split groups
      # Add lines for multiple simulation groups
      # we know split.by has a value
      split.categories = unique (df.truth$stratum)
      # At this point we don't know if facet.by is non-null
      if (is.null(facet.by)) {
        if (plotly.debug) {
          cat("  no facet by\n")
        }        
        # print("facet.by is null")
        # TODO
        # If it is null, we want only one figure
        # Add as many traces to the figure as we have split.categories
          
      } else {
        if (plotly.debug) {
          cat("  facet by\n")
        }        
        # browser()
        current.facet = 1
        # If it is non null, we want multiple figures within this plot
        facet.categories = unique (df.sim.groupids.many.members$facet.by1)
        # For each figure, assign the split.by traces
        for (fac.cat in facet.categories) {
          data.for.this.facet = subset(df.truth,
                                       df.truth$facet.by1 == fac.cat)
          # For each facet, we need to collect the trace for each split.by category
          traces = collect.traces.for.facet(split.categories, data.for.this.facet, "stratum", "stratum", marker.type,current.facet)
          current.facet = current.facet + 1
          fig$data = append(fig$data, traces)
        } # End of facets
      }
    } else {
      if (plotly.debug) {
        cat("  no split by\n")
      }        
      # Add points for truth data without split groups
      # We have to be careful about multiple outcomes but no faceting
      current.facet = 1
      if (length(outcomes) > 1) {
        # print("Multiple Outcomes for Data")
        facet.categories = outcomes
        figure.count = length(facet.categories)
        # For each figure, assign the split.by traces
        for (fac.cat in facet.categories) {
          data.for.this.facet = subset(df.truth,
                                       df.truth[["outcome"]] == fac.cat)
          # For each facet, we need to collect the trace for each split.by category
          traces = collect.traces.for.facet(NULL, data.for.this.facet, split.by, "outcome", marker.type,current.facet)
          current.facet = current.facet + 1
          fig$data = append(fig$data, traces)
        } # End of facets
          
      } else {
        if (figure.count > 1) {
          # Facet these results
          current.facet = 1
          # print("Multiple Facet no split by")
          facet.categories = unique(df.truth[['facet.by1']])
          figure.count = length(facet.categories)
          # For each figure, assign the split.by traces
          for (fac.cat in facet.categories) {
            data.for.this.facet = subset(df.truth,
                                         df.truth[["facet.by1"]] == fac.cat)
            # For each facet, we need to collect the trace for each split.by category
            traces = collect.traces.for.facet(NULL, data.for.this.facet, split.by, "outcome", marker.type,current.facet)
            current.facet = current.facet + 1
            fig$data = append(fig$data, traces)
          } # End of facets
        } else {
          # Only one figure
          # We need to check here if there are multiple sources for truth data
          
          # browser()
          
          marker.types = unique(df.truth$color.data.by)
          # The way I understand this this could be either 
          # color.data.by or shape.data.by above
          
          # for (m.type in marker.types) {
          raw.traces = inner.collector(marker.types,
                                  df.truth,
                                  "color.data.by",
                                  marker.type,
                                  current.facet)
          # traces = unlist(raw.traces, recursive = FALSE)
          fig$data = append(fig$data,raw.traces)
        }
      }
    }
  }
  
  # browser()
  # LAYOUT
  # At this point we have processed all the traces and now need to lay them out
  
  # How many figures do we need? One for each facet.
  if (figure.count > 1) {
    figures.per.row = ceiling(sqrt(figure.count))
    # if (figure.count >= 3) {
    #   figures.per.row = 3
    # } else if (figure.count < 3) {
    #   figures.per.row = figure.count
    # }
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
      # 0,0 is the top left corner
      x_left = ((x_i - 1) * xdelta) + buffer
      x_right = (x_i * xdelta) - buffer
      y_bottom = (1-((y_i - 1) * ydelta))- buffer
      y_top = (1-(y_i * ydelta)) + buffer
        
      fig$layout[[paste0("xaxis", i)]] = list(title = "Years", domain = c(x_left,x_right), anchor = paste0("y",i))
      fig$layout[[paste0("yaxis", i)]] = list(title = fig$y.axis.label[i], domain = c(y_bottom,y_top), anchor = paste0("x",i))
      fig$layout$annotations = append ( fig$layout$annotations, list(list ( 
        text = facet.categories[i],
        showarrow = F,
        xref = paste0("x",i," domain"),
        yref = paste0("y",i," domain"),
        x = 0.5,
        y = 1.1,
        font = list (
          size = "14"
        )
      ))) 
        
      if (x_i == figures.per.row) {
        x_i = 1
        y_i = y_i + 1
      } else {
        x_i = x_i + 1
      }
    }
  } else {
    # browser()
    fig$layout[["xaxis"]] = list(title = "Years", anchor = "y1")
    fig$layout[["yaxis"]] = list(title = fig$y.axis.labels[1], anchor = "x1")
  }
  # browser()
  # The marker traces are nested one list() deep; this code will un-nest them
  sim.traces = fig$data[1:sim.trace.count]
  doubled.traces = fig$data[(sim.trace.count + 1):length(fig$data)]
  flattened.traces = unlist(doubled.traces, recursive = FALSE)
  fig$data = c(sim.traces, flattened.traces) 
  # browser()
  # print(fig)
  # Return the final plot object
  return(plotly_build(fig))
  
}
