
#'@export
create.style.manager = function(color.sim.by = 'stratum',
                                color.data.by = 'location.type', # change it to location type
                                shade.data.by = 'location',
                                linetype.sim.by = 'simset',
                                shape.data.by = 'source',
                                shape.sim.by = 'simset',
                                sim.palette = ggsci::pal_jama(),
                                data.palette = ggsci::pal_jama(),
                                linetypes = c("solid", "dashed", "dotted","dotdash"),
                                shapes = 21:25,
                                shade.increment = -16)
{
    JHEEM.STYLE.MANAGER$new(color.sim.by = color.sim.by,
                            color.data.by = color.data.by,
                            shade.data.by = shade.data.by,
                            linetype.sim.by = linetype.sim.by,
                            shape.data.by = shape.data.by,
                            shape.sim.by = shape.sim.by,
                            sim.palette = sim.palette,
                            data.palette = data.palette,
                            linetypes = linetypes,
                            shapes = shapes,
                            shade.increment = shade.increment)
}

#'@export
get.default.style.manager = function(type=c('ggplot','plotly')[1])
{
    if (!is.character(type) || length(type)!=1 || is.na(type))
        stop("Cannot get.default.style.manager() - 'type' must be a single, non-NA, character value")
    if (type!='ggplot' && type!='plotly')
        stop("Cannot get.default.style.manager() - 'type' must be either 'ggplot' or")
    
    DEFAULT.STYLE.MANAGERS[[type]]
}

#'@export
set.default.style.manager = function(style.manager,
                                     type=c('ggplot','plotly')[1])
{
    if (!is.character(type) || length(type)!=1 || is.na(type))
        stop("Cannot set.default.style.manager() - 'type' must be a single, non-NA, character value")
    if (type!='ggplot' && type!='plotly')
        stop("Cannot set.default.style.manager() - 'type' must be either 'ggplot' or 'plotly'")
    
    if (!is(style.manager, 'jheem.style.manager'))
            stop("Cannot set.default.style.manager() - 'style.manager' must be an object of class 'jheem.style.manager', as created by the function create.style.manager()")
    
    DEFAULT.STYLE.MANAGERS[[type]] = style.manager
}

# Possiblities for <x>.sim.by are
#   - "simulation" - each simulation set gets a different value for the attribute <x>
#   - "stratum" - each stratum - ie, level of 'split.by' - gets a different value
# Possibilites for <x>.data.by are
#   - "source" - each data source from the data manager gets a different value for the attribute <x>
#   - "location" - each data location gets a different value
#   - "location.type" - each type of location in the data (as given by locations::get.location.type) gets a different value
#   - "stratum" - each stratum - ie level of split by - gets a different value
JHEEM.STYLE.MANAGER = R6::R6Class(
    'jheem.style.manager',
    
    public = list(
        
        initialize = function(color.sim.by,
                              color.data.by,
                              shade.data.by,
                              linetype.sim.by,
                              shape.data.by,
                              shape.sim.by,
                              sim.palette,
                              data.palette,
                              linetypes,
                              shapes,
                              shade.increment)
        {
            # need to do error checking
            
            private$i.color.sim.by = color.sim.by
            private$i.color.data.by = color.data.by
            private$i.shade.data.by = shade.data.by
            private$i.shape.data.by = shape.data.by
            private$i.shape.sim.by = shape.sim.by
            private$i.linetype.sim.by = linetype.sim.by
            
            private$i.sim.palette = sim.palette
            private$i.data.palette = data.palette
            private$i.linetypes = linetypes
            private$i.shapes = shapes
            private$i.shade.increment = shade.increment
        },
        
        get.sim.colors = function(n)
        {
            private$do.get.color(palette = private$i.sim.palette, n = n)
        },
        
        get.data.colors = function(n)
        {
            private$do.get.color(palette = private$i.sim.palette, n = n)
        },
        
        get.shades = function(base.color, n)
        {
            rgb.base = col2rgb(base.color)
            
            reds = pmin(255, pmax(0, rgb.base[1,1] + (1:n-1)*private$i.shade.increment))
            greens = pmin(255, pmax(0, rgb.base[2,1] + (1:n-1)*private$i.shade.increment))
            blues = pmin(255, pmax(0, rgb.base[3,1] + (1:n-1)*private$i.shade.increment))
            
            rv = sapply(1:n, function(i){
                rgb(red=reds[i]/255,
                    green=greens[i]/255,
                    blue=blues[i]/255,
                    alpha=1)
            })
            
            substr(rv, 1, 7)
        },
        
        get.linetypes = function(n, for.plotly=F)
        {
            private$do.get.from.vector(values = private$i.linetypes, n = n)
        },
        
        get.shapes = function(n, for.plotly=F)
        {
            private$do.get.from.vector(values = private$i.shapes, n = n)
        }
    ),
    
    active = list(
        
        color.sim.by = function(value)
        {
            if (missing(value))
                private$i.color.sim.by
            else
                stop("Cannot overwrite a style.manager's 'color.sim.by' - it is read only")
        },
        
        color.data.by = function(value)
        {
            if (missing(value))
                private$i.color.data.by
            else
                stop("Cannot overwrite a style.manager's 'color.sim.by' - it is read only")
        },
        
        shade.data.by = function(value)
        {
            if (missing(value))
                private$i.shade.data.by
            else
                stop("Cannot overwrite a style.manager's 'color.sim.by' - it is read only")
        },
        
        linetype.sim.by = function(value)
        {
            if (missing(value))
                private$i.linetype.sim.by
            else
                stop("Cannot overwrite a style.manager's 'color.sim.by' - it is read only")
        },
        
        shape.data.by = function(value)
        {
            if (missing(value))
                private$i.shape.data.by
            else
                stop("Cannot overwrite a style.manager's 'color.sim.by' - it is read only")
        },
        
        shape.sim.by = function(value)
        {
            if (missing(value))
                private$i.shape.sim.by
            else
                stop("Cannot overwrite a style.manager's 'shape.sim.by' - it is read only")
        }
    ),
    
    private = list(
        
        i.color.sim.by = NULL,
        i.color.data.by = NULL,
        i.shade.data.by = NULL,
        i.linetype.sim.by = NULL,
        i.shape.data.by = NULL,
        i.shape.sim.by= NULL,
        
        i.sim.palette = NULL,
        i.data.palette = NULL,
        i.linetypes = NULL,
        i.shapes = NULL,
        i.shade.increment = NULL,
        
        do.get.from.vector = function(values, n)
        {
            if (length(values)==0)
                stop("The values must have at least one element")
            
            rep(values, ceiling(n/length(values)))[1:n]
        },
        
        do.get.color = function(palette, n)
        {
            if (is.character(palette))
                private$do.get.from.vector(values = palette, n = n)
            else if (is.function(palette))
            {
                colors = palette(n)
                if (!is.character(colors))
                    stop("The palette should return a character vector")
                private$do.get.from.vector(values = colors, n = n)
            }
            else
                stop("'palette' must be either a character vector or a function that takes ")
        }
    )
)



DEFAULT.STYLE.MANAGERS = new.env()
set.default.style.manager(create.style.manager(), 'ggplot')
set.default.style.manager(create.style.manager(), 'plotly')
