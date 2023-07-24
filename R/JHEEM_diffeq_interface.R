

create.diffeq.settings <- function(engine, error.prefix)
{
    #-- Set up the Overall Structure --#
    settings = list()
    
    #-- Pull the Specification and Metadata --#
    settings$specification.metadata = engine$specification.metadata
    settings$specification = get.compiled.specification.for.version(settings$specification.metadata$version)
    
    #-- Set up Ontologies --#
    settings$ontologies = lapply(settings$specification$ontologies, 
                                 settings$specification.metadata$apply.aliases,
                                 error.prefix=error.prefix)
    
    #-- Set up Core Components --#
    settings$core.components = compile.and.sort.core.components.for.location(specification = settings$specification,
                                                                             specification.metadata = settings$specification.metadata,
                                                                             ontologies = settings$ontologies,
                                                                             error.prefix = error.prefix)

    #-- Set up state/dx sizes and indices --#
    settings$state_and_dx_sizes = c(
        infected = prod(sapply(settings$ontologies$infected, length)),
        uninfected = prod(sapply(settings$ontologies$uninfected, length)),
        tracked_transitions = 0,
        tracked_births = 0,
        tracked_mortality = 0,
        tracked_incidence = 0,
        tracked_remission = 0,
        tracked_population = 0
    )
    settings$state_and_dx_sizes = sapply(settings$state_and_dx_sizes, as.integer)
    
    settings$indices_into_state_and_dx = cumsum(settings$state_and_dx_sizes) - settings$state_and_dx_sizes[1]
    
    settings$state_length = sum(settings$state_and_dx_sizes)
    
    #-- Set up Quantity Indices --#
    settings$quantity.indices = 1:length(settings$specification$quantity.names) - 1
    names(settings$quantity.indices) = settings$specification$quantity.names

    #-- Prepare Fixed Strata Info --#
    settings = prepare.fixed.strata.info(settings)
    
    #-- Prepare Scratch Vector --#
    # See comment in diffeq.cpp, above compute_dx
    #  for info on how big the scratch vector needs to be
    scratch.size = settings$state_and_dx_sizes['infected'] +
        max(settings$state_and_dx_sizes['infected'] + settings$state_and_dx_sizes['uninfected']) +
        settings$state_and_dx_sizes['uninfected'] +
        max(settings$state_and_dx_sizes['infected'] + settings$state_and_dx_sizes['uninfected'])
    
    settings$scratch.vector = numeric(scratch.size)
    
    #-- Prepare Need-to-change storage --#
    settings$need.to.change = list(
        quantities = T
    )
    
    #-- Done: Return --#
    settings
}


prepare.diffeq.settings <- function(settings,
                                    quantity.dim.names,
                                    quantity.values,
                                    error.prefix)
{
    #-- Prepare for quantities --#
    settings = prepare.quantities.info(settings,
                                       quantity.values = quantity.values)
    
    #-- Prepare for each core component --#
    settings = prepare.initial.state(settings,
                                     quantity.values = quantity.values,
                                     quantity.dim.names = quantity.dim.names,
                                     error.prefix = error.prefix)
    settings = prepare.natality.info(settings, 
                                     quantity.dim.names = quantity.dim.names,
                                     error.prefix = error.prefix)
    settings = prepare.mortality.info(settings, 
                                      quantity.dim.names = quantity.dim.names,
                                      error.prefix = error.prefix)
    settings = prepare.transitions.info(settings, 
                                        quantity.dim.names = quantity.dim.names,
                                        error.prefix = error.prefix)
    settings = prepare.infections.info(settings, 
                                       quantity.dim.names = quantity.dim.names,
                                       error.prefix = error.prefix)
    settings = prepare.remission.info(settings, 
                                      quantity.dim.names = quantity.dim.names,
                                      error.prefix = error.prefix)

    #-- Prepare population trackers --#
    settings = prepare.population.trackers(settings,
                                           quantity.dim.names = quantity.dim.names,
                                           error.prefix = error.prefix)
    
    #-- Done: Return --#
    settings
}

notify.diffeq.settings.of.quantity.values.change = function(settings,
                                                            quantity.name)
{
    
    settings
}

notify.diffeq.settings.of.quantity.dim.names.change = function(settings,
                                                               quantity.name)
{
    
    settings
}


prepare.initial.state <- function(settings, quantity.dim.names, quantity.values, error.prefix)
{
    need.to.update = T
    if (need.to.update)
    {
        if (is.null(settings$initial_state))
            settings$initial_state = numeric(settings$state_length)
        
        for (comp in settings$core.components$initial.population)
        {
            quantity.value = quantity.values[[comp$initial.population]][[1]]
            if (length(quantity.value) != settings$state_and_dx_sizes[comp$group])
                stop(paste0(error.prefix,
                            "Cannot set the initial population for '", comp$group, 
                            "' - expected a vector of length ",
                            settings$state_and_dx_sizes[comp$group],
                            ", but calculated quantity '",
                            comp$initial.population,
                            "' had length ", length(quantity.value)))
            
            settings$initial.state = overwrite_arr_with_offsets(dst = settings$initial_state,
                                                                dst_offset = settings$indices_into_state_and_dx[[comp$group]] + 1,
                                                                src = quantity.value,
                                                                src_offset = 1,
                                                                len = length(quantity.value))
        }
    }
    
    settings
}

prepare.natality.info <- function(settings, quantity.dim.names, error.prefix)
{
    
    settings
}

prepare.mortality.info <- function(settings, quantity.dim.names, error.prefix)
{
    if (is.null(settings$mortality.info))
    {
        settings$mortality.info = list()
        need.to.update.mask = rep(T, length(settings$core.components$mortality))
    }
    else    # will eventually need to update this
        need.to.update.mask = rep(T, length(settings$core.components$mortality))
    
    settings$mortality.info[need.to.update.mask] = lapply(settings$core.components$mortality[need.to.update.mask], function(comp){
        
        group.ontology = settings$ontologies[[comp$group]]
        mortality.dim.names = comp$mechanism.dim.names$mortality.rate
        
        list(
            group = comp$group,
            quantity_index = settings$quantity.indices[comp$mortality.rate],
            state_indices = as.integer(get.array.access.indices(arr.dim.names = group.ontology, dimension.values = mortality.dim.names)),
            rate_indices = as.integer(get.expand.array.indices(quantity.dim.names[[comp$mortality.rate]],
                                                               target.dim.names = mortality.dim.names)),
            trackers = list()
        )
    })
    
    settings
}

prepare.transitions.info <- function(settings, quantity.dim.names, error.prefix)
{
    
    settings
}


prepare.infections.info <- function(settings, quantity.dim.names, error.prefix)
{
    
    settings
}


prepare.remission.info <- function(settings, quantity.dim.names, error.prefix)
{
    
    settings
}


prepare.fixed.strata.info <- function(settings, quantity.dim.names, error.prefix)
{
    
    settings
}

prepare.population.trackers <- function(settings, quantity.dim.names, error.prefix)
{
    
}

prepare.quantities.info <- function(settings,
                                    quantity.values, 
                                    error.prefix)
{
    #-- Prepare the quantities info --#
    
    quantity.names = names(settings$quantity.indices)
    
    need.to.update = T
    
    if (need.to.update)
    {
        #-- Calculate the scratch sizes needed for each quantity --#
        scratch.size.per.quantity = sapply(quantity.names, function(quantity.name){
            if (length(quantity.values[[quantity.name]])==1)
                0
            else
                length(quantity.values[[quantity.name]])
        })
        
        #-- Update the scratch offsets into the quantities info --#
        settings$quantity_scratch_offsets = cumsum(scratch.size.per.quantity) - scratch.size.per.quantity[1]

        #-- Set the quantity scratch vector --#
        quantity.scratch.size = sum(scratch.size.per.quantity)
            
        if (is.null(settings$quantity_scratch_vector) || length(settings$quantity_scratch_vector)<quantity.scratch.size)
            settings$quantity_scratch_vector = numeric(quantity.scratch.size)
    }
    
    settings
}