


#this should match the constants defined at the top of diffeq.cpp
DIFFEQ.GROUP.INDICES = c(
    'infected' = 0,
    'uninfected' = 1
)


create.diffeq.settings <- function(jheem, error.prefix)
{
    #-- Set up the Overall Structure --#
    settings = list()
    
    #-- Pull the Specification and Metadata --#
    settings$specification.metadata = jheem$specification.metadata
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

    #-- Pull Dyanmic Outcomes --#
    
    specification = get.compiled.specification.for.version(jheem$version)
    outcomes = lapply(specification$outcome.names, specification$get.outcome)
    names(outcomes) = specification$outcome.names
    outcome.is.dynamic.or.intrinsic.mask = sapply(outcomes, is, 'dynamic.model.outcome') | sapply(outcomes, is, 'intrinsic.model.outcome')
    settings$outcomes = outcomes[outcome.is.dynamic.or.intrinsic.mask]
    settings$outcome.dim.names = lapply(settings$outcomes, function(outcome){
        settings$specification.metadata$apply.aliases(outcome$dim.names, error.prefix=error.prefix)
    })
    
    dynamic.outcomes = settings$outcomes[sapply(settings$outcomes, is, 'dynamic.model.outcome')]
    
    settings$outcome.names.by.core.component = lapply(settings$core.components, function(components){
        lapply(components, function(comp){
            
            outcome.applies.to.comp = sapply(dynamic.outcomes, comp$schema$dynamic.tracker.involves.component, comp=comp)
            names(dynamic.outcomes)[outcome.applies.to.comp]
        })
    })
    
    #-- Set up state/dx sizes and indices --#
    settings$state_and_dx_sizes = c(
        infected = prod(sapply(settings$ontologies$infected, length)),
        uninfected = prod(sapply(settings$ontologies$uninfected, length)),
        sapply(settings$outcomes, function(out){
            prod(sapply(out$dim.names, length))
        })
    )
    settings$state_and_dx_sizes = sapply(settings$state_and_dx_sizes, as.integer)
    
    settings$indices_into_state_and_dx = c(0, cumsum(settings$state_and_dx_sizes[-length(settings$state_and_dx_sizes)]))
    names(settings$indices_into_state_and_dx) = names(settings$state_and_dx_sizes)
    
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
    
    #-- Empty Holders --#
    settings$indices.for.initial.state.quantity = list()
    
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
                                    quantity.after.values,
                                    quantity.times,
                                    quantity.value.applies.mask,
                                    quantity.after.value.applies.mask,
                                    check.consistency,
                                    error.prefix)
{
    #-- Prepare for quantities --#
    settings = prepare.quantities.info(settings,
                                       quantity.values = quantity.values,
                                       quantity.after.values = quantity.after.values,
                                       quantity.times = quantity.times,
                                       quantity.value.applies.mask = quantity.value.applies.mask,
                                       quantity.after.value.applies.mask = quantity.after.value.applies.mask,
                                       check.consistency = check.consistency,
                                       error.prefix = error.prefix)
    
    #-- Prepare for each core component --#
    settings = prepare.initial.state(settings,
                                     quantity.values = quantity.values,
                                     quantity.dim.names = quantity.dim.names,
                                     check.consistency = check.consistency,
                                     error.prefix = error.prefix)
    settings = prepare.natality.info(settings, 
                                     quantity.dim.names = quantity.dim.names,
                                     check.consistency = check.consistency,
                                     error.prefix = error.prefix)
    settings = prepare.mortality.info(settings, 
                                      quantity.dim.names = quantity.dim.names,
                                      check.consistency = check.consistency,
                                      error.prefix = error.prefix)
    settings = prepare.transitions.info(settings, 
                                        quantity.dim.names = quantity.dim.names,
                                        check.consistency = check.consistency,
                                        error.prefix = error.prefix)
    settings = prepare.infections.info(settings, 
                                       quantity.dim.names = quantity.dim.names,
                                       check.consistency = check.consistency,
                                       error.prefix = error.prefix)
    settings = prepare.remission.info(settings, 
                                      quantity.dim.names = quantity.dim.names,
                                      check.consistency = check.consistency,
                                      error.prefix = error.prefix)

    #-- Prepare population trackers --#
    settings = prepare.population.trackers(settings,
                                           quantity.dim.names = quantity.dim.names,
                                           check.consistency = check.consistency,
                                           error.prefix = error.prefix)
    
    #-- Done: Return --#
    settings
}

notify.diffeq.settings.of.quantity.values.change = function(settings,
                                                            quantity.name,
                                                            check.consistency,
                                                            error.prefix)
{
    
    settings
}

notify.diffeq.settings.of.quantity.dim.names.change = function(settings,
                                                               quantity.name)
{
    
    # if it's an initial population value, we need to clear indices.for.initial.state.quantity[[group]]
    
    settings
}


prepare.initial.state <- function(settings, quantity.dim.names, quantity.values,
                                  check.consistency, error.prefix)
{
    need.to.update = T
    if (need.to.update)
    {
        if (is.null(settings$initial_state))
            settings$initial_state = numeric(settings$state_length)
        
        for (comp in settings$core.components$initial.population)
        {
            quantity.value = quantity.values[[comp$initial.population]][[1]]
            required.ontology = settings$specification$ontologies[[comp$group]]
            
            missing.dimensions = setdiff(names(required.ontology)[sapply(required.ontology, length)>1],
                                         names(dimnames(quantity.value)))
            
            if (length(missing.dimensions)>0)
                stop(paste0(error.prefix,
                            "Cannot set the initial population for '", comp$group, 
                            "' - the quantity '",
                            comp$initial.population, 
                            "' must include at least one value for ",
                            ifelse(length(missing.dimensions)==1, 'dimension ', 'dimensions '),
                            collapse.with.and("'", missing.dimensions, "'")))
            
            if (is.null(settings$indices.for.initial.state.quantity[[comp$group]]))
                settings$indices.for.initial.state.quantity[[comp$group]] = get.array.access.indices(arr.dim.names = required.ontology,
                                                                                                             dimension.values = dimnames(quantity.value))
            settings$initial.state = overwrite_arr(dst = settings$initial_state,
                                                   dst_indices = settings$indices.for.initial.state.quantity[[comp$group]],
                                                   src = quantity.value,
                                                   src_indices = 1:length(quantity.value))
        }
    }
    
    settings
}

prepare.natality.info <- function(settings, quantity.dim.names,
                                  check.consistency, error.prefix)
{
    
    settings
}

prepare.mortality.info <- function(settings, quantity.dim.names, 
                                   check.consistency, error.prefix)
{
    if (is.null(settings$mortality.info))
    {
        settings$mortality.info = list()
        need.to.update.mask = rep(T, length(settings$core.components$mortality))
    }
    else    # will eventually need to update this
        need.to.update.mask = rep(T, length(settings$core.components$mortality))
    
    settings$mortality.info[need.to.update.mask] = lapply((1:length(need.to.update.mask))[need.to.update.mask], function(i){
        
        comp = settings$core.components$mortality[[i]]
        group.ontology = settings$ontologies[[comp$group]]
        mortality.dim.names = comp$mechanism.dim.names$mortality.rate
        outcome.names = settings$outcome.names.by.core.component$mortality[[i]]
        
        if (check.consistency)
        {
            check.dim.names.can.expand(to.expand.dim.names = quantity.dim.names[[comp$mortality.rate]],
                                       target.dim.names = mortality.dim.names,
                                       to.expand.name = paste0("the mortality rate quantity for tag '", comp$tag, "' ('", comp$mortality.rate, "')"),
                                       target.name = "the expected mortality rate",
                                       error.prefix = paste0(error.prefix, "Mis-aligned dim.names for mortality quantity - "))
        }
        
        list(
            group = DIFFEQ.GROUP.INDICES[comp$group],
            quantity_index = settings$quantity.indices[comp$mortality.rate],
            state_indices = as.integer(get.array.access.indices(arr.dim.names = group.ontology, dimension.values = mortality.dim.names)-1),
            rate_indices = as.integer(get.expand.array.indices(quantity.dim.names[[comp$mortality.rate]],
                                                               target.dim.names = mortality.dim.names)-1),
            trackers = lapply(outcome.names, prepare.tracker,
                              comp = comp, 
                              settings = settings,
                              quantity.dim.names = quantity.dim.names,
                              check.consistency = check.consistency,
                              error.prefix = error.prefix)
        )
    })
    
    settings
}

prepare.transitions.info <- function(settings, quantity.dim.names, 
                                     check.consistency,error.prefix)
{
    
    settings
}


prepare.infections.info <- function(settings, quantity.dim.names, 
                                    check.consistency, error.prefix)
{
    
    settings
}


prepare.remission.info <- function(settings, quantity.dim.names,
                                   check.consistency, error.prefix)
{
    
    settings
}


prepare.fixed.strata.info <- function(settings, quantity.dim.names,
                                      check.consistency, error.prefix)
{
    
    settings
}

prepare.population.trackers <- function(settings, quantity.dim.names,
                                        check.consistency, error.prefix)
{
    # for now
    settings$population_trackers = list(NULL, NULL)
    
    settings
}

prepare.quantities.info <- function(settings,
                                    quantity.values, 
                                    quantity.after.values,
                                    quantity.times,
                                    quantity.value.applies.mask,
                                    quantity.after.value.applies.mask,
                                    check.consistency,
                                    error.prefix)
{
    #-- Prepare the quantities info --#
    
    quantity.names = names(settings$quantity.indices)
    
    need.to.update = T
    
    if (need.to.update)
    {
        #-- Calculate the scratch sizes needed for each quantity --#
        scratch.size.per.quantity = sapply(quantity.names, function(quantity.name){
            if (length(quantity.values[[quantity.name]])==0)
                stop(paste0(error.prefix, "There has been an error crunching quantity values - no values whatsoever for '", quantity.name, "' have been rendered"))
            else if (length(quantity.values[[quantity.name]])==1 && is.null(quantity.after.values[[quantity.name]][[1]]))
                0
            else
                length(quantity.values[[quantity.name]][[1]])
        })
        
        #-- Update the scratch offsets into the quantities info --#
        settings$quantity_scratch_offsets = c(0, cumsum(scratch.size.per.quantity[-length(scratch.size.per.quantity)]))
        names(settings$quantity_scratch_offsets) = quantity.names
        
        #-- Set the quantity scratch vector --#
        quantity.scratch.size = sum(scratch.size.per.quantity)
        if (is.null(settings$quantity_scratch_vector) || length(settings$quantity_scratch_vector)<quantity.scratch.size)
            settings$quantity_scratch_vector = numeric(quantity.scratch.size)
    }
    
    settings$quantities.info = lapply(names(settings$quantity.indices), function(quantity.name){
        rv = list(
            is.single.value = length(quantity.values[[quantity.name]])==1 && is.null(quantity.after.values[[quantity.name]][[1]]),
            times = quantity.times[[quantity.name]],
            values = quantity.values[[quantity.name]],
            after.values = quantity.after.values[[quantity.name]],
            value.applies = quantity.value.applies.mask[[quantity.name]],
            after.value.applies = quantity.after.value.applies.mask[[quantity.name]],
            scratch_offset = settings$quantity_scratch_offsets[quantity.name]
        )
        
        null.after = sapply(rv$after.values, is.null)
        rv$after.values[null.after] = rv$values[null.after]
        
        null.after.applies = sapply(rv$after.value.applies, is.null)
        rv$after.value.applies[null.after.applies] = rv$value.applies[null.after.applies]
        
        rv
        
    })
    names(settings$quantities.info) = names(settings$quantity.indices)
    
    settings
}

prepare.tracker <- function(outcome.name, comp, settings,
                            quantity.dim.names,
                            check.consistency, error.prefix)
{
    outcome = settings$outcomes[[outcome.name]]
    outcome.dim.names = settings$outcome.dim.names[[outcome.name]]
    group.ontology = settings$ontologies[[comp$group]]
    
    if (check.consistency)
    {
        if (!dim.names.are.subset(sub.dim.names = outcome.dim.names,
                                  super.dim.names = group.ontology))
            stop(paste0(error.prefix,
                        "Cannot set up outcome tracking for '", outcome.name,
                        "' - the dim.names for the outcome are not a subset of the dim.names for the '",
                        comp$group, "' group"))
    }
    
    # the subset of group.ontology that is needed to map outcome.dim.names
    intersected.dim.names = intersect.joined.dim.names(outcome.dim.names, group.ontology)
    if (!is.null(outcome$subset.dimension.values))
        intersected.dim.names = intersect.joined.dim.names(intersected.dim.names, outcome$subset.dimension.values)
    
    intersected.indices.into.group = get.array.access.indices(arr.dim.names = group.ontology,
                                                            dimension.values = intersected.dim.names)
    outcome.indices.into.intersected = get.array.access.indices(arr.dim.names = intersected.dim.names,
                                                                dimension.values = outcome.dim.names)
    
    intersected.indices.into.outcome = get.expand.array.indices(to.expand.dim.names = outcome.dim.names,
                                                                target.dim.names = intersected.dim.names)

    if (is.null(outcome$multiply.by))
    {
        multiply.by.quantity.index = -1
        intersected.indices.into.multiply.by = integer()
    }
    else
    {
        multiply.by.quantity.index = settings$quantity.indices[outcome$multiply.by]
        multiply.by.dim.names = quantity.dim.names[[multiply.by]]
        intersected.indices.into.multiply.by = get.expand.array.indices(to.expand.dim.names = multiply.by.dim.names,
                                                                        target.dim.names = intersected.dim.names)
    }
    
    list(
        offset_into_tracked = settings$state_and_dx_sizes[outcome.name],
        n = settings$state_and_dx_sizes[comp$group],
        group = DIFFEQ.GROUP.INDICES[comp$group],
        multiply_by_quantity_index = multiply.by.quantity.index,
        state_indices = as.integer(intersected.indices.into.group[outcome.indices.into.intersected] - 1),
        track_indices = as.integer(intersected.indices.into.outcome - 1),
        multiply_by_indices = as.integer(intersected.indices.into.multiply.by - 1)
    )
}

can.get.outcome.value.from.ode.output <- function(outcome.name,
                                              settings)
{
    any(names(settings$outcomes)==outcome.name)
}

get.outcome.value.from.ode.output <- function(outcome.name,
                                              settings,
                                              ode.results,
                                              outcome.years)
{
    outcome = settings$outcomes[[outcome.name]]
    first.ode.year = ode.results[1,1]
    
    cols = 1 + settings$indices_into_state_and_dx[outcome.name] + 1:settings$state_and_dx_sizes[outcome.name]
    # The "1 + ..." is because the first column is "time"
    
    rows = outcome.years - first.ode.year + 1
    
    if (outcome$is.cumulative)
    {
        rv = lapply(rows, function(r){
            as.numeric(ode.results[r+1,cols]) - as.numeric(ode.results[r,cols])
        })
    }
    else
    {
        rv = lapply(rows, function(r){
            as.numeric(ode.results[r,cols])
        })
    }
    
    names(rv) = as.character(outcome.years)
    rv
}
