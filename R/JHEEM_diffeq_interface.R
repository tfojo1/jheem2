


#this should match the constants defined at the top of diffeq.cpp
DIFFEQ.GROUP.INDICES = c(
    'infected' = 0,
    'uninfected' = 1
)


create.diffeq.settings <- function(jheem, 
                                   kernel,
                                   error.prefix)
{
    #-- Set up the Overall Structure --#
    settings = list()
    
    #-- Pull the Specification and Metadata --#
    settings$kernel = kernel
#    settings$specification = get.compiled.specification.for.version(settings$specification.metadata$version)
    
    #-- Set up Ontologies --#
    settings$ontologies = kernel$ontologies
    
    #-- Set up Core Components --#
    # settings$core.components = compile.and.sort.core.components.for.location(specification = settings$specification,
    #                                                                          specification.metadata = settings$specification.metadata,
    #                                                                          ontologies = settings$ontologies,
    #                                                                          error.prefix = error.prefix)
    settings$core.components = kernel$core.components

    #-- Pull Dynamic Outcomes --#
    outcome.names = union(c('infected','uninfected'), kernel$get.outcome.names.for.sub.version(jheem$sub.version)) #make sure infected and uninfected come first
    outcomes = lapply(outcome.names, kernel$get.outcome.kernel)
    names(outcomes) = outcome.names
    
    outcome.is.dynamic.or.intrinsic.mask = vapply(outcomes, function(outcome){outcome$is.dynamic}, FUN.VALUE = logical(1)) |
        vapply(outcomes, function(outcome){outcome$is.intrinsic}, FUN.VALUE = logical(1))
    settings$outcomes = outcomes[outcome.is.dynamic.or.intrinsic.mask]
    settings$outcome.dim.names = lapply(settings$outcomes, function(outcome){
        outcome$dim.names
    })
    
    dynamic.outcomes = settings$outcomes[vapply(settings$outcomes, function(outcome){outcome$is.dynamic}, FUN.VALUE = logical(1))]
    
    settings$outcome.names.by.core.component = lapply(settings$core.components, function(components){
        lapply(components, function(comp){
            intersect(outcome.names, comp$outcome.names.that.apply)
        })
    })
    
    
    # Pull the population outcomes
    pop.outcomes.mask = vapply(dynamic.outcomes, function(outcome){outcome$dynamic.quantity.name}, FUN.VALUE=character(1))=='population'
    pop.outcomes = dynamic.outcomes[pop.outcomes.mask]
    settings$population.outcomes = list()
    
    for (group in names(DIFFEQ.GROUP.INDICES))
    {
        if (any(pop.outcomes.mask))
        {
            group.mask = sapply(pop.outcomes, function(outcome){any(outcome$groups==group)})
            settings$population.outcomes[[group]] = pop.outcomes[group.mask]
        }
        else
            settings$population.outcomes[[group]] = list()
    }
    
    settings$population.tracker.dependencies = lapply(settings$population.outcomes, function(pop.outcomes){
        lapply(pop.outcomes, function(outcome){
            outcome$multiply.by
        })
    })
    
    settings$population_trackers = lapply(settings$population.outcomes, function(pop.outcomes){
        list()
    })
    
    #-- Set up state/dx sizes and indices --#
    settings$state_and_dx_sizes = c(
 #       infected = prod(vapply(settings$ontologies$infected, length, FUN.VALUE = numeric(1))),
 #       uninfected = prod(vapply(settings$ontologies$uninfected, length, FUN.VALUE = numeric(1))),
        vapply(settings$outcomes, function(out){
            prod(vapply(out$dim.names, length, FUN.VALUE = numeric(1)))
        }, FUN.VALUE = numeric(1))
    )
    settings$state_and_dx_sizes = vapply(settings$state_and_dx_sizes, as.integer, integer(1))
    
    settings$indices_into_state_and_dx = c(0, cumsum(settings$state_and_dx_sizes[-length(settings$state_and_dx_sizes)]))
    names(settings$indices_into_state_and_dx) = names(settings$state_and_dx_sizes)
    
    settings$state_length = sum(settings$state_and_dx_sizes)

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
    settings$quantity.dim.names.have.changed = rep(T, length(settings$kernel$quantity.names))
    names(settings$quantity.dim.names.have.changed) = settings$kernel$quantity.names
    
    #-- Set up Dependency Tracking and Skeleton Core Component Info --#
    
    for (core.name in names(settings$core.components))
        settings[[paste0(core.name, '.info')]] = list()
    
    settings$core.component.dependencies = lapply(settings$core.components, function(core.comp){
        lapply(core.comp, function(comp){
            unlist(comp[comp$mechanism.types])
        })
    })
    
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
                                    prior.simulation.set,
                                    prior.sim.index,
                                    start.year,
                                    check.consistency,
                                    error.prefix)
{
    #-- Set up Quantity Indices --#
    #   (depends on whether there is a prior sim or not)
    
    if (is.null(prior.simulation.set))
        top.level.quantity.names = settings$kernel$top.level.quantity.names
    else
        top.level.quantity.names = settings$kernel$top.level.quantity.names.except.initial.population

    settings$quantity.indices = 1:length(top.level.quantity.names) - 1
    names(settings$quantity.indices) = top.level.quantity.names
    
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
    
    settings$need.to.update = lapply(settings$core.component.dependencies, function(core.comp.dependencies){
        vapply(core.comp.dependencies, function(dep){
            any(settings$quantity.dim.names.have.changed[dep])
        }, FUN.VALUE = logical(1))
    })
    
    settings = prepare.initial.state(settings,
                                     quantity.values = quantity.values,
                                     quantity.dim.names = quantity.dim.names,
                                     prior.simulation.set = prior.simulation.set,
                                     prior.sim.index = prior.sim.index,
                                     start.year = start.year,
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
    
    #-- Clear the quantity changed flags --#
    settings$quantity.dim.names.have.changed[] = F
    
    #-- Done: Return --#
    settings
}


notify.diffeq.settings.of.quantity.dim.names.change = function(settings,
                                                               quantity.name)
{
    settings$quantity.dim.names.have.changed[quantity.name] = T
    
    settings
}


prepare.initial.state <- function(settings,
                                  quantity.dim.names,
                                  quantity.values,
                                  prior.simulation.set,
                                  prior.sim.index,
                                  start.year,
                                  check.consistency,
                                  error.prefix)
{
    # Make sure we have a vector to fill in
    if (is.null(settings$initial_state))
        settings$initial_state = numeric(settings$state_length)
    
    if (is.null(prior.simulation.set))
    {    
        # Pull each core component touching the initial population    
        for (i in 1:length(settings$core.components$initial.population))
        {
            comp = settings$core.components$initial.population[[i]]
            quantity.name = comp$initial.population
            quantity.value = quantity.values[[quantity.name]][[1]]
            
            # Update the indices if dim.names have changed
            if (settings$need.to.update$initial.population[i])
            {
                required.ontology = settings$kernel$ontologies[[comp$group]]
                
                # Make sure no dimensions are missing
                missing.dimensions = setdiff(names(required.ontology)[vapply(required.ontology, length, numeric(1))>1],
                                             names(dimnames(quantity.value)))
                
                if (length(missing.dimensions)>0)
                    stop(paste0(error.prefix,
                                "Cannot set the initial population for '", comp$group, 
                                "' - the quantity '",
                                comp$initial.population, 
                                "' must include at least one value for ",
                                ifelse(length(missing.dimensions)==1, 'dimension ', 'dimensions '),
                                collapse.with.and("'", missing.dimensions, "'")))
                
                # Recalculate the indices
                settings$indices.for.initial.state.quantity[[comp$group]] = settings$indices_into_state_and_dx[comp$group] +
                    get.array.access.indices(arr.dim.names = required.ontology, dimension.values = dimnames(quantity.value))
            }
            
            # Always recalculate the values
            settings$initial_state = overwrite_arr(dst = settings$initial_state,
                                                   dst_indices = settings$indices.for.initial.state.quantity[[comp$group]],
                                                   src = quantity.value,
                                                   src_indices = 1:length(quantity.value))
        }
    }
    else
    {
        for (group in names(DIFFEQ.GROUP.INDICES))
        {
            src = prior.simulation.set[[group]]
            if (prior.simulation.set$n.sim>1 && !names(dimnames(src))[length(dimnames(src))]=='sim')
                stop("Cannot pull initial state from prior sim in diffeq_interface: the prior simulation set must have 'sim' as the last dimension")
            
            if(!names(dimnames(src))[length(dimnames(src))]=='sim')
                stop("Cannot pull initial state from prior sim in diffeq_interface: the prior simulation set must have 'year' as the first dimension")
            
            start.year.mask = dimnames(src)$year==as.character(start.year)
            if (!any(start.year.mask))
                stop(paste0("Cannot pull initial state from prior sim in diffeq_interface: the prior simulation set does not include the requested 'start.year' of ", start.year))
            
            
            n.year = as.numeric(dim(src)['year'])
            n.sim = as.numeric(dim(src)['sim'])
            n.src = length(src)
            
            dim(src) = c(year = n.year, other = n.src / n.year / n.sim, sim = n.sim)
            src = src[start.year.mask,,prior.sim.index]
            
            settings$initial_state = overwrite_arr(dst = settings$initial_state,
                                                   dst_indices = settings$indices_into_state_and_dx[group] + 1:length(src),
                                                   src = src,
                                                   src_indices = 1:length(src))
        }
    }
    
    settings
}


prepare.natality.info <- function(settings, quantity.dim.names,
                                  check.consistency, error.prefix)
{
    settings$natality.info[settings$need.to.update$natality] = 
        lapply((1:length(settings$need.to.update$natality))[settings$need.to.update$natality], function(i){
        
        # Pull component, relevant ontologies/dim.names and outcome.names
        comp = settings$core.components$natality[[i]]
        
        offspring.ontology = settings$ontologies[[comp$child.group]]
        parent.ontology = settings$ontologies[[comp$parent.group]]
        
        fertility.dim.names = comp$mechanism.dim.names$fertility.rate # the dimnames we need fertility values for
        birth.proportions.dim.names = comp$mechanism.dim.names$birth.proportions # the dimnames we need birth proportions for
        
        birth.proportions.quantity.dim.names = quantity.dim.names[[comp$birth.proportions]] # the dimnames that are actually in the birth.proportions quantity
        fertility.quantity.dim.names = quantity.dim.names[[comp$fertility.rate]] # the dimnames thare are actually in the fertility.rate quantity
        
        # Pull outcomes
        outcome.names = settings$outcome.names.by.core.component$natality[[i]]
        outcome.trackable.types = sapply(settings$outcomes[outcome.names], function(outcome){outcome$trackable.type})
        
#        if (i==1 && length(outcome.names)>0)
#            stop("*** NOTE *** We haven't tested tracking dynamic outcomes related to natality yet. It's possible we'll get errors here - contact Todd for debugging questions")
        
        # Set up the parent categories 
        #   - defined by the values of dimensions present in the parent ontology
        #     but missing from the birth.proportions ontology
        parent.category.dimensions = intersect(names(fertility.dim.names),
                                               union(gsub("\\.from", "", names(birth.proportions.quantity.dim.names)),
                                                     comp$parent.child.concordant.dimensions))
        
        if (length(parent.category.dimensions)==0)
            parent.categories = list(list())
        else
        {
            parent.category.dimension.values = intersect.joined.dim.names(fertility.dim.names[parent.category.dimensions],
                                                                           parent.ontology[parent.category.dimensions])
            parent.category.matrix = get.every.combination(parent.category.dimension.values)
            parent.categories = apply(parent.category.matrix, 1, as.list)
        }
        
        n.parent.categories = length(parent.categories)
        n.parent.compartments.per.parent.category = prod(vapply(fertility.dim.names, length, FUN.VALUE = numeric(1))) / n.parent.categories
        
        
        # Set up the indices for fertility/parent ontology
        
        if (check.consistency) # should theoretically not be necessary
        {
            check.dim.names.can.expand(to.expand.dim.names = fertility.quantity.dim.names,
                                       target.dim.names = fertility.dim.names,
                                       to.expand.name = paste0("the fertility rate quantity for tag '", comp$tag, "' ('", comp$fertility.rate, "')"),
                                       target.name = "the expected fertility rate",
                                       error.prefix = paste0(error.prefix, "Mis-aligned dim.names for fertility quantity - "))
        }
        
        
        fertility.to.parent.state.indices = get.array.access.indices(arr.dim.names = parent.ontology,
                                                                           dimension.values = fertility.dim.names)
        
        fertility.to.fertility.rate.indices = get.expand.array.indices(to.expand.dim.names = fertility.quantity.dim.names,
                                                                           target.dim.names = fertility.dim.names)
        
        fertility.indices.for.parent.categories = lapply(parent.categories, function(catg){
            get.array.access.indices(arr.dim.names = fertility.dim.names,
                                     dimension.values = catg)
        })
        
        fertility.rate.indices.for.parent.categories = lapply(fertility.indices.for.parent.categories, function(catg.to.fertility.indices){
            fertility.to.fertility.rate.indices[catg.to.fertility.indices] - 1
        })

        state.indices.for.parent.categories = lapply(fertility.indices.for.parent.categories, function(catg.to.fertility.indices){
            fertility.to.parent.state.indices[catg.to.fertility.indices] - 1
        })
        
        # Set up the indices into birth proportions
        
        all.born.into.dimensions = substr(names(comp$all.births.into.compartments), 1, nchar(names(comp$all.births.into.compartments))-3)
        dimensions.with.proportions = setdiff(names(offspring.ontology)[vapply(offspring.ontology, length, FUN.VALUE = numeric(1))>1],
                                              union(all.born.into.dimensions,
                                                    comp$parent.child.concordant.dimensions))
        if (length(dimensions.with.proportions)==0)
            n.offspring.compartments.per.parent.category = 1
        else
            n.offspring.compartments.per.parent.category = prod(vapply(offspring.ontology[dimensions.with.proportions], length, FUN.VALUE = numeric(1)))
        
        base.offspring.dim.names = offspring.ontology
        base.offspring.dim.names[all.born.into.dimensions] = comp$all.births.into.compartments

        offspring.dim.names.for.parent.categories = lapply(parent.categories, function(catg){
            dim.names = base.offspring.dim.names
            dim.names[comp$parent.child.concordant.dimensions] = catg[comp$parent.child.concordant.dimensions]
            
            dim.names
        })
        
        offspring.indices.for.parent.categories = lapply(offspring.dim.names.for.parent.categories, function(dim.names){
            get.array.access.indices(arr.dim.names = offspring.ontology,
                                     dimension.values = dim.names) - 1
        })
        
        base.birth.proportions.dimension.values = birth.proportions.quantity.dim.names
        base.birth.proportions.dimension.values[paste0(dimensions.with.proportions, '.to')] = base.offspring.dim.names[dimensions.with.proportions]
        variable.birth.proportion.dimensions.by.parent.category = intersect(substr(names(birth.proportions.quantity.dim.names), 1, nchar(names(birth.proportions.quantity.dim.names))-5),
                                                                            parent.category.dimensions)
        
        birth.proportion.indices.for.parent.categories = lapply(parent.categories, function(catg){
            dimension.values = base.birth.proportions.dimension.values
            if (length(variable.birth.proportion.dimensions.by.parent.category)>0)
                dimension.values[paste0(variable.birth.proportion.dimensions.by.parent.category, '.from')] = catg[variable.birth.proportion.dimensions.by.parent.category]
            if (length(dimension.values)==0)
                0
            else
                get.array.access.indices(arr.dim.names = birth.proportions.quantity.dim.names,
                                         dimension.values = dimension.values, 
                                         index.from = 0)
        })
        
        
        # Package it up
        
        list(
            from_group = DIFFEQ.GROUP.INDICES[comp$parent.group],
            to_group = DIFFEQ.GROUP.INDICES[comp$child.group],
            
            n_parent_categories = n.parent.categories,
            n_parent_compartments_per_parent_category = n.parent.compartments.per.parent.category,
            n_offspring_compartments_per_parent_category = n.offspring.compartments.per.parent.category,
            
            fertility_quantity_index = settings$quantity.indices[comp$fertility.rate],
            birth_proportions_quantity_index = settings$quantity.indices[comp$birth.proportions],
            
            state_indices_for_parent_categories = state.indices.for.parent.categories,
            fertility_rate_indices_for_parent_categories = fertility.rate.indices.for.parent.categories,
            
            offspring_indices_for_parent_categories = offspring.indices.for.parent.categories,
            birth_proportion_indices_for_parent_categories = birth.proportion.indices.for.parent.categories,
            
            from_birth_trackers = prepare.and.pare.trackers(outcome.names[outcome.trackable.types=='births.from'], 
                                         settings = settings,
                                         subset.dim.names = fertility.dim.names,
                                         quantity.dim.names = quantity.dim.names,
                                         group = comp$parent.group,
                                         check.consistency = check.consistency,
                                         error.prefix = error.prefix),
            
            by_incidence_trackers = prepare.and.pare.trackers(outcome.names[outcome.trackable.types=='incidence.by'], 
                                           settings = settings,
                                           quantity.dim.names = quantity.dim.names,
                                           subset.dim.names = fertility.dim.names,
                                           group = comp$parent.group,
                                           check.consistency = check.consistency,
                                           error.prefix = error.prefix),
            
            to_birth_trackers = prepare.and.pare.trackers(outcome.names[outcome.trackable.types=='births.to'], 
                                       settings = settings,
                                       quantity.dim.names = quantity.dim.names,
                                       subset.dim.names = base.offspring.dim.names,
                                       group = comp$child.group,
                                       check.consistency = check.consistency,
                                       error.prefix = error.prefix),
            
            to_incidence_trackers = prepare.and.pare.trackers(outcome.names[outcome.trackable.types=='incidence.to'], 
                                           settings = settings,
                                           quantity.dim.names = quantity.dim.names,
                                           subset.dim.names = base.offspring.dim.names,
                                           group = comp$child.group,
                                           check.consistency = check.consistency,
                                           error.prefix = error.prefix)
        )
    })
    
    
    settings
}

prepare.mortality.info <- function(settings, quantity.dim.names, 
                                   check.consistency, error.prefix)
{
    #-- Update whichever ones we need to update --#
    settings$mortality.info[settings$need.to.update$mortality] = 
        lapply((1:length(settings$need.to.update$mortality))[settings$need.to.update$mortality], function(i){
        
        comp = settings$core.components$mortality[[i]]
        group.ontology = settings$ontologies[[comp$group]]
        
        mortality.dim.names = comp$mechanism.dim.names$mortality.rate
        outcome.names = settings$outcome.names.by.core.component$mortality[[i]]
        
        if (check.consistency) # should theoretically not be needed
        {
            check.dim.names.can.expand(to.expand.dim.names = quantity.dim.names[[comp$mortality.rate]],
                                       target.dim.names = mortality.dim.names,
                                       to.expand.name = paste0("the mortality rate quantity for tag '", comp$tag, "' ('", comp$mortality.rate, "')"),
                                       target.name = "the expected mortality rate",
                                       error.prefix = paste0(error.prefix, "Mis-aligned dim.names for mortality quantity - "))
        }
        
        
        list(
            group = DIFFEQ.GROUP.INDICES[comp$group],
            n = prod(vapply(mortality.dim.names, length, FUN.VALUE = numeric(1))),
            quantity_index = settings$quantity.indices[comp$mortality.rate],
            state_indices = as.integer(get.array.access.indices(arr.dim.names = group.ontology, 
                                                                dimension.values = mortality.dim.names,
                                                                index.from = 0)),
            rate_indices = as.integer(get.expand.array.indices(quantity.dim.names[[comp$mortality.rate]],
                                                               target.dim.names = mortality.dim.names,
                                                               index.from = 0)),
            trackers = prepare.and.pare.trackers(outcome.names,
                              settings = settings,
                              quantity.dim.names = quantity.dim.names,
                              subset.dim.names = mortality.dim.names,
                              group = comp$group,
                              check.consistency = check.consistency,
                              error.prefix = error.prefix)
        )
    })
    
    settings
}

prepare.transitions.info <- function(settings, quantity.dim.names, 
                                     check.consistency,error.prefix)
{
    settings$transitions.info[settings$need.to.update$transition] = lapply((1:length(settings$need.to.update$transition))[settings$need.to.update$transition], function(i){
  
        comp = settings$core.components$transition[[i]]
        group.ontology = settings$ontologies[[comp$group]]
        
        trate.dim.names = comp$mechanism.dim.names$transition.rate
        trate.quantity.dim.names = quantity.dim.names[[comp$transition.rate]]
        names(trate.quantity.dim.names)[names(trate.quantity.dim.names)==comp$dimension] = paste0(comp$dimension, '.from')
        
        from.dim.names = trate.dim.names[setdiff(names(trate.dim.names), paste0(comp$dimension, '.to'))]
        names(from.dim.names)[names(from.dim.names)==paste0(comp$dimension, '.from')] = comp$dimension
        
        to.dim.names = from.dim.names
        to.dim.names[[comp$dimension]] = trate.dim.names[[paste0(comp$dimension, '.to')]]
        
        outcome.names = settings$outcome.names.by.core.component$transition[[i]]
        
        
        list(
            
            group = DIFFEQ.GROUP.INDICES[comp$group],
            quantity_index = settings$quantity.indices[comp$transition.rate],
            
            n = prod(vapply(from.dim.names, length, FUN.VALUE = numeric(1))),
            
            state_from_indices = get.array.access.indices(arr.dim.names = group.ontology,
                                                          dimension.values = from.dim.names,
                                                          index.from = 0),
            rate_indices = get.expand.array.indices(to.expand.dim.names = trate.quantity.dim.names,
                                                    target.dim.names = trate.dim.names,
                                                    index.from = 0),
            state_to_indices = get.array.access.indices(arr.dim.names = group.ontology,
                                                        dimension.values = to.dim.names,
                                                        index.from = 0),
            
            trackers = prepare.and.pare.trackers(outcome.names, 
                              settings = settings,
                              quantity.dim.names = quantity.dim.names,
                              group = comp$group,
                              subset.dim.names = from.dim.names,
                              check.consistency = check.consistency,
                              error.prefix = error.prefix)
        )
    })
        
    settings
}

prepare.infections.info <- function(settings, quantity.dim.names, 
                                    check.consistency, error.prefix)
{
    settings$infections.info[settings$need.to.update$transmission] = lapply((1:length(settings$need.to.update$transmission))[settings$need.to.update$transmission], function(i){
        
        comp = settings$core.components$transmission[[i]]
        
        # Pull transmissibility and susceptibility dim names
        
        transmissibility.dim.names = comp$mechanism.dim.names$transmissibility
        susceptibility.dim.names = comp$mechanism.dim.names$susceptibility
        
        transmissibility.quantity.dim.names = quantity.dim.names[[comp$transmissibility]]
        susceptibility.quantity.dim.names = quantity.dim.names[[comp$susceptibility]]
        
        infected.ontology = settings$ontologies$infected
        uninfected.ontology = settings$ontologies$uninfected
        

        # Parse contact matrix
   #     contact.dim.names = comp$mechanism.dim.names$contact
        contact.quantity.dim.names = quantity.dim.names[[comp$contact]]
        
        contact.from.dim.names = contact.quantity.dim.names[grepl(".from$", names(contact.quantity.dim.names))]
        contact.to.dim.names = contact.quantity.dim.names[setdiff(names(contact.quantity.dim.names), names(contact.from.dim.names))]
        
        names(contact.from.dim.names) = substr(names(contact.from.dim.names), 1, nchar(names(contact.from.dim.names))-5)
        names(contact.to.dim.names) = substr(names(contact.to.dim.names), 1, nchar(names(contact.to.dim.names))-3)
        
        n.from.contacts = prod(vapply(contact.from.dim.names, length, FUN.VALUE = numeric(1)))
        n.to.contacts = prod(vapply(contact.to.dim.names, length, FUN.VALUE = numeric(1)))
        
        n.compartments.per.from.contact = prod(vapply(transmissibility.dim.names, length, FUN.VALUE = numeric(1))) / n.from.contacts
        n.compartments.per.to.contact = prod(vapply(susceptibility.dim.names, length, FUN.VALUE = numeric(1))) / n.to.contacts
        
        # Parse new infection probabilities
        
        new.infection.proportions.dim.names = as.list(comp$mechanism.dim.names$new.infection.proportions)
        new.infection.proportions.dim.names[names(comp$all.new.infections.into.compartments)] = comp$all.new.infections.into.compartments
        new.infection.proportions.quantity.dim.names = quantity.dim.names[[comp$new.infection.proportions]]
        
        # Parse outcomes
        outcome.names = settings$outcome.names.by.core.component$transmission[[i]]
        outcome.trackable.types = sapply(settings$outcomes[outcome.names], function(outcome){outcome$trackable.type})
        
        # Set up indices for from/infected/transmitting
        if (length(contact.from.dim.names)>0)
            from.contact.categories = apply(get.every.combination(contact.from.dim.names), 1, as.list)
        else
            from.contact.categories = list(NULL)
        
        transmissibility.to.infected.indices = get.array.access.indices(arr.dim.names = infected.ontology,
                                                                        dimension.values = transmissibility.dim.names,
                                                                        index.from = 0)
        
        transmissibility.indices.per.from.contact = lapply(from.contact.categories, function(catg){
            get.array.access.indices(arr.dim.names = transmissibility.dim.names,
                                     dimension.values = catg)
        })
        
        state.indices.for.from.contacts = lapply(transmissibility.indices.per.from.contact, function(transmissibility.indices){
            transmissibility.to.infected.indices[transmissibility.indices]
        })
        
        transmissibility.to.quantity.indices = get.expand.array.indices(to.expand.dim.names = transmissibility.quantity.dim.names,
                                                                        target.dim.names = transmissibility.dim.names,
                                                                        index.from = 0)
        
        transmissibility.quantity.indices.for.from.contacts = lapply(transmissibility.indices.per.from.contact, function(transmissibility.indices){
            transmissibility.to.quantity.indices[transmissibility.indices]
        })
        
        
        # Set up indices for to/uninfected/susceptible
        if (length(contact.to.dim.names)>0)
            to.contact.categories = apply(get.every.combination(contact.to.dim.names), 1, as.list)
        else
            to.contact.categories = list(NULL)
            
        susceptibility.to.uninfected.indices = get.array.access.indices(arr.dim.names = uninfected.ontology,
                                                                        dimension.values = susceptibility.dim.names,
                                                                        index.from = 0)
        
        susceptibility.indices.per.to.contact = lapply(to.contact.categories, function(catg){
            get.array.access.indices(arr.dim.names = susceptibility.dim.names,
                                     dimension.values = catg)
        })
        
        state.indices.for.to.contacts = lapply(susceptibility.indices.per.to.contact, function(susceptibility.indices){
            susceptibility.to.uninfected.indices[susceptibility.indices]
        })
 
        susceptibility.to.quantity.indices = get.expand.array.indices(to.expand.dim.names = susceptibility.quantity.dim.names,
                                                                        target.dim.names = susceptibility.dim.names,
                                                                        index.from = 0)
        
        susceptibility.quantity.indices.for.to.contacts = lapply(susceptibility.indices.per.to.contact, function(susceptibility.indices){
            susceptibility.to.quantity.indices[susceptibility.indices]
        })
        
        # Set up denominator indices
        
        denominator.infected.indices.for.from.contacts = lapply(from.contact.categories, function(catg){
            get.array.access.indices(arr.dim.names = infected.ontology,
                                     dimension.values = catg,
                                     index.from = 0)
        })
        
        denominator.uninfected.indices.for.from.contacts = lapply(from.contact.categories, function(catg){
            get.array.access.indices(arr.dim.names = uninfected.ontology,
                                     dimension.values = catg,
                                     index.from = 0)
        })
        
        # Set up new infection proportions indices
        
        infected.dimensions = names(infected.ontology)
        uninfected.categories = apply(get.every.combination(uninfected.ontology), 1, as.list)
        
        # uninfected.categories.for.to.contacts[[to.contact]] is a list of n.compartments.per.to.contact categories
        uninfected.categories.for.to.contacts = lapply(state.indices.for.to.contacts, function(to.state.indices){
            uninfected.categories[1 + to.state.indices]
        })
        
        # new.infection.dim.names.for.to.contacts[[to.contact]] is a list of n.compartments.per.to.contact lists
        # new.infection.dim.names.for.to.contacts[[to.contact]][[to.compartment]] is a set of dim.names (which index into new.infection.proportions)
        new.infection.dim.names.for.to.contacts = lapply(uninfected.categories.for.to.contacts, function(uninfected.categories){
            lapply(uninfected.categories, function(catg){
                dim.names = new.infection.proportions.dim.names
                dim.names[names(catg)] = catg
                
                dim.names
            })
        })
        
        # new.infection.state.indices[[to.contact]] is a list of n.compartments.per.to.contact lists
        # new.infection.state.indices[[to.contact]][[to.compartment]] is an IntegerVector of indices into infected.ontology
        new.infection.state.indices = lapply(new.infection.dim.names.for.to.contacts, function(dim.names.for.to.compartments){
            lapply(dim.names.for.to.compartments, function(dim.names){
                get.array.access.indices(arr.dim.names = infected.ontology,
                                         dimension.values = dim.names[infected.dimensions],
                                         index.from=0)
            })
        })
        
        # new.infection.proportion.indices[[to.contact]] is a list of n.compartments.per.to.contact lists
        # new.infection.proportion.indices[[to.contact]][[to.compartment]] is an IntegerVector of indices into the new.infections.proportions.quantity
        new.infection.proportion.indices = lapply(new.infection.dim.names.for.to.contacts, function(dim.names.for.to.compartments){
            lapply(dim.names.for.to.compartments, function(dim.names){
                get.expand.array.indices(to.expand.dim.names = new.infection.proportions.quantity.dim.names,
                                         target.dim.names = dim.names,
                                         index.from=0)
            })
        })
        
        # Pull dim names into which new infections can go
        new.infection.dim.names = new.infection.proportions.dim.names[infected.dimensions]
        
        # Contact indices for to contacts
        contact.indices.for.to.contacts = lapply(to.contact.categories, function(catg){
            names(catg) = paste0(names(catg), ".to")
            get.array.access.indices(arr.dim.names = contact.quantity.dim.names, 
                                     dimension.values = catg,
                                     index.from = 0)
        })
        
        # Package it up
        list(
            
            contact_quantity_index = settings$quantity.indices[comp$contact],
            transmissibility_quantity_index = settings$quantity.indices[comp$transmissibility],
            susceptibility_quantity_index = settings$quantity.indices[comp$susceptibility],
            new_infection_proportions_quantity_index = settings$quantity.indices[comp$new.infection.proportions],
            
            n_from_contacts = n.from.contacts,
            n_to_contacts = n.to.contacts,
            
            state_indices_for_to_contacts = state.indices.for.to.contacts,
            susceptibility_indices_for_to_contacts = susceptibility.quantity.indices.for.to.contacts,
            
            state_indices_for_transmitting_from_contacts = state.indices.for.from.contacts,
            transmissibility_indices_for_transmitting_from_contacts = transmissibility.quantity.indices.for.from.contacts,
            
            denominator_infected_indices_for_from_contacts = denominator.infected.indices.for.from.contacts,
            denominator_uninfected_indices_for_from_contacts = denominator.uninfected.indices.for.from.contacts,
            
            new_infection_proportions_indices_for_to_contacts = new.infection.proportion.indices,
            new_infection_state_indices_for_to_contacts = new.infection.state.indices,
            
            contact_indices_for_to_contacts = contact.indices.for.to.contacts,
            
            by_incidence_trackers = prepare.and.pare.trackers(outcome.names[outcome.trackable.types=='incidence.by'], 
                                           settings = settings,
                                           quantity.dim.names = quantity.dim.names,
                                           subset.dim.names = transmissibility.dim.names,
                                           group = 'infected',
                                           check.consistency = check.consistency,
                                           error.prefix = error.prefix),
            
            to_incidence_trackers = prepare.and.pare.trackers(outcome.names[outcome.trackable.types=='incidence.to'], 
                                           settings = settings,
                                           quantity.dim.names = quantity.dim.names,
                                           subset.dim.names = new.infection.dim.names,
                                           group = 'infected',
                                           check.consistency = check.consistency,
                                           error.prefix = error.prefix),
            
            from_incidence_trackers = prepare.and.pare.trackers(outcome.names[outcome.trackable.types=='incidence.from'], 
                                           settings = settings,
                                           quantity.dim.names = quantity.dim.names,
                                           subset.dim.names = susceptibility.dim.names,
                                           group = 'uninfected',
                                           check.consistency = check.consistency,
                                           error.prefix = error.prefix)
        )
    })
    
    settings
}

#      $state_indices_for_to_contacts - A list of length n_to_contacts, where each element is an IntegerVector that gives indices into the uninfected state array for the individual compartments (that are susceptible) of the corresponding to_contact group
#      $susceptibility_indices_for_to_contacts - A list of length n_to_contacts, where each element is an IntegerVector that gives into the susceptibility quantity for the individual compartments (that are susceptible) of the corresponding to_contact group
#      
#      $state_indices_for_transmitting_from_contacts - A list of length n_from_contacts, where each element is an IntegerVector that gives indices into the infected state array for the individual compartments (that can transmit) of the corresponding from_contact group
#      $transmissibility_indices_for_transmitting_from_contacts - A list of length n_from_contacts, where each element is an IntegerVector that gives indices into the transmissibility quantity for the individual compartments (that can transmit) of the corresponding from_contact group
#      
#      $denominator_infected_indices_for_from_contacts - A list of length n_from_contacts, where each element is an IntegerVector that gives indices into the infected state array for the individual compartments (regardless of whether or not they can transmit) of the corresponding from_contact group
#      $denominator_uninfected_indices_for_from_contacts - A list of length n_from_contacts, where each element is an IntegerVector that gives indices into the UNinfected state array for the individual compartments of the corresponding from_contact group
#      
#      $new_infection_proportions_indices_for_to_contacts - A list of length n_to_contacts, where each element is a List of length n_to_compartments_per_to_contact, each element of which is an IntegerVector that gives indices into the new_infections_proportions quantity for each compartment from which new infections in this to compartment from this to contact are distributed
#      $new_infection_state_indices_for_to_contacts - A list of length n_to_contacts, where each element is a List of length n_to_compartments_per_to_contact, each element of which is an IntegerVector that gives indices into the infected state array for each compartment from which new infections in this to compartment from this to contact are distributed

prepare.remission.info <- function(settings, quantity.dim.names,
                                   check.consistency, error.prefix)
{
    #-- Update whichever ones we need to update --#
    settings$remission.info[settings$need.to.update$remission] = 
        lapply((1:length(settings$need.to.update$remission))[settings$need.to.update$remission], function(i){
            
            # if (i==1)
            #     stop("*** NOTE *** We haven't modeling remission yet. It's possible we'll get errors here - contact Todd for debugging questions")
            
            comp = settings$core.components$remission[[i]]
            from.ontology = settings$ontologies[['infected']]
            to.ontology = settings$ontologies[['uninfected']]
            
            remission.rate.dim.names = comp$mechanism.dim.names$remission.rate
            remission.proportions.dim.names = comp$mechanism.dim.names$remission.proportions
            
            remission.rate.quantity.dim.names = quantity.dim.names[[comp$remission.rate]]
            remission.proportions.quantity.dim.names = quantity.dim.names[[comp$remission.proportions]]
            
            outcome.names = settings$outcome.names.by.core.component$remission[[i]]
            outcome.trackable.types = sapply(settings$outcomes[outcome.names], function(outcome){outcome$trackable.type})
            
            if (check.consistency)
            {
                check.dim.names.can.expand(to.expand.dim.names = remission.rate.quantity.dim.names,
                                           target.dim.names = remission.rate.dim.names,
                                           to.expand.name = paste0("the remission rate quantity for tag '", comp$tag, "' ('", comp$remission.rate, "')"),
                                           target.name = "the expected remission rate",
                                           error.prefix = paste0(error.prefix, "Mis-aligned dim.names for remission quantity - "))
            }
            
            # Set up the indices for applying remission rates to infected compartments
            remission.rate.indices = get.expand.array.indices(to.expand.dim.names = remission.rate.quantity.dim.names,
                                                              target.dim.names = remission.rate.dim.names)
            from.state.indices = get.array.access.indices(arr.dim.names = from.ontology,
                                                          dimension.values = remission.rate.dim.names)
            
            from.categories = apply(get.every.combination(remission.rate.dim.names), 1, as.list)
            n.from = length(from.categories)
            
            # Set up the indices for distributing remissions into the uninfected state
            base.to.dim.names = to.ontology
            base.to.dim.names[names(comp$all.remissions.into.compartments)] = comp$all.remissions.into.compartments
            
            shared.to.from.dimensions = intersect(names(to.ontology), names(remission.rate.dim.names))
            n.to.per.from = prod(vapply(base.to.dim.names[setdiff(names(base.to.dim.names), shared.to.from.dimensions)], length, FUN.VALUE = numeric(1)))
            
            to.dim.names.per.from = lapply(from.categories, function(catg){
                
                to.dim.names = base.to.dim.names
                to.dim.names[shared.to.from.dimensions] = catg[shared.to.from.dimensions]
                
                to.dim.names
            })
            
            to.indices.for.from = lapply(to.dim.names.per.from,
                                         get.array.access.indices,
                                         arr.dim.names = to.ontology)
            
            # This was an error
#             unique.to.dimensions = setdiff(names(to.ontology), names(remission.rate.dim.names))
#             base.proportions.dim.names = remission.rate.dim.names
# #            names(base.proportions.dim.names) = paste0(names(base.proportions.dim.names), '.from')
#             unique.to.dim.names = base.to.dim.names[unique.to.dimensions]
#             names(unique.to.dim.names) = paste0(names(unique.to.dim.names), '.to')
#             base.proportions.dim.names = c(base.proportions.dim.names, unique.to.dim.names)
#             
#             proportions.dim.names.per.from = lapply(from.categories, function(catg){
#                 
#                 dim.names = base.proportions.dim.names
#                 dim.names[shared.to.from.dimensions] = catg[shared.to.from.dimensions]
#  #               dim.names[paste0(shared.to.from.dimensions, '.from')] = catg[shared.to.from.dimensions]
#                 
#                 dim.names
#             })
            
            proportions.indices.for.from = lapply(to.dim.names.per.from,
                       get.expand.array.indices,
                       to.expand.dim.names = remission.proportions.quantity.dim.names,
                       index.from = 0)
            
            list(
                remission_quantity_index = settings$quantity.indices[comp$remission.rate],
                proportions_quantity_index = settings$quantity.indices[comp$remission.proportions],
                
                remission_rate_indices = remission.rate.indices,
                from_state_indices = from.state.indices,
                
                to_indices_for_from = to.indices.for.from,
                proportions_indices_for_from = proportions.indices.for.from,
                
                n_from = n.from,
                n_to_per_from = n.to.per.from,
                
                from_trackers = prepare.and.pare.trackers(outcome.names[outcome.trackable.types=='remission.from'], 
                                                          settings = settings,
                                                          quantity.dim.names = quantity.dim.names,
                                                          subset.dim.names = remission.rate.dim.names,
                                                          group = 'infected',
                                                          check.consistency = check.consistency,
                                                          error.prefix = error.prefix),
                
                to_trackers = prepare.and.pare.trackers(outcome.names[outcome.trackable.types=='remission.to'],
                                                        settings = settings,
                                                        quantity.dim.names = quantity.dim.names,
                                                        subset.dim.names = base.to.dim.names,
                                                        group = 'uninfected',
                                                        check.consistency = check.consistency,
                                                        error.prefix = error.prefix)
            )
        })
    
    settings
}


prepare.fixed.strata.info <- function(settings, quantity.dim.names,
                                      check.consistency, error.prefix)
{
    if (is.null(settings$fixed.strata.info))
    {
        settings$fixed.strata.info = lapply(settings$kernel$fixed.strata.info, function(fixed.info){
            
            infected.ontology = settings$ontologies$infected
            uninfected.ontology = settings$ontologies$uninfected
            
            fixed.dim.names = infected.ontology[fixed.info$dimensions.to.fix]
            fixed.categories = apply(get.every.combination(fixed.dim.names), 1, as.list)
            
            n.fixed.strata = length(fixed.categories)
            n.uninfected.compartments.per.fixed.stratum = prod(vapply(uninfected.ontology, length, FUN.VALUE = numeric(1))) / n.fixed.strata
            n.infected.compartments.per.fixed.stratum = prod(vapply(infected.ontology, length, FUN.VALUE = numeric(1))) / n.fixed.strata
            
            uninfected.indices.for.stratum = lapply(fixed.categories, 
                                                    get.array.access.indices,
                                                    arr.dim.names = uninfected.ontology,
                                                    index.from = 0)
            
            infected.indices.for.stratum = lapply(fixed.categories, 
                                                  get.array.access.indices,
                                                  arr.dim.names = infected.ontology,
                                                  index.from = 0)
            list(
                applies_after_time = fixed.info$applies.after.time,
                applies_before_time = fixed.info$applies.before.time,
                fix_strata = fixed.info$fix.strata,
                
                n_fixed_strata = n.fixed.strata,
                n_uninfected_compartments_per_fixed_stratum = n.uninfected.compartments.per.fixed.stratum,
                n_infected_compartments_per_fixed_stratum = n.infected.compartments.per.fixed.stratum,
                
                uninfected_indices_for_stratum = uninfected.indices.for.stratum,
                infected_indices_for_stratum = infected.indices.for.stratum
            )
        })
    }
    
    settings
}

prepare.population.trackers <- function(settings, quantity.dim.names,
                                        check.consistency, error.prefix)
{
    # Set up need.to.update
    need.to.update = lapply(settings$population.tracker.dependencies, function(pop.dependencies){
        vapply(pop.dependencies, function(dep){
            any(settings$quantity.dim.names.have.changed[dep])
        }, FUN.VALUE = logical(1))
    })
    
    # for now
    for (group in names(need.to.update))
    {   
        trackers = lapply(settings$population.outcomes[[group]][ need.to.update[[group]] ], function(outcome){
            
            prepare.tracker(outcome.name = outcome$name, 
                            settings = settings,
                            quantity.dim.names = quantity.dim.names,
                            subset.dim.names = NULL,
                            group = group,
                            check.consistency = check.consistency,
                            error.prefix = error.prefix)
            
        })
        
        settings$population_trackers[[group]][ need.to.update[[group]] ] = 
            trackers[!vapply(trackers, is.null, FUN.VALUE = logical(1))]
    }
    
    
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
    
    need.to.update = any(settings$quantity.dim.names.have.changed[quantity.names])
 
    if (need.to.update)
    {
        #-- Calculate the scratch sizes needed for each quantity --#
        scratch.size.per.quantity = vapply(quantity.names, function(quantity.name){
            if (length(quantity.values[[quantity.name]])==0)
                stop(paste0(error.prefix, "There has been an error crunching quantity values - no values whatsoever for '", quantity.name, "' have been rendered"))
            else if (length(quantity.values[[quantity.name]])==1 && is.null(quantity.after.values[[quantity.name]][[1]]))
                0
            else
                length(quantity.values[[quantity.name]][[1]])
        }, FUN.VALUE = numeric(1))
        
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
        
        null.after = vapply(rv$after.values, is.null, FUN.VALUE = logical(1))
        rv$after.values[null.after] = rv$values[null.after]
        
        null.after.applies = vapply(rv$after.value.applies, is.null, FUN.VALUE = logical(1))
        rv$after.value.applies[null.after.applies] = rv$value.applies[null.after.applies]
        
        rv
        
    })
    names(settings$quantities.info) = names(settings$quantity.indices)
    
    settings
}

prepare.and.pare.trackers <- function(outcome.names,
                                      settings,
                                      quantity.dim.names,
                                      group,
                                      subset.dim.names,
                                      check.consistency,
                                      error.prefix)
{
    trackers = lapply(outcome.names, 
                      prepare.tracker,
                      settings = settings,
                      subset.dim.names = subset.dim.names,
                      quantity.dim.names = quantity.dim.names,
                      group = group,
                      check.consistency = check.consistency,
                      error.prefix = error.prefix)
    
    trackers[!vapply(trackers, is.null, FUN.VALUE = logical(1))]
}

prepare.tracker <- function(outcome.name, 
                            settings,
                            quantity.dim.names,
                            group,
                            subset.dim.names,
                            check.consistency,
                            error.prefix)
{
    outcome = settings$outcomes[[outcome.name]]
    outcome.dim.names = settings$outcome.dim.names[[outcome.name]]
    group.ontology = settings$ontologies[[group]]
    
    if (is.null(subset.dim.names))
        subset.dim.names = group.ontology
    
    if (check.consistency)
    {
        if (!dim.names.are.subset(sub.dim.names = subset.dim.names,
                                  super.dim.names = group.ontology))
            stop(paste0(error.prefix,
                        "Cannot set up outcome tracking for '", outcome.name,
                        "' - the dim.names of interest for tracking to the outcome are not a subset of the dim.names for the '",
                        group, "' group"))
    }
    
    subset.to.group.indices = get.array.access.indices(arr.dim.names = group.ontology,
                                                       dimension.values = subset.dim.names,
                                                       index.from = 0)

    # the subset of subset.dim.names that is needed to map outcome.dim.names
    relevant.outcome.dim.names = intersect.shared.dim.names(outcome.dim.names,
                                                            subset.dim.names)
    intersected.dim.names = relevant.outcome.dim.names
    
    if (!is.null(outcome$subset.dimension.values))
        intersected.dim.names = intersect.joined.dim.names(intersected.dim.names, 
                                                           outcome$subset.dimension.values)
    intersected.dim.names = intersect.joined.dim.names(intersected.dim.names, 
                                                       subset.dim.names)
    
    if (any(sapply(intersected.dim.names, length)==0))
        return (NULL) # there is no overlap, so nothing to track!
        
    intersected.indices.into.subset = get.array.access.indices(arr.dim.names = subset.dim.names,
                                                                dimension.values = intersected.dim.names,
                                                                index.from = 1)
    
    intersected.indices.into.group = subset.to.group.indices[intersected.indices.into.subset]
    
    outcome.indices.into.intersected = get.array.access.indices(arr.dim.names = intersected.dim.names,
                                                                dimension.values = relevant.outcome.dim.names)
    
    
    intersected.indices.into.outcome = get.expand.array.indices(to.expand.dim.names = outcome.dim.names,
                                                                target.dim.names = intersected.dim.names,
                                                                index.from = 0)

    if (is.null(outcome$multiply.by))
    {
        multiply.by.quantity.index = -1
        intersected.indices.into.multiply.by = integer()
    }
    else
    {
        multiply.by.quantity.index = settings$quantity.indices[outcome$multiply.by]
        
        multiply.by.dim.names = quantity.dim.names[[outcome$multiply.by]]
        intersected.indices.into.multiply.by = get.expand.array.indices(to.expand.dim.names = multiply.by.dim.names,
                                                                        target.dim.names = intersected.dim.names,
                                                                        index.from = 0)
    }
    
    list(
        offset_into_tracked = settings$indices_into_state_and_dx[outcome.name],
        n = prod(vapply(intersected.dim.names, length, FUN.VALUE = numeric(1))),
        group = DIFFEQ.GROUP.INDICES[group],
        multiply_by_quantity_index = multiply.by.quantity.index,
        state_indices = as.integer(intersected.indices.into.group[outcome.indices.into.intersected]),
        track_indices = as.integer(intersected.indices.into.outcome),
        multiply_by_indices = as.integer(intersected.indices.into.multiply.by)
    )
}

# this will be implemented in cpp in calculate_quantity_background_value.cpp
can.get.outcome.value.from.ode.output <- function(outcome.name,
                                                  settings)
{
    any(names(settings$outcomes)==outcome.name)
}

# this will be implemented in cpp in calculate_quantity_background_value.cpp
# ode.results has
# $times - a numeric vector of times
# $values - a numeric matrix with ncol = length(times)
get.outcome.value.from.ode.output <- function(outcome.name,
                                              settings,
                                              ode.results,
                                              outcome.years)
{
    outcome = settings$outcomes[[outcome.name]]
    first.ode.year = ode.results$times[1]
    
    rows = settings$indices_into_state_and_dx[outcome.name] + 1:settings$state_and_dx_sizes[outcome.name]

    cols = outcome.years - first.ode.year + 1
    
    if (outcome$is.cumulative)
    {
        rv = lapply(cols, function(j){
            pmax(ode.results$values[rows,j+1] - ode.results$values[rows,j],0)
        })
    }
    else
    {
        rv = lapply(cols, function(j){
            pmax(ode.results$values[rows,j],0)
        })
    }
    
    names(rv) = as.character(outcome.years)
    rv
}
