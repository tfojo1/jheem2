

##----------------------------------------------------##
##--          JHEEM *COMPILED* SPECIFICATION        --##
##-- (The private one to interface with components) --##
##----------------------------------------------------##

JHEEM.COMPILED.SPECIFICATION = R6::R6Class(
    classname = 'jheem.compiled.specification',
    inherit = JHEEM.SPECIFICATION,
    portable = F,
    class = F,
    
    public = list(
        
        initialize = function(version,
                              iteration,
                              description,
                              
                              compartment.value.character.aliases,
                              compartment.value.function.aliases,
                              ontologies,
                              
                              compartments,
                              
                              fixed.strata.info,
                              quantities,
                              outcomes,
                              core.components,
                              mechanisms,
                              
                              foregrounds,
                              default.parameter.values,
                              
                              age.info,

                              parent.specification,
                              do.not.inherit.model.quantity.names,
                              do.not.inherit.model.outcome.names,
                              do.not.inherit.transitions.for.dimension,
                              do.not.inherit.components.with.tags,
                              
                              do.compile)
        {
            #-- Copy over --#            
            private$i.version = version
            private$i.iteration = iteration
            private$i.description = description
            
            private$i.compartment.value.character.aliases = compartment.value.character.aliases
            private$i.compartment.value.function.aliases = compartment.value.function.aliases
            private$i.ontologies = ontologies
            
            private$i.compartments = compartments
            
            private$i.fixed.strata.info = fixed.strata.info
            private$i.quantities = lapply(quantities, function(quant){quant$compile()})
            private$i.outcomes = outcomes
            private$i.core.components = core.components
            private$i.mechanisms = mechanisms
            
            private$i.foregrounds = foregrounds
            private$i.default.parameter.values = default.parameter.values
            
            private$i.age.info = age.info

            
            private$i.parent.specification = parent.specification
            private$i.do.not.inherit.model.quantity.names = do.not.inherit.model.quantity.names
            private$i.do.not.inherit.model.outcome.names = do.not.inherit.model.outcome.names
            private$i.do.not.inherit.transitions.for.dimension = do.not.inherit.transitions.for.dimension
            private$i.do.not.inherit.components.with.tags = do.not.inherit.components.with.tags
            
            
            #-- Copy ancestor specifications and add this one --#
            private$i.ancestor.specifications = list(self)
            names(private$i.ancestor.specifications) = private$i.version
            if (!is.null(private$i.parent.specification))
                private$i.ancestor.specifications = c(private$i.ancestor.specifications,
                                                      private$i.parent.specification$ancestor.specifications)
            
            private$i.locked = F
            
            #-- If requested, do the compilation --#
            if (do.compile)
                private$do.compile()
        },
        
        ##--------------------------------------##
        ##-- PUBLIC to the JHEEM.ENGINE CLASS --##
        ##--------------------------------------##
        
        get.quantity = function(name)
        {
            if (private$i.locked)
                private$i.quantities[[name]]
            else
            {
                # as an intermediate step in compiling, the name in the quantity may be
                # different from the name it is stored under in the specification
                # we need to search for both
                
                rv = private$i.quantities[[name]]
                if (is.null(rv))
                {
                    for (quant in private$i.quantities)
                    {
                        if (quant$name==name)
                            return (quant)
                    }
                }
                rv
            }
        },
        
        get.outcome = function(name)
        {
            if (private$i.locked)
                private$i.outcomes[[name]]
            else
            {
                # as an intermediate step in compiling, the name in the outcome may be
                # different from the name it is stored under in the specification
                # we need to search for both
                
                rv = private$i.outcomes[[name]]
                if (is.null(rv))
                {
                    for (out in private$i.outcomes)
                    {
                        if (out$name==name)
                            return (out)
                    }
                }
                rv
            }
        },
        
        get.dependent.quantity.names = function(quantity.name)
        {
            private$i.dependent.quantity.names[[quantity.name]]
        },
        
        get.co.dependee.element.names = function(element.name)
        {
            private$i.co.dependee.element.names[[element.name]]
        },
        
        get.dependee.element.names = function(quantity.name)
        {
            private$i.dependee.element.names[[quantity.name]]
        },
        
        get.dependee.quantity.names = function(quantity.name)
        {
            private$i.dependee.quantity.names[[quantity.name]]
        },
        
        get.dependent.top.level.quantity.names = function(quantity.name)
        {
            private$i.dependent.top.level.quantity.names[[quantity.name]]
        },

        ##---------------------------------------------------------------##
        ##--                     CONSTRUCTOR HELPERS                   --##
        ##-- (That have to be public to recurse up the ancestor chain) --##
        ##---------------------------------------------------------------##
        
        rename.quantities = function(names.in.use)
        {
            for (quant in private$i.quantities)
            {
                if (any(quant$name==names.in.use))
                    quant$rename(paste0(quant$name, '__', quant$version))
            }
            
            if (!is.null(private$i.parent.specification))
            {
                names.in.use = union(names.in.use,
                                     sapply(private$i.quantities, function(quant){quant$name}))
                
                private$i.parent.specification$rename.quantities(names.in.use)
            }
            
            self
        },
        
        # returns a vector of the names of saved outcomes
        process.and.rename.outcomes = function(wrt.specification, names.in.use, error.prefix)
        {
            rv = character()
            private$i.outcomes = lapply(private$i.outcomes, function(outcome){outcome$compile(wrt.specification, error.prefix)})
            
            for (outcome in private$i.outcomes)
            {
                if (any(outcome$name==names.in.use))
                    outcome$rename(paste0(outcome$name, "__", outcome$version))
                else if (outcome$save)
                    rv = c(rv, outcome$name)
            }

            if (!is.null(private$i.parent.specification))
            {
                names.in.use = union(names.in.use,
                                     sapply(private$i.outcomes, function(outcome){outcome$name}))
                
                rv = c(rv, private$i.parent.specification$process.and.rename.outcomes(self, names.in.use=names.in.use, error.prefix=error.prefix))
            }
            
            rv
        },
        
        process.core.components = function(error.prefix)
        {
            # compile all the core components
            compiled.components = list()
            for (uncompiled.comp in private$i.core.components)
            {
                compiled.components = c(compiled.components, 
                                        uncompiled.comp$schema$compile.component(uncompiled.comp,
                                                                                 specification = self,
                                                                                 unpack = T,
                                                                                 error.prefix = error.prefix))
            }
            
            # check for clashes among the compiled core components in this version
            #  (we may have introduced previously-unrecognized clashes by swapping in aliases)
            for (i in 1:length(compiled.components))
            {
                comp = compiled.components[[i]]
                for (other.comp in compiled.components[-c(1:i)])
                {
                    if (comp$schema$components.clash(comp, other.comp))
                    {
                        stop(paste0(error.prefix, "After substituting compartment aliases in specification '",
                                    private$i.version, "', there is a clash between ",
                                    comp$name, " and ", other.comp$name))
                    }
                }
            }
            
            if (!is.null(private$i.parent.specification))
            {
                # process the parent specification's components
                private$i.parent.specification$process.core.components(error.prefix=error.prefix)
            
                # pull down components from the parent that don't clash
                # (and don't contain tags we don't want to inherit)
                for (comp in private$i.parent.specification$core.components)
                {
                    if (all(comp$tag != private$i.do.not.inherit.components.with.tags) &&
                        (comp$type != 'transition' || all(comp$dimension != private$i.do.not.inherit.transitions.for.dimension)))
                    {
                        clashes = sapply(compiled.components, function(other.comp){
                            comp$schema$components.clash(comp, other.comp)
                        })
                        
                        if (!all(clashes))
                            compiled.components = c(compiled.components, list(comp))
                    }
                }
            }
            
            private$i.core.components = compiled.components
            
            # Make sure all required core components are present
            errors = character()
            for (sch in CORE.COMPONENT.SCHEMATA)
            {
                any.match = any(sapply(private$i.core.components, function(comp){comp$type})==sch$type)
                if (sch$is.required(self) && !any.match)
                    errors = c(errors, paste0("- No ", sch$type, " has been registered for the specification. Use ",
                                sch$register.function.name, "() to do so"))
            }
            if (length(errors)>0)
                stop(paste0(error.prefix, paste0(errors, collapse='\n')))
            
            #-- Done --#
            self
        }
    ),
    
    active = list(
        
        ancestor.specifications = function(value)
        {
            if (missing(value))
                private$i.ancestor.specifications
            else
                stop("Cannot modify a specification's 'ancestor.specifications' - they are read-only")
        },
        
        do.not.inherit.model.quantity.names = function(value)
        {
            if (missing(value))
                private$i.do.not.inherit.model.quantity.names
            else
                stop("Cannot modify a specification's 'do.not.inherit.model.quantity.names' - they are read-only")
        },
        
        do.not.inherit.model.outcome.names = function(value)
        {
            if (missing(value))
                private$i.do.not.inherit.model.outcome.names
            else
                stop("Cannot modify a specification's 'do.not.inherit.model.outcome.names' - they are read-only")
        },

        top.level.references = function(value)
        {
            if (missing(value))
                private$i.top.level.references
            else
                stop("Cannot modify a specification's 'top.level.references' - they are read-only")
        },
        
        top.level.quantity.names = function(value)
        {
            if (missing(value))
                i.top.level.quantity.names
            else
                stop("Cannot modify a specification's 'top.level.quantity.names' - they are read-only")
        },
        
        element.names = function(value)
        {
            if (missing(value))
                private$i.element.names
            else
                stop("Cannot modify a specification's 'element.names' - they are read-only")
        },
        
        quantity.names = function(value)
        {
            if (missing(value))
                names(private$i.quantities)
            else
                stop("Cannot modify a specification's 'quantity.names' - they are read-only")
        },
        
        quantities = function(value)
        {
            if (missing(value))
                private$i.quantities
            else
                stop("Cannot modify a specification's 'quantities' - they are read-only")  
        },
        
        core.components = function(value)
        {
            if (missing(value))
                private$i.core.components
            else
                stop("Cannot modify a specification's 'core.components' - they are read-only")  
        },
        
        mechanisms = function(value)
        {
            if (missing(value))
                private$i.mechanisms
            else
                stop("Cannot modify a specification's 'mechanisms' - they are read-only")  
        },
        
        resolved.aliases = function(value)
        {
            if (missing(value))
                private$i.compartment.value.character.aliases
            else
                stop("Cannot modify a specification's 'resolved.aliases' - they are read-only")
        },
        
        unresolved.alias.names = function(value)
        {
            if (missing(value))
                names(private$i.compartment.value.function.aliases)
            else
                stop("Cannot modify a specification's 'resolved.aliases' - they are read-only")
        },
        
        foregrounds = function(value)
        {
            if (missing(value))
                names(private$i.foregrounds)
            else
                stop("Cannot modify a specification's 'foregrounds' - they are read-only")
        }
    ),
    
    private = list(
        
        i.ancestor.specifications = NULL,
        
        i.verbose = NULL,
        
        i.top.level.references = NULL,
        i.top.level.quantity.names = NULL,
        i.element.names = NULL,
        
        # indexed dependee, dependent
        i.direct.dependencies = NULL,
        i.dependencies = NULL,
        
        i.dependent.quantity.names = NULL,
        i.dependent.top.level.quantity.names = NULL,
        i.co.dependee.element.names = NULL,
        i.dependee.element.names = NULL,
        i.dependee.quantity.names = NULL,
        
        #----------------------------------#
        #-- COMPILE FUNCTION and HELPERS --#
        #----------------------------------#
        
        do.compile = function(verbose=T) #the verbose flag is for debugging only
        {
            private$i.verbose = verbose
            error.prefix = paste0("Error compiling model specification for '", private$i.version, "': ")
            
            #-- Print a start message --#
            do.cat(paste0("Starting compilation of specification '", private$i.version, "'\n"))

            #-- Process aliases for consistency --#
            do.cat("Processing character aliases...")
            private$process.compartment.aliases()
            do.cat("done\n")

            #-- Rename outcomes and quantities so they are unique across versions --#
            do.cat("Compiling outcomes, renaming outcomes and quantities...")
            top.level.outcome.names = self$process.and.rename.outcomes(self, 
                                                                       names.in.use = character(),
                                                                       error.prefix = error.prefix) # rename the quantities first so we can get a unique name for each
            self$rename.quantities(character()) # rename the quantities first so we can get a unique name for each
            do.cat("done\n")
            
            
            #-- Parse outcomes --#
            #-- Make sure there are no circular references --#
            do.cat("Parsing outcome tree...")
            top.level.outcomes = sapply(top.level.outcome.names, private$resolve.outcome.name, this.refers.to.version=self$version)
            outcomes.to.use = private$parse.outcome.tree(top.level.outcomes)
            do.cat("done\n")

            
            #-- Process Core Components and Mechanisms --#
            #-- Make sure required are present --#
            #-- Check for clashes --#
            do.cat("Processing core components...")          
            self$process.core.components(error.prefix)
            private$process.mechanisms(error.prefix)
            do.cat("done\n")
            
            #-- Process Foregrounds --#
            do.cat("Processing foregrounds...")
            private$process.foregrounds(error.prefix)
            do.cat("done\n")
            
            #-- Pull all top-level references FROM CORE COMPONENTS --#
            do.cat("Deriving top-level references...")
            references.from.core.components = list()
            for (i in 1:length(private$i.core.components))
            {
                comp = private$i.core.components[[i]]
                references = comp$schema$create.top.level.references(comp=comp,
                                                                     specification=self,
                                                                     error.prefix = error.prefix)
                
                references.from.core.components = c(references.from.core.components, references)
                private$i.core.components[[i]] = comp$schema$set.component.top.level.references(comp, 
                                                                                                references=references,
                                                                                                specification = self,
                                                                                                error.prefix = error.prefix)
            }
            
            #-- (First of twice) Pull top-level references FROM OUTCOMES --#
            references.from.outcomes = list()
            for (outcome in outcomes.to.use)
            {
                references.from.outcomes = c(references.from.outcomes,
                                             outcome$create.top.level.references(specification = self,
                                                                                 all.outcomes = outcomes.to.use,
                                                                                 error.prefix = error.prefix))
            }
            
            private$i.top.level.references = c(references.from.core.components, references.from.outcomes)
            do.cat("done\n")
            
            
            #-- Make sure there are no circular references --#
            #-- Make sure that any reference to 'super' and 'this' (in parent mapping) model.quantities are valid --#
            #-- Rename quantities as needed to refer to ancestor quantities --#
            #-- Identify all the quantities and elements we will need --#
            #     (Either in this mapping or in an ancestor mapping)    #
            do.cat("Parsing model.quantity tree...")
            quantities.to.use = private$parse.quantity.tree(quantities.or.references = c(references.from.core.components,
                                                                                         references.from.outcomes))
            do.cat("done\n")
            
            #-- Check for missing quantities that have not been registered --#
            if (any(sapply(quantities.to.use, is.null)))
            {
                missing.quantity.names = names(quantities.to.use)[sapply(quantities.to.use, is.null)]
                stop(paste0("The following ",
                            ifelse(length(missing.quantity.names)==1, "name is", "names are"),
                            " required in the definition of model quantities or transitions, ",
                            "but ", ifelse(length(missing.quantity.names)==1, 'has', 'have'),
                            " not been registered (as ",
                            ifelse(length(missing.quantity.names)==1, "a model quantity or element", "model quantities or elements"),
                            "):\n",
                            paste0("- '", missing.quantity.names, "'", collapse='\n')))
                
            }
         
            #-- Check for quantities that have been registered but will not be used --#
            surplus.quantity.names = setdiff(names(private$i.quantities),
                                             names(quantities.to.use))
            
            if (length(surplus.quantity.names)>0)
                cat(paste0("*** WARNING: The following have been registered as model quantities or elements, but are not actually used to define any transitions or required model quantities:\n",
                           paste0("- '", surplus.quantity.names, "'", collapse='\n'),
                           "\nThey will be ignored.\n"))


                        
            #-- Pull Top-Level Quantities --#
            do.cat("Pulling top-level quantities...")
            private$i.top.level.quantity.names = unique(sapply(private$i.top.level.references, function(ref){
                ref$value.quantity.name
            }))
            do.cat("done\n")


            #-- Remake into a new, flattened (parentless) mapping with just the quantities and outcomes we need --#
            private$i.quantities = quantities.to.use
            private$i.outcomes = outcomes.to.use
            private$i.parent.specification = NULL
            private$i.ancestor.specifications = private$i.ancestor.specifications[1]
            
            
            ##** FROM HERE DOWN THE TRANSITION MAPPING IS FLATTENED **##
            
            #-- Check that outcomes depends.on outcomes/quantities are appropriately cumulative/static or not --#
            do.cat("Checking outcome depends.on for consistency...")
            for (outcome in private$i.outcomes)
                private$check.outcome.depends.on(outcome, error.prefix=error.prefix)
            do.cat("done\n")
            
            #-- Resolve compartment names for quantities --#
            do.cat("Resolving aliases for needed quantities...")
            for (quantity in private$i.quantities)
                quantity$resolve.compartment.values(aliases = private$i.compartment.value.character.aliases, 
                                                    ontology = private$i.ontologies$all,
                                                    unresolved.alias.names = names(i.compartment.value.function.aliases),
                                                    ontology.name.for.error = "the ontology",
                                                    error.prefix = error.prefix,
                                                    wrt.specification = self)
            do.cat("done\n")
            
            #-- Reduce the model quantities --#
            #-- Strip out model quantities that have been eliminated by reducing --#
            can.reduce.quantities = sapply(private$i.quantities, function(quant){
                private$can.reduce.quantity(quant)
            })
            
            if (sum(can.reduce.quantities)==0)
                do.cat("No model quantities to reduce\n")
            else
            {
                reducible.quantity.names = names(can.reduce.quantities)[can.reduce.quantities]
                do.cat("Reducing ", sum(can.reduce.quantities),
                       ifelse(sum(can.reduce.quantities)==1, " model quantity", " model quantities"),
                       ":\n", paste0("- '", reducible.quantity.names, "'", collapse='\n'), "\n")
                
                for (quant in private$i.quantities[!can.reduce.quantities])
                    private$reduce.quantity(quant, reducible.quantity.names = reducible.quantity.names)
                
                private$i.quantities = private$i.quantities[!can.reduce.quantities]
                
                do.cat("Done reducing\n")
            }
            
            #-- Map out dim.names --#
            #   We have to do the following sequence
            #     1) Calculate quantity dim.names (preliminary)
            #     2) Derive outcome dim.names
            #     3) Re-calculate quantity dim.names
            #   We have to do it this way because some outcomes' dim.names may depend on their depends on quantities (preliminary) dim.names
            #   But setting dim.names for outcomes can restrict the max dim.names of quantities
            
            #-- 1) Map out max dim.names for each quantity --#
            #-- Check that applies.to dimensions are a subset of the calculated dim.names --#
            
            do.cat("Calculating dim.names for quantities...")
            for (quantity in private$i.quantities)
                private$calculate.quantity.dim.names(quantity, error.prefix=error.prefix)
            
            null.dim.names.mask = sapply(private$i.quantities, function(quant){
                is.null(quant$max.dim.names)
            })
            for (quantity in private$i.quantities)
                private$calculate.quantity.dim.names.bottom.up(quantity, error.prefix=error.prefix)
            
            do.cat("done\n")
            
            #-- 2) Derive the outcome dim.names --#
            do.cat("Calculating dim.names for outcomes...")
            for (outcome in private$i.outcomes)
                outcome$derive.dim.names(specification = self,
                                         all.outcomes = private$i.outcomes,
                                         error.prefix = error.prefix, 
                                         set = T)
            
            # 2B: Update the top-level references from outcomes
            references.from.outcomes = list()
            for (outcome in outcomes.to.use)
            {
                references.from.outcomes = c(references.from.outcomes,
                                             outcome$create.top.level.references(specification = self,
                                                                                 all.outcomes = outcomes.to.use,
                                                                                 error.prefix = error.prefix))
            }
            private$i.top.level.references = c(references.from.core.components, references.from.outcomes)
            
            do.cat("done\n")
            
            
            #-- 3) Recalculate max dim.names for each quantity --#
            #-- Check that applies.to dimensions are a subset of the calculated dim.names --#
#            do.cat("Calculating final dim.names for quantities...")
#            for (quantity in private$i.quantities)
#                private$calculate.quantity.dim.names(quantity, error.prefix=error.prefix)
#            do.cat("done\n")
            
            
            #-- Check that numeric values of model quantity components have appropriate dimensions --#
            #-- Check that models for model.elements have appropriate dimensions --#
            #-- Check that default values of model elements have appropriate dimensions --#
            do.cat("Verifying dimnames for quantities...")
            for (quantity in private$i.quantities)
                quantity$verify.dim.names(error.prefix = error.prefix,
                                          wrt.specification = self)
            do.cat("done\n")
            
            #-- Finalize Dependencies --#
            do.cat("Finalizing dependencies...")
            private$calculate.dependencies()
            do.cat("done\n")
            
            #-- Print a 'done' message --#
            do.cat(paste0("DONE compiling specification '", private$i.version, "'\n"))
            
            
            #-- Lock and return self --#
            private$i.locked = T
            invisible(self)
        },

        ##---------------------##
        ##-- COMPILE HELPERS --##
        ##---------------------##
        
        process.foregrounds = function(error.prefix)
        {
            foregrounds = list()
            for (i in (1:length(private$i.ancestor.specifications))[-1])
            {
                one.foregrounds = private$i.ancestor.specifications[[i]]$foregrounds
                to.add = setdiff(names(one.foregrounds), names(private$i.foregrounds))
                
                private$i.foregrounds[names(to.add)] = to.add
            }
            
            for (frgd.name in names(private$i.foregrounds))
            {
                frgd = private$i.foregrounds[[frgd.name]]
                missing.parameters = setdiff(frgd$depends.on, names(private$i.default.parameter.values))
                
                if (length(missing.parameters)>0)
                    stop(paste0(error.prefix, "Foreground '", frgd.name, "' depends on ",
                                ifelse(length(paramaters)==1, "parameter ", "parameters "),
                                collapse.with.and("'", missing.parameters, "'"),
                                ifelse(length(paramaters)==1, " but it has", " but they have"),
                                " not had default values set. Use register.default.parameter.values() to do so"
                                ))
            }
            
            
            self
        },
        
        # call AFTER process.core.components()
        process.mechanisms = function(error.prefix)
        {
            # Pull each mechanism for each core component
            for (i in 1:length(private$i.core.components))
            {
                comp = private$i.core.components[[i]]
                
                for (mechanism.type in comp$schema$mechanism.types)
                {
                    mechanism = private$find.mechanism.for.component(comp,
                                                                     mechanism.type = mechanism.type,
                                                                     error.prefix = error.prefix)
                    if (is.null(mechanism))
                        stop(paste0(error.prefix,
                                    "No '", mechanism.type, "' mechanism has been registered for ",
                                    comp$name))
                    
                    # Set the quantity name to the component (for the mechanism)
                    quantity = private$resolve.quantity.name(mechanism$quantity.name, this.refers.to.version=mechanism$version)
                    
                    private$i.core.components[[i]] = 
                        private$i.core.components[[i]]$schema$set.mechanism.for.component(comp = private$i.core.components[[i]],
                                                                                          mechanism = mechanism,
                                                                                          quantity = quantity)
                }
            }
            
            self
        },

         find.mechanism.for.component = function(comp, mechanism.type, error.prefix, ancestor.index=1)
         {
             # The recursion stopping condition
             if (ancestor.index > length(private$i.ancestor.specifications))
                 return (NULL)
             
             for (mechanism in private$i.ancestor.specifications[[ancestor.index]]$mechanisms)
             {
                 if (mechanism$type==mechanism.type &&
                     comp$schema$component.uses.mechanism(comp=comp, mechanism=mechanism))
                     return (mechanism)
             }
             
             # We didn't find anything, so recurse up
             private$find.mechanism.for.component(comp, 
                                                  mechanism.type = mechanism.type,
                                                  error.prefix = error.prefix,
                                                  ancestor.index = ancestor.index + 1)
         },
 
        # Accomplishes several goals
        #   1) Checks for circular references
        #   2) Generates a list of every model.quantity we need (from which ancestor)
        #   3) Renames the dependencies in each quantity or reference so that they apply to quantity from the specified version
        #   4) Identify any missing dependencies
        #   
        # The return value is a list of quantities (names of the list are names of the quantities)  
        # If we could not find a name for a referenced quantity, then the element of the returned list
        #   corresponding to that name will be NULL
        parse.quantity.tree = function(quantities.or.references,
                                       ancestor.quantities = list())
        {
            if (is.list(quantities.or.references)) #Recurse on each one and merge
            {
                rv = list()
                for (one.ref.or.quant in quantities.or.references)
                {
                    sub.rv = private$parse.quantity.tree(quantities.or.references = one.ref.or.quant,
                                                         ancestor.quantities = ancestor.quantities)
                    rv[names(sub.rv)] = sub.rv
                }
                
                rv
            }
            else if (!is.null(quantities.or.references$value.quantity.name)) # this is a top.level.reference
            {
                ref = quantities.or.references # aliased for readability
                quantity = private$resolve.quantity.name(name = ref$value.quantity.name,
                                                         this.refers.to.version = ref$version)
                
                if (is.null(quantity))
                {
                    rv = list(NULL)
                    names(rv) = ref$value.quantity.name
                    rv
                }
                else
                {
                    ref$set.value.quantity.name(quantity$name)
                    
                    private$parse.quantity.tree(quantities.or.references = quantity,
                                                ancestor.quantities = ancestor.quantities)
                }
            }
            else # A single quantity - do the work
            {
                # Setup
                quantity = quantities.or.references # alias for readability
                new.ancestor.quantities = c(ancestor.quantities, list(quantity))
                
                # Make sure we don't have a loop
                matches.ancestor = sapply(ancestor.quantities, function(anc.quant){
                    anc.quant$name == quantity$name && anc.quant$version == quantity$version
                })
                
                if (any(matches.ancestor))
                {
                    ancestor.names = sapply(new.ancestor.quantities, function(quant){
                        quant$get.original.name(wrt.version = private$i.version)
                    })
                    
                    # Get just the loop
                    ancestor.names = ancestor.names[first.index.of(ancestor.names, ancestor.names[length(ancestor.names)]):length(ancestor.names)]
                    
                    stop(paste0("There is a circular loop of dependencies through model.quantity ",
                                quantity$get.original.name(wrt.version=private$i.version),
                                ": ",
                                paste0(ancestor.names, collapse=" -> ")))
                    
                }   
                
                if (length(quantity$depends.on)>0)
                {
                    # Recurse down for each of the sub-quantities
                    depends.on.quantities = lapply(quantity$depends.on, function(depends.on.name){
                        private$resolve.quantity.name(name = depends.on.name, this.refers.to.version = quantity$version)
                    })
                    names(depends.on.quantities) = quantity$depends.on
                    
                    null.depends.on.quantities = depends.on.quantities[sapply(depends.on.quantities, is.null)]
                    depends.on.quantities = depends.on.quantities[!sapply(depends.on.quantities, is.null)]
                    
                    if (length(depends.on.quantities)>0)
                    {
                        rv = private$parse.quantity.tree(depends.on.quantities,
                                                         ancestor.quantities = new.ancestor.quantities)
                    
                        # Sub the new names of the depends.on quantities into this quantity 
                        new.depends.on.names = sapply(depends.on.quantities, function(quant){quant$name})
                        names(new.depends.on.names) = names(depends.on.quantities)
                        quantity$rename.depends.on(mapping = new.depends.on.names)   
                    }
                    else
                        rv = list()
                    
                    rv[names(null.depends.on.quantities)] = null.depends.on.quantities
                }
                else
                    rv = list()
                
                rv[[quantity$name]] = quantity
                
                rv
            }
        },
        
        parse.outcome.tree = function(outcomes,
                                      ancestor.outcomes = list(),
                                      error.prefix='')
        {
            rv = list()
            for (outcome in outcomes)
            {
                new.ancestor.outcomes = c(ancestor.outcomes, list(outcome))
                
                #-- Make sure we don't have a loop --#
                matches.ancestor = sapply(ancestor.outcomes, function(anc.outcome){
                    anc.outcome$name == outcome$name && anc.outcome$version == outcome$version
                })
                
                if (any(matches.ancestor))
                {
                    ancestor.names = sapply(new.ancestor.outcomes, function(out){
                        out$get.original.name(wrt.version = private$i.version)
                    })
                    
                    # Get just the loop
                    ancestor.names = ancestor.names[first.index.of(ancestor.names, ancestor.names[length(ancestor.names)]):length(ancestor.names)]
                    
                    stop(paste0(error.prefix,
                                "There is a circular loop of dependencies through model outcome ",
                                outcome$get.original.name(wrt.version=private$i.version),
                                ": ",
                                paste0(ancestor.names, collapse=" -> ")))
                    
                }
                
                #-- Resolve Dependencies --#
                # Resolve depends on outcomes
                depends.on.outcomes = lapply(outcome$depends.on.outcomes, private$resolve.outcome.name, this.refers.to.version=outcome$version)
                names(depends.on.outcomes) = outcome$depends.on.outcomes
                if (length(depends.on.outcomes)>0)
                {
                    missing.depends.on = outcome$depends.on.outcomes[sapply(depends.on.outcomes, is.null)]
                    if (length(missing.depends.on)>0)
                        stop(paste0(error.prefix,
                                    "Model outcome ", outcome$get.original.name(wrt.version=private$i.version), " depends on ",
                                    ifelse(length(missing.depends.on)==1, "another model outcome ", "other model outcomes "),
                                    collapse.with.and("'", missing.depends.on, "'"),
                                    ifelse(length(missing.depends.on)==1, ' which has ', ' which have '),
                                    "not been registered as ",
                                    ifelse(length(missing.depends.on)==1, "a tracked outcome", "tracked outcomes")))
                }
                
                # Resolve depends on quantities
                depends.on.quantities = lapply(outcome$depends.on.quantities, private$resolve.quantity.name, this.refers.to.version=outcome$version)
                names(depends.on.quantities) = outcome$depends.on.quantities
                if (length(depends.on.quantities)>0)
                {
                    missing.depends.on = outcome$depends.on.quantities[sapply(depends.on.quantities, is.null)]
                    if (length(missing.depends.on)>0)
                        stop(paste0(error.prefix,
                                    "Model outcome ", outcome$get.original.name(wrt.version=private$i.version), " depends on ",
                                    ifelse(length(missing.depends.on)==1, "model quantity ", "model quantities "),
                                    collapse.with.and("'", missing.depends.on, "'"),
                                    ifelse(length(missing.depends.on)==1, ' which has ', ' which have '),
                                    "not been registered using register.model.outcome()"))
                }
                
                # Resolve depends on ambiguous whether outcomes or quantities
                depends.on.ambiguous.as.outcomes = lapply(outcome$depends.on.quantities.or.outcomes, private$resolve.outcome.name, this.refers.to.version=outcome$version)
                depends.on.ambiguous.as.quantities = lapply(outcome$depends.on.quantities.or.outcomes, private$resolve.quantity.name, this.refers.to.version=outcome$version)
                names(depends.on.ambiguous.as.outcomes) = names(depends.on.ambiguous.as.quantities) = outcome$depends.on.quantities.or.outcomes
            
                ambiguous.is.outcome = !as.logical(sapply(depends.on.ambiguous.as.outcomes, is.null)) & 
                    sapply(depends.on.ambiguous.as.outcomes, function(out){out$original.name}) != outcome$original
                
                ambiguous.is.quantity = !ambiguous.is.outcome & !as.logical(sapply(depends.on.ambiguous.as.quantities, is.null))
                
                missing.depends.on = outcome$depends.on.outcomes[!ambiguous.is.outcome & !ambiguous.is.quantity]
                if (length(missing.depends.on)>0)
                    stop(paste0(error.prefix,
                                "Model outcome ", outcome$get.original.name(wrt.version=private$i.version), " depends on ",
                                collapse.with.and("'", missing.depends.on, "'"),
                                ifelse(length(missing.depends.on)==1, ' which has ', ' which have '),
                                "not been registered as either model.quantities (using register.model.outcome() ) or tracked model outcomes"))
                
                depends.on.outcomes = c(depends.on.outcomes, depends.on.ambiguous.as.outcomes[ambiguous.is.outcome])
                depends.on.quantities = c(depends.on.outcomes, depends.on.ambiguous.as.quantities[ambiguous.is.quantity])
                
                depends.on.outcomes = depends.on.outcomes[unique(names(depends.on.outcomes))]
                depends.on.quantities = depends.on.quantities[unique(names(depends.on.quantities))]
 
# I believe we no longer need this - have split into two functions. Keeping just in case i'm wrong 8/10/23                               
#                double.counted.names = intersect(names(depends.on.outcomes), names(depends.on.quantities))
#                if (length(double.counted.names)>0)
#                    stop(paste0(error.prefix,
#                                "Model outcome ", outcome$get.original.name(wrt.version=private$i.version), " depends on both ",
#                                ifelse(length(double.counted.names)==1, 'a model quantity and a model outcome', 'model quantities and model outcomes'),
#                                " named ",
#                                collapse.with.and("'", double.counted.names, "'"),
#                                ". This introduces an ambiguitiy - a model outcome cannot depend on quantities and other outcomes with the same name."))
                
                # Rename the depends on
                new.depends.on.outcome.names = sapply(depends.on.outcomes, function(outcome){
                    outcome$name
                })
                outcome$rename.depends.on.outcomes(new.depends.on.outcome.names)
                
                new.depends.on.quantity.names = sapply(depends.on.quantities, function(quantity){
                    quantity$name
                })
                outcome$rename.depends.on.quantities(new.depends.on.quantity.names)
                
                # Save to our rv
                rv[[outcome$name]] = outcome
                
                # Recurse
                sub.rv = private$parse.outcome.tree(outcomes = depends.on.outcomes,
                                                    ancestor.outcomes = new.ancestor.outcomes,
                                                    error.prefix = error.prefix)
                for (sub.name in names(sub.rv))
                    rv[[sub.name]] = sub.rv[[sub.name]]
                
            }
            
            rv
        },

        # Should only be called on flattened specification
        check.outcome.depends.on = function(outcome, error.prefix)
        {
            error.prefix = paste0(error.prefix, "Cannot parse model outcome ",
                                  outcome$get.original.name(wrt.version=self$version), " - ")
            
            # Figure out which depends.on outcomes and quantities are cumulative
            depends.on.outcomes.mask = !sapply(lapply(outcome$depends.on, self$get.outcome), is.null) & outcome$depends.on != outcome$original.name
            depends.on.outcomes = outcome$depends.on[depends.on.outcomes.mask]
            depends.on.quantities = outcome$depends.on[!depends.on.outcomes.mask]
            
            depends.on.outcomes.cumulative = intersect(depends.on.outcomes, outcome$depends.on.cumulative)
            depends.on.outcomes.non.cumulative = setdiff(depends.on.outcomes, depends.on.outcomes.cumulative)
            depends.on.quantities.static = intersect(depends.on.quantities, outcome$depends.on.cumulative)
            
            # Check that depends.on.outcomes
            
            # Check that these are cumulative or not as they should be
            invalid.outcomes.cumulative = depends.on.outcomes.cumulative[as.logical(sapply(depends.on.outcomes.cumulative, function(dep.on){
                !self$get.outcome(dep.on)$is.cumulative
            }))]
            
            if (length(invalid.outcomes.cumulative)>0)
                stop(paste0(error.prefix, "The outcome requires ",
                            collapse.with.and("'", invalid.outcomes.cumulative, "'"),
                            " to be *cumulative*, ",
                            ifelse(length(invalid.outcomes.cumulative)==1, "but it is", "but they are"),
                            " NON-cumulative"))
            
            
            invalid.outcomes.non.cumulative = depends.on.outcomes.non.cumulative[as.logical(sapply(depends.on.outcomes.non.cumulative, function(dep.on){
                self$get.outcome(dep.on)$is.cumulative
            }))]
            
            if (length(invalid.outcomes.non.cumulative)>0)
                stop(paste0(error.prefix, "The outcome requires ",
                            collapse.with.and("'", invalid.outcomes.non.cumulative, "'"),
                            " to be *NON-cumulative*, ",
                            ifelse(length(invalid.outcomes.non.cumulative)==1, "but it IS", "but they ARE"),
                            " cumulative"))
            
            
            invalid.quantities.non.static = depends.on.quantities.static[!as.logical(sapply(depends.on.quantities.static, private$quantity.may.be.static, error.prefix=error.prefix))]
            
            if (length(invalid.quantities.non.static)>0)
                stop(paste0(error.prefix, "The outcome requires ",
                            collapse.with.and("'", invalid.quantities.non.static, "'"),
                            " to be *static*, ",
                            ifelse(length(invalid.quantities.non.static)==1, "but it", "but they"),
                            " CANNOT be static as defined"))
            
            # Flag that the static quantities must be static
            sapply(depends.on.quantities.static, private$set.quantity.must.be.static, error.prefix=error.prefix)
        },
        
        # Can only be called AFTER flattening quantities
        quantity.may.be.static = function(quantity.name, error.prefix, ignore.missing=F)
        {
            quant = self$get.quantity(quantity.name)
            if (is.null(quantity))
            {
                if (ignore.missing)
                    return (T)
                else
                    stop(paste0(error.prefix, "Quantity '", quantity.name, "' has not been registered"))
            }
            else if (quantity$type=='element')
            {
                quantity$may.be.static()
            }
            else
            {
                depends.on.may.be.static = sapply(quantity$depends.on, private$quantity.may.be.static, 
                                                  ignore.missing=ignore.missing, error.prefix=error.prefix)
                
                all(depends.on.may.be.static)
            }
        },
        
        # Can only be called AFTER flattening quantities
        set.quantity.must.be.static = function(quantity.name, error.prefix, ignore.missing=T)
        {
            quantity = self$get.quantity(quantity.name)
            if (is.null(quantity))
            {
                if (ignore.missing)
                    return()
                else
                    stop(paste0("Quantity '", quantity.name, "' has not been registered"))
            }
            else
            {
                quantity$set.must.be.static()
                sapply(quantity$depends.on, private$set.quantity.must.be.static, 
                       error.prefix=error.prefix, ignore.missing=ignore.missing)
            }
        },

        # Sub in the character compartment aliases to ontologies and compartments
        # Figure out where function compartment aliases are going to plug in and make sure they are not duplicated where they should not be
        process.compartment.aliases = function(error.prefix)
        {
            # Sub-in aliases to ontologies and compartments
            private$i.ontologies = apply.aliases(private$i.ontologies, aliases=private$i.compartment.value.character.aliases)
            private$i.compartments = apply.aliases(private$i.compartments, aliases=private$i.compartment.value.character.aliases)
          
            # Figure out where function aliases will plug in to compartments
            function.aliases.plug.into.compartments = lapply(private$i.compartments, function(compartments){
                
                if (length(compartments)==0)
                    array(0, dim=c(0,length(private$i.compartment.value.function.aliases)),
                          dimnames=list(NULL, names(private$i.compartment.value.function.aliases)))
                else
                    sapply(names(private$i.compartment.value.function.aliases), function(alias){
                        sapply(compartments, function(values.for.dim){
                            any(values.for.dim==alias)
                        })
                    })
            })
            
            # Check for overlaps
            # A function alias cannot be in both infected+general or uninfected+general
            # (I actually think this check is redundant - we should have checked in constructor that there were no
            #  overlaps in compartment names across these, but we'll keep it here anyway just in case)
            aliases.in.general = names(private$i.compartment.value.function.aliases)[apply(function.aliases.plug.into.compartments$general, 2, any)]
            aliases.in.infected = names(private$i.compartment.value.function.aliases)[apply(function.aliases.plug.into.compartments$infected, 2, any)]
            aliases.in.uninfected = names(private$i.compartment.value.function.aliases)[apply(function.aliases.plug.into.compartments$uninfected, 2, any)]

            aliases.in.general.and.infected = intersect(aliases.in.general, aliases.in.infected)            
            if (length(aliases.in.general.and.infected)>0)
                stop(paste0(error.prefix,
                            "compartment.value.aliases which are functions cannot apply to both 'infected only' and 'infected and uninfected'. ",
                            ifelse(length(aliases.in.general.and.infected)==1, "Alias ", "Aliases "),
                            collapse.with.and("'", aliases.in.general.and.infected, "'"),
                            ifelse(length(aliases.in.general.and.infected)==1, " appears", " appear"),
                            " in both"))
            
            aliases.in.general.and.uninfected = intersect(aliases.in.general, aliases.in.uninfected)            
            if (length(aliases.in.general.and.uninfected)>0)
                stop(paste0(error.prefix,
                            "compartment.value.aliases which are functions cannot apply to both 'uninfected only' and 'infected and uninfected'. ",
                            ifelse(length(aliases.in.general.and.uninfected)==1, "Alias ", "Aliases "),
                            collapse.with.and("'", aliases.in.general.and.uninfected, "'"),
                            ifelse(length(aliases.in.general.and.uninfected)==1, " appears", " appear"),
                            " in both"))
        },
        

        get.quantities.that.depend.on = function(depends.on.name)
        {
            private$i.quantities[sapply(private$i.quantities, function(quantity){
                any(quantity$depends.on == depends.on.name)
            })]
        },
        
        get.quantity.components.that.depend.on = function(depends.on.name)
        {
            unlist(sapply(private$i.quantities, function(quantity){
                quantity$components[sapply(quantity$components, function(comp){
                    any(comp$depends.on == depends.on.name)
                })]
            }))
        },
        
        get.references.that.refer.to = function(refer.to.name)
        {
            private$i.top.level.references[sapply(private$i.top.level.references, function(ref){
                ref$value.quantity.name == refer.to.name
            })]
        },
        
        reduce.quantity = function(quantity, reducible.quantity.names)
        {
            if (length(intersect(quantity$depends.on, reducible.quantity.names))>0)
            {
                for (comp in quantity$components)
                    private$reduce.quantity.component(comp, reducible.quantity.names)
            }
        },
        
        reduce.quantity.component = function(comp, reducible.quantity.names)
        {
            reducible.depends.on = intersect(comp$depends.on, reducible.quantity.names)
            if (length(reducible.depends.on)>0)
            {
                for (depends.on.name in reducible.depends.on) # have to make sure these are reduced first before we replace them
                    private$reduce.quantity(private$i.quantities[[depends.on.name]], reducible.quantity.names)
                
                reducible.depends.on.values = lapply(reducible.depends.on, function(dep.on){
                    # a model quantity is reducible only if it has only one component - so we refer to just the first component
                    private$i.quantities[[dep.on]]$components[[1]]$value 
                })
                names(reducible.depends.on.values) = reducible.depends.on
                
                
                if (comp$value.type=='character')
                    comp$set.value(reducible.depends.on.values[[1]])
                else # it's an expression
                {
                    new.value = do.call(substitute,
                                        list(expr=comp$value,
                                             env=reducible.depends.on.values))
                    comp$set.value(new.value)
                }
            }
        },
        
        can.reduce.quantity = function(quantity)
        {
            !quantity$type=='element' &&
                all(private$i.top.level.quantity.names != quantity$name) && 
                quantity$n.components==1 &&
                is.null(quantity$scale) &&
                is.null(quantity$components[[1]]$applies.to) &&
                is.na(quantity$components[[1]]$na.replacement) &&
                (quantity$components[[1]]$value.type=='expression' || quantity$components[[1]]$value.type=='character') &&
                all(sapply(private$get.quantities.that.depend.on(quantity$name), function(quant){
                    all(sapply(quant$components, function(comp){
                        comp$value.type!='function'
                    }))
                }))
        },
        
        # max.dim.names can derive from
        # (1) A reference that refers to this quantity
        # (2) A parent quantity
        # (3) Fixed dimensions
        calculate.quantity.dim.names = function(quantity, error.prefix)
        {
            if (!is.null(quantity$max.dim.names)) # we have already calculated it on a previous recursive call
                return()
            
            #-- Set up --#
            max.dim.names.and.aliases = list(
                dim.names = NULL,
                aliases = NULL,
                dimensions = NULL
            )
            
            required.dim.names = list()
            ref.sources = character()
            
            
            #-- Pull from top-level references --#
            for (ref in private$get.references.that.refer.to(quantity$name))
            {
                max.dim.names.and.aliases = union.shared.dim.names.and.dimensions.with.aliases(dim.names.and.aliases.1 = max.dim.names.and.aliases,
                                                                                               dim.names.2 = ref$get.max.dim.names(self, error.prefix=error.prefix),
                                                                                               aliases.2 = ref$get.dimension.aliases(self, error.prefix=error.prefix),
                                                                                               dimensions.2 = ref$max.dimensions)

                new.required.dim.names = ref$get.required.dim.names(self, error.prefix=error.prefix)
                overlapping.required.dimensions = intersect(names(required.dim.names), names(new.required.dim.names))
                for (d in overlapping.required.dimensions)
                {
                    if (!setequal(required.dim.names[[d]], new.required.dim.names[[d]]))
                        stop(paste0(error.prefix,
                                    "Cannot calculate dimnames for quantity ", 
                                    quantity$get.original.name(private$i.version),
                                    " - the required dimnames in dimension '", d,
                                    "' from the reference for ", ref$source, " [",
                                    paste0("'", new.required.dim.names[[d]], "'", collapse=', '),
                                    "] do not align with the required dimnames for the reference(s) for ",
                                    collapse.with.and(ref.sources), " [",
                                    paste0("'", required.dim.names[[d]], "'", collapse=', '),
                                    "]"))
                }
                required.dim.names[names(new.required.dim.names)] = new.required.dim.names
                
                ref.sources = c(ref.sources, ref$source)
            }
            
            #-- Pull from parent quantity.components --#
            for (comp in private$get.quantity.components.that.depend.on(quantity$name))
            {
                if (is.null(comp$parent.quantity$max.dim.names)) # Recurse up the tree
                    private$calculate.quantity.dim.names(comp$parent.quantity, error.prefix=error.prefix)
                
                if (comp$value.type != 'function')
                {   
                    bk = max.dim.names.and.aliases
                    
                    if (is.null(comp$max.dim.names))
                        to.merge.dim.names = comp$fixed.dim.names
                    else
                        to.merge.dim.names = comp$max.dim.names
                    
                    max.dim.names.and.aliases = union.shared.dim.names.and.dimensions.with.aliases(dim.names.and.aliases.1 = max.dim.names.and.aliases,
                                                                                                   dim.names.2 = to.merge.dim.names,
                                                                                                   aliases.2 = comp$dimension.aliases,
                                                                                                   dimensions.2 = comp$max.dimensions)
                }
            }
            
            max.dim.names = max.dim.names.and.aliases$dim.names
            dimension.aliases = max.dim.names.and.aliases$aliases
            max.dimensions = max.dim.names.and.aliases$dimensions
            
            #-- Reconcile fixed.dim.names --#
            max.dim.names.and.aliases = private$incorporate.quantity.fixed.dimensions(quantity = quantity,
                                                                                      max.dim.names = max.dim.names,
                                                                                      max.dimensions = max.dimensions,
                                                                                      dimension.aliases = dimension.aliases,
                                                                                      apply.even.if.max.dim.names.null = F,
                                                                                      error.prefix = error.prefix)
            
            max.dim.names = max.dim.names.and.aliases$dim.names
            dimension.aliases = max.dim.names.and.aliases$aliases
            
            
            #-- Make sure required.dim.names are compatible with these dimensions --#
            for (d in names(required.dim.names))
            {
                
            }
            
            #-- Make sure applies.to for components are compatible with these dimensions --#
            if (!is.null(max.dim.names))
            {
                sapply((1:length(quantity$components))[-1], function(i){
                    comp = quantity$components[[i]]
                    if (!is.null(comp$applies.to))
                    {
                        invalid.dimensions = setdiff(names(comp$applies.to), names(max.dim.names))
                        if (length(invalid.dimensions)>0)
                        {
                            browser()
                            stop(paste0(error.prefix,
                                        "Cannot calculate dimnames for quantity ", 
                                        quantity$get.original.name(private$i.version),
                                        ". The ", get.ordinal(i-1), " subset references ",
                                        ifelse(length(invalid.dimensions)==1, "dimension ", "dimensions "),
                                        collapse.with.and("'", invalid.dimensions, "'"), 
                                        " in it's applies.to, but ",
                                        ifelse(length(invalid.dimensions)==1, "that dimension is", "those dimensions are"),
                                        " not present in the dimnames inferred from ancestor quantities and references"))
                        }
                        
                        sapply(names(comp$applies.to), function(d){
                            invalid.values = setdiff(comp$applies.to[[d]], max.dim.names[[d]])
                            if (length(invalid.values)>0)
                                stop(paste0(error.prefix,
                                            "Cannot calculate dimnames for quantity ", 
                                            quantity$get.original.name(private$i.version),
                                            ". The ", get.ordinal(i-1), " subset applies.to for dimension '",
                                            d, "' references ",
                                            ifelse(length(invalid.values)==1, "value ", "values "),
                                            collapse.with.and("'", invalid.values, "'"),
                                            ", but ",
                                            ifelse(length(invalid.values)==1, "that value is", "those values are"),
                                            " not present in the dimnames for '", d, 
                                            "' inferred from ancestor quantities and references"))
                        })
                    }
                })
            }
            
            
            #-- Set it and return --#
            quantity$set.dim.names.and.dimension.aliases(max.dim.names = max.dim.names,
                                                         required.dim.names = required.dim.names,
                                                         max.dimensions = max.dimensions,
                                                         dimension.aliases = dimension.aliases,
                                                         error.prefix = paste0(error.prefix, "Cannot set aliases when calculating dimnames for quantity ", quantity$get.original.name(private$i.version), " - "))

            invisible(self)
        },

        calculate.quantity.dim.names.bottom.up = function(quantity, error.prefix)
        {
            if (length(quantity$depends.on)==0)
                return()
           
            #-- Step 1: Process the quantities this quantity depends on --#
            #   - make sure that each has max.dim.names set. if not, give up
            
            for (dep.on in quantity$depends.on)
            {
                dep.on.quant = self$get.quantity(dep.on)
                if (is.null(dep.on.quant$max.dim.names))
                    private$calculate.quantity.dim.names.bottom.up(quantity = dep.on.quant, error.prefix = error.prefix)
                if (is.null(dep.on.quant$max.dim.names))
                    return() # we can't unless every depends on has dim.names set
            }
            
            #-- Step 2: Process each component and its applies.to --#
            #   - If any component has value.type=='function', give up
            #   - Make sure that each quantity for each component can fit into its applies.to
            #   - Make sure that the dimnames of the quantities each component depends on overlap (ie, have a non-empty intersection)
            #   - Merge (union.joined) the applies.to for each component

            applies.to.dim.names.and.aliases = list(
                dim.names = NULL,
                aliases = NULL
            )
            
            comp.max.dim.names.and.aliases = list()
            
            
            for (i in 1:quantity$n.components)
            {
                # Pull the component and check it's not a 'function' value.type
                comp = quantity$components[[i]]
                if (comp$value.type=='function')
                    return() # we cannot infer anything from a function type
                
                # Set up the comp.max.dim.names
                comp.dn.and.aliases = list(
                    dim.names = NULL,
                    aliases = NULL
                )
                
                # For each quantity that goes into the component, make sure it fits into the applies.to for the component
                for (dep.on in comp$depends.on)
                {
                    dep.on.quant = self$get.quantity(dep.on)
                    merged.with.applies.to.dn = intersect.joined.dim.names.with.reverse.aliases(dim.names.1 = dep.on.quant$max.dim.names, 
                                                                                                aliases.1 = dep.on.quant$dimension.aliases,
                                                                                                dim.names.2 = comp$applies.to,
                                                                                                aliases.2 = character())
                    
                    # Make sure it fits into the applies.to for the component
                    for (d in names(comp$applies.to))
                    {
                        if (length(setdiff(comp$applies.to[[d]], merged.with.applies.to.dn$dim.names[[d]]))>0)
                        {
                            stop(paste0("Cannot infer dim.names, bottom-up, for quantity ",
                                        quantity$get.original.name(wrt.version=self$version),
                                        " - the dim.names of ",
                                        dep.on.quant$get.original.name(wrt.version=self$version),
                                        ", on which ",
                                        quantity$get.original.name(wrt.version=self$version),
                                        " depends, do not align with the applies.to setting for the ",
                                        get.ordinal(i-1), " subset"))
                        } 
                    }
                    
                    bk = comp.dn.and.aliases
                    # Merge into the comp.dn
                    comp.dn.and.aliases = 
                        intersect.joined.dim.names.with.reverse.aliases(dim.names.1 = comp.dn.and.aliases$dim.names, 
                                                                        aliases.1 = comp.dn.and.aliases$aliases,
                                                                        dim.names.2 = merged.with.applies.to.dn$dim.names,
                                                                        aliases.2 = merged.with.applies.to.dn$aliases)
                    if (any(sapply(comp.dn.and.aliases$dim.names, length)==0))
                        browser()
                }
                
                if (any(sapply(comp.dn.and.aliases$dim.names, length)==0))
                {
                    stop(paste0("Cannot infer dim.names, bottom-up, for quantity ",
                                quantity$get.original.name(wrt.version=self$version),
                                " - the dim.names of the quantities it depends on (",
                                collapse.with.and("'", comp$depends.on, "'"),
                                ") do not overlap (their intersection is empty)"))
                }
                
                # Save the component's max dim.names and aliases
                comp.max.dim.names.and.aliases[[i]] = comp.dn.and.aliases
                
                # Merge the applies.to with all the others
                applies.to.dim.names.and.aliases = 
                    union.joined.dim.names.with.reverse.aliases(dim.names.1 = applies.to.dim.names.and.aliases$dim.names, 
                                                                aliases.1 = applies.to.dim.names.and.aliases$aliases,
                                                                dim.names.2 = comp$applies.to,
                                                                aliases.2 = character())
            }
            
            #-- Step 3: Combine the dim.names for different components --#
            #   - Make sure that the dim.names for each component can accomodate the applies.to for all other components 
            #   - Merge the dim.names for different components (intersect.joined)
            
            merged.dim.names.and.aliases = list(
                dim.names = NULL,
                aliases = NULL
            )
            
            for (i in 1:quantity$n.components)
            {
                comp = quantity$components[[i]]
                comp.dn.and.aliases = comp.max.dim.names.and.aliases[[i]]
                
                for (j in (1:quantity$n.components)[-i])
                {
                    comp2 = quantity$components[[j]]
                    comp2.dn.and.aliases = comp.max.dim.names.and.aliases[[j]]
                    
                    dims.to.check = setdiff(intersect(names(comp$applies.to), names(comp.dn.and.aliases$dim.names)),
                                            names(comp2.dn.and.aliases$dim.names))
                    for (d in dims.to.check)
                    {
                        if (!is.null(comp2.dn.and.aliases$dim.names[[d]]))
                        {
                            missing.values = setdiff(comp.dn.and.aliases$dim.names[[d]],
                                                     comp2.dn.and.aliases$dim.names[[d]])
                         
                            if (length(missing.values)>0)
                            {
                                stop(paste0("Cannot infer dim.names, bottom-up, for quantity ",
                                            quantity$get.original.name(wrt.version=self$version),
                                            " - the dim.names of ",
                                            ifelse(j==1, "the base value of the quantity", paste0("the ", get.ordinal(j-1), " subset of the quantity")),
                                            " cannot accomodate the applies.to of the ",
                                            get.ordinal(i-1), " subset"))
                            }
                        }
                    } 
                }
                
                comp.dims.to.merge = setdiff(names(comp.dn.and.aliases$dim.names), names(comp$applies.to))
                
                merged.dim.names.and.aliases = 
                    intersect.joined.dim.names.with.reverse.aliases(dim.names.1 = merged.dim.names.and.aliases$dim.names, 
                                                                    aliases.1 = merged.dim.names.and.aliases$aliases,
                                                                    dim.names.2 = comp.dn.and.aliases$dim.names[comp.dims.to.merge],
                                                                    aliases.2 = comp.dn.and.aliases$aliases)
                
            }
            
            #-- Step 4: Fold back in the merged.applies to --#
            #   - Union.joined the merged applies.to with the merged.dim.names
            
            final.dim.names.and.aliases = 
                union.joined.dim.names.with.reverse.aliases(dim.names.1 = merged.dim.names.and.aliases$dim.names, 
                                                            aliases.1 = merged.dim.names.and.aliases$aliases,
                                                            dim.names.2 = applies.to.dim.names.and.aliases$dim.names,
                                                            aliases.2 = applies.to.dim.names.and.aliases$aliases)
            
            
            
            #-- Reconcile fixed.dim.names --#
            final.dim.names.and.aliases = 
                private$incorporate.quantity.fixed.dimensions(quantity = quantity,
                                                              max.dim.names = final.dim.names.and.aliases$dim.names,
                                                              dimension.aliases = final.dim.names.and.aliases$aliases,
                                                              apply.even.if.max.dim.names.null = T,
                                                              error.prefix = error.prefix)
            
            #-- Set it and return --#
            quantity$set.dim.names.and.dimension.aliases(max.dim.names = final.dim.names.and.aliases$dim.names,
                                                         required.dim.names = NULL,
                                                         dimension.aliases = final.dim.names.and.aliases$aliases,
                                                         error.prefix = paste0(error.prefix, "Cannot set aliases when calculating dimnames for quantity ", quantity$get.original.name(private$i.version), " - "))
            
            invisible(self)
        },

        incorporate.quantity.fixed.dimensions = function(quantity,
                                                         max.dim.names,
                                                         max.dimensions = NULL,
                                                         dimension.aliases,
                                                         apply.even.if.max.dim.names.null,
                                                         error.prefix)
        {
            if (!is.null(quantity$fixed.dimensions))
            {
                if (!is.null(max.dimensions))
                {
                    missing.dimensions = setdiff(quantity$fixed.dimensions, max.dimensions)
                    if (length(missing.dimensions)>0)
                        stop(paste0(error.prefix, "The max.dimensions for quantity ",
                                    quantity$get.original.name(private$i.version),
                                    "(", paste0(quantity$fixed.dimensions, collapse=', '), ")",
                                    " are not a subset of the inferred max dimensions (",
                                    ifelse(length(max.dimensions)==0, "dimensionless",
                                           paste0(max.dimensions, collapse=', ')),
                                    ")"))
                }
                
                # NB - NULL max.dim.names here means we have no information from which to calculate dim.names
                #      (because it is a descendant of a function)
                #      It does NOT mean that this is a scalar - that would be denoted by max.dim.name == empty list
                if (!is.null(max.dim.names))
                {
                    # make sure we are not missing any dimensions (after reconciling with aliases)
                    missing.from.max = setdiff(quantity$fixed.dimensions, names(max.dim.names))
                    if (length(missing.from.max)>0)
                    {
                        aliases.for.missing.mask = sapply(names(dimension.aliases), function(alias){
                            any(alias==missing.from.max)
                        })
                        names(max.dim.names) = replace.with.aliases(names(max.dim.names), dimension.aliases[aliases.for.missing.mask])
                        dimension.aliases = dimension.aliases[!aliases.for.missing.mask]
                        
                        missing.from.max = setdiff(quantity$fixed.dimensions, names(max.dim.names))
                        if (length(missing.from.max)>0)
                        {
                            stop(paste0(error.prefix,
                                        "Cannot calculate dimnames for quantity ", 
                                        quantity$get.original.name(private$i.version),
                                        " - the dimensions inferred from ancestor quantities and references do not include ",
                                        collapse.with.and("'", missing.from.max, "'"), ", which ",
                                        ifelse(length(missing.from.max)==1, "was a dimension", "were dimensions"),
                                        " specified by the 'dimensions' argument used in constructing this quantity"))
                        }
                    }
                }
                
                if (apply.even.if.max.dim.names.null || !is.null(max.dim.names))
                {
                    fixed.dim.names = max.dim.names[quantity$fixed.dimensions]
                    fixed.dim.names[names(quantity$fixed.dimension.values)] = quantity$fixed.dimension.values
                    
                    if (!is.null(max.dim.names))
                    {
                        sapply(names(fixed.dim.names), function(d){
                            if (!setequal(fixed.dim.names[[d]], max.dim.names[[d]]))
                            {
                                stop(paste0(error.prefix,
                                            "Cannot calculate dimnames for quantity ", 
                                            quantity$get.original.name(private$i.version),
                                            " - the values for dimension '", d, "' inferred from ancestor quantities (",
                                            collapse.with.and("'", max.dim.names[[d]], "'"),
                                            " do not match the values set by the 'dimension.values' used in constructing the quantity (",
                                            collapse.with.and("'", fixed.dim.names[[d]], "'")))
                            }
                        })
                    }
                    
                    quantity$set.fixed.dim.names(fixed.dim.names)
                    max.dim.names[names(fixed.dim.names)] = fixed.dim.names
                    dimension.aliases = dimension.aliases[intersect(names(dimension.aliases), names(fixed.dim.names))]
                }
            }
            
            list(dim.names = max.dim.names,
                 aliases = dimension.aliases)
        },
        
        calculate.dependencies = function()
        {
            private$i.element.names = names(private$i.quantities)[sapply(private$i.quantities, function(quantity){
                quantity$type == 'element'
            })]
            
            #-- Make Direct Dependency Matrix --#
            # i.direct.dependencies[i,j] is true if quantity j's depends.on value includes quantity i
            private$i.direct.dependencies = sapply(private$i.quantities, function(dependent){
                sapply(private$i.quantities, function(dependee){
                    any(dependent$depends.on == dependee$name)
                })
            })
            
            dimnames(private$i.direct.dependencies) = list(
                dependee = names(private$i.quantities),
                dependent = names(private$i.quantities)
            )
            
            #-- Make Dependency Matrix --#
            # i.dependencies[i,j] is true if quantity j ultimately depends on quantity i
            private$i.dependencies = t(sapply(names(private$i.quantities), private$get.dependency.row))
            dimnames(private$i.dependencies) = list(
                dependee = names(private$i.quantities),
                dependent = names(private$i.quantities)
            )
            
            #-- Set up dependency vectors --#
            private$i.dependent.quantity.names = lapply(names(private$i.quantities), function(quant.name){
                names(private$i.quantities)[private$i.dependencies[quant.name,]]
            })
            names(private$i.dependent.quantity.names) = names(private$i.quantities)
            
            private$i.dependent.top.level.quantity.names = lapply(private$i.dependent.quantity.names, function(quant.names){
                intersect(quant.names, private$i.top.level.quantity.names)
            })
            
            private$i.co.dependee.element.names = lapply(private$i.element.names, function(elem.name){
                mask = apply(private$i.dependencies[private$i.element.names, private$i.dependend.quantity.names[[elem.name]], drop=F], 1, any)
                setdiff(private$i.element.names[mask], elem.name)
            })
            names(private$i.co.dependee.element.names) = private$i.element.names
            
            private$i.dependee.element.names = lapply(names(private$i.quantities), function(quant.name){
                private$i.element.names[private$i.dependencies[private$i.element.names, quant.name]]
            })
            names(private$i.dependee.element.names) = names(private$i.quantities)
            
            private$i.dependee.quantity.names = lapply(names(private$i.quantities), function(quant.name){
                names(private$i.quantities)[private$i.dependencies[ names(private$i.quantities), quant.name]]
            })
            names(private$i.dependee.quantity.names) = names(private$i.quantities)
            
            #-- Done --#
            invisible(self)
        },
        
        # a logical vector with one element corresponding to each quantity name
        # rv[i] is true if this quantity or a descendant quantity depends on quantity i
        get.dependency.row = function(quantity.name)
        {
            rv = private$i.direct.dependencies[quantity.name,]
            if (any(rv))
            {
                direct.depends.on = names(private$i.quantities[rv])
                for (depends.on.name in direct.depends.on)
                    rv = rv | private$get.dependency.row(depends.on.name)
            }
            rv
        },
        
        # a helper function that wraps print statements in a check if verbose
        do.print = function(text)
        {
            if (i.verbose)
                base::print(text)
        },
        
        do.cat = function(..., sep='')
        {
            if (i.verbose)
                base::cat(..., sep='')
        },
        
        ##-------------##
        ##-- GETTERS --##
        ##-------------##
        
        get.quantity.from.ancestor = function(name,
                                               ancestor.version)
        {
            private$i.ancestor.specifications[[ancestor.version]]$get.quantity(name)
        },
        
        resolve.quantity.name = function(name, this.refers.to.version)
        {
            if (string.begins.with(name, prefix='super'))
            {
                min.ancestor.index = 1 + first.index.of(names(private$i.ancestor.specifications), this.refers.to.version)
                name = substr(name, 7, nchar(name))
            }
            else if (string.begins.with(name, prefix='this'))
            {
                min.ancestor.index = index.of(names(private$i.ancestor.specifications), this.refers.to.version)
                name = substr(name, 6, nchar(name))
            }
            else if (string.begins.with(name, prefix='quantity'))
            {
                min.ancestor.index = 1
                name = substr(name, 10, nchar(name))
            }
            else
            {
                min.ancestor.index = 1
            }
            
            
            for (index in min.ancestor.index:length(private$i.ancestor.specifications))
            {
                quantity = private$i.ancestor.specifications[[index]]$get.quantity(name)
                if (!is.null(quantity)) # we found it, package up and return
                    return(quantity)
                
                if (any(name == private$i.ancestor.specifications[[index]]$do.not.inherit.model.quantity.names))
                    return (NULL)
            }
            
            # we didn't find it
            NULL
        },

        resolve.outcome.name = function(name, this.refers.to.version)
        {
            if (string.begins.with(name, prefix='super'))
            {
                min.ancestor.index = 1 + first.index.of(names(private$i.ancestor.specifications), this.refers.to.version)
                name = substr(name, 7, nchar(name))
            }
            else if (string.begins.with(name, prefix='this'))
            {
                min.ancestor.index = index.of(names(private$i.ancestor.specifications), this.refers.to.version)
                name = substr(name, 6, nchar(name))
            }
            else
            {
                min.ancestor.index = 1
            }
            
            for (index in min.ancestor.index:length(private$i.ancestor.specifications))
            {
                outcome = private$i.ancestor.specifications[[index]]$get.outcome(name)
                if (!is.null(outcome)) # we found it, package up and return
                    return(outcome)
                
                if (any(name == private$i.ancestor.specifications[[index]]$do.not.inherit.model.outcome.names))
                    return (NULL)
            }
            
            # we didn't find it
            NULL
        }
        
        ##-------------------------##
        ##-- CONSTRUCTOR HELPERS --##
        ##-------------------------##
        
       
    )
)

##------------------------------------------##
##------------------------------------------##
##-- COMPILING and SORTING CORE COMPONENT --##
##------------------------------------------##
##------------------------------------------##

compile.and.sort.core.components.for.location <- function(specification,
                                                          specification.metadata,
                                                          ontologies,
                                                          error.prefix)
{
    #-- Compile the components --#
    compiled.components = lapply(specification$sorted.core.components, function(comps){
        
        rv = list()
        for (comp in comps)
        {
            rv = c(rv, 
                   comp$schema$compile.component(comp,
                                                 specification = specification,
                                                 ontologies = ontologies,
                                                 aliases = specification.metadata$compartment.aliases,
                                                 unresolved.alias.names = character(),
                                                 unpack = T,
                                                 error.prefix = error.prefix))
        }
        
        rv
    })
    
    #-- Check for Clashes --#
    
    # If there is a clash
    # - If both components are from the same version, throw an error
    # - If different versions, throw out the later component (from an earlier version)
    
    i = 1
    while (i <= length(compiled.components))
    {
        comp = compiled.components[[j]]
        j = i + 1
        while (j <= length(compiled.components))
        {
            other.comp = compiled.components[[j]]
            if (comp$schema$components.clash(comp, other.comp))
            {
                if (comp$version == other.comp$version)
                    stop()
                else # remove the later component
                    compiled.components = compiled.components[-j]
            }
            else
                j = j+1
        }
        
        i = i+1
    }
    

    #-- Sort --#
    sorted.components = lapply(CORE.COMPONENT.SCHEMATA, function(sch){
        compiled.components[sapply(compiled.components, function(comp){comp$type}) == sch$type]
    })
    names(sorted.components) = names(CORE.COMPONENT.SCHEMATA)
    
    #-- Done: Return --#
    sorted.components
}


##-----------------------------------------##
##-----------------------------------------##
##-- TOP-LEVEL REFERENCE CLASS HIERARCHY --##
##-----------------------------------------##
##-----------------------------------------##

TOP.LEVEL.REFERENCE = R6::R6Class(
    'top.level.reference',
    portable = F,
    
    public = list(
        
        initialize = function(specification,
                              version,
                              ontology.name = NULL,
                              dim.names = NULL,
                              value.quantity.name,
                              source,
                              type='top.level.reference',
                              applies.to,
                              required.sub.ontology.name=NULL,
                              exclude.ontology.dimensions=character(),
                              max.dimensions = NULL,
                              alias.suffix,
                              error.prefix)
        {
            # Validate dim.names or ontology.name
            if (is.null(dim.names))
            {
                if (!is.null(ontology.name))
                {
                    # Validate ontology.name
                    if (!is.character(ontology.name) || length(ontology.name)!=1 || is.na(ontology.name) || nchar(ontology.name)==0)
                        stop(paste0(error.prefix, "'ontology.name' for a ", self$descriptor, " must be a single, non-NA, non-empty character value"))
                    
                    
                    if (is.null(names(ontology.name)) || is.na(names(ontology.name)) || nchar(names(ontology.name))==0)
                        names(ontology.name) = ontology.name
                    
                    if (all(names(ontology.name)!=names(specification$ontologies)))
                        stop(paste0(error.prefix, 
                                    "In creating a ", self$descriptor, " names(ontology.name) must be one of ",
                                    collapse.with.or("'", names(specification$ontologies), "'"),
                                    ". ", names(ontology.name), 
                                    " is not a valid name"))
                }
            }
            else 
            {
                if (!is.null(ontology.name))
                    stop(paste0(error.prefix, "In creating a top-level reference, either 'ontology.name' or 'dim.names' but NOT both can be set. One or the other must be NULL"))
                
                check.dim.names.valid(dim.names = dim.names,
                                      variable.name.for.error = 'dim.names',
                                      allow.empty = T,
                                      allow.duplicate.values.across.dimensions = T,
                                      error.prefix = error.prefix)
            }
            
            # Validate max.dimensions
            if (!is.null(max.dimensions))
            {
                if (!is.character(max.dimensions) || any(is.na(max.dimensions)))
                    stop(paste0(error.prefix, "'max.dimensions' for a ", self$descriptor, " must be a character vector with no NA values"))
            }
            
            # Validate version
            if (!is.character(version) || length(version)!=1 || is.na(version))
                stop(paste0(error.prefix, "'version' for a ", self$descriptor, " must be a single, non-NA character vector"))
            
            # Validate value.quantity.name
            if (!is.character(value.quantity.name) || length(value.quantity.name)!=1 || is.na(value.quantity.name) || nchar(value.quantity.name)==0)
                stop(paste0(error.prefix, "'value.quantity.name' for a ", self$descriptor, " must be a single, non-NA, non-empty character value"))
            
            # Validate type
            if (!is.character(type) || length(type)!=1 || is.na(type) || nchar(type)==0)
                stop(paste0(error.prefix, "'type' for a ", self$descriptor, " must be a single, non-NA, non-empty character value"))
            
            # Validate source
            if (!is.character(source) || length(source)!=1 || is.na(source) || nchar(source)==0)
                stop(paste0(error.prefix, "'source' for a ", self$descriptor, " must be a single, non-NA, non-empty character value"))
            
            # Validate applies.to
            if (length(applies.to)==0)
                applies.to = list()
            else
                validate.applies.to(applies.to, variable.name.for.error = 'applies.to', error.prefix = error.prefix)
            
            # Validate required.sub.ontology.name
            if (!is.null(required.sub.ontology.name) &&
                (!is.character(required.sub.ontology.name) || length(required.sub.ontology.name)!=1 || is.na(required.sub.ontology.name)))
                stop(paste0(error.prefix, "If it is not NULL, 'required.sub.ontology.name' for a ", self$descriptor,
                            " must be a single, non-NA character value"))
            
            # Validate exclude.ontology.dimensions
            if (!is.null(exclude.ontology.dimensions) &&
                (!is.character(exclude.ontology.dimensions) || any(is.na(exclude.ontology.dimensions))))
                stop(paste0(error.prefix, "If it is not NULL, 'exclude.ontology.dimensions' for a ", self$descriptor,
                            " must be a character vector without NA values"))
            
            
            # Validate alias.suffix
            if (!is.null(alias.suffix))
            {
                if (!is.character(alias.suffix) || length(alias.suffix)!=1 || is.na(alias.suffix) ||
                    (alias.suffix != 'from' && alias.suffix != 'to'))
                    stop(paste0(error.prefix, "If it is not NULL, 'alias.suffix' for a ", self$descriptor,
                                " must be a single, non-NA character value that is either 'from' or 'to'"))
            }
            
            private$i.version = version
            private$i.dim.names = dim.names
            private$i.ontology.name = ontology.name
            private$i.value.quantity.name = value.quantity.name
            private$i.type = type
            private$i.source = source
            private$i.alias.suffix = alias.suffix
            private$i.applies.to = applies.to
            private$i.required.sub.ontology.name = required.sub.ontology.name
            private$i.exclude.ontology.dimensions = exclude.ontology.dimensions
            private$i.max.dimensions = max.dimensions
        },
        
        
        
        
        
        overlaps = function(other.reference)
        {
            self$type == other.reference$type &&
                self$name == other.reference$name &&
                self$ontology.name == other.reference$ontology.name
        },
        
        equals = function(other.reference)
        {
            self$type == other.reference$type &&
                self$name == other.reference$name &&
                self$ontology.name == other.reference$ontology.name
        },
        
        compile = function()
        {
            rv = self$clone(deep=T)
            class(rv) = NULL
            rv
        },
        
        resolve.compartment.values = function(aliases, 
                                              ontology,
                                              unresolved.alias.names,
                                              ontology.name.for.error,
                                              error.prefix,
                                              wrt.specification)
        {
            # nothing to do for this class
            invisible(self)
        },
        
        get.description = function(wrt.specification, with.quotes=T)
        {
            if (with.quotes)
                qu = "'"
            else
                qu = ''
            
            if (length(wrt.specification$top.level.schemata[private$i.name])>1)
                paste0(qu, private$i.name, qu, " (for ", qu, private$i.ontology.name, qu, ")")
            else
                paste0(qu, private$i.name, qu)
        },
        
        set.value.quantity.name = function(value.quantity.name)
        {
            if (!is.character(value.quantity.name) || length(value.quantity.name)!=1 || is.na(value.quantity.name))
                stop("In set.value.quantity.name(), 'value.quantity.name' must be a single, non-NA character value")
            
            private$i.value.quantity.name = value.quantity.name
        },
        
        get.max.dim.names = function(specification, error.prefix)
        {
            if (is.null(private$i.dim.names))
            {
                if (is.null(private$i.ontology.name))
                    return (NULL)
                
                rv = specification$ontologies[[private$i.ontology.name]]
            }
            else
                rv = private$i.dim.names
            
            if (is.null(rv))
                stop(paste0(error.prefix, "Missing 'ontology.name' - the specification does not contain an ontology named '", private$i.ontology.name, "'"))
            
            # Check that applies to is a subset of the rv
            if (!dim.names.are.subset(sub.dim.names = private$i.applies.to, super.dim.names = rv))
                stop(paste0(error.prefix, "The applies.to for the top-level reference using '", private$i.value.quantity.name, 
                            "' is not a subset of the dim.names derived from the ontology ('", private$i.ontology.name, "')",
                            ifelse(length(private$i.exclude.ontology.dimensions)==1, '', 
                                   paste0(" less dimension(s) ", collapse.with.and("'", private$i.exclude.ontology.dimensions, "'")))))
            
            rv = rv[setdiff(names(rv), private$i.exclude.ontology.dimensions)]
            
            # Overwrite applies to values
            rv[names(private$applies.to)] = private$i.applies.to
         
            if (!is.null(private$i.max.dimensions))
                rv = rv[intersect(private$i.max.dimensions, names(rv))]
               
            rv
        },

        get.required.dim.names = function(specification, error.prefix)
        {
            if (is.null(private$i.required.sub.ontology.name))
                list()
            else
            {
                rv = specification$ontologies[[private$i.required.sub.ontology.name]]
                if (is.null(rv))
                    stop(paste0(error.prefix, "Missing 'required.sub.ontology.name' - the specification does not contain an ontology named '", private$i.required.sub.ontology.name, "'"))
                
                rv = rv[setdiff(names(rv), private$i.exclude.ontology.dimensions)]
                
                applies.to.dimensions = intersect(names(private$i.applies.to), names(rv))
                rv[applies.to.dimensions] = private$i.applies.to[applies.to.dimensions]
                
                rv
            }
        },
        
        get.dimension.aliases = function(specification, error.prefix)
        {
            if (is.null(private$i.alias.suffix))
                character()
            else
            {
                rv = names(self$get.max.dim.names(specification, error.prefix))
                names(rv) = rv
                
                has.suffix.mask = substr(rv, nchar(rv)-nchar(private$i.alias.suffix)+1, nchar(rv)) == private$i.alias.suffix
                rv[has.suffix.mask] = substr(rv[has.suffix.mask], 1, nchar(rv[has.suffix.mask])-nchar(private$i.alias.suffix)-1)
                
                rv = rv[names(rv) != rv]
                
                rv
            }
        }
    ),
    
    active = list(
        
        descriptor = function(value)
        {
            if (missing(value))
                "top-level reference"
            else
                stop("Cannot modify a top.level.reference's 'descriptor' - it is read-only")
        },
        
        version = function(value)
        {
            if (missing(value))
                private$i.version
            else
                stop(paste0("Cannot modify a ", self$descriptor, "'s 'version' - it is read-only"))
        },
        
        ontology.name = function(value)
        {
            if (missing(value))
                private$i.ontology.name
            else
                stop(paste0("Cannot modify a ", self$descriptor, "'s 'ontology.name' - it is read-only"))
        },
        
        type = function(value)
        {
            if (missing(value))
                private$i.type
            else
                stop(paste0("Cannot modify a ", self$descriptor, "'s 'type' - it is read-only"))
        },
        
        source = function(value)
        {
            if (missing(value))
                private$i.source
            else
                stop(paste0("Cannot modify a ", self$descriptor, "'s 'source' - it is read-only"))
        },
        
        value.quantity.name = function(value)
        {
            if (missing(value))
                private$i.value.quantity.name
            else
                stop(paste0("Cannot modify a ", self$descriptor, "'s 'value.quantity.name' - it is read-only"))
        },
        
        max.dimensions = function(value)
        {
            if (missing(value))
                private$i.max.dimensions
            else
                stop(paste0("Cannot modify a ", self$descriptor, "'s 'max.dimensions' - it is read-only"))
        }
    ),
    
    private = list(
        
        i.version = NULL,
        i.value.quantity.name = NULL,
        i.dim.names = NULL,
        i.ontology.name = NULL,
        i.type = NULL,
        i.source = NULL,
        i.alias.suffix = NULL,
        i.applies.to = NULL,
        i.required.sub.ontology.name = NULL,
        i.exclude.ontology.dimensions = NULL,
        i.max.dimensions = NULL
    )
)


