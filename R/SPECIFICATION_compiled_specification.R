

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
                              required.dimensions.for.ontologies,
                              
                              compartments,
                              
                              quantities,
                              
                              age.info,
                              transmission.modes,
                              fix.strata.sizes.prior.to,
                              fix.strata.sizes.for.dimensions,
                              
                              enable.perinatal.transmission,
                              parent.child.concordant.dimensions,
                              all.births.into.compartments,
                              
                              parent.specification,
                              do.not.inherit.model.quantity.names,
                              do.not.inherit.transitions.for.dimension,
                              
                              top.level.schemata,
                              top.level.references,
                              
                              do.compile)
        {
            #-- Copy over --#            
            private$i.version = version
            private$i.iteration = iteration
            private$i.description = description
            
            private$i.compartment.value.character.aliases = compartment.value.character.aliases
            private$i.compartment.value.function.aliases = compartment.value.function.aliases
            private$i.ontologies = ontologies
            private$i.required.dimensions.for.ontologies = required.dimensions.for.ontologies
            
            private$i.compartments = compartments
            
            private$i.quantities = lapply(quantities, function(quant){quant$compile()})
            
            private$i.age.info = age.info
            private$i.transmission.modes = transmission.modes
            private$i.fix.strata.sizes.prior.to = fix.strata.sizes.prior.to
            private$i.fix.strata.sizes.for.dimensions = fix.strata.sizes.for.dimensions
            
            private$i.enable.perinatal.transmission = enable.perinatal.transmission
            private$i.parent.child.concordant.dimensions = parent.child.concordant.dimensions
            private$i.all.births.into.compartments = all.births.into.compartments
            
            private$i.parent.specification = parent.specification
            private$i.do.not.inherit.model.quantity.names = do.not.inherit.model.quantity.names
            private$i.do.not.inherit.transitions.for.dimension = do.not.inherit.transitions.for.dimension
            
            private$i.top.level.schemata = top.level.schemata
            private$i.top.level.references = top.level.references
            
            
            #-- Copy ancestor specifications and add this one --#
            private$i.ancestor.specifications = list(self)
            names(private$i.ancestor.specifications) = private$i.version
            if (!is.null(private$i.parent.specification))
                private$i.ancestor.specifications = c(private$i.ancestor.specifications,
                                                      private$i.parent.specification$ancestor.specifications)
            
            #-- If requested, do the compilation --#
            if (do.compile)
                private$do.compile()
        },
        
        ##--------------------------------------##
        ##-- PUBLIC to the JHEEM.ENGINE CLASS --##
        ##--------------------------------------##
                
        get.quantity = function(name)
        {
            i.quantities[[name]]
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
        },
        
        resolve.reference.compartment.values = function(aliases, 
                                                        ontology,
                                                        unresolved.alias.names,
                                                        ontology.name.for.error,
                                                        error.prefix,
                                                        wrt.specification)
        {
            private$i.top.level.references = lapply(private$i.top.level.references, function(ref){
                ref$resolve.compartment.values(aliases = aliases, 
                                               ontology = ontology,
                                               unresolved.alias.names = unresolved.alias.names,
                                               ontology.name.for.error = ontology.name.for.error,
                                               error.prefix = error.prefix,
                                               wrt.specification = wrt.specification)
            })
            
            invisible(self)
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
        }
    ),
    
    private = list(
        
        i.ancestor.specifications = NULL,
        
        i.verbose = NULL,
        
        i.top.level.quantity.names = NULL,
        i.element.names = NULL,
        
        # indexed dependee, dependent
        i.direct.dependencies = NULL,
        i.dependencies = NULL,
        
        i.dependent.quantity.names = NULL,
        i.dependent.top.level.quantity.names = NULL,
        i.co.dependee.element.names = NULL,
        i.dependee.element.names = NULL,
        
        #----------------------------------#
        #-- COMPILE FUNCTION and HELPERS --#
        #----------------------------------#
        
        do.compile = function(verbose=F) #the verbose flag is for debugging only
        {
            private$i.verbose = verbose
            error.prefix = paste0("Error compiling model specification for '", private$i.version, "': ")
            
            #-- Print a start message --#
            do.cat(paste0("Starting compilation of specification '", private$i.version, "'\n"))

            #-- Process aliases for consistency --#
            #-- Apply aliases/resolve compartment values for top-level references --#
            do.cat("Processing character aliases and resolving top-level reference names...")
            private$process.compartment.aliases()
            for (spec in private$i.ancestor.specifications)
                spec$resolve.reference.compartment.values(aliases = private$i.compartment.value.character.aliases,
                                                          ontology = private$i.ontologies$all,
                                                          unresolved.alias.names = names(private$i.compartment.value.function.aliases),
                                                          ontology.name.for.error = "the ontology",
                                                          error.prefix = error.prefix,
                                                          wrt.specification = self)
            do.cat("done\n")
            
            #-- Pull all top-level references --#
            #-- Make sure required are present --#
            #-- Check top-level references for clashes --#
            do.cat("Deriving top-level references...")
            private$derive.top.level.references(error.prefix=error.prefix)
            do.cat("done\n")
            
            #-- Make sure there are no circular references --#
            #-- Make sure that any reference to 'super' and 'this' (in parent mapping) model.quantities are valid --#
            #-- Rename quantities as needed to refer to ancestor quantities --#
            #-- Identify all the quantities and elements we will need --#
            #     (Either in this mapping or in an ancestor mapping)    #
            do.cat("Parsing model.quantity tree...")
            self$rename.quantities(character()) # rename the quantities first so we can get a unique name for each
            quantities.to.use = private$parse.quantity.tree(private$i.top.level.references)
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


            #-- Remake into a new, flattened (parentless) mapping with just the quantities we need --#
            private$i.quantities = quantities.to.use
            private$i.parent.specification = NULL
            private$i.ancestor.specifications = private$i.ancestor.specifications[1]
            
            
            ##** FROM HERE DOWN THE TRANSITION MAPPING IS FLATTENED **##
            
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
            
            
            #-- Map out max dim.names for each quantity --#
            #-- Check that applies.to dimensions are a subset of the calculated dim.names --#
            do.cat("Calculating dim.names for quantities...")
            for (quantity in private$i.quantities)
                private$calculate.quantity.dim.names(quantity, error.prefix=error.prefix)
            do.cat("done\n")
            
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
                        rv = private$parse.quantity.tree(depends.on.quantities,
                                                         ancestor.quantities = new.ancestor.quantities)
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

        # Sub in the character compartment aliases to ontologies and compartments
        # Figure out where function compartment aliases are going to plug in and make sure they are not duplicated where they should not be
        process.compartment.aliases = function(error.prefix)
        {
            # Sub-in aliases to ontologies and compartments
            private$i.ontologies = apply.aliases(private$i.ontologies, aliases=private$i.compartment.value.character.aliases)
            private$i.compartments = apply.aliases(private$i.compartments, aliases=private$i.compartment.value.character.aliases)
          
            # Sub-in aliases to all.births.into.compartments
            private$i.all.births.into.compartments = do.resolve.dimension.values(dimension.values = private$i.all.births.into.compartments,
                                                                                 aliases = private$i.compartment.value.character.aliases,
                                                                                 ontology = private$i.ontologies$all,
                                                                                 unresolved.alias.names = names(private$i.compartment.value.function.aliases),
                                                                                 variable.name.for.error = 'all.births.into.compartments',
                                                                                 ontology.name.for.error = 'the ontology',
                                                                                 error.prefix = paste0(error.prefix, "Cannot substitute aliases into 'all.births.into.compartments': "))
            
            more.than.one.element.mask = sapply(private$i.all.births.into.compartments, length)>1
            if (any(more.than.one.element.mask))
                stop(paste0(error.prefix,
                            "After substituting in aliases, 'all.births.into.compartments' for ",
                            ifelse(sum(more.than.one.element.mask)==1, 'dimension ', 'dimensions '),
                            collapse.with.and("'", names(private$i.all.births.into.compartments)[more.than.one.element.mask], "'"),
                            ifelse(sum(more.than.one.element.mask)==1, 'has more than one compartment in it', 'have more than one compartment in them'),
                            ". 'all.births.into.compartments' can only have one compartment value per dimension"))
                                                                   
            
            invalid.mask = sapply(private$i.all.births.into.compartments, function(val){
                any(val == names(private$i.compartment.value.function.aliases))
            })
            if (any(invalid.mask))
                stop(paste0(error.prefix,
                            "'all.births.into.compartments' cannot contain values that are compartment.value.aliases given by functions. After substituting in (character) aliases, ",
                            ifelse(sum(invalid.mask)==1, "dimension ", "dimensions "),
                            collapse.with.and("'", names(private$i.all.births.into.compartments)[invalid.mask], "'"),
                            ifelse(sum(invalid.mask)==1, "contains an invalid value: ", "contain invalid values: "),
                            collapse.with.and("'", sapply(private$i.all.births.into.compartments[invalid.mask], function(val){val}), "'"),
                            ifelse(sum(invalid.mask)==1, "", " respectively."),
                ))
            
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
        
        # NB - cannot do this until aliases have been applied (could obscure 'equals' criteria)
        derive.top.level.references = function(error.prefix)
        {
            #-- Pull top-level references from ancestors --#
            for (ancestor.spec in private$i.ancestor.specifications[-1])
            {
                # Decide which ones we will import from ancestor
                # To keep from parent, must satisfy both:
                # (1) Not duplicated (equal to) a reference in this specification
                # (2) If a transition or not in the list of 'do-not-inherit' transition dimensions and dimension applies to group in our current ontology
                #     If not a transition, name and ontology is in top-level schemata
                not.duplicated.mask =  sapply(ancestor.spec$top.level.references, function(ref){
                    !any(sapply(private$i.top.level.references, ref$equals))
                })
                can.inherit.mask =  sapply(ancestor.spec$top.level.references, function(ref){
                    if (ref$type == 'transition.reference')
                        all(ref$dimension != private$i.do.not.inherit.transitions.for.dimension) &&
                            any(names(private$i.ontologies[[ref$ontology.name]]) == ref$dimension)
                    else
                        any(sapply(private$i.top.level.schemata, function(schema){
                            schema$name == ref$name && any(schema$ontology.names == ref$ontology.name)
                        }))
                })
                
                to.inherit = ancestor.spec$top.level.references[not.duplicated.mask & can.inherit.mask]
                
                # Check for overlaps between references we are inheriting and this top-level references
                sapply(private$i.top.level.references, function(ref){
                    overlaps.mask = sapply(to.inherit, ref$overlaps)
                    if (any(overlaps.mask))
                    {
                        overlaps.with = sapply(to.inherit[overlaps.mask], function(clashing.ref){
                            paste0(clashing.ref$get.description(wrt.specification=self),
                                   " in specification '", clashing.ref$version, "'")
                        })
                        
                        stop(paste0(error.prefix,
                                    ifelse(ref$type=='transition.reference', '', "Top-level quantity "),
                                    ref$get.description(wrt.specification=self),
                                    "' overlaps with ",
                                    collapse.with.and(overlaps.with)))
                    }
                })
                
                # Merge this references with references imported from parent
                private$i.top.level.references = c(private$i.top.level.references, to.inherit)
            }
            
            #-- Check to see if missing top-level reference schemata have a matching quantity name registered --#
            for (schema in private$i.top.level.schemata)
            {
                missing.ontology.name.mask = sapply(schema$ontology.names, function(ont.name){
                    ref.matches = sapply(private$i.top.level.references, function(ref){
                        !is.null(ref$name) && ref$name==schema$name && ref$ontology.name==ont.name
                    })
                    all(!ref.matches)
                })
                
                if (any(missing.ontology.name.mask))
                {
                    missing.ontology.names = schema$ontology.names[missing.ontology.name.mask]
                    if (is.null(private$resolve.quantity.name(schema$name,
                                                              this.refers.to.version=private$i.version)))
                    { # there is no quantity with that name
                        if (schema$required)
                        {
                            stop(paste0(error.prefix,
                                        "Missing required model quantity '",
                                        schema$name, "'",
                                        ifelse(length(schema$ontology.names)==1, 
                                               "",
                                               paste0(" for ", collapse.with.and("'", missing.ontology.names, "'"))
                                        )))
                        }
                    }
                    else # there is a quantity with that name - register it as a top-level reference
                    {
                        self$register.top.level.quantity(name = schema$name,
                                                         value = schema$name,
                                                         groups=names(missing.ontology.names))
                    }
                }
                
            }
            
            
            #-- Check for clashes between the aging quantity and transitions in the age dimension --#
            
            aging.references = private$i.top.level.references[sapply(private$i.top.level.references, function(ref){
                !is.null(ref$name) && ref$name=='aging'
            })]
            
            if (length(aging.references)>0)
            {
                ontology.names = sapply(aging.references, function(ref){
                    ref$ontology.name
                })
                
                clashes.with.ontology.name = sapply(ontology.names, function(ont){
                    any(sapply(private$i.top.level.references, function(ref){
                        ref$type == 'transition.reference' && ref$ontology.name == ont &&
                            ref$dimension == 'age'
                    }))
                })
                
                if (any(clashes.with.ontology.name))
                    stop(paste0(error.prefix,
                                "The quantity 'aging' has been registered for ",
                                collapse.with.and(ontology.names[clashes.with.ontology.name]),
                                ", but one or more transitions have been registered in the 'age' dimension. You can register EITHER the 'aging' *quantity* OR transitions in the 'age' dimension"))
            }
            
            #-- Done - return --#
            
            invisible(self)
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
                aliases = NULL
            )
            
            #-- Pull from top-level references --#
            for (ref in private$get.references.that.refer.to(quantity$name))
            {
                max.dim.names.and.aliases = union.shared.dim.names.with.aliases(dim.names.1 = max.dim.names.and.aliases$dim.names, 
                                                                                aliases.1 = max.dim.names.and.aliases$aliases,
                                                                                dim.names.2 = ref$get.max.dim.names(self),
                                                                                aliases.2 = ref$get.dimension.aliases(self))
            }
            
            #-- Pull from parent quantity.components --#
            for (comp in private$get.quantity.components.that.depend.on(quantity$name))
            {
                if (is.null(comp$parent.quantity$max.dim.names)) # Recurse up the tree
                    private$calculate.quantity.dim.names(comp$parent.quantity, error.prefix=error.prefix)
                
                if (comp$value.type != 'function')
                {   
                    max.dim.names.and.aliases = union.shared.dim.names.with.aliases(dim.names.1 = max.dim.names.and.aliases$dim.names, 
                                                                                    aliases.1 = max.dim.names.and.aliases$aliases,
                                                                                    dim.names.2 = comp$max.dim.names,
                                                                                    aliases.2 = comp$dimension.aliases)
                }
            }
            
            max.dim.names = max.dim.names.and.aliases$dim.names
            dimension.aliases = max.dim.names.and.aliases$aliases
            
            #-- Reconcile fixed.dim.names --#
            if (!is.null(quantity$fixed.dimensions))
            {
                # NB - NULL max.dim.names here means we have no information from which to calculate dim.names
                #      (because it is a descendant of a function)
                #      It does NOT mean that this is a scalar - that would be denoted by max.dim.name == empty list
                if (!is.null(max.dim.names))
                {
                    # need to reconcile with aliases
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
                max.dim.names = fixed.dim.names
                dimension.aliases = dimension.aliases[intersect(names(dimension.aliases), names(fixed.dim.names))]
            }
            
            #-- Make sure applies.to for components are compatible with these dimensions --#
            sapply((1:length(quantity$components))[-1], function(i){
                comp = quantity$components[[i]]
                if (!is.null(comp$applies.to))
                {
                    invalid.dimensions = setdiff(names(comp$applies.to), names(max.dim.names))
                    if (length(invalid.dimensions)>0)
                    {
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
            
            
            #-- Set it and return --#
            quantity$set.max.dim.names.and.dimension.aliases(max.dim.names = max.dim.names,
                                                             dimension.aliases = dimension.aliases,
                                                             error.prefix = paste0(error.prefix, "Cannot set aliases when calculating dimnames for quantity ", quantity$get.original.name(private$i.version), " - "))
            
            invisible(self)
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
        }
        
        ##-------------------------##
        ##-- CONSTRUCTOR HELPERS --##
        ##-------------------------##
        
       
    )
)

