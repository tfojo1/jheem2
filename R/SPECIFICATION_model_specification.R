
# Depends on
#  SPECIFICATION_evaluatable_value.R
#  SPECIFICATION_functional_forms.R
#  SPECIFICATION_links.R
#  HELPERS_dim_names_helpers.R
#  HELPERS_misc_helpers.R


##---------------------------------##
##---------------------------------##
##-- PUBLIC INTERFACE for SET-UP --##
##---------------------------------##
##---------------------------------##

#'@description Create a Model Specification for Running the JHEEM
#'
#'@param version A single character value denoting the version
#'@param iteration A single character or numeric value denoting the iteration of the model specification for this version
#'@param description A short text description of this version
#'
#'@param parent.version The character version of the specification from which this new specification should inherit. Must have been registered
#'@param do.not.inherit.model.quantity.names A vector of names of model.quantities which should NOT be inherited from ancestor specifications
#'@param do.not.inherit.transitions.for.dimension A vector of names of dimensions for which transitions should NOT be inherited from ancestor specifications
#'
#'@param compartments.for.infected.only,compartments.for.uninfected.only,compartments.for.infected.and.uninfected Named lists of character vectors specifying the compartments for uninfected and infected groups, or compartments shared by both. The names of the lists represent dimensions, and the values the compartments for each dimension. Compartments can either be string referencing the compartments themselves, or strings representing aliases passed to compartment.value.aliases
#'
#'@param transmission.modes A character vector of one or more modes of transmission
#'@param age.endpoints Optional. A numeric vector (with at least two elements) giving the endpoints of the age brackets to use for the 'age' dimension. Results in length(age.endpoints)-1 different brackets, where the first bracket spans age.endpoints[1] (inclusive) to age.endpoints[2] (exclusive), the second bracket spans age.endpoints[2] to age.endpoints[3], etc
#'
#'@param compartment.value.aliases A named list representing substitutions to be made into compartment names (both in compartments.for.infected.only, compartments.for.uninfected.only, compartments.for.infected.and.uninfected and in subsequent references in registering model quantities). The names of the list represent what is to be replaced. The values of the list can be either (1) character vectors that are substituted in place or (2) functions that take parameter 'location' and return a character vector
#'
#'@param enable.perinatal.transmission A logical indicator of whether infection can be passed through birth
#'@param parent.child.concordant.dimensions A character vector listing the names of dimensions which must be the same for a child as for their parent (eg, if parent.child.concordant.dimensions='race', then the model will force new births into the model to have the same race as their parent)
#'@param all.births.into.compartments A named list, each element of which is a single character or integeter value. The names of the list represent dimensions, and the values are the compartments for those dimensions into which new births must fall
#'
#'@param fix.strata.sizes.prior.to A numeric time, prior to which strata sizes across dimensions specified by fix.strata.sizes.for.dimensions are held constant
#'@param fix.strata.sizes.for.dimensions A character vector of dimension names across which strata sizes should be held constant prior to fix.strata.sizes.prior.to. Must be a subset of names(compartments.for.infected.and.uninfected)
#'
#'@export
create.jheem.specification <- function(version,
                                       iteration,
                                       description,
                                       
                                       parent.version = NULL,
                                       do.not.inherit.model.quantity.names = character(),
                                       do.not.inherit.transitions.for.dimension = character(),
                                       
                                       compartments.for.infected.only,
                                       compartments.for.uninfected.only,
                                       compartments.for.infected.and.uninfected,
                                       
                                       transmission.modes,
                                       age.endpoints,
                                       
                                       compartment.value.aliases,
                                       
                                       enable.perinatal.transmission,
                                       parent.child.concordant.dimensions,
                                       all.births.into.compartments,
                                       
                                       fix.strata.sizes.prior.to=NULL,
                                       fix.strata.sizes.for.dimensions=NULL)
{
    error.prefix = "Cannot create jheem.specification: "
    
    ##-- ALLOWED MISSING OR NULL --##
    if (missing(do.not.inherit.model.quantity.names) ||
        is.null(do.not.inherit.model.quantity.names))
        do.not.inherit.model.quantity.names = character()
    
    if (missing(do.not.inherit.transitions.for.dimension) ||
        is.null(do.not.inherit.transitions.for.dimension))
        do.not.inherit.transitions.for.dimension = character()
    
    if (missing(compartment.value.aliases) || is.null(compartment.value.aliases))
        compartment.value.aliases = list()
    
    if (is.null(compartments.for.infected.only))
        compartments.for.infected.only = list()
    
    if (is.null(compartments.for.uninfected.only))
        compartments.for.uninfected.only = list()
    
    if (is.null(compartments.for.infected.and.uninfected))
        compartments.for.infected.and.uninfected = list()
    
    if (missing(age.endpoints))
        age.endpoints = NULL
    
    if (missing(enable.perinatal.transmission))
        enable.perinatal.transmission = NULL
    
    if (missing(parent.child.concordant.dimensions))
        parent.child.concordant.dimensions = NULL
    
    if (missing(all.births.into.compartments))
        all.births.into.compartments = NULL
    
    ##-- CHECK ARGUMENTS --##

    #-- Version --#    
    if (!is.character(version) || length(version)!=1 || is.na(version))
        stop(paste0(error.prefix, "'version' must be a single, non-NA character value"))
    if (nchar(version)<MIN.SPECIFICATION.VERSION.NCHAR || nchar(version)>MAX.SPECIFICATION.VERSION.NCHAR)
        stop(paste0(error.prefix,
                    "'version' must have between ", MIN.SPECIFICATION.VERSION.NCHAR,
                    " and ", MAX.SPECIFICATION.VERSION.NCHAR, " letters. ('",
                    version, "' has ", nchar(version), ")"))
    if (string.contains.invalid.characters(version, valid.characters = NUMBERS.LETTERS.DASH.PERIOD))
    {
        invalid = setdiff(strsplit(version, '')[[1]], strsplit(NUMBERS.LETTERS.DASH.PERIOD, '')[[1]])
        stop(paste0(error.prefix,
                    "Invalid ",
                    ifelse(length(invalid)==1, "character", "characters"),
                    "(", paste0(invalid, collapse=', '), 
                    ") in version '", version, 
                    "' - can only contain numbers, letters, periods, and dashes"))
    }
    
    # make a more informative error prefix with the version
    error.prefix = paste0("Cannot create jheem.specification '", version, "': ")
    
    
    #-- Iteration --#
    if ((!is.character(iteration) && !is.numeric(iteration)) || length(iteration)!=1 || is.na(iteration))
        stop(paste0(error.prefix, "'iteration' must be a single, non-NA character or numeric value"))
    
    if (is.numeric(iteration))
        iteration = as.character(iteration)
    
    if (nchar(iteration)<MIN.SPECIFICATION.ITERATION.NCHAR || nchar(iteration)>MIN.SPECIFICATION.ITERATION.NCHAR)
        stop(paste0(error.prefix,
                    "'iteration' must have between ", MIN.SPECIFICATION.ITERATION.NCHAR,
                    " and ", MIN.SPECIFICATION.ITERATION.NCHAR, " letters. ('",
                    iteration, "' has ", nchar(iteration), ")"))
    if (string.contains.invalid.characters(iteration, valid.characters = NUMBERS.LETTERS.DASH.PERIOD))
    {
        invalid = setdiff(strsplit(iteration, '')[[1]], strsplit(NUMBERS.LETTERS.DASH.PERIOD, '')[[1]])
        stop(paste0(error.prefix,
                    "Invalid ",
                    ifelse(length(invalid)==1, "character", "characters"),
                    "(", paste0(invalid, collapse=', '), 
                    ") in iteration '", iteration, 
                    "' - can only contain numbers, letters, periods, and dashes"))
    }
    
    #-- Description --#
    if (!is.character(description) || length(description)!=1 || is.na(description))
        stop(paste0(error.prefix, "'description' must be a single, non-NA character value"))
    if (nchar(description)<MIN.SPECIFICATION.DESCRIPTION.NCHAR || nchar(version)>MAX.SPECIFICATION.DESCRIPTION.NCHAR)
        stop(paste0(error.prefix,
                    "'description' must have between ", MIN.SPECIFICATION.DESCRIPTION.NCHAR,
                    " and ", MAX.SPECIFICATION.DESCRIPTION.NCHAR, " letters. ('",
                    description, "' has ", nchar(description), ")"))
    
    #-- Parent Version --#
    
    if (missing(parent.version) || is.null(parent.version))
        parent.specification = NULL
    else
    {
        if (!is.character(parent.version) || length(parent.version)!=1 || is.na(parent.version))
            stop(paste0(error.prefix, "If 'parent.version' is not NULL, it must be a single, non-NA character value"))
        
        if (!is.specification.registered.for.version(parent.version))
            stop(paste0(error.prefix,
                        "Invalid 'parent.version' - No JHEEM specification for version '", 
                        version, "' has been registered"))
        
        parent.specification = get.specification.for.version(parent.version)
    }    
    
    #--  Do not inherit <x> --#
    
    if (is.null(parent.specification))
    {
        if (length(do.not.inherit.model.quantity.names)>0)
            stop("'do.not.inherit.model.quantity.names' can only be set if 'parent.version' is specified")
        
        if (length(do.not.inherit.transitions.for.dimension)>0)
            stop("'do.not.inherit.transitions.for.dimension' can only be set if 'parent.version' is specified")
    }
    else
    {
        if (!is.character(do.not.inherit.model.quantity.names))
            stop(paste0(error.prefix,
                        "'do.not.inherit.model.quantity.names' must be a character vector"))
        
        if (!is.character(do.not.inherit.transitions.for.dimension))
            stop(paste0(error.prefix,
                        "'do.not.inherit.transitions.for.dimension' must be a character vector"))
    }

    #-- Age Cutoffs --#
        
    if (is.null(age.endpoints))
    {
        if (is.null(parent.specification))
            age.info = NULL
        else
            age.info = parent.specification$age.info
    }
    else
    {
        # Validate argument
        if (!is.numeric(age.endpoints))
            stop(paste0(error.prefix, "If specified, 'age.endpoints' must be a numeric vector"))
        if (length(age.endpoints)<2)
            stop(paste0(error.prefix, "If specified, 'age.endpoints' must contain at least two values"))
        if (any(is.na(age.endpoints)))
            stop(paste0(error.prefix, "'age.endpoints' cannot contain NA values"))
        if (any(age.endpoints<0))
            stop(paste0(error.prefix, "Values of 'age.endpoints' cannot be less than zero"))
        tabled.age.endpoints = table(age.endpoints)
        if (any(tabled.age.endpoints>1))
            stop(paste0(error.prefix,
                        "'age.endpoints' cannot contain repeated values. ",
                        collapse.with.and("'", names(tabled.age.endpoints)[tabled.age.endpoints>1], 
                                                  "' is repeated ", 
                                                  tabled.age.endpoints[tabled.age.endpoints>1],
                                                  " times")))
        if (!all(age.endpoints==sort(age.endpoints)))
            stop(paste0(error.prefix,
                        "The values of 'age.endpoints' must be arranged in ascending order"))
        
        # Process the argument
        
        age.info = list(
            n.ages = length(age.endpoints)-1,
            endpoints = age.endpoints,
            lowers = age.endpoints[-length(age.endpoints)],
            uppers = age.endpoints[-1]
        )
        
        age.info$spans = age.info$uppers - age.info$lowers
        age.info$ages = make.age.strata.names(age.endpoints)
    }
    
    #-- Compartment Value Aliases --#
    
    if (!is.list(compartment.value.aliases))
        stop(paste0(error.prefix,
                    "'compartment.value.aliases' must be a list"))
    
    if (is.null(names(compartment.value.aliases)))
        stop(paste0(error.prefix,
                    "'compartment.value.aliases' must be a NAMED list"))
    
    if (any(is.na(names(compartment.value.aliases))))
        stop(paste0(error.prefix,
                    "'compartment.value.aliases' cannot have NA names"))
    
    tabled.aliases = table(names(compartment.value.aliases))
    if (any(tabled.aliases>1))
        stop(paste0(error.prefix,
                    "The names of 'compartment.value.aliases' must be unique. ",
                    collapse.with.and("'", names(tabled.aliases)[tabled.aliases>1], "'"),
                    ifelse(sum(tabled.aliases>1)==1, " is", " are"),
                    " used more than once"))
    
    is.alias.function = sapply(names(compartment.value.aliases), function(alias){
        if (is.function(compartment.value.aliases[[alias]]))
        {
            arg.names = get.function.argument.names(fn=compartment.value.aliases[[alias]],
                                                    exclude.arguments.with.default.values=T)
            
            invalid.arg.names = setdiff(arg.names, c('location'))
            if (length(invalid.arg.names)>0)
                stop(paste0(error.prefix,
                            "Elements of 'compartment.value.aliases' that are functions can only take one argument: 'location'. The function for '",
                            alias, "' requires ",
                            ifelse(length(invalid.arg.names)==1, "argument ", 'arguments '),
                            collapse.with.and("'", invalid.arg.names, "'")))
            
            T
            
        }
        else if (is.character(compartment.value.aliases[[alias]]))
        {
            F
        }
        else
            stop(paste0(error.prefix,
                        "The elements of 'compartment.value.aliases' must be either character vectors or functions. compartment.value.aliases[['",
                        alias, "']] is neither"))
    })
    
    compartment.value.character.aliases = compartment.value.aliases[!is.alias.function]
    compartment.value.function.aliases = compartment.value.aliases[is.alias.function]
    
    # This makes sure it is a named list of non-empty, non-NA character vectors,
    #  with no duplicates in any entry
    check.dim.names.valid(compartment.value.character.aliases,
                          variable.name.for.error = 'compartment.value.aliases',
                          refer.to.dimensions.as = 'entry',
                          refer.to.dimension.plural.as = 'entries',
                          allow.duplicate.values.across.dimensions = T,
                          error.prefix = error.prefix)

    # Check that the actual values themselves are valid
    validate.compartment.value.aliases(names(compartment.value.aliases),
                                       descriptor = 'name',
                                       variable.name.for.error = 'compartment.value.aliases',
                                       error.prefix = error.prefix)

    # replaces age value with ages
    if (!is.null(age.endpoints))
    {
        compartment.value.character.aliases[['all.ages']] = age.info$ages
    }
    
    # Pull from parent
    if (!is.null(parent.specification))
    {
        to.add.compartment.value.character.aliases = compartment.value.character.aliases
        compartment.value.character.aliases = parent.specification$compartment.value.character.aliases
        compartment.value.character.aliases[name(to.add.compartment.value.character.aliases)] = to.add.compartment.value.character.aliases
        
        to.add.compartment.value.function.aliases = compartment.value.function.aliases
        compartment.value.function.aliases = parent.specification$compartment.value.function.aliases
        compartment.value.function.aliases[name(to.add.compartment.value.function.aliases)] = to.add.compartment.value.function.aliases
    }
    
    #-- Dim Names --#
    # In general, first we're going to check what's given
    # Then we're going to pull from parent and recheck
    # Then we're going to sub in aliases and check a third time
    
    # dim.names - age is allowed to be NULL if we have age.endpoints set
    if (!is.null(age.endpoints))
    {
        if (any(names(compartments.for.infected.and.uninfected)=='age') && 
            length(compartments.for.infected.and.uninfected[['age']])==0)
            compartments.for.infected.and.uninfected[['age']] = 'all.ages'
        if (any(names(compartments.for.infected.only)=='age') && 
            length(compartments.for.infected.only[['age']])==0)
            compartments.for.infected.only[['age']] = 'all.ages'
        if (any(names(compartments.for.uninfected.only)=='age') && 
            length(compartments.for.uninfected.only[['age']])==0)
            compartments.for.uninfected.only[['age']] = 'all.ages'
    }
    
    
    # dim.names - basic validity of passed arguments
    check.dim.names.valid(compartments.for.infected.only, error.prefix = error.prefix,
                          variable.name.for.error = 'compartments.for.infected.only',
                          allow.empty = T, allow.duplicate.values.across.dimensions = F)
    check.dim.names.valid(compartments.for.uninfected.only, error.prefix = error.prefix,
                          variable.name.for.error = 'compartments.for.uninfected.only',
                          allow.empty = T, allow.duplicate.values.across.dimensions = F)
    check.dim.names.valid(compartments.for.infected.and.uninfected, error.prefix = error.prefix,
                          variable.name.for.error = 'compartments.for.infected.and.uninfected',
                          allow.empty = T, allow.duplicate.values.across.dimensions = F)
    
    
    # dim.names - check for use of reserved dimensions
    validate.dimensions(names(compartments.for.infected.only),
                        variable.name.for.error = "'compartments.for.infected.only'",
                        error.prefix = error.prefix)
    validate.dimensions(names(compartments.for.uninfected.only),
                        variable.name.for.error = "'compartments.for.uninfected.only'",
                        error.prefix = error.prefix)
    validate.dimensions(names(compartments.for.infected.and.uninfected),
                        variable.name.for.error = "'compartments.for.infected.and.uninfected'",
                        error.prefix = error.prefix)
    
    # dim.names - check for use of reserved categories
    sapply(names(compartments.for.infected.only), function(d){
        validate.compartment.values(values = compartments.for.infected.only[[d]],
                                    variable.name.for.error = paste0("compartments.for.infected.only[['", d, "']]"),
                                    error.prefix = error.prefix)
    })
    
    sapply(names(compartments.for.uninfected.only), function(d){
        validate.compartment.values(values = compartments.for.uninfected.only[[d]],
                                    variable.name.for.error = paste0("compartments.for.uninfected.only[['", d, "']]"),
                                    error.prefix = error.prefix)
    })
    
    sapply(names(compartments.for.infected.and.uninfected), function(d){
        validate.compartment.values(values = compartments.for.infected.and.uninfected[[d]],
                                    variable.name.for.error = paste0("compartments.for.infected.and.uninfected[['", d, "']]"),
                                    error.prefix = error.prefix)
    })
    
    
    # make sure that dimensions are not shared across any of the three of general/infected/uninfected
    shared.dimensions = intersect(names(compartments.for.infected.only), names(compartments.for.uninfected.only))
    if (length(shared.dimensions)>0)
        stop(paste0(error.prefix,
                    "Dimensions cannot be shared between 'compartments.for.infected.only' and 'compartments.for.uninfected.only', but ",
                    collapse.with.and("'", shared.dimensions, "'"),
                    ifelse(length(shared.dimensions)==1, " is", " are"),
                    " used by both"))
    
    shared.dimensions = intersect(names(compartments.for.infected.only), names(compartments.for.infected.and.uninfected))
    if (length(shared.dimensions)>0)
        stop(paste0(error.prefix,
                    "Dimensions cannot be shared between 'compartments.for.infected.only' and 'compartments.for.infected.and.uninfected', but ",
                    collapse.with.and("'", shared.dimensions, "'"),
                    ifelse(length(shared.dimensions)==1, " is", " are"),
                    " used by both"))
    
    shared.dimensions = intersect(names(compartments.for.uninfected.only), names(compartments.for.infected.and.uninfected))
    if (length(shared.dimensions)>0)
        stop(paste0(error.prefix,
                    "Dimensions cannot be shared between 'compartments.for.uninfected.only' and 'compartments.for.infected.and.uninfected', but ",
                    collapse.with.and("'", shared.dimensions, "'"),
                    ifelse(length(shared.dimensions)==1, " is", " are"),
                    " used by both"))
    
    
    # make sure that values are not shared across general+infected or general+uninfected
    check.dim.names.valid(c(compartments.for.infected.and.uninfected, compartments.for.infected.only), 
                          variable.name.for.error = 'general + infected compartments',
                          error.prefix = error.prefix,
                          allow.empty = T, allow.duplicate.values.across.dimensions = F)
    check.dim.names.valid(c(compartments.for.infected.and.uninfected, compartments.for.uninfected.only), 
                          variable.name.for.error = 'general + uninfected compartments',
                          error.prefix = error.prefix,
                          allow.empty = T, allow.duplicate.values.across.dimensions = F)

    
    # dim.names - pull from parent
    if (!is.null(parent.specification))
    {
        # Pull down the dim names
        to.add.compartments.for.infected.only = compartments.for.infected.only
        compartments.for.infected.only = parent.specification$compartments$infected
        compartments.for.infected.only[names(to.add.compartments.for.infected.only)] = to.add.compartments.for.infected.only
        
        to.add.compartments.for.uninfected.only = compartments.for.uninfected.only
        compartments.for.uninfected.only = parent.specification$compartments$uninfected
        compartments.for.uninfected.only[names(to.add.compartments.for.uninfected.only)] = to.add.compartments.for.uninfected.only
        
        to.add.compartments.for.infected.and.uninfected = compartments.for.infected.and.uninfected
        compartments.for.infected.and.uninfected = parent.specification$compartments$general
        compartments.for.infected.and.uninfected[names(to.add.compartments.for.infected.and.uninfected)] = to.add.compartments.for.infected.and.uninfected
        
        # dim.names - basic validity of merged with parent arguments
        check.dim.names.valid(compartments.for.infected.only, error.prefix = error.prefix,
                              variable.name.for.error = 'compartments.for.infected.only (after merging with parent specification)',
                              allow.empty = T, allow.duplicate.values.across.dimensions = F)
        check.dim.names.valid(compartments.for.uninfected.only, error.prefix = error.prefix,
                              variable.name.for.error = 'compartments.for.uninfected.only (after merging with parent specification)',
                              allow.empty = T, allow.duplicate.values.across.dimensions = F)
        check.dim.names.valid(compartments.for.infected.and.uninfected, error.prefix = error.prefix,
                              variable.name.for.error = 'compartments.for.infected.and.uninfected (after merging with parent specification)',
                              allow.empty = T, allow.duplicate.values.across.dimensions = F)
        
        # make sure that we have location as a dimension in the general dimnames
        if (all(names(compartments.for.infected.and.uninfected) != 'location'))
            stop(paste0(error.prefix, "'compartments.for.infected.and.uninfected' MUST have a dimension titled 'location'"))
        
        # make sure that dimensions are not shared across any of the three of general/infected/uninfected
        #  (AFTER pulling from parent)
        shared.dimensions = intersect(names(compartments.for.infected.only), names(compartments.for.uninfected.only))
        if (length(shared.dimensions)>0)
            stop(paste0(error.prefix,
                        "Dimensions cannot be shared between 'compartments.for.infected.only' and 'compartments.for.uninfected.only', but ",
                        collapse.with.and("'", shared.dimensions, "'"),
                        ifelse(length(shared.dimensions)==1, " is", " are, after mergning with parent specification"),
                        " used by both"))
        
        shared.dimensions = intersect(names(compartments.for.infected.only), names(compartments.for.infected.and.uninfected))
        if (length(shared.dimensions)>0)
            stop(paste0(error.prefix,
                        "Dimensions cannot be shared between 'compartments.for.infected.only' and 'compartments.for.infected.and.uninfected', but ",
                        collapse.with.and("'", shared.dimensions, "'"),
                        ifelse(length(shared.dimensions)==1, " is", " are, after mergning with parent specification"),
                        " used by both"))
        
        shared.dimensions = intersect(names(compartments.for.uninfected.only), names(compartments.for.infected.and.uninfected))
        if (length(shared.dimensions)>0)
            stop(paste0(error.prefix,
                        "Dimensions cannot be shared between 'compartments.for.uninfected.only' and 'compartments.for.infected.and.uninfected', but ",
                        collapse.with.and("'", shared.dimensions, "'"),
                        ifelse(length(shared.dimensions)==1, " is", " are, after mergning with parent specification"),
                        " used by both"))
        
        
        # make sure that values are not shared across general+infected or general+uninfected
        check.dim.names.valid(c(compartments.for.infected.and.uninfected, compartments.for.infected.only), 
                              variable.name.for.error = 'general + infected compartments',
                              error.prefix = error.prefix,
                              allow.empty = T, allow.duplicate.values.across.dimensions = F)
        check.dim.names.valid(c(compartments.for.infected.and.uninfected, compartments.for.uninfected.only), 
                              variable.name.for.error = 'general + uninfected compartments',
                              error.prefix = error.prefix,
                              allow.empty = T, allow.duplicate.values.across.dimensions = F)
    }
    
    
    # dim.names - sub in ages, if appropriate
    # - if it was not specified, but age cutoffs was specified, add it to the start of compartments.for.infected.and.uninfected
    if (!is.null(age.endpoints))
    {
        all.dimensions = c(names(compartments.for.infected.and.uninfected),
                           names(compartments.for.infected.only),
                           names(compartments.for.uninfected.only))
        if (!any(all.dimensions=='age'))
            compartments.for.infected.and.uninfected = c(list(age='all.ages'),
                                  compartments.for.infected.and.uninfected)
    }
    
    # dim.names - make sure that we contain location
    if (all(names(compartments.for.infected.and.uninfected)!='location'))
        stop(paste0(error.prefix, "'compartments.for.infected.and.uninfected' must include a 'location' dimension"))
    
    # dim.names - make sure that not all empty for infected or uninfected
    #if (length(compartments.for.infected.and.uninfected)==0)
    #{
    #    if (length(compartments.for.infected.only)==0)
    #        stop(paste0(error.prefix,
    #                    "'compartments.for.infected.and.uninfected' and 'compartments.for.infected.only' cannot both be empty"))
    #    if (length(compartments.for.uninfected.only)==0)
    #        stop(paste0(error.prefix,
    #                    "'compartments.for.infected.and.uninfected' and 'compartments.for.uninfected.only' cannot both be empty"))
    #}
    
    # dim.names - make sure that dimensions are not shared across general+infected or general+uninfected
    overlapping.dimensions = intersect(names(compartments.for.infected.and.uninfected), names(compartments.for.uninfected.only))
    if (length(overlapping.dimensions)>0)
        stop(paste0(error.prefix,
                    " 'compartments.for.infected.and.uninfected' and 'compartments.for.uninfected.only, cannot share dimensions, but both contain ",
                    collapse.with.and("'", overlapping.dimensions, "'")))
    
    overlapping.dimensions = intersect(names(compartments.for.infected.and.uninfected), names(compartments.for.infected.only))
    if (length(overlapping.dimensions)>0)
        stop(paste0(error.prefix,
                    " 'compartments.for.infected.and.uninfected' and 'compartments.for.infected.only, cannot share dimensions, but both contain ",
                    collapse.with.and("'", overlapping.dimensions, "'")))
    
    
    # dim.names - substitute in aliases
    aliased.compartments.for.infected.only = lapply(compartments.for.infected.only, substitute.aliases.into.vector, aliases=compartment.value.aliases)
    aliased.compartments.for.uninfected.only = lapply(compartments.for.uninfected.only, substitute.aliases.into.vector, aliases=compartment.value.aliases)
    aliased.compartments.for.infected.and.uninfected = lapply(compartments.for.infected.and.uninfected, substitute.aliases.into.vector, aliases=compartment.value.aliases)
    
    # dim.names - basic validity of aliased arguments
    check.dim.names.valid(aliased.compartments.for.infected.only, error.prefix = error.prefix,
                          variable.name.for.error = 'compartments.for.infected.only (after plugging in compartment.value.aliases)',
                          allow.empty = T, allow.duplicate.values.across.dimensions = F)
    check.dim.names.valid(aliased.compartments.for.uninfected.only, error.prefix = error.prefix,
                          variable.name.for.error = 'compartments.for.uninfected.only (after plugging in compartment.value.aliases)',
                          allow.empty = T, allow.duplicate.values.across.dimensions = F)
    check.dim.names.valid(aliased.compartments.for.infected.and.uninfected, error.prefix = error.prefix,
                          variable.name.for.error = 'compartments.for.infected.and.uninfected (after plugging in compartment.value.aliases)',
                          allow.empty = T, allow.duplicate.values.across.dimensions = F)
    
    # make sure that values are not shared across general+infected or general+uninfected
    check.dim.names.valid(c(aliased.compartments.for.infected.and.uninfected, aliased.compartments.for.infected.only), 
                          variable.name.for.error = 'general + infected compartments (after plugging in compartment.value.aliases)',
                          error.prefix = error.prefix,
                          allow.empty = T, allow.duplicate.values.across.dimensions = F)
    check.dim.names.valid(c(aliased.compartments.for.infected.and.uninfected, aliased.compartments.for.uninfected.only), 
                          variable.name.for.error = 'general + uninfected compartments (after plugging in compartment.value.aliases)',
                          error.prefix = error.prefix,
                          allow.empty = T, allow.duplicate.values.across.dimensions = F)

    
    
    #-- Transmission Modes --#
    if (missing(transmission.modes) || is.null(transmission.modes))
    {
        if (is.null(parent.version))
            stop(paste0(error.prefix,
                        "'transmission.modes' must be specified when no parent specification is provided"))
        
        transmission.modes = parent.specification$transmission.modes
    }
    else
    {
        if (!is.character(transmission.modes))
            stop(paste0(error.prefix, "'transmission.modes' must be a character vector"))
        if (length(transmission.modes)==0)
            stop(paste0(error.prefix, "'transmission.modes' must have at least one element"))
        if (any(is.na(transmission.modes)))
            stop(paste0(error.prefix, "'transmission.modes' cannot contain NA values"))
        if (any(nchar(transmission.modes)==0))
            stop(paste0(error.prefix, "'transmission.modes' cannot contain the empty string ('')"))
        tabled.transmission.modes = table(transmission.modes)
        if (any(tabled.transmission.modes>1))
            stop(paste0(error.prefix,
                        "'transmission.modes' cannot contain repeated values. ",
                        collapse.with.and("'", names(tabled.transmission.modes)[tabled.transmission.modes>1], 
                                                  "' is repeated ", 
                                                  tabled.transmission.modes[tabled.transmission.modes>1],
                                                  " times")))
        
        contains.invalid.characters.mask = sapply(transmission.modes, function(val){
            string.contains.invalid.characters(val, valid.characters = NUMBERS.LETTERS.DASH.PERIOD.UNDERSCORE)
        })
        if (any(contains.invalid.characters.mask))
        {
            invalid.values = transmission.modes[contains.invalid.characters.mask]
            invalid.characters = setdiff(unlist(strsplit(values, split='')),
                                         strsplit(NUMBERS.LETTERS.DASH.PERIOD.UNDERSCORE, split='')[[1]])
            
           stop(paste0(error.prefix,
                       "Invalid ",
                       ifelse(length(invalid.values)==1, "value", "values"),
                       " in 'transmission.mode: ",
                       collapse.with.and("'", invalid.values, "'"),
                       ". Transmission modes cannot contain ",
                       collapse.with.or("'", invalid.characters, "'"),
                       " - only numbers, letters, periods, dashes, and underscores"))
        }
        
        if (!is.null(parent.specification))
            transmission.modes = union(parent.specification$transmission.modes, transmission.modes)
    }
    
    # fix.strata.sizes.prior.to, fix.strata.sizes.for.dimensions
    if (missing(fix.strata.sizes.prior.to) || is.null(fix.strata.sizes.prior.to))
    {
        if (is.null(parent.specification))
            stop(paste0(error.prefix,
                        "'fix.strata.sizes.prior.to' must be specified when no parent specification is provided"))
        
        fix.strata.sizes.prior.to = parent.specification$fix.strata.sizes.prior.to
    }
    else
    {
        if (!is.numeric(fix.strata.sizes.prior.to))
            stop(paste0(error.prefix, "'fix.strata.sizes.prior.to' must be a single, numeric value"))
        if (length(fix.strata.sizes.prior.to)!=1)
            stop(paste0(error.prefix, "'fix.strata.sizes.prior.to' must be a SINGLE, numeric value"))
        if (is.na(fix.strata.sizes.prior.to))
            stop(paste0(error.prefix, "'fix.strata.sizes.prior.to' cannot be NA"))
    }    
    
    if (missing(fix.strata.sizes.for.dimensions) || is.null(fix.strata.sizes.for.dimensions))
    {
        if (is.null(parent.specification))
            stop(paste0(error.prefix,
                        "'fix.strata.sizes.for.dimensions' must be specified when no parent specification is provided"))
        
        fix.strata.sizes.for.dimensions = parent.specification$fix.strata.sizes.for.dimensions
    }
    else
    {
        if (!is.character(fix.strata.sizes.for.dimensions))
            stop(paste0(error.prefix, "'fix.strata.sizes.for.dimensions' must be a character vector"))
        if (any(is.na(fix.strata.sizes.for.dimensions)))
            stop(paste0(error.prefix, "'fix.strata.sizes.for.dimensions' cannot contain NA values"))
        
        tabled.fix.strata.sizes.for.dimensions = table(fix.strata.sizes.for.dimensions)
        if (any(tabled.fix.strata.sizes.for.dimensions>1))
            stop(paste0(error.prefix,
                        "'tabled.fix.strata.sizes.for.dimensions' cannot contain repeated values. ",
                        collapse.with.and("'", names(tabled.fix.strata.sizes.for.dimensions)[tabled.fix.strata.sizes.for.dimensions>1], 
                                          "' is repeated ", 
                                          tabled.fix.strata.sizes.for.dimensions[tabled.fix.strata.sizes.for.dimensions>1],
                                          " times")))
    }
    
    if (length(fix.strata.sizes.for.dimensions)==0 && fix.strata.sizes.prior.to > -Inf)
        stop(paste0(error.prefix, "'fix.strata.sizes.for.dimensions' must be a character vector if fix.strata.sizes.prior.to is >-Inf"))
    
    invalid.fixed.dimensions = setdiff(fix.strata.sizes.for.dimensions, names(compartments.for.infected.and.uninfected))
    if (length(invalid.fixed.dimensions)>0)
        stop(paste0(error.prefix,
                    "'fix.strata.sizes.for.dimensions' can only contain dimensions given in 'compartments.for.infected.and.uninfected'",
                    " (ie, 'fix.strata.sizes.for.dimensions' must be a subset of names(compartments.for.infected.and.uninfected) ).",
                    collapse.with.and("'", invalid.fixed.dimensions, "'"),
                    ifelse(length(invalid.fixed.dimensions)==1, ' is', ' are'),
                    " not dimensions in 'compartments.for.infected.and.uninfected' (which comprise ",
                    collapse.with.and("'", names(compartments.for.infected.and.uninfected), "'"), ")"))
    
    #-- perinatal transmission, parent.child.concordant.dimensions and all.births.into.compartments --#
    
    if (is.null(enable.perinatal.transmission))
    {
        if (is.null(parent.specification))
            stop(paste0(error.prefix, "'enable.perinatal.transmission' must be specified when there is no parent specification"))
        
        parent.child.concordant.dimensions = parent.specification$enable.perinatal.transmission
    }
    else
    {
        if (!is.logical(enable.perinatal.transmission))
            stop(paste0(error.prefix, "'enable.perinatal.transmission' must be a single LOGICAL value"))
        if (length(enable.perinatal.transmission)!=1)
            stop(paste0(error.prefix, "'enable.perinatal.transmission' must be a SINGLE, logical value"))
        if (is.na(enable.perinatal.transmission))
            stop(paste0(error.prefix, "'enable.perinatal.transmission' cannot be NA"))
    }
    
    if (is.null(parent.child.concordant.dimensions)) #pull from parent
    {
        if (is.null(parent.specification))
            stop(paste0(error.prefix, "'parent.child.concordant.dimensions' must be specified when there is no parent specification"))
        
        parent.child.concordant.dimensions = parent.specification$parent.child.concordant.dimensions
    }
    else
    {
        if (!is.character(parent.child.concordant.dimensions))
            stop(paste0(error.prefix, "'parent.child.concordant.dimensions' must be a character vector"))
        if (any(is.na(parent.child.concordant.dimensions)))
            stop(paste0(error.prefix, "'parent.child.concordant.dimensions' cannot contain NA values"))
        tabled.parent.child.concordant.dimensions = table(parent.child.concordant.dimensions)
        if (any(tabled.parent.child.concordant.dimensions>1))
            stop(paste0(error.prefix,
                        "'parent.child.concordant.dimensions' cannot contain repeat values. ",
                        collapse.with.and("'", names(tabled.parent.child.concordant.dimensions)[tabled.parent.child.concordant.dimensions>1], "'"),
                        ifelse(sum(tabled.parent.child.concordant.dimensions>1)==1, " is", " are"),
                        " included more than once"))
        
        invalid.dimensions = setdiff(parent.child.concordant.dimensions, all.dimensions)
        if (length(invalid.dimensions)>0)
            stop(paste0(error.prefix,
                        "Invalid ",
                        ifelse(length(invalid.dimensions)==1, "value", "values"),
                        " for 'parent.child.concordant.dimensions': ",
                        collapse.with.and("'", invalid.dimensions, "'"),
                        ". Values must be one of ",
                        collapse.with.or("'", all.dimensions, "'")))
            
    }
    
    if (is.null(all.births.into.compartments))
    {
        if (is.null(parent.specification))
            stop(paste0(error.prefix, "'all.births.into.compartments' must be specified when there is no parent specification"))
        
        all.births.into.compartments = parent.specification$all.births.into.compartments
    }
    else
    {
        check.dimension.values.valid(all.births.into.compartments,
                                     variable.name.for.error="all.births.into.compartments",
                                     allow.empty=T,
                                     allow.duplicate.values.within.dimensions = F,
                                     error.prefix = error.prefix)
        
        sapply(names(all.births.into.compartments), function(d){
            if (is.logical(all.births.into.compartments[[d]]))
            {
                if (sum(all.births.into.compartments[[d]])!=1)
                    stop(paste0(error.prefix,
                                "When values of 'all.births.into.compartments' are ",
                                "logical vectors, they must be have exactly one TRUE value. all.births.into.compartments[['",
                                d, "']] has ", sum(all.births.into.compartments[[d]]), 
                                " values that are TRUE"))
            }
            else
            {
                if (length(all.births.into.compartments[[d]])>1)
                    stop(paste0(error.prefix,
                                "When values of 'all.births.into.compartments' are ",
                                class(all.births.into.compartments)[1],
                                " vectors, they must contain a single value. all.births.into.compartments[['",
                                d, "']] contains ", length(all.births.into.compartments[[d]]), " values"))
            }
        })
        
        
        invalid.dimensions = setdiff(names(all.births.into.compartments), all.dimensions)
        if (length(invalid.dimensions)>0)
            stop(paste0(error.prefix,
                        "Invalid ",
                        ifelse(length(invalid.dimensions)==1, "name", "names"),
                        " for 'all.births.into.compartments': ",
                        collapse.with.and("'", invalid.dimensions, "'"),
                        ". names(all.births.into.compartments) must be a subset of ",
                        collapse.with.and("'", all.dimensions, "'")))
    }
    
    # Check for clashes between concordant and all births into dimensions
    overlapping.dimensions = intersect(parent.child.concordant.dimensions, names(all.births.into.compartments))
    if (length(overlapping.dimensions)>0)
        stop(paste0(error.prefix,
                    collapse.with.and("'", overlapping.dimensions, "'"),
                    ifelse(length(overlapping.dimensions)==1, " is", " are"),
                    " present in both 'parent.child.concordant.dimensions' and names(all.births.into.compartments). ",
                    "A dimension must EITHER be concordant for parents/children OR specified as all born into a compartment"))
    
    # all.births.into.compartments cannot contain function aliases
    invalid.mask = sapply(all.births.into.compartments, function(val){
        any(val == names(compartment.value.function.aliases))
    })
    if (any(invalid.mask))
        stop(paste0(error.prefix,
                    "'all.births.into.compartments' cannot contain values that are compartment.value.aliases given by functions. ",
                    ifelse(sum(invalid.mask)==1, "Dimension ", "Dimensions "),
                    collapse.with.and("'", names(all.births.into.compartments)[invalid.mask], "'"),
                    ifelse(sum(invalid.mask)==1, "contains an invalid value: ", "contain invalid values: "),
                    collapse.with.and("'", sapply(all.births.into.compartments[invalid.mask], function(val){val}), "'"),
                    ifelse(sum(invalid.mask)==1, "", " respectively."),
                    ))
    
    #-- Call the constructor --#
    JHEEM.SPECIFICATION$new(
        
        version = version,
        iteration = iteration,
        description = description,
        
        parent.specification = parent.specification,
        do.not.inherit.model.quantity.names = do.not.inherit.model.quantity.names,
        do.not.inherit.transitions.for.dimension = do.not.inherit.transitions.for.dimension,
        
        compartments.for.infected.only = compartments.for.infected.only,
        compartments.for.uninfected.only = compartments.for.uninfected.only,
        compartments.for.infected.and.uninfected = compartments.for.infected.and.uninfected,
        transmission.modes = transmission.modes,
        age.info = age.info,
        
        compartment.value.character.aliases = compartment.value.character.aliases,
        compartment.value.function.aliases = compartment.value.function.aliases,
        
        enable.perinatal.transmission = enable.perinatal.transmission,
        parent.child.concordant.dimensions = parent.child.concordant.dimensions,
        all.births.into.compartments = all.births.into.compartments,
        
        fix.strata.sizes.prior.to = fix.strata.sizes.prior.to,
        fix.strata.sizes.for.dimensions = fix.strata.sizes.for.dimensions,
        
        model.quantities = list(),
        top.level.references = list()
    )
}


#'@description Register a Model Element
#'
#'@param specification The jheem.specification object
#'
#'@param name The name of the model element. Cannot overlap with the names of model quantities
#'@param scale The scale for this model.element. Can be 'rate', 'ratio', 'proportion', 'time', 'number', 'non.negative.number'
#'
#'@param dimensions Optional parameter to specify which dimensions the model.element is expected to take
#'@param dimension.values Optional parameter to specify which dimension.values the model.element will use in its dimension names
#'@param apply.aliases.to.dimension.values A logical indicating whether, if dimension.values are supplied, they should have the compartment.value.aliases for the model specification applied to them
#'
#'@param value A numeric value that serves as the value for this element if it is not otherwise specified. If NULL, then a value must be specified either by a get.value.function (a function which returns a value), or by a functional.form
#'@param get.value.function An alternative to passing value directly, if the value needs to be different by location. This should be a function that takes arguments location, specification, and ... 
#'
#'@param functional.form An object of class 'functional.form' that produces the background values for this element. Pass NULL if no functional.form is to be used, or if it is to be obtained by get.functional.form.function
#'@param get.functional.form.function An alternative to passing background functional.form directly, if the background functional.form needs to be different by location. This should be a function that takes arguments location, specification, and ... 
#'@param functional.form.scale The scale of values produced by the background functional.form
#'@param functional.form.from.time,functional.form.to.time The time frame over which the functional.form applies
#'
#'@param get.value.function.name,get.functional.form.function.name Optional - the actual names of the functions passed to 'get.value.function' or 'get.functional.form.function' - used to make error messages more informative
#'
#'@param ... Arguments to be passed to either get.functional.form.function or get.value.function
#'
#'@param ramp.scale,taper.scale If the ramp or taper operates on a different scale than the element evaluates to, specified here
#'@param ramp.times,taper.times The (default) times at which to ramp up to taper down
#'@param ramp.values,taper.values The (default) multipliers for ramp or taper times
#'@param ramp.value.application,taper.value.application How to interpret ramp and taper values. 'multiplier' multiplies the ramp by the first functional.form-generated value (and taper by the last functional.form-generated value). 'absolute' just plugs in the value directly
#'@param ramp.interpolate.links,taper.interpolate.links The name(s) of link function (either 'identity', 'log', or 'exp') denoting the scale at which to interpolate values for the ramp or taper. Can be either a single value (applied to all elements in the ramp), or a vector of length(ramp.times) or length(taper.times)
#'
#'@export
register.model.element <- function(specification,
                                   
                                   name,
                                   scale,
                                   
                                   dimensions=NULL,
                                   dimension.values=NULL,
                                   apply.aliases.to.dimension.values=F,
                                   
                                   value=NULL,
                                   get.value.function=NULL,
                                   
                                   functional.form=NULL,
                                   get.functional.form.function=NULL,
                                   functional.form.scale=scale,
                                   functional.form.from.time=NULL,
                                   functional.form.to.time=Inf,
                                   
                                   get.functional.form.function.name = NULL,
                                   get.value.function.name = NULL,
                                   ...,
                                   
                                   ramp.scale=scale,
                                   ramp.times=numeric(),
                                   ramp.values=numeric(),
                                   ramp.value.application=c('multiplier','absolute')[1],
                                   ramp.interpolate.links='identity',
                                   
                                   taper.scale=scale,
                                   taper.times=numeric(),
                                   taper.values=numeric(),
                                   taper.value.application=c('multiplier','absolute')[1],
                                   taper.interpolate.links='identity')
{
    if (!is(specification, 'jheem.specification') || !R6::is.R6(specification))
        stop("'specification' must be an R6 object with class 'jheem.specification")

    # Plug in default values for the function names
    if (is.null(get.value.function.name) && !is.null(get.value.function))
    {
        get.value.function.name = deparse(substitute(get.value.function))
        if (!is.character(get.value.function.name) || length(get.value.function.name) != 1)
            get.value.function.name = 'get.value.function'
    }
    if (is.null(get.functional.form.function.name) && !is.null(get.functional.form.function))
    {
        get.functional.form.function.name = deparse(substitute(get.functional.form.function))
        if (!is.character(get.functional.form.function.name) || length(get.functional.form.function.name) != 1)
            get.functional.form.function.name = 'get.functional.form.function'
    }
    
    specification$register.element(
        name=name,
        scale=scale,
        
        dimensions=dimensions,
        dimension.values=dimension.values,
        apply.aliases.to.dimension.values = apply.aliases.to.dimension.values,
        
        value=value,
        get.value.function=get.value.function,
        
        functional.form=functional.form,
        get.functional.form.function=get.functional.form.function,
        functional.form.scale=functional.form.scale,
        functional.form.from.time=functional.form.from.time,
        functional.form.to.time=functional.form.to.time,
        
        get.functional.form.function.name = get.functional.form.function.name,
        get.value.function.name = get.value.function.name,
        ...,
        
        ramp.scale=ramp.scale,
        ramp.times=ramp.times,
        ramp.values=ramp.values,
        ramp.value.application=ramp.value.application,
        ramp.interpolate.links=ramp.interpolate.links,
        
        taper.scale=taper.scale,
        taper.times=taper.times,
        taper.values=taper.values,
        taper.value.application=taper.value.application,
        taper.interpolate.links=taper.interpolate.links
    )
}

#'@description A convenience function to streamline registering multiple model.elements for which we only need to specify a value
#'
#'@param specification The jheem.specification object to modify
#'@param ... A set of named values. The names correspond to the names of the model elements, and the values should be either (1) numeric objects or (2) functions that take arguments 'location' and 'settings' and returns a numeric value
#'@param scale The scale at which the elements apply
#'
#'@export
register.model.element.values <- function(specification,
                                          ...,
                                          scale)
{
    args = list(...)
    if (length(args)==0)
        stop("There must be at lease one value passed to ... for register.model.element.values")
    
    if (is.null(names(args)) || any(names(args)==''))
        stop("The ... values passed to register.model.element.values must all have names")
    
    invalid.type.mask = !sapply(args, is.numeric) & !sapply(args, is.function)
    if (any(invalid.type.mask))
        stop("The ... values passed to register.model.element.values must all be either numeric objects or functions. The value",
             ifelse(sum(invalid.type.mask)==1, '', 's'),
             " for ",
             collapse.with.and("'", names(args)[invalid.type.mask], "'"), " ",
             ifelse(sum(invalid.type.mask)==1, 'was', 'were'),
             " not.")
    
    for (i in 1:length(args))
    {
        if (is.numeric(args[[i]]))
            register.model.element(specification,
                                   name = names(args)[i],
                                   value = args[[i]],
                                   scale = scale)
        else
        {
            get.value.function.name = deparse(substitute(get.value.function))
            if (!is.character(get.value.function.name) || length(get.value.function.name) != 1)
                get.value.function.name = 'get.value.function'
            
            register.model.element(specification,
                                   name = names(args)[i],
                                   get.value.function = args[[i]],
                                   get.value.function.name = get.value.function.name,
                                   scale = scale)
        }
    }
    
    specification
}

#'@description Register a Model Quantity
#'
#'@inheritParams register.model.element
#'@param specification The jheem.specification object to modify
#'@param name The name of the model quantity. Cannot overlap with names of model elements
#'@param value Either: (1) a numeric object, (2) a character string giving the name of another model quantity or a model element, (3) an 'call' or 'expression' object (generated by expression() or expr()) containing an expression comprising other model quantities or model elements, and the operators +, -, *, /, ^, sqrt, log, and exp, or (4) a function whose arguments are some subset of 'specification.info', 'location', other model quantities and model elements, and ...
#'@param scale An optional parameter, indicating the scale for this model.quantity. If specified, running the model will throw an error if evaluating the quantity results in an invalid value for the scale (eg a negative rate or a proportion > 1). Can be 'rate', 'ratio', 'proportion', 'time', 'number', 'non.negative.number'
#'@param dimensions An optional parameter, indicating the dimensions the quantity is expected to have (if dim.names is not specified, the specific values of the dimensions will be inferred automatically)
#'@param dimension.values An optional parameter, indicating (some of) the elements that the dimnames of the quantity are supposed to contain. This must be a named list of character vectors, whose names are a subset of dimensions.
#'@param ... Additional parameters to be passed to value if a function is passed as the value
#'@param na.replacement A single numeric value that should be used to replace NA values generated in calculating the quantity's value (note, if na.replacement==NA, no handling of NAs is performed and NAs will cause an error in model execution)
#'
#'@export
register.model.quantity <- function(specification,
                                    name,
                                    value,
                                    scale=NULL,
                                    dimensions=names(dimension.values),
                                    dimension.values=NULL,
                                    apply.aliases.to.dimension.values=F,
                                    ...,
                                    na.replacement = as.numeric(NA))
{
    if (!is(specification, 'jheem.specification') || !R6::is.R6(specification))
        stop("'specification' must be an R6 object with class 'jheem.specification")
    
    value.name = deparse(substitute(value))
    if (!is.character(value.name) || length(value.name) != 1 || is.na(value.name))
        value.name = 'value'

    specification$register.quantity(name = name,
                                    value = value,
                                    scale = scale,
                                    dimensions = dimensions,
                                    dimension.values = dimension.values,
                                    apply.aliases.to.dimension.values = apply.aliases.to.dimension.values,
                                    ...,
                                    na.replacement = na.replacement)
}


#'@description Register a Value to a Subset of a Model Quantity or Transition
#'
#'@param name The name of the model quantity or transition to which to register a subset
#'@inheritParams register.model.quantity
#'@param applies.to A named list representing the subset of value in question. The names of applies.to should correspond to dimensions of value, and the values must be either character, integer, or logical vectors accessing values in each dimension
#'@param apply.function A character value indicating how the value for this subset should be incorporated into the main quantity. Options are 'overwrite' (the value for the subset overwrites the value in the quantity), 'add' (subset value is added to the quantity), 'subtract' (subset value subtracted from the quantity), 'multiply' (subset value multiplied by the quantity), or 'divide' (quantity is divided by the subset value)
#'
#'@export
register.model.quantity.subset <- function(specification,
                                           name,
                                           value,
                                           applies.to,
                                           na.replacement = as.numeric(NA),
                                           apply.function = c('overwrite','add','subtract','divide','multiply')[1],
                                           ...)
{
    if (!is(specification, 'jheem.specification') || !R6::is.R6(specification))
        stop("'specification' must be an R6 object with class 'jheem.specification")
    
    #-- Get value name --#
    value.name = deparse(substitute(value))
    if (!is.character(value.name) || length(value.name) != 1 || is.na(value.name))
        value.name = 'value'

    specification$register.quantity.subset(name = name,
                                           value = value,
                                           applies.to = applies.to,
                                           value.name = value.name,
                                           na.replacement = na.replacement,
                                           apply.function = apply.function,
                                           ...)
}

#'@description Register a Top-Level Quantity (ie a quantity required or pre-specified as optional for running a JHEEM simulation)
#'
#'@inheritParams register.model.quantity
#'@param name The name of the quantity. Must be one of specification$top.level.quantity.names
#'@param groups The groups to which this quantity applies. Either 'uninfected', 'infected', or 'all' ('all' applies the quantity to all groups which it could apply to)
#'
#'@export
register.top.level.quantity <- function(specification,
                                        name,
                                        value,
                                        groups='all',
                                        na.replacement = as.numeric(NA),
                                        ...)
{
    if (!is(specification, 'jheem.specification') || !R6::is.R6(specification))
        stop("'specification' must be an R6 object with class 'jheem.specification")
    
    specification$register.top.level.quantity(name = name,
                                              value = value,
                                              groups = groups,
                                              na.replacement = na.replacement,
                                              ...)
}

#'@description Register a Transition between Compartments in a Dimension
#'
#'@inheritParams register.model.quantity
#'@param name The name by which to refer to this transition. Optional, but if not specified (NULL), you will not be able to refer to this transition in other quantities or override it in descendant specifications
#'@param dimension The name of the dimension in which this transition operates
#'@param from.compartments,to.compartments The compartments from and to which this transition applies. May be (1) one or more names of compartments, (2) one or more character compartment.value.aliases, (3) one or more integers denoting compartments, (4) a logical vector, which, when applied to the dimension names, denotes the compartments
#'@param groups The groups to which this transition applies. Either 'uninfected', 'infected', or 'all' ('all' applies the transition to all groups which it could apply to)
#'
#'@export
register.transition <- function(specification,
                                name = NULL,
                                dimension,
                                from.compartments,
                                to.compartments,
                                value,
                                groups,
                                na.replacement = as.numeric(NA),
                                ...)
{
    if (!is(specification, 'jheem.specification') || !R6::is.R6(specification))
        stop("'specification' must be an R6 object with class 'jheem.specification")
    
    value.name = deparse(substitute(value))
    if (!is.character(value.name) || length(value.name) != 1 || is.na(value.name))
        value.name = 'value'

    specification$register.transition(name = name,
                                      dimension = dimension,
                                      from.compartments = from.compartments,
                                      to.compartments = to.compartments,
                                      value = value,
                                      groups = groups,
                                      na.replacement = na.replacement,
                                      ...)
}





#'@description Indicate a Transition to Track for Simulations

#'@param specification The jheem.specification object to modify
#'
#'@param groups The groups to include in this transition. Must be a subset of 'infected', 'uninfected', or 'all' ('all' selects all possible groups to which the transition could apply)
#'@param dimension The name of the (single) dimension along which we are tracking a transition
#'@param from.compartments,to.compartments The names (or aliases) of the from and to compartments of the transition to track that gives information about the resulting quantity
#'
#'@param multiply.by An optional quantity to multiply the transition by at each time step before storing. Can be either (1) a numeric value, (2) a single character value referencing a model quantity or element, or (3) an expression that includes only the names of model quantities and elements and operators +, -, *, /, ^, sqrt, log, and exp
#'
#'@param name The name by which to access the transition value after the simulation runs. Cannot be 'infected', 'uninfected' or the name of a tracked quantity
#'@param metadata An object of class 'outcome.metadata', as created by \code{\link{create.outcome.metadata}}
#'
#'@param keep.dimensions The dimensions over which to keep in storing values. By default (keep.dimensions==NULL), will keep all possible dimensions for the value
#'
#'@details Integrates the transition (or the product of transition x multiply by), such that the simulation stores, for each year y, the integral from y to y-1 of (transition * multiply.by)
#'
#'@export
track.transition <- function(specification,
                             
                             groups='all',
                             dimension,
                             from.compartments,
                             to.compartments,
                             
                             multiply.by = NULL,
                             
                             name,
                             metadata,
                             
                             keep.dimensions=NULL)
{
    if (!is(specification, 'jheem.specification') || !R6::is.R6(specification))
        stop("'specification' must be an R6 object with class 'jheem.specification")
    
    specification$track.transition(name = name,
                                   groups = groups,
                                   dimension = dimension,
                                   from.compartments = from.compartments,
                                   to.compartmens = to.compartments,
                                   keep.dimensions = keep.dimensions)
    
}

#'@description Indicate a Quantity to Track for Simulations
#'
#'@inheritParams 
#'@param name The name by which to access the quantity after the simulation runs. Cannot be 'infected', 'uninfected' or the name of a tracked transition
#'@param value Either a character string or an expression. If dynamic==T, value must be one of 'infected', 'uninfected', 'incidence', 'mortality.infected', 'mortality.uninfected', or the name of a tracked transition (set by \code{\link{track.transition}}), or an expression in which one of those values is multiplied or divided by a combination of registered model.quantities/model.elements. If dynamic==F, value must contain only 'infected', 'uninfected', registered model.quantities/model.elements, other tracked quantities, or tracked transitions. If cumulative==T and dynamic==F, then value must furthermore be either a 'infected', 'uninfected', a tracked transition, or cumulative tracked quantity multiplied by some combination of registered model.quantities/model.elements (or a sum of such terms)
#'@param scale The scale to which getting this quantity from a simulation should evaluate. 
#'@param dynamic If cumulative==T, whether value should be integrated WITHIN the diffeq solver (if dynamic==T) or AFTER it. Setting dynamic=T yields a (pretty) exact solution to the integral, whereas setting dynamic=F, yields an approximate (but computationally faster at run-time) solution
#'@param value.scale The scale to which the value denoted in value should evaluate. NB: if dynamic==T or cumulative==T, then value.scale, by definition, must be 'non.negative.number'
#'@param cumulative Whether this quantity should evaluate to the integral of value over time, or value itself
#'@param denominator If scale or value.scale requires a denominator to aggregate (ie 'rate', 'time', 'proportion'), then the character name of the denominator used in aggregating. Must be either the name of atracked transition or a tracked quantity with scale=='non.negative.number'
#' 
#'@param

#'@export
track.quantity <- function(specification,
                           name,
                           value,
                           
                           scale,
                           metadata,
                           
                           cumulative,
                           denominator.quantity.name=NULL,
                           keep.dimensions=NULL)
{
    if (!is(specification, 'jheem.specification') || !R6::is.R6(specification))
        stop("'specification' must be an R6 object with class 'jheem.specification")
    
    specification$track.quantity(name = name,
                                 value = value,
                                 scale = scale,
                                 value.scale = value.scale,
                                 cumulative = cumulative,
                                 dynamic = dynamic,
                                 denominator = denominator,
                                 keep.dimensions = keep.dimensions
                                 )
}


track.dynamic.quantity <- function(specification,
                                   name,
                                   dynamic.quantity,
                                   multiply.by = NULL)
{
    
}

track.quantity <- function(specification,
                           name,
                           value,
                           cumulative=F)
{
    
}

track.transition <- function(specification,
                             name,
                             dimension,
                             from.compartments,
                             to.compartments,
                             multiply.by = NULL,
                             keep.dimensions = NULL)
{
    
}

#'@description Create a metadata object for tracked quantities or tracked transitions
#'
#'@param scale The scale of the quantity. Either "rate", "ratio", "proportion", "time", "number", "non.negative.number"
#'@param display.name The (nicely formatted) name of the quantity - to use in titling figures and other reporting. Should be capitalized
#'@param description A phrase or single sentence describing the quantity. Should be captialized
#'@param axis.name A name to use in labeling axes on figures that display this quantity. Should be capitalized
#'@param units The units the quantity takes (eg, "cases", "%", "tests/year")
#'@param display.as.percent A logical indicator for whether, when plotted or otherwise displayed, the quantity should be converted to a percent. Only valid if scale=='proportion'
#'
#'@example Examples for incidence, new diagnoses, force:
#'create.outcome.metadata(scale = 'non.negative.number',
#'                        display.name = 'Incidence',
#'                        axis.name = 'Incident Infections',
#'                        units = 'infections')
#'create.outcome.metadata(scale = 'non.negative.number',
#'                        display.name = 'New Diagnoses',
#'                        axis.name = 'Diagnosed Cases',
#'                        units = 'cases')
#'create.outcome.metadata(scale = 'proportion',
#'                        display.name = "Viral Suppression",
#'                        axis.name = "Proportion Suppressed",
#'                        units = '%',
#'                        display.as.percent = T)
#'
#'@return An object of class 'outcome.metadata' which can be passed to \code{\link{track.transition}}, \code{\link{track.quantity}}, \code{\link{track.dynamic.quantity}}
#'
#'@export
create.outcome.metadata <- function(scale,
                                    display.name,
                                    description,
                                    axis.name,
                                    units,
                                    display.as.percent = scale=='proportion')
{
    OUTCOME.METADATA$new(
        scale = scale,
        description = description,
        display.name = display.name,
        axis.name = axis.name,
        units = units,
        display.as.percent = display.as.percent
    )
}

OUTCOME.METADATA = R6::R6Class(
    'outcome.metadata',
    
    public = list(
        
        initialize = function(scale,
                              display.name,
                              description,
                              axis.name,
                              units,
                              display.as.percent = scale=='proportion')
        {
            #-- Validate Argument --#
            error.prefix = "Error creating outcome.metadata: "
            if (!is.character(display.name) || length(display.name)!=1 || is.na(display.name) || nchar(display.name)==0)
                stop(error.prefix, "'display.name' must be a single, non-NA, non-empty character value")
            
            error.prefix = paste0("Error creating outcome.metadata for '", display.name, "': ")
            if (!is.character(description) || length(description)!=1 || is.na(description) || nchar(description)==0)
                stop(error.prefix, "'description' must be a single, non-NA, non-empty character value")
            
            if (!is.character(axis.name) || length(axis.name)!=1 || is.na(axis.name) || nchar(axis.name)==0)
                stop(error.prefix, "'axis.name' must be a single, non-NA, non-empty character value")

            if (!is.character(units) || length(units)!=1 || is.na(units) || nchar(units)==0)
                stop(error.prefix, "'units' must be a single, non-NA, non-empty character value")
            
            check.model.scale(scale, varname.for.error = 'scale', error.prefix = error.prefix)
            
            if (!is.logical(display.as.percent) || length(display.as.percent)!=1 || is.na(display.as.percent))
                stop(error.prefix, "'display.as.percent' must be a single, non-NA logical value")
                
            
            #-- Store Values --#
            private$i.scale = scale
            private$i.display.name = display.name
            private$i.description = description
            private$i.axis.name = axis.name
            private$i.units = units
            private$i.display.as.percent = display.as.percent
        }
    ),
    
    active = list(
        
        scale = function(value)
        {
            if (missing(value))
                private$i.scale
            else
                stop("Cannot modify outcome.metadata's 'scale' - it is read-only")
        },
        
        display.name = function(value)
        {
            if (missing(value))
                private$i.display.name
            else
                stop("Cannot modify outcome.metadata's 'display.name' - it is read-only")
        },
        
        description = function(value)
        {
            if (missing(value))
                private$i.description
            else
                stop("Cannot modify outcome.metadata's 'description' - it is read-only")
        },
        
        axis.name = function(value)
        {
            if (missing(value))
                private$i.axis.name
            else
                stop("Cannot modify outcome.metadata's 'axis.name' - it is read-only")
        },
        
        units = function(value)
        {
            if (missing(value))
                private$i.units
            else
                stop("Cannot modify outcome.metadata's 'units' - it is read-only")
        },
        
        display.as.percent = function(value)
        {
            if (missing(value))
                private$i.display.as.percent
            else
                stop("Cannot modify outcome.metadata's 'display.as.percent' - it is read-only")
        }
    ),
    
    private = list(
        
        i.scale = NULL,
        i.display.name = NULL,
        i.axis.name = NULL,
        i.units = NULL,
        i.description = NULL,
        i.display.as.percent = NULL
        
    )
)

##-----------------------##
##-----------------------##
##-- CLASS DEFINITIONS --##
##-----------------------##
##-----------------------##

##-------------------------------------------------##
##-- SOME INTERNAL (to this code file) CONSTANTS --##
##-------------------------------------------------##

MIN.SPECIFICATION.VERSION.NCHAR = 2
MAX.SPECIFICATION.VERSION.NCHAR = 12
MIN.SPECIFICATION.ITERATION.NCHAR = 1
MAX.SPECIFICATION.ITERATION.NCHAR = 20

MIN.SPECIFICATION.DESCRIPTION.NCHAR = 2
MAX.SPECIFICATION.DESCRIPTION.NCHAR = 500

MIN.MODEL.FROM.YEAR = 1970
MAX.MODEL.FROM.YEAR.OFFSET.FROM.CURRENT.YEAR = 10

RAMP.TAPER.SCALES = c('identity','log','exp')
RAMP.TAPER.APPLICATIONS = c('multiplier','absolute')

QUANTITY.SUBSET.APPLY.FUNCTIONS = c('overwrite','add','subtract','multiply','divide')

ALLOWED.MODEL.QUANTITY.VALUE.EXPRESSION.FUNCTIONS = c("+","-","*","/","(","log","exp","sqrt")

MIN.FUNCTIONAL.FORM.FROM.YEAR = 1970
MAX.FUNCTIONAL.FORM.FROM.YEAR.OFFSET.FROM.CURRENT.YEAR = 50


##-------------------------------##
##-- TOP-LEVEL QUANTITY SCHEMA --##
##-------------------------------##

TOP.LEVEL.QUANTITY.GROUPS = c('all','uninfected','infected')
TOP.LEVEL.QUANTITY.SCHEMA = R6::R6Class(
    'top.level.quantity.schema',
    portable = F,
    
    public = list(
        
        initialize = function(name,
                              ontology.names,
                              required=T,
                              specification,
                              alias.suffix=NULL)
        {
            #-- Validate name --#
            if (!is.character(name) || length(name) != 1 || is.na(name) || nchar(name)==0)
                stop("In creating a top.level.quantity.schema, 'name' must be a single, non-NA character value")
            
            #-- Validate required --#
            if (!is.logical(required) || length(required)!=1 || is.na(required))
                stop("In creating a top.level.quantity.schema, 'required' must be a single, non-NA logical value")
            
            #-- Validate groups --#
            if (!is.character(ontology.names) || length(ontology.names)==0 || 
                any(is.na(ontology.names)) || any(nchar(ontology.names)==0))
                stop("In creating a top.level.quantity.schema, 'ontology.names' must be a non-empty, non-NA character vector")
            
            if (is.null(names(ontology.names)))
                names(ontology.names) = ontology.names
            
            invalid.names = setdiff(names(ontology.names), TOP.LEVEL.QUANTITY.GROUPS)
            if (length(invalid.names)>0)
                stop(paste0("In creating a top.level.quantity.schema, names(ontology.names) must be one of ",
                            collapse.with.or("'", TOP.LEVEL.QUANTITY.GROUPS, "'"),
                            ". ",
                            collapse.with.and("'", invalid.names, "'"),
                            ifelse(length(invalid.names)==1, " is not a valid name", " are not valid names")))
            
            invalid.ontology.names = setdiff(ontology.names, specification$ontology.names)
            if (length(invalid.ontology.names)>0)
                stop(paste0("Invalid ",
                            ifelse(length(invalid.ontology.names)==1, 'ontology.name','ontology.names'),
                            " ", collapse.with.and("'", invalid.ontology.names, "'"),
                            " in creating top.level.quantity.schema for specification '",
                            specification$name, "'. Ontology names must be one of ",
                            collapse.with.or("'", specification$ontology.names, "'")))
            
            #-- Validate alias.suffix --#
            if (!is.null(alias.suffix))
            {
                if (!is.character(alias.suffix) || length(alias.suffix)!=1 || is.na(alias.suffix) ||
                    (alias.suffix != 'from' && alias.suffix != 'to'))
                    stop(paste0("If it is not NULL, 'alias.suffix' for a ", self$descriptor,
                                " must be a single, non-NA character value that is either 'from' or 'to'"))
            }
            
            #-- Store Values --#
            private$i.name = name
            private$i.ontology.names = ontology.names
            private$i.required = required
            private$i.alias.suffix = alias.suffix
        }
    ),
    
    active = list(
        
        ontology.names = function(value)
        {
            if (missing(value))
                private$i.ontology.names
            else
                stop("Cannot modify 'ontology.names' for a top.level.quantity.schema - they are read-only")
        },
        
        name = function(value)
        {
            if (missing(value))
                private$i.name
            else
                stop("Cannot modify 'name' for a top.level.quantity.schema - it is read-only")
        },
        
        required = function(value)
        {
            if (missing(value))
                private$i.required
            else
                stop("Cannot modify 'required' for a top.level.quantity.schema - it is read-only")
        },
        
        alias.suffix = function(value)
        {
            if (missing(value))
                private$i.alias.suffix
            else
                stop("Cannot modify 'alias.suffix' for a top.level.quantity.schema - it is read-only")
        }
    ),
    
    private = list(
        i.name = NULL,
        i.ontology.names = NULL,
        i.required = NULL,
        i.alias.suffix = NULL
    )
)


##------------------------------------------##
##------------------------------------------##
##-- JHEEM SPECIFICATION: CLASS DEFINTION --##
##--  (The public-facing, uncompiled one) --##
##------------------------------------------##
##------------------------------------------##


JHEEM.SPECIFICATION = R6::R6Class(
    classname = 'jheem.specification',
    portable = F,
        
    ##-- PUBLIC --##
    public = list(
        
        ##-- CONSTRUCTOR --## 
        ##-- (private to the package) --##    
        initialize = function(version,
                              iteration,
                              description,
                              
                              parent.specification,
                              do.not.inherit.model.quantity.names,
                              do.not.inherit.transitions.for.dimension,
                              
                              compartments.for.infected.only,
                              compartments.for.uninfected.only,
                              compartments.for.infected.and.uninfected,
                              transmission.modes,
                              age.info,
                              
                              compartment.value.character.aliases,
                              compartment.value.function.aliases,
                              
                              enable.perinatal.transmission,
                              parent.child.concordant.dimensions,
                              all.births.into.compartments,
                              
                              fix.strata.sizes.prior.to,
                              fix.strata.sizes.for.dimensions,
                              
                              model.quantities,
                              top.level.references)
        {
            # As of now, I am assuming these have already been error-checked
            # Either by the create.jheem.specification function
            # Or they are being passed forward from a (validated) specification in the 
            #   finalized.jheem.specification constructor
            
            #-- Store variables --#
            private$i.version = version
            private$i.iteration = iteration
            private$i.description = description
            
            private$i.parent.specification = parent.specification
            private$i.do.not.inherit.model.quantity.names = do.not.inherit.model.quantity.names
            private$i.do.not.inherit.transitions.for.dimension = do.not.inherit.transitions.for.dimension
            
            private$i.transmission.modes = transmission.modes
            
            private$i.age.info = age.info
            
            private$i.compartment.value.character.aliases = compartment.value.character.aliases
            private$i.compartment.value.function.aliases = compartment.value.function.aliases
            
            private$i.enable.perinatal.transmission = enable.perinatal.transmission
            private$i.parent.child.concordant.dimensions = parent.child.concordant.dimensions
            private$i.all.births.into.compartments = all.births.into.compartments
            
            private$i.fix.strata.sizes.prior.to = fix.strata.sizes.prior.to
            private$i.fix.strata.sizes.for.dimensions = fix.strata.sizes.for.dimensions
            
            private$i.quantities = model.quantities
            
            
            #-- Process dim.names --#
            
            # Store the components
            private$i.compartments = list(
                infected = compartments.for.infected.only,
                uninfected = compartments.for.uninfected.only,
                general = compartments.for.infected.and.uninfected
            )    
            
            # Calculations - temp quantities we will need
            general.infected.uninfected.dim.names = c(compartments.for.infected.and.uninfected, compartments.for.infected.only, compartments.for.uninfected.only)
            from.dim.names = to.dim.names = general.infected.uninfected.dim.names
            names(from.dim.names) = paste0(names(from.dim.names), '.from')
            names(to.dim.names) = paste0(names(to.dim.names), '.to')
            
            #alternate general.infected.uninfected with to
            alternating.orig.from.dim.names = lapply(1:(2*length(general.infected.uninfected.dim.names)), function(i){
                if (i%%2==1)
                    general.infected.uninfected.dim.names[[ceiling(i/2)]]
                else
                    from.dim.names[[i/2]]
            })
            names(alternating.orig.from.dim.names) = sapply(1:(2*length(general.infected.uninfected.dim.names)), function(i){
                if (i%%2==1)
                    names(general.infected.uninfected.dim.names)[[ceiling(i/2)]]
                else
                    names(from.dim.names)[[i/2]]
            })
            
            general.infected.from.dim.names = general.infected.to.dim.names = c(compartments.for.infected.and.uninfected, compartments.for.infected.only)
            names(general.infected.from.dim.names) = paste0(names(general.infected.from.dim.names), '.from')
            names(general.infected.to.dim.names) = paste0(names(general.infected.to.dim.names), '.to')
            
            general.uninfected.from.dim.names = general.uninfected.to.dim.names = c(compartments.for.infected.and.uninfected, compartments.for.infected.only)
            names(general.uninfected.from.dim.names) = paste0(names(general.uninfected.from.dim.names), '.from')
            names(general.uninfected.to.dim.names) = paste0(names(general.uninfected.to.dim.names), '.to')
            
            # figure out what we can leave out of birth proportions
            uninfected.birth.proportions.to.dimensions = setdiff(names(general.uninfected.to.dim.names),
                                                                 c(private$i.parent.child.concordant.dimensions,
                                                                   names(private$i.all.births.into.compartments)))
            infected.birth.proportions.to.dimensions = setdiff(names(general.infected.to.dim.names),
                                                               c(private$i.parent.child.concordant.dimensions,
                                                                 names(private$i.all.births.into.compartments)))
            
            # Store the combined dim.names
            private$i.ontologies = list(
                general = compartments.for.infected.and.uninfected,
                infected = c(compartments.for.infected.and.uninfected, compartments.for.infected.only),
                uninfected = c(compartments.for.infected.and.uninfected, compartments.for.uninfected.only),
                all = c(alternating.orig.from.dim.names, to.dim.names),
                
                contact = c(general.infected.from.dim.names, general.uninfected.to.dim.names),
                
                birth.proportions.uninfected = c(general.uninfected.from.dim.names, general.uninfected.to.dim.names[uninfected.birth.proportions.to.dimensions]),
                birth.proportions.infected.to.uninfected = c(general.infected.from.dim.names, general.uninfected.to.dim.names[uninfected.birth.proportions.to.dimensions]),
                birth.proportions.infected.to.infected = c(general.infected.from.dim.names, general.infected.to.dim.names[infected.birth.proportions.to.dimensions])
            )
            
            private$i.required.dimensions.for.ontologies = list(
                birth.proportions.uninfected = uninfected.birth.proportions.to.dimensions,
                birth.proportions.infected.to.uninfected = uninfected.birth.proportions.to.dimensions,
                birth.proportions.infected.to.infected = infected.birth.proportions.to.dimensions
            )
            
            # Some default settings
            private$i.locked = F
            
            # Create the top-level schema
            private$i.top.level.schemata = list(
                
                TOP.LEVEL.QUANTITY.SCHEMA$new(name = "new.infection.proportions",
                                              ontology.names = 'all',
                                              required = T,
                                              specification = self),
                
                #-- Mortality --#
                TOP.LEVEL.QUANTITY.SCHEMA$new(name = "infection.specific.mortality",
                                              ontology.names = 'infected',
                                              required = T,
                                              specification = self),
                
                TOP.LEVEL.QUANTITY.SCHEMA$new(name = "general.mortality",
                                              ontology.names = c('uninfected','infected'),
                                              required = T,
                                              specification = self),
                
                #-- Initial Population --#
                TOP.LEVEL.QUANTITY.SCHEMA$new(name = "initial.population",
                                              ontology.names = c('uninfected','infected'),
                                              required = T,
                                              specification = self),
                
                #-- Fertility/Births --#
                TOP.LEVEL.QUANTITY.SCHEMA$new(name = "fertility",
                                              ontology.names = c('uninfected','infected'),
                                              required = T,
                                              specification = self),
                
                TOP.LEVEL.QUANTITY.SCHEMA$new(name = 'uninfected.birth.proportions',
                                              ontology.names = c(uninfected='birth.proportions.uninfected',
                                                                 infected='birth.proportions.infected.to.uninfected'),
                                              required = T,
                                              specification = self,
                                              alias.suffix = 'from')
            )
            
            # Only if perinatal transmission is enabled
            if (private$i.enable.perinatal.transmission)
            {
                private$i.top.level.schemata = c(private$i.top.level.schemata,
                                                 list(
                                                     TOP.LEVEL.QUANTITY.SCHEMA$new(name = 'fraction.births.infected',
                                                                                   ontology.names = 'infected',
                                                                                   required = T,
                                                                                   specification = self),
                                                     
                                                     TOP.LEVEL.QUANTITY.SCHEMA$new(name = 'infected.birth.proportions',
                                                                                   ontology.names = c(infected='birth.proportions.infected.to.infected'),
                                                                                   required = T,
                                                                                   specification = self,
                                                                                   alias.suffix = 'from')
                                                 ))
            }
            
            
            # Aging (optional, if we have an age dimension)
            if (any(self$all.dimensions=='age'))
                private$i.top.level.schemata = c(private$i.top.level.schemata,
                                                 list(TOP.LEVEL.QUANTITY.SCHEMA$new("aging",
                                                                                    ontology.names = c('infected','uninfected'),
                                                                                    required = F,
                                                                                    specification = self)))
            
            # Add in transmission-mode specific schema
            for (mode in transmission.modes)
            {
                to.add = list(
                    
                    TOP.LEVEL.QUANTITY.SCHEMA$new(name = paste0(mode, ".susceptibility"),
                                                  ontology.names = 'uninfected',
                                                  required = F,
                                                  specification = self),
                    
                    TOP.LEVEL.QUANTITY.SCHEMA$new(name = paste0(mode, ".transmissibility"),
                                                  ontology.names = 'infected',
                                                  required = F,
                                                  specification = self),
                    
                    TOP.LEVEL.QUANTITY.SCHEMA$new(name = paste0(mode, ".contact"),
                                                  ontology.names = c(all='contact'),
                                                  required = T,
                                                  specification = self,
                                                  alias.suffix = 'from')
                )
                
                private$i.top.level.schemata = c(private$i.top.level.schemata, to.add)
            }
            
            # Set the schema names
            names(private$i.top.level.schemata) = sapply(private$i.top.level.schemata, function(sch){sch$name})
            
            # Check for duplicate schema (shouldn't happen)
            tabled.schema.names = table(names(private$i.top.level.schemata))
            if (any(tabled.schema.names>1))
                stop(paste0("Error creating specification: repeated top-level schema name(s): ",
                            collapse.with.and("'", names(tabled.schema.names)[tabled.schema.names>1], "'")))
            
            #-- We're done! --#
        },
        
        print = function(...)
        {
            cat("A JHEEM model specification for version '",
                private$i.version, "'\n", sep='')
        
            invisible(self)
        },

    ##-- PUBLIC-FACING FUNCTIONS --##
        register.element = function(name,
                                    scale,
                                    
                                    dimensions=NULL,
                                    dimension.values=NULL,
                                    apply.aliases.to.dimension.values=F,
                                    
                                    value=NULL,
                                    get.value.function=NULL,
                                    
                                    functional.form=NULL,
                                    get.functional.form.function=NULL,
                                    functional.form.scale=scale,
                                    functional.form.from.time=NULL,
                                    functional.form.to.time=Inf,
                                    
                                    get.value.function.name = NULL,
                                    get.functional.form.function.name = NULL,
                                    ...,
                                    
                                    ramp.scale=scale,
                                    ramp.times=numeric(),
                                    ramp.values=numeric(),
                                    ramp.value.application=c('multiplier','absolute')[1],
                                    ramp.interpolate.links='identity',
                                    
                                    taper.scale=scale,
                                    taper.times=numeric(),
                                    taper.values=numeric(),
                                    taper.value.application=c('multiplier','absolute')[1],
                                    taper.interpolate.links='identity')
        {
            #-- Validate name --#
            validate.quantity.name(name, descriptor = self$descriptor,
                                   error.prefix='Cannot register model element: ')
            
            error.prefix = paste0("Cannot register model element '", name, "': ")
            
            #-- Plug in default values for the function names --#
            if (is.null(get.value.function.name) && !is.null(get.value.function))
            {
                get.value.function.name = deparse(substitute(get.value.function))
                if (!is.character(get.value.function.name) || length(get.value.function.name) != 1)
                    get.value.function.name = 'get.value.function'
            }
            if (is.null(get.functional.form.function.name) && !is.null(get.functional.form.function))
            {
                get.functional.form.function.name = deparse(substitute(get.functional.form.function))
                if (!is.character(get.functional.form.function.name) || length(get.functional.form.function.name) != 1)
                    get.functional.form.function.name = 'get.functional.form.function'
            }
            
            #-- Call the constructor and register --#
            element = MODEL.ELEMENT$new(name = name,
                                        version = private$i.version,
                                        scale = scale,
                                        
                                        dimensions = dimensions,
                                        dimension.values = dimension.values,
                                        apply.aliases.to.dimension.values = apply.aliases.to.dimension.values,
                                        
                                        value = value,
                                        get.value.function = get.value.function,
                                        
                                        functional.form = functional.form,
                                        get.functional.form.function = get.functional.form.function,
                                        functional.form.scale = functional.form.scale,
                                        functional.form.from.time = functional.form.from.time,
                                        functional.form.to.time = functional.form.to.time,
                                        
                                        get.value.function.name = get.value.function.name,
                                        get.functional.form.function.name = get.functional.form.function.name,
                                        ...,
                                        
                                        ramp.scale = ramp.scale,
                                        ramp.times = ramp.times,
                                        ramp.values = ramp.values,
                                        ramp.value.application = ramp.value.application,
                                        ramp.interpolate.links = ramp.interpolate.links,
                                        
                                        taper.scale = taper.scale,
                                        taper.times = taper.times,
                                        taper.values = taper.values,
                                        taper.value.application = taper.value.application,
                                        taper.interpolate.links = taper.interpolate.links,
                                        
                                        error.prefix = error.prefix)
            
            private$do.register.quantity(element, error.prefix=error.prefix)
        },
        
        register.quantity = function(name,
                                     value,
                                     value.name = NULL,
                                     scale=NULL,
                                     dimensions=names(dimension.values),
                                     dimension.values=NULL,
                                     apply.aliases.to.dimension.values=F,
                                     na.replacement=as.numeric(NA),
                                     ...)
        {
            #-- Validate name --#
            validate.quantity.name(name, descriptor = 'model quantity',
                                   error.prefix = "Cannot register model quantity: ")
            
            error.prefix = paste0("Cannot register model quantity '", name, "': ")
            
            #-- Get value name --#
            if (is.null(value.name))
            {
                value.name = deparse(substitute(value))
                if (!is.character(value.name) || length(value.name) != 1 || is.na(value.name))
                    value.name = 'value'
            }
            
            #-- Call the constructor and register --#
            quantity = NON.TERMINAL.MODEL.QUANTITY$new(name = name,
                                                       version = private$i.version,
                                                       value = value,
                                                       value.name = value.name,
                                                       scale = scale,
                                                       dimensions = dimensions,
                                                       dimension.values = dimension.values,
                                                       apply.aliases.to.dimension.values = apply.aliases.to.dimension.values,
                                                       na.replacement = na.replacement,
                                                       ...,
                                                       error.prefix = error.prefix)

            private$do.register.quantity(quantity, error.prefix=error.prefix)
        },
        
        register.quantity.subset = function(name,
                                            value,
                                            applies.to,
                                            value.name = NULL,
                                            na.replacement=as.numeric(NA),
                                            apply.function = c('overwrite','add','subtract','divide','multiply')[1],
                                            ...)
        {
            #-- Validate name --#
            error.prefix = "Cannot register subset for model quantity: "
            if (!is.character(name))
                stop(paste0(error.prefix, "'name' must be a character value"))
            if (length(name)!=1)
                stop(paste0(error.prefix, "'name' must be a SINGLE character value"))
            if (is.na(name))
                stop(paste0(error.prefix, "'name' cannot be NA"))

            error.prefix = paste0("Cannot register subset for model quantity '", name, "': ")

            #-- Pull the parent model quantity
            quantity = private$i.quantities[[name]]
            if (is.null(quantity))
                stop(paste0(error.prefix,
                            "No quantity with name '", name, 
                            "' has been registered. You must register the quantity before you can register subsets for it."))
            
            #-- Get value name --#
            if (is.null(value.name))
            {
                value.name = deparse(substitute(value))
                if (!is.character(value.name) || length(value.name) != 1 || is.na(value.name))
                    value.name = 'value'
            }
            
            #-- Create the model quantity component object --#
            comp = MODEL.QUANTITY.COMPONENT$new(value = value,
                                                applies.to = applies.to,
                                                allow.empty.applies.to = T,
                                                na.replacement = na.replacement,
                                                apply.function = apply.function,
                                                ...,
                                                value.name = value.name,
                                                error.prefix = error.prefix,
                                                parent.quantity = quantity)
            
            #-- Check if any of the applies.to overlap --#
            #   (the first component applies to the whole quantity, by definition, and does not overlap)

            if (quantity$n.components>1)
                sapply(2:quantity$n.components, function(i){
                    other.comp = quantity$components[[i]]
                    if (dimension.values.overlap(comp$applies.to, other.comp$applies.to))
                    {
                        stop(paste0(error.prefix,
                                    "The specified applies.to argument for this subset overlaps with the (previously registered) ",
                                    get.ordinal(i-1), " subset of '",
                                    name, "'"))
                    }
                })
            
            #-- Add to the quantity --#
            quantity$add.component(comp = comp,
                                   error.prefix=error.prefix)

            #-- Return --#
            invisible(self)
        },
        
        register.transition = function(name = NULL,
                                       dimension,
                                       from.compartments,
                                       to.compartments,
                                       value,
                                       groups,
                                       value.name = NULL,
                                       na.replacement = as.numeric(NA),
                                       ...)
        {
            #-- Set up error prefix --#
            #-- Validate name (if not NULL) --#
            if (is.null(name))
            {
                if (is.character(dimension) && length(dimension)==1 && !is.na(dimension))
                    error.prefix = paste0("Cannot register transition in dimension '", dimension, "': ")
                else
                    error.prefix = "Cannot register transition: "
            }
            else
            {
                validate.quantity.name(name, descriptor = "transition",
                                       error.prefix = "Cannot register transition: ")
                error.prefix = paste0("Cannot register transition '", name, "': ")
            }        
            error.prefix = "Cannot register transition: "
            
            #-- Validate groups --#
            if (!is.null(groups))
            {
                if (!is.character(groups))
                    stop(paste0(error.prefix, "'groups' must be a character vector"))
                if (length(groups)==0)
                    stop(paste0(error.prefix, "'groups' must have at least one element"))
                if (any(is.na(groups)))
                    stop(paste0(error.prefix, "'groups' cannot contain NA values"))
                
                invalid.groups = setdiff(groups, TOP.LEVEL.QUANTITY.GROUPS)
                if (length(invalid.groups)>0)
                {
                    stop(paste0(error.prefix, "Invalid ",
                                ifelse(length(invalid.groups)==0, 'value', 'values'),
                                " for 'groups': ",
                                collapse.with.and("'", invalid.groups, "'"),
                                ". (Must be a subset of ",
                                collapse.with.and("'", TOP.LEVEL.QUANTITY.GROUPS, "'")))
                }
            }
            
            
            #-- Validate dimension --#
            if (!is.character(dimension))
                stop(paste0(error.prefix, "'dimension' must be a single CHARACTER value"))
            if (length(dimension)!=1)
                stop(paste0(error.prefix, "'dimension' must be a SINGLE character value"))
            if (is.na(dimension))
                stop(paste0(error.prefix, "'dimension' cannot be NA"))
            
            dimension.valid.for.group = sapply(TOP.LEVEL.QUANTITY.GROUPS, function(g){
                any(names(private$i.ontologies[[g]]) == dimension)
            })
            
            if (!dimension.valid.for.group['all'])
                stop(paste0(error.prefix,
                            "'", dimension, "' is not a valid dimension for a version '",
                            self$version, "' specification"))
            
            invalid.groups.for.dimension = groups[!dimension.valid.for.group[groups]]
            valid.groups.for.dimension = setdiff(TOP.LEVEL.QUANTITY.GROUPS[dimension.valid.for.group], 'all')
            if (length(invalid.groups.for.dimension)>0)
                stop(paste0(error.prefix, 
                            "'", group, "' is not a valid group for dimension '",
                            dimension, "'. Only ", 
                            collapse.with.and("'", c(valid.groups.for.dimension, 'all'), "'"),
                            " are valid groups for '", dimension, "'"))

            #-- Set up ontology names for groups --#
            
            if (any(groups=='all'))
                ontology.names = valid.groups.for.dimension
            else
                ontology.names = groups
            
            #-- Validate from.compartments, to.compartments --#
            validate.from.or.to.compartments(value = from.compartments,
                                             variable.name.for.error = 'from.compartments',
                                             error.prefix = error.prefix)
            validate.from.or.to.compartments(value = to.compartments,
                                             variable.name.for.error = 'to.compartments',
                                             error.prefix = error.prefix)
            
            #-- If the name is NULL, make it --#
            #   (If not NULL, it has already been validated) 
            
            if (is.null(name))
            {
                if (is.logical(from.compartments))
                    from.val = (1:length(from.compartments))[from.compartments]
                else
                    from.val = from.compartments
                
                if (is.logical(to.compartments))
                    to.val = (1:length(to.compartments))[to.compartments]
                else
                    to.val = to.compartments
                
                name = paste0(DEFAULT.TRANSITION.NAME.PREFIX,
                              dimension, "_",
                              paste0(from.val, collapse=','),
                              "--",
                              paste0(to.val, collapse=','))
                
                error.prefix = paste0("Cannot register transition in dimension '", dimension, 
                                      "' (from ", paste0("'", from.compartments, "'", collapse='/'),
                                      " to ", paste0("'", to.compartments, "'", collapse='/'),
                                      ")",
                                      ifelse(length(groups)==1, paste0("for '", groups, "'"), ""),
                                      ": ")
                
            }
            
            #-- Get value name --#
            if (is.null(value.name))
            {
                value.name = deparse(substitute(value))
                if (!is.character(value.name) || length(value.name) != 1 || is.na(value.name))
                    value.name = 'value'
            }
            
            #-- Make the transition object --#
            transition = NON.TERMINAL.MODEL.QUANTITY$new(name = name,
                                                         version = private$i.version,
                                                         value = value,
                                                         value.name = value.name,
                                                         is.transition = T,
                                                         na.replacement = na.replacement,
                                                         ...,
                                                         error.prefix = error.prefix)
            
        #-- Register a reference to the transition for each group --#
            for (ontology.name in ontology.names)
            {
                ref = TRANSITION.REFERENCE$new(value.quantity.name = name,
                                               specification = self,
                                               ontology.name = ontology.name,
                                               dimension = dimension,
                                               from.compartments = from.compartments,
                                               to.compartments = to.compartments)
                private$do.register.top.level.reference(ref, error.prefix)
            }
            
            #-- Register the transition as a quantity --#
            private$do.register.quantity(transition, error.prefix=error.prefix)
        },
    
        register.top.level.quantity = function(name,
                                               value,
                                               groups='all',
                                               na.replacement = as.numeric(NA),
                                               ...)
        {
            error.prefix = "Cannot register top-level quantity: "
            #-- Check that name is a valid name for an expected top-level quantity --#
            if (!is.character(name))
                stop(paste0(error.prefix, "'name' must be a character value"))
            if (length(name)!=1)
                stop(paste0(error.prefix, "'name' must be a SINGLE character value"))
            if (is.na(name))
                stop(paste0(error.prefix, "'name' cannot be NA"))
            if (nchar(name)==0)
                stop(paste0(error.prefix, "'name' cannot be an empty string('')"))
            if (all(name != names(private$i.top.level.schemata)))
                stop(paste0(error.prefix, "'", name, "' is not a valid name for a top-level quantity. Must be one of: ",
                            collapse.with.or("'", names(private$i.top.level.schemata), "'")))
            
            error.prefix = paste0("Cannot register top-level quantity '", name, "': ")
            
            #-- Check the group argument --#
            schema = private$i.top.level.schemata[[name]]
            valid.groups = union(names(schema$ontology.names), 'all')
            invalid.groups = setdiff(groups, valid.groups)
            
            if (length(invalid.groups)>0)
                stop(paste0(error.prefix,
                            collapse.with.and("'", invalid.groups, "'"),
                            ifelse(length(invalid.groups)==1, ' is not a valid group ', ' are not valid groups '),
                            "for quantity '", name, "'. Must be one of ",
                            collapse.with.or("'", valid.groups, "'")))
            
            ontology.names = character()
            if (any(groups=='all'))
                ontology.names = schema$ontology.names
            else
                ontology.names = schema$ontology.names[groups]
            
            #-- If we need to register this as a quantity before registering the reference, do so --#
            if (is.character(value) && length(value)==1 && !is.na(value) && nchar(value)!=0)
            {
                #do we want to check that this is a valid quantity name
                # (ie, no 'this' or 'super')
                
                value.quantity.name = value
            }
            else
            {
                self$register.quantity(name = name,
                                       value = value,
                                       dimensions = NULL,
                                       dimension.values = NULL,
                                       apply.aliases.to.dimension.values = F,
                                       na.replacement = na.replacement,
                                       ...)
                
                value.quantity.name = name
            }
            
            #-- Register a reference to the quantity for each group --#
            error.prefix = paste0("Cannot register top-level quantity '", name, "': ")
            for (ontology.name in ontology.names)
            {
                ref = TOP.LEVEL.REFERENCE$new(name = name,
                                              specification = self,
                                              ontology.name = ontology.name,
                                              value.quantity.name = value.quantity.name,
                                              alias.suffix = schema$alias.suffix)
                
                private$do.register.top.level.reference(ref, error.prefix)
            }
            
            invisible(self)
        },
        
        track.transition = function(specification,
                                    name,
                                    groups='all',
                                    dimension,
                                    from.compartments,
                                    to.compartments,
                                    keep.dimensions)
        {
            
        },
    
        track.quantity = function(specification,
                                  name,
                                  value,
                                  scale,
                                  value.scale=scale,
                                  denominator=NULL,
                                  dynamic=NULL
                                  )
        {
            
        },
    
        #to be called by code in the package
        compile = function()
        {            
            if (private$i.locked || !is.specification.registered.for.version(private$i.version) ||
                is.compiled.specification.registered.for.version(private$i.version))
                stop("compile() should not be invoked directly for a JHEEM specification object - use register.model.specification() to register the specification instead")
            
            is.recursive.call = is(parent.env(parent.frame()), 'jheem.specification')
            # ^true if the call to compile on THIS specification was a recursive call 
            #   on the parent.specification within the compile() method of a
            #   descendant specification
    
            if (is.null(private$i.parent.specification))
                compiled.parent = NULL
            else
                compiled.parent = private$i.parent.specification$compile()
            
            rv = JHEEM.COMPILED.SPECIFICATION$new(version = private$i.version,
                                                  iteration = private$i.iteration,
                                                  description = private$i.description,
                                                  
                                                  compartment.value.character.aliases = private$i.compartment.value.character.aliases,
                                                  compartment.value.function.aliases = private$i.compartment.value.function.aliases,
                                                  ontologies = private$i.ontologies,
                                                  required.dimensions.for.ontologies = private$i.required.dimensions.for.ontologies,
                                                  
                                                  compartments = private$i.compartments,
                                                  
                                                  quantities = private$i.quantities,
                                                  
                                                  age.info = private$i.age.info,
                                                  transmission.modes = private$i.transmission.modes,
                                                  fix.strata.sizes.prior.to = private$i.fix.strata.sizes.prior.to,
                                                  fix.strata.sizes.for.dimensions = private$i.fix.strata.sizes.for.dimensions,
                                                  
                                                  enable.perinatal.transmission = private$i.enable.perinatal.transmission,
                                                  parent.child.concordant.dimensions = private$i.parent.child.concordant.dimensions,
                                                  all.births.into.compartments = private$i.all.births.into.compartments,
                                                  
                                                  parent.specification = private$i.parent.specification,
                                                  do.not.inherit.model.quantity.names = private$i.do.not.inherit.model.quantity.names,
                                                  do.not.inherit.transitions.for.dimension = private$i.do.not.inherit.transitions.for.dimension,
                                                  
                                                  top.level.schemata = private$i.top.level.schemata,
                                                  top.level.references = private$i.top.level.references,
                                                  
                                                  do.compile = !is.recursive.call)
            
            
            if (!is.recursive.call)
                private$i.locked = T
            
            rv
        }
    ),

##-- ACTIVE - GETTERS --##
    active = list(
    
        parent.version = function(value)
        {
            if (missing(value))
            {
                if (is.null(private$i.parent.specification))
                    character()
                else
                    private$i.parent.specification$version
            }
            else
                stop("Cannot modify 'parent.version' for a jheem.specification - it is read-only")  
        },
        
        ontologies = function(value)
        {
            if (missing(value))
                private$i.ontologies
            else
                stop("Cannot modify 'ontologies' for a jheem.specification - they are read-only")
        },
        
        compartment.value.character.aliases = function(value)
        {
            if (missing(value))
                private$i.compartment.value.character.aliases
            else
                stop("Cannot modify 'compartment.value.character.aliases' for a jheem.specification - they are read-only")
        },
        
        compartment.value.function.aliases = function(value)
        {
            if (missing(value))
                private$i.compartment.value.function.aliases
            else
                stop("Cannot modify 'compartment.value.function.aliases' for a jheem.specification - they are read-only")
        },
        
        enable.perinatal.transmission = function(value)
        {
            if (missing(value))
                private$i.enable.perinatal.transmission
            else
                stop("Cannot modify 'enable.perinatal.transmission' for a jheem.specification - it is read-only")
        },
        
        parent.child.concordant.dimensions = function(value)
        {
            if (missing(value))
                private$i.parent.child.concordant.dimensions
            else
                stop("Cannot modify 'parent.child.concordant.dimensions' for a jheem.specification - they are read-only")
        },
        
        all.births.into.compartments = function(value)
        {
            if (missing(value))
                private$i.all.births.into.compartments
            else
                stop("Cannot modify 'all.births.into.compartments' for a jheem.specification - they are read-only")
        },
        
        ontology.names = function(value)
        {
            if (missing(value))
                names(private$i.ontologies)
            else
                stop("Cannot modify 'ontology.names' for a jheem.specification - they are read-only")
        },
        
        all.dimension.names = function(value)
        {
            if (missing(value))
                private$i.ontologies[['all']]
            else
                stop("Cannot modify 'all.dimension.names' for a jheem.specification - it is read-only")
        },
        
        all.dimensions = function(value)
        {
            if (missing(value))
                names(private$i.ontologies[['all']])
            else
                stop("Cannot modify 'all.dimensions' for a jheem.specification - they are read-only")
        },
        
        compartments = function(value)
        {
            if (missing(value))
                private$i.compartments
            else
                stop("Cannot modify 'compartments' for a jheem.specification - they are read-only")
        },
        
        groups = function(value)
        { 
            if (missing(value))
                names(private$i.ontologies)
            else
                stop("Cannot modify 'groups' for a jheem.specification - it is read-only")
        },
        
        version = function(value)
        {
            if (missing(value))
                private$i.version
            else
                stop("Cannot modify 'version' for a jheem.specification - it is read-only")
        },
        
        iteration = function(value)
        {
            if (missing(value))
                private$i.iteration
            else
                stop("Cannot modify 'iteration' for a jheem.specification - it is read-only")
        },
        
        description = function(value)
        {
            if (missing(value))
                private$i.description
            else
                stop("Cannot modify 'description' for a jheem.specification - it is read-only")
        },
        
        age.info = function(value)
        {
            if (missing(value))
                private$i.age.info
            else
                stop("Cannot modify a specification's 'age.info' - it is read-only")
        },
        
        transmission.modes = function(value)
        {
            if (missing(value))
                private$i.transmission.modes
            else
                stop("Cannot modify a specification's 'transmission.modes' - they are read-only")
        },
        
        fix.strata.sizes.prior.to = function(value)
        {
            if (missing(value))
                private$i.fix.strata.sizes.prior.to
            else
                stop("Cannot modify a specification's 'fix.strata.sizes.prior.to' - it is read-only")
        },
        
        fix.strata.sizes.for.dimensions = function(value)
        {
            if (missing(value))
                private$i.fix.strata.sizes.for.dimensions
            else
                stop("Cannot modify a specification's 'fix.strata.sizes.for.dimensions' - it is read-only")
        },
        
        is.locked = function(value)
        {
            if (missing(value))
                private$i.locked
            else
                stop("Cannot modify a specification's 'is.locked' value - it is read-only")
        },
        
        required.quantity.schema = function(value)
        {
            if (missing(value))
                stop('need to implement')
            else
                stop("Cannot modify a specification's 'required.quantity.schema' - it is read-only")
        },
        
        quantity.names = function(value)
        {
            if (missing(value))
                names(private$i.quantities)
            else
                stop("Cannot modify a specification's 'quantity.names' - it is read-only")
        },
        
        top.level.quantity.names = function(value)
        {
            if (missing(value))
                names(private$i.top.level.schemata)
            else
                stop("Cannot modify a specification's 'top.level.quantity.names' - it is read-only")
        }
    ),

    ##-- PRIVATE --##
    private = list(
    
    ##-- MEMBER VARIABLES --##
    
        i.locked = NULL,
        
        i.version = NULL,
        i.iteration = NULL,
        i.description = NULL,
        
        i.compartment.value.character.aliases = NULL,
        i.compartment.value.function.aliases = NULL,
        
        i.ontologies = NULL,
        i.compartments = NULL,
        i.required.dimensions.for.ontologies = NULL,
        
        i.enable.perinatal.transmission = NULL,
        i.parent.child.concordant.dimensions = NULL,
        i.all.births.into.compartments = NULL,
        
        i.quantities = NULL,
        
        i.age.info = NULL,
        i.transmission.modes = NULL,
        i.fix.strata.sizes.prior.to = NULL,
        i.fix.strata.sizes.for.dimensions = NULL,
        
        i.parent.specification = NULL,
        i.do.not.inherit.model.quantity.names = NULL,
        i.do.not.inherit.transitions.for.dimension = NULL,
        
        i.top.level.schemata = NULL,
        i.top.level.references = NULL,
        
    ##-- INTERNAL METHODS --##
    
        check.lock = function(error.prefix)
        {
            if (private$i.locked)
                stop(paste0(error.prefix, "The '", private$i.name, "' specification has already been registered and cannot be modified further"))
        },
    
        do.register.quantity = function(quantity, error.prefix=error.prefix)
        {
            # Check lock
            private$check.lock(error.prefix)
               
            # Check for name clashes
            if (any(self$quantity.names==quantity$name))
            {
                clashing.quantity = private$i.quantities[[quantity$name]]
                if (string.begins.with(str = quantity$name, prefix = DEFAULT.TRANSITION.NAME.PREFIX))
                    stop(paste0(error.prefix,
                                "A transition for the same dimension and compartments (",
                                quantity$name, ") has already been registered"))
                else
                    stop(paste0(error.prefix, "The name '", quantity$name, "' has already been used (for a ",
                                clashing.quantity$descriptor, ")"))
            }
            
            # Add to the transition manager
            private$i.quantities[[quantity$name]] = quantity
            
            # Silently return self
            invisible(self)
        },
    
        do.register.top.level.reference = function(ref, error.prefix)
        {
            #-- Check for clashes --#
            sapply(private$i.top.level.references, function(other.ref){
                
                if (ref$overlaps(other.ref))
                {
                    if (ref$type=='transition')
                        stop(paste0(error.prefix,
                                    "An overlapping in the '", dimension, "' (from ",
                                    collapse.with.and("'", intersect(ref$from.compartments, other.ref$from.compartments), "'"),
                                    " to ",
                                    collapse.with.and("'", intersect(ref$to.compartments, other.ref$to.compartments), "'"),
                                    ") for group '", ref$group, "' has already been registered"))
                    else
                    {
                        stop(paste0(error.prefix,
                                    "The top-level quantity '", ref$name, 
                                    "' for group '", names(ref$ontology.name), 
                                    "' has already been registered"))
                    }
                }
            })
            
            private$i.top.level.references = c(private$i.top.level.references, ref)
            
            invisible(self)
        },
    
        ##----------------------------------------------##
        ##--             INTERNAL HELPERS             --##
        ##-- To Recurse Up the Ancestor Specification --##
        ##----------------------------------------------##
        
        get.ancestor.element.names = function(include.self.names)
        {
            if (include.self.names)
                rv = names(private$i.model.elements)
            else
                rv = character()
            
            if (!is.null(private$i.parent.specification))
                rv = union(rv, private$i.parent.specification$get.ancestor.mapping.element.names(include.self.names = T))
            
            rv
        },
    
        get.ancestor.quantity.names = function(include.self.names)
        {
            if (include.self.names)
                rv = names(private$i.model.quantities)
            else
                rv = character()
            
            if (!is.null(private$i.parent.specification))
                rv = union(rv, private$i.parent.specification$get.ancestor.quantity.names(include.self.names = T))
            
            rv
        }
    )
)

##------------------------------------##
##------------------------------------##
##-- MODEL QUANTITY CLASS HIERARCHY --##
##------------------------------------##
##------------------------------------##


MODEL.QUANTITY = R6::R6Class(
    'model.quantity',
    portable = F,
    
    public = list(
        
        initialize = function(name, 
                              type,
                              version,
                              scale,
                              allow.scale.missing,
                              dimensions,
                              dimension.values,
                              apply.aliases.to.dimension.values,
                              error.prefix)
        {
            #**Note: We don't validate.quantity.name here because transition names
            #        will violate the rules for quantity name's
            #        (which are in place partly to allow unique names for transitions)
            
            #-- Validate type --#
            if (!is.character(type) || length(type)!=1 || is.na(type) || nchar(type)==0)
                stop(paste0(error.prefix, "'type' must be a single, non-NA, non-empty character value"))
            
            #-- Validate version --#
            if (!is.character(version) || length(version)!=1 || is.na(version) || nchar(version)==0)
                stop(paste0(error.prefix, "'version' must be a single, non-NA, non-empty character value"))
            
            #-- Validate scale --#
            if (!allow.scale.missing || !is.null(scale))
                check.model.scale(scale, 'scale', error.prefix = error.prefix)
            
            #-- Validate dimensions and dimension.values --#
            if (is.null(dimensions))
            {
                if (!is.null(dimension.values))
                    stop(paste0(error.prefix,
                                "if 'dimensions' is NULL, 'dimension.values' must also be NULL"))
            }
            else
            {
                if (!is.character(dimensions) || length(dimensions)==0 || any(is.na(dimensions)) || any(dimensions==''))
                    stop(paste0(error.prefix, "'dimensions' must be a non-NA character vector with at least one element"))
                if (max(table(dimensions))>1)
                    stop(paste0(error.prefix, "'dimensions' cannot have repeated values"))
                
                if (!is.null(dimension.values))
                {
                    check.dim.names.valid(dim.names = dimension.values,
                                          variable.name.for.error = 'dimension.values',
                                          refer.to.dimensions.as = 'dimension',
                                          allow.empty = F,
                                          allow.duplicate.values.within.dimensions = F,
                                          allow.duplicate.values.across.dimensions = F,
                                          error.prefix = error.prefix)
                    
                    missing.from.dimensions = setdiff(names(dimension.values), dimensions)
                    if (length(missing.from.dimensions)>0)
                        stop(paste0(error.prefix,
                                    ifelse(length(missing.from.dimensions)==1, "Dimension ", "Dimensions "),
                                    collapse.with.and("'", missing.from.dimensions, "'"),
                                    ifelse(length(missing.from.dimensions)==1, " was", " were"),
                                    " listed as dimensions in 'dimension.values', but ",
                                    ifelse(length(missing.from.dimensions)==1, "is", "are"),
                                    " not present in 'dimensions'"))
                }
            }
            
            #-- Validate apply.aliases.to.dimension.values --#
            if (!is.logical(apply.aliases.to.dimension.values) || length(apply.aliases.to.dimension.values) != 1 ||
                is.na(apply.aliases.to.dimension.values))
                stop(paste0(error.prefix, "'apply.aliases.to.dimension.values' must be a single, non-NA logical value (T or F)"))

            #-- Store values --#
            private$i.name = private$i.original.name = name
            private$i.version = version
            private$i.scale = scale
            private$i.type = type
            
            private$i.fixed.dimensions = dimensions
            private$i.fixed.dimension.values = dimension.values
            private$i.apply.aliases.to.dimension.values = apply.aliases.to.dimension.values
            
            
            #-- Set some empty values --#
            private$i.depends.on = character()
            
            private$i.dimension.aliases = character()
            private$i.reversed.aliases = character()
            private$i.maximum.dim.names = NULL
            private$i.fixed.dim.names = NULL
        },
        
        set.max.dim.names.and.dimension.aliases = function(max.dim.names, dimension.aliases, error.prefix)
        {
            # Set the max dim names
            private$i.maximum.dim.names = max.dim.names
            
            # Check dimension.aliases
            if (length(dimension.aliases)==0)
                dimension.aliases = reversed.aliases = character()
            else
            {
                if (!is.character(dimension.aliases) || 
                    is.null(names(dimension.aliases)) ||
                    any(is.na(names(dimension.aliases))) ||
                    any(names(dimension.aliases)=='') ||
                    any(is.na(dimension.aliases)) ||
                    any(dimension.aliases==''))
                    stop(paste0(error.prefix, "If 'dimension.aliases' is not empty, it must be a non-NA, named character vector with non-empty names"))

                tabled.aliases = table(dimension.aliases)
                if (any(tabled.aliases>1))
                    stop(paste0(error.prefix,
                                "dimensions cannot appear more than once in 'dimension.aliases' (",
                                collapse.with.and("'", tabled.aliases[tabled.aliases>1], "' appears ",
                                                  tabled.aliases[tabled.aliases>1], " times")))
                
                tabled.alias.names = table(names(dimension.aliases))
                if (any(tabled.alias.names>1))
                    stop(paste0(error.prefix,
                                "dimensions cannot appear more than once in the names of 'dimension.aliases' (",
                                collapse.with.and("'", names(tabled.alias.names)[tabled.alias.names>1], 
                                                  "' appears ", tabled.alias.names[tabled.alias.names>1], " times")))
                
                reversed.aliases = names(dimension.aliases)
                names(reversed.aliases) = as.character(dimension.aliases)
            }   
            
            # Set 
            private$i.dimension.aliases = dimension.aliases
            private$i.reversed.aliases = reversed.aliases
            
            # Calculate the reverse mapping
            if (is.null(max.dim.names))
                private$i.reversed.dimension.alias.mapping = NULL
            else
            {
                private$i.reversed.dimension.alias.mapping = names(max.dim.names)
                names(private$i.reversed.dimension.alias.mapping) = private$i.reversed.dimension.alias.mapping
                private$i.reversed.dimension.alias.mapping[names(reversed.aliases)] = reversed.aliases
            }
            
            # Done
            invisible(self)
        },
        
        rename = function(name)
        {
            if (!is.character(name) || length(name) != 1 || is.na(name))
                stop("In renaming a model quantity, 'name' must be a single, non-NA character value")
            
            private$i.name = name
            invisible(self)
        },
        
        get.original.name = function(wrt.version, with.quotes=T)
        {
            if (with.quotes)
                qu = "'"
            else
                qu = ''
            
            if (wrt.version == private$i.version)
                paste0(qu, private$i.original.name, qu)
            else
                paste0(qu, private$i.original.name, qu, " (from '", private$i.version, "')")
        },
        
        apply.compartment.aliases = function(aliases, error.prefix)
        {
            stop("the apply.compartment.aliases() method must be implemented in a subclass of model.quantity")
        },
        
        set.fixed.dim.names = function(dim.names)
        {
            private$i.fixed.dim.names = dim.names
            invisible(self)
        },
        
        resolve.compartment.values = function(aliases, 
                                              ontology,
                                              unresolved.alias.names,
                                              ontology.name.for.error,
                                              error.prefix,
                                              wrt.specification)
        {   
            if (private$i.apply.aliases.to.dimension.values)
            {
                private$i.fixed.dimension.values = do.resolve.dimension.values(dimension.values = private$i.fixed.dimension.values,
                                                                               aliases = aliases,
                                                                               ontology = ontology,
                                                                               unresolved.alias.names = unresolved.alias.names,
                                                                               variable.name.for.error = paste0("'dimension.values' for model.element ", 
                                                                                                                self$get.original.name(wrt.specification$version)),
                                                                               ontology.name.for.error = ontology.name.for.error,
                                                                               error.prefix = error.prefix)
            }
            else
            {
                is.char.mask = sapply(private$i.fixed.dimension.values, is.character)
                if (any(!is.char.mask))
                {
                    private$i.fixed.dimension.values[!is.char.mask] = 
                        do.resolve.dimension.values(dimension.values = private$i.fixed.dimension.values[!is.char.mask],
                                                    aliases = aliases,
                                                    ontology = ontology,
                                                    unresolved.alias.names = unresolved.alias.names,
                                                    variable.name.for.error = paste0("'dimension.values' for model.element ", 
                                                                                     self$get.original.name(wrt.specification$version)),
                                                    ontology.name.for.error = ontology.name.for.error,
                                                    error.prefix = error.prefix)
                }
            }
        },
        
        # apply to can be either a character vector of dimension names, or a dimnames list
        apply.reversed.dimension.aliases = function(apply.to)
        {
            if (length(apply.to)==0)
                apply.to
            else if (is.null(private$i.reversed.dimension.alias.mapping))
            {
                if (is.character(apply.to))
                    dimensions = apply.to
                else
                    dimensions = names(apply.to)
                
                dimensions = sapply(dimensions, function(d){
                    if (any(names(private$i.reversed.aliases)==d))
                        private$i.reversed.aliases[d]
                    else
                        d
                })
                
                if (is.character(apply.to))
                {
                    names(dimensions) = names(apply.to)
                    dimensions
                }
                else
                {
                    names(apply.to) = dimensions
                    apply.to
                }
            }
            else
            {
                if (is.character(apply.to))
                {
                    rv = private$i.reversed.dimension.alias.mapping[apply.to]
                    names(rv) = names(apply.to)
                    rv
                }
                else
                {
                    rv = apply.to
                    names(rv) = private$i.reversed.dimension.alias.mapping[names(rv)]
                    rv
                }
            }
        }
    ),
    
    active = list(
        
        descriptor = function(value)
        {
            if (missing(value))
            {
                if (private$i.type=='transition')
                    'transition'
                else if (private$i.type=='element')
                    'model element'
                else
                    'model quantity'
            }
            else
                stop(paste0("Cannot modify a ", self$descriptor, "'s 'descriptor' - it is read-only"))
        },
        
        is.element = function(value)
        {
            if (missing(value))
                private$i.type == 'element'
            else
                stop(paste0("Cannot modify a ", self$descriptor, "'s 'is.element' value - it is read-only"))
        },
        
        name = function(value)
        {
            if (missing(value))
                private$i.name
            else
                stop(paste0("Cannot modify a ", self$descriptor, "'s 'name' - it is read-only"))
        },
        
        version = function(value)
        {
            if (missing(value))
                private$i.version
            else
                stop(paste0("Cannot modify a ", self$descriptor, "'s 'version' - it is read-only"))
        },
        
        type = function(value)
        {
            if (missing(value))
                private$i.type
            else
                stop(paste0("Cannot modify a ", self$descriptor, "'s 'type' - it is read-only"))
        },
        
        scale = function(value)
        {
            if (missing(value))
                private$i.scale
            else
                stop(paste0("Cannot modify a ", self$descriptor, "'s 'scale' - it is read-only"))
        },
        
        depends.on = function(value)
        {
            if (missing(value))
                private$i.depends.on
            else
                stop(paste0("Cannot modify a ", self$descriptor, "'s 'depends.on' value - it is read-only"))
        },
        
        max.dim.names = function(value)
        {
            if (missing(value))
                private$i.maximum.dim.names
            else
                stop(paste0("Cannot modify a ", self$descriptor, "'s 'max.dim.names' value - it is read-only"))
        },
        
        dimension.aliases = function(value)
        {
            if (missing(value))
                private$i.dimension.aliases
            else
                stop(paste0("Cannot modify a ", self$descriptor, "'s 'dimension.aliases' value - it is read-only"))
        },
        
        reversed.dimension.aliases = function(value)
        {
            if (missing(value))
                private$i.reversed.aliases
            else
                stop(paste0("Cannot modify a ", self$descriptor, "'s 'reversed.dimension.aliases' value - it is read-only"))
        },
        
        reversed.dimension.alias.mapping = function(value)
        {
            if (missing(value))
                private$i.reversed.dimension.alias.mapping
            else
                stop(paste0("Cannot modify a ", self$descriptor, "'s 'reversed.dimension.alias.mapping' - they are read-only"))
        },
        
        fixed.dimensions= function(value)
        {
            if (missing(value))
                private$i.fixed.dimensions
            else
                stop(paste0("Cannot modify a ", self$descriptor, "'s 'fixed.dimensions' - they are read-only"))
        },
        
        fixed.dimension.values= function(value)
        {
            if (missing(value))
                private$i.fixed.dimension.values
            else
                stop(paste0("Cannot modify a ", self$descriptor, "'s 'fixed.dimension.values' - they are read-only"))
        },
        
        fixed.dim.names = function(value)
        {
            if (missing(value))
                private$i.fixed.dim.names
            else
                stop(paste0("Cannot modify a ", self$descriptor, "'s 'fixed.dim.names' - they are read-only"))
        }
          
    ),
    
    private = list(
        
        i.name = NULL,
        i.original.name = NULL,
        i.version = NULL,
        i.type = NULL,
        i.scale = NULL,
        
        i.depends.on = NULL,
        
        i.dimension.aliases = NULL,
        i.reversed.aliases = NULL,
        i.reversed.dimension.alias.mapping = NULL,
        
        i.maximum.dim.names = NULL,
        
        i.fixed.dimensions = NULL,
        i.fixed.dimension.values = NULL,
        i.apply.aliases.to.dimension.values = NULL,
        i.fixed.dim.names = NULL
    )
)

MODEL.ELEMENT = R6::R6Class(
    'model.element',
    portable = F,
    inherit = MODEL.QUANTITY,
    
    public = list(
        
        initialize = function(name,
                              version,
                              scale,
                              
                              dimensions = names(dimensions),
                              dimension.values = NULL,
                              apply.aliases.to.dimension.values = F,
                              
                              value=NULL,
                              get.value.function=NULL,
                              
                              functional.form=NULL,
                              get.functional.form.function=NULL,
                              functional.form.scale=scale,
                              functional.form.from.time=NULL,
                              functional.form.to.time=Inf,
                              
                              get.value.function.name=NULL,
                              get.functional.form.function.name=NULL,
                              ...,
                              
                              ramp.scale=scale,
                              ramp.times=numeric(),
                              ramp.values=numeric(),
                              ramp.value.application=c('multiplier','absolute')[1],
                              ramp.interpolate.links='identity',
                              
                              taper.scale=scale,
                              taper.times=numeric(),
                              taper.values=numeric(),
                              taper.value.application=c('multiplier','absolute')[1],
                              taper.interpolate.links='identity',
                              
                              error.prefix='')
        {
            #-- Call the superclass constructor --#
            super$initialize(name = name,
                             type = 'element',
                             version = version,
                             scale = scale,
                             dimensions = dimensions,
                             dimension.values = dimension.values,
                             apply.aliases.to.dimension.values = apply.aliases.to.dimension.values,
                             allow.scale.missing = F,
                             error.prefix = error.prefix)
            
            #-- Check model scales --#
            check.model.scale(functional.form.scale, 'functional.form.scale', error.prefix = error.prefix)
            check.model.scale(ramp.scale, 'ramp.scale', error.prefix = error.prefix)
            check.model.scale(taper.scale, 'taper.scale', error.prefix = error.prefix)
            
            #-- Plug in default values for the function names --#
            if (is.null(get.value.function.name) && !is.null(get.value.function))
            {
                get.value.function.name = deparse(substitute(get.value.function))
                if (!is.character(get.value.function.name) || length(get.value.function.name) != 1)
                    get.value.function.name = 'get.value.function'
            }
            if (is.null(get.functional.form.function.name) && !is.null(get.functional.form.function))
            {
                get.functional.form.function.name = deparse(substitute(get.functional.form.function))
                if (!is.character(get.functional.form.function.name) || length(get.functional.form.function.name) != 1)
                    get.functional.form.function.name = 'get.functional.form.function'
            }
            
            #-- Must pass either value or functional.form, or a function to get one --#
            get.value.function.wrapper = get.functional.form.function.wrapper = NULL
            if (!is.null(value) || !is.null(get.value.function)) # We are specifying value, not functional.form
            {
                if (!is.null(functional.form) || !is.null(get.functional.form.function))
                    stop(paste0(error.prefix,
                                "you cannot specify BOTH a value and a functional.form (or functions to get value/functional.form) - you must specify one or the other"))
                else if (!is.null(value)) # Using value, not get.value.function
                {
                    if (!is.null(get.value.function))
                        stop(paste0(error.prefix, "you cannot specity BOTH a value and get.value.function - one or the other must be NULL"))
                    
                    if (!is.numeric(value) || length(value)==0 || any(is.na(value)))
                        stop(paste0(error.prefix,
                                    "'value' must be a non-empty, numeric object with no NA values"))
                }
                else # Using get.value.function
                {
                    if (!is.function(get.value.function))
                        stop(paste0(error.prefix,
                                    "The value passed to 'get.value.function' ",
                                    ifelse(get.value.function.name=='get.value.function', '',
                                           paste0("('", get.value.function.name, "') ")),
                                    " is not a function"))
                    get.value.function.wrapper = prepare.specification.function.wrapper(fn = get.value.function,
                                                                                        ...,
                                                                                        fn.name.for.error = get.value.function.name,
                                                                                        require.all.arguments.up.front = T,
                                                                                        error.prefix = error.prefix)
                }
                
                if (!is.null(functional.form.from.time) && !is.infinite(functional.form.from.time))
                    stop(paste0(error.prefix, "'functional.form.from.time' cannot be set unless a functional.form is specified (either through the 'functional.form' or 'get.functional.form.function' arguments"))
                if (!is.null(functional.form.to.time) && !is.infinite(functional.form.to.time))
                    stop(paste0(error.prefix, "'functional.form.to.time' cannot be set unless a functional.form is specified (either through the 'functional.form' or 'get.functional.form.function' arguments"))
                
            }
            else # We are specifying functional.form, not value
            {
                if (!is.null(functional.form))
                {
                    if (!is.null(get.functional.form.function))
                        stop(paste0(error.prefix, "you cannot specity BOTH a functional.form and get.functional.form.function - one or the other must be NULL"))
                    
                    if (!is(functional.form, 'functional.form'))
                        stop(paste0(error.prefix, "'functional.form' must be an object of class 'functional.form'"))
                }
                else if (!is.null(get.functional.form.function))
                {
                    if (!is.function(get.functional.form.function))
                        stop(paste0(error.prefix,
                                    "The value passed to 'get.functional.form.function' ",
                                    ifelse(get.functional.form.function.name=='get.functional.form.function', '',
                                           paste0("('", get.functional.form.function, "') ")),
                                    " is not a function"))
                    
                    get.functional.form.function.wrapper = prepare.specification.function.wrapper(fn = get.functional.form.function,
                                                                                            ...,
                                                                                            fn.name.for.error = get.functional.form.function.name,
                                                                                            require.all.arguments.up.front = T,
                                                                                            error.prefix = error.prefix)
                }
                else
                    stop(paste0(error.prefix, "either 'value' or 'functional.form' (or a function to get value or functional.form) must be specified"))
            }
            
            
            #-- Check ramp --#
            if (length(ramp.times)>0)
            {
                if (is.null(functional.form) && is.null(get.functional.form.function))
                    stop(paste0(error.prefix, "a ramp can only be set up if a functional.form is specified (through the 'functional.form' or 'get.functional.form.function' arguments)"))
                
                if (!is.numeric(ramp.values))
                    stop(paste0(error.prefix, "ramp.values must be a numeric vector"))
                if (any(is.na(ramp.values)))
                    stop(paste0(error.prefix, "ramp.values cannot contain NA values"))
                
                if (length(ramp.values)==0)
                    stop(paste0(error.prefix, "ramp.times has been set, but ramp.values is empty"))
                if (length(ramp.interpolate.links)==0)
                    stop(paste0(error.prefix, "ramp.times has been set, but ramp.interpolate.links is empty"))
                if (length(ramp.value.application)==0)
                    stop(paste0(error.prefix, "ramp.times has been set, but ramp.value.application is empty"))
                
                if (!is.numeric(ramp.times))
                    stop(paste0(error.prefix, "ramp.times must be a numeric vector"))
                if (!all(ramp.times==sort(ramp.times)))
                    stop(paste0(error.prefix, "ramp.times must be in increasing order"))
                
                if (!is.character(ramp.interpolate.links) || any(is.na(ramp.interpolate.links)))
                    stop(paste0(error.prefix, "ramp.interpolate.links must be a non-NA character vector"))
                invalid.scales = setdiff(ramp.interpolate.links, RAMP.TAPER.SCALES)
                if (length(invalid.scales)>0)
                    stop(paste0(error.prefix, "invalid value(s) for ramp.interpolate.links (",
                                paste0("'", invalid.scales, "'", collapse=', '),
                                ") - values must be one of: ",
                                collapse.with.or("'", RAMP.TAPER.SCALES, "'")))
                
                if (!is.character(ramp.value.application) || length(ramp.value.application) != 1 || any(is.na(ramp.value.application)))
                    stop(paste0(error.prefix, "ramp.value.application must be a single, non-NA character value"))
                if (all(ramp.value.application != RAMP.TAPER.APPLICATIONS))
                    stop(paste0(error.prefix, "invalid value for ramp.value.application ('",
                                ramp.value.application,
                                "') - applications must be one of: ",
                                collapse.with.or("'", RAMP.TAPER.APPLICATIONS, "'")))
                
                if (length(ramp.interpolate.links)==1)
                    ramp.interpolate.links = rep(ramp.interpolate.links, length(ramp.times))
                if (length(ramp.times) != length(ramp.interpolate.links))
                    stop(paste0(error.prefix, "ramp.times and ramp.interpolate.links must have the same length OR ramp.interpolate.links must be a single value"))
                
                if (length(ramp.times) != length(ramp.values))
                    stop(paste0(error.prefix, "ramp.times and ramp.values must have the same length"))
                
                if (any(ramp.times>=functional.form.from.time))
                    stop(paste0(error.prefix, "All ramp.times must PRECEDE functional.form.start.time"))
                
                if (is.null(names(ramp.times)))
                {
                    if (!is.null(names(ramp.values)))
                        names(ramp.times) = names(ramp.values)
                }
                else
                {
                    if (is.null(names(ramp.values)))
                        names(ramp.values) = names(ramp.times)
                    else if (!all(names(ramp.values)==names(ramp.times)))
                        stop(paste0(error.prefix, "ramp.values and ramp.times must have the same names"))
                }
                
                if (!is.null(names(ramp.interpolate.links)) && 
                    !all(names(ramp.interpolate.links)==names(ramp.values)))
                    stop(paste0(error.prefix, "ramp.interpolate.links must have the same names as ramp.times and/or ramp.values"))
                
                names(ramp.interpolate.links) = names(ramp.values)
            }
            else if (length(ramp.values)>0)
                stop(paste0(error.prefix, "ramp.values has been set, but ramp.times is empty"))
            else
                ramp.values = ramp.times = NULL
            
            
            #-- Check taper --#
            if (length(taper.times)>0)
            {
                if (is.null(functional.form) && is.null(get.functional.form.function))
                    stop(paste0(error.prefix, "a taper can only be set up if a functional.form is specified (through the 'functional.form' or 'get.functional.form.function' arguments)"))
                
                if (!is.numeric(taper.values))
                    stop(paste0(error.prefix, "taper.values must be a numeric vector"))
                if (any(is.na(taper.values)))
                    stop(paste0(error.prefix, "taper.values cannot contain NA values"))
                
                if (length(taper.values)==0)
                    stop(paste0(error.prefix, "taper.times has been set, but taper.values is empty"))
                if (length(taper.interpolate.links)==0)
                    stop(paste0(error.prefix, "taper.times has been set, but taper.interpolate.links is empty"))
                if (length(taper.value.application)==0)
                    stop(paste0(error.prefix, "taper.times has been set, but taper.value.application is empty"))
                
                if (!is.numeric(taper.times))
                    stop(paste0(error.prefix, "taper.times must be a numeric vector"))
                if (!all(taper.times==sort(taper.times)))
                    stop(paste0(error.prefix, "taper.times must be in increasing order"))
                
                if (!is.character(taper.interpolate.links) || any(is.na(taper.interpolate.links)))
                    stop(paste0(error.prefix, "taper.interpolate.links must be a non-NA character vector"))
                invalid.scales = setdiff(taper.interpolate.links, RAMP.TAPER.SCALES)
                if (length(invalid.scales)>0)
                    stop(paste0(error.prefix, "invalid value(s) for taper.interpolate.links (",
                                paste0("'", invalid.scales, "'", collapse=', '),
                                ") - values must be one of: ",
                                collapse.with.or("'", RAMP.TAPER.SCALES, "'")))
                
                if (!is.character(taper.value.application) || length(taper.value.application) != 1 || any(is.na(taper.value.application)))
                    stop(paste0(error.prefix, "taper.value.application must be a single, non-NA character value"))
                if (all(taper.value.application != RAMP.TAPER.APPLICATIONS))
                    stop(paste0(error.prefix, "invalid value for taper.value.application ('",
                                taper.value.application,
                                "') - applications must be one of: ",
                                collapse.with.or("'", RAMP.TAPER.APPLICATIONS, "'")))
                
                if (length(taper.interpolate.links)==1)
                    taper.interpolate.links = rep(taper.interpolate.links, length(taper.times))
                if (length(taper.times) != length(taper.interpolate.links))
                    stop(paste0(error.prefix, "taper.times and taper.interpolate.links must have the same length OR taper.interpolate.links must be a single value"))
                
                if (length(taper.times) != length(taper.values))
                    stop(paste0(error.prefix, "taper.times and taper.values must have the same length"))
                
                if (any(ramp.times<=functional.form.to.time))
                    stop(paste0(error.prefix, "All taper.times must come AFTER functional.form.to.time"))
                
                if (is.null(names(taper.times)))
                {
                    if (!is.null(names(taper.values)))
                        names(taper.times) = names(taper.values)
                }
                else
                {
                    if (is.null(names(taper.values)))
                        names(taper.values) = names(taper.times)
                    else if (!all(names(taper.values)==names(taper.times)))
                        stop(paste0(error.prefix, "taper.values and taper.times must have the same names"))
                }
                
                if (!is.null(names(taper.interpolate.links)) && 
                    !all(names(taper.interpolate.links)==names(taper.values)))
                    stop(paste0(error.prefix, "taper.interpolate.links must have the same names as taper.times and/or taper.values"))
                
                names(taper.interpolate.links) = names(taper.values)
            }
            else if (length(taper.values)>0)
                stop(paste0(error.prefix, "taper.values has been set, but taper.times is empty"))
            else
                taper.values = taper.times = NULL
            
            
            # check functional.form times
            functional.form.times.required = (!is.null(functional.form) && !functional.form$is.static) ||
                !is.null(ramp.times) || !is.null(taper.times)
            
            if (functional.form.times.required)
            {
                if (is.null(functional.form.from.time))
                    stop(paste0(error.prefix, 
                                "'functional.form.from.time' must be set if a dynamic functional.form is specified (through the 'functional.form' or 'get.functional.form.function' arguments) or if a ramp or taper is set"))
                
                if (is.null(functional.form.to.time))
                    stop(paste0(error.prefix, 
                                "'functional.form.to.time' must be set if a functional.form is specified (through the 'functional.form' or 'get.functional.form.function' arguments) or if a ramp or taper is set"))
            }
            
            if (is.null(functional.form.from.time) && !is.null(functional.form.to.time) && is.infinite(functional.form.to.time))
                functional.form.to.time = NULL
            if (is.null(functional.form.to.time) && !is.null(functional.form.from.time) && is.infinite(functional.form.from.time))
                functional.form.from.time = NULL
            
            if (!is.null(functional.form.from.time) || !is.null(functional.form.to.time))
            {
                if (is.null(functional.form.from.time))
                    stop(paste0(error.prefix, "If 'functional.form.to.time' is non-NULL, 'functional.form.from.time' must also be non-NULL"))
                if (is.null(functional.form.to.time))
                    stop(paste0(error.prefix, "If 'functional.form.from.time' is non-NULL, 'functional.form.to.time' must also be non-NULL"))
                
                if (!is.null(functional.form) && functional.form$is.static && 
                    is.null(ramp.times) && is.null(taper.times) &&
                    !suppress.warnings)
                    print(paste0(gsub('error', 'WARNING', error.prefix, ignore.case = T),
                                 "You are using a static functional.form without a ramp or taper. 'functional.form.from.time' and 'functional.form.to.time' will be ignored"))
                
                if (!is.numeric(functional.form.from.time) || length(functional.form.from.time)!=1 || is.na(functional.form.from.time))
                    stop(paste0(error.prefix, "'functional.form.from.time' must be a single, non-NA, numeric value"))
                
                current.year = as.numeric(format(Sys.Date(), "%Y"))
                if (functional.form.from.time < MIN.FUNCTIONAL.FORM.FROM.YEAR || functional.form.from.time > (current.year+MAX.FUNCTIONAL.FORM.FROM.YEAR.OFFSET.FROM.CURRENT.YEAR))
                    stop(paste0(error.prefix, "'functional.form.from.time' (", functional.form.from.time, ") should be between ",
                                MIN.FUNCTIONAL.FORM.FROM.YEAR,
                                " and ", (current.year+MAX.FUNCTIONAL.FORM.FROM.YEAR.OFFSET.FROM.CURRENT.YEAR)))
                
                if (!is.numeric(functional.form.to.time) || length(functional.form.to.time)!=1 || is.na(functional.form.to.time))
                    stop(paste0(error.prefix, "'functional.form.to.time' must be a single, non-NA, numeric value"))
                
                if (functional.form.to.time < functional.form.from.time)
                    stop(paste0(error.prefix, "'functional.form.to.time' (",
                                functional.form.to.time,
                                ") should be greater than 'functional.form.from.time' (",
                                functional.form.from.time, ")"))
            }
            else
            {
                functional.form.from.time = functional.form.to.time = NULL
            } 
            
            #-- Clear names from scalar values --#
            if (!is.null(value) && is.null(dim(value)) && length(value)==1)
                names(value) = NULL
            
            #-- Make the object and return --#
            
            private$i.value = value
            private$i.get.value.function.wrapper = get.value.function.wrapper
                
            private$i.functional.form = functional.form
            private$i.get.functional.form.function.wrapper = get.functional.form.function.wrapper
            private$i.functional.form.scale=functional.form.scale
            private$i.functional.form.from.time=functional.form.from.time
            private$i.functional.form.to.time=functional.form.to.time
            
            private$i.get.value.function.name = get.value.function.name
            private$i.get.functional.form.function.name = get.functional.form.function.name
                
            private$i.ramp.scale = ramp.scale
            private$i.ramp.times = ramp.times
            private$i.ramp.values = ramp.values
            private$i.ramp.value.application = ramp.value.application
            private$i.all.ramp.applications.identity = all(private$i.ramp.value.application=='identity')
            private$i.ramp.interpolate.links=ramp.interpolate.links
                
            private$i.taper.scale = taper.scale
            private$i.taper.times = taper.times
            private$i.taper.values = taper.values
            private$i.taper.value.application = taper.value.application
            private$i.all.taper.applications.identity = all(private$i.taper.value.application=='identity')
            private$i.taper.interpolate.links=taper.interpolate.links
        },
        
        resolve.compartment.values = function(aliases, 
                                               ontology,
                                               unresolved.alias.names,
                                               ontology.name.for.error,
                                               error.prefix,
                                               wrt.specification)
        {   
            super$resolve.compartment.values(aliases = aliases,
                                             ontology = ontology,
                                             unresolved.alias.names = unresolved.alias.names,
                                             ontology.name.for.error = ontology.name.for.error,
                                             error.prefix = error.prefix,
                                             wrt.specification = wrt.specification)
            
            # Apply to value (if set)
            if (!is.null(private$i.value))
            {
                sub.error.prefix = paste0(error.prefix,
                                          "Error creating an ontology.mapping based off of aliases for the value of model.quantity ",
                                          self$get.original.name(wrt.specification$version))
                ontology.mapping = create.ontology.mapping.from.aliases(dim.names = dimnames(private$i.value),
                                                                        aliases = aliases,
                                                                        error.prefix = sub.error.prefix)
                if (!is.null(ontology.mapping))
                {   
                    sub.error.prefix = paste0(error.prefix,
                                              "Error applying ontology.mapping based off of aliases to the value of model.quantity ",
                                              self$get.original.name(wrt.specification$version))
                    private$i.value = ontology.mapping$apply(private$i.value, error.prefix=sub.error.prefix)
                }
            }
            
            # Apply to functional.form (if set)
            if (!is.null(private$i.functional.form))
            {
                sub.error.prefix = paste0(error.prefix,
                                          "Error creating an ontology.mapping based off of aliases for the functional.form of model.quantity ",
                                          self$get.original.name(wrt.specification$version))
                ontology.mapping = create.ontology.mapping.from.aliases(dim.names = private$i.functional.form$minimum.dim.names,
                                                                        aliases = aliases,
                                                                        error.prefix = sub.error.prefix)
                
                if (!is.null(ontology.mapping))
                {
                    sub.error.prefix = paste0(error.prefix,
                                              "Error applying ontology.mapping based off of aliases to the functional.form of model.quantity ",
                                              self$get.original.name(wrt.specification$version))
                    
                    private$i.functional.form = private$i.functional.form$apply.ontology.mapping(ontology.mapping, 
                                                                                                 modify.in.place=F,
                                                                                                 error.prefix=sub.error.prefix)
                }
            }
            
            invisible(self)
        },
        
        compile = function()
        {
            rv = self$clone(deep=T)
            class(rv) = NULL
            rv
        },
        
        rename.depends.on = function(mapping)
        {
            # nothing to do for a model element, since it does not depend on anything
        },
        
        verify.dim.names = function(error.prefix, wrt.specification)
        {
            # Check value if present
            if (!is.null(private$i.value))
                verify.dim.names.for.quantity(dim.names = dimnames(private$i.value),
                                              quantity = self,
                                              variable.name.for.error = "the dimnames of 'value'",
                                              error.prefix = error.prefix,
                                              wrt.version = wrt.specification$version)

            # Check functional form if present
            if (!is.null(private$i.functional.form))
                verify.dim.names.for.quantity(dim.names = private$i.functional.form$minimum.dim.names,
                                              quantity = self,
                                              variable.name.for.error = "the minimum dimnames of the functional.form",
                                              error.prefix = error.prefix,
                                              wrt.version = wrt.specification$version)
        },
        
        get.element.background = function(specification.info,
                                          error.prefix)
        {
            value = private$i.value
            if (!is.null(private$i.get.value.function.wrapper))
            {
                # get the value
                tryCatch({
                    value = private$i.get.value.function.wrapper$execute(location=specification.info$location, 
                                                                         specification.info=specification.info)
                },
                error = function(e){
                    e$message = paste0(error.prefix, "There was an error evaluating ", private$i.get.value.function,
                                "()",
                                ifelse(private$i.get.value.function.name=='get.value.function', '',
                                       " (the get.value.function)"),
                                " for model element ",
                                self$get.original.name(wrt.version = specification.info$version), ": ",
                                e$message)
                
                    stop(e)
                })
                
                # make sure it is numeric, non-empty, non-NA
                if (!is.numeric(value))
                    stop(paste0(error.prefix, "Evaluating get.value.function() for model element ",
                                private$get.original.name(wrt.version = specification.info$version),
                                " yields a non-numeric value"))
                if (length(value)==0)
                    stop(paste0(error.prefix, "Evaluating get.value.function() for model element ",
                                private$get.original.name(wrt.version = specification.info$version),
                                " yields an empty (length-zero) value"))
                if (any(is.na(value)))
                    stop(paste0(error.prefix, "Evaluating get.value.function() for model element ",
                                private$get.original.name(wrt.version = specification.info$version),
                                " yields NAs"))
                
                # make sure it accords with the expected max.dim.names
                verify.dim.names.for.quantity(dim.names = dimnames(value),
                                              quantity = self,
                                              variable.name.for.error = "the value generated by get.value.function()",
                                              error.prefix = error.prefix,
                                              wrt.version = specification$version)
            }
            
            functional.form = private$i.functional.form
            if (!is.null(private$i.get.functional.form.function.wrapper))
            {
                # get the functional.form
                tryCatch({
                    functional.form = private$i.get.functional.form.function.wrapper$execute(location=specification.info$location, 
                                                                                             specification.info=specification.info)
                },
                error = function(e){
                    e$message = paste0(error.prefix, "There was an error evaluating ",
                                       private$i.get.functional.form.function.name, "()",
                                       ifelse(private$i.get.functional.form.function.name=='get.functional.form.function', '',
                                              " (the get.functional.form.function)"),
                                       " for model element ",
                                       self$get.original.name(wrt.version = specification.info$version), ": ",
                                       e$message)
                    stop(e)
                })
                
                # make sure it is a functional form object
                if (!is(functional.form, 'functional.form'))
                    stop(paste0(error.prefix, "Evaluating get.functional.form.function() for model element ",
                                private$get.original.name(wrt.version = specification.info$version),
                                " yields a value that is NOT an object of class 'functional.form'"))
                    
                # make sure it's minimum.dim.names accords with the expected max.dim.names
                verify.dim.names.for.quantity(dim.names = functional.form$minimum.dim.names,
                                              quantity = self,
                                              variable.name.for.error = "the minimum dimnames of the functional.form generated by get.functional.form.function()",
                                              error.prefix = error.prefix,
                                              wrt.version = specification.info$version)
                
                # If no from/to times have been set, then make sure this is a static model
                if (!functional.form$is.static &&
                    (is.null(private$i.functional.form.from.time) || is.null(private$i.functional.form.to.time)))
                {
                    stop(paste0(error.prefix, "Evaluating get.functional.form.function() for model element ",
                                private$get.original.name(wrt.version = specification.info$version),
                                " yields a functional.form that is NOT static. However, no functional.form.from.time or functional.form.to.time were set for the element. These must either be set, or get.functional.form.function() must return a static functional form"))
                }
            }
            
            rv = list(
                name = private$i.name,
                value = value,
                
                functional.form = functional.form,
                functional.form.from.time = private$i.functional.form.from.time,
                functional.form.to.time = private$i.functional.form.to.time,
                
                ramp.times = private$i.ramp.times,
                ramp.values = private$i.ramp.values,
                
                taper.times = private$i.taper.times,
                taper.values = private$i.taper.values
            )
            
            rv$is.static = !is.null(rv$value) ||
                (rv$functional.form$is.static && is.null(rv$ramp.times) && is.null(rv$taper.times))
            
            
            rv
        },
        
        calculate.ramp.values = function(ramp.values,
                                         ramp.times,
                                         first.functional.form.value,
                                         functional.form.from.time)
        {
            times = ramp.times
            if (i.ramp.value.application=='absolute')
            {
                if (length(dim(first.functional.form.value))==0)
                    rv = as.list(ramp.values)
                else
                    rv = lapply(ramp.values, function(val){
                        array(val, dim=dim(first.functional.form.value), dimnames=dimnames(first.functional.form.value))
                    })
            }
            else
            {
                if (i.all.ramp.applications.identity)
                {
                    rv = lapply(ramp.values, function(val){
                        val * first.functional.form.value
                    })
                }
                else
                {
                    n.segments = length(ramp.times)
                    times = c(ramp.times, functional.form.from.time)
                    
                    multipliers = unlist(sapply(1:n.segments, function(i){
                        interpolate.times = times[i]:times[i+1]
                        interpolate.times = interpolate.times[-length(interpolate.times)]
                        
                        if (private$i.ramp.interpolate.links[i]=='log')
                            exp(interpolate(values=log(ramp.values[i:(i+1)]),
                                            value.times=times[i:(i+1)],
                                            desired.times = interpolate.times))
                        else if (private$i.ramp.interpolate.links[i]=='exp')
                            log(interpolate(values=exp(ramp.values[i:(i+1)]),
                                            value.times=times[i:(i+1)],
                                            desired.times = interpolate.times))
                        else #identity
                            ramp.values[i]
                    }))
                    
                    rv = lapply(multipliers, function(mult){
                        mult * first.functional.form.value
                    })
                    times = calculate.ramp.interpolated.times(ramp.times, functional.form.from.time)
                }
            }
            
            names(rv) = as.character(times)
            rv
        },
        
        calculate.taper.value = function(taper.values,
                                         taper.times,
                                         last.functional.form.value,
                                         functional.form.to.time)
        {
            times = taper.times
            if (i.taper.value.application=='absolute')
            {
                if (length(dim(first.functional.form.value))==0)
                    rv = as.list(taper.values)
                else
                    rv = lapply(taper.values, function(val){
                        array(val, dim=dim(last.functional.form.value), dimnames=dimnames(last.functional.form.value))
                    })
            }
            else
            {
                if (i.all.taper.applications.identity)
                {
                    rv = lapply(taper.values, function(val){
                        val * last.functional.form.value
                    })
                }
                else
                {
                    n.segments = length(taper.times)
                    times = c(functional.form.to.time, taper.times)
                    
                    multipliers = unlist(sapply(1:n.segments, function(i){
                        interpolate.times = times[i+1]:times[i]
                        interpolate.times = rev(interpolate.times[-1])
                        
                        if (private$i.taper.interpolate.links[i]=='log')
                            exp(interpolate(values=log(taper.values[i:(i+1)]),
                                            value.times=times[i:(i+1)],
                                            desired.times = interpolate.times))
                        else if (private$i.taper.interpolate.links[i]=='exp')
                            log(interpolate(values=exp(taper.values[i:(i+1)]),
                                            value.times=times[i:(i+1)],
                                            desired.times = interpolate.times))
                        else #identity
                            taper.values[i]
                    }))
                    
                    rv = lapply(multipliers, function(mult){
                        mult * last.functional.form.value
                    })
                    times = calculate.taper.interpolated.times(taper.times, functiona.form.to.time)
                }
            }
            
            names(rv) = as.character(times)
            rv
        },
        
        calculate.ramp.interpolated.times = function(ramp.times,
                                                     functional.form.from.time)
        {
            if (i.all.ramp.applications.identity)
                ramp.times
            else
            {
                times = c(ramp.times, functional.form.from.time)
                n.segments = length(ramp.times)
                unlist(sapply(1:n.segments, function(i){
                    if (private$i.ramp.interpolate.links[i]=='identity')
                        times[i]
                    else #not identity
                    {
                        rv = times[i]:times[i+1]
                        rv[-length(rv)]
                    }
                }))
            }
        },
        
        calculate.taper.interpolated.times = function(taper.times,
                                         functional.form.to.time)
        {
            if (i.all.taper.applications.identity)
                taper.times
            else
            {
                times = c(functional.form.to.time, taper.times)
                n.segments = length(taper.times)
                unlist(sapply(1:n.segments, function(i){
                    if (private$i.taper.interpolate.links[i]=='identity')
                        times[i]
                    else #not identity
                    {
                        rv = times[i+1]:times[i]
                        rev(rv[-1])
                    }
                }))
            }
        }
    ),

    active = list(
        
        functional.form.scale = function(value)
        {
            if (missing(value))
                private$i.functional.form.scale
            else
                stop("Cannot modify a model.quantity's 'functional.form.scale' value - it is read-only")
        },
        
        ramp.scale = function(value)
        {
            if (missing(value))
                length(private$i.ramp.scale)
            else
                stop("Cannot modify a model.quantity's 'ramp.scale' value - it is read-only")
        },
        
        taper.scale = function(value)
        {
            if (missing(value))
                length(private$i.taper.scale)
            else
                stop("Cannot modify a model.quantity's 'taper.scale' value - it is read-only")
        }
        
    ),
        
    private = list(
        
        i.value = NULL,
        i.get.value.function.wrapper = NULL,
        
        i.functional.form = NULL,
        i.get.functional.form.function.wrapper = NULL,
        i.functional.form.scale= NULL,
        i.functional.form.from.time = NULL,
        i.functional.form.to.time = NULL,
        
        i.get.value.function.name = NULL,
        i.get.functional.form.function.name = NULL,
        
        i.ramp.scale = NULL,
        i.ramp.times = NULL,
        i.ramp.values = NULL,
        i.ramp.value.application = NULL,
        i.all.ramp.applications.identity = NULL,
        i.ramp.interpolate.links = NULL,
        
        i.taper.scale = NULL,
        i.taper.times = NULL,
        i.taper.values = NULL,
        i.taper.value.application = NULL,
        i.all.taper.applications.identity = NULL,
        i.taper.interpolate.links = NULL
    )
)



NON.TERMINAL.MODEL.QUANTITY = R6::R6Class(
    'non.terminal.model.quantity',
    inherit = MODEL.QUANTITY,
    portable = F,
    
    public = list(
        
        check=1,
        initialize = function(name,
                              version,
                              value,
                              value.name,
                              is.transition=F,
                              scale=NULL,
                              dimensions=names(dimension.values),
                              dimension.values=NULL,
                              apply.aliases.to.dimension.values=F,
                              na.replacement=as.numeric(NA),
                              ...,
                              error.prefix)
        {   
            if (!is.logical(is.transition) || length(is.transition)!=1 || is.na(is.transition))
                stop(paste0(error.prefix,
                            "'is.transition' for creating a non-terminal model quantity must be a single, non-NA logical value"))
            
            if (is.transition)
                type = 'transition'
            else
                type = 'non.terminal.quantity'
            
            #-- Call the super-class constructor --#
            super$initialize(name = name,
                             type = type,
                             version = version,
                             scale = scale,
                             allow.scale.missing = T,
                             dimensions = dimensions,
                             dimension.values = dimension.values,
                             apply.aliases.to.dimension.values = apply.aliases.to.dimension.values,
                             error.prefix = error.prefix)
            
            #-- Make the first component and add it --#
            first.component = MODEL.QUANTITY.COMPONENT$new(value = value,
                                                           applies.to = NULL,
                                                           allow.empty.applies.to = T,
                                                           apply.function = NULL,
                                                           na.replacement = na.replacement,
                                                           value.name = value.name,
                                                           ...,
                                                           parent.quantity = self,
                                                           error.prefix = error.prefix)
        
            private$i.components = list()
        
            self$add.component(comp = first.component,
                               error.prefix=error.prefix)
            
            #-- If value.type of first component is a function, names(dimension.values) must == dimensions --#
            if (first.component$value.type=='function')
            {
                missing.dimension.values = setdiff(private$i.dimensions,
                                                   names(private$i.dimension.values))
                
                if (length(missing.dimension.values)>0)
                    stop(paste0(error.prefix,
                                "If 'value' is a function, then all dimensions specified in the 'dimensions' argument must also have their values specified in the 'dimension.values' argument. ",
                                collapse.with.and("'", missing.dimension.values, "'"),
                                ifelse(length(missing.dimension.values)==1, " is", " are"),
                                " not present in 'dimension.values'"))
            }
            
            #-- Invisibly Return --#
            invisible(self)
        },
        
        add.component = function(comp,
                                 error.prefix)
        {
            if (!is(comp, 'model.quantity.component'))
                stop(paste0(error.prefix, "'comp' must be an object of class 'model.quantity.component'"))
            
            private$i.components = c(private$i.components, list(comp))
            
            self$calculate.depends.on()
            invisible(self)
        },
        
        # This is just a dummy method for now for the components to call
        #  The real method will be implemented by the subclass
        calculate.depends.on = function()
        {
            private$i.depends.on = unique(as.character(unlist(sapply(private$i.components, function(comp){
                comp$depends.on
            }))))
            
            invisible(self)
        },
        
        compile = function()
        {
            rv = self$clone(deep=T)
            rv$compile.components()
            
            class(rv) = NULL
            rv
        },
        
        compile.components = function()
        {
            self$check = 2
            private$i.components = lapply(private$i.components, function(comp){
                comp$compile(parent.quantity=self)
            })
            
            invisible(self)
        },
        
        resolve.compartment.values = function(aliases, 
                                              ontology,
                                              unresolved.alias.names,
                                              ontology.name.for.error,
                                              error.prefix,
                                              wrt.specification)
        {
            super$resolve.compartment.values(aliases = aliases,
                                             ontology = ontology,
                                             unresolved.alias.names = unresolved.alias.names,
                                             ontology.name.for.error = ontology.name.for.error,
                                             error.prefix = error.prefix,
                                             wrt.specification = wrt.specification)
            
            #apply to each component
            private$i.components = lapply(1:length(private$i.components), function(i){
                private$i.components[[i]]$resolve.compartment.values(aliases, 
                                                                     ontology = ontology,
                                                                     unresolved.alias.names = unresolved.alias.names,
                                                                     component.index = i,
                                                                     ontology.name.for.error = ontology.name.for.error,
                                                                     error.prefix = error.prefix,
                                                                     wrt.specification = wrt.specification)
            })
            
            # Make sure components' applies.to do not overlap
            if (length(private$i.components)>2)
            {
                sapply(2:(length(private$i.components)-1), function(i){
                    comp1 = private$i.components[[i]]
                    
                    sapply((i+1):length(private$i.components), function(j){
                        comp2 = private$i.components[[j]]
                        
                        if (dimension.values.overlap(comp1$applies.to, comp2$applies.to))
                            stop(paste0(error.prefix,
                                        "After substituting in aliases, the ",
                                        get.ordinal(i-1), " and ", get.ordinal(j-1),
                                        " subsets of model.quantity ", 
                                        self$get.original.name(wrt.specification$version),
                                        " have overlapping applies.to values"))
                    })
                })
            }
            
            # Return
            invisible(self)
        },
        
        rename.depends.on = function(mapping)
        {
            for (comp in private$i.components)
                comp$rename.depends.on(mapping)
        },
        
        verify.dim.names = function(error.prefix, wrt.specification)
        {
            for (i in 1:length(private$i.components))
                private$i.components[[i]]$verify.dim.names(component.index=i, 
                                                           error.prefix=error.prefix,
                                                           wrt.specification=wrt.specification)
        }
    ),

    active = list(
        n.components = function(value)
        {
            if (missing(value))
                length(private$i.components)
            else
                stop("Cannot modify a model.quantity's 'n.components' value - it is read-only")
        },
        
        components = function(value)
        {
            if (missing(value))
                private$i.components
            else
                stop("Cannot modify a model.quantity's 'components' - they are read-only")
        }
    ),
        
    private = list(
        
        i.components = NULL
    )
)

MODEL.QUANTITY.COMPONENT = R6::R6Class(
    'model.quantity.component',
    inherit = EVALUATABLE.VALUE,
    portable = F,
    
    public = list(
        
        initialize = function(value,
                              applies.to,
                              allow.empty.applies.to,
                              na.replacement,
                              apply.function,
                              ...,
                              value.name,
                              error.prefix,
                              parent.quantity)
        {
            #-- Call the superclass constructor --#
            super$initialize(na.replacement = na.replacement,
                             allow.numeric.value = T,
                             allow.character.value = T,
                             allow.expression.value = T,
                             allow.function.value = T,
                             allowed.expression.functions = ALLOWED.MODEL.QUANTITY.VALUE.EXPRESSION.FUNCTIONS,
                             function.arguments.to.be.supplied.later = c('specification.info', 'location'),
                             ...,
                             error.prefix = error.prefix)
            
            #-- Check parent.quantity --#
      
            if (!is(parent.quantity, 'non.terminal.model.quantity'))
                stop(paste0(error.prefix, "'parent.quantity' must be an object of class 'non.terminal.model.quantity'"))

            #-- Validate applies.to --#
            if (allow.empty.applies.to)
            {
                if (length(applies.to)==0)
                    applies.to = NULL
                else
                    validate.applies.to(applies.to, error.prefix=error.prefix, variable.name.for.error='applies.to')
            }
            else if (length(applies.to)==0)
            {
                stop(paste0(error.prefix, "'applies.to' must be a non-empty named list"))
                validate.applies.to(applies.to, error.prefix=error.prefix, variable.name.for.error='applies.to')
            }
            
            #-- Validate apply.function --#
            if (allow.empty.applies.to)
            {
                if (length(apply.function)==0)
                    apply.function = NULL
            }
            
            if (!allow.empty.applies.to || !is.null(apply.function))
            {
                if (!is.character(apply.function))
                    stop(paste0(error.prefix, "'apply.function' must be a character value"))
                if (length(apply.function)!=1)
                    stop(paste0(error.prefix, "'apply.function' must be a SINGLE character value"))
                if (is.na(apply.function))
                    stop(paste0(error.prefix, "'apply.function' cannot be NA"))
                if (all(apply.function != QUANTITY.SUBSET.APPLY.FUNCTIONS))
                    stop(paste0(error.prefix, "Invalid apply.function '", apply.function, 
                                "'. Must be one of ",
                                collapse.with.or("'", QUANTITY.SUBSET.APPLY.FUNCTIONS, "'")))
            }
            
            
            #-- Store Values --#
            private$i.applies.to = applies.to
            private$i.apply.function = apply.function
            private$i.parent.quantity = parent.quantity
            
            #-- Set Value --#
            self$set.value(value, value.name=value.name, error.prefix=error.prefix)
            
            #-- Make sure all the arguments to the function are valid quantity names --#
            if (private$i.value.type=='function' && length(private$i.depends.on)>0)
            {
                non.reserved.depends.on = setdiff(private$i.depends.on, c('specification.info','location'))
                if (length(non.reserved.depends.on)>0)
                {
                    if (private$i.parent.quantity$n.components==0)
                        descriptor.for.error = paste0("model.quantity '", private$i.parent.quantity$name, "'")
                    else
                        descriptor.for.error = paste0("the ", get.ordinal(private$i.parent.quantity$n.components),
                                                         " subset of model.quantity '", private$i.parent.quantity$name, "'")
                    
                    do.validate.names.or.values(values = non.reserved.depends.on,
                                                descriptor = descriptor.for.error,
                                                descriptor.plural = descriptor.for.error,
                                                variable.name.for.error = 'arguments to the function value',
                                                error.prefix = error.prefix,
                                                reserved.values = RESERVED.QUANTITY.NAMES,
                                                reserved.prefixes = RESERVED.QUANTITY.NAME.PREFIXES,
                                                reserved.postfixes = NULL,
                                                reserved.infixes = RESERVED.INFIXES,
                                                is.single.value = T)
                }
            }
        },
        
        set.value = function(value, ..., value.name=NULL, error.prefix)
        {
            #-- Get value.name --#
            if (is.null(value.name))
            {
                if (is.null(private$i.value.name))
                    value.name = 'value'
                else
                    value.name = private$i.value.name
            }
            
            #-- Call the superclass method --#
            super$set.value(value, ..., value.name=value.name, error.prefix=error.prefix)
            
            #-- Update the parent quantity's depends.on --#
            private$i.parent.quantity$calculate.depends.on()
            
            #-- Return Self --#
            invisible(self)
        },
        
        rename.depends.on = function(mapping)
        {
            if (private$i.value.type=='character')
            {
                if (any(names(mapping)==private$i.value))
                    self$set.value(mapping[private$i.value])
            }
            else if (private$i.value.type=='expression')
            {
                if (length(intersect(names(mapping), private$i.depends.on))>0)
                    self$set.value(rename.expression.vars(expr = private$i.value,
                                                          new.names = mapping))
            }
        },
        
        compile = function(parent.quantity)
        {
            rv = self$clone(deep=T)
            rv$set.parent(parent.quantity)
            class(rv) = NULL
            
            rv
        },
        
        set.parent = function(parent.quantity)
        {
            if (!is(parent.quantity, 'non.terminal.model.quantity'))
                stop("The 'parent.quantity' of a model.quantity.component must be an object of class 'non.terminal.model.quantity'")
            private$i.parent.quantity = parent.quantity
            
            invisible(self)
        },
        
        resolve.compartment.values = function(aliases, 
                                              ontology,
                                              unresolved.alias.names,
                                              component.index,
                                              ontology.name.for.error,
                                              error.prefix,
                                              wrt.specification)
        {
            private$i.applies.to = do.resolve.dimension.values(dimension.values = private$i.applies.to,
                                                               aliases = aliases,
                                                               ontology = ontology,
                                                               unresolved.alias.names = unresolved.alias.names,
                                                               variable.name.for.error = paste0("'applies.to' for the ",
                                                                                                get.ordinal(component.index),
                                                                                                " subset of model.quantity ", 
                                                                                                private$i.parent.quantity$get.original.name(wrt.specification$version)),
                                                               ontology.name.for.error = ontology.name.for.error,
                                                               error.prefix = error.prefix)
            
            if (private$i.value.type=='numeric')
            {
                if (component.index==1)
                    sub.error.prefix = paste0(error.prefix,
                                              "Error creating an ontology.mapping based off of aliases to model.quantity ",
                                              private$i.parent.quantity$get.original.name(wrt.specification$version))
                else
                    sub.error.prefix = paste0(error.prefix,
                                              "Error creating an ontology.mapping based off of aliases to the ",
                                              get.ordinal(component.index-1), " subset of model.quantity ",
                                              private$i.parent.quantity$get.original.name(wrt.specification$version))
                
                ontology.mapping = create.ontology.mapping.from.aliases(dim.names = dimnames(private$i.value),
                                                                        aliases = aliases,
                                                                        error.prefix = sub.error.prefix)
                if (!is.null(ontology.mapping))
                {
                    if (component.index==1)
                        sub.error.prefix = paste0(error.prefix,
                                              "Error applying ontology.mapping based off of aliases to model.quantity ",
                                              private$i.parent.quantity$get.original.name(wrt.specification$version))
                    else
                        sub.error.prefix = paste0(error.prefix,
                                              "Error applying ontology.mapping based off of aliases to the ",
                                              get.ordinal(component.index-1), " subset of model.quantity ",
                                              private$i.parent.quantity$get.original.name(wrt.specification$version))
                    
                    private$i.value = ontology.mapping$apply(private$i.value, error.prefix=sub.error.prefix)
                }
            }
            
            self
        },
        
        verify.dim.names = function(component.index,
                                    error.prefix, 
                                    wrt.specification)
        {
            # Check value if present
            if (private$i.value.type=='numeric')
            {
                if (component.index==1)
                    variable.name.for.error = "the dimnames of the value"
                else
                    variable.name.for.error = paste0("the dimnames of the value of the ", get.ordinal(component.index-1), " subset")
                
                verify.dim.names.for.quantity(dim.names = dimnames(private$i.value),
                                              quantity = private$i.parent.quantity,
                                              variable.name.for.error = variable.name.for.error,
                                              error.prefix = error.prefix,
                                              wrt.version = wrt.specification$version)
            }
        },
        
        # apply to can be either a character vector of dimension names, or a dimnames list
        apply.reversed.dimension.aliases = function(apply.to)
        {
            private$i.parent.quantity$apply.reversed.dimension.aliases(apply.to)
        }
    ),
    
    active = list(
        
        # override the superclass method with a more descriptive error
        depends.on = function(value)
        {
            if (missing(value))
                private$i.depends.on
            else
                stop("Cannot modify a model.quantity.component's 'depends.on' value - it is read-only")
        },
        
        applies.to = function(value)
        {
            if (missing(value))
                private$i.applies.to
            else
                stop("Cannot modify a model.quantity.component's 'applies.to' value - it is read-only")
        },
        
        na.replacement = function(value)
        {
            if (missing(value))
                private$i.na.replacement
            else
                stop("Cannot modify a model.quantity.component's 'na.replacement' value - it is read-only")
        },
        
        value.type = function(value)
        {
            if (missing(value))
                private$i.value.type
            else
                stop("Cannot modify a model.quantity.component's 'value.type' value - it is read-only")
        },
        
        value = function(value)
        {
            if (missing(value))
                private$i.value
            else
                stop("Cannot modify a model.quantity.component's 'value' - it is read-only")
        },
        
        max.dim.names = function(value)
        {
            if (missing(value))
            {
                rv = private$i.parent.quantity$max.dim.names
                rv[names(private$i.applies.to)] = private$i.applies.to
                rv
            }
            else
                stop("Cannot modify a model.quantity.component's 'max.dim.names' - it is read-only")
        },
        
        # The name of the alias in the name of the dimension IN THIS QUANTITY
        # The value of the alias is the name that we COULD USE
        dimension.aliases = function(value)
        {
            if (missing(value))
                private$i.parent.quantity$dimension.aliases
            else
                stop("Cannot modify a model.quantity.component's 'dimension.aliases' - it is read-only")
        },
        
        reversed.dimension.aliases = function(value)
        {
            if (missing(value))
                private$i.parent.quantity$reversed.dimension.aliases
            else
                stop("Cannot modify a model.quantity.component's 'reversed.dimension.aliases' - it is read-only")
        },
        
        reversed.dimension.alias.mapping = function(value)
        {
            if (missing(value))
                private$i.parent.quantity$reversed.dimension.alias.mapping
            else
                stop("Cannot modify a model.quantity.component's 'reversed.dimension.alias.mapping' - it is read-only")
        },
        
        parent.quantity = function(value)
        {
            if (missing(value))
                private$i.parent.quantity
            else
                stop("Cannot modify a model.quantity.component's 'parent.quantity' - it is read-only")
        },
        
        apply.function = function(value)
        {
            if (missing(value))
                private$i.apply.function
            else
                stop("Cannot modify a model.quantity.component's 'apply.function' - it is read-only")
        }
    ),
    
    private = list(
        
        i.applies.to = NULL,
        i.apply.function = NULL,
        i.parent.quantity = NULL
    )
)

##-----------------------------------------##
##-----------------------------------------##
##-- TOP-LEVEL REFERENCE CLASS HIERARCHY --##
##-----------------------------------------##
##-----------------------------------------##

TOP.LEVEL.REFERENCE = R6::R6Class(
    'top.level.reference',
    portable = F,
    
    public = list(
        
        initialize = function(name,
                              specification,
                              ontology.name,
                              value.quantity.name,
                              type='top.level.reference',
                              alias.suffix)
        {
            if (!is.null(name) &&
                (!is.character(name) || length(name)!=1 || is.na(name) || nchar(name)==0))
                stop(paste0("If 'name' is specified for a ", self$descriptor, " it must be a single, non-NA, non-empty character value"))

#            if (!is.character(version) || length(version)!=1 || is.na(version) || nchar(version)==0)
 #               stop("'version' for a ", self$descriptor, " must be a single, non-NA, non-empty character value")
  
            if (!is.character(ontology.name) || length(ontology.name)!=1 || is.na(ontology.name) || nchar(ontology.name)==0)
                stop(paste0("'ontology.name' for a ", self$descriptor, " must be a single, non-NA, non-empty character value"))
            
            if (is.null(names(ontology.name)) || is.na(names(ontology.name)) || nchar(names(ontology.name))==0)
                names(ontology.name) = ontology.name
            
            if (all(names(ontology.name)!=names(specification$ontologies)))
                stop(paste0("In creating a ", self$descriptor, " names(ontology.name) must be one of ",
                            collapse.with.or("'", names(specification$ontologies), "'"),
                            ". ", names(ontology.name), 
                            " is not a valid name"))
            
            if (!is.character(value.quantity.name) || length(value.quantity.name)!=1 || is.na(value.quantity.name) || nchar(value.quantity.name)==0)
                stop(paste0("'value.quantity.name' for a ", self$descriptor, " must be a single, non-NA, non-empty character value"))
            
            if (!is.character(type) || length(type)!=1 || is.na(type) || nchar(type)==0)
                stop(paste0("'type' for a ", self$descriptor, " must be a single, non-NA, non-empty character value"))
            
            if (!is.null(alias.suffix))
            {
                if (!is.character(alias.suffix) || length(alias.suffix)!=1 || is.na(alias.suffix) ||
                    (alias.suffix != 'from' && alias.suffix != 'to'))
                    stop(paste0("If it is not NULL, 'alias.suffix' for a ", self$descriptor,
                                " must be a single, non-NA character value that is either 'from' or 'to'"))
            }
            
            private$i.name = name
            private$i.version = specification$version
            private$i.ontology.name = ontology.name
            private$i.value.quantity.name = value.quantity.name
            private$i.type = type
            private$i.alias.suffix = alias.suffix
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

        get.max.dim.names = function(specification)
        {
            specification$ontologies[[private$i.ontology.name]]
        },

        get.dimension.aliases = function(specification)
        {
            if (is.null(private$i.alias.suffix))
                character()
            else
            {
                rv = names(self$get.max.dim.names(specification))
                names(rv) = rv
                
                has.suffix.mask = substr(rv, nchar(rv)-nchar(private$i.alias.suffix)+1, nchar(rv)) == private$i.alias.suffix
                rv[has.suffix.mask] = substr(rv[has.suffix.mask], 1, nchar(rv[has.suffix.mask])-nchar(private$i.alias.suffix)-1)
                
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
        
        name = function(value)
        {
            if (missing(value))
                private$i.name
            else
                stop(paste0("Cannot modify a ", self$descriptor, "'s 'name' - it is read-only"))
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
        
        value.quantity.name = function(value)
        {
            if (missing(value))
                private$i.value.quantity.name
            else
                stop(paste0("Cannot modify a ", self$descriptor, "'s 'value.quantity.name' - it is read-only"))
        }
    ),
    
    private = list(
        i.name = NULL,
        i.version = NULL,
        i.value.quantity.name = NULL,
        i.ontology.name = NULL,
        i.type = NULL,
        i.alias.suffix = NULL
    )
)

TRANSITION.REFERENCE = R6::R6Class(
    'transition.reference',
    inherit = TOP.LEVEL.REFERENCE,
    portable = F,
    
    public = list(
        
        initialize = function(value.quantity.name,
                              specification,
                              ontology.name,
                              dimension,
                              from.compartments,
                              to.compartments)
        {
            super$initialize(name = NULL,
                             specification = specification,
                             ontology.name = ontology.name,
                             value.quantity.name = value.quantity.name,
                             type = 'transition.reference',
                             alias.suffix = 'from')
            
            if (!is.character(dimension) || length(dimension)!=1 || is.na(dimension) || nchar(dimension)==0)
                stop("'dimension' for a ", self$descriptor, " must be a single, non-NA, non-empty character value")
            
            if (!is.character(from.compartments) || length(from.compartments)==0 || 
                any(is.na(from.compartments)) || any(nchar(from.compartments)==0))
                stop("'from.compartments' for a ", self$descriptor, " (in dimension '", dimension, 
                     "') must be a non-NA, non-empty character vector")
            
            if (!is.character(to.compartments) || length(to.compartments)==0 ||
                any(is.na(to.compartments)) || any(nchar(to.compartments)==0))
                stop("'to.compartments' for a ", self$descriptor, " (in dimensions '", dimension,
                     "') must be a non-NA, non-empty character vector")
            
            private$i.dimension = dimension
            private$i.from.compartments = from.compartments
            private$i.to.compartments = to.compartments
        },
        
        overlaps = function(other.reference)
        {
            super$overlaps(other.reference) &&
                self$dimension == other.reference$dimension &&
                length(intersect(self$from.compartments, other.reference$from.compartments)) > 0 &&
                length(intersect(self$to.compartments, other.reference$to.compartments)) > 0
        },
        
        equals = function(other.reference)
        {
            self$type == other.reference$type &&
                self$ontology.name == other.reference$ontology.name &&
                self$dimension == other.reference$dimension &&
                setequal(self$from.compartments, other.reference$from.compartments) &&
                setequal(self$to.compartments, other.reference$to.compartments)
        },
        
        resolve.compartment.values = function(aliases, 
                                              ontology,
                                              unresolved.alias.names,
                                              ontology.name.for.error,
                                              error.prefix,
                                              wrt.specification)
        {
            private$i.from.compartments = do.resolve.compartment.values(values = private$i.from.compartments,
                                                                        aliases = aliases,
                                                                        template = ontology[[private$i.dimension]],
                                                                        unresolved.alias.names= unresolved.alias.names,
                                                                        variable.name.for.error = "'from.compartments' for transition",
                                                                        template.name.for.error = ontology.name.for.error,
                                                                        dimension = private$i.dimension,
                                                                        error.prefix = error.prefix)
                
            private$i.to.compartments = do.resolve.compartment.values(values = private$i.to.compartments,
                                                                      aliases = aliases,
                                                                      template = ontology[[private$i.dimension]],
                                                                      unresolved.alias.names= unresolved.alias.names,
                                                                      variable.name.for.error = "'to.compartments' for transition",
                                                                      template.name.for.error = ontology.name.for.error,
                                                                      dimension = private$i.dimension,
                                                                      error.prefix = error.prefix)

            self
        },
        
        get.description = function(wrt.specification, with.quotes=T)
        {
            if (with.quotes)
                qu = "'"
            else
                qu = ''
            
            dimension.valid.for.group = sapply(TOP.LEVEL.QUANTITY.GROUPS, function(g){
                any(names(wrt.specification$ontologies[[g]]) == dimension)
            })
            
            if (sum(dimension.valid.for.group)>1)
                paste0("transition in ", qu, private$i.dimension, qu, " (for ", qu, private$i.ontology.name, qu, ")")
            else
                paste0("transition in ", qu, private$i.dimension, qu)
        },
        
        get.max.dim.names = function(specification)
        {
            dim.names = specification$ontologies[[private$i.ontology.name]]
            names(dim.names)[names(dim.names)==private$i.dimension] = paste0(private$i.dimension, '.from')
            
            dim.names
        },
        
        get.dimension.aliases = function(specification)
        {
            rv = private$i.dimension
            names(rv) = paste0(rv, ".from")
            rv
        }
    ),
    
    active = list(
        
        descriptor = function(value)
        {
            if (missing(value))
                "transition reference"
            else
                stop("Cannot modify a top.level.reference's 'descriptor' - it is read-only")
        },
        
        dimension = function(value)
        {
            if (missing(value))
                private$i.dimension
            else
                stop("Cannot modify a top.level.reference's 'dimension' - it is read-only")
        },
        
        from.compartments = function(value)
        {
            if (missing(value))
                private$i.from.compartments
            else
                stop("Cannot modify a top.level.reference's 'from.compartments' - they are read-only")
        },
        
        to.compartments = function(value)
        {
            if (missing(value))
                private$i.to.compartments
            else
                stop("Cannot modify a top.level.reference's 'to.compartments' - they are read-only")
        }
    ),
    
    private = list(
        
        i.dimension = NULL,
        i.from.compartments = NULL,
        i.to.compartments = NULL
    )
)

##----------------------##
##----------------------##
##-- HELPER FUNCTIONS --##
##----------------------##
##----------------------##

##------------------------------------------------##
##--            SANITIZING NAMES                --##
##-- (Compartment and dimension names, aliases) --##
##------------------------------------------------##

RESERVED.INFIXES = c(
    '=',
    '__'
)

RESERVED.DIMENSIONS = c(
    'sim',
    'outcome',
    'year',
    'source'
)

RESERVED.DIMENSION.POSTFIXES = c(
    '.from',
    '.to'
)

RESERVED.COMPARTMENT.VALUES = character()

RESERVED.COMPARTMENT.VALUE.ALIASES = c(
    'all.ages'
)

# Quantity and element names cannot EQUAL these
RESERVED.QUANTITY.NAMES = c(
    "infected",
    "uninfected",
    "incidence",
    "births",
    "infected.mortality",
    "infected.specific.mortality",
    "uninfected.mortality"
)

DEFAULT.TRANSITION.NAME.PREFIX = 'transition_'
# Quantity and element names cannot BEGIN with these
RESERVED.QUANTITY.NAME.PREFIXES = c(
    DEFAULT.TRANSITION.NAME.PREFIX,
    'super',
    'this',
    'location',
    'specification'
)

do.validate.names.or.values <- function(values,
                                        descriptor,
                                        descriptor.plural=paste0(descriptor, 's'),
                                        variable.name.for.error,
                                        only.character.values,
                                        error.prefix,
                                        reserved.values = NULL,
                                        reserved.prefixes = NULL,
                                        reserved.postfixes = NULL,
                                        reserved.infixes = NULL,
                                        is.single.value)
{
    if (is.character(values))
    {
        #-- Screen out reserved values --#
        if (length(reserved.values)>0)
        {
            reserved.in.use.mask = sapply(tolower(values), function(val){
                any(val==reserved.values)
            })
            if (any(reserved.in.use.mask))
            {
                reserved.in.use = values[reserved.in.use.mask]
                if (is.single.value)
                    stop(paste0(error.prefix,
                                "Cannot use '", reserved.in.use, " as ",
                                variable.name.for.error, " of a ", descriptor,
                                " - it is a reserved keyword"))
                else
                    stop(paste0(error.prefix, 
                                "Cannot use ",
                                collapse.with.or("'", reserved.in.use, "'"),
                                ifelse(length(reserved.in.use)==1, 
                                       paste0(" as ", descriptor), 
                                       paste0(" as ", descriptor.plural)),
                                " in ", variable.name.for.error, ". ",
                                ifelse(length(reserved.in.use)==1, 
                                       "It is reserved for other uses.", 
                                       "They are reserved for other uses.")
                    ))
            }
        }
        
        #-- Check for invalid characters --#
        contains.invalid.characters.mask = sapply(values, function(val){
            string.contains.invalid.characters(val, valid.characters = NUMBERS.LETTERS.SPACE.DASH.PERIOD.UNDERSCORE)
        })
        if (any(contains.invalid.characters.mask))
        {
            invalid.values = values[contains.invalid.characters.mask]
            invalid.characters = setdiff(unlist(strsplit(values, split='')),
                                         strsplit(NUMBERS.LETTERS.SPACE.DASH.PERIOD.UNDERSCORE, split='')[[1]])
            
            if (is.single.value)
                stop(paste0(error.prefix,
                            "The ", variable.name.for.error, " of a ", descriptor,
                            " ('", invalid.values, "') cannot contain ", 
                            collapse.with.or("'", invalid.characters, "'"), 
                            " - it can only contain numbers, letters, spaces, periods, dashes, and underscores"))
            else
                stop(paste0(error.prefix,
                            "Cannot use ",
                            collapse.with.or("'", invalid.values, "'"),
                            ifelse(length(invalid.values)==1, 
                                   paste0(" as ", descriptor), 
                                   paste0(" as ", descriptor.plural)),
                            " in ", variable.name.for.error, ". ",
                            descriptor.plural, " cannot contain ",
                            collapse.with.or("'", invalid.characters, "'"),
                            " - only numbers, letters, spaces, periods, dashes, and underscores"))
        }
        
        #-- Screen out reserved prefixes --#
        if (length(reserved.prefixes)>0)
        {
            prefix.mask.mat = sapply(reserved.prefixes, function(prefix){
                string.begins.with(tolower(values), prefix)
            })
            dim(prefix.mask.mat) = c(length(values), length(reserved.prefixes))
            
            if (any(prefix.mask.mat))
            {
                invalid.values = values[apply(prefix.mask.mat, 1, any)]
                used.prefixes = reserved.prefixes[apply(prefix.mask.mat, 2, any)]
                
                if (is.single.value)
                    stop(paste0(error.prefix,
                                "The ", variable.name.for.error, " of ", descriptor,
                                " ('", invalid.values, "') cannot begin with ", 
                                collapse.with.or("'", used.prefixes, "'"), " - ",
                                ifelse(length(used.prefixes)==1, 
                                       "it is a reserved prefix",
                                       "they are reserved prefixes")))
                else
                    stop(paste0(error.prefix,
                                "Cannot use ",
                                collapse.with.or("'", invalid.values, "'"),
                                
                                ifelse(length(invalid.values)==1, 
                                       paste0(" as ", descriptor), 
                                       paste0(" as ", descriptor.plural)),
                                " in ", variable.name.for.error, ". ",
                                descriptor.plural, " cannot begin with ",
                                collapse.with.or("'", used.prefixes, "'")
                    ))
            }
        }
        
        #-- Screen out reserved postfixes --#
        if (length(reserved.postfixes)>0)
        {
            postfix.mask.mat = sapply(reserved.postfixes, function(postfix){
                string.ends.with(tolower(values), postfix)
            })
            dim(postfix.mask.mat) = c(length(values), length(reserved.postfixes))
            
            if (any(postfix.mask.mat))
            {
                invalid.values = values[apply(postfix.mask.mat, 1, any)]
                used.postfixes = reserved.postfixes[apply(postfix.mask.mat, 2, any)]
                
                if (is.single.value)
                    stop(paste0(error.prefix,
                                "The ", variable.name.for.error, " of ", descriptor,
                                " ('", invalid.values, "') cannot end with ", 
                                collapse.with.or("'", used.postfixes, "'"), " - ",
                                ifelse(length(used.postfixes)==1, 
                                       "it is a reserved postfix",
                                       "they are reserved postfixes")))
                else
                    stop(paste0(error.prefix,
                                "Cannot use ",
                                collapse.with.or("'", invalid.values, "'"),
                                
                                ifelse(length(invalid.values)==1, 
                                       paste0(" as ", descriptor), 
                                       paste0(" as ", descriptor.plural)),
                                " in ", variable.name.for.error, ". ",
                                descriptor.plural, " cannot end with ",
                                collapse.with.or("'", used.postfixes, "'")
                    ))
            }
        }
        
        #-- Screen out reserved infixes --#
        if (length(reserved.infixes)>0)
        {
            infix.mask.mat = sapply(reserved.infixes, function(infix){
                string.contains(tolower(values), infix)
            })
            dim(infix.mask.mat) = c(length(values), length(reserved.infixes))
            
            if (any(infix.mask.mat))
            {
                invalid.values = values[apply(infix.mask.mat, 1, any)]
                used.infixes = reserved.infixes[apply(infix.mask.mat, 2, any)]
                
                if (is.single.value)
                    stop(paste0(error.prefix,
                                "The ", variable.name.for.error, " of ", descriptor,
                                "('", invalid.values, "') is not allowed to contain ",
                                collapse.with.or("'", used.infixes, "'"),
                                " - ",
                                ifelse(length(used.infixes)==1, "it is", "they are"),
                                " reserved infixes"))
                else
                    stop(paste0(error.prefix,
                                "Cannot use ",
                                collapse.with.or("'", invalid.values, "'"),
                                
                                ifelse(length(invalid.values)==1, 
                                       paste0(" as ", descriptor), 
                                       paste0(" as ", descriptor.plural)),
                                " in ", variable.name.for.error, ". ",
                                descriptor.plural, " cannot contain ",
                                collapse.with.or("'", used.infixes, "'")
                    ))
            }
        }
    }
    else # not a character value
    {
        if (only.character.values)
        {
            if (is.single.value)
                stop(paste0(error.prefix,
                            "The ", variable.name.for.error, " of ", descriptor,
                            " must be a character value"))
            else
                stop(paste0(error.prefix,
                            "Invalid ", variable.name.for.error, ". ",
                            descriptor.plural, " must be character values"))
        }
        
        if (is.numeric(values))
        {
            if (any(values<1))
            {
                if (is.single.value)
                    stop(paste0(error.prefix,
                                "If it is a numeric value, the ", variable.name.for.error, 
                                " of ", descriptor, " must be >= 1"))
                else
                    stop(paste0(error.prefix,
                                "Invalid ", variable.name.for.error, ". If they are numeric, ",
                                descriptor.plural, " must be >= 1"))
            }
        }
        else if (is.logical(values))
        {
            if (is.single.value)
            {
                if (sum(values) != 1)
                {
                    if (sum(values) == 0)
                        stop(paste0(error.prefix,
                                    "If it is a logical vector, the ", variable.name.for.error, 
                                    " of ", descriptor, " must have exactly one TRUE value"))
                }
            }
            else
            {
                if (sum(values) == 0)
                    stop(paste0(error.prefix,
                                "Invalid ", variable.name.for.error, ". If they are logical vectors, ",
                                descriptor.plural, " must contain at least one TRUE value"))
            }
        }
        else
        {
            if (is.single.value)
                stop(paste0(error.prefix,
                            "The ", variable.name.for.error, " of ", descriptor,
                            " must be either a character, numeric, or logical value"))
            else
                stop(paste0(error.prefix,
                            "Invalid ", variable.name.for.error, ". ",
                            descriptor.plural, " must be either character, numeric, or logical values"))
        }
    }
}

validate.dimensions <- function(dimensions,
                                variable.name.for.error,
                                error.prefix)
{
    if (length(dimensions)>0)
    {
        do.validate.names.or.values(values = dimensions,
                                    variable.name.for.error = variable.name.for.error,
                                    descriptor = 'a dimension',
                                    descriptor.plural = 'dimensions',
                                    only.character.values = T,
                                    error.prefix = error.prefix,
                                    reserved.values = RESERVED.DIMENSIONS,
                                    reserved.prefixes = NULL,
                                    reserved.postfixes = RESERVED.DIMENSION.POSTFIXES,
                                    reserved.infixes = RESERVED.INFIXES,
                                    is.single.value = F)
    }
}

validate.compartment.values <- function(values,
                                        variable.name.for.error,
                                        only.character.values=T,
                                        error.prefix)
{
    do.validate.names.or.values(values = values,
                                variable.name.for.error = variable.name.for.error,
                                descriptor = 'a compartment value',
                                descriptor.plural = 'compartment values',
                                only.character.values = only.character.values,
                                error.prefix = error.prefix,
                                reserved.values = RESERVED.COMPARTMENT.VALUES,
                                reserved.prefixes = NULL,
                                reserved.postfixes = NULL,
                                reserved.infixes = RESERVED.INFIXES,
                                is.single.value = F)
}

validate.compartment.value.aliases <- function(aliases,
                                               variable.name.for.error,
                                               descriptor,
                                               descriptor.plural=paste0(descriptor, 's'),
                                               error.prefix)
{
    do.validate.names.or.values(values = aliases,
                                variable.name.for.error = variable.name.for.error,
                                descriptor = descriptor,
                                descriptor.plural = descriptor.plural,
                                error.prefix = error.prefix,
                                reserved.values = unique(c(RESERVED.COMPARTMENT.VALUES, RESERVED.COMPARTMENT.VALUE.ALIASES)),
                                reserved.prefixes = NULL,
                                reserved.postfixes = NULL,
                                reserved.infixes = RESERVED.INFIXES,
                                is.single.value = F)
}

validate.quantity.name <- function(name, 
                                   descriptor,
                                   descriptor.plural=paste0(descriptor, 's'),
                                   error.prefix = paste0("Cannot register ", descriptor, ": "))
{
    if (!is.character(name))
        stop(paste0(error.prefix, "'name' must be a single *character* value"))
    if (length(name)!=1)
        stop(paste0(error.prefix, "'name' must be a *SINGLE* character value"))
    if (is.na(name))
        stop(paste0(error.prefix, "'name' cannot be NA"))
    if (nchar(name)==0)
        stop(paste0(error.prefix, "'name' cannot be an empty string ('')"))
    
    do.validate.names.or.values(values = name,
                                variable.name.for.error = 'name',
                                descriptor = descriptor,
                                descriptor.plural = descriptor.plural,
                                error.prefix = error.prefix,
                                reserved.values = RESERVED.QUANTITY.NAMES,
                                reserved.prefixes = RESERVED.QUANTITY.NAME.PREFIXES,
                                reserved.postfixes = NULL,
                                reserved.infixes = RESERVED.INFIXES,
                                is.single.value = T)
}

validate.from.or.to.compartments <- function(value,
                                             variable.name.for.error,
                                             error.prefix = "Cannot register model transition: ")
{
    if (!is.vector(value))
        stop(paste0(error.prefix, "'", variable.name.for.error,
                    "' must be either a non-empty character value, a vector of integer values >= 1, or a logical vector with at least one TRUE value."))
    
    if (length(value)==0)
        stop(paste0(error.prefix, "'", variable.name.for.error,
                    "' must contain at least one value"))
    
    if (any(is.na(value)))
        stop(paste0(error.prefix, "'", variable.name.for.error,
                    "' cannot contain NA values"))
    
    if (is.character(value))
    {
        if (any(nchar(value)==0))
            stop(paste0(error.prefix, "If '", variable.name.for.error,
                        "' is a character vector, it cannot contain empty values ('')"))
        
        validate.compartment.values(value,
                                    variable.name.for.error = variable.name.for.error,
                                    error.prefix = error.prefix)
    }
    else if (is.numeric(value))
    {
        if (any(value<1))
            stop(paste0(error.prefix, "If '", variable.name.for.error,
                        "' is a numeric vector, it cannot contain values less than 1"))
    }
    else if (is.logical(value))
    {
        if (sum(value)==0)
            stop(paste0(error.prefix, "If '", variable.name.for.error,
                        "' is a logical vector, it must contain at least one TRUE value"))
    }
    else
        stop(paste0(error.prefix, "'", variable.name.for.error,
                    "' must be either a non-empty character value, a vector of integer values >= 1, or a logical vector with at least one TRUE value."))
}

validate.applies.to <- function(applies.to,
                                variable.name.for.error,
                                error.prefix)
{
    check.dimension.values.valid(dimension.values = applies.to,
                                 variable.name.for.error = 'applies.to',
                                 allow.empty = F,
                                 allow.duplicate.values.within.dimensions = F,
                                 error.prefix = error.prefix)

    sapply(names(applies.to), function(d){
        
    })
    
}

##---------------------------------------------------##
##-- HELPER FUNCTIONS (specific to this code file) --##
##---------------------------------------------------##

prepare.specification.function.wrapper <- function(fn,
                                                ...,
                                                fn.name.for.error,
                                                require.all.arguments.up.front,
                                                error.prefix='')
{
    FUNCTION.WRAPPER$new(fn=fn,
                         ...,
                         fn.name=fn.name.for.error,
                         require.all.arguments.up.front=require.all.arguments.up.front,
                         throw.error.if.missing.arguments.at.execute=T,
                         arguments.to.be.supplied.later = c('specification.info','location'),
                         error.prefix=error.prefix)
}

##-----------------------------------------------------##
##--                HELPER FUNCTIONS                 --##
##-- Internal to the package, but 'public' within it --##
##-----------------------------------------------------##

do.resolve.compartment.values <- function(values,
                                          aliases,
                                          template,
                                          unresolved.alias.names,
                                          variable.name.for.error,
                                          template.name.for.error,
                                          dimension,
                                          error.prefix)
{
    if (length(values)==0)
        stop(paste0(error.prefix,
                    "Error resolving values in ", variable.name.for.error,
                    " for dimension '", dimension, "' - the values are empty"))
    if (any(is.na(values)))
        stop(paste0(error.prefix,
                    "Error resolving values in ", variable.name.for.error,
                    " for dimension '", dimension, "' - the values contain NA elements"))
        
    
    if (is.numeric(values))
    {
        if (length(intersect(template, unresolved.alias.names))>0)
        {
            pending = intersect(template, unresolved.alias.names)
            stop(paste0(error.prefix, "Cannot use numeric indices in ",
                        variable.name.for.error, " for dimension '", dimension, 
                        "' - ", template.name.for.error, " for '", dimension,
                        "' contains ",
                        ifelse(length(pending)==1, "a value that needs", "values that need"),
                        " to be resolved using aliases that are functions (",
                        collapse.with.and("'", pending, "'"), ") so we cannot use numeric indices in resolving values for this dimension"))
        }
        if (any(values<1))
            stop(paste0(error.prefix, "Error with numeric indices in ",
                        variable.name.for.error, " for dimension '", dimension,
                        "' - numeric values must be >= 1, but the indices contain ",
                        collapse.with.and(values[values<1])))
        if (any(values>length(template)))
            stop(paste0(error.prefix, "Error with numeric indices in ",
                        variable.name.for.error, " for dimension '", dimension,
                        "' - numeric values must be <= ", length(template), 
                        ", but the indices contain ",
                        collapse.with.and(values[values>length(template)])))        
        
        # actually resolve
        template[ sort(values) ]
    }
    else if (is.logical(values))
    {
        if (length(intersect(template, unresolved.alias.names))>0)
        {
            pending = intersect(template, unresolved.alias.names)
            stop(paste0(error.prefix, "Cannot use logical indices in ",
                        variable.name.for.error, " for dimension '", dimension, 
                        "' - ", template.name.for.error, " for '", dimension,
                        "' contains ",
                        ifelse(length(pending)==1, "a value that needs", "values that need"),
                        " to be resolved using aliases that are functions (",
                        collapse.with.and("'", pending, "'"), ") so we cannot use logical indices in resolving values for this dimension"))
        }
        
        if (length(template) != length(values))
            stop(paste0(error.prefix, "Error with logical indices in ",
                        variable.name.for.error, " for dimension '", dimension,
                        "' - a vector of logical indices must have length ",
                        length(template), " [",
                        paste0("'", template, "'", collapse=', ')
                        ,"], but the logical vector specified has length ",
                        length(values)))
        if (!any(values))
            stop(paste0(error.prefix, "Error with logcial indices in ",
                        variable.name.for.error, " for dimension '", dimension,
                        "' - a vector of logical indices must contain at least one TRUE value, but the logical vector specified does not"))
        
        template[ values ]
    }
    else if (is.character(values))
    {
        invalid.values = setdiff(values,
                                 union(names(aliases), template))
        
        if (length(invalid.values)>0)
            stop(paste0(error.prefix,
                        "Error resolving values in ", variable.name.for.error,
                        " for dimension '", dimension, "' - the values contain ",
                        collapse.with.and("'", invalid.values, "'"), " but ",
                        ifelse(length(invalid.values)==1, "this is not a valid value", "these are not valid values"),
                        " of ", template.name.for.error, " for dimension '", dimension, "' (",
                        collapse.with.and("'", template, "'"), ")"))
        
        sub.rv = substitute.aliases.into.vector(values=values, aliases=aliases)
        invalid.values = setdiff(sub.rv, template)
        if (length(invalid.values)>0)
            stop(paste0(error.prefix,
                        "Error resolving values in ", variable.name.for.error,
                        " for dimension '", dimension, "' - after substituting aliases, the values contain ",
                        collapse.with.and("'", invalid.values, "'"), " but ",
                        ifelse(length(invalid.values)==1, "this is not a valid value", "these are not valid values"),
                        " of ", template.name.for.error, " for dimension '", dimension, "' (",
                        collapse.with.and("'", template, "'"), ")"))

        intersect(template, sub.rv) # to get them in the matching order
    }
    else
        stop(paste0(error.prefix,
                    "Error in resolving ", variable.name.for.error,
                    " for dimension '", dimension, "' - the indices must be either a character, numeric, or logical vector"))
}

do.resolve.dimension.values <- function(dimension.values,
                                        aliases,
                                        ontology,
                                        unresolved.alias.names,
                                        variable.name.for.error,
                                        ontology.name.for.error,
                                        error.prefix)
{
    if (is.null(dimension.values))
        return (dimension.values)
    
    rv = lapply(names(dimension.values), function(d){
        
        template = ontology[[d]]
        if (is.null(template))
            stop(paste0(error.prefix,
                        "Cannot resolve dimension values for ",
                        variable.name.for.error, " for dimension '", d, 
                        "' - the ontology has no template for dimension '", d, "'"))
        
        do.resolve.compartment.values(values = dimension.values[[d]],
                                      aliases = aliases,
                                      template = ontology[[d]],
                                      unresolved.alias.names = unresolved.alias.names,
                                      dimension = d,
                                      variable.name.for.error = variable.name.for.error,
                                      template.name.for.error = ontology.name.for.error,
                                      error.prefix = error.prefix)
        
    })
    
    names(rv) = names(dimension.values)
    rv
}

apply.aliases <- function(apply.to, aliases)
{
    if (is.character(apply.to))
        substitute.aliases.into.vector(values=apply.to, aliases=aliases)
    else if (is.list(apply.to))
        lapply(apply.to, apply.aliases, aliases=aliases)
    else if (is.null(apply.to) || is.numeric(apply.to) || is.logical(apply.to))
        apply.to
    else
        stop("In apply.aliases(), 'apply.to' must be either a character vector or a list containing character vectors")
}

substitute.aliases.into.vector <- function(values, aliases)
{
    unique(as.character(unlist(sapply(values, function(val){
        if (any(val==names(aliases)))
            aliases[[val]]
        else
            val
    }))))
}

apply.structured.aliases <- function(apply.to, structured.aliases)
{
    apply.to.names = intersect(names(apply.to), names(structured.aliases))
    if (length(apply.to.names)>0)
        apply.to[apply.to.names] = lapply(apply.to.names, function(name){
            substitute.aliases.into.vector(apply.to[[name]], aliases = structured.aliases[[name]]) 
        })
    else
        apply.to
}

# returns NULL if no mapping is needed
create.ontology.mapping.from.aliases <- function(aliases, dim.names,
                                                 error.prefix='')
{
    if (length(dim.names)==0)
        NULL
    else
    {
        dimension.needs.substitution.mask = sapply(dim.names, function(values){
            length(intersect(values, names(aliases)))>0
        })
        
        if (any(dimension.needs.substitution.mask))
        {
            error.prefix = paste0(error.prefix, "Error creating ontology.mapping from aliases - ")
            
            sub.mappings = lapply(names(dim.names)[dimension.needs.substitution.mask], function(d){
                
                to.replace = intersect(values, names(aliases))
                replacement.mappings = cbind(
                    from = unlist(sapply(to.replace, function(r){
                        rep(r, length(aliases[[r]]))
                    })),
                    to = unlist(aliases[to.replace])
                )
                
                not.replace = setdiff(values, to.replace)
                non.replacement.mappings = cbind(from=not.replace, to=not.replace)
                
                mappings = rbind(replacement.mappings, non.replacement.mappings)
                
                create.ontology.mapping(mappings=mappings,
                                        from.dimensions=d,
                                        to.dimensions=d,
                                        error.prefix=error.prefix)
            })
            
            combine.ontology.mappings(sub.mappings)
        }
        else
            NULL
    }
}

# new names is a character vector
# the names of new.names are the names of the old variables in the expression
# the values of new.names are the values to rename to
rename.expression.vars <- function(expr, new.names)
{
    env = lapply(new.names, function(name){
        parse(text=name)[[1]]
    })
    names(env) = names(new.names)
    
    do.call(substitute,
            list(expr=expr,
                 env=env))
}

verify.dim.names.for.quantity <- function(dim.names,
                                          quantity,
                                          variable.name.for.error,
                                          error.prefix,
                                          wrt.version,
                                          component.index=1)
{
    if (is.null(quantity$max.dim.names))
    {}
    else if (length(quantity$max.dim.names)==0)
    {
        if (length(dim.names)!=0)
            stop(paste0(error.prefix,
                        ifelse(component.index==1, '', paste0("The ", get.ordinal(component.index-1), " subset of ")),
                        quantity$get.original.name(wrt.version),
                        " can only accept scalar values, but ",
                        variable.name.for.error,
                        " are not empty (they include ",
                        ifelse(length(dim.names)==1, "dimension ", "dimensions "),
                        collapse.with.and("'", names(dim.names), "'"), ")"))
    }
    else
    {
        excess.dimensions = setdiff(names(dim.names), names(quantity$max.dim.names))
        if (length(excess.dimensions)>0)
            stop(paste0(error.prefix,
                        "Excess ",
                        ifelse(length(excess.dimensions)==1, "dimension", "dimensions"),
                        " (", collapse.with.and("'", excess.dimensions, "'"),
                        ") present in ",
                        variable.name.for.error, " for ",
                        ifelse(component.index==1, '', paste0("the ", get.ordinal(component.index-1), " subset of ")),
                        "model quantity ",
                        quantity$get.original.name(wrt.version),
                        ", which expects dimensions ",
                        collapse.with.and("'", names(quantity$max.dim.names), "'")))
        
        mismatched.dimensions = sapply(names(dim.names), function(d){
            !setequal(dim.names[[d]], quantity$max.dim.names[[d]])
        })
        
        if (any(mismatched.dimensions))
            stop(paste0(error.prefix,
                        variable.name.for.error, " do not match expected dimnames for ",
                        ifelse(component.index==1, '', paste0("the ", get.ordinal(component.index-1), " subset of ")),
                        "model quantity ",
                        quantity$get.original.name(wrt.version), " for ",
                        ifelse(sum(mismatched.dimensions)==1, "dimension ", "dimensions "), ":\n",
                        paste0(sapply(names(dim.names)[mismatched.dimensions], function(d){
                            paste0("- Dimension '", d, "' expects values ", 
                                   collapse.with.and("'", quantity$max.dim.names[[d]], "'"),
                                   " but ", variable.name.for.error, " has values ",
                                   collapse.with.and("'", dim.names[[d]], "'"))
                        }), collapse='\n') ))
    }
}