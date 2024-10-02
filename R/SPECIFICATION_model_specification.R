
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

#'@title Create a Model Specification for Running the JHEEM
#'
#'@param version A single character value denoting the version
#'@param sub.versions A character vector indicating the names of 'sub-versions' of the model. A sub-version has the same structure but records a different set of outcomes
#'@param iteration A single character or numeric value denoting the iteration of the model specification for this version
#'@param description A short text description of this version
#'
#'@param parent.version The character version of the specification from which this new specification should inherit. Must have been registered
#'@param do.not.inherit.model.quantity.names A vector of names of model.quantities which should NOT be inherited from ancestor specifications
#'@param do.not.inherit.model.outcome.names A vector of names of model outcomes which should NOT be inherited from ancestor specifications
#'@param do.not.inherit.transitions.for.dimension A vector of names of dimensions for which transitions should NOT be inherited from ancestor specifications
#'@param do.not.inherit.components.with.tags A vector of tags for which core components (transmission, natality, mortality, transitions, aging) should NOT be inherited from ancestor specifications
#'
#'@param compartments.for.infected.only,compartments.for.uninfected.only,compartments.for.infected.and.uninfected Named lists of character vectors F@specifying the compartments for uninfected and infected groups, or compartments shared by both. The names of the lists represent dimensions, and the values the compartments for each dimension. Compartments can either be string referencing the compartments themselves, or strings representing aliases passed to compartment.value.aliases
#'
#'@param age.endpoints Optional. A numeric vector (with at least two elements) giving the endpoints of the age brackets to use for the 'age' dimension. Results in length(age.endpoints)-1 different brackets, where the first bracket spans age.endpoints[1] (inclusive) to age.endpoints[2] (exclusive), the second bracket spans age.endpoints[2] to age.endpoints[3], etc
#'
#'@param compartment.value.aliases A named list representing substitutions to be made into compartment names (both in compartments.for.infected.only, compartments.for.uninfected.only, compartments.for.infected.and.uninfected and in subsequent references in registering model quantities). The names of the list represent what is to be replaced. The values of the list can be either (1) character vectors that are substituted in place or (2) functions that take parameter 'location' and return a character vector
#'
#'@param start.year The numeric year at which simulations should start. If NULL inherits the parent specification's start year
#'
#'@export
create.jheem.specification <- function(version,
                                       iteration,
                                       description,
                                       sub.versions=character(),
                                       start.year,
                                       
                                       parent.version = NULL,
                                       do.not.inherit.model.quantity.names = character(),
                                       do.not.inherit.model.outcome.names = character(),
                                       do.not.inherit.transitions.for.dimension = character(),
                                       do.not.inherit.components.with.tags = character(),
                                       compartments.for.infected.only = list(),
                                       compartments.for.uninfected.only = list(),
                                       compartments.for.infected.and.uninfected = list(),
                                       
                                       age.endpoints = NULL,
                                       
                                       compartment.value.aliases = list())
{
    error.prefix = "Cannot create jheem.specification: "
    
    ##-- ALLOWED MISSING OR NULL --##
    if (missing(do.not.inherit.model.quantity.names) ||
        is.null(do.not.inherit.model.quantity.names))
        do.not.inherit.model.quantity.names = character()
    
    if (missing(do.not.inherit.model.outcome.names) ||
        is.null(do.not.inherit.model.outcome.names))
        do.not.inherit.model.outcome.names = character()
    
    if (missing(do.not.inherit.transitions.for.dimension) ||
        is.null(do.not.inherit.transitions.for.dimension))
        do.not.inherit.transitions.for.dimension = character()
    
    if (missing(do.not.inherit.components.with.tags) ||
        is.null(do.not.inherit.components.with.tags))
        do.not.inherit.components.with.tags = character()
    
    if (missing(compartment.value.aliases) || is.null(compartment.value.aliases))
        compartment.value.aliases = list()
    
    if (missing(compartments.for.infected.only) || is.null(compartments.for.infected.only))
        compartments.for.infected.only = list()
    
    if (missing(compartments.for.uninfected.only) || is.null(compartments.for.uninfected.only))
        compartments.for.uninfected.only = list()
    
    if (missing(compartments.for.uninfected.only) || is.null(compartments.for.infected.and.uninfected))
        compartments.for.infected.and.uninfected = list()
    
    if (missing(age.endpoints))
        age.endpoints = NULL
    
    if (missing(sub.versions))
        sub.versions = character()
    
    ##-- CHECK ARGUMENTS --##

    #-- Version --#    
    validate.version.code(code = version, 
                          error.prefix = "Cannot create jheem.specification",
                          code.name.for.error = 'version')
        
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
    
    #-- Sub Versions --#
    if (!is.character(sub.versions) || any(is.na(sub.versions)))
        stop(paste0(error.prefix, "'sub.versions' must be a character vector with no NA values"))
    
    for (one.sub.version in sub.versions)
    {
        validate.sub.version.code(code = one.sub.version,
                                  error.prefix = error.prefix,
                                  code.name.for.error = 'the elements of sub.versions')
    }
    
    
    if (!is.null(parent.specification))
        sub.versions = c(sub.versions, parent.specification$sub.versions)
    sub.versions = unique(sub.versions)
    
    #--  Do not inherit <x> --#
    
    if (is.null(parent.specification))
    {
        if (length(do.not.inherit.model.quantity.names)>0)
            stop("'do.not.inherit.model.quantity.names' can only be set if 'parent.version' is specified")
        
        if (length(do.not.inherit.model.outcome.names)>0)
            stop("'do.not.inherit.model.outcome.names' can only be set if 'parent.version' is specified")
        
        if (length(do.not.inherit.transitions.for.dimension)>0)
            stop("'do.not.inherit.transitions.for.dimension' can only be set if 'parent.version' is specified")
        
        if (length(do.not.inherit.components.with.tags)>0)
            stop("'do.not.inherit.components.with.tags' can only be set if 'parent.version' is specified")
    }
    else
    {
        if (!is.character(do.not.inherit.model.quantity.names))
            stop(paste0(error.prefix,
                        "'do.not.inherit.model.quantity.names' must be a character vector"))
        
        if (!is.character(do.not.inherit.model.outcome.names))
            stop(paste0(error.prefix,
                        "'do.not.inherit.model.outcome.names' must be a character vector"))
        
        if (!is.character(do.not.inherit.transitions.for.dimension))
            stop(paste0(error.prefix,
                        "'do.not.inherit.transitions.for.dimension' must be a character vector"))
        
        if (!is.character(do.not.inherit.components.with.tags))
            stop(paste0(error.prefix,
                        "'do.not.inherit.components.with.tags' must be a character vector"))
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
    
    #-- Start year --#
    
    if (missing(start.year) || is.null(start.year))
    {
        if (is.null(parent.specification))
            stop(paste(error.prefix, "start.year cannot be NULL or missing unless a parent.version has been specified"))
        
        start.year = parent.specification$start.year
    }
    else
    {
        if (!is.numeric(start.year) || length(start.year)!=1 || is.na(start.year))
            stop(paste0(error.prefix, "'start.year' must be a single, non-NA numeric value"))
        
        min.start.year = 1800
        max.start.year = 1980
        if (start.year < min.start.year || start.year > max.start.year)
            stop(paste0(error.prefix,
                        "'start.year' (", start.year,
                        ") must be between ", min.start.year,
                        " and ", max.start.year))
    }
     
    #-- Compartment Value Aliases --#
    
    if (!is.list(compartment.value.aliases))
        stop(paste0(error.prefix,
                    "'compartment.value.aliases' must be a list"))
    
    if (length(compartment.value.aliases)>0)
    {
        if (length(compartment.value.aliases)>0 && is.null(names(compartment.value.aliases)))
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
        
        is.alias.function = as.logical(sapply(names(compartment.value.aliases), function(alias){
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
        }))
        
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
    }
    else
    {
        compartment.value.character.aliases = list()
        compartment.value.function.aliases = list()
    }
    
    # replaces age value with ages
    if (!is.null(age.endpoints))
    {
        compartment.value.character.aliases[['all.ages']] = age.info$ages
        compartment.value.character.aliases[['all.ages.but.last']] = age.info$ages[-length(age.info$ages)]
    }
    
    # Pull from parent
    if (!is.null(parent.specification))
    {
        to.add.compartment.value.character.aliases = compartment.value.character.aliases
        compartment.value.character.aliases = parent.specification$compartment.value.character.aliases
        compartment.value.character.aliases[names(to.add.compartment.value.character.aliases)] = to.add.compartment.value.character.aliases
        
        to.add.compartment.value.function.aliases = compartment.value.function.aliases
        compartment.value.function.aliases = parent.specification$compartment.value.function.aliases
        compartment.value.function.aliases[names(to.add.compartment.value.function.aliases)] = to.add.compartment.value.function.aliases
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
    # if (all(names(compartments.for.infected.and.uninfected)!='location'))
    #     stop(paste0(error.prefix, "'compartments.for.infected.and.uninfected' must include a 'location' dimension"))
    
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


    
    #-- Call the constructor --#
    JHEEM.SPECIFICATION$new(
        
        version = version,
        iteration = iteration,
        description = description,
        sub.versions = sub.versions,
        
        parent.specification = parent.specification,
        do.not.inherit.model.quantity.names = do.not.inherit.model.quantity.names,
        do.not.inherit.model.outcome.names = do.not.inherit.model.outcome.names,
        do.not.inherit.transitions.for.dimension = do.not.inherit.transitions.for.dimension,
        do.not.inherit.components.with.tags = do.not.inherit.components.with.tags,
        
        compartments.for.infected.only = compartments.for.infected.only,
        compartments.for.uninfected.only = compartments.for.uninfected.only,
        compartments.for.infected.and.uninfected = compartments.for.infected.and.uninfected,
        age.info = age.info,
        start.year = start.year,
        
        compartment.value.character.aliases = compartment.value.character.aliases,
        compartment.value.function.aliases = compartment.value.function.aliases
    )
}


#'@title Set Whether to Fix Strata Sizes During a Time Period
#'
#'@param specification The jheem.specification object
#'@param applies.after.time,applies.before.time Single numeric values giving the time frame over whether this setting applies
#'@param fix.strata A single logical value indicating whether strata should be fixed during this time frame
#'@param dimensions.to.fix A character vector denoting which dimensions should be fixed. These should be dimensions common to both infected and uninfected groups. These are only used if fix.strata==TRUE
#'
#'@export
register.fixed.model.strata <- function(specification,
                                        applies.after.time,
                                        applies.before.time,
                                        fix.strata=T,
                                        dimensions.to.fix=NULL)
{
    if (!is(specification, 'jheem.specification') || !R6::is.R6(specification))
        stop("'specification' must be an R6 object with class 'jheem.specification")

    specification$register.fixed.strata(applies.after.time = applies.after.time,
                                      applies.before.time = applies.before.time,
                                      fix.strata = fix.strata,
                                      dimensions.to.fix = dimensions.to.fix)    
}

#'@title Set The Initial Model Population
#'
#'@inheritParams register.model.quantity
#'@param specification The jheem.specification object
#'@param group Which group ("infected" or "uninfected") this initial population is for
#'
#'@export
register.initial.population <- function(specification,
                                        value,
                                        group)
{
    if (!is(specification, 'jheem.specification') || !R6::is.R6(specification))
        stop("'specification' must be an R6 object with class 'jheem.specification")
    
    specification$register.initial.population(value = value,
                                              group = group)
}

#'@title Register a Model Element
#'
#'@param specification The jheem.specification object
#'
#'@param name The name of the model element. Cannot overlap with the names of model quantities
#'@param scale The scale for this model.element. Can be 'rate', 'ratio', 'proportion', 'time', 'number', 'non.negative.number'
#'
#'@param dimensions Optional parameter to specify which dimensions the model.element is expected to take
#'@param dimension.values Optional parameter to specify which dimension.values the model.element will use in its dimension names
#'@param resolve.dimension.values.against.model.ontologies A logical indicating whether, if dimension.values are supplied, they should be resolved against model ontologies and have compartment.value.aliases for the model specification applied to them
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
                                   resolve.dimension.values.against.model=T,
                                   
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
        resolve.dimension.values.against.model = resolve.dimension.values.against.model,
        
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

#'@title A convenience function to streamline registering multiple model.elements for which we only need to specify a value
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

#'@title Register a Model Quantity
#'
#'@inheritParams register.model.element
#'@param specification The jheem.specification object to modify
#'@param name The name of the model quantity. Cannot overlap with names of model elements
#'@param value Either: (1) a numeric object, (2) a character string giving the name of a model quantity or a model element, (3) an 'call' or 'expression' object (generated by expression() or expr()) containing an expression comprising other model quantities or model elements, and the operators +, -, *, /, ^, sqrt, log, and exp, or (4) a function whose arguments are some subset of 'specification.metadata', 'location', other model quantities and model elements, and ...
#'@param scale An optional parameter, indicating the scale for this model.quantity. If specified, running the model will throw an error if evaluating the quantity results in an invalid value for the scale (eg a negative rate or a proportion > 1). Can be 'rate', 'ratio', 'proportion', 'time', 'number', 'non.negative.number'
#'@param dimensions An optional parameter, indicating the dimensions the quantity is expected to have (if dim.names is not specified, the specific values of the dimensions will be inferred automatically)
#'@param dimension.values An optional parameter, indicating (some of) the elements that the dimnames of the quantity are supposed to contain. This must be a named list of character vectors (whose names are a subset of dimensions if dimensions are specified).
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
                                    resolve.dimension.values.against.model=T,
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
                                    resolve.dimension.values.against.model = resolve.dimension.values.against.model,
                                    ...,
                                    na.replacement = na.replacement)
}


#'@title Register a Value to a Subset of a Model Quantity or Transition
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

#'@title Register a Foreground for a Model Specification
#'
#'@inheritParams register.model.quantity
#'@param foreground An object of class 'jheem.model.foreground', as created by \code{\link{create.model.foreground}}
#'@param name A single character value giving the name by which the foreground can be referred to
#'
#'@export
register.model.foreground <- function(specification,
                                      foreground,
                                      name)
{
    if (!is(specification, 'jheem.specification') || !R6::is.R6(specification))
        stop("'specification' must be an R6 object with class 'jheem.specification")
    
    specification$register.foreground(foreground, name=name)
}


#'@title Create
register.default.parameter.values <- function(specification,
                                        parameter.values)
{
    if (!is(specification, 'jheem.specification') || !R6::is.R6(specification))
        stop("'specification' must be an R6 object with class 'jheem.specification")
    
    specification$register.default.parameter.values(parameter.values)
}


#'@title Register Information about Mortality for a Model Specification
#'
#'@inheritParams register.model.quantity
#'@param mortality.rate.value The mortality.rate. Either: (1) a numeric object, (2) a character string giving the name of a model quantity or a model element, (3) an 'call' or 'expression' object (generated by expression() or expr()) containing an expression comprising other model quantities or model elements, and the operators +, -, *, /, ^, sqrt, log, and exp, or (4) a function whose arguments are some subset of 'specification.metadata', 'location', other model quantities and model elements, and ...
#'@param groups The group(s) to which the mortality apply. Either 'infected' or 'uninfected' or both
#'@param tag A tag used to follow track this component
#'@param applies.to A named list of character or integer vectors, denoting what subset of the groups the component apply to
#'
#'@family Defining Core Components for a Model Specification
#'
#'@export
register.mortality <- function(specification,
                               mortality.rate.value,
                               groups = c('uninfected','infected'),
                               tag = 'mortality',
                               applies.to=list())
{
    if (!is(specification, 'jheem.specification') || !R6::is.R6(specification))
        stop("'specification' must be an R6 object with class 'jheem.specification")
    
    specification$register.mortality(mortality.rate.value = mortality.rate.value,
                                     groups = groups,
                                     tag = tag,
                                     applies.to = applies.to)
}

#'@title Register Information about Natality (Births) for a Model Specification
#'
#'@inheritParams register.mortality
#'@param parent.groups The group representing parents (infected or uninfected)
#'@param child.groups The group into which births are distributed (infected or uninfected)
#'@param fertility.rate.value,birth.proportions.value The values for the fertility rate and the proportions by which births are distributed into the to.group. Either: (1) a numeric object, (2) a character string giving the name of a model quantity or a model element, (3) an 'call' or 'expression' object (generated by expression() or expr()) containing an expression comprising other model quantities or model elements, and the operators +, -, *, /, ^, sqrt, log, and exp, or (4) a function whose arguments are some subset of 'specification.metadata', 'location', other model quantities and model elements, and ...
#'@param parent.child.concordant.dimensions A character vector of the dimensions for which all new births share the same value as their parents
#'@param all.births.from.compartments A named list of character or integer values. The names represent dimensions, and the elements represent the compartments from those dimensions that can give birth
#'@param all.births.into.compartments A named list of single character or integer values. The names represent dimensions and the elements represent the single compartment in that dimension into which all births go
#'@param applies.to A named list of character or integer vectors, denoting what subset of the from.group fertility applies to
#'
#'@family Defining Core Components for a Model Specification
#'
#'@export
register.natality <- function(specification,
                              parent.groups,
                              child.groups,
                              fertility.rate.value,
                              birth.proportions.value,
                              parent.child.concordant.dimensions=character(),
                              all.births.from.compartments=list(),
                              all.births.into.compartments=character(),
                              tag='natality',
                              applies.to=list())
{
    if (!is(specification, 'jheem.specification') || !R6::is.R6(specification))
        stop("'specification' must be an R6 object with class 'jheem.specification")
    
    specification$register.natality(parent.groups = parent.groups,
                                    child.groups = child.groups,
                                    fertility.rate.value = fertility.rate.value,
                                    birth.proportions.value = birth.proportions.value,
                                    parent.child.concordant.dimensions = parent.child.concordant.dimensions,
                                    all.births.from.compartments = all.births.from.compartments,
                                    all.births.into.compartments = all.births.into.compartments,
                                    tag = tag,
                                    applies.to = applies.to)
}

#'@title Register Information about Remission from Infection for a Model Specification
#'
#'@inheritParams register.mortality
#'@param remission.rate.value,remission.proportions.value The values for the remission rate and the proportions by which remitted cases are distributed into the uninfected group. Either: (1) a numeric object, (2) a character string giving the name of a model quantity or a model element, (3) an 'call' or 'expression' object (generated by expression() or expr()) containing an expression comprising other model quantities or model elements, and the operators +, -, *, /, ^, sqrt, log, and exp, or (4) a function whose arguments are some subset of 'specification.metadata', 'location', other model quantities and model elements, and ...
#'@param applies.to A named list of character or integer vectors, denoting what subset of the infected group remissions apply to
#'
#'@family Defining Core Components for a Model Specification
#'
#'@export
register.remission <- function(specification,
                               remission.rate.value,
                               remission.proportions.value,
                               tag = 'remission',
                               applies.to = list(),
                               all.remissions.into.compartments = character())
{
    if (!is(specification, 'jheem.specification') || !R6::is.R6(specification))
        stop("'specification' must be an R6 object with class 'jheem.specification")
    
    specification$register.remission(remission.rate.value =remission.rate.value,
                                     remission.proportions.value = remission.proportions.value,
                                     tag = tag,
                                     applies.to = applies.to,
                                     all.remissions.into.compartments = all.remissions.into.compartments)
}

#'@title Register Information about Aging for a Model Specification
#'
#'@inheritParams register.mortality
#'@param groups The group(s) to which the aging apply. Either 'infected' or 'uninfected' or both
#'@param aging.rate.value The values for the aging rate. Either: (1) a numeric object, (2) a character string giving the name of a model quantity or a model element, (3) an 'call' or 'expression' object (generated by expression() or expr()) containing an expression comprising other model quantities or model elements, and the operators +, -, *, /, ^, sqrt, log, and exp, or (4) a function whose arguments are some subset of 'specification.metadata', 'location', other model quantities and model elements, and ...
#'@param applies.to A named list of character or integer vectors, denoting what subset of the group aging applies to
#'
#'@family Defining Core Components for a Model Specification
#'
#'@export
register.aging <- function(specification,
                           aging.rate.value,
                           tag = 'aging',
                           groups = c('uninfected','infected'),
                           applies.to = list())
{
    if (!is(specification, 'jheem.specification') || !R6::is.R6(specification))
        stop("'specification' must be an R6 object with class 'jheem.specification")
    
    specification$register.aging(aging.rate.value = aging.rate.value,
                                 tag = tag,
                                 groups = groups,
                                 applies.to = applies.to)
}

#'@title Register Information about Disease Transmission for a Model Specification
#'
#'@inheritParams register.mortality
#'@param contact.value,susceptibility.value,transmissibility.value,new.infection.proportions.value The values for the contact matrix, susceptibility, transmissibility, and proportions by which new infections are distributed into the infected group. Either: (1) a numeric object, (2) a character string giving the name of a model quantity or a model element, (3) an 'call' or 'expression' object (generated by expression() or expr()) containing an expression comprising other model quantities or model elements, and the operators +, -, *, /, ^, sqrt, log, and exp, or (4) a function whose arguments are some subset of 'specification.metadata', 'location', other model quantities and model elements, and ...
#'@param from.applies.to A named list of character or integer vectors, denoting what subset of the population PARTNERS may come from (regardless of whether or not they actually transmit). Must be a subset of compartments.for.infected.and.uninfected
#'@param to.applies.to A named list of character or integer vectors, denoting what subset of the uninfected group are susceptible to infection. Must be a subset of compartments.for.infected.and.uninfected + compartments.for.uninfected.only
#'@param transmission.applies.to A named list of character or integer vectors, denoting what subset of the infected group may transmit infection. Must be a subset of compartments.for.infected.and.uninfected + compartments.for.infected.only
#'@param new.infections.applies.to A named list of character or integer vectors, denoting what subset of the infected group new infections may enter into. Must be a subset of compartments.for.infected.only
#'
#'@family Defining Core Components for a Model Specification
#'
#'@export
register.transmission <- function(specification,
                                  contact.value,
                                  susceptibility.value,
                                  transmissibility.value,
                                  new.infection.proportions.value,
                                  tag = 'transmission',
                                  all.new.infections.into.compartments=list(),
                                  from.applies.to=list(),
                                  to.applies.to=list(),
                                  transmission.applies.to=list(),
                                  new.infections.applies.to=list())
{
    if (!is(specification, 'jheem.specification') || !R6::is.R6(specification))
        stop("'specification' must be an R6 object with class 'jheem.specification")
    
    specification$register.transmission(contact.value = contact.value,
                                        susceptibility.value = susceptibility.value,
                                        transmissibility.value = transmissibility.value,
                                        new.infection.proportions.value = new.infection.proportions.value,
                                        tag = tag,
                                        all.new.infections.into.compartments = all.new.infections.into.compartments,
                                        from.applies.to = from.applies.to, 
                                        to.applies.to = to.applies.to,
                                        transmission.applies.to = transmission.applies.to,
                                        new.infections.applies.to = new.infections.applies.to)
}

#'@title Register Information about Disease Transmission for a Model Specification
#'
#'@inheritParams register.mortality,register.model.quantity
#'@param dimension The name of the dimension in which this transition operates
#'@param from.compartments,to.compartments The compartments from and to which this transition applies. May be (1) one or more names of compartments, (2) one or more character compartment.value.aliases, (3) one or more integers denoting compartments, (4) a logical vector, which, when applied to the dimension names, denotes the compartments
#'@param groups The group(s) to which this transition applies. Either 'uninfected', 'infected', or both
#'
#'@family Defining Core Components for a Model Specification
#'
#'@export
register.transition <- function(specification,
                                dimension,
                                from.compartments,
                                to.compartments,
                                value,
                                groups,
                                applies.to=list(),
                                tag='transition',
                                na.replacement = as.numeric(NA),
                                ...)
{
    if (!is(specification, 'jheem.specification') || !R6::is.R6(specification))
        stop("'specification' must be an R6 object with class 'jheem.specification")
    
    value.name = deparse(substitute(value))
    if (!is.character(value.name) || length(value.name) != 1 || is.na(value.name))
        value.name = 'value'
    
    specification$register.transition(dimension = dimension,
                                      from.compartments = from.compartments,
                                      to.compartments = to.compartments,
                                      value = value,
                                      groups = groups,
                                      applies.to = applies.to,
                                      tag = tag,
                                      na.replacement = na.replacement,
                                      ...)
}


#'@title Register Information about Disease Transmission for a Model Specification
#'
#'@inheritParams register.mortality,register.model.quantity
#'@param type The type of the model mechanism. Must be one of 'mortality.rate', 'fertility.rate', 'birth.proportions', 'aging.rate', 'transition.rate', 'susceptibility', 'transmissibility', 'contact', 'new.infection.proportions', 'remission.rate', 'remission.proportions'
#'@param dimension The name of the dimension in which this transition operates
#'@param from.compartments,to.compartments The compartments from and to which this transition applies. May be (1) one or more names of compartments, (2) one or more character compartment.value.aliases, (3) one or more integers denoting compartments, (4) a logical vector, which, when applied to the dimension names, denotes the compartments
#'@param group The group(s) to which this transition applies. Either 'uninfected', 'infected', or both
#'
#'@family Defining Core Components for a Model Specification
#'
#@export
#'
register.model.mechanism <- function(specification,
                                     type,
                                     value,
                                     tags,
                                     groups,
                                     from.groups,
                                     to.groups,
                                     dimension,
                                     from.compartments,
                                     to.compartments,
                                     na.replacement = as.numeric(NA),
                                     ...)
{
    stop("This function is not currently implemented")
}

##----------------------##
##-- OUTCOME TRACKING --##
##----------------------##

#'@title Mark a Transition for Tracking
#'
#'@param specification The jheem.specification object to modify
#'@param name The name by which to access the outcome value after the simulation runs. Cannot be 'infected', 'uninfected' or the name of another outcome
#'@param outcome.metadata An object created by \code{\link{create.outcome.metadata}}, giving information on the outcome
#'
#'@param dynamic.quantity.name The name of the dynamic quantity to track for the outcome. Must be one of 'population', 'mortality', 'births', 'births.from', 'births.to', 'incidence', 'incidence.to', 'incidence.from', 'incidence.by', 'remission', 'remission.from', 'remission.to'
#'@param denominator.outcome Optional: A denominator for calculating proportions. Must be the name of a separately registered cumulative outcome that has type 'non.negative.number'
#'
#'@param groups The groups to track for ('infected' or 'uninfected'). Choosing NULL (the default value) tracks for all groups for which the outcome could apply
#'@param include.tags The tags to track for (as given in the call to \code{\link{register.transition}}). Choosing NULL (the default value) tracks for all tags for which the outcome could apply minus excluded tags
#'@param exclude.tags Tags to exclude from tracking.
#'@param sub.versions The sub-versions of the model for which this outcome will be recorded. If NULL, the outcome will be recorded for all sub-versions
#'
#'@param multiply.by An optional quantity to multiply the outcome value by at each time step before storing. Can be either (1) a numeric value, (2) a single character value referencing a model quantity or element, or (3) an expression that includes only the names of model quantities and elements and operators +, -, *, /, ^, sqrt, log, and exp
#'
#'@param corresponding.data.outcome The (optional) name of a real-world outcome (with observations registered to a data.manager) to which this model outcome corresponds
#'
#'@param keep.dimensions The dimensions which to keep in storing outcome values. All other dimensions are marginalized. Either keep.dimensions OR exclude.dimensions but not both can be set; if both are NULL, will keep all possible dimensions for the outcome.
#'@param exclude.dimensions Dimensions to be marginalized out when storing outcome values. Either keep.dimensions OR exclude.dimensions but not both can be set; if both are NULL, will keep all possible dimensions for the outcome.
#'@param subset.dimension.values A list of dimension values indicating for which values of which dimensions the outcome should be tracked
#'@param rename.dimension.values A list of names character vectors indicating which dimension values should be renamed in the 
#'@param scale The scale of this outcome. Can be NULL when outcome.metadata is not NULL (only required when not saving and not using outcome.metadata)
#'@param save A logical indicator of whether this outcome should be stored in simulations. If FALSE, the outcome will be available for calculating other outcomes (with \code{\link{track.cumulative.outcome}} or \code{\link{track.point.outcome}}), but will not be retrievable afterwards
#'@param from.year,to.year The time span during which the outcome should be recorded
#'
#'@param dimension.aliases A named character vector indicating what dimensions in the inputs to the outcome (the names of the vector) should be converted to (the values of the vector)
#'@param dimension.alias.suffix A single character value, indicating a suffix that should be removed from the dimensions of the inputs to the outcome. Eg, if ".to" is specified, then dimension "race.to" in outcome inputs becomes "race" in the outcome
#'
#'@details Integrates the dynamic quantity (or the product of the dynamic quantity x multiply by), such that the simulation stores, for each year y, the integral from y to y+1 of (dynamic quantity * multiply.by)
#'
#'@export
track.dynamic.outcome <- function(specification,
                                  name,
                                  outcome.metadata,
                                  dynamic.quantity.name,
                                  denominator.outcome = NULL,
                                  
                                  include.tags = NULL,
                                  exclude.tags = NULL,
                                  groups = NULL,
                                  sub.versions = NULL,
                                  
                                  multiply.by = NULL,
                                  corresponding.data.outcome = NULL,
                                  keep.dimensions = NULL,
                                  exclude.dimensions = NULL,
                                  subset.dimension.values = NULL,
                                  rename.dimension.values = NULL,
                                  dimension.aliases = NULL,
                                  dimension.alias.suffix = NULL,
                                  scale = NULL,
                                  from.year = -Inf,
                                  to.year = Inf,
                                  save = T)
{
    if (!is(specification, 'jheem.specification') || !R6::is.R6(specification))
        stop("'specification' must be an R6 object with class 'jheem.specification")
    
    specification$track.dynamic.outcome(name = name,
                                        outcome.metadata = outcome.metadata,
                                        dynamic.quantity.name = dynamic.quantity.name,
                                        denominator.outcome = denominator.outcome,
                                        include.tags = include.tags,
                                        groups = groups,
                                        sub.versions = sub.versions,
                                        multiply.by = multiply.by,
                                        corresponding.data.outcome = corresponding.data.outcome,
                                        keep.dimensions = keep.dimensions,
                                        exclude.dimensions = exclude.dimensions,
                                        subset.dimension.values = subset.dimension.values,
                                        rename.dimension.values = rename.dimension.values,
                                        dimension.aliases = dimension.aliases,
                                        dimension.alias.suffix = dimension.alias.suffix,
                                        scale = scale,
                                        from.year = from.year,
                                        to.year = to.year,
                                        save = save)
}

#'@title Mark a Transition for Tracking
#'
#'@inheritParams track.dynamic.outcome
#'
#'@param dimension The name of the (single) dimension along which we are tracking a transition
#'@param from.compartments,to.compartments The names (or aliases) of the from and to compartments of the transition to track that gives information about the resulting quantity
#'
#'@details Integrates the transition (or the product of transition x multiply by), such that the simulation stores, for each year y, the integral from y to y+1 of (transition * multiply.by)
#'
#'@export
track.transition <- function(specification,
                             name,
                             outcome.metadata,
                             dimension,
                             from.compartments,
                             to.compartments,
                             denominator.outcome = NULL,
                             
                             include.tags = NULL,
                             exclude.tags = NULL,
                             groups = NULL,
                             sub.versions = NULL,
                             
                             multiply.by = NULL,
                             corresponding.data.outcome = NULL,
                             keep.dimensions = NULL,
                             exclude.dimensions = NULL,
                             subset.dimension.values = NULL,
                             rename.dimension.values = NULL,
                             dimension.aliases = NULL,
                             dimension.alias.suffix = NULL,
                             scale = NULL,
                             from.year = -Inf,
                             to.year = Inf,
                             save = T)
{
    if (!is(specification, 'jheem.specification') || !R6::is.R6(specification))
        stop("'specification' must be an R6 object with class 'jheem.specification")
    
    specification$track.transition(name = name,
                                   outcome.metadata = outcome.metadata,
                                   dimension = dimension,
                                   from.compartments = from.compartments,
                                   to.compartments = to.compartments,
                                   denominator.outcome = denominator.outcome,
                                   include.tags = include.tags,
                                   exclude.tags = exclude.tags,
                                   groups = groups,
                                   sub.versions = sub.versions,
                                   multiply.by = multiply.by,
                                   corresponding.data.outcome = corresponding.data.outcome,
                                   keep.dimensions = keep.dimensions,
                                   exclude.dimensions = exclude.dimensions,
                                   subset.dimension.values = subset.dimension.values,
                                   rename.dimension.values = rename.dimension.values,
                                   dimension.aliases = dimension.aliases,
                                   dimension.alias.suffix = dimension.alias.suffix,
                                   scale = scale,
                                   from.year = from.year,
                                   to.year = to.year,
                                   save = save)
}

#'@title Mark an Outcome to Be Integrated and Tracked
#'
#'@details Tracking outcomes with this function allows "point" outcomes to be converted into "cumulative" outcomes without going through the differential equation solver. This function yields an approximation of the outcome that would be generated by using \code{\link{track.dynamic.outcome}} in a way that is computationally much less expensive when running simulations
#'
#'@inheritParams track.dynamic.outcome
#'
#'@param value.to.integrate Either (1) a single character value referencing a separately-registered "point" (non-cumulative) outcome or a quantity, or (2) an expression that includes only the names of point outcomes and model quantities and operators +, -, *, /, ^, sqrt, log, and exp
#'@param value.is.numerator In the case where the outcome's scale is proportion, rate, or ratio (any scale that results from a numerator or a denominator), an indicator of whether the value.to.integrate represents just the numerator or the actual proportion/rate/ratio
#'
#'@export
track.integrated.outcome <- function(specification,
                                     name,
                                     outcome.metadata,
                                     
                                     value.to.integrate,
                                     value.is.numerator = F,
                                     multiply.by = NULL,
                                     denominator.outcome = NULL,
                                     sub.versions = NULL,
                                     
                                     corresponding.data.outcome = NULL,
                                     keep.dimensions = NULL,
                                     exclude.dimensions = NULL,
                                     subset.dimension.values = NULL,
                                     rename.dimension.values = NULL,
                                     dimension.aliases = NULL,
                                     dimension.alias.suffix = NULL,
                                     scale = NULL,
                                     from.year = -Inf,
                                     to.year = Inf,
                                     save = T)
{
    if (!is(specification, 'jheem.specification') || !R6::is.R6(specification))
        stop("'specification' must be an R6 object with class 'jheem.specification")
    
    specification$track.integrated.outcome(name = name,
                                           outcome.metadata = outcome.metadata,
                                           value.to.integrate = value.to.integrate,
                                           value.is.numerator = value.is.numerator,
                                           multiply.by = multiply.by,
                                           denominator.outcome = denominator.outcome,
                                           sub.versions = sub.versions,
                                           corresponding.data.outcome = corresponding.data.outcome,
                                           keep.dimensions = keep.dimensions,
                                           exclude.dimensions = exclude.dimensions,
                                           subset.dimension.values = subset.dimension.values,
                                           rename.dimension.values = rename.dimension.values,
                                           dimension.aliases = dimension.aliases,
                                           dimension.alias.suffix = dimension.alias.suffix,
                                           from.year = from.year,
                                           to.year = to.year,
                                           scale = scale,
                                           save = save)
}

#'@title Track a Combination of Cumulative Outcomes
#'
#'@details Creates a cumulative outcome to track by combining other cumulative outcomes and static quantities
#'
#'@inheritParams track.dynamic.outcome
#'
#'@param value Either (1) a single character value referencing a separately-registered cumulative outcome or a static (value does not change over time) quantity, or (2) an expression that includes only the names of cumulative outcomes and static model quantities and operators +, -, *, /, ^, sqrt, log, and exp
#'@param denominator.outcome A denominator for aggregating the outcome or calculating proportions. Required if the scale of this outcome is not 'number' or 'non.negative.number', otherwise optional. Must be the name of a separately registered cumulative outcome that has type 'non.negative.number'
#'@param value.is.numerator In the case where the outcome's scale is proportion, rate, or ratio (any scale that results from a numerator or a denominator), an indicator of whether the value represents just the numerator or the actual proportion/rate/ratio
#'
#'@export
track.cumulative.outcome <- function(specification,
                                     name,
                                     outcome.metadata,
                                     value,
                                     value.is.numerator = F,
                                     sub.versions = NULL,
                                     
                                     denominator.outcome = NULL,
                                     corresponding.data.outcome = NULL,
                                     keep.dimensions = NULL,
                                     exclude.dimensions = NULL,
                                     subset.dimension.values = NULL,
                                     rename.dimension.values = NULL,
                                     dimension.aliases = NULL,
                                     dimension.alias.suffix = NULL,
                                     scale = NULL,
                                     from.year = -Inf,
                                     to.year = Inf,
                                     save = T)
{
    if (!is(specification, 'jheem.specification') || !R6::is.R6(specification))
        stop("'specification' must be an R6 object with class 'jheem.specification")
    
    specification$track.cumulative.outcome(name = name,
                                         outcome.metadata = outcome.metadata,
                                         value = value,
                                         value.is.numerator = value.is.numerator,
                                         sub.versions = sub.versions,
                                         denominator.outcome = denominator.outcome,
                                         corresponding.data.outcome = corresponding.data.outcome,
                                         keep.dimensions = keep.dimensions,
                                         exclude.dimensions = exclude.dimensions,
                                         subset.dimension.values = subset.dimension.values,
                                         rename.dimension.values = rename.dimension.values,
                                         dimension.aliases = dimension.aliases,
                                         dimension.alias.suffix = dimension.alias.suffix,
                                         scale = scale,
                                         from.year = from.year,
                                         to.year = to.year,
                                         save = save)
}

#'@title Track a Combination of "Point" (Non-Cumulative) Outcomes
#'
#'@details Creates a "point" (non-cumulative) outcome to track by combining other point outcomes and quantities
#'
#'@inheritParams track.cumulative.outcome
#'
#'@param value Either (1) a single character value referencing a separately-registered "point" (non-cumulative) outcome or a quantity, or (2) an expression that includes only the names of point outcomes and model quantities and operators +, -, *, /, ^, sqrt, log, and exp
#'@param denominator.outcome A denominator for aggregating the outcome or calculating proportions. Required if the scale of this outcome is not 'number' or 'non.negative.number', otherwise optional. Must be the name of a separately registered "point" (non-cumulative) outcome that has type 'non.negative.number'
#'
#'@export
track.point.outcome <- function(specification,
                                name,
                                outcome.metadata,
                                value,
                                value.is.numerator = F,
                                sub.versions = NULL,
                                
                                denominator.outcome = NULL,
                                corresponding.data.outcome = NULL,
                                keep.dimensions = NULL,
                                exclude.dimensions = NULL,
                                subset.dimension.values = NULL,
                                rename.dimension.values = NULL,
                                dimension.aliases = NULL,
                                dimension.alias.suffix = NULL,
                                scale = NULL,
                                from.year = -Inf,
                                to.year = Inf,
                                save = T)
{
    if (!is(specification, 'jheem.specification') || !R6::is.R6(specification))
        stop("'specification' must be an R6 object with class 'jheem.specification")
    
    specification$track.point.outcome(name = name,
                                      outcome.metadata = outcome.metadata,
                                      value = value,
                                      value.is.numerator = value.is.numerator,
                                      sub.versions = sub.versions,
                                      denominator.outcome = denominator.outcome,
                                      corresponding.data.outcome = corresponding.data.outcome,
                                      keep.dimensions = keep.dimensions,
                                      exclude.dimensions = exclude.dimensions,
                                      subset.dimension.values = subset.dimension.values,
                                      rename.dimension.values = rename.dimension.values,
                                      dimension.aliases = dimension.aliases,
                                      dimension.alias.suffix = dimension.alias.suffix,
                                      scale = scale,
                                      from.year = from.year,
                                      to.year = to.year,
                                      save = save)
}

#'@title Track an Outcome Formed by Integrating a Time-Varying Rate into a Proportion
#'
#'@details Calculates a proportion derived from having a 'compartment' which people leave at a given rate, and calculates the proportion who either leave or remain in that compartment over time
#'
#'@inheritParams track.dynamic.outcome
#'
#'@param rate.value The rate to be integrated. Either (1) a single character value referencing a separately-registered "point" (non-cumulative) outcome or a quantity, or (2) an expression that includes only the names of point outcomes and model quantities and operators +, -, *, /, ^, sqrt, log, and exp
#'@param denominator.outcome A denominator for aggregating the outcome or calculating proportions. Required if the scale of this outcome is not 'number' or 'non.negative.number', otherwise optional. Must be the name of a separately registered cumulative outcome that has type 'non.negative.number'
#'@param calculate.proportion.leaving If TRUE, integrates to get the proportion of those who "leave" the imaginary compartment at the given rate. If FALSE, calculates the proportion who have not left (ie 1-proportion.leaving)
#'
#'@export
track.cumulative.proportion.from.rate <- function(specification,
                                                  name,
                                                  outcome.metadata,
                                                  rate.value,
                                                  
                                                  denominator.outcome,
                                                  sub.versions = NULL,
                                                  corresponding.data.outcome = NULL,
                                                  calculate.proportion.leaving = T,
                                                  keep.dimensions = NULL,
                                                  exclude.dimensions = NULL,
                                                  subset.dimension.values = NULL,
                                                  rename.dimension.values = NULL,
                                                  dimension.aliases = NULL,
                                                  dimension.alias.suffix = NULL,
                                                  from.year = -Inf,
                                                  to.year = Inf,
                                                  save = T)
{
    if (!is(specification, 'jheem.specification') || !R6::is.R6(specification))
        stop("'specification' must be an R6 object with class 'jheem.specification")
    
    specification$track.cumulative.proportion.from.rate(name = name,
                                                        outcome.metadata = outcome.metadata,
                                                        rate.value = rate.value,
                                                        denominator.outcome = denominator.outcome,
                                                        sub.versions = sub.versions,
                                                        corresponding.data.outcome = corresponding.data.outcome,
                                                        calculate.proportion.leaving = calculate.proportion.leaving,
                                                        keep.dimensions = keep.dimensions,
                                                        exclude.dimensions = exclude.dimensions,
                                                        subset.dimension.values = subset.dimension.values,
                                                        rename.dimension.values = rename.dimension.values,
                                                        dimension.aliases = dimension.aliases,
                                                        dimension.alias.suffix = dimension.alias.suffix,
                                                        from.year = from.year,
                                                        to.year = to.year,
                                                        save = save)
}

#'@title Create a metadata object for tracked quantities or tracked transitions
#'
#'@param scale The scale of the quantity. Either "rate", "ratio", "proportion", "time", "number", "non.negative.number"
#'@param display.name The (nicely formatted) name of the quantity - to use in titling figures and other reporting. Should be capitalized
#'@param description A phrase or single sentence describing the quantity. Should be captialized
#'@param axis.name A name to use in labeling axes on figures that display this quantity. Should be capitalized
#'@param units The units the quantity takes (eg, "cases", "%", "tests/year")
#'@param singular.unit The unit to use if the value of the quantity is exactly == 1
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
create.outcome.metadata <- function(display.name,
                                    description,
                                    scale,
                                    axis.name,
                                    units,
                                    singular.unit=units,
                                    display.as.percent = scale=='proportion')
{
    OUTCOME.METADATA$new(
        scale = scale,
        description = description,
        display.name = display.name,
        axis.name = axis.name,
        units = units,
        singular.unit = singular.unit,
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
                              singular.unit,
                              display.as.percent = scale=='proportion')
        {
            #-- Validate Arguments --#
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
            
            if (!is.character(singular.unit) || length(singular.unit)!=1 || is.na(singular.unit) || nchar(singular.unit)==0)
                stop(error.prefix, "'singular.unit' must be a single, non-NA, non-empty character value")
            
            check.model.scale(scale, varname.for.error = 'scale', error.prefix = error.prefix)
            
            if (!is.logical(display.as.percent) || length(display.as.percent)!=1 || is.na(display.as.percent))
                stop(error.prefix, "'display.as.percent' must be a single, non-NA logical value")
                
            
            #-- Store Values --#
            private$i.scale = scale
            private$i.display.name = display.name
            private$i.description = description
            private$i.axis.name = axis.name
            private$i.units = units
            private$i.singular.unit = singular.unit
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
        
        singular.unit = function(value)
        {
            if (missing(value))
                private$i.singular.unit
            else
                stop("Cannot modify outcome.metadata's 'singular.unit' - it is read-only")
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
        i.singular.unit = NULL,
        i.description = NULL,
        i.display.as.percent = NULL
        
    )
)

MODEL.OUTCOME.METADATA = R6::R6Class(
    'model.outcome.metadata',
    inherit = OUTCOME.METADATA,
    
    public = list(
        
        initialize = function(outcome.metadata,
                              is.cumulative,
                              is.intrinsic,
                              corresponding.observed.outcome)
        {
            if (!is(outcome.metadata, 'outcome.metadata'))
                stop("Error creating model outcome.metadata: 'outcome.metadata' must be an object of class outcome.metadata")
            
            super$initialize(scale = outcome.metadata$scale,
                             display.name = outcome.metadata$display.name,
                             description = outcome.metadata$description,
                             axis.name = outcome.metadata$axis.name,
                             units = outcome.metadata$units,
                             singular.unit = outcome.metadata$singular.unit,
                             display.as.percent = outcome.metadata$display.as.percent)
            
            error.prefix = paste0("Error creating model outcome.metadata: '", outcome.metadata$name, "'")
            
            # Validate is.cumulative
            if (!is.logical(is.cumulative) || length(is.cumulative)!=1 || is.na(is.cumulative))
                stop(paste0(error.prefix, "'is.cumulative' must be a single, non-NA logical value"))
            
            # Validate is.intrinsic
            if (!is.logical(is.intrinsic) || length(is.intrinsic)!=1 || is.na(is.intrinsic))
                stop(paste0(error.prefix, "'is.intrinsic' must be a single, non-NA logical value"))
            
            # Validate corresponding.observed.outcome
            if (!is.null(corresponding.observed.outcome) && 
                (!is.character(corresponding.observed.outcome) || length(corresponding.observed.outcome)!=1 || is.na(corresponding.observed.outcome)))
                stop(paste0(error.prefix, "If 'corresponding.observed.outcome' is specified, it must be a single, non-NA character value"))
                
            
            # Store variables
            private$i.is.cumulative = is.cumulative
            private$i.is.intrinsic = is.intrinsic
            private$i.corresponding.observed.outcome = corresponding.observed.outcome
        }
            
    ),
    
    active = list(
        
        is.cumulative = function(value)
        {
            if (missing(value))
                private$i.is.cumulative
            else
                stop("Cannot modify outcome.metadata's 'is.cumulative' - it is read-only")
        },
        
        is.intrinsic = function(value)
        {
            if (missing(value))
                private$i.is.intrinsic
            else
                stop("Cannot modify outcome.metadata's 'is.intrinsic' - it is read-only")
        },
        
        corresponding.observed.outcome = function(value)
        {
            if (missing(value))
                private$i.corresponding.observed.outcome
            else
                stop("Cannot modify outcome.metadata's 'corresponding.observed.outcome' - it is read-only")
        }
    ),
    
    private = list(
        
        i.is.cumulative = NULL,
        i.is.intrinsic = NULL,
        i.corresponding.observed.outcome = NULL
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

MIN.SPECIFICATION.ITERATION.NCHAR = 1
MAX.SPECIFICATION.ITERATION.NCHAR = 20

MIN.SPECIFICATION.DESCRIPTION.NCHAR = 2
MAX.SPECIFICATION.DESCRIPTION.NCHAR = 500

MIN.MODEL.FROM.YEAR = 1800
MAX.MODEL.FROM.YEAR.OFFSET.FROM.CURRENT.YEAR = 10

RAMP.TAPER.SCALES = c('identity','log','exp')
RAMP.TAPER.APPLICATIONS = c('multiplier','absolute')

QUANTITY.SUBSET.APPLY.FUNCTIONS = c('overwrite','add','subtract','multiply','divide')

ALLOWED.MODEL.QUANTITY.VALUE.EXPRESSION.FUNCTIONS = c("+","-","*","/","(","log","exp","sqrt")
ALLOWED.MODEL.OUTCOME.VALUE.EXPRESSION.FUNCTIONS = c("+","-","*","/","(","log","exp","sqrt")

MIN.FUNCTIONAL.FORM.FROM.YEAR = MIN.MODEL.FROM.YEAR
MAX.FUNCTIONAL.FORM.FROM.YEAR.OFFSET.FROM.CURRENT.YEAR = 50

ALLOWED.GROUPS = c('infected', 'uninfected')

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
        
        ##------------------------------##
        ##--     The CONSTRUCTOR      --## 
        ##-- (private to the package) --##    
        ##------------------------------##
        
        initialize = function(version,
                              iteration,
                              description,
                              sub.versions,
                              
                              parent.specification,
                              do.not.inherit.model.quantity.names,
                              do.not.inherit.model.outcome.names,
                              do.not.inherit.transitions.for.dimension,
                              do.not.inherit.components.with.tags,
                              
                              compartments.for.infected.only,
                              compartments.for.uninfected.only,
                              compartments.for.infected.and.uninfected,
                              age.info,
                              start.year,
                              
                              compartment.value.character.aliases,
                              compartment.value.function.aliases)
        {
            # As of now, I am assuming these have already been error-checked
            # Either by the create.jheem.specification function
            # Or they are being passed forward from a (validated) specification in the 
            #   finalized.jheem.specification constructor
            
            #-- Store variables --#
            private$i.version = version
            private$i.iteration = iteration
            private$i.description = description
            private$i.sub.versions = sub.versions
            
            private$i.parent.specification = parent.specification
            private$i.do.not.inherit.model.quantity.names = do.not.inherit.model.quantity.names
            private$i.do.not.inherit.model.outcome.names = do.not.inherit.model.outcome.names
            private$i.do.not.inherit.transitions.for.dimension = do.not.inherit.transitions.for.dimension
            private$i.do.not.inherit.components.with.tags = do.not.inherit.components.with.tags
            
            private$i.age.info = age.info
            private$i.start.year = start.year
            
            private$i.compartment.value.character.aliases = compartment.value.character.aliases
            private$i.compartment.value.function.aliases = compartment.value.function.aliases

            private$i.quantities = list()
            private$i.core.components = list()
            private$i.mechanisms = list()
            
            private$i.foregrounds = list()
            if (is.null(parent.specification))
                private$i.default.parameter.values = numeric()
            else
                private$i.default.parameter.values = parent.specification$default.parameter.values
            
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
            
            general.uninfected.from.dim.names = general.uninfected.to.dim.names = c(compartments.for.infected.and.uninfected, compartments.for.uninfected.only)
            names(general.uninfected.from.dim.names) = paste0(names(general.uninfected.from.dim.names), '.from')
            names(general.uninfected.to.dim.names) = paste0(names(general.uninfected.to.dim.names), '.to')
            
            # Store the combined dim.names
            private$i.ontologies = list(
                general = compartments.for.infected.and.uninfected,
                infected = c(compartments.for.infected.and.uninfected, compartments.for.infected.only),
                uninfected = c(compartments.for.infected.and.uninfected, compartments.for.uninfected.only),
                
                general.compartments = compartments.for.infected.and.uninfected,
                infected.compartments = compartments.for.infected.only,
                uninfected.compartments = compartments.for.uninfected.only,
                
                infected.from = general.infected.from.dim.names,
                infected.to = general.infected.to.dim.names,
                uninfected.from = general.uninfected.from.dim.names,
                uninfected.to = general.uninfected.to.dim.names,
                
                infected.plus.uninfected = c(compartments.for.infected.and.uninfected, compartments.for.infected.only, compartments.for.uninfected.only),
                
                all = c(alternating.orig.from.dim.names, to.dim.names),
                
                contact = c(general.uninfected.to.dim.names, general.infected.from.dim.names),
                
                birth.proportions.uninfected.to.uninfected = c(general.uninfected.from.dim.names, general.uninfected.to.dim.names),
                birth.proportions.uninfected.to.infected = c(general.uninfected.from.dim.names, general.infected.to.dim.names),
                birth.proportions.infected.to.uninfected = c(general.infected.from.dim.names, general.uninfected.to.dim.names),
                birth.proportions.infected.to.infected = c(general.infected.from.dim.names, general.infected.to.dim.names)
            )
            
            # add in ontologies for aging
            if (!is.null(private$i.age.info) && length(private$i.age.info$endpoints)>2)
            {
                private$i.ontologies$aging.infected = private$i.ontologies$infected
                private$i.ontologies$aging.infected$age = 'all.ages.but.last'
                
                private$i.ontologies$aging.uninfected = private$i.ontologies$uninfected
                private$i.ontologies$aging.uninfected$age = 'all.ages.but.last'
            }

            # add in ontologies for transitions
            for (group in c('infected', 'uninfected'))
            {
                ont = private$i.ontologies[[group]]
                group.dimensions = names(ont)
                for (d in group.dimensions)
                {
                    transition.ontology = c(ont, ont[d])
                    names(transition.ontology)[names(transition.ontology)==d] = paste0(d, '.from')
                    names(transition.ontology)[length(transition.ontology)] = paste0(d, '.to')
                    
                    private$i.ontologies[[paste0('transition.',d,'.',group)]] = transition.ontology
                }
            }
            
            # Convert all ontologies to ontology objects
            private$i.ontologies = lapply(private$i.ontologies, as.ontology, incomplete.dimensions='location')
            
            # Some default settings
            private$i.locked = F
            
            # Add the default tracked outcomes
            private$do.store.outcome(INTRINSIC.MODEL.OUTCOME$new('infected', version=self$version),
                                     error.prefix="Error creating default outcome for 'infected' population")
            private$do.store.outcome(INTRINSIC.MODEL.OUTCOME$new('uninfected', version=self$version),
                                     error.prefix="Error creating default outcome for 'uninfected' population")
        #    private$do.store.outcome(INTRINSIC.MODEL.OUTCOME$new('population'),
        #                             error.prefix="Error creating default outcome for total population")
            

            #-- We're done! --#
        },
        
        print = function(...)
        {
            cat("A JHEEM model specification for version '",
                private$i.version, "'\n", sep='')
        
            invisible(self)
        },
        
        ##----------------##
        ##-- FIX STRATA --##
        ##----------------##
        
        register.fixed.strata = function(applies.after.time,
                                        applies.before.time,
                                        fix.strata,
                                        dimensions.to.fix,
                                        error.prefix = 'Cannot register to fix model strata: ')
        {
            fixed.strata.info = FIXED.STRATA.INFO$new(applies.after.time = applies.after.time,
                                                  applies.before.time = applies.before.time,
                                                  fix.strata = fix.strata,
                                                  dimensions.to.fix = dimensions.to.fix)
            
            for (other.fix.info in private$i.fixed.strata.info)
            {
                if (fixed.strata.info$overlaps(other.fix.info) &&
                    (fix.strata != other.fix.info$fix.strata || 
                     !setequal(dimensions.to.fix, other.fix.info$dimensions.to.fix)))
                {
                    stop(paste0(error.prefix, "The interval [", applies.after.time, ", ", applies.before.time, "] ",
                                "overlaps previously-registered fix strata information on the interval [",
                                other.fix.info$applies.after.time, ", ", other.fix.info$applies.before.time, "]"))
                }
            }
            
            # First registered takes priority
            private$i.fixed.strata.info = c(private$i.fixed.strata.info, list(fixed.strata.info))
        },
        
        ##-----------------------------------------##
        ##-- REGISTERING ELEMENTS and QUANTITIES --##
        ##-----------------------------------------##
        
        register.element = function(name,
                                    scale,
                                    
                                    dimensions=NULL,
                                    dimension.values=NULL,
                                    resolve.dimension.values.against.model=T,
                                    
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
                                        resolve.dimension.values.against.model = resolve.dimension.values.against.model,
                                        
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
            
            private$do.store.quantity(element, error.prefix=error.prefix)
        },
        
        register.quantity = function(name,
                                     value,
                                     value.name = NULL,
                                     scale=NULL,
                                     dimensions=names(dimension.values),
                                     dimension.values=NULL,
                                     resolve.dimension.values.against.model=T,
                                     na.replacement=as.numeric(NA),
                                     ...,
                                     error.prefix=NULL)
        {
            #-- Validate name --#
            validate.quantity.name(name, descriptor = 'model quantity',
                                   error.prefix = ifelse(is.null(error.prefix), "Cannot register model quantity: ", error.prefix))
            
            private$do.register.quantity(name = name,
                                     value = value,
                                     value.name = value.name,
                                     scale = scale,
                                     dimensions = dimensions,
                                     dimension.values = dimension.values,
                                     resolve.dimension.values.against.model = resolve.dimension.values.against.model,
                                     na.replacement = na.replacement,
                                     ...,
                                     error.prefix = error.prefix)
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
        
        ##---------------------------------##
        ##-- MISC REGISTRATION FUNCTIONS --##
        ##---------------------------------##
        
        register.foreground = function(foreground, name)
        {
            if (!is(foreground, 'jheem.model.foreground'))
                stop("Cannot register model foreground: foreground must be an object of class 'jheem.model.foreground', as created by create.model.foreground()")
            
            if (!is.character(name) || length(name)!=1 || is.na(name) || nchar(name)==0)
                stop("Cannot register model foreground: 'name' must be a single, non-NA, non-empty character value")
            
            if (any(names(private$i.foregrounds)==name))
                stop("Cannot register model foreground: a foreground with name '", name, 
                     "' has already been registered to the '", private$i.version, "' specification")
            
            
            private$i.foregrounds[[name]] = foreground
            
            invisible(self)
        },
        
        register.default.parameter.values = function(parameter.values)
        {
            if (!is.numeric(parameter.values))
                stop("Cannot register model default parameter values: 'parameter.values' must be a numeric vector")
            
            if (length(parameter.values)==0)
                stop("Cannot register model default parameter values: 'parameter.values' cannot be an empty vector")
            
            if (any(is.na(parameter.values)))
                stop("Cannot register model default parameter values: 'parameter.values' cannot contain NA values")
            
            if (is.null(names(parameter.values)))
                stop("Cannot register model default parameter values: 'parameter.values' must be a NAMED numeric vector")
            
            if (any(is.na(names(parameter.values))) || any(nchar(names(parameter.values))==0))
                stop("Cannot register model default parameter values: the names of 'parameter.values' must be non-empty and non-NA")
            
            if (max(table(names(parameter.values)))>1)
                stop("Cannot register model default parameter values: the names of 'parameter.values' must be unique")
            
            
            private$i.default.parameter.values[names(parameter.values)] = parameter.values
            invisible(self)
        },
        
        
        ##---------------------------------##
        ##-- REGISTERING CORE COMPONENTS --##
        ##---------------------------------##
        
        
        register.mortality = function(mortality.rate.value,
                                      groups = c('uninfected','infected'),
                                      tag = 'mortality',
                                      applies.to=list(),
                                      error.prefix = "Error registering mortality: ")
        {
            mortality.rate.quantity.name = private$do.register.quantity.if.needed(value = mortality.rate.value,
                                                                                  value.name = 'mortality.rate.value',
                                                                                  scale = 'rate',
                                                                                  error.prefix = error.prefix)
            
            private$check.groups(groups)
            
            for (one.group in groups)
            {
                args = list(group = one.group,
                            tag = tag,
                            applies.to = applies.to)
                
                private$do.register.core.component(type='mortality',
                                                   args = args,
                                                   error.prefix = error.prefix)
             
                
                private$do.register.mechanism(type = 'mortality.rate', 
                                              quantity.name = mortality.rate.quantity.name,
                                              tags = tag,
                                              args = args, 
                                              error.prefix = error.prefix)   
            }
        },

        
        register.natality = function(parent.groups,
                                     child.groups,
                                     fertility.rate.value,
                                     birth.proportions.value,
                                     parent.child.concordant.dimensions=character(),
                                     all.births.from.compartments = list(),
                                     all.births.into.compartments=character(),
                                     tag=NULL,
                                     applies.to=list(),
                                     na.replacement = as.numeric(NA),
                                     ...,
                                     error.prefix = "Error registering natality: ")
        {
            fertility.rate.quantity.name = private$do.register.quantity.if.needed(value = fertility.rate.value,
                                                                                  value.name = 'fertility.rate.value',
                                                                                  scale = 'rate',
                                                                                  error.prefix = error.prefix)
            
            birth.proportions.quantity.name = private$do.register.quantity.if.needed(value = birth.proportions.value,
                                                                                     value.name = 'birth.proportions.value',
                                                                                     scale = 'proportion',
                                                                                     error.prefix = error.prefix)
            
            private$check.groups(parent.groups, 'parent.groups')
            private$check.groups(child.groups, 'child.groups')
            
            for (one.parent.group in parent.groups)
            {
                for (one.child.group in child.groups)
                {
                    args = list(parent.group = one.parent.group,
                                child.group = one.child.group,
                                tag = tag,
                                applies.to = applies.to,
                                all.births.from.compartments = all.births.from.compartments,
                                all.births.into.compartments = all.births.into.compartments,
                                parent.child.concordant.dimensions = parent.child.concordant.dimensions)
                    
                    private$do.register.core.component(type='natality',
                                                       args = args,
                                                       error.prefix = error.prefix)
                    
                    private$do.register.mechanism(type = 'fertility.rate', 
                                                  quantity.name = fertility.rate.quantity.name,
                                                  tags = tag,
                                                  args = args, 
                                                  error.prefix = error.prefix)   
                    
                    private$do.register.mechanism(type = 'birth.proportions', 
                                                  quantity.name = birth.proportions.quantity.name,
                                                  tags = tag,
                                                  args = args, 
                                                  error.prefix = error.prefix)
                }
            }
        },
        
        register.remission = function(remission.rate.value,
                                      remission.proportions.value,
                                      tag = 'remission',
                                      applies.to = list(),
                                      all.remissions.into.compartments = character(),
                                      na.replacement = as.numeric(NA),
                                      ...,
                                      error.prefix = "Error registering remission: ")
        {
            remission.rate.quantity.name = private$do.register.quantity.if.needed(value = remission.rate.value,
                                                                          value.name = 'remission.rate.value',
                                                                          scale = 'rate',
                                                                          error.prefix = error.prefix)
            
            remission.proportions.quantity.name = private$do.register.quantity.if.needed(value = remission.proportions.value,
                                                                                 value.name = 'remission.proportions.value',
                                                                                 scale = 'proportion',
                                                                                 error.prefix = error.prefix)
            
            args = list(tag = tag,
                        applies.to = applies.to,
                        all.remissions.into.compartments = all.remissions.into.compartments)
            
            private$do.register.core.component(type='remission',
                                               args = args,
                                               error.prefix = error.prefix)
            
            private$do.register.mechanism(type = 'remission.rate', 
                                          quantity.name = remission.rate.quantity.name,
                                          tags = tag,
                                          args = args, 
                                          error.prefix = error.prefix)   
            
            private$do.register.mechanism(type = 'remission.proportions', 
                                          quantity.name = remission.proportions.quantity.name,
                                          tags = tag,
                                          args = args, 
                                          error.prefix = error.prefix)
        },
        
        register.aging = function(aging.rate.value,
                                  tag = 'aging',
                                  groups = c('uninfected','infected'),
                                  applies.to = list(),
                                  na.replacement = as.numeric(NA),
                                  ...,
                                  error.prefix = "Error registering aging: ")
        {
            aging.rate.quantity.name = private$do.register.quantity.if.needed(value = aging.rate.value,
                                                                      value.name = 'aging.rate.value',
                                                                      scale = 'rate',
                                                                      error.prefix = error.prefix)
            
            private$check.groups(groups)
            
            for (one.group in groups)
            {
                args = list(group = one.group,
                            tag = tag,
                            applies.to = applies.to)
                
                private$do.register.core.component(type='aging',
                                                   args = args,
                                                   error.prefix = error.prefix)
                
                
                private$do.register.mechanism(type = 'aging.rate', 
                                              quantity.name = aging.rate.quantity.name,
                                              tags = tag,
                                              args = args, 
                                              error.prefix = error.prefix)   
            }
        },
        
        register.transmission = function(contact.value,
                                         susceptibility.value,
                                         transmissibility.value,
                                         new.infection.proportions.value,
                                         tag=NULL,
                                         all.new.infections.into.compartments = list(),
                                         from.applies.to=list(),
                                         to.applies.to=list(),
                                         transmission.applies.to=list(),
                                         new.infections.applies.to=list(),
                                         na.replacement = as.numeric(NA),
                                         ...,
                                         error.prefix = "Error registering transmission: ")
        {
            contact.quantity.name = private$do.register.quantity.if.needed(value = contact.value,
                                                                   value.name = 'contact.value',
                                                                   scale = 'non.negative.number',
                                                                   error.prefix = error.prefix)
            
            susceptibility.quantity.name = private$do.register.quantity.if.needed(value = susceptibility.value,
                                                                          value.name = 'susceptibility.value',
                                                                          scale = 'non.negative.number',
                                                                          error.prefix = error.prefix)
            
            transmissibility.quantity.name = private$do.register.quantity.if.needed(value = transmissibility.value,
                                                                            value.name = 'transmissibility.value',
                                                                            scale = 'non.negative.number',
                                                                            error.prefix = error.prefix)
            
            new.infection.proportions.quantity.name = private$do.register.quantity.if.needed(value = new.infection.proportions.value,
                                                                                     value.name = 'new.infection.proportions.value',
                                                                                     scale = 'proportion',
                                                                                     error.prefix = error.prefix)
            
            
            # Package up and register
            args = list(tag = tag,
                        all.new.infections.into.compartments = all.new.infections.into.compartments,
                        from.applies.to = from.applies.to,
                        to.applies.to = to.applies.to,
                        transmission.applies.to = transmission.applies.to,
                        new.infections.applies.to = new.infections.applies.to)
            
            private$do.register.core.component(type='transmission',
                                               args = args,
                                               error.prefix = error.prefix)
            
            private$do.register.mechanism(type = 'contact', 
                                          quantity.name = contact.quantity.name,
                                          tags = tag,
                                          args = args, 
                                          error.prefix = error.prefix)   
            
            private$do.register.mechanism(type = 'susceptibility', 
                                          quantity.name = susceptibility.quantity.name,
                                          tags = tag,
                                          args = args, 
                                          error.prefix = error.prefix)   
            
            private$do.register.mechanism(type = 'transmissibility', 
                                          quantity.name = transmissibility.quantity.name,
                                          tags = tag,
                                          args = args, 
                                          error.prefix = error.prefix)   
            
            private$do.register.mechanism(type = 'new.infection.proportions', 
                                          quantity.name = new.infection.proportions.quantity.name,
                                          tags = tag,
                                          args = args, 
                                          error.prefix = error.prefix)
        },
        
        register.transition = function(dimension,
                                       from.compartments,
                                       to.compartments,
                                       value,
                                       groups,
                                       applies.to=list(),
                                       tag='default',
                                       na.replacement = as.numeric(NA),
                                       ...,
                                       error.prefix = "Error registering transition: ")
        {
            if (!is.character(dimension) || length(dimension)!=1 || is.na(dimension))
                stop("'dimension' must be a single, non-NA, character value")
            
            private$check.groups(groups)
            
            for (group in groups)
            {
                if (all(dimension!=names(private$i.ontologies[[group]])))
                    stop(paste0("'", dimension, "' is not a valid dimension for the ", group, " group"))
            }
            
            transition.rate.quantity.name = private$do.register.quantity.if.needed(value = value,
                                                                           value.name = 'value',
                                                                           scale = 'rate',
                                                                           error.prefix = error.prefix,
                                                                           na.replacement = na.replacement,
                                                                           ...)
            
            for (one.group in groups)
            {
                args = list(group = one.group,
                            tag = tag,
                            applies.to = applies.to,
                            dimension = dimension,
                            from.compartments = from.compartments,
                            to.compartments = to.compartments)
                
                private$do.register.core.component(type='transition',
                                                   args = args,
                                                   error.prefix = error.prefix)
                
                
                private$do.register.mechanism(type = 'transition.rate', 
                                              quantity.name = transition.rate.quantity.name,
                                              tags = tag,
                                              args = args, 
                                              error.prefix = error.prefix)   
            }
        },
        
        register.initial.population = function(value,
                                               group,
                                               error.prefix = "Error registering initial population: ")
        {
            initial.population.quantity.name = private$do.register.quantity.if.needed(value = value,
                                                                              value.name = 'value',
                                                                              scale = 'non.negative.number',
                                                                              error.prefix = error.prefix)
            
            tag = 'initial.population'
            args = list(group = group,
                        tag = tag)
            
            private$do.register.core.component(type='initial.population',
                                               args = args,
                                               error.prefix = error.prefix)
            
            
            private$do.register.mechanism(type = 'initial.population', 
                                          quantity.name = initial.population.quantity.name,
                                          tags = tag,
                                          args = args, 
                                          error.prefix = error.prefix)   
        },

        ##---------------------------------##
        ##-- REGISTER TRACKED QUANTITIES --##
        ##---------------------------------##

        
        track.dynamic.outcome = function(name,
                                         outcome.metadata,
                                         dynamic.quantity.name,
                                         denominator.outcome = NULL,
                                         
                                         include.tags = NULL,
                                         exclude.tags = NULL,
                                         groups = NULL,
                                         sub.versions = NULL,
                                         
                                         multiply.by = NULL,
                                         corresponding.data.outcome = corresponding.data.outcome,
                                         keep.dimensions = NULL,
                                         exclude.dimensions = NULL,
                                         subset.dimension.values = NULL,
                                         rename.dimension.values = NULL,
                                         dimension.aliases = NULL,
                                         dimension.alias.suffix = NULL,
                                         scale = NULL,
                                         from.year = -Inf,
                                         to.year = Inf,
                                         save = T)
        {
            multiply.by = private$do.register.quantity.if.needed(value = multiply.by,
                                                                 value.name = 'multiply.by',
                                                                 error.prefix = "Error processing 'multiply.by' for tracked dynamic outcome: ")
            
            outcome = DYNAMIC.MODEL.OUTCOME$new(name = name,
                                                version = self$version,
                                                sub.versions = sub.versions,
                                                outcome.metadata = outcome.metadata,
                                                dynamic.quantity.name = dynamic.quantity.name,
                                                denominator.outcome = denominator.outcome,
                                                multiply.by = multiply.by,
                                                corresponding.data.outcome = corresponding.data.outcome,
                                                include.tags = include.tags,
                                                exclude.tags = exclude.tags,
                                                groups = groups,
                                                keep.dimensions = keep.dimensions,
                                                exclude.dimensions = exclude.dimensions,
                                                subset.dimension.values = subset.dimension.values,
                                                rename.dimension.values = rename.dimension.values,
                                                dimension.aliases = dimension.aliases,
                                                dimension.alias.suffix = dimension.alias.suffix,
                                                scale = scale,
                                                from.year = from.year,
                                                to.year = to.year,
                                                save = save)
            
            private$check.sub.versions(outcome$sub.versions,
                                       error.prefix = paste0("Cannot track dynamic outcome '", outcome$name, "': "))
            
            private$do.store.outcome(outcome,
                                     error.prefix = "Error tracking dynamic outcome: ")
        },
        
        track.transition = function(name,
                                    outcome.metadata,
                                    dimension,
                                    from.compartments,
                                    to.compartments,
                                    denominator.outcome = NULL,
                                    
                                    include.tags = NULL,
                                    exclude.tags = NULL,
                                    groups,
                                    sub.versions = NULL,
                                    
                                    multiply.by = NULL,
                                    corresponding.data.outcome = NULL,
                                    keep.dimensions = NULL,
                                    exclude.dimensions = NULL,
                                    subset.dimension.values = NULL,
                                    rename.dimension.values = NULL,
                                    dimension.aliases = NULL,
                                    dimension.alias.suffix = NULL,
                                    scale = NULL,
                                    from.year = -Inf,
                                    to.year = Inf,
                                    save = T)
        {
            multiply.by = private$do.register.quantity.if.needed(value = multiply.by,
                                                                 value.name = 'multiply.by',
                                                                 error.prefix = "Error processing 'multiply.by' for tracked transition: ")
            
            
            outcome = TRANSITION.MODEL.OUTCOME$new(name = name,
                                                   version = self$version,
                                                   sub.versions = sub.versions,
                                                   outcome.metadata = outcome.metadata,
                                                   dimension = dimension,
                                                   from.compartments = from.compartments,
                                                   to.compartments = to.compartments,
                                                   denominator.outcome = denominator.outcome,
                                                   multiply.by = multiply.by,
                                                   corresponding.data.outcome = corresponding.data.outcome,
                                                   include.tags = include.tags,
                                                   exclude.tags = exclude.tags,
                                                   groups = groups,
                                                   keep.dimensions = keep.dimensions,
                                                   exclude.dimensions = exclude.dimensions,
                                                   subset.dimension.values = subset.dimension.values,
                                                   rename.dimension.values = rename.dimension.values,
                                                   dimension.aliases = dimension.aliases,
                                                   dimension.alias.suffix = dimension.alias.suffix,
                                                   scale = scale,
                                                   from.year = from.year,
                                                   to.year = to.year,
                                                   save = save)
            
            private$check.sub.versions(outcome$sub.versions,
                                       error.prefix = paste0("Cannot track transition '", outcome$name, "': "))
            
            private$do.store.outcome(outcome,
                                     error.prefix = "Error tracking transition: ")
        },
        
        track.integrated.outcome = function(specification,
                                            name,
                                            outcome.metadata,
                                            
                                            value.to.integrate,
                                            value.is.numerator,
                                            denominator.outcome = NULL,
                                            multiply.by = NULL,
                                            sub.versions = NULL,
                                            
                                            corresponding.data.outcome = NULL,
                                            keep.dimensions = NULL,
                                            exclude.dimensions = NULL,
                                            subset.dimension.values = NULL,
                                            rename.dimension.values = NULL,
                                            dimension.aliases = NULL,
                                            dimension.alias.suffix = NULL,
                                            from.year = -Inf,
                                            to.year = Inf,
                                            scale = NULL,
                                            save = T)
        {
            outcome = INTEGRATED.MODEL.OUTCOME$new(name = name,
                                                   version = self$version,
                                                   sub.versions = sub.versions,
                                                   outcome.metadata = outcome.metadata,
                                                   value.to.integrate = value.to.integrate,
                                                   value.is.numerator = value.is.numerator,
                                                   denominator.outcome = denominator.outcome,
                                                   multiply.by = multiply.by,
                                                   corresponding.data.outcome = corresponding.data.outcome,
                                                   keep.dimensions = keep.dimensions,
                                                   exclude.dimensions = exclude.dimensions,
                                                   subset.dimension.values = subset.dimension.values,
                                                   rename.dimension.values = rename.dimension.values,
                                                   dimension.aliases = dimension.aliases,
                                                   dimension.alias.suffix = dimension.alias.suffix,
                                                   from.year = from.year,
                                                   to.year = to.year,
                                                   scale = scale,
                                                   save = save)
            
            private$check.sub.versions(outcome$sub.versions,
                                       error.prefix = paste0("Cannot track integrated outcome '", outcome$name, "': "))
            
            private$do.store.outcome(outcome,
                                     error.prefix = "Error tracking integrated outcome: ")
        },
        
        track.cumulative.outcome = function(name,
                                            outcome.metadata,
                                            value,
                                            value.is.numerator,
                                            sub.versions = NULL,
                                            
                                            denominator.outcome = NULL,
                                            corresponding.data.outcome = NULL,
                                            keep.dimensions = NULL,
                                            exclude.dimensions = NULL,
                                            subset.dimension.values = NULL,
                                            rename.dimension.values = NULL,
                                            dimension.aliases = NULL,
                                            dimension.alias.suffix = NULL,
                                            scale = NULL,
                                            from.year = -Inf,
                                            to.year = Inf,
                                            save = T)
        {
            outcome = CUMULATIVE.MODEL.OUTCOME$new(name = name,
                                                   version = self$version,
                                                   sub.versions = sub.versions,
                                                   outcome.metadata = outcome.metadata,
                                                   value = value,
                                                   value.is.numerator = value.is.numerator,
                                                   denominator.outcome = denominator.outcome,
                                                   corresponding.data.outcome = corresponding.data.outcome,
                                                   keep.dimensions = keep.dimensions,
                                                   exclude.dimensions = exclude.dimensions,
                                                   subset.dimension.values = subset.dimension.values,
                                                   rename.dimension.values = rename.dimension.values,
                                                   dimension.aliases = dimension.aliases,
                                                   dimension.alias.suffix = dimension.alias.suffix,
                                                   from.year = from.year,
                                                   to.year = to.year,
                                                   scale = scale,
                                                   save = save)
            
            private$check.sub.versions(outcome$sub.versions,
                                       error.prefix = paste0("Cannot track cumulative outcome '", outcome$name, "': "))
            
            private$do.store.outcome(outcome,
                                     error.prefix = "Error tracking cumulative outcome: ")
        },
        
        track.point.outcome = function(name,
                                       outcome.metadata,
                                       value,
                                       value.is.numerator,
                                       sub.versions = NULL,
                                       
                                       denominator.outcome = NULL,
                                       corresponding.data.outcome = NULL,
                                       keep.dimensions = NULL,
                                       exclude.dimensions = NULL,
                                       subset.dimension.values = NULL,
                                       rename.dimension.values = NULL,
                                       dimension.aliases = NULL,
                                       dimension.alias.suffix = NULL,
                                       scale = NULL,
                                       from.year = -Inf,
                                       to.year = Inf,
                                       save = T)
        {
            outcome = POINT.MODEL.OUTCOME$new(name = name,
                                              version = self$version,
                                              sub.versions = sub.versions,
                                              outcome.metadata = outcome.metadata,
                                              value = value,
                                              value.is.numerator = value.is.numerator,
                                              denominator.outcome = denominator.outcome,
                                              corresponding.data.outcome = corresponding.data.outcome,
                                              keep.dimensions = keep.dimensions,
                                              exclude.dimensions = exclude.dimensions,
                                              subset.dimension.values = subset.dimension.values,
                                              rename.dimension.values = rename.dimension.values,
                                              dimension.aliases = dimension.aliases,
                                              dimension.alias.suffix = dimension.alias.suffix,
                                              scale = scale,
                                              from.year = from.year,
                                              to.year = to.year,
                                              save = save)
            
            private$check.sub.versions(outcome$sub.versions,
                                       error.prefix = paste0("Cannot track point outcome '", outcome$name, "': "))
            
            private$do.store.outcome(outcome,
                                     error.prefix = "Error tracking point outcome: ")
        },
        
        track.cumulative.proportion.from.rate = function(name,
                                                         outcome.metadata,
                                                         rate.value,
                                                         
                                                         denominator.outcome,
                                                         sub.versions = NULL,
                                                         calculate.proportion.leaving = T,
                                                         corresponding.data.outcome = NULL,
                                                         keep.dimensions = NULL,
                                                         exclude.dimensions = NULL,
                                                         subset.dimension.values = NULL,
                                                         rename.dimension.values = NULL,
                                                         dimension.aliases = NULL,
                                                         dimension.alias.suffix = NULL,
                                                         from.year = -Inf,
                                                         to.year = Inf,
                                                         save = T)
        {
            outcome = RATE.TO.PROPORTION.MODEL.OUTCOME$new(name = name,
                                                           version = self$version,
                                                           sub.versions = sub.versions,
                                                           outcome.metadata = outcome.metadata,
                                                           rate.value = rate.value,
                                                           denominator.outcome = denominator.outcome,
                                                           calculate.proportion.leaving = calculate.proportion.leaving,
                                                           corresponding.data.outcome = corresponding.data.outcome,
                                                           keep.dimensions = keep.dimensions,
                                                           exclude.dimensions = exclude.dimensions,
                                                           subset.dimension.values = subset.dimension.values,
                                                           rename.dimension.values = rename.dimension.values,
                                                           dimension.aliases = dimension.aliases,
                                                           dimension.alias.suffix = dimension.alias.suffix,
                                                           from.year = from.year,
                                                           to.year = to.year,
                                                           save = save)
            
            private$check.sub.versions(outcome$sub.versions,
                                       error.prefix = paste0("Cannot track cumulative proportion from rate '", outcome$name, "': "))
            
            private$do.store.outcome(outcome,
                                     error.prefix = "Error tracking rate-to-proportion outcome: ")
        },
        
        ##---------------##
        ##-- COMPILING --##
        ##---------------##
    
        #to be called by code in the package
        compile = function()
        {            
            is.recursive.call = is(parent.env(parent.frame()), 'jheem.specification')
            # ^true if the call to compile on THIS specification was a recursive call 
            #   on the parent.specification within the compile() method of a
            #   descendant specification
            
            if (!is.recursive.call && 
                (private$i.locked || !is.specification.registered.for.version(private$i.version) ||
                 is.compiled.specification.registered.for.version(private$i.version)))
                stop("compile() should not be invoked directly for a JHEEM specification object - use register.model.specification() to register the specification instead")
            
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

                                                  compartments = private$i.compartments,
                                                  
                                                  fixed.strata.info = private$i.fixed.strata.info,
                                                  quantities = private$i.quantities,
                                                  outcomes = private$i.outcomes,
                                                  core.components = private$i.core.components,
                                                  mechanisms = private$i.mechanisms,
                                                  
                                                  foregrounds = private$i.foregrounds,
                                                  default.parameter.values = private$i.default.parameter.values,
                                                  
                                                  age.info = private$i.age.info,
                                                  start.year = private$i.start.year,

                                                  parent.specification = compiled.parent,
                                                  do.not.inherit.model.quantity.names = private$i.do.not.inherit.model.quantity.names,
                                                  do.not.inherit.model.outcome.names = private$i.do.not.inherit.model.outcome.names,
                                                  do.not.inherit.transitions.for.dimension = private$i.do.not.inherit.transitions.for.dimension,
                                                  do.not.inherit.components.with.tags = private$i.do.not.inherit.components.with.tags,
                                                  
                                                  do.compile = !is.recursive.call)
            
            
            if (!is.recursive.call)
                private$i.locked = T
            
            rv
        }
    ),

##-- ACTIVE - GETTERS --##
    active = list(
        
        sub.versions = function(value)
        {
            if (missing(value))
                private$i.sub.versions
            else
                stop("Cannot modify 'sub.versions' for a jheem.specification - they are read-only")
        },
        
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
        
        start.year = function(value)
        {
            if (missing(value))
                private$i.start.year
            else
                stop("Cannot modify 'start.year' for a jheem.specification - it is read-only")
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

        is.locked = function(value)
        {
            if (missing(value))
                private$i.locked
            else
                stop("Cannot modify a specification's 'is.locked' value - it is read-only")
        },
        
        quantity.names = function(value)
        {
            if (missing(value))
                names(private$i.quantities)
            else
                stop("Cannot modify a specification's 'quantity.names' - they are read-only")
        },
        
        outcome.names = function(value)
        {
            if (missing(value))
                names(private$i.outcomes)
            else
                stop("Cannot modify a specification's 'outcome.names' - they are read-only")
        },
        
        default.parameter.values = function(value)
        {
            if (missing(value))
                private$i.default.parameter.values
            else
                stop("Cannot modify a specification's 'default.parameter.values' - they are read-only")
        }
    ),

    ##-- PRIVATE --##
    private = list(
    
    ##-- MEMBER VARIABLES --##
    
        i.locked = NULL,
        
        i.version = NULL,
        i.iteration = NULL,
        i.description = NULL,
        i.sub.versions = NULL,
        
        i.compartment.value.character.aliases = NULL,
        i.compartment.value.function.aliases = NULL,
        
        i.ontologies = NULL,
        i.compartments = NULL,
        
        i.foregrounds = NULL,
        i.default.parameter.values = NULL,
        
        i.fixed.strata.info = NULL,
        i.quantities = NULL,
        i.outcomes = NULL,
        i.core.components = NULL,
        i.mechanisms = NULL,
        
        i.age.info = NULL,
        i.start.year = NULL,

        i.parent.specification = NULL,
        i.do.not.inherit.model.quantity.names = NULL,
        i.do.not.inherit.model.outcome.names = NULL,
        i.do.not.inherit.transitions.for.dimension = NULL,
        i.do.not.inherit.components.with.tags = NULL,
        
        i.internal.name.counter = 0,
        
    ##-- INTERNAL METHODS --##
    
        check.lock = function(error.prefix)
        {
            if (private$i.locked)
                stop(paste0(error.prefix, "The '", private$i.version, "' specification has already been registered and cannot be modified further"))
        },
    
        check.sub.versions = function(sub.versions, error.prefix)
        {
            invalid.sub.versions = setdiff(sub.versions, private$i.sub.versions)
            if (length(invalid.sub.versions)>0)
                stop(paste0(error.prefix,
                            collapse.with.and("'", invalid.sub.versions, "'"),
                            ifelse(length(invalid.sub.versions)==1, 
                                   " is not a sub-version",
                                   " are not sub-versions"),
                            " in the '", private$i.version, "' specification. ",
                            ifelse(length(private$i.sub.versions)==0,
                                   "There are no sub-versions registered",
                                   paste0("Sub-versions must be ",
                                          ifelse(length(private$i.sub.versions)==1, "", "one of "),
                                          collapse.with.or("'", private$i.sub.versions, "'")
                                   ))))
        },
    
        check.groups = function(groups, groups.name.for.error='groups')
        {
            if (!is.character(groups) || length(groups)==0 || any(is.na(groups)))
                stop("'groups' must be a non-empty character vector with no NA values")
            
            allowed.groups = c('uninfected','infected')
            invalid.groups = setdiff(groups, allowed.groups)
            if (length(invalid.groups)>1)
                stop(paste0(collapse.with.and("'", invalid.groups, "'"),
                            ifelse(length(invalid.groups)==1, " is not a valid value", " are not valid values"),
                            " for '", groups.name.for.error, "' - which must be either ",
                            collapse.with.or("'", allowed.groups, "'")))
        },
        
        get.unique.internal.name = function()
        {
            private$i.internal.name.counter = private$i.internal.name.counter + 1
            paste0("quant_", private$i.internal.name.counter, "_", private$i.version)
        },
    
        do.register.quantity = function(name,
                                        value,
                                        value.name = NULL,
                                        scale=NULL,
                                        dimensions=names(dimension.values),
                                        dimension.values=NULL,
                                        resolve.dimension.values.against.model=T,
                                        na.replacement=as.numeric(NA),
                                        ...,
                                        error.prefix=NULL)
        {
            # name is already validated
            
            if (is.null(error.prefix))
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
                                                       resolve.dimension.values.against.model = resolve.dimension.values.against.model,
                                                       na.replacement = na.replacement,
                                                       ...,
                                                       error.prefix = error.prefix)
            
            private$do.store.quantity(quantity, error.prefix=error.prefix)
        },
    
        # if value is NULL, do nothing (return NULL)
        # if value is a single character value, then no need to register (return value)
        # otherwise, register a quantity under a uniquely-generated name (return the unique name)
        do.register.quantity.if.needed = function(value,
                                                  value.name,
                                                  scale=NULL,
                                                  dimensions=names(dimension.values),
                                                  dimension.values=NULL,
                                                  resolve.dimension.values.against.model=T,
                                                  na.replacement=as.numeric(NA),
                                                  ...,
                                                  error.prefix=NULL)
        {
            if (is.null(value))
                value
            else if (is.character(value))
            {
                if (length(value) != 1 || is.na(value))
                    stop(paste0(error.prefix, "If '", value.name, "' is a character, it must be a single, non-NA value"))
                
                value
            }
            else
            {
                quantity.name = private$get.unique.internal.name()
                private$do.register.quantity(name = quantity.name,
                                             value = value,
                                             value.name = value.name,
                                             scale = scale,
                                             dimensions = names(dimension.values),
                                             dimension.values = NULL,
                                             resolve.dimension.values.against.model = resolve.dimension.values.against.model,
                                             na.replacement = na.replacement,
                                             ...,
                                             error.prefix = error.prefix)
                
                quantity.name
            }
        },
    
        do.store.quantity = function(quantity, error.prefix=error.prefix)
        {
            # Check lock
            private$check.lock(error.prefix)
               
            # Check for name clashes
            if (any(self$quantity.names==quantity$name))
            {
                clashing.quantity = private$i.quantities[[quantity$name]]
                stop(paste0(error.prefix, "The name '", quantity$name, "' has already been used (for a ",
                            clashing.quantity$descriptor, ")"))
            }
            
            # Add to the list
            private$i.quantities[[quantity$name]] = quantity
            
            # Silently return self
            invisible(self)
        },
    
        do.store.outcome = function(outcome, error.prefix)
        {
            # Check lock
            private$check.lock(error.prefix)
            
            # Check for name clashes
            if (any(self$outcome.names==outcome$name))
            {
                clashing.outcome = private$i.outcome[[outcome$name]]
                stop(paste0(error.prefix, "The name '", outcome$name, "' has already been used (for a ",
                    clashing.outcome$type, ")"))
            }
            
            # Add to the list
            private$i.outcomes[[outcome$name]] = outcome
            
            # Silently return self
            invisible(self)
        },
    
    
        do.register.core.component = function(type, args, error.prefix)
        {
            private$check.lock(error.prefix)
            
            schema = CORE.COMPONENT.SCHEMATA[[type]]
            if (is.null(schema))
                stop(paste0(error.prefix, "'", type, 
                            "' is not a valid core-component type"))
            
            core.comp = schema$create.component(args=args, version=private$i.version, error.prefix=error.prefix)
            for (other.comp in private$i.core.components)
            {
                if (schema$components.clash(core.comp, other.comp))
                    stop(paste0(error.prefix, core.comp$name, " has already been registered"))
            }

            private$i.core.components = c(private$i.core.components, list(core.comp))
        },
    
        do.register.mechanism = function(type, 
                                         quantity.name,
                                         tags,
                                         args, 
                                         error.prefix)
        {
            schema = CORE.COMPONENT.SCHEMATA.FOR.MECHANISMS[[type]]
            if (is.null(schema))
                stop(paste0(error.prefix, "'", type, 
                            "' is not a valid mechanism type"))
            
            mechanism = schema$create.mechanism(type = type, 
                                                quantity.name = quantity.name,
                                                version = self$version,
                                                tags = tags,
                                                args = args,
                                                error.prefix)
            private$i.mechanisms = c(private$i.mechanisms, list(mechanism))
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

##-----------------------------##
##-----------------------------##
##-- FIXED STRATA INFO CLASS --##
##-----------------------------##
##-----------------------------##

FIXED.STRATA.INFO = R6::R6Class(
    'fixed.strata.info',
    
    public = list(
        
        initialize = function(applies.after.time,
                              applies.before.time,
                              fix.strata,
                              dimensions.to.fix,
                              error.prefix)
        {
            #-- Validate Arguments --#
            
            if (!is.numeric(applies.after.time) || length(applies.after.time)!=1 || is.na(applies.after.time))
                stop(paste0(error.prefix, "'applies.after.time' must be a single, non-NA numeric value"))
            
            if (!is.numeric(applies.before.time) || length(applies.before.time)!=1 || is.na(applies.before.time))
                stop(paste0(error.prefix, "'applies.before.time' must be a single, non-NA numeric value"))
            
            if (applies.before.time <= applies.after.time)
                stop(paste0(error.prefix, "'applies.before.time' (",
                            applies.before.time, ") must be GREATER THAN 'applies.after.time' (",
                            applies.after.time, ")"))
            
            if (!is.logical(fix.strata) || length(fix.strata)!=1 || is.na(fix.strata))
                stop(paste0(error.prefix, "'fix.strata' must be a single, non-NA logical value"))
            
            if (fix.strata)
            {
                if (is.null(dimensions.to.fix))
                    stop(paste0(error.prefix, "If 'fix.strata' is TRUE, 'dimensions.to.fix' must be specified"))
                
                if (!is.character(dimensions.to.fix) || length(dimensions.to.fix)==0 || any(is.na(dimensions.to.fix)))
                    stop(paste0(error.prefix, "If 'fix.strata' is TRUE, 'dimensions.to.fix' must be a non-empty, non-NA character vector"))
            }
            else
            {
                if (is.null(dimensions.to.fix))
                    stop(paste0(error.prefix, "If 'fix.strata' is FALSE, 'dimensions.to.fix' must be NULL"))
            }
            
            #-- Store --#
            private$i.applies.before.time = applies.before.time
            private$i.applies.after.time = applies.after.time
            private$i.fix.strata = fix.strata
            private$i.dimensions.to.fix = unique(dimensions.to.fix)
        },
        
        overlaps = function(other.info)
        {
            private$i.applies.before.time >= other.info$applies.after.time &&
                private$i.applies.after.time <= other.info$applies.before.time
        },
        
        is.valid.for.specification = function(specification)
        {
            # Make sure dimensions.to.fix are all in general dim names
        }
    ),
    
    active = list(
        
        applies.before.time = function(value)
        {
            if (missing(value))
                private$i.applies.before.time
            else
                stop("Cannot overwrite a fixed.strata.info's 'applies.before.time' - it is read-only")
        },
        
        applies.after.time = function(value)
        {
            if (missing(value))
                private$i.applies.after.time
            else
                stop("Cannot overwrite a fixed.strata.info's 'applies.after.time' - it is read-only")
        },
        
        fix.strata = function(value)
        {
            if (missing(value))
                private$i.fix.strata
            else
                stop("Cannot overwrite a fixed.strata.info's 'fix.strata' - it is read-only")
        },
        
        dimensions.to.fix = function(value)
        {
            if (missing(value))
                private$i.dimensions.to.fix
            else
                stop("Cannot overwrite a fixed.strata.info's 'dimensions.to.fix' - they are read-only")
        }
    ),
    
    private = list(
        
        i.applies.before.time = NULL,
        i.applies.after.time = NULL,
        i.fix.strata = NULL,
        i.dimensions.to.fix = NULL
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
                              resolve.dimension.values.against.model,
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
            
            #-- Validate resolve.dimension.values.against.model --#
            if (!is.logical(resolve.dimension.values.against.model) || length(resolve.dimension.values.against.model) != 1 ||
                is.na(resolve.dimension.values.against.model))
                stop(paste0(error.prefix, "'resolve.dimension.values.against.model' must be a single, non-NA logical value (T or F)"))
            
            #-- Validate dimensions and dimension.values --#

            if (!is.null(dimension.values))
            {
                check.dimension.values.valid(dimension.values = dimension.values,
                                             variable.name.for.error = 'dimension.values',
                                             refer.to.dimensions.as = 'dimension',
                                             allow.empty = F,
                                             allow.duplicate.values.within.dimensions = F,
                                             error.prefix = error.prefix)
                
                if (!resolve.dimension.values.against.model)
                {
                    if (any(!sapply(dimension.values, is.character)))
                        stop(paste0(error.prefix, "If resolve.dimension.values.against.model==F, then 'dimension.values' can only contain character vectors"))
                }
            }
            
            if (!is.null(dimensions))
            {
                if (!is.character(dimensions) || length(dimensions)==0 || any(is.na(dimensions)) || any(dimensions==''))
                    stop(paste0(error.prefix, "'dimensions' must be a non-NA character vector with at least one element"))
                if (max(table(dimensions))>1)
                    stop(paste0(error.prefix, "'dimensions' cannot have repeated values"))
                
                if (!is.null(dimension.values))
                {
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
            
            #-- Store values --#
            private$i.name = private$i.original.name = name
            private$i.version = version
            private$i.scale = scale
            private$i.type = type
            
            private$i.fixed.dimensions = dimensions
            private$i.fixed.dimension.values = dimension.values
            private$i.resolve.dimension.values.against.model = resolve.dimension.values.against.model
            
            
            #-- Set some empty values --#
            private$i.depends.on = character()
            
            private$i.dimension.aliases = character()
            private$i.reversed.aliases = character()
            private$i.maximum.dim.names = NULL
            private$i.fixed.dim.names = NULL
            
            private$i.must.be.static = F
        },
        
        set.dim.names.and.dimension.aliases = function(max.dim.names, 
                                                       required.dim.names,
                                                       max.dimensions = NULL,
                                                       dimension.aliases, 
                                                       error.prefix)
        {
            # Set the max dim names
            private$i.maximum.dim.names = max.dim.names
            
            # Set the required.dim.names
            private$i.required.dim.names = required.dim.names
            
            # Set the max.dimensions
            if (is.null(max.dim.names))
                private$i.max.dimensions = max.dimensions
            
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
            if (private$i.resolve.dimension.values.against.model)
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
        },
        
        set.must.be.static = function()
        {
            private$i.must.be.static = T
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
                stop(paste0("Cannot modify a ", self$descriptor, "'s 'max.dim.names' value - they are read-only"))
        },
        
        max.dimensions = function(value)
        {
            if (missing(value))
                private$i.max.dimensions
            else
                stop(paste0("Cannot modify a ", self$descriptor, "'s 'max.dimensions' value - they are read-only"))
        },
        
        required.dim.names = function(value)
        {
            if (missing(value))
                private$i.required.dim.names
            else
                stop(paste0("Cannot modify a ", self$descriptor, "'s 'required.dim.names' value - they are read-only"))
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
        },
        
        must.be.static = function(value)
        {
            if (missing(value))
                private$i.must.be.static
            else
                stop(paste0("Cannot modify a ", self$descriptor, "'s 'must.be.static' value - it is read-only"))
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
        i.required.dim.names = NULL,
        i.max.dimensions = NULL,
        
        i.fixed.dimensions = NULL,
        i.fixed.dimension.values = NULL,
        i.resolve.dimension.values.against.model = NULL,
        i.fixed.dim.names = NULL,
        
        i.must.be.static = NULL
    )
)

recursively.set.dim.names.and.aliases <- function(quantity, 
                                                  specification,
                                                  max.dim.names,
                                                  required.dim.names,
                                                  max.dimensions,
                                                  dimension.aliases,
                                                  error.prefix)
{
    quantity$set.dim.names.and.dimension.aliases(max.dim.names = max.dim.names,
                                              required.dim.names = required.dim.names,
                                              max.dimensions = max.dimensions,
                                              dimension.aliases = dimension.aliases,
                                              error.prefix = paste0(error.prefix, "Cannot set aliases when recursively setting dimnames for quantity ", quantity$get.original.name(specification$version), " - "))
    
    for (dep.on.name in quantity$depends.on)
    {
        dep.on.quant = specification$get.quantity(dep.on.name)
        recursively.set.dim.names.and.aliases(quantity = dep.on.quant,
                                              specification = specification,
                                              max.dim.names = max.dim.names,
                                              required.dim.names = required.dim.names,
                                              max.dimensions = max.dimensions,
                                              dimension.aliases = dimension.aliases,
                                              error.prefix = error.prefix)
    }
    
    for (possible.parent.quant in specification$quantities)
    {
        if (is.null(possible.parent.quant$max.dim.names) && any(possible.parent.quant$depends.on==quantity$name))
        {
            recursively.set.dim.names.and.aliases(quantity = possible.parent.quant,
                                                  specification = specification,
                                                  max.dim.names = max.dim.names,
                                                  required.dim.names = required.dim.names,
                                                  max.dimensions = max.dimensions,
                                                  dimension.aliases = dimension.aliases,
                                                  error.prefix = error.prefix)
        }
    }
}



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
                              resolve.dimension.values.against.model = T,
                              
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
                             resolve.dimension.values.against.model = resolve.dimension.values.against.model,
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
                    
                    if (length(value) != 1 && is.null(dimnames(value)))
                        stop(paste0(error.prefix, "'value' must either be a scalar (single) numeric value OR it must have dimnames set"))
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
                    base::print(paste0(gsub('error', 'WARNING', error.prefix, ignore.case = T),
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
        
        get.element.background = function(specification.metadata,
                                          error.prefix)
        {
            error.prefix = paste0(error.prefix, "When generating the background for model element ", 
                                  self$get.original.name(wrt.version = specification.metadata$version), ", ")
            
            value = private$i.value
            if (!is.null(private$i.get.value.function.wrapper))
            {
                if (private$i.get.value.function.name=='get.value.function')
                    get.value.function.name = "the get.value.function()"
                else
                    get.value.function.name = paste0(private$i.get.value.function.name, "()",
                                                 " (the get.value.function)")
                # get the value
                tryCatch({
                    value = private$i.get.value.function.wrapper$execute(location=specification.metadata$location, 
                                                                         specification.metadata=specification.metadata)
                },
                error = function(e){
                    e$message = paste0(error.prefix, "There was an error evaluating ", get.value.function.name,
                                " for model element ",
                                self$get.original.name(wrt.version = specification.metadata$version), ": ",
                                e$message)
                    
                    stop(e)
                })
                
                # make sure it is numeric, non-empty, non-NA
                if (!is.numeric(value))
                    stop(paste0(error.prefix, "Evaluating ", get.value.function.name, " for model element ",
                                self$get.original.name(wrt.version = specification.metadata$version),
                                " yields a non-numeric value"))
                if (length(value)==0)
                    stop(paste0(error.prefix, "Evaluating ", get.value.function.name, " for model element ",
                                self$get.original.name(wrt.version = specification.metadata$version),
                                " yields an empty (length-zero) value"))
                if (any(is.na(value)))
                    stop(paste0(error.prefix, "Evaluating ", get.value.function.name, " for model element ",
                                self$get.original.name(wrt.version = specification.metadata$version),
                                " yields NAs"))
                if (length(value) != 1 && is.null(dimnames(value)))
                    stop(paste0(error.prefix, "Evaluating ", get.value.function.name, " for model element ",
                                self$get.original.name(wrt.version = specification.metadata$version),
                                " must yield either a scalar (single) numeric value OR it must have dimnames set"))
                
                
                # make sure it accords with the expected max.dim.names
                verify.dim.names.for.quantity(dim.names = dimnames(value),
                                              quantity = self,
                                              variable.name.for.error = paste0("the value generated by ", get.value.function.name),
                                              error.prefix = error.prefix,
                                              wrt.version = specification.metadata$version)
            }
            
            functional.form = private$i.functional.form
            if (!is.null(private$i.get.functional.form.function.wrapper))
            {
                if (private$i.get.functional.form.function.name=='get.functional.form.function')
                    get.functional.form.function.name = "the get.functional.form.function()"
                else
                    get.functional.form.function.name = paste0(private$i.get.functional.form.function.name, "()",
                                                     " (the get.functional.form.function)")
                
                # get the functional.form
                tryCatch({
                    functional.form = private$i.get.functional.form.function.wrapper$execute(location=specification.metadata$location, 
                                                                                             specification.metadata=specification.metadata)
                },
                error = function(e){
                    e$message = paste0(error.prefix, "There was an error evaluating ",
                                       get.functional.form.function.name,
                                       " for model element ",
                                       self$get.original.name(wrt.version = specification.metadata$version), ": ",
                                       e$message)
                    stop(e)
                })
                
                # make sure it is a functional form object
                if (!is(functional.form, 'functional.form'))
                    stop(paste0(error.prefix, "Evaluating ", get.functional.form.function.name, " for model element ",
                                self$get.original.name(wrt.version = specification.metadata$version),
                                " yields a value that is NOT an object of class 'functional.form'"))
                    
                # make sure it's minimum.dim.names accords with the expected max.dim.names
                verify.dim.names.for.quantity(dim.names = functional.form$minimum.dim.names,
                                              quantity = self,
                                              variable.name.for.error = paste0("the minimum dimnames of the functional.form generated by ", get.functional.form.function.name),
                                              error.prefix = error.prefix,
                                              wrt.version = specification.metadata$version)
                
                # If this must be static, then make sure this this is a static model
                if (private$i.must.be.static && !functional.form$is.static)
                {
                    stop(paste0(error.prefix, "evaluating ", get.functional.form.function.name, " for model element ",
                                self$get.original.name(wrt.version = specification.metadata$version),
                                " yields a functional.form that is NOT static but, according to the outcomes defined in the model specification, it MUST be static."))
                }
                
                # If no from/to times have been set, then make sure this is a static model
                if (!functional.form$is.static &&
                    (is.null(private$i.functional.form.from.time) || is.null(private$i.functional.form.to.time)))
                {
                    stop(paste0(error.prefix, "evaluating ", get.functional.form.function.name, " for model element ",
                                self$get.original.name(wrt.version = specification.metadata$version),
                                " yields a functional.form that is NOT static. However, no functional.form.from.time or functional.form.to.time were set for the element. These must either be set, or ",
                                get.functional.form.function.name, " must return a static functional form"))
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
        
        # calculate.ramp.values = function(ramp.values,
        #                                  ramp.times,
        #                                  first.functional.form.value,
        #                                  functional.form.from.time)
        # {
        #     times = ramp.times
        #     if (i.ramp.value.application=='absolute')
        #     {
        #         if (length(dim(first.functional.form.value))==0)
        #             rv = as.list(ramp.values)
        #         else
        #             rv = lapply(ramp.values, function(val){
        #                 array(val, dim=dim(first.functional.form.value), dimnames=dimnames(first.functional.form.value))
        #             })
        #     }
        #     else
        #     {
        #         if (i.all.ramp.applications.identity)
        #         {
        #             rv = lapply(ramp.values, function(val){
        #                 val * first.functional.form.value
        #             })
        #         }
        #         else
        #         {
        #             n.segments = length(ramp.times)
        #             times = c(ramp.times, functional.form.from.time)
        #             
        #             multipliers = unlist(sapply(1:n.segments, function(i){
        #                 interpolate.times = times[i]:times[i+1]
        #                 interpolate.times = interpolate.times[-length(interpolate.times)]
        #                 
        #                 if (private$i.ramp.interpolate.links[i]=='log')
        #                     exp(interpolate(values=log(ramp.values[i:(i+1)]),
        #                                     value.times=times[i:(i+1)],
        #                                     desired.times = interpolate.times))
        #                 else if (private$i.ramp.interpolate.links[i]=='exp')
        #                     log(interpolate(values=exp(ramp.values[i:(i+1)]),
        #                                     value.times=times[i:(i+1)],
        #                                     desired.times = interpolate.times))
        #                 else #identity
        #                     ramp.values[i]
        #             }))
        #             
        #             rv = lapply(multipliers, function(mult){
        #                 mult * first.functional.form.value
        #             })
        #             times = calculate.ramp.interpolated.times(ramp.times, functional.form.from.time)
        #         }
        #     }
        #     
        #     names(rv) = as.character(times)
        #     rv
        # },
        # 
        # calculate.taper.value = function(taper.values,
        #                                  taper.times,
        #                                  last.functional.form.value,
        #                                  functional.form.to.time)
        # {
        #     times = taper.times
        #     if (i.taper.value.application=='absolute')
        #     {
        #         if (length(dim(first.functional.form.value))==0)
        #             rv = as.list(taper.values)
        #         else
        #             rv = lapply(taper.values, function(val){
        #                 array(val, dim=dim(last.functional.form.value), dimnames=dimnames(last.functional.form.value))
        #             })
        #     }
        #     else
        #     {
        #         if (i.all.taper.applications.identity)
        #         {
        #             rv = lapply(taper.values, function(val){
        #                 val * last.functional.form.value
        #             })
        #         }
        #         else
        #         {
        #             n.segments = length(taper.times)
        #             times = c(functional.form.to.time, taper.times)
        #             
        #             multipliers = unlist(sapply(1:n.segments, function(i){
        #                 interpolate.times = times[i+1]:times[i]
        #                 interpolate.times = rev(interpolate.times[-1])
        #                 
        #                 if (private$i.taper.interpolate.links[i]=='log')
        #                     exp(interpolate(values=log(taper.values[i:(i+1)]),
        #                                     value.times=times[i:(i+1)],
        #                                     desired.times = interpolate.times))
        #                 else if (private$i.taper.interpolate.links[i]=='exp')
        #                     log(interpolate(values=exp(taper.values[i:(i+1)]),
        #                                     value.times=times[i:(i+1)],
        #                                     desired.times = interpolate.times))
        #                 else #identity
        #                     taper.values[i]
        #             }))
        #             
        #             rv = lapply(multipliers, function(mult){
        #                 mult * last.functional.form.value
        #             })
        #             times = calculate.taper.interpolated.times(taper.times, functiona.form.to.time)
        #         }
        #     }
        #     
        #     names(rv) = as.character(times)
        #     rv
        # },
        
        # calculate.ramp.interpolated.times = function(ramp.times,
        #                                              functional.form.from.time)
        # {
        #     if (i.all.ramp.applications.identity)
        #         ramp.times
        #     else
        #     {
        #         times = c(ramp.times, functional.form.from.time)
        #         n.segments = length(ramp.times)
        #         unlist(sapply(1:n.segments, function(i){
        #             if (private$i.ramp.interpolate.links[i]=='identity')
        #                 times[i]
        #             else #not identity
        #             {
        #                 rv = times[i]:times[i+1]
        #                 rv[-length(rv)]
        #             }
        #         }))
        #     }
        # },
        # 
        # calculate.taper.interpolated.times = function(taper.times,
        #                                  functional.form.to.time)
        # {
        #     if (i.all.taper.applications.identity)
        #         taper.times
        #     else
        #     {
        #         times = c(functional.form.to.time, taper.times)
        #         n.segments = length(taper.times)
        #         unlist(sapply(1:n.segments, function(i){
        #             if (private$i.taper.interpolate.links[i]=='identity')
        #                 times[i]
        #             else #not identity
        #             {
        #                 rv = times[i+1]:times[i]
        #                 rev(rv[-1])
        #             }
        #         }))
        #     }
        # },
        # 
        may.be.static = function()
        {
            !is.null(private$i.value) || !is.null(private$i.get.value.function.wrapper) ||
                (( (!is.null(private$i.functional.form) && private$i.functional.form$is.static) ||
                     !is.null(private$i.get.functional.form.function.wrapper)) && 
                    is.null(private$i.ramp.times) && is.null(private$i.taper.times)
                )
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
        },
        
        ramp.value.application = function(value)
        {
            if (missing(value))
                length(private$i.ramp.value.application)
            else
                stop("Cannot modify a model.element's 'ramp.value.application' value - it is read-only")
        },
        
        all.ramp.applications.identity = function(value)
        {
            if (missing(value))
                length(private$i.all.ramp.applications.identity)
            else
                stop("Cannot modify a model.element's 'all.ramp.applications.identity' value - it is read-only")
        },
        
        ramp.interpolate.links = function(value)
        {
            if (missing(value))
                length(private$i.ramp.interpolate.links)
            else
                stop("Cannot modify a model.element's 'ramp.interpolate.links' value - it is read-only")
        },
        
        taper.value.application = function(value)
        {
            if (missing(value))
                length(private$i.taper.value.application)
            else
                stop("Cannot modify a model.element's 'taper.value.application' value - it is read-only")
        },
        
        all.taper.applications.identity = function(value)
        {
            if (missing(value))
                length(private$i.all.taper.applications.identity)
            else
                stop("Cannot modify a model.element's 'all.taper.applications.identity' value - it is read-only")
        },
        
        taper.interpolate.links = function(value)
        {
            if (missing(value))
                length(private$i.taper.interpolate.links)
            else
                stop("Cannot modify a model.element's 'taper.interpolate.links' value - it is read-only")
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
                              resolve.dimension.values.against.model=T,
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
                             resolve.dimension.values.against.model = resolve.dimension.values.against.model,
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
                             function.arguments.to.be.supplied.later = c('specification.metadata', 'location'),
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
                non.reserved.depends.on = setdiff(private$i.depends.on, c('specification.metadata','location'))
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
                if (is.null(private$i.parent.quantity$max.dim.names))
                    NULL
                else
                {
                    rv = private$i.parent.quantity$max.dim.names
                    rv[names(private$i.applies.to)] = private$i.applies.to
                    rv
                }
            }
            else
                stop("Cannot modify a model.quantity.component's 'max.dim.names' - it is read-only")
        },
        
        max.dimensions = function(value)
        {
            if (missing(value))
                private$i.parent.quantity$max.dimensions
            else
                stop("Cannot modify a model.quantity.component's 'max.dimensions' - it is read-only")
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

##------------------------------------##
##------------------------------------##
##-- CORE COMPONENT CLASS HIERARCHY --##
##------------------------------------##
##------------------------------------##

CORE.COMPONENT.SCHEMA = R6::R6Class(
    'core.component.schema',
    portable = F,
    
    public = list(
        
        initialize = function(type,
                              mechanism.types,
                              ontology.name.for.mechanism,
                              applies.to.name.for.mechanism,
                              required,
                              group.names=character(),
                              required.sub.ontology.name.for.mechanism=character(),
                              into.compartments.name.for.mechanism=character(),
                              into.compartments.suffix = NULL,
                              alias.suffix.for.mechanism=character(),
                              additional.member.names=character(),
                              register.function.name = paste0('register.', type),
                              trackable.types,
                              ontology.name.for.trackable=character(),
                              applies.to.name.for.trackable=character(),
                              all.applies.to.names=unique(applies.to.name.for.mechanism),
                              into.compartments.name.for.trackable=character())
        {
            
            #-- Validate the arguments --#
            error.prefix = "Error creating CORE.COMPONENT.SCHEMA: "
            
            # Validate Type
            if (!is.character(type) || length(type) != 1 || is.na(type))
                stop(paste0(error.prefix, "'type' must be a single, non-NA character value"))
            
            error.prefix = paste0("Error creating CORE.COMPONENT.SCHEMA '", type, "': ")
            
            # Validate required
            if (!is.logical(required) || length(required)!=1 || is.na(required))
                stop(paste0(error.prefix, "'required' must be a single, non-NA logical value"))
            
            # Validate mechanism types
            if (!is.character(mechanism.types) || length(mechanism.types) == 0 || any(is.na(mechanism.types)))
                stop(paste0(error.prefix, "'mechanism.types' must be a non-empty character vector with no NA values"))
            mechanism.types = unique(mechanism.types)
            
            # Validate Group Names
            if (!is.character(group.names) || any(is.na(group.names)))
                stop(paste0(error.prefix, "'group.names' must be a character vector with no NA values"))
            
            # Validate ontology.name.for.mechanism
            if (!is.character(ontology.name.for.mechanism))
                stop(paste0(error.prefix, "'ontology.name.for.mechanism' must be a character vector"))
            if (length(ontology.name.for.mechanism)>0 && is.null(names(ontology.name.for.mechanism)))
                stop(paste0(error.prefix, "'ontology.name.for.mechanism' must be a NAMED character vector"))
            if (any(table(names(ontology.name.for.mechanism))>1))
                stop(paste0(error.prefix, "The names of 'ontology.name.for.mechanism' must be unique"))
            if (any(is.na(ontology.name.for.mechanism)))
                stop(paste0(error.prefix, "'ontology.name.for.mechanism' cannot contain NA values"))
            
            
            valid.ontology.names = c('infected',
                                     'uninfected',
                                     'contact',
                                     'infected.plus.uninfected')
            invalid.ontology.names = setdiff(ontology.name.for.mechanism, 
                                             union(valid.ontology.names, group.names))
            if (length(invalid.ontology.names)>0)
                stop(paste0(error.prefix, "Invalid ",
                            ifelse(length(invalid.ontology.names)==1, "value", "values"),
                            " for ontology.name.for.mechanism: ",
                            collapse.with.and("'", invalid.ontology.names, "'"),
                            ". (Must be one of ",
                            collapse.with.and("'", valid.ontology.names, "'"), ")"))
            
            invalid.mechanism.names = setdiff(names(ontology.name.for.mechanism), mechanism.types)
            if (length(invalid.mechanism.names)>0)
                stop(paste0(error.prefix,
                            "Invalid mechanism ",
                            ifelse(length(invalid.mechanism.names)==1, "name", "names"),
                            " in the names of 'ontology.name.for.mechanism': ",
                            collapse.with.and("'", invalid.mechanism.names, "'")))
            
#            missing.for.mechanism = setdiff(mechanism.types, names(ontology.name.for.mechanism))
#            if (length(missing.for.mechanism)>0)
#                stop(paste0(error.prefix,
#                            "Missing value in 'ontology.name.for.mechanism' for ",
#                            ifelse(length(missing.for.mechanism)==1, "mechanism", "mechanisms"),
#                            " ", collapse.with.and("'", missing.for.mechanism, "'")))
            
            
            # Validate required.sub.ontology.name.for.mechanism
            if (!is.character(required.sub.ontology.name.for.mechanism))
                stop(paste0(error.prefix, "'required.sub.ontology.name.for.mechanism' must be a character vector"))
            if (length(required.sub.ontology.name.for.mechanism)>0 && is.null(names(required.sub.ontology.name.for.mechanism)))
                stop(paste0(error.prefix, "'required.sub.ontology.name.for.mechanism' must be a NAMED character vector"))
            if (any(table(names(required.sub.ontology.name.for.mechanism))>1))
                stop(paste0(error.prefix, "The names of 'required.sub.ontology.name.for.mechanism' must be unique"))
            if (any(is.na(required.sub.ontology.name.for.mechanism)))
                stop(paste0(error.prefix, "'required.sub.ontology.name.for.mechanism' cannot contain NA values"))
            
            
            valid.required.sub.ontology.names = c('infected.compartments', 'uninfected.compartments',group.names)
            invalid.ontology.names = setdiff(required.sub.ontology.name.for.mechanism, 
                                             valid.required.sub.ontology.names)
            if (length(invalid.ontology.names)>0)
                stop(paste0(error.prefix, "Invalid ",
                            ifelse(length(invalid.ontology.names)==1, "value", "values"),
                            " for required.sub.ontology.name.for.mechanism: ",
                            collapse.with.and("'", invalid.ontology.names, "'"),
                            ". (Must be one of ",
                            collapse.with.and("'", valid.required.sub.ontology.names, "'"), ")"))
            
            invalid.mechanism.names = setdiff(names(required.sub.ontology.name.for.mechanism), mechanism.types)
            if (length(invalid.mechanism.names)>0)
                stop(paste0(error.prefix,
                            "Invalid mechanism ",
                            ifelse(length(invalid.mechanism.names)==1, "name", "names"),
                            " in the names of 'required.sub.ontology.name.for.mechanism': ",
                            collapse.with.and("'", invalid.mechanism.names, "'")))
            
            # Validate all.applies.to.names
            if (!is.character(all.applies.to.names) || any(is.na(all.applies.to.names)))
                stop(paste0(error.prefix, "'all.applies.to.names' must be a character vector with no NA values"))
            
            # Validate applies.to.name.for.mechanism
            if (!is.character(applies.to.name.for.mechanism))
                stop(paste0(error.prefix, "'applies.to.name.for.mechanism' must be a character vector"))
            if (length(applies.to.name.for.mechanism)>0 && is.null(names(applies.to.name.for.mechanism)))
                stop(paste0(error.prefix, "'applies.to.name.for.mechanism' must be a NAMED character vector"))
            if (any(table(names(applies.to.name.for.mechanism))>1))
                stop(paste0(error.prefix, "The names of 'applies.to.name.for.mechanism' must be unique"))
            if (any(is.na(applies.to.name.for.mechanism)))
                stop(paste0(error.prefix, "'applies.to.name.for.mechanism' cannot contain NA values"))
            
#            missing.for.mechanism = setdiff(mechanism.types, names(applies.to.name.for.mechanism))
#            if (length(missing.for.mechanism)>0)
#                stop(paste0(error.prefix,
#                            "Missing value in 'applies.to.name.for.mechanism' for ",
#                            ifelse(length(missing.for.mechanism)==1, "mechanism", "mechanisms"),
#                            " ", collapse.with.and("'", missing.for.mechanism, "'")))
            
            invalid.mechanism.names = setdiff(names(applies.to.name.for.mechanism), mechanism.types)
            if (length(invalid.mechanism.names)>0)
                stop(paste0(error.prefix,
                            "Invalid mechanism ",
                            ifelse(length(invalid.mechanism.names)==1, "name", "names"),
                            " in the names of 'applies.to.name.for.mechanism': ",
                            collapse.with.and("'", invalid.mechanism.names, "'")))
            
            
            invalid.applies.to.names = setdiff(applies.to.name.for.mechanism, all.applies.to.names)
            if (length(invalid.applies.to.names)>0)
                stop(paste0(error.prefix,
                            "Invalid applies.to.",
                            ifelse(length(invalid.mechanism.names)==1, "name", "names"),
                            " in the names of 'applies.to.name.for.mechanism': ",
                            collapse.with.and("'", invalid.mechanism.names, "'. All elements of 'applies.to.name.for.mechanism' must appear in 'all.applies.to.names'")))
            
            # Validate into.compartments.name.for.mechanism
            if (!is.character(into.compartments.name.for.mechanism))
                stop(paste0(error.prefix, "'into.compartments.name.for.mechanism' must be a character vector"))
            if (length(into.compartments.name.for.mechanism)>0 && is.null(names(into.compartments.name.for.mechanism)))
                stop(paste0(error.prefix, "'into.compartments.name.for.mechanism' must be a NAMED character vector"))
            if (any(table(names(into.compartments.name.for.mechanism))>1))
                stop(paste0(error.prefix, "The names of 'into.compartments.name.for.mechanism' must be unique"))
            if (any(is.na(into.compartments.name.for.mechanism)))
                stop(paste0(error.prefix, "'into.compartments.name.for.mechanism' cannot contain NA values"))
            
            invalid.mechanism.names = setdiff(names(into.compartments.name.for.mechanism), mechanism.types)
            if (length(invalid.mechanism.names)>0)
                stop(paste0(error.prefix,
                            "Invalid mechanism ",
                            ifelse(length(invalid.mechanism.names)==1, "name", "names"),
                            " in the names of 'into.compartments.name.for.mechanism': ",
                            collapse.with.and("'", invalid.mechanism.names, "'")))
            
            # Validate into.compartments.suffix
            if (!is.null(into.compartments.suffix))
            {
                if (!is.character(into.compartments.suffix) || length(into.compartments.suffix)!=1 || is.na(into.compartments.suffix))
                    stop(paste0(error.prefix, "'into.compartments.suffix' must be a single, non-NA character value"))
            }
            
            
            # Validate alias.suffix.for.mechanism
            if (!is.character(alias.suffix.for.mechanism))
                stop(paste0(error.prefix, "'alias.suffix.for.mechanism' must be a character vector"))
            if (length(alias.suffix.for.mechanism)>0 && is.null(names(alias.suffix.for.mechanism)))
                stop(paste0(error.prefix, "'alias.suffix.for.mechanism' must be a NAMED character vector"))
            if (any(table(names(alias.suffix.for.mechanism))>1))
                stop(paste0(error.prefix, "The names of 'alias.suffix.for.mechanism' must be unique"))
            
            invalid.mechanism.names = setdiff(names(alias.suffix.for.mechanism), mechanism.types)
            if (length(invalid.mechanism.names)>0)
                stop(paste0(error.prefix,
                            "Invalid mechanism ",
                            ifelse(length(invalid.mechanism.names)==1, "name", "names"),
                            " in the names of 'alias.suffix.for.mechanism': ",
                            collapse.with.and("'", invalid.mechanism.names, "'")))
            
            
            # Validate additional.member.names
            if (!is.character(additional.member.names) || any(is.na(additional.member.names)))
                stop(paste0(error.prefix, "'additional.member.names' must be a character vector with no NA values"))
            
            # Validate register.function.name
            if (!is.character(register.function.name) || length(register.function.name) != 1 || is.na(register.function.name))
                stop(paste0(error.prefix, "'register.function.name' must be a single, non-NA character value"))
            
            
            #-- Validate Trackable Stuff --#
            
            # Validate trackable types
            if (!is.character(trackable.types) || any(is.na(trackable.types)))
                stop(paste0(error.prefix, "'trackable.types' must be a character vector with no NA values"))
            trackable.types = unique(trackable.types)
            
            # Validate ontology.name.for.trackable
            if (!is.character(ontology.name.for.trackable))
                stop(paste0(error.prefix, "'ontology.name.for.trackable' must be a character vector"))
            if (length(ontology.name.for.trackable)>0 && is.null(names(ontology.name.for.trackable)))
                stop(paste0(error.prefix, "'ontology.name.for.trackable' must be a NAMED character vector"))
            if (any(table(names(ontology.name.for.trackable))>1))
                stop(paste0(error.prefix, "The names of 'ontology.name.for.trackable' must be unique"))
            if (any(is.na(ontology.name.for.trackable)))
                stop(paste0(error.prefix, "'ontology.name.for.trackable' cannot contain NA values"))
            
            invalid.ontology.names = setdiff(ontology.name.for.trackable, 
                                             union(valid.ontology.names, group.names))
            if (length(invalid.ontology.names)>0)
                stop(paste0(error.prefix, "Invalid ",
                            ifelse(length(invalid.ontology.names)==1, "value", "values"),
                            " for ontology.name.for.trackable: ",
                            collapse.with.and("'", invalid.ontology.names, "'"),
                            ". (Must be one of ",
                            collapse.with.and("'", valid.ontology.names, "'"), ")"))
            
            invalid.trackable.types = setdiff(names(ontology.name.for.trackable), trackable.types)
            if (length(invalid.mechanism.names)>0)
                stop(paste0(error.prefix,
                            "Invalid trackable ",
                            ifelse(length(invalid.trackable.types)==1, "type", "types"),
                            " in the names of 'ontology.name.for.trackable': ",
                            collapse.with.and("'", invalid.trackable.types, "'")))
            
            # Validate applies.to.name.for.trackable
            if (!is.character(applies.to.name.for.trackable))
                stop(paste0(error.prefix, "'applies.to.name.for.trackable' must be a character vector"))
            if (length(applies.to.name.for.trackable)>0 && is.null(names(applies.to.name.for.trackable)))
                stop(paste0(error.prefix, "'applies.to.name.for.trackable' must be a NAMED character vector"))
            if (any(table(names(applies.to.name.for.trackable))>1))
                stop(paste0(error.prefix, "The names of 'applies.to.name.for.trackable' must be unique"))
            if (any(is.na(applies.to.name.for.trackable)))
                stop(paste0(error.prefix, "'applies.to.name.for.trackable' cannot contain NA values"))
            
            invalid.trackable.types = setdiff(names(applies.to.name.for.trackable), trackable.types)
            if (length(invalid.trackable.types)>0)
                stop(paste0(error.prefix,
                            "Invalid trackable ",
                            ifelse(length(invalid.trackable.types)==1, "type", "types"),
                            " in the names of 'applies.to.name.for.trackable': ",
                            collapse.with.and("'", invalid.trackable.types, "'")))
            
            # Validate into.compartments.name.for.trackable
            if (!is.character(into.compartments.name.for.trackable))
                stop(paste0(error.prefix, "'into.compartments.name.for.trackable' must be a character vector"))
            if (length(into.compartments.name.for.trackable)>0 && is.null(names(into.compartments.name.for.trackable)))
                stop(paste0(error.prefix, "'into.compartments.name.for.trackable' must be a NAMED character vector"))
            if (any(table(names(into.compartments.name.for.trackable))>1))
                stop(paste0(error.prefix, "The names of 'into.compartments.name.for.trackable' must be unique"))
            if (any(is.na(into.compartments.name.for.trackable)))
                stop(paste0(error.prefix, "'into.compartments.name.for.trackable' cannot contain NA values"))
            
            invalid.trackable.types = setdiff(names(into.compartments.name.for.trackable), trackable.types)
            if (length(invalid.trackable.types)>0)
                stop(paste0(error.prefix,
                            "Invalid trackable ",
                            ifelse(length(invalid.trackable.types)==1, "type", "types"),
                            " in the names of 'into.compartments.name.for.trackable': ",
                            collapse.with.and("'", invalid.trackable.types, "'")))
            
            
            #-- Create the object --#
            
            private$i.type = type
            
            # Pull Mechanism Types
            private$i.mechanism.types = mechanism.types
            
            # Pull applies.to.names
            private$i.applies.to.names = unique(all.applies.to.names)
            
            # Pull into.compartments.names
            private$i.into.compartments.names = unique(into.compartments.name.for.mechanism)
            private$i.into.compartments.suffix = into.compartments.suffix
            
            # Store other variables
            private$i.required = required
            private$i.group.names = unique(group.names)
            private$i.ontology.name.for.mechanism = ontology.name.for.mechanism
            private$i.required.sub.ontology.name.for.mechanism = required.sub.ontology.name.for.mechanism
            private$i.applies.to.name.for.mechanism = applies.to.name.for.mechanism
            private$i.into.compartments.name.for.mechanism = into.compartments.name.for.mechanism
            private$i.alias.suffix.for.mechanism = alias.suffix.for.mechanism
            private$i.additional.member.names = additional.member.names
            private$i.register.function.name = register.function.name
            
            private$i.trackable.types = trackable.types
            private$i.ontology.name.for.trackable = ontology.name.for.trackable
            private$i.applies.to.name.for.trackable = applies.to.name.for.trackable
            private$i.into.compartments.name.for.trackable = into.compartments.name.for.trackable
        },
        
        ##-- Functions to Create and Compile Components --##
        
        create.component = function(args, version, error.prefix)
        {
            # Make sure other members are present in arguments
            missing.names = setdiff(private$i.additional.member.names, names(args))
            if (length(missing.names)>0)
                stop(paste0(error.prefix,
                            "Missing ",
                            ifelse(length(missing.names)==1, "value", "values"),
                            " for ",
                            collapse.with.and("'", missing.names, "'")))
            
            # Validate tag
            if (is.null(args$tag))
                stop(paste0(error.prefix, "Missing value for 'tag'"))
            else if (!is.character(args$tag) || length(args$tag)!=1 || is.na(args$tag))
                stop(paste0(error.prefix, "'tag' must be a single, non-NA character value"))
            
            # Validate groups
            for (group.name in private$i.group.names)
            {
                if (all(names(args) != group.name))
                    stop(paste0(error.prefix, "Missing value for '", group.name, "'"))
                
                group.val = args[[group.name]]
                if (!is.character(group.val))
                    stop(paste0(error.prefix, "'", group.name, "' must be a single CHARACTER value"))
                if (length(group.val) != 1)
                    stop(paste0(error.prefix, "'", group.name, "' must be a SINGLE character value"))
                if (is.na(group.val))
                    stop(paste0(error.prefix, "'", group.name, "' cannot be NA"))
                if (all(group.val != ALLOWED.GROUPS))
                    stop(paste0(error.prefix, "Invalid ", group.name, " value '", group.val, "' - must be either ",
                                collapse.with.or("'", ALLOWED.GROUPS, "'")))
            }
            
            # Validate applies.to
            for (applies.to.name in private$i.applies.to.names)
            {
                if (all(names(args) != applies.to.name))
                    stop(paste0(error.prefix, "Missing value for '", applies.to.name, "'"))
                
                applies.to.val = args[[applies.to.name]]
                check.dimension.values.valid(applies.to.val, 
                                             variable.name.for.error = applies.to.name, 
                                             allow.empty = T, 
                                             error.prefix = error.prefix)
                
            }
            
            # Validate into.compartments
            for (into.compartments.name in private$i.into.compartments.names)
            {
                if (all(names(args) != into.compartments.name))
                    stop(paste0(error.prefix, "Missing value for '", into.compartments.name, "'"))
                
                into.compartments.val = args[[into.compartments.name]]
                
                check.dimension.values.valid(into.compartments.val,
                                             variable.name.for.error = into.compartments.name, 
                                             allow.empty = T, 
                                             error.prefix = error.prefix)
                
                too.long = sapply(into.compartments.val, length) > 1
                if (any(too.long))
                    stop(paste0(error.prefix, "'", into.compartments.name, "' can only contain length-one vectors (ie, each element of the list is a single value)"))
                
                if (!is.null(private$i.into.compartments.suffix))
                {
                    suffix = private$i.into.compartments.suffix
                    if (!substr(suffix, 1, 1)=='.')
                        suffix = paste0(".", suffix)
                    
                    missing.suffix = substr(names(into.compartments.val), 
                                            nchar(names(into.compartments.val)) - nchar(suffix) + 1,
                                            nchar(names(into.compartments.val))) != suffix
                    
                    names(into.compartments.val)[missing.suffix] = paste0(names(into.compartments.val)[missing.suffix], suffix)
                    
                    args[[into.compartments.name]] = into.compartments.val
                }
            }
            
            # Make sure into.compartments don't overlap dimensions with applies.to
            for (mechanism.type in names(private$i.into.compartments.name.for.mechanism))
            {
                into.compartments.name = private$i.into.compartments.name.for.mechanism[mechanism.type]
                into.compartments.val = args[[into.compartments.name]]
                
                applies.to.name = private$i.applies.to.name.for.mechanism[mechanism.type]
                applies.to.val = args[[applies.to.name]]
                
                overlapping.dimensions = intersect(names(into.compartments.val), names(applies.to.val))
                if (length(overlapping.dimensions)>0)
                    stop(paste0(error.prefix, "'", applies.to.name, "' and '", 
                                into.compartments.name, "' cannot share dimensions but ",
                                ifelse(length(overlapping.dimensions)==1, "dimension ", "dimensions "),
                                collapse.with.and("'", overlapping.dimensions, "'"),
                                ifelse(length(overlapping.dimensions)==1, " is", " are"),
                                " present in both."))
            }
            
            # Validate version
            if (!is.character(version) || length(version)!=1 || is.na(version))
                stop(paste0(error.prefix, "'version' must be a single, non-NA character value"))
            
            # Set class and return
            rv = c(list(type = private$i.type,
                        tag = args$tag,
                        schema = self,
                        version = version,
                        mechanisms = list()),
                   args[c(private$i.additional.member.names,
                          private$i.group.names,
                          private$i.applies.to.names,
                          private$i.into.compartments.names)])
            
            rv$name = private$get.component.name(rv)
            
            class(rv) = 'core.component'
            rv
        },
        
        # unpack - whether to split the component into multiple components if warranted
        compile.component = function(comp,
                                     specification,
                                     ontologies = specification$ontologies,
                                     aliases = specification$resolved.aliases,
                                     unresolved.alias.names = specification$unresolved.alias.names,
                                     error.prefix)
        {
            #-- Validate and Resolve applies.to --#
            
            # There is a potential flaw in this loop below:
            #   If an applies.to name or an into.compartments name goes with more than one mechanism
            #   and does not have the same properties in all of them
            for (mechanism.type in private$i.mechanism.types)
            {
                if (mechanism.type != 'initial.population')
                {
                    applies.to = private$get.applies.to.for.mechanism(mechanism.type, comp=comp, specification=specification)
                    if (!is.null(applies.to))
                    {
                        ont.name = private$get.ontology.name.for.mechanism(mechanism.type, comp=comp, specification=specification)
                        ont = ontologies[[ont.name]]
                        
                        #-- Validate and Resolve into.compartments --#
                        if (!is.na(private$i.into.compartments.name.for.mechanism[mechanism.type]))
                        {
                            into.compartments = comp[[ private$i.into.compartments.name.for.mechanism[mechanism.type] ]]
                            if (length(into.compartments)>0)
                            {
                                into.compartments = do.resolve.dimension.values(dimension.values = into.compartments,
                                                                                aliases = aliases,
                                                                                ontology = ont,
                                                                                unresolved.alias.names = unresolved.alias.names,
                                                                                variable.name.for.error = paste0(private$i.into.compartments.name.for.mechanism[mechanism.type], " for '", mechanism.type, "' of ", comp$name),
                                                                                ontology.name.for.error = paste0("the ontology ('", ont.name, "')"),
                                                                                error.prefix = error.prefix)
                                # Make sure they are length 1 after resolving
                                too.long.mask = sapply(into.compartments, length)>1
                                if (any(too.long.mask))
                                    stop(paste0(error.prefix, "'", private$i.into.compartments.name.for.mechanism[mechanism.type], "' for '", mechanism.type,
                                                "' of ", comp$name, " can only contain single values, but after resolving aliases, some elements of the list contain more than one value"))
                                
                                # Store
                                comp[[ private$i.into.compartments.name.for.mechanism[mechanism.type] ]] = into.compartments
                             
                                ont = ont[setdiff(names(ont), names(into.compartments))]
                                ont.name = paste0(ont.name, " after excluding ", paste0(names(into.compartments), collapse='/'))
                            }
                        }
                        
                        if (is.na(private$i.applies.to.name.for.mechanism[mechanism.type]))
                            applies.to.name = paste0("the applies.to for '", mechanism.type, "'")
                        else
                            applies.to.name = paste0(private$i.applies.to.name.for.mechanism[mechanism.type], 
                                                     " (for '", mechanism.type, "' mechanism)")
                        
                        resolved.applies.to = do.resolve.dimension.values(dimension.values = applies.to,
                                                                          aliases = aliases,
                                                                          ontology = ont,
                                                                          unresolved.alias.names = unresolved.alias.names,
                                                                          variable.name.for.error = paste0(applies.to.name, " of ", comp$name),
                                                                          ontology.name.for.error = paste0("the ontology ('", ont.name, "')"),
                                                                          error.prefix = error.prefix)
                        
                        if (!is.na(private$i.applies.to.name.for.mechanism[mechanism.type]))
                            comp[[ private$i.applies.to.name.for.mechanism[mechanism.type] ]] = resolved.applies.to
                    }
                }
            }
            
            for (applies.to.name in setdiff(private$i.applies.to.names, private$i.applies.to.name.for.mechanism))
            {
                resolved.applies.to = do.resolve.dimension.values(dimension.values = comp[[applies.to.name]],
                                                                  aliases = aliases,
                                                                  ontology = specification$ontologies$all,
                                                                  unresolved.alias.names = unresolved.alias.names,
                                                                  variable.name.for.error = paste0(applies.to.name, " of ", comp$name),
                                                                  ontology.name.for.error = paste0("the ontology for all dimensions"),
                                                                  error.prefix = error.prefix)
                comp[[applies.to.name]] = resolved.applies.to
            }
            
            rv = private$do.compile.component(comp,
                                              specification,
                                              ontologies = specification$ontologies,
                                              aliases = specification$resolved.aliases,
                                              unresolved.alias.names = specification$unresolved.alias.names,
                                              error.prefix)
            
            lapply(rv, function(comp){
                comp$schema$derive.component.mechanism.dim.names(comp = comp,
                                                                 specification = specification,
                                                                 ontologies = ontologies,
                                                                 aliases = aliases,
                                                                 unresolved.alias.names = unresolved.alias.names,
                                                                 error.prefix = error.prefix)
            })
                   
        },
        
        is.required = function(specification)
        {
            private$i.required
        },
        
        is.allowed = function(specification)
        {
            T
        },
        
        #-- Functions to Create a Mechanism or Top-Level Quantity --#
        
        create.mechanism = function(type, 
                                    quantity.name,
                                    version,
                                    tags,
                                    args,
                                    error.prefix)
        {
            if (!is.character(type) || length(type) != 1 || is.na(type))
                stop(paste0(error.prefix, "'type' must be a single, non-NA character value"))
            
            if (all(private$i.mechanism.types != type))
                stop(error.prefix, "'", type, "' is not a valid mechanism for a '",
                     private$i.type, "' core-component (must be ",
                     collapse.with.or("'", private$i.mechanism.types, "'"), ")")
            
            if (!is.character(quantity.name) || length(quantity.name) != 1 || is.na(quantity.name))
                stop(paste0(error.prefix, "'quantity.name' must be a single, non-NA character value"))
            
            for (group.name in private$i.group.names)
            {
                group.val = args[[group.name]]
                
                if (is.null(group.val))
                    stop(paste0(error.prefix,
                                "Must specify '", group.name, "' in creating a '", type, "' mechanism"))
                
                if (!is.character(group.val) || length(group.val)!=1 || is.na(group.val))
                    stop(paste0(error.prefix, "'", group.name, 
                                "' must be a single, non-NA character value when creating a '",
                                type, "' mechanism"))
            }
            
            
            rv = c(list(type = type,
                        quantity.name = quantity.name,
                        version = version,
                        tags = tags,
                        schema = self),
                   args[private$i.group.names])
            
            class(rv) = 'model.mechanism'
            rv
        },
        
        create.top.level.references = function(comp, 
                                               specification,
                                               error.prefix,
                                               use.dummy.quantity.name=F)
        {
            rv = lapply(private$i.mechanism.types, function(mechanism.type){
                       
                applies.to = private$get.applies.to.for.mechanism(mechanism.type, comp=comp, specification=specification)
                into.compartments = comp[[ private$i.into.compartments.name.for.mechanism[mechanism.type] ]]
 
                if (length(intersect(names(applies.to), names(into.compartments)))>0)
                    stop(paste0("Unable to merge applies.to and into.compartments for mechanism '",
                                mechanism.type, "': applies.to and into.compartments cannot have overlapping dimensions, but both contain ",
                                collapse.with.and("'", intersect(names(applies.to), names(into.compartments)), "'")))

                ontology.name = private$get.ontology.name.for.mechanism(mechanism.type, comp=comp, specification=specification)
                
                required.sub.ontology.name = private$get.required.sub.ontology.name.for.mechanism(mechanism.type, comp=comp, specification=specification)
                
                alias.suffix = private$i.alias.suffix.for.mechanism[mechanism.type]
                if (is.na(alias.suffix))
                    alias.suffix = NULL
                
                exclude.ontology.dimensions = private$get.exclude.ontology.dimensions.for.mechanism(mechanism.type, comp=comp, specification=specification)
                
                if (use.dummy.quantity.name)
                    value.quantity.name = 'dummy'
                else
                    value.quantity.name = comp$mechanisms[[mechanism.type]]$quantity.name
                
                x = TOP.LEVEL.REFERENCE$new(specification = specification,
                                            version = comp$version,
                                            source = comp$name,
                                            ontology.name = ontology.name,
                                            required.sub.ontology.name = required.sub.ontology.name,
                                            value.quantity.name = value.quantity.name,
                                            applies.to = applies.to,
                                            exclude.ontology.dimensions = exclude.ontology.dimensions,
                                            alias.suffix = alias.suffix,
                                            for.core.component.type = private$i.type,
                                            error.prefix = error.prefix)
                x$get.max.dim.names(specification, 'test: ')
                
                x
            })
            
            names(rv) = private$i.mechanism.types
            rv
        },

        set.mechanism.for.component = function(comp,
                                               mechanism,
                                               quantity)
        {
            comp[[mechanism$type]] = quantity$name
            comp$mechanisms[[mechanism$type]] = mechanism
            
            comp
        },

        ##-- Functions to Check if Components are "the same" or Use a Particular Mechanism --##
        component.uses.mechanism = function(comp, mechanism)
        {
            # mechanism is for component
            # tags overlap
            # groups overlap
            
            any(private$i.mechanism.types == mechanism$type) &&
                (is.null(mechanism$tag) || any(comp$tag == mechanism$tag)) &&
                all(sapply(private$i.group.names, function(group.name){
                    is.null(mechanism[[group.name]]) ||
                        comp[[group.name]] == mechanism[[group.name]]
                }))
            
        },
        
        components.clash = function(comp1, comp2)
        {
            # type equal
            # tags equal
            # groups equal
            
            comp1$type == comp2$type &&
                comp1$tag == comp2$tag &&
                all(sapply(private$i.group.names, function(group.name){
                    comp1[[group.name]] == comp2[[group.name]]
                }))
        },

        dynamic.tracker.involves.component = function(tracker, comp)
        {
            any(tracker$trackable.type == private$i.trackable.types) &&
                ( is.null(tracker$exclude.tags) || all(tracker$exclude.tags != comp$tag)) &&
                ( is.null(tracker$include.tags) || any(tracker$include.tags == comp$tag) ) &&
                ( is.null(tracker$groups) || any(tracker$groups == comp[[ private$i.ontology.name.for.trackable[tracker$trackable.type] ]]) )
            # ^The last term of the last line relies on the fact that if a group applies to this tracker
            #  then the ontology name for the tracker is the group name
        },

        get.ontology.for.trackable = function(trackable.type,
                                              comp,
                                              ontologies)
        {
            # Pull Down the Ontology
            ontology.name = private$get.ontology.name.for.trackable(trackable.type, comp)
            rv = ontologies[[ontology.name]]
            
            # Merge with applies.to
            applies.to = private$get.applies.to.for.trackable(trackable.type, comp = comp)
            if (!is.null(applies.to))
            {
                applicable.applies.to.dimensions = intersect(names(rv),
                                                             names(applies.to))
                rv[applicable.applies.to.dimensions] = applies.to[applicable.applies.to.dimensions]
            }
            
            # Fold in into-compartments
            into.compartments.name = private$i.into.compartments.name.for.trackable[trackable.type]
            if (!is.na(into.compartments.name))
            {
                applicable.into.dimensions = intersect(names(rv), 
                                                       names(comp[[into.compartments.name]]))
                rv[applicable.into.dimensions] = comp[[into.compartments.name]][applicable.into.dimensions]
            }
            
            # Return
            rv
        },

        derive.component.mechanism.dim.names = function(comp,
                                                        specification,
                                                        ontologies,
                                                        aliases,
                                                        unresolved.alias.names,
                                                        error.prefix)
        {
            references = self$create.top.level.references(comp = comp,
                                                          specification = specification,
                                                          error.prefix = error.prefix,
                                                          use.dummy.quantity.name=T)
            
            comp$mechanism.dim.names = lapply(private$i.mechanism.types, function(mechanism.type){
                
                ref = references[[mechanism.type]]
                dim.names = ref$get.max.dim.names(specification = specification,
                                                  error.prefix = paste0(error.prefix, " - cannot get dimnames for ", comp$name, ": "))
                
                ont.name = private$get.ontology.name.for.mechanism(mechanism.type, comp=comp, specification=specification)
                ont = ontologies[[ont.name]]
                
                do.resolve.dimension.values(dimension.values = dim.names,
                                            aliases = aliases,
                                            ontology = ont,
                                            unresolved.alias.names = unresolved.alias.names,
                                            variable.name.for.error = paste0("mechanism '", mechanism.type, "' of ", comp$name),
                                            ontology.name.for.error = paste0("the ontology ('", ont.name, "')"),
                                            error.prefix = error.prefix)
            })
            names(comp$mechanism.dim.names) = private$i.mechanism.types
            
            
            comp
        }
    ),
    
    active = list(
        
        type = function(value)
        {
            if (missing(value))
                private$i.type
            else
                stop(paste0("Cannot modify a ", self$descriptor, "'s 'type' - it is read-only"))
        },
        
        mechanism.types = function(value)
        {
            if (missing(value))
                private$i.mechanism.types
            else
                stop(paste0("Cannot modify a ", self$descriptor, "'s 'mechanism.types' - they are read-only"))
        },
        
        register.function.name = function(value)
        {
            if (missing(value))
                private$i.register.function.name
            else
                stop(paste0("Cannot modify a ", self$descriptor, "'s 'register.function.name' - it is read-only"))
        },
        
        trackable.types = function(value)
        {
            if (missing(value))
                private$i.trackable.types
            else
                stop(paste0("Cannot modify a ", self$descriptor, "'s 'trackable.types' - they are read-only"))
        }
    ),
    
    private = list(
        i.type = NULL,
        
        i.mechanism.types = NULL,
        
        i.required = NULL,
        
        i.group.names = NULL,
        i.applies.to.names = NULL,
        i.into.compartments.names = NULL,
        
        i.into.compartments.suffix = NULL,
        
        i.ontology.name.for.mechanism = NULL,
        i.required.sub.ontology.name.for.mechanism = NULL,
        i.applies.to.name.for.mechanism = NULL,
        i.into.compartments.name.for.mechanism = NULL,
        i.alias.suffix.for.mechanism = NULL,
        
        i.additional.member.names = NULL,
        
        i.register.function.name = NULL,
        
        i.trackable.types = NULL,
        i.ontology.name.for.trackable = NULL,
        i.applies.to.name.for.trackable = NULL,
        i.into.compartments.name.for.trackable = NULL,
        
        
        # Private Member Functions
        get.ontology.name.for.mechanism = function(mechanism.type, comp, specification)
        {
            ontology.name = private$i.ontology.name.for.mechanism[[mechanism.type]]
            if (is.na(ontology.name))
                stop(paste0("CORE.COMPONENT.SCHEMA cannot find ontology name for mechanism '", mechanism.type, "' for a '", private$i.type, "' core component"))
            
            if (any(ontology.name == private$i.group.names))
                ontology.name = comp[[ontology.name]]
            
            ontology.name
        },
        
        get.ontology.name.for.trackable = function(trackable.type, comp)
        {
            ontology.name = private$i.ontology.name.for.trackable[[trackable.type]]
            if (is.na(ontology.name))
                stop(paste0("CORE.COMPONENT.SCHEMA cannot find ontology name for trackable '", trackable.type, "' for a '", private$i.type, "' core component"))
            
            if (any(ontology.name == private$i.group.names))
                ontology.name = comp[[ontology.name]]
            
            ontology.name
        },
        
        get.required.sub.ontology.name.for.mechanism = function(mechanism.type, comp, specification)
        {
            required.sub.ontology.name = private$i.required.sub.ontology.name.for.mechanism[mechanism.type]
            if (is.na(required.sub.ontology.name))
                required.sub.ontology.name = NULL
            else if (any(required.sub.ontology.name == private$i.group.names))
                required.sub.ontology.name = comp[[required.sub.ontology.name]]
            
            required.sub.ontology.name
        },
        
        get.applies.to.for.mechanism = function(mechanism.type, comp, specification)
        {
            applies.to.name = private$i.applies.to.name.for.mechanism[mechanism.type]
            if (is.na(applies.to.name))
                NULL
            else
                comp[[applies.to.name]]
        },
        
        get.applies.to.for.trackable = function(trackable.type, comp)
        {
            applies.to.name = private$i.applies.to.name.for.trackable[trackable.type]
            if (is.na(applies.to.name))
                NULL
            else
                comp[[applies.to.name]]
        },
        
        get.exclude.ontology.dimensions.for.mechanism = function(mechanism.type, comp, specification)
        {
            rv = names(comp[[ private$i.into.compartments.name.for.mechanism[mechanism.type] ]])
            rv = rv[!is.na(rv)]
            rv
        },
        
        get.component.name = function(comp)
        {
            rv = private$i.type
            if (length(private$i.group.names)==0)
            {}
            else if (length(private$i.group.names)==1)
            {
                rv = paste0(rv, " for ",
                            paste0("'", comp[[private$i.group.names]], "'", collapse='/'))
            }
            else
            {
                group.descriptors = sapply(private$i.group.names, function(group.name){
                    paste0(group.name, "=", 
                           paste0("'", comp[[group.name]], "'", collapse='/'))
                })
                rv = paste0(rv, " for ", paste0(group.descriptors, collapse=', '))
            }
            
            if (comp$tag != private$i.type)
                rv = paste0(rv, " (with tag='", comp$tag, "')")
            
            rv
        },
        
        do.compile.component = function(comp,
                                        specification,
                                        ontologies = specification$ontologies,
                                        aliases = specification$resolved.aliases,
                                        unresolved.alias.names = specification$unresolved.alias.names,
                                        error.prefix)
        {
            list(comp)
        }
    )
)

AGING.CORE.COMPONENT.SCHEMA = R6::R6Class(
    'aging.core.component.schema',
    inherit = CORE.COMPONENT.SCHEMA,
    portable = F,
    
    public = list(
        
        initialize = function()
        {
            super$initialize(type = 'aging',
                             mechanism.types = "aging.rate",
                             group.names='group',
                             ontology.name.for.mechanism = c(aging.rate='group'),
                             applies.to.name.for.mechanism = c(aging.rate='applies.to'),
                             required = F,
                             trackable.types = character())
        },
        
        is.allowed = function(specification)
        {
            !is.null(specification$age.info) &&  length(specification$age.info$endpoints)>2
        }
    ),
    
    private = list(
        
        get.ontology.name.for.mechanism = function(mechanism.type, comp, specification)
        {
            ontology.name = private$i.ontology.name.for.mechanism[[mechanism.type]]
            if (is.na(ontology.name))
                stop(paste0("CORE.COMPONENT.SCHEMA cannot find ontology name for mechanism '", mechanism.type, "' for a '", private$i.type, "' core component"))
            
            if (any(ontology.name == private$i.group.names))
                ontology.name = paste0('aging.', comp[[ontology.name]])
            
            ontology.name
        },
        
        do.compile.component = function(comp,
                                        specification,
                                        ontologies = specification$ontologies,
                                        aliases = specification$resolved.aliases,
                                        unresolved.alias.names = specification$unresolved.alias.names,
                                        error.prefix)
        {
            if (is.null(comp$mechanism.dim.names))
                list(comp)
            else
            {
                lapply(comp$mechanism.dim.names$aging.rate$age, function(age.from){
                    
                    # get the numeric index into ages, and the next age
                    age.from.index = (1:specification$age.info$n.ages)[specification$age.info$ages==age.from]
                    age.to = specification$age.info$ages[age.from.index+1]
                    
                    # reduce the applies to
                    applies.to = comp$applies.to[setdiff(names(comp$applies.to), 'age')]
                    
                    # set up the arguments
                    args = list(group = comp$group,
                                tag = comp$tag,
                                applies.to = applies.to,
                                dimension = 'age',
                                from.compartments = age.from,
                                to.compartments = age.to)
                    
                    # create it
                    new.comp = CORE.COMPONENT.SCHEMATA$transition$create.component(args=args, 
                                                                                   version=comp$version, 
                                                                                   error.prefix=error.prefix)
                    
                    
                    old.mechanism = comp$mechanisms$aging.rate
                    
                    new.mechanism = new.comp$schema$create.mechanism(type='transition.rate',
                                                                 quantity.name = old.mechanism$quantity.name,
                                                                 version = old.mechanism$version,
                                                                 tags = old.mechanism$tags,
                                                                 args = list(
                                                                     dimension = 'age',
                                                                     group = old.mechanism$group,
                                                                     from.compartments = age.from,
                                                                     to.compartments = age.to),
                                                                 error.prefix = error.prefix)
                    
                    new.comp = new.comp$schema$set.mechanism.for.component(comp = new.comp,
                                                                           mechanism = new.mechanism,
                                                                           quantity = specification$get.quantity(old.mechanism$quantity.name))
                    
                    new.comp
                })
            }
        }
    )
)


NATALITY.CORE.COMPONENT.SCHEMA = R6::R6Class(
    'natality.core.component.schema',
    inherit = CORE.COMPONENT.SCHEMA,
    portable = F,
 
    public = list(
        
        initialize = function()
        {
            super$initialize(type = 'natality',
                             mechanism.types = c('fertility.rate', 'birth.proportions'),
                             group.names = c('parent.group','child.group'),
                             ontology.name.for.mechanism = c(fertility.rate='parent.group'),
                             applies.to.name.for.mechanism = c(fertility.rate='applies.to'),
                             required = T,
                             into.compartments.name.for.mechanism=c(birth.proportions='all.births.into.compartments'),
                             into.compartments.suffix = '.to',
                             alias.suffix.for.mechanism=c(birth.proportions='from'),
                             additional.member.names='parent.child.concordant.dimensions',
                             trackable.types = c('births.from','births.to',
                                                 'incidence.to','incidence.by'),
                             ontology.name.for.trackable = c(births.from='parent.group',
                                                             births.to='child.group',
                                                             incidence.to='infected',
                                                             incidence.by='infected'),
                             applies.to.name.for.trackable = c(births.from='applies.to',
                                                               incidence.by='applies.to'))
        },
        
        create.component = function(args, version, error.prefix)
        {
            if (is.null(args$all.births.into.compartments))
                args$parent.child.concordant.dimensions = character()
            else
            {
                if (!is.character(args$parent.child.concordant.dimensions) || any(is.na(args$parent.child.concordant.dimensions)))
                    stop(paste0(error.prefix, "'parent.child.concordant.dimensions' must be a character vector with no NA values"))
            }
            
            super$create.component(args, version=version, error.prefix=error.prefix)
        },
        
        
        dynamic.tracker.involves.component = function(tracker, comp)
        {
            super$dynamic.tracker.involves.component(tracker=tracker, comp=comp) &&
                ( comp$child.group=='infected' || 
                    (tracker$trackable.type != 'incidence.to' && 
                         tracker$trackable.type != 'incidence.by') )
        }
    ),
    
    private = list(
        
        get.ontology.name.for.mechanism = function(mechanism.type, comp, specification)
        {
            if (mechanism.type=='birth.proportions')
                paste0('birth.proportions.', comp$parent.group, ".to.", comp$child.group)
            else
                super$get.ontology.name.for.mechanism(mechanism.type, comp=comp, specification=specification)
        },
        
        get.required.sub.ontology.name.for.mechanism = function(mechanism.type, comp, specification)
        {
            if (mechanism.type=='birth.proportions')
                paste0(comp$child.group, ".to")
            else
                super$get.required.sub.ontology.name.for.mechanism(mechanism.type, comp=comp, specification=specification)
        },
        
        get.applies.to.for.mechanism = function(mechanism.type, comp, specification)
        {
            if (mechanism.type=='birth.proportions')
            {
                applies.to = comp$applies.to
                if (length(applies.to)>0)
                    names(applies.to) = paste0(names(applies.to), ".from")
                applies.to
            }
            else
                super$get.applies.to.for.mechanism(mechanism.type, comp=comp, specification=specification)
        },
        
        get.applies.to.for.trackable = function(trackable.type, comp, specification)
        {
            if (trackable.type=='births.to' || trackable.type=='incidence.to')
            {
                concordant.dimensions.in.applies.to = intersect(substr(comp$parent.child.concordant.dimensions, 1, nchar(comp$parent.child.concordant.dimensions)-nchar(private$i.into.compartments.suffix)), 
                                                                names(comp$applies.to))
                comp$applies.to[concordant.dimensions.in.applies.to]
            }
            else
                super$get.applies.to.for.mechanism(mechanism.type, comp=comp, specification=specification)
        },
        
        get.exclude.ontology.dimensions.for.mechanism = function(mechanism.type, comp, specification)
        {
            rv = super$get.exclude.ontology.dimensions.for.mechanism(mechanism.type, comp=comp, specification=specification)
            if (mechanism.type=='birth.proportions')
                rv = c(rv, paste0(comp$parent.child.concordant.dimensions, '.to'))
            
            rv
        },
        
        do.compile.component = function(comp,
                                        specification,
                                        ontologies = specification$ontologies,
                                        aliases = specification$resolved.aliases,
                                        unresolved.alias.names = specification$unresolved.alias.names,
                                        error.prefix)
        {
            #-- Make sure that all parent.child.concordant.dimensions are in both the from and to ontologies --#
            from.ontology = ontologies[[comp$parent.group]]
            to.ontology = ontologies[[comp$child.group]]
            
            # Check for missing from from.ontology
            concordant.dimensions.missing.from.from = setdiff(comp$parent.child.concordant.dimensions,
                                                              names(from.ontology))
            
            if (length(concordant.dimensions.missing.from.from)>0)
                stop(paste0(error.prefix,
                            ifelse(length(concordant.dimensions.missing.from.from)>1, "Dimensions ", "Dimension "),
                            collapse.with.and("'", concordant.dimensions.missing.from.from, "'"),
                            ifelse(length(concordant.dimensions.missing.from.from)>1, " are included as ", " is included as one of "),
                            "'parent.child.concordant.dimensions' for ", comp$name,
                            " but ",
                            ifelse(length(concordant.dimensions.missing.from.from)>1, "are", "is"),
                            " not present in the parent ontology ('", comp$parent.group, "')"))
            
            # Check for missing from to.ontology
            concordant.dimensions.missing.from.to = setdiff(comp$parent.child.concordant.dimensions,
                                                            names(to.ontology))
            if (length(concordant.dimensions.missing.from.to)>0)
                stop(paste0(error.prefix,
                            ifelse(length(concordant.dimensions.missing.from.to)>1, "Dimensions ", "Dimension "),
                            collapse.with.and("'", concordant.dimensions.missing.from.to, "'"),
                            ifelse(length(concordant.dimensions.missing.from.to)>1, " are included as ", " is included as one of "),
                            "'parent.child.concordant.dimensions' for ", comp$name,
                            " but ",
                            ifelse(length(concordant.dimensions.missing.from.to)>1, "are", "is"),
                            " not present in the child ontology ('", comp$child.group, "')"))
            
            # Check for not the same in from and to ontologies
            for (d in comp$parent.child.concordant.dimensions)
            {
                if (!setequal(from.ontology[[d]], to.ontology[[d]]))
                    stop(paste0(error.prefix,
                                "Dimension '", d, "' is included as one of 'parent.child.concordant.dimensions' for ", comp$name,
                                " but does not have the same values in the parent - '", comp$parent.group, "' - ontology (",
                                paste0("'", from.ontology[[d]], "'", collapse=', '),
                                ") as in the child - '", comp$child.group, "' - ontology (",
                                paste0("'", to.ontology[[d]], "'", collapse=', '),
                                ")"))
            }
            
            
            #-- Check for clashes between all.births.into.compartments and parent.child.concordant.dimensions --#
            clashing.dimensions = intersect(names(comp$all.births.into.compartments), comp$parent.child.concordant.dimensions)
            if (length(clashing.dimensions)>0)
                stop(paste0(error.prefix, 
                            ifelse(length(clashing.dimensions)>1, "Dimensions ", "Dimension "),
                            collapse.with.and("'", clashing.dimensions, "'"),
                            ifelse(length(clashing.dimensions)>1, " are", " is"),
                            " present in both 'all.births.into.compartments' and 'parent.child.concordant.dimensions' for ",
                            comp$name, ". Dimensions may only be present in one or the other, but not both"
                ))
            
            #-- Make sure all.births.into.compartments are present in the to ontology --#
            missing.all.births.into.dimensions = setdiff(names(comp$all.births.into.compartments), paste0(names(to.ontology), private$i.into.compartments.suffix))
            if (length(missing.all.births.into.dimensions)>0)
                stop(paste0(error.prefix,
                            ifelse(length(missing.all.births.into.dimensions)>1, "Dimensions ", "Dimension "),
                            collapse.with.and("'", missing.all.births.into.dimensions, "'"),
                            ifelse(length(missing.all.births.into.dimensions)>1, " are included as dimensions", " is included as a dimension"),
                            " in 'all.births.into.compartments' for ", comp$name,
                            " but ",
                            ifelse(length(missing.all.births.into.dimensions)>1, "are", "is"),
                            " not present in the child ontology ('", comp$child.group, "')"))
            
            for (d in names(comp$all.births.into.compartments))
            {
                d.to = substr(d, 1, nchar(d)-nchar(private$i.into.compartments.suffix))
                if (all(to.ontology[[d.to]] != comp$all.births.into.compartments[d]))
                    stop(paste0(error.prefix,
                                "'", comp$all.births.into.compartments[d], "' is given as the all.births.into.compartments value for dimension '",
                                d, "', but is not a value in the child ontology ('", comp$child.group, "')"))
            }
            
            list(comp)
        }
    )
)

TRANSMISSION.CORE.COMPONENT.SCHEMA = R6::R6Class(
    'transmission.core.component.schema',
    inherit = CORE.COMPONENT.SCHEMA,
    portable = F,
    
    public = list(
        
        initialize = function()
        {
            super$initialize(type = 'transmission',
                             mechanism.types = c('susceptibility','transmissibility','contact','new.infection.proportions'),
                             required = T,
                             ontology.name.for.mechanism = c(susceptibility='uninfected',
                                                             transmissibility='infected',
                                                             contact='contact',
                                                             new.infection.proportions='infected.plus.uninfected'),
                             all.applies.to.names = c('to.applies.to',
                                                      'transmission.applies.to',
                                                      'new.infections.applies.to',
                                                      'from.applies.to'),
                             applies.to.name.for.mechanism = c(susceptibility='to.applies.to',
                                                               transmissibility='transmission.applies.to',
                                                               new.infection.proportions='new.infections.applies.to'),
                             required.sub.ontology.name.for.mechanism=c(new.infection.proportions='infected.compartments'),
                             into.compartments.name.for.mechanism=c(new.infection.proportions='all.new.infections.into.compartments'),
                             alias.suffix.for.mechanism=c(contact='to'),
                             trackable.types = c('incidence.from','incidence.by','incidence.to'),
                             ontology.name.for.trackable = c(incidence.from='uninfected',
                                                             incidence.by='infected',
                                                             incidence.to='infected'),
                             applies.to.name.for.trackable = c(incidence.from='to.applies.to',
                                                               incidence.by='transmission.applies.to',
                                                               incidence.to='new.infections.applies.to'),
                             into.compartments.name.for.trackable = c(incidence.to='all.new.infections.into.compartments'))
        }
    ),
    
    private = list(
        
        get.applies.to.for.mechanism = function(mechanism.type, comp, specification)
        {
            if (mechanism.type=='contact')
            {
                # Create 'extra' contact.applies.to variable out of from.applies.to and to.applies.to
                from.applies.to = comp$from.applies.to
                if (length(from.applies.to)>0)
                    names(from.applies.to) = paste0(names(from.applies.to), '.from')
                
                to.applies.to = comp$to.applies.to
                if (length(to.applies.to)>0)
                    names(to.applies.to) = paste0(names(to.applies.to), '.to')
                
                contact.applies.to = c(to.applies.to, from.applies.to)
                
                contact.applies.to = contact.applies.to[intersect(names(contact.applies.to),
                                                                  names(specification$ontologies[['contact']]))]
                
                contact.applies.to
            }
            else
                super$get.applies.to.for.mechanism(mechanism.type, comp=comp, specification=specification)
        },
        
        do.compile.component = function(comp,
                                        specification,
                                        ontologies = specification$ontologies,
                                        aliases = specification$resolved.aliases,
                                        unresolved.alias.names = specification$unresolved.alias.names,
                                        error.prefix)
        {
            # Make sure transmission applies to and from applies to line up
            # transmission.applies.to cannot have any compartments not in from.applies.to
            # and there is no point keeping any compartments in from.applies.to that do not transmit
            overlapping.dimensions = intersect(names(comp$transmission.applies.to), names(comp$from.applies.to))
            if (!dim.names.are.subset(sub.dim.names = comp$transmission.applies.to[overlapping.dimensions],
                                      super.dim.names = comp$from.applies.to[overlapping.dimensions]))
                stop(paste0(error.prefix,
                            "If a dimension appears in both 'from.applies.to' and 'transmission.applies.to', every value that appears in that dimension in 'transmission.applies.to' must also appear in the dimension in 'from.applies.to' (otherwise, you are saying that a dimension value can transmit, but cannot be a partner, which doesn't make sense)"))
            
            if (!dim.names.are.subset(sub.dim.names = comp$from.applies.to[overlapping.dimensions],
                                      super.dim.names = comp$transmission.applies.to[overlapping.dimensions]))
                stop(paste0(error.prefix,
                            "If a dimension appears in both 'from.applies.to' and 'transmission.applies.to', every value that appears in that dimension in 'from.applies.to' should also appear in the dimension in 'transmission.applies.to' (otherwise, you are saying that a dimension value can be a partner, but cannot transmit, in which case there is no reason to model that partnership)"))
            

            # force transmission.applies.to to include all of from.applies.to
            comp$transmission.applies.to = intersect.joined.dim.names(comp$transmission.applies.to, comp$from.applies.to)
            
            # Check new infections applies to
            relevant.dimensions.from.to.applies.to = intersect(names(comp$to.applies.to), names(specification$compartments$general))
            valid.dimensions = c(names(specification$compartments$infected),
                                 relevant.dimensions.from.to.applies.to)
            invalid.new.infections.dimensions = setdiff(names(comp$new.infections.applies.to), valid.dimensions)
            if (length(invalid.new.infections.dimensions)>0)
                stop(paste0(error.prefix, "'new.infections.applies.to' can only contain dimensions that are specific to the infected group or present in infection-related dimensions of to.applies.to (",
                            collapse.with.and("'", names(valid.dimensions), "'"),
                            "). ", collapse.with.and("'", invalid.new.infections.dimensions, "'"),
                            ifelse(length(invalid.new.infections.dimensions)==1, " is not an valid dimension", " are not valid dimensions")))
            
            
            dimensions.present.in.to.and.new.infections = intersect(names(comp$new.infections.applies.to),
                                                                    relevant.dimensions.from.to.applies.to)
            for (d in dimensions.present.in.to.and.new.infections)
            {
                if (!setequal(comp$new.infections.applies.to[[d]],
                              comp$to.applies.to[[d]]))
                {
                    stop(paste0(error.prefix,
                                "When 'new.infetions.applies.to' shares a dimension with 'to.applies.to', the dimension values must be the same, but this is not true for the '", d, "' dimension"))
                }
            }
            
            # Force new.infections.applies.to to contain everything relevant from to.applies.to
            comp$new.infections.applies.to[relevant.dimensions.from.to.applies.to] = comp$to.applies.to[relevant.dimensions.from.to.applies.to]
           
            # Package up and be done
            list(comp)
        }
    )
    
)

TRANSITION.CORE.COMPONENT.SCHEMA = R6::R6Class(
    'transition.core.component.schema',
    inherit = CORE.COMPONENT.SCHEMA,
    portable = F,
    
    public = list(
        
        initialize = function()
        {
            super$initialize(type = 'transition',
                             mechanism.types = 'transition.rate',
                             group.names='group',
                             ontology.name.for.mechanism=character(),
                             applies.to.name.for.mechanism=c(transition.rate='applies.to'),
                             required=F,
                             alias.suffix.for.mechanism=c(transition.rate='from'),
                             additional.member.names=c('dimension','from.compartments','to.compartments'),
                             trackable.types = 'transition')
        },
        
        create.component = function(args, version, error.prefix)
        {
            # Check dimension
            if (is.null(args$dimension))
                stop(paste0(error.prefix, "'dimension' must be specified"))
            if (!is.character(args$dimension) || length(args$dimension)!=1 || is.na(args$dimension))
                stop(paste0(error.prefix, "'dimension' must be a single, non-NA character value"))
            
            # Check from.compartments
            if (is.null(args$from.compartments))
                stop(paste0(error.prefix, "'from.compartments' must be specified"))
            if (!is.character(args$from.compartments) || length(args$from.compartments)==0 || any(is.na(args$from.compartments)))
                stop(paste0(error.prefix, "'from.compartments' must be a character vector with length >= 1 and no NA values"))
            
            # Check to.compartments
            if (is.null(args$to.compartments))
                stop(paste0(error.prefix, "'to.compartments' must be specified"))
            if (!is.character(args$to.compartments) || length(args$to.compartments)==0 || any(is.na(args$to.compartments)))
                stop(paste0(error.prefix, "'to.compartments' must be a character vector with length >= 1 and no NA values"))
            
            rv = super$create.component(args, version=version, error.prefix=error.prefix)
            
            # Check applies to
            if (any(names(args$applies.to)==paste0(args$dimension, '.from')))
                stop(paste0(error.prefix, "'applies.to' for a transition in the '",
                            args$dimension, "' dimension cannot contain values for '",
                            paste0(args$dimension, '.from'), "'"))
            
            if (any(names(args$applies.to)==paste0(args$dimension, '.to')))
                stop(paste0(error.prefix, "'applies.to' for a transition in the '",
                            args$dimension, "' dimension cannot contain values for '",
                            paste0(args$dimension, '.to'), "'"))
            
            rv
        },
        
        create.mechanism = function(type, 
                                    quantity.name,
                                    version,
                                    tags,
                                    args,
                                    error.prefix)
        {
            rv = super$create.mechanism(type,
                                        quantity.name = quantity.name,
                                        version = version,
                                        tags = tags,
                                        args = args,
                                        error.prefix)
            
            # Check dimension
            if (is.null(args$dimension))
                stop(paste0(error.prefix, "'dimension.value' must be specified"))
            if (!is.character(args$dimension) || length(args$dimension)!=1 || is.na(args$dimension))
                stop(paste0(error.prefix, "'dimension' must be a single, non-NA character value"))
            
            # Check from.compartments
            if (is.null(args$from.compartments))
                stop(paste0(error.prefix, "'from.compartments' must be specified"))
            if (!is.character(args$from.compartments) || length(args$from.compartments)==0 || any(is.na(args$from.compartments)))
                stop(paste0(error.prefix, "'from.compartments' must be a character vector with length >= 1 and no NA values"))
            
            # Check to.compartments
            if (is.null(args$to.compartments))
                stop(paste0(error.prefix, "'to.compartments' must be specified"))
            if (!is.character(args$to.compartments) || length(args$to.compartments)==0 || any(is.na(args$to.compartments)))
                stop(paste0(error.prefix, "'to.compartments' must be a character vector with length >= 1 and no NA values"))
            
            # Store and return
            rv$dimension = args$dimension
            rv$from.compartments = args$from.compartments
            rv$to.compartments = args$to.compartments
            
            rv
        },
        
        component.uses.mechanism = function(comp, mechanism)
        {
            comp$type=='transition' && mechanism$type=='transition.rate' &&
                super$component.uses.mechanism(comp, mechanism = mechanism) &&
                comp$dimension == mechanism$dimension &&
                setequal(comp$from.compartments, mechanism$from.compartments) &&
                setequal(comp$to.compartments, mechanism$to.compartments)
        },
        
        components.clash = function(comp1, comp2)
        {
            comp1$type=='transition' && comp2$type=='transition' &&
                super$components.clash(comp1, comp2) &&
                comp1$dimension == comp2$dimension &&
                length(intersect(comp1$from.compartments, comp2$from.compartments)>1) &&
                length(intersect(comp1$to.compartments, comp2$to.compartments)>1)
        },        
        
        dynamic.tracker.involves.component = function(tracker, comp)
        {
            super$dynamic.tracker.involves.component(tracker=tracker, comp=comp) &&
                tracker$dimension == comp$dimension &&
                length(intersect(tracker$from.compartments, comp$from.compartments)) > 0 &&
                length(intersect(tracker$to.compartments, comp$to.compartments)) > 0
        }
    ),
    
    private = list(
        
        get.ontology.name.for.mechanism = function(mechanism.type, comp, specification)
        {
            if (mechanism.type=='transition.rate')
                paste0('transition.', comp$dimension, '.', comp$group)
            else
                super$get.ontology.name.for.mechanism(mechanism.type, comp=comp, specification=specification)
        },
        
        get.applies.to.for.mechanism = function(mechanism.type, comp, specification)
        {
            if (mechanism.type=='transition.rate')
            {
                applies.to = comp$applies.to
                applies.to[[paste0(comp$dimension, '.from')]] = comp$from.compartments
                applies.to[[paste0(comp$dimension, '.to')]] = comp$to.compartments
                
                applies.to
            }
            else
                super$get.applies.to.for.mechanism(mechanism.type, comp=comp, specification=specification)
        },
        
        get.component.name = function(comp)
        {
            rv = super$get.component.name(comp)
            rv = substr(rv, nchar(private$i.type)+2, nchar(rv))
            
            paste0("'", comp$dimension, "' transition from ",
                   paste0("'", comp$from.compartments, "'", collapse='/'),
                   " to ",
                   paste0("'", comp$to.compartments, "'", collapse='/'),
                   " ", rv)
        },
        
        get.ontology.name.for.trackable = function(trackable.type, comp)
        {
            if (trackable.type=='transition')
                paste0('transition.', comp$dimension, ".", comp$group)
            else
                super$get.ontology.name.for.trackable(trackable.type = trackable.type, comp = comp)
        },

        get.applies.to.for.trackable = function(trackable.type, comp)
        {
            if (trackable.type=='transition')
            {
                applies.to = list(comp$from.compartments,
                                  comp$to.compartments)
                names(applies.to) = paste0(comp$dimension, c('.from','.to'))
                applies.to
            }
            else
                super$get.applies.to.for.trackable(trackable.type = trackable.type, comp = comp)
        },
        
        do.compile.component = function(comp,
                                        specification,
                                        ontologies = specification$ontologies,
                                        aliases = specification$resolved.aliases,
                                        unresolved.alias.names = specification$unresolved.alias.names,
                                        error.prefix)
        {
            # Make sure that dimension is in the ontology for the group
            ont = ontologies[[comp$group]]
            if (all(comp$dimension != names(ont)))
                stop(paste0(error.prefix, "Dimension '", comp$dimension, "', which was passed as the transition dimension for '",
                            comp$group, "'",
                            ifelse(comp$tag=='transition', '', paste0(" with tag='", comp$tag, "'")),
                            " is not a valid dimension for the '", comp$group, "' ontology"))
            
            # Make sure that from.compartments and to.compartments are the dimensions in the ontology for the group
            dimension.values = ont[[comp$dimension]]
            
            missing.to.compartments = setdiff(comp$to.compartments, union(dimension.values, unresolved.alias.names))
            if (length(missing.to.compartments)>1)
                stop(paste0(error.prefix, "'to.compartments' for transition in dimension '", comp$dimension,
                            "' for '", comp$group, "'",
                            ifelse(tag=='transition', "", paste0(" with tag='", comp$tag, "'")),
                            " includes ",
                            collapse.with.and("'", missing.to.compartments, "'"),
                            " but ",
                            ifelse(length(missing.to.compartments)==1, "it is", "they are"),
                            " not valid values for the '", comp$dimension, "' dimension"))
            
            missing.from.compartments = setdiff(comp$from.compartments, union(dimension.values, unresolved.alias.names))
            if (length(missing.from.compartments)>1)
                stop(paste0(error.prefix, "'from.compartments' for transition in dimension '", comp$dimension,
                            "' for '", comp$group, "'",
                            ifelse(tag=='transition', "", paste0(" with tag='", comp$tag, "'")),
                            " includes ",
                            collapse.with.and("'", missing.to.compartments, "'"),
                            " but ",
                            ifelse(length(missing.to.compartments)==1, "it is", "they are"),
                            " not valid values for the '", comp$dimension, "' dimension"))
            
            # Make sure from.compartments and to.compartments do not overlap
            overlapping.compartments = intersect(comp$from.compartments, comp$to.compartments)
            if (length(overlapping.compartments)>1)
                stop(paste0(error.prefix, "'from.compartments' and 'to.compartments' for transition in dimension '", comp$dimension,
                            "' for '", comp$group, "'",
                            ifelse(tag=='transition', "", paste0(" with tag='", comp$tag, "'")),
                            " both include ",
                            collapse.with.and("'", overlapping.compartments, "'"),
                            " after substituting in aliases; 'from.compartments' and 'to.compartments' for a transition cannot overlap"))
            
            # Split into one component for each single-pair combo for a from and to compartment
            rv = list()
            for (one.from.compartment in comp$from.compartments)
            {
                for (one.to.compartment in comp$to.compartments)
                {
                    new.comp = comp
                    new.comp$from.compartments = one.from.compartment
                    new.comp$to.compartments = one.to.compartment
                    new.comp$name = private$get.component.name(new.comp)
                    
                    rv = c(rv, list(new.comp))
                }
            }
            
            rv
        }
    )
)

# Make a list of all the schemata for Core Components

CORE.COMPONENT.SCHEMATA = list(
    mortality = CORE.COMPONENT.SCHEMA$new(type = 'mortality',
                                          mechanism.types = 'mortality.rate',
                                          required = T,
                                          group.names = 'group',
                                          ontology.name.for.mechanism = c(mortality.rate='group'),
                                          applies.to.name.for.mechanism = c(mortality.rate='applies.to'),
                                          trackable.types = 'mortality',
                                          ontology.name.for.trackable = c(mortality='group'),
                                          applies.to.name.for.trackable = c(mortality='applies.to')),
    natality = NATALITY.CORE.COMPONENT.SCHEMA$new(),
    aging = AGING.CORE.COMPONENT.SCHEMA$new(),
    transition = TRANSITION.CORE.COMPONENT.SCHEMA$new(),
    transmission = TRANSMISSION.CORE.COMPONENT.SCHEMA$new(),
    remission = CORE.COMPONENT.SCHEMA$new(type='remission',
                                          mechanism.types = c('remission.rate','remission.proportions'),
                                          required = F,
                                          ontology.name.for.mechanism=c(remission.rate='infected',
                                                                         remission.proportions='infected.plus.uninfected'),
                                          required.sub.ontology.name.for.mechanism=c(remission.proportions='uninfected.compartments'),
                                          applies.to.name.for.mechanism=c(remission.rate='applies.to',
                                                                          remission.proportions='applies.to'),
                                          into.compartments.name.for.mechanism=c(remission.proportions='all.remissions.into.compartments'),
                                          trackable.types = c('remission.from','remission.to'),
                                          ontology.name.for.trackable = c(remission.from='infected',
                                                                          remission.to='uninfected'),
                                          applies.to.name.for.trackable = c(remission.from='applies.to',
                                                                            remission.to='applies.to'),
                                          into.compartments.name.for.trackable = c(remission.to='all.remissions.into.compartments')),
    initial.population = CORE.COMPONENT.SCHEMA$new(type='initial.population',
                                                   mechanism.types = 'initial.population',
                                                   required = T,
                                                   group.names = 'group',
                                                   required.sub.ontology.name.for.mechanism = c(initial.population='group'),
                                                   ontology.name.for.mechanism = c(initial.population='group'),
                                                   applies.to.name.for.mechanism = character(),
                                                   trackable.types = character())
)
names(CORE.COMPONENT.SCHEMATA) = sapply(CORE.COMPONENT.SCHEMATA, function(sch){
    sch$type
})

CORE.COMPONENT.SCHEMATA.FOR.MECHANISMS = list()
for (sch in CORE.COMPONENT.SCHEMATA)
{
    for (mechanism.type in sch$mechanism.types)
        CORE.COMPONENT.SCHEMATA.FOR.MECHANISMS[[mechanism.type]] = sch
}


##-----------------------------------##
##-----------------------------------##
##-- MODEL OUTCOME CLASS HIERARCHY --##
##-----------------------------------##
##-----------------------------------##


MODEL.OUTCOME = R6::R6Class(
    'model.outcome',
    portable = F,
    
    public = list(
        
        initialize = function(name,
                              version,
                              sub.versions,
                              outcome.metadata,
                              value.is.numerator,
                              scale,
                              corresponding.data.outcome,
                              denominator.outcome,
                              keep.dimensions,
                              exclude.dimensions,
                              subset.dimension.values,
                              rename.dimension.values,
                              save,
                              from.year,
                              to.year,
                              required.scale = NULL,
                              dimension.aliases = NULL,
                              dimension.alias.suffix = NULL)
        {
            # Validate name
            validate.outcome.name(name, 
                                  descriptor = type,
                                  error.prefix = paste0("Error setting tracking for ", self$descriptor, ": "))
            
            error.prefix = paste0("Error setting tracking for ", self$descriptor, " '", name, "': ")
            
            # Validate version
            if (!is.character(version) || length(version)!=1 || is.na(version))
                stop(paste0(error.prefix, "'version' must be a single, non-NA character value"))
            
            # Validate sub-versions
            if (length(sub.versions)==0)
                sub.versions = NULL
            else if (!is.character(sub.versions) || any(is.na(sub.versions)) || any(nchar(sub.versions)==0))
                stop(paste0(error.prefix, "sub.versions must either be NULL or a character vector with no empty or NA values"))
           
            
            # Validate save
            if (!is.logical(save) || length(save)!=1 || is.na(save))
                stop(paste0(error.prefix, "'save must be a single, non-NA logical value"))
            
            # Validate scale
            if (!is.null(scale))
            {
                check.model.scale(scale, varname.for.error = 'scale', error.prefix = error.prefix)
                if (!is.null(required.scale) && all(scale != required.scale))
                    stop(paste0(error.prefix, "The scale for a ", self$descriptor, " must be ", 
                                ifelse(length(required.scale)==1, '', 'one of '),
                                collapse.with.or("'", required.scale, "'"),
                                " (not '", scale, "')"))
            }
            
            # Validate outcome.metadata
            if (is.null(outcome.metadata))
            {
                if (is.null(scale))
                    stop(paste0(error.prefix, "If outcome.metadata is NULL, scale must be specified (it can NOT be NULL)"))
                
                if (save)
                    stop(paste0(error.prefix, "If save is TRUE, outcome.metadata cannot be NULL"))
            }
            else
            {
                if (!is(outcome.metadata, 'outcome.metadata'))
                    stop(paste0(error.prefix, "'outcome.metadata' must be of class 'outcome.metadata', as created by create.outcome.metadata()"))
                
                if (!is.null(required.scale))
                {
                    if (all(outcome.metadata$scale!=required.scale))
                        stop(paste0(error.prefix, "The outcome.metadata$scale for a ", self$descriptor, " must be ", 
                                    ifelse(length(required.scale)==1, '', 'one of '),
                                    collapse.with.or("'", required.scale, "'"),
                                    " (not '", outcome.metadata$scale, "')"))

                    # Don't need to check for equality here
                    # We know that both scale (if present) and outcome.metadata$scale are one of the required scales
                    #  Just set them to be the same                    
                    scale = outcome.metadata$scale
                }
                else
                {
                    if (is.null(scale))
                        scale = outcome.metadata$scale
                    else if (scale != outcome.metadata$scale)
                        stop(paste0(error.prefix, "If 'scale' is specified, it must match the scale given by outcome.metadata$scale"))
                }
            }
            
            # Validate value.is.numerator
            if (scale == 'non.negative.number' || scale=='number')
                value.is.numerator = NULL
            else
            {
                if (!is.logical(value.is.numerator) || length(value.is.numerator)!=1 || is.na(value.is.numerator))
                    stop(paste0(error.prefix, "'value.is.number' must be a single, non-NA logical value"))
            }
            
            # Validate denominator.outcome
            if (scale != 'non.negative.number' && scale != 'number')
            {
                if (is.null(denominator.outcome))
                    stop(paste0(error.prefix, "When the outcome's scale is '", scale, "', a denominator.outcome must be specified (to be able to aggregate this outcome)"))
            }
            
            if (!is.null(denominator.outcome) &&
                (!is.character(denominator.outcome) || length(denominator.outcome)!=1 || is.na(denominator.outcome)))
                stop(paste0(error.prefix, "'denominator.outcome' must be a single, non-NA character value"))
            
            
            # Validate corresponding.data.outcome
            if (!is.null(corresponding.data.outcome))
            {
                if (!is.character(corresponding.data.outcome) || length(corresponding.data.outcome) != 1 || is.na(corresponding.data.outcome))
                    stop(paste0(error.prefix, "If it is not NULL, 'corresponding.data.outcome' must be a single, non-NA character value"))
                
                if (!save)
                    stop(paste0(error.prefix, "Setting a value for 'corresponding.data.outcome' when save==FALSE does not make much sense. If you want to tie the outcome to data, then you should save it (or else, leave off the corresponding data)"))
            }
            
            # Validate keep.dimension
            # Validate exclude.dimensions
            if (length(keep.dimensions)==0)
            {
                keep.dimensions = NULL
                
                if (length(exclude.dimensions)==0)
                    exclude.dimensions = NULL
                else
                {
                    if (!is.character(exclude.dimensions) || any(is.na(exclude.dimensions)))
                        stop(paste0(error.prefix, "'exclude.dimensions' must be a character vector with no NA values"))
                }
            }
            else
            {
                if (length(exclude.dimensions)==0)
                    exclude.dimensions = NULL
                else
                    stop(paste0(error.prefix, "You cannot specify BOTH 'keep.dimensions' and 'exclude.dimensions' - one or the other must be NULL"))
                
                if (!is.character(keep.dimensions) || any(is.na(keep.dimensions)))
                    stop(paste0(error.prefix, "'keep.dimensions' must be a character vector with no NA values"))
            }
            
            # Validate subset.dimension.values
            if (is.null(subset.dimension.values))
                subset.dimension.values = list()
            check.dimension.values.valid(subset.dimension.values, 
                                         variable.name.for.error = 'subset.dimension.values', 
                                         allow.empty = T,
                                         error.prefix = error.prefix)
            
            # Validate rename.dimension.values
            if (is.null(rename.dimension.values))
                rename.dimension.values = list()
            check.dimension.values.valid(rename.dimension.values, 
                                         variable.name.for.error = 'rename.dimension.values', 
                                         allow.empty = T,
                                         error.prefix = error.prefix)
            
            sapply(rename.dimension.values, function(values){
                if (is.null(names(values)) || any(is.na(names(values))) || any(nchar(names(values))==0) || max(table(names(values)))>1)
                    stop(paste0(error.prefix,
                                "'rename.dimension.values' must be a named list of NAMED character vectors"))
            })
            
            # Validate from.year, to.year
            if (!is.numeric(from.year) || length(from.year)!=1 || is.na(from.year))
                stop(paste0(error.prefix, "'from.year' must be a single, non-NA, numeric value"))
            
            if (!is.numeric(to.year) || length(to.year)!=1 || is.na(to.year))
                stop(paste0(error.prefix, "'to.year' must be a single, non-NA, numeric value"))
            
            if (from.year > to.year)
                stop(paste0(error.prefix, "'from.year' (", from.year, ") must be BEFORE 'to.year' (", to.year, ")"))
            
            # Validate aliases
            if (!is.null(dimension.alias.suffix))
            {
                if (!is.null(dimension.aliases))
                    stop(paste0(error.prefix, "You cannot specify BOTH 'dimension.aliases' and 'dimension.alias.suffix' - at least one of the two must be NULL"))
                
                # if (!is.character(dimension.alias.suffix) || length(dimension.alias.suffix)!=1 || 
                #     is.na(dimension.alias.suffix) || nchar(dimension.alias.suffix)==0)
                #     stop(paste0(error.prefix, "'If 'dimension.alias.suffix' is not NULL, it must be a single, non-NA, non-empty character value"))
                
                if (!is.character(dimension.alias.suffix) || length(dimension.alias.suffix)!=1 || is.na(dimension.alias.suffix) ||
                    (dimension.alias.suffix != 'from' && dimension.alias.suffix != 'to'))
                    stop(paste0(error.prefix, "If it is not NULL, 'dimension.alias.suffix' must be a single, non-NA character value that is either 'from' or 'to'"))
            }
            
            if (!is.null(dimension.aliases))
            {
                if (!is.character(dimension.aliases) || length(dimension.aliases)==0 ||
                    any(is.na(dimension.aliases)))
                    stop(paste0(error.prefix, "If 'dimension.aliases' is not NULL, it must be a non-empty character vector with no NA values"))

                if (is.null(names(dimension.aliases)) ||
                    any(is.na(names(dimension.aliases))))
                    stop(paste0(error.prefix, "If 'dimension.aliases' is not NULL, it must be a NAMED character vector with no NA names"))

            }
            
            
            
            # Store variables
            private$i.name = private$i.original.name = name
            private$i.version = version
            private$i.sub.versions = sub.versions
            private$i.denominator.outcome = denominator.outcome
            private$i.type = class(self)[1]
            private$i.metadata = outcome.metadata
            private$i.value.is.numerator = value.is.numerator
            private$i.scale = scale
            private$i.keep.dimensions = unique(keep.dimensions)
            private$i.exclude.dimensions = unique(exclude.dimensions)
            private$i.subset.dimension.values = subset.dimension.values
            private$i.rename.dimension.values = rename.dimension.values
            private$i.corresponding.data.outcome = corresponding.data.outcome
            private$i.save = save
            private$i.from.year = from.year
            private$i.to.year = to.year
            private$i.dimension.alias.suffix = dimension.alias.suffix
            private$i.dimension.aliases = dimension.aliases
        },
        
        compile = function(specification, error.prefix)
        {
            rv = self$clone(deep=T)
            rv$resolve.compartment.aliases(specification$resolved.aliases)
            rv
        },
        
        resolve.compartment.aliases = function(aliases)
        {
            private$i.subset.dimension.values = apply.aliases(private$i.subset.dimension.values, aliases)
            
            self
        },
        
        validate.arguments = function()
        {
            # Instantiate in
        },
        
        get.inherent.dim.names = function(specification)
        {
            NULL
        },
        
        rename = function(name)
        {
            private$i.save = F
            private$i.name = name
            self
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
        
        rename.depends.on.outcomes = function(mapping)
        {
            # Nothing to do in default version of the method
        },
        
        rename.depends.on.quantities = function(mapping)
        {
            # Nothing to do in default version of the method
        },
        
        derive.dim.names = function(specification, all.outcomes, error.prefix, set=F)
        {
            error.prefix = paste0(error.prefix, "Cannot derive dim.names for outcome ",
                                  self$get.original.name(specification$version), " - ")
            
            dim.names = max.dim.names = 
                private$derive.max.dim.names(specification = specification, 
                                             all.outcomes = all.outcomes,
                                             include.denominator.outcome = F,
                                             error.prefix = error.prefix)
            
            if (set && is.null(dim.names) && length(private$i.keep.dimensions)>0)
            {
                # Would it be better to do a top-down calculation here?
                
                invalid.dimensions = setdiff(private$i.keep.dimensions, names(specification$ontologies$all))
                if (length(invalid.dimensions)>0)
                {
                    stop(paste0(error.prefix,
                                "Outcome ", self$get.original.name(wrt.version=specification$version),
                                " cannot infer its dim.names and so will derive them from the specifications ontologies. However, ",
                                ifelse(length(invalid.dimensions)==1, "dimension ", "dimensions "),
                                collapse.with.and("'", invalid.dimensions, "'"),
                                " are not present in the specification ontologies"))
                }
                
                dim.names = max.dim.names = specification$ontologies$all[private$i.keep.dimensions]
            }
            
            if (set || !is.null(dim.names))
            {
                if (!is.null(private$i.keep.dimensions))
                {
                    missing.dimensions = setdiff(private$i.keep.dimensions, names(dim.names))
                    if (length(missing.dimensions)>0)
                    {
                        stop(paste0(error.prefix,
                                    "Outcome ", self$get.original.name(wrt.version=specification$version),
                                    " has ",
                                    ifelse(length(missing.dimensions)==1, "keep.dimension ", "keep.dimensions "),
                                    collapse.with.and("'", missing.dimensions, "'"),
                                    " but ",
                                    ifelse(length(dim.names)==0,
                                           "the derived dimensions for the outcome indicate it must be dimensionless",
                                           paste0(ifelse(length(missing.dimensions)==1, " it is ", " they are "),
                                                  "not present in the derived set of possible for the dimensions for the outcome (",
                                                  collapse.with.and("'", names(dim.names), "'"), ")"))
                                    ))
                    }
                    
                    dim.names = dim.names[private$i.keep.dimensions]
                }
                
                if (!is.null(private$i.exclude.dimensions))
                {
                    dim.names = dim.names[setdiff(names(dim.names), private$i.exclude.dimensions)]
                }
            }
            
            
            # Generate the renamed ontology
            missing.rename.dimensions = setdiff(names(private$i.rename.dimension.values), names(dim.names))
            if (length(missing.rename.dimensions)>0)
            {
                stop(paste0(error.prefix,
                            "The rename.dimension.values argument for the outcome '",
                            self$get.original.name(specification$version), 
                            "' contained ",
                            ifelse(length(missing.rename.dimensions)==1, "dimension ", "dimensions "),
                            collapse.with.and("'", missing.rename.dimensions, "'"),
                            ". ",
                            ifelse(length(missing.rename.dimensions)==1, "It is", "They are"),
                            " not present in the calculated dimnames for the outcome"))
            }
            
            renamed.dim.names = do.rename.dim.names(outcome = self,
                                                    dim.names,
                                                    rename.dimension.values = private$i.rename.dimension.values,
                                                    error.prefix = paste0(error.prefix, "Error renaming outcome's dimnames - "))
            
            
            if (set)
            {
                # Check that the denominator can accommodate (are supsersets of) renamed.dim.names
                if (!is.null(private$i.denominator.outcome))
                {
                    denominator.outcome = all.outcomes[[private$i.denominator.outcome]]
                    denominator.dim.names = denominator.outcome$derive.dim.names(specification,
                                                                                 all.outcomes = all.outcomes,
                                                                                 error.prefix = error.prefix,
                                                                                 set = F)
                    denominator.dim.names = apply.outcome.dimension.aliases.to.dim.names(self,
                                                                                         denominator.dim.names)
      
                    # A lot of work below into printing a useful error message
                    if (!dim.names.are.subset(sub.dim.names=renamed.dim.names, super.dim.names=denominator.dim.names))
                    {
                        dimensions = names(renamed.dim.names)
                        denominator.dimensions = names(denominator.dim.names)
                        
                        missing.dimensions = setdiff(dimensions, denominator.dimensions)
                        if (length(missing.dimensions)>0)
                        {
                            error.details = paste0("The denominator outcome (with ",
                                                   ifelse(length(denominator.dimensions)==0, "no dimensions",
                                                          ifelse(length(denominator.dimensions)==1, "dimension", "dimensions")),
                                                   collapse.with.and("'", denominator.dimensions, "'"),
                                                   ") is missing ",
                                                   ifelse(length(missing.dimensions)==1, "dimension ", "dimensions "),
                                                   collapse.with.and("'", missing.dimensions, "'"),
                                                   ", which are present in the outcome's calculated dimensions.")
                        }
                        else
                        {
                            missing.values.per.dimension = sapply(dimensions, function(d){
                                setdiff(renamed.dim.names[[d]], denominator.dim.names[[d]])
                            })
                            dimensions.with.missing.mask = sapply(missing.values.per.dimension, length)>0
                            
                            dimensions.with.missing = dimensions[dimensions.with.missing.mask]
                            missing.values.text = sapply(missing.values.per.dimension[dimensions.with.missing.mask], function(val){
                                if (length(val)==1)
                                    paste0("value '", val, "'")
                                else
                                    paste0("values ", collapse.with.and("'", val, "'"))
                            })
                            
                            error.details = paste0("Dimension values which are present in the outcome's calculated dim.names are missing from the denominator's dim.names: ",
                                                   paste0("Dimension '", dimensions.with.missing, "' is missing ", missing.values.text,
                                                          collapse='. '),
                                                   ".")
                        }
                        
                        stop(paste0(error.prefix,
                                    "The denominator outcome (",
                                    denominator.outcome$get.original.name(specification$version),
                                    ") cannot accomodate the dimensions for the outcome",
                                    ifelse(private$i.value.is.numerator, '', ' prior to aggregating'),
                                    ". ",
                                    error.details))
                    }
                }
                
                # Check that all quantities can accommodate (are subsets of) max.dim.names
                # (and confirm that quantities do not have NULL max.dim.names)
                for (quant in private$get.all.depends.on.quantities(specification, all.outcomes=all.outcomes, error.prefix=error.prefix))
                {
                    if (is.null(quant$max.dim.names))
                    {
                        # I believe this is opening us up to an error here
                        # the max dim names set here might be incompatible with max dim names set on the same quantity from
                        #  this same block of code for another outcome
                        #   will deal with it when it comes up
                        recursively.set.dim.names.and.aliases(quantity = quant, 
                                                              specification = specification,
                                                              max.dim.names = max.dim.names,
                                                              required.dim.names = NULL,
                                                              max.dimensions = NULL,
                                                              dimension.aliases = NULL,
                                                              error.prefix = error.prefix)
                        
                        # stop(paste0(error.prefix,
                        #             "The max.dim.names of quantity ", quant$get.original.name(specification$version),
                        #             ", on which the outcome depends, cannot be inferred from the model specification"))
                    }
                    
                    quant.max.dim.names = apply.outcome.dimension.aliases.to.dim.names(self,
                                                                                       quant$max.dim.names)
                    extra.dimensions.in.quantity = setdiff(names(quant.max.dim.names), names(max.dim.names))
                    if (length(extra.dimensions.in.quantity)>0)
                        stop(paste0(error.prefix,
                                    "The calculated possible dimensions which quantity ", quant$get.original.name(specification$version),
                                    ", on which the outcome depends, can have, are broader than the inferred dimensions for the outcome. ",
                                    ifelse(length(extra.dimensions.in.quantity)==1, "Dimension ", "Dimensions "),
                                    collapse.with.and("'", extra.dimensions.in.quantity, "'"),
                                    " present in the quantity ('", quant$get.original.name(specification$version),
                                    "') but not in the outcome ('",
                                    self$get.original.name(specification$version),"')"))
                    
                    
                    missing.values.per.dimension = sapply(names(quant.max.dim.names), function(d){
                        setdiff(max.dim.names[[d]], quant.max.dim.names[[d]])
                    })
                    dimensions.with.missing.mask = sapply(missing.values.per.dimension, length)>0
                    
                    if (any(dimensions.with.missing.mask))
                    {
                        dimensions.with.missing = dimensions[dimensions.with.missing.mask]
                        missing.values.text = sapply(missing.values.per.dimension[dimensions.with.missing.mask], function(val){
                            if (length(val)==1)
                                paste0("value '", val, "'")
                            else
                                paste0("values ", collapse.with.and("'", val, "'"))
                        })
                        
                        error.details = paste0("Dimension values which are present in the calculated dim.names of quantity ", quant$get.original.name(specification$version),
                                               ", on which the outcome depends, are missing from the outcome's dim.names: ",
                                               paste0("Dimension '", dimensions.with.missing, "' is missing ", missing.values.text,
                                                      collapse='. '),
                                               ".")
                    }
                }
            
                # Set the ontologies
                to.be.incomplete = intersect(names(dim.names), incomplete.dimensions(specification$ontologies$all))
                private$i.unrenamed.ontology = as.ontology(dim.names, incomplete.dimensions = to.be.incomplete)
                private$i.ontology = as.ontology(renamed.dim.names, incomplete.dimensions = to.be.incomplete)
            }
            
            renamed.dim.names
        },
        
        
        create.top.level.references = function(specification, all.outcomes, error.prefix)
        {
            dep.on.quantity.names = setdiff(union(self$depends.on.quantities,
                                                  self$depends.on.quantities.or.outcomes),
                                            names(private$get.all.depends.on.outcomes(all.outcomes = all.outcomes,
                                                                                      include.denominator.outcome = T,
                                                                                      error.prefix = error.prefix)))
            
            dim.names = private$derive.max.dim.names(specification = specification, 
                                                     all.outcomes = all.outcomes,
                                                     include.denominator.outcome = F,
                                                     error.prefix = error.prefix)
            # 
            # if (is.null(dim.names))
            #     dim.names = specification$ontologies$all[private$i.keep.dimensions]
            
            if (is.null(private$i.denominator.outcome) || private$i.scale=='number' || private$i.scale=='non.negative.number' || private$i.value.is.numerator)
                max.dimensions = NULL
            else
            {
                denominator.outcome = all.outcomes[[private$i.denominator.outcome]]
                max.dimensions = names(denominator.outcome$derive.dim.names(specification,
                                                                            all.outcomes = all.outcomes,
                                                                            error.prefix = error.prefix,
                                                                            set = F))
            }
            
            lapply(dep.on.quantity.names, function(dep.on.name){
                
                TOP.LEVEL.REFERENCE$new(specification = specification,
                                        version = self$version,
                                        ontology.name = NULL,
                                        dim.names = dim.names,
                                        value.quantity.name = dep.on.name,
                                        source = paste0("Outcome ", self$get.original.name(wrt.version=specification$version)),
                                        applies.to = NULL,
                                        required.sub.ontology.name=NULL,
                                        exclude.ontology.dimensions=character(),
                                        alias.suffix = private$i.dimension.alias.suffix,
                                        for.core.component.type = NULL,
                                        error.prefix = error.prefix)
            })
        },
        
        # calculate.value.and.denominator = function(outcome.values.and.denominators,
        #                                            dynamic.and.intrinsic.numerators,
        #                                            quantity.values,
        #                                            quantity.after.values,
        #                                            quantity.times)
        # {
        #     value = private$calculate.value(outcome.values.and.denominators,
        #                             dynamic.and.intrinsic.numerators,
        #                             quantity.values,
        #                             quantity.after.values,
        #                             quantity.times)
        #     
        #     if (is.null(private$i.denominator.outcome))
        #         denominator.value = NULL
        #     else
        #         private$calculate.denominator.value(outcome.values.and.denominators,
        #                                             dynamic.and.intrinsic.numerators,
        #                                             quantity.values,
        #                                             quantity.after.values,
        #                                             quantity.times)
        #     
        #     # Expand years if we need to
        #     # Expand 
        # },
        
        # here for now until we replace outcomes with outcome.kernels in the engine code
        calculate.values = function(desired.times,
                                    bindings,
                                    binding.times,
                                    cumulative.interval = 1,
                                    error.prefix)
        {
            fn = get.calculate.values.function(self, error.prefix='error in temp get.calculate.values.function')
            fn(desired.times,
               bindings,
               binding.times,
               cumulative.interval,
               error.prefix)
        },
        
        get.calculate.values.function = function(parent.environment,
                                                 error.prefix)
        {
            env = new.env(parent = parent.environment)
            env$descriptor = self$descriptor
            
            fn = function(desired.times,
                          bindings,
                          binding.times,
                          cumulative.interval = 1,
                          error.prefix)
            {
                stop(paste0(error.prefix, "calculate.values does not apply for this ", descriptor, " - the logic of engine code should not result in this getting called"))
            }
            
            environment(fn) = env
            fn
        }
    ),
    
    active = list(
        
        descriptor = function(value)
        {
            stop("A model outcome's descriptor must be implemented at the subclass level")
        },
        
        name = function(value)
        {
            if (missing(value))
                private$i.name
            else
                stop(paste0("Cannot modify a model outcome's 'name' - it is read-only"))
        },
        
        original.name = function(value)
        {
            if (missing(value))
                private$i.original.name
            else
                stop(paste0("Cannot modify a model outcome's 'original.name' - it is read-only"))
        },
        
        version = function(value)
        {
            if (missing(value))
                private$i.version
            else
                stop(paste0("Cannot modify a model outcome's 'version' - it is read-only"))
        },
        
        sub.versions = function(value)
        {
            if (missing(value))
                private$i.sub.versions
            else
                stop(paste0("Cannot modify a model outcome's 'sub.versions' - they are read-only"))
        },
        
        denominator.outcome = function(value)
        {
            if (missing(value))
                private$i.denominator.outcome
            else
                stop(paste0("Cannot modify a model outcome's 'denominator.outcome' - it is read-only"))
        },
        
        type = function(value)
        {
            if (missing(value))
                private$i.type
            else
                stop(paste0("Cannot modify a model outcome's 'type' - it is read-only"))
        },
        
        metadata = function(value)
        {
            if (missing(value))
                private$i.metadata
            else
                stop(paste0("Cannot modify a model outcome's 'metadata' - it is read-only"))
        },
        
        scale = function(value)
        {
            if (missing(value))
                private$i.scale
            else
                stop(paste0("Cannot modify a model outcome's 'scale' - it is read-only"))
        },
        
        depends.on = function(value)
        {
            if (missing(value))
                c(self$depends.on.quantities,
                  self$depends.on.outcomes,
                  self$depends.on.quantities.or.outcomes)
            else
                stop(paste0("Cannot modify a model outcome's 'depends.on' - it is read-only"))
        },
        
        depends.on.quantities = function(value)
        {
            if (missing(value))
                character()
            else
                stop(paste0("Cannot modify a model outcome's 'depends.on.quantities' - it is read-only"))
        },
        
        depends.on.outcomes.except.denominator = function(value)
        {
            if (missing(value))
                character()
            else
                stop(paste0("Cannot modify a model outcome's 'depends.on.outcomes.except.denominator' - it is read-only"))
        },
        
        depends.on.outcomes = function(value)
        {
            if (missing(value))
            {
                union(self$depends.on.outcomes.except.denominator, private$i.denominator.outcome)
            }
            else
                stop(paste0("Cannot modify a model outcome's 'depends.on.outcomes' - it is read-only"))
        },
        
        depends.on.quantities.or.outcomes = function(value)
        {
            if (missing(value))
                character()
            else
                stop(paste0("Cannot modify a model outcome's 'depends.on.quantities.or.outcomes' - it is read-only"))
        },
        
        depends.on.cumulative = function(value)
        {
            if (missing(value))
            {
                if (self$is.cumulative)
                    c(self$depends.on, private$i.denominator.outcome)
                else
                    character()
            }
            else
                stop(paste0("Cannot modify a model outcome's 'depends.on.cumulative' - it is read-only"))
        },
        
        depends.on.non.cumulative = function(value)
        {
            if (missing(value))
                setdiff(self$depends.on, self$depends.on.cumulative)
            else
                stop(paste0("Cannot modify a model outcome's 'depends.on.non.cumulative' - it is read-only"))
        },
        
        is.cumulative = function(value)
        {
            stop(paste0("A model outcome's 'is.cumulative' must be defined at the subclass level"))
        },
        
        is.intrinsic = function(value)
        {
            if (missing(value))
                F
            else
                stop(paste0("Cannot modify a model outcome's 'is.intrinsic' - it is read-only"))
        },
        
        is.dynamic = function(value)
        {
            if (missing(value))
                F
            else
                stop(paste0("Cannot modify a model outcome's 'is.dynamic' - it is read-only"))
        },
        
        keep.dimensions = function(value)
        {
            if (missing(value))
                private$i.keep.dimensions
            else
                stop(paste0("Cannot modify a model outcome's 'keep.dimensions' - it is read-only"))
        },
        
        exclude.dimensions = function(value)
        {
            if (missing(value))
                private$i.exclude.dimensions
            else
                stop(paste0("Cannot modify a model outcome's 'exclude.dimensions' - it is read-only"))
        },
        
        subset.dimension.values = function(value)
        {
            if (missing(value))
                private$i.subset.dimension.values
            else
                stop(paste0("Cannot modify a model outcome's 'subset.dimension.values' - it is read-only"))
        },
        
        rename.dimension.values = function(value)
        {
            if (missing(value))
                private$i.rename.dimension.values
            else
                stop(paste0("Cannot modify a model outcome's 'rename.dimension.values' - it is read-only"))
        },
        
        save = function(value)
        {
            if (missing(value))
                private$i.save
            else
                stop(paste0("Cannot modify a model outcome's 'save' - it is read-only"))
        },
        
        unrenamed.ontology = function(value)
        {
            if (missing(value))
                private$i.unrenamed.ontology
            else
                stop(paste0("Cannot modify a model outcome's 'unrenamed.ontology' - it is read-only"))
        },
        
        ontology = function(value)
        {
            if (missing(value))
                private$i.ontology
            else
                stop(paste0("Cannot modify a model outcome's 'ontology' - it is read-only"))
        },
        
        unrenamed.dim.names = function(value)
        {
            if (missing(value))
                private$i.unrenamed.ontology
            else
                stop(paste0("Cannot modify a model outcome's 'unrenamed.dim.names' - they are read-only"))
        },
        
        dim.names = function(value)
        {
            if (missing(value))
                private$i.ontology
            else
                stop(paste0("Cannot modify a model outcome's 'dim.names' - they are read-only"))
        },
        
        corresponding.data.outcome = function(value)
        {
            if (missing(value))
                private$i.corresponding.data.outcome
            else
                stop(paste0("Cannot modify a model outcome's 'corresponding.data.outcome' - it is read-only"))
        },
        
        from.year = function(value)
        {
            if (missing(value))
                private$i.from.year
            else
                stop(paste0("Cannot modify a model outcome's 'from.year' - it is read-only"))
        },
        
        to.year = function(value)
        {
            if (missing(value))
                private$i.to.year
            else
                stop(paste0("Cannot modify a model outcome's 'to.year' - it is read-only"))
        },
        
        value.is.numerator = function(value)
        {
            if (missing(value))
                private$i.value.is.numerator
            else
                stop(paste0("Cannot modify a model outcome's 'value.is.numerator' - it is read-only"))
        },
        
        dimension.alias.suffix = function(value)
        {
            if (missing(value))
                private$i.dimension.alias.suffix
            else
                stop(paste0("Cannot modify a model outcome's 'dimension.alias.suffix' - it is read-only"))
        },
        
        dimension.aliases = function(value)
        {
            if (missing(value))
                private$i.dimension.aliases
            else
                stop(paste0("Cannot modify a model outcome's 'dimension.aliases' - they are read-only"))
        }
    ),
    
    private = list(
        
        i.name = NULL,
        i.original.name = NULL,
        i.version = NULL,
        i.sub.versions = NULL,
        i.type = NULL,
        i.scale = NULL,
        i.metadata = NULL,
        i.denominator.outcome = NULL,
        i.value.is.numerator = NULL,
        
        i.corresponding.data.outcome = NULL,
        i.keep.dimensions = NULL,
        i.exclude.dimensions = NULL,
        i.subset.dimension.values = NULL,
        i.rename.dimension.values = NULL,
        i.dimension.alias.suffix = NULL,
        i.dimension.aliases = NULL,
        
        i.save = NULL,
        
        i.ontology = NULL,
        i.unrenamed.ontology = NULL,
        
        i.from.year = NULL,
        i.to.year = NULL,
        
        # will check the max possible dim.names to make sure they are compatible with subset.dimension.values
        derive.max.dim.names = function(specification, all.outcomes, 
                                        include.denominator.outcome,
                                        error.prefix)
        {
            dim.names = private$derive.max.possible.dim.names(specification = specification,
                                                              include.denominator.outcome = include.denominator.outcome,
                                                              all.outcomes = all.outcomes,
                                                              error.prefix = error.prefix)

            if (!is.null(private$i.subset.dimension.values))
            {
                missing.dimensions = setdiff(names(private$i.subset.dimension.values), names(dim.names))
                if (length(missing.dimensions)>0)
                    stop(paste0(error.prefix,
                                "Outcome ", self$get.original.name(wrt.version=specification$version),
                                " has subset.dimension.values that include ",
                                ifelse(length(missing.dimensions)==1, "dimension ", "dimensions "),
                                collapse.with.and("'", missing.dimensions, "'"),
                                " but ",
                                ifelse(length(dim.names)==0,
                                       "the outcome must be dimensionless",
                                       paste0(ifelse(length(missing.dimensions)==1, " it is ", " they are "),
                                              "not present in the derived set of possible for the dimensions for the outcome (",
                                              collapse.with.and("'", names(dim.names), "'"), ")"))
                    ))
                
                for (d in names(private$i.subset.dimension.values))
                {
                    missing.values = setdiff(private$i.subset.dimension.values[[d]], dim.names[[d]])
                    if (length(missing.values)>0)
                        stop(paste0(error.prefix,
                                    "Outcome ", self$get.original.name(wrt.version=specification$version),
                                    " has subset.dimension.values that include ",
                                    ifelse(length(missing.dimensions)==1, "value ", "values "),
                                    collapse.with.and("'", missing.values, "'"),
                                    " for dimension '", d, "', but ",
                                    paste0(ifelse(length(missing.dimensions)==1, " it is ", " they are "),
                                           "not present in the derived set of values for dimension '", d, "' (",
                                           collapse.with.and("'", dim.names[[d]], "'"), ")")))
                }
                
                dim.names[names(private$i.subset.dimension.values)] = private$i.subset.dimension.values
            }
            
            dim.names
        },
        
        # max dim.names before taking into account subset.dimension.values
        derive.max.possible.dim.names = function(specification, all.outcomes, 
                                                 include.denominator.outcome,
                                                 error.prefix)
        {
            dep.on.outcomes = private$get.all.depends.on.outcomes(all.outcomes = all.outcomes,
                                                                  include.denominator.outcome = include.denominator.outcome,
                                                                  error.prefix = error.prefix)

            rv = NULL

            for (dep.on in dep.on.outcomes)
            {
                dep.on.dim.names = dep.on$derive.dim.names(specification = specification,
                                                           all.outcomes = all.outcomes,
                                                           error.prefix = error.prefix)
                dep.on.dim.names = apply.outcome.dimension.aliases.to.dim.names(self, dep.on.dim.names)
                rv = intersect.shared.dim.names(rv, dep.on.dim.names)
            }
            
            dep.on.quantities = get.all.depends.on.quantities(specification = specification,
                                                              all.outcomes = all.outcomes,
                                                              error.prefix = error.prefix)

            for (quant in dep.on.quantities)
            {
                quant.max.dim.names = apply.outcome.dimension.aliases.to.dim.names(self, quant$max.dim.names)
                if (!is.null(quant$max.dim.names))
                    rv = intersect.joined.dim.names(rv, quant.max.dim.names)
            }

            rv
        },
        
        get.all.depends.on.outcomes = function(all.outcomes,
                                               include.denominator.outcome,
                                               error.prefix)
        {
            if (include.denominator.outcome)
                rv = all.outcomes[self$depends.on.outcomes]
            else
                rv = all.outcomes[self$depends.on.outcomes.except.denominator]
            
            missing.outcomes = sapply(rv, is.null)
            if (any(missing.outcomes))
                stop(paste0(error.prefix, "Outcome '",
                            self$name, "' depends on ",
                            ifelse(sum(missing.outcomes)==1, 'outcome ', 'outcomes '),
                            collapse.with.and("'", self$depends.on.outcomes[missing.outcomes], "'"),
                            ", but ",
                            ifelse(sum(missing.outcomes)==1, " is", " are"),
                            " not present in the outcomes for the specification"))
            
            rv = c(rv, all.outcomes[setdiff(self$depends.on.quantities.or.outcomes, c(self$name, self$depends.on.outcomes))])
            rv = rv[!sapply(rv, is.null) & names(rv)!=private$i.name]
            
            rv
        },
        
        get.all.depends.on.quantities = function(specification, all.outcomes, error.prefix)
        {
            quantity.names = setdiff(self$depends.on,
                                     names(private$get.all.depends.on.outcomes(all.outcomes = all.outcomes,
                                                                               include.denominator.outcome = T,
                                                                               error.prefix = error.prefix)))
            
            quantities = lapply(quantity.names, specification$get.quantity)
            names(quantities) = quantity.names
            quantities
        }
    )
)

# A helper for actualizing the rename
do.rename.dim.names <- function(outcome,
                               dim.names,
                               rename.dimension.values,
                               error.prefix)
{
    if (is.null(dim.names))
        NULL
    else
    {
        renamed.dim.names = lapply(names(dim.names), function(d){
            
            values = dim.names[[d]]
            renames = rename.dimension.values[[d]]
            if (!is.null(renames))
            {
                missing.rename.values = setdiff(names(renames), values)
                if (length(missing.rename.values))
                {
                    stop(paste0(error.prefix,
                                "The rename.dimension.values argument for the outcome '",
                                outcome$get.original.name(specification$version), 
                                "' references ",
                                ifelse(length(missing.rename.values)==1, "value ", "value "),
                                collapse.with.and("'", missing.rename.values, "'"),
                                " in the '", d, "' dimension. ",
                                ifelse(length(missing.rename.dimensions)==1, "It is", "They are"),
                                " not present in the calculated dimnames for the outcome"))
                }
                
                apply.aliases(values, aliases = renames)
            }
            else
                values
        })
        names(renamed.dim.names) = names(dim.names)
        renamed.dim.names
    }
}

apply.outcome.dimension.aliases.to.dim.names = function(outcome, dim.names)
{
    if (!is.null(dim.names))
    {
        if (!is.null(outcome$dimension.alias.suffix))
        {
            suffix.to.match = paste0(".", outcome$dimension.alias.suffix)
            mask = substr(names(dim.names), 
                          nchar(names(dim.names))-nchar(suffix.to.match)+1,
                          nchar(names(dim.names))) == suffix.to.match
            
            names(dim.names)[mask] = substr(names(dim.names)[mask],
                                            1,
                                            nchar(names(dim.names)[mask])-nchar(suffix.to.match))
        }
        
        if (!is.null(outcome$dimension.aliases))
        {
            to.overwrite = intersect(names(outcome$dimension.aliases),
                                     names(dim.names))
            
            names(dim.names)[to.overwrite] = outcome$dimension.aliases[to.overwrite]
        }
        
        tabled.dimensions = table(names(dim.names))
        if (length(dim.names)>0 && max(tabled.dimensions)>1)
        {
            duplicate.dimensions = names(tabled.dimensions)[tabled.dimensions>1]
            stop(paste0("After apply dimension aliases to the dimnames of outcome '", outcome$name,
                        "', ",
                        ifelse(length(duplicate.dimensions)==1, "dimension ", "dimensions "),
                        collapse.with.and("'", duplicate.dimensions, "'"),
                        " appear more than once in the dimnames."))
        }
    }            
    
    dim.names
}

INTRINSIC.MODEL.OUTCOME = R6::R6Class(
    'intrinsic.model.outcome',
    inherit = MODEL.OUTCOME,
    portable = F,
    
    public = list(
        
        initialize = function(name, version)
        {
            if (!is.character(name) || length(name)!=1 || is.na(name))
                error.prefix = paste0("Error setting tracking for ", self$descriptor, ": 'name' must be a single, non-NA character value")
            
            if (name=='infected' || name=='uninfected')
            {
                private$i.groups = name
                save = T
                private$i.ontology.name = name
                
                if (name=='infected')
                {
                    outcome.metadata = create.outcome.metadata(display.name = 'HIV-Infected Population',
                                                               description = "The Number of People with HIV (diagnosed and undiagnosed) at a given point in time",
                                                               scale = 'non.negative.number',
                                                               axis.name = 'HIV-Positive Cases',
                                                               units = 'cases',
                                                               singular.unit = 'case')
                    corresponding.data.outcome = 'prevalence'
                }
                else
                {
                    outcome.metadata = create.outcome.metadata(display.name = 'HIV-Negative Population',
                                                               description = "The Number of People without HIV at a given point in time",
                                                               scale = 'non.negative.number',
                                                               axis.name = 'Uninfected People',
                                                               units = 'people',
                                                               singular.unit = 'person')
                    corresponding.data.outcome = NULL
                }
            }
            else if (name=='population')
            {
                private$i.groups = c('infected','uninfected')
                save = F
                private$i.ontology.name = 'general.compartments'
                
                outcome.metadata = create.outcome.metadata(display.name = 'Population',
                                                           description = "The Number of People in the Population (with and without HIV)",
                                                           scale = 'non.negative.number',
                                                           axis.name = 'Population',
                                                           units = 'people',
                                                           singular.unit = 'person')
                corresponding.data.outcome = 'population'
            }
            else
                error.prefix = paste0("Error setting tracking for ", self$descriptor, ": 'name' must be either 'infected', 'uninfected', or 'population'")
        
            super$initialize(name = name,
                             version = version,
                             sub.versions = NULL,
                             outcome.metadata = outcome.metadata,
                             denominator.outcome = NULL,
                             value.is.numerator = NULL,
                             scale = 'non.negative.number',
                             corresponding.data.outcome = corresponding.data.outcome,
                             keep.dimensions = NULL,
                             exclude.dimensions = NULL,
                             subset.dimension.values = NULL,
                             rename.dimension.values = NULL,
                             save = save,
                             from.year = -Inf,
                             to.year = Inf,
                             required.scale = 'non.negative.number')    
        }
    ),
    
    active = list(
        
        descriptor = function(value)
        {
            if (missing(value))
                'intrinsic outcome'
            else
                stop(paste0("Cannot modify a model outcome's 'descriptor' - it is read-only"))
        },
        
        is.cumulative = function(value)
        {
            if (missing(value))
                F
            else
                stop(paste0("Cannot modify a model outcome's 'is.cumulative' - it is read-only"))
        },
        
        is.intrinsic = function(value)
        {
            if (missing(value))
                T
            else
                stop(paste0("Cannot modify a model outcome's 'is.intrinsic' - it is read-only"))
        },
        
        groups = function(value)
        {
            if (missing(value))
                private$i.groups
            else
                stop(paste0("Cannot modify a model outcome's 'groups' - they are read-only"))
        }
    ),
    
    private = list(
        
        i.groups = NULL,
        i.ontology.name = NULL,
        
        derive.max.possible.dim.names = function(specification, all.outcomes,  
                                                 include.denominator.outcome,
                                                 error.prefix)
        {
            specification$ontologies[[private$i.ontology.name]]
        }
    )
)

DYNAMIC.MODEL.OUTCOME = R6::R6Class(
    'dynamic.model.outcome',
    inherit = MODEL.OUTCOME,
    portable = F,
    
    public = list(
        
        initialize = function(name,
                              version,
                              sub.versions,
                              outcome.metadata,
                              dynamic.quantity.name,
                              scale,
                              value.is.numerator,
                              denominator.outcome,
                              
                              multiply.by,
                              corresponding.data.outcome,
                              include.tags,
                              exclude.tags,
                              groups,
                              
                              keep.dimensions,
                              exclude.dimensions,
                              subset.dimension.values,
                              rename.dimension.values,
                              dimension.aliases,
                              dimension.alias.suffix,
                              save,
                              
                              from.year,
                              to.year)
        {
            super$initialize(name = name,
                             version = version,
                             sub.versions = sub.versions,
                             outcome.metadata = outcome.metadata,
                             denominator.outcome = denominator.outcome,
                             value.is.numerator = T,
                             scale = scale,
                             corresponding.data.outcome = corresponding.data.outcome,
                             keep.dimensions = keep.dimensions,
                             exclude.dimensions = exclude.dimensions,
                             subset.dimension.values = subset.dimension.values,
                             rename.dimension.values = rename.dimension.values,
                             dimension.aliases = dimension.aliases,
                             dimension.alias.suffix = dimension.alias.suffix,
                             save = save,
                             from.year = from.year,
                             to.year = to.year)
            
            
            error.prefix = paste0("Error setting tracking for ", self$descriptor, " '", name, "': ")
            
            
            # Validate dynamic.quantity.name
            if (!is.character(dynamic.quantity.name) || length(dynamic.quantity.name) != 1 || is.na(dynamic.quantity.name))
                stop(paste0(error.prefix, "'dynamic.quantity.name' must be a single, non-NA character value"))
            
            if (type=='transition.model.outcome')
                allowed.dynamic.quantity.names = 'transition'
            else
                allowed.dynamic.quantity.names = setdiff(names(DYNAMIC.TRACKER.SCHEMATA), 'transition')
            
            if (all(dynamic.quantity.name != allowed.dynamic.quantity.names))
                stop(paste0(error.prefix, "'", dynamic.quantity.name, "' is not a valid value for dynamic.quantity.name - it must be one of ",
                            collapse.with.or("'", names(allowed.dynamic.quantity.names), "'")))
            
            
            # Validate multiply.by
            if (length(multiply.by)==0)
                multiply.by = NULL
            else if (!is.character(multiply.by) || length(multiply.by) != 1 || is.na(multiply.by))
                stop(paste0(error.prefix, "If it is not NULL, 'multiply.by' must be a single, non-NA character value"))
            
            # Validate tags
            if (!is.null(include.tags))
            {
                if (!is.character(include.tags) || length(include.tags)==0 || any(is.na(include.tags)))
                    stop(paste0(error.prefix, "If it is not NULL, 'include.tags' must be a non-empty character vector with no NA values"))
            }
            
            if (!is.null(exclude.tags))
            {
                if (!is.character(exclude.tags) || length(exclude.tags)==0 || any(is.na(exclude.tags)))
                    stop(paste0(error.prefix, "If it is not NULL, 'exclude.tags' must be a non-empty character vector with no NA values"))
            }
            
            included.and.excluded.tags = intersect(include.tags, exclude.tags)
            if (length(included.and.excluded.tags))
                stop(paste0(error.prefix, "'include.tags' and 'exclude.tags' cannot share values, but ",
                            collapse.with.and("'", included.and.excluded.tags, "'"),
                            ifelse(length(included.and.excluded.tags)==1, " is", " are"),
                            " present in both"))
            
            # Validate groups
            if (is.null(groups))
            {
                if (dynamic.quantity.name=='population')
                    stop(paste0(error.prefix, "For tracking a dynamic 'population' outcome, 'groups' must be specified (not NULL)"))
            }
            else
            {
                if (!is.character(groups) || length(groups)==0 || any(is.na(groups)))
                    stop(paste0(error.prefix, "If it is not NULL, 'groups' must be a non-empty character vector with no NA values"))
                
                invalid.groups = setdiff(groups, ALLOWED.GROUPS)
                if (length(invalid.groups)>0)
                    stop(paste0(error.prefix,
                                "Invalid ",
                                ifelse(length(invalid.groups)==1, "group ", "groups "),
                                collapse.with.and("'", invalid.groups, "'"),
                                " - groups must be one of ",
                                collapse.with.or("'", ALLOWED.GROUPS, "'")))
            }
            
            
            # Store Variables
            if (dynamic.quantity.name=='population')
                private$i.dynamic.quantity.name = 'population'
            else
                private$i.dynamic.quantity.name = DYNAMIC.TRACKER.SCHEMATA[[dynamic.quantity.name]]$type
            
            private$i.multiply.by = multiply.by
            private$i.include.tags = unique(include.tags)
            private$i.exclude.tags = unique(exclude.tags)
            private$i.groups = groups
        },
        
        get.inherent.dim.names = function(specification)
        {
            matching.comps = self$get.matching.core.components(specification)
            
            rv = NULL
            for (comp in matching.comps)
            {
                one.ont = comp$schema$get.ontology.for.trackable(trackable.type = self$trackable.type,
                                                                 comp = comp,
                                                                 ontologies = specification$ontologies)
                
                if (is.null(rv))
                    rv = one.ont
                else
                    union.shared.dim.names(rv,)
            }
            
            rv
        },
        
        get.matching.core.components = function(specification)
        {
            matching.core.components.mask = sapply(specification$core.components, function(comp){
                comp$schema$dynamic.tracker.involves.component(tracker=self, comp=comp)
            })
            
            specification$core.components[matching.core.components.mask]
        },
        
        rename.depends.on.quantities = function(mapping)
        {
            if (any(names(mapping)==private$i.multiply.by))
                private$i.multiply.by = mapping[private$i.multiply.by]
        }
    ),
    
    active = list(
        
        descriptor = function(value)
        {
            if (missing(value))
                'dynamic outcome'
            else
                stop(paste0("Cannot modify a model outcome's 'descriptor' - it is read-only"))
        },
        
        depends.on.cumulative = function(value)
        {
            if (missing(value))
            {
                if (is.null(private$i.denominator.outcome))
                    character()
                else
                    private$i.denominator.outcome
            }
            else
                stop(paste0("Cannot modify a model outcome's 'is.cumulative' - it is read-only"))
        },
        
        depends.on.quantities = function(value)
        {
            if (missing(value))
            {
                if (is.null(private$i.multiply.by))
                    character()
                else
                    private$i.multiply.by
            }
            else
                stop(paste0("Cannot modify a model outcome's 'depends.on.quantities' - it is read-only"))
        },
        
        is.cumulative = function(value)
        {
            if (missing(value))
                T
            else
                stop(paste0("Cannot modify a model outcome's 'is.cumulative' - it is read-only"))
        },
        
        dynamic.quantity.name = function(value)
        {
            if (missing(value))
                private$i.dynamic.quantity.name
            else
                stop(paste0("Cannot modify a model outcome's 'dynamic.quantity.name' - it is read-only"))
        },
        
        multiply.by = function(value)
        {
            if (missing(value))
                private$i.multiply.by
            else
                stop(paste0("Cannot modify a model outcome's 'multiply.by' - it is read-only"))
        },
        
        include.tags = function(value)
        {
            if (missing(value))
                private$i.include.tags
            else
                stop(paste0("Cannot modify a model outcome's 'include.tags' - they are read-only"))
        },
        
        exclude.tags = function(value)
        {
            if (missing(value))
                private$i.exclude.tags
            else
                stop(paste0("Cannot modify a model outcome's 'exclude.tags' - they are read-only"))
        },
        
        groups = function(value)
        {
            if (missing(value))
                private$i.groups
            else
                stop(paste0("Cannot modify a model outcome's 'groups' - they are read-only"))
        },
        
        trackable.type = function(value)
        {
            if (missing(value))
                private$i.dynamic.quantity.name
            else
                stop(paste0("Cannot modify a model outcome's 'trackable.type' - it is read-only"))
        },
        
        is.dynamic = function(value)
        {
            if (missing(value))
                T
            else
                stop(paste0("Cannot modify a model outcome's 'is.dynamic' - it is read-only"))
        }
    ),
    
    private = list(
        
        i.dynamic.quantity.name = NULL,
        i.multiply.by = NULL,
        i.include.tags = NULL,
        i.exclude.tags = NULL,
        i.groups = NULL,
        
        derive.max.possible.dim.names = function(specification, all.outcomes,  
                                                 include.denominator.outcome,
                                                 error.prefix)
        {
            # identify the relevant core components
            relevant.components = specification$core.components[sapply(specification$core.components, function(comp){
                comp$schema$dynamic.tracker.involves.component(tracker=self, comp=comp)
            })]
            
            if (length(relevant.components)==0)
            {
                if (private$i.dynamic.quantity.name=='population')
                {
                    dim.names = NULL
                    for (group in private$i.groups)
                    {
                        dim.names = union.shared.dim.names(dim.names, specification$ontologies[[group]])
                    }
                    
                    dim.names
                }
                else
                {
                    if (is(self, 'transition.model.outcome'))
                        stop(paste0(error.prefix,
                                    "Empty model outcome '", 
                                    self$get.original.name(wrt.version=specification$version),
                                    "' - no transitions from <",
                                    paste0(private$i.from.compartments, collapse=', '),
                                    "> to <", 
                                    paste0(private$i.to.compartments, collapse=', '),
                                    ">",
                                    ifelse(length(private$i.include.tags)==0, '', 
                                           paste0(", for include.tag(s) '", collapse.with.or("'", private$i.include.tags, "'"), ", ")),
                                    " have been registered to the '", specification$version, "' specification"))
                    else
                        stop(paste0(error.prefix,
                                    "Empty model outcome '", 
                                    self$get.original.name(wrt.version=specification$version),
                                    "' - no '", private$i.dynamic.quantity.name, "' core components ",
                                    ifelse(length(private$i.include.tags)==0, '', 
                                           paste0(" for include.tag(s) '", collapse.with.or("'", private$i.include.tags, "'"), ", ")),
                                    " have registered to the '",  specification$version, "' specification"))
                }
            }
            else
            {
                dim.names = NULL
                for (comp in relevant.components)
                {
                    
                    ont = comp$schema$get.ontology.for.trackable(trackable.type = self$trackable.type,
                                                                 comp = comp,
                                                                 ontologies = specification$ontologies)
                    
                    dim.names = union.shared.dim.names(dim.names, ont)
                }
                
                dim.names
            }
        }
    )
)

TRANSITION.MODEL.OUTCOME = R6::R6Class(
    'transition.model.outcome',
    inherit = DYNAMIC.MODEL.OUTCOME,
    portable = F,
    
    public = list(
        
        initialize = function(name,
                              version,
                              sub.versions,
                              outcome.metadata,
                              scale,
                              value.is.numerator,
                              denominator.outcome,

                              dimension,
                              from.compartments,
                              to.compartments,
                                                            
                              multiply.by,
                              corresponding.data.outcome,
                              include.tags,
                              exclude.tags,
                              groups,
                              
                              keep.dimensions,
                              exclude.dimensions,
                              subset.dimension.values,
                              rename.dimension.values,
                              dimension.aliases,
                              dimension.alias.suffix,
                              save,
                              
                              from.year,
                              to.year)
        {
            super$initialize(name = name,
                             version = version,
                             sub.versions = sub.versions,
                             outcome.metadata = outcome.metadata,
                             scale = scale,
                             value.is.numerator = value.is.numerator,
                             denominator.outcome = denominator.outcome,
                             dynamic.quantity.name = 'transition',
                             multiply.by = multiply.by,
                             corresponding.data.outcome = corresponding.data.outcome,
                             include.tags = include.tags,
                             exclude.tags = exclude.tags,
                             groups = groups,
                             keep.dimensions = keep.dimensions,
                             exclude.dimensions = exclude.dimensions,
                             subset.dimension.values = subset.dimension.values,
                             rename.dimension.values = rename.dimension.values,
                             dimension.aliases = dimension.aliases,
                             dimension.alias.suffix = dimension.alias.suffix,
                             from.year = from.year,
                             to.year = to.year,
                             save = save)
            
            error.prefix = paste0("Error setting tracking for ", self$descriptor, " '", name, "': ")
            
            # Validate dimension
            if (!is.character(dimension) || length(dimension)!=1 || is.na(dimension))
                stop(paste0(error.prefix, "'dimension' must be a single, non-NA character value"))
                
            # Validate from.compartments, to.compartments
            if (!is.character(from.compartments) || length(from.compartments)==0 || any(is.na(from.compartments)))
                stop(paste0(error.prefix, "'from.compartments' must be a non-empty character vector with no NA values"))
            
            if (!is.character(to.compartments) || length(to.compartments)==0 || any(is.na(to.compartments)))
                stop(paste0(error.prefix, "'to.compartments' must be a non-empty character vector with no NA values"))
            
            # Store variables
            private$i.dimension = dimension
            private$i.from.compartments = from.compartments
            private$i.to.compartments = to.compartments
        },
        
        resolve.compartment.aliases = function(aliases)
        {
            super$resolve.compartment.aliases(aliases)
            
            private$i.from.compartments = substitute.aliases.into.vector(private$i.from.compartments, aliases)
            private$i.to.compartments = substitute.aliases.into.vector(private$i.to.compartments, aliases)
        }
    ),
    
    active = list(
        
        descriptor = function(value)
        {
            if (missing(value))
                'transition'
            else
                stop(paste0("Cannot modify a model outcome's 'descriptor' - it is read-only"))
        },
        
        dimension = function(value)
        {
            if (missing(value))
                private$i.dimension
            else
                stop(paste0("Cannot modify a model outcome's 'dimension' - it is read-only"))
        },
        
        from.compartments = function(value)
        {
            if (missing(value))
                private$i.from.compartments
            else
                stop(paste0("Cannot modify a model outcome's 'from.compartments' - they are read-only"))
        },
        
        to.compartments = function(value)
        {
            if (missing(value))
                private$i.to.compartments
            else
                stop(paste0("Cannot modify a model outcome's 'to.compartments' - they are read-only"))
        }
    ),
    
    private = list(
        
        i.dimension = NULL,
        i.from.compartments = NULL,
        i.to.compartments = NULL
    )
)

INTEGRATED.MODEL.OUTCOME = R6::R6Class(
    'integrated.model.outcome',
    inherit = MODEL.OUTCOME,
    portable = F,
    
    public = list(
        
        initialize = function(name,
                              version,
                              sub.versions,
                              outcome.metadata,
                              
                              value.to.integrate,
                              value.is.numerator,
                              multiply.by,
                              denominator.outcome,
                              
                              corresponding.data.outcome,
                              keep.dimensions,
                              exclude.dimensions,
                              subset.dimension.values,
                              rename.dimension.values,
                              dimension.aliases,
                              dimension.alias.suffix,
                              scale,
                              save,
                              
                              from.year,
                              to.year)
        {
            super$initialize(name,
                             version = version,
                             sub.versions = sub.versions,
                             outcome.metadata = outcome.metadata,
                             value.is.numerator = value.is.numerator,
                             denominator.outcome = denominator.outcome,
                             scale = scale,
                             corresponding.data.outcome = corresponding.data.outcome,
                             keep.dimensions = keep.dimensions,
                             exclude.dimensions = exclude.dimensions,
                             subset.dimension.values = subset.dimension.values,
                             rename.dimension.values = rename.dimension.values,
                             dimension.aliases = dimension.aliases,
                             dimension.alias.suffix = dimension.alias.suffix,
                             save = save,
                             from.year = from.year,
                             to.year = to.year)
            
            error.prefix = paste0("Error setting tracking for ", self$descriptor, " '", name, "': ")
            
            # Validate value.to.integrate
            if (!is.character(value.to.integrate) || length(value.to.integrate) != 1 || is.na(value.to.integrate))
                stop(paste0(error.prefix, "'value.to.integrate' must be a single, non-NA character value"))
            
            
            # Validate multiply.by
            if (length(multiply.by)==0)
                multiply.by = NULL
            else if (!is.character(multiply.by) || length(multiply.by) != 1 || is.na(multiply.by))
                stop(paste0(error.prefix, "If it is not NULL, 'multiply.by' must be a single, non-NA character value"))
            
            # Store Variables
            private$i.value.to.integrate = value.to.integrate
            private$i.multiply.by = multiply.by
        },
        
        rename.depends.on.quantities = function(mapping)
        {
            if (any(names(mapping)==private$i.multiply.by))
                private$i.multiply.by = mapping[private$i.multiply.by]
        },
        
        rename.depends.on.outcomes = function(mapping)
        {
            if (any(names(mapping)==private$i.value.to.integratee))
                private$i.value.to.integrate = mapping[private$i.value.to.integrate]
        },
        
        get.calculate.values.function = function(parent.environment,
                                                 error.prefix)
        {
            env = new.env(parent = parent.environment)
            env$value.to.integrate = private$i.value.to.integrate
            env$multiply.by = private$i.multiply.by
            
            fn = function(desired.times,
                          bindings,
                          binding.times,
                          cumulative.interval = 1,
                          error.prefix)
            {
                # Process Times
                first.desired.time = desired.times[1]
                last.desired.time = desired.times[length(desired.times)]
                
                # Protect ourselves against interpolating beyond the endpoints
                if (first.desired.time < binding.times[1])
                {
                    binding.times = c(first.desired.time, binding.times)
                    for (i in 1:length(bindings))
                        bindings[[i]] = c(bindings[[i]][1], bindings[[i]])
                }
                
                if ((last.desired.time+cumulative.interval) > binding.times[length(binding.times)])
                {
                    binding.times = c(binding.times, last.desired.time+cumulative.interval)
                    for (i in 1:length(bindings))
                        bindings[[i]] = c(bindings[[i]], bindings[[i]][length(bindings[[i]])])
                }

                # Set up
                n.binding.times = length(binding.times)
                mask = binding.times[-1] > (first.desired.time + 1 - cumulative.interval) & 
                    binding.times[-n.binding.times] < (last.desired.time + 1)
    #                binding.times[-1] != binding.times[-n.binding.times] - can't have this condition - we will ignore step changes if we do
                
                n.intervals = sum(mask)
                
                t0.all = binding.times[-n.binding.times][mask]
                t1.all = binding.times[-1][mask]
                
                # Process integrand into slopes
                integrand = bindings[[value.to.integrate]]
                n.integrand = length(integrand)
                
                if (n.integrand>1)
                {
                    v0.all = integrand[-n.binding.times][mask]
                    v1.all = integrand[-1][mask]
                    
                    # indexed [time, stratum]
                    m.all = t(sapply(1:n.intervals, function(i){
                        (v1.all[[i]] - v0.all[[i]]) / (t1.all[i] - t0.all[i])
                    }))
                    dim(m.all) = c(n.intervals, length(m.all)/n.intervals)
                    
                    b.all = t(sapply(1:n.intervals, function(i){
                        v0.all[[i]]
                    }))
                    dim(b.all) = c(n.intervals, length(b.all)/n.intervals)
                }
                
                if (is.null(multiply.by))
                    n.multiplier = 0
                else
                {
                    multiplier = bindings[[multiply.by]]
                    n.multiplier = length(multiplier)
                    
                    if (n.multiplier>1)
                    {
                        mult0.all = multiplier[-n.binding.times][mask]
                        mult1.all = multiplier[-1][mask]
                        
                        # indexed [time, stratum]
                        m2.all = t(sapply(1:n.intervals, function(i){
                            (mult1.all[[i]] - mult0.all[[i]]) / (t1.all[i] - t0.all[i])
                        }))
                        dim(m2.all) = c(n.intervals, length(m2.all)/n.intervals)
                        
                        b2.all = t(sapply(1:n.intervals, function(i){
                            mult0.all[[i]]
                        }))
                        dim(b2.all) = c(n.intervals, length(b2.all)/n.intervals)
                    }
                }
                
                if (n.multiplier==0 && n.integrand==1)
                {
                    val = integrand[[1]] * cumulative.interval
                    rv = lapply(desired.times, function(time){val})
                }
                else if (n.multiplier==1 && n.integrand==1)
                {
                    val = integrand[[1]] * multplier[[1]] * cumulative.interval
                    rv = lapply(desired.times, function(time){val})
                }
                else if (n.multiplier<=1)
                {
                    if (n.multiplier==0)
                        mult = 1
                    else
                        mult = multiplier[[1]]
                    
                    rv = lapply(desired.times, function(time){
                        
                        last.integrate.to.time = time + 1
                        first.integrate.from.time = last.integrate.to.time - cumulative.interval
                        
                        sub.mask = t0.all < last.integrate.to.time & t1.all > first.integrate.from.time
                        
                        b = b.all[sub.mask,,drop=F]
                        m = m.all[sub.mask,,drop=F]
                        
                        t0 = pmax(first.integrate.from.time, t0.all[sub.mask]) - t0.all[sub.mask]
                        t1 = pmin(t1.all[sub.mask], last.integrate.to.time) - t0.all[sub.mask]
                        
                        mult * colSums(m/2*t1^2 + b*t1 - m/2*t0^2 - b*t0)
                    })
                }
                else if (n.integrand==1)
                {
                    rv = lapply(desired.times, function(time){
                        
                        last.integrate.to.time = time + 1
                        first.integrate.from.time = last.integrate.to.time - cumulative.interval
                        
                        sub.mask = t0.all < last.integrate.to.time & t1.all > first.integrate.from.time
                        
                        b2 = b2.all[sub.mask,,drop=F]
                        m2 = m2.all[sub.mask,,drop=F]
                        
                        t0 = pmax(first.integrate.from.time, t0.all[sub.mask]) - t0.all[sub.mask]
                        t1 = pmin(t1.all[sub.mask], last.integrate.to.time) - t0.all[sub.mask]
                        
                        integrand[[1]] * colSums(m2/2*t1^2 + b2*t1 - m2/2*t0^2 - b2*t0)
                    })
                }
                else # both n.multiplier and n.integrand are >1
                {
                    rv = lapply(desired.times, function(time){
                        
                        last.integrate.to.time = time + 1
                        first.integrate.from.time = last.integrate.to.time - cumulative.interval
                        
                        sub.mask = t0.all < last.integrate.to.time & t1.all > first.integrate.from.time
                        
                        b = b.all[sub.mask,,drop=F]
                        m = m.all[sub.mask,,drop=F]
                        
                        b2 = b2.all[sub.mask,,drop=F]
                        m2 = m2.all[sub.mask,,drop=F]
                        
                        t0 = pmax(first.integrate.from.time, t0.all[sub.mask]) - t0.all[sub.mask]
                        t1 = pmin(t1.all[sub.mask], last.integrate.to.time) - t0.all[sub.mask]
                        
                        colSums(m*m2/3*t1^3 + (b*m2+b2*m)/2*t1^2 + b*b2*t1 -
                                    m*m2/3*t0^3 - (b*m2+b2*m)/2*t0^2 - b*b2*t0)
                    })
                }
                
                rv
            }
            environment(fn) = env
            fn
        }
    ),
    
    active = list(
        
        descriptor = function(value)
        {
            if (missing(value))
                'integrated outcome'
            else
                stop(paste0("Cannot modify a model outcome's 'descriptor' - it is read-only"))
        },
        
        depends.on.cumulative = function(value)
        {
            if (missing(value))
            {
                if (is.null(private$i.denominator.outcome))
                    character()
                else
                    private$i.denominator.outcome
            }
            else
                stop(paste0("Cannot modify a model outcome's 'is.cumulative' - it is read-only"))
        },
        
        depends.on.quantities.or.outcomes = function(value)
        {
            if (missing(value))
                private$i.value.to.integrate
            else
                stop(paste0("Cannot modify a model outcome's 'depends.on.quantities.or.outcomes' - it is read-only"))
        },
        
        depends.on.quantities = function(value)
        {
            if (missing(value))
            {
                if (is.null(private$i.multiply.by))
                    character()
                else
                    private$i.multiply.by
            }
            else
                stop(paste0("Cannot modify a model outcome's 'depends.on.quantities' - it is read-only"))
        },
        
        is.cumulative = function(value)
        {
            if (missing(value))
                T
            else
                stop(paste0("Cannot modify a model outcome's 'is.cumulative' - it is read-only"))
        },
        
        value.to.integrate = function(value)
        {
            if (missing(value))
                private$i.value.to.integrate
            else
                stop(paste0("Cannot modify a model outcome's 'value.to.integrate' - it is read-only"))
        },
        
        multiply.by = function(value)
        {
            if (missing(value))
                private$i.multiply.by
            else
                stop(paste0("Cannot modify a model outcome's 'multiply.by' - it is read-only"))
        }
    ),
    
    private = list(
        
        i.value.to.integrate = NULL,
        i.multiply.by = NULL
    )
)

# An 'abstract' (never instantiated class) that allows the main value to be 
#   an expression that *combines* quantities and outcomes
COMBINED.MODEL.OUTCOME = R6::R6Class(
    'combined.model.outcome',
    inherit = MODEL.OUTCOME,
    portable = F,
    
    public = list(
        
        initialize = function(name,
                              version,
                              sub.versions,
                              outcome.metadata,
                              value,
                              value.is.numerator,
                              denominator.outcome,
                              corresponding.data.outcome,
                              keep.dimensions,
                              exclude.dimensions,
                              subset.dimension.values,
                              rename.dimension.values,
                              dimension.aliases,
                              dimension.alias.suffix,
                              scale,
                              save,
                              from.year,
                              to.year,
                              required.scale=NULL)
        {
            super$initialize(name,
                             version = version,
                             sub.versions = sub.versions,
                             outcome.metadata = outcome.metadata,
                             value.is.numerator = value.is.numerator,
                             denominator.outcome = denominator.outcome,
                             scale = scale,
                             corresponding.data.outcome = corresponding.data.outcome,
                             keep.dimensions = keep.dimensions,
                             exclude.dimensions = exclude.dimensions,
                             subset.dimension.values = subset.dimension.values,
                             rename.dimension.values = rename.dimension.values,
                             dimension.aliases = dimension.aliases,
                             dimension.alias.suffix = dimension.alias.suffix,
                             save = save,
                             from.year = from.year,
                             to.year = to.year,
                             required.scale = required.scale)
            
            error.prefix = paste0("Error setting tracking for ", self$descriptor, " '", name, "': ")
            
            # Validate value
            evaluatable.value = EVALUATABLE.VALUE$new(na.replacement = as.numeric(NA),
                                                      allow.numeric.value = F,
                                                      allow.character.value = T,
                                                      allow.expression.value = T,
                                                      allow.function.value = F,
                                                      allowed.expression.functions = ALLOWED.MODEL.OUTCOME.VALUE.EXPRESSION.FUNCTIONS,
                                                      error.prefix = error.prefix)
            
            evaluatable.value$set.value(value, 
                                        value.name = 'value',
                                        error.prefix = error.prefix)

            # Store Variables
            private$i.value = evaluatable.value
        },
        
        rename.depends.on.outcomes = function(mapping)
        {
            if (any(names(mapping)==private$i.denominator.outcome))
                private$i.denominator.outcome = mapping[private$i.denominator.outcome]
            
            private$i.value$rename.depends.on(mapping)
        },
        
        rename.depends.on.quantities = function(mapping)
        {
            private$i.value$rename.depends.on(mapping)
        },
        
        compile = function(specification, error.prefix)
        {
            rv = super$compile(specification, error.prefix)
            rv$compile.value()
            rv
        },
        
        compile.value = function()
        {
            private$i.value = private$i.value$clone(deep=T)
        },
        
        get.calculate.values.function = function(parent.environment,
                                                 error.prefix)
        {
            env = new.env(parent = parent.environment)
            env$evaluate.function = private$i.value$get.evaluate.function(parent.environment = env,
                                                                          error.prefix = error.prefix)
            env$outcome.name = private$i.outcome$name
            
            fn = function(desired.times,
                                    bindings,
                                    binding.times,
                                    cumulative.interval = 1,
                                    error.prefix)
            {
                rv = lapply(1:length(desired.times), function(i){
                    bindings.for.time = lapply(bindings, function(b){
                        if (length(b)==1)
                            b[[1]]
                        else
                            b[[i]]
                    })
                    names(bindings.for.time) = names(bindings)
                    
                    evaluate.function(bindings = bindings.for.time,
                                      error.prefix = paste0(error.prefix, 
                                                            "Error calculating value for time ", 
                                                            desired.times[i], " for outcome '",
                                                            outcome.name, "' - "))
                })
                
                names(rv) = as.character(desired.times)
                rv
            }
            
            environment(fn) = env
            fn
        }
    ),
    
    active = list(
        
        depends.on.quantities.or.outcomes = function(value)
        {
            if (missing(value))
                private$i.value$depends.on
            else
                stop(paste0("Cannot modify a model outcome's 'depends.on.quantities.or.outcomes' - it is read-only"))
        }
    ),
    
    private = list(
        
        i.value = NULL
    )
)

CUMULATIVE.MODEL.OUTCOME = R6::R6Class(
    'cumulative.model.outcome',
    inherit = COMBINED.MODEL.OUTCOME,
    portable = F,
    
    active = list(
        
        descriptor = function(value)
        {
            if (missing(value))
                'cumulative outcome'
            else
                stop(paste0("Cannot modify a model outcome's 'descriptor' - it is read-only"))
        },
        
        is.cumulative = function(value)
        {
            if (missing(value))
                T
            else
                stop(paste0("Cannot modify a model outcome's 'is.cumulative' - it is read-only"))
        }
    )
    
)

POINT.MODEL.OUTCOME = R6::R6Class(
    'point.model.outcome',
    inherit = COMBINED.MODEL.OUTCOME,
    portable = F,
    
    active = list(
        
        descriptor = function(value)
        {
            if (missing(value))
                'point outcome'
            else
                stop(paste0("Cannot modify a model outcome's 'descriptor' - it is read-only"))
        },
        
        is.cumulative = function(value)
        {
            if (missing(value))
                F
            else
                stop(paste0("Cannot modify a model outcome's 'is.cumulative' - it is read-only"))
        }
    )
)


RATE.TO.PROPORTION.MODEL.OUTCOME = R6::R6Class(
    'rate.to.proportion.model.outcome',
    inherit = COMBINED.MODEL.OUTCOME,
    portable = F,
    
    public = list(
        
        initialize = function(specification,
                              name,
                              version,
                              sub.versions,
                              outcome.metadata,
                              rate.value,
                              
                              denominator.outcome,
                              corresponding.data.outcome,
                              calculate.proportion.leaving = T,
                              keep.dimensions = NULL,
                              exclude.dimensions = NULL,
                              subset.dimension.values = NULL,
                              rename.dimension.values = NULL,
                              dimension.aliases = NULL,
                              dimension.alias.suffix = NULL,
                              save = T,
                              from.year,
                              to.year)
        {
            # Validate calculate.proportion.leaving and infer the required scale
            if (!is.logical(calculate.proportion.leaving) || length(calculate.proportion.leaving) !=1 || is.na(calculate.proportion.leaving))
                stop(paste0("Error setting tracking for ", self$descriptor, ": 'calculate.proportion.leaving' must be a single, non-NA logical value"))
            
            if (calculate.proportion.leaving)
                required.scale = c('proportion','proportion.leaving')
            else
                required.scale = 'proportion.staying'
            
            # Call the super-constructor
            super$initialize(name = name,
                             version = version,
                             sub.versions = sub.versions,
                             outcome.metadata = outcome.metadata,
                             value = rate.value,
                             value.is.numerator = F,
                             scale = required.scale[1],
                             denominator.outcome = denominator.outcome,
                             corresponding.data.outcome = corresponding.data.outcome,
                             keep.dimensions = keep.dimensions,
                             exclude.dimensions = exclude.dimensions,
                             subset.dimension.values = subset.dimension.values,
                             rename.dimension.values = rename.dimension.values,
                             dimension.aliases = dimension.aliases,
                             dimension.alias.suffix = dimension.alias.suffix,
                             save = save,
                             from.year = from.year,
                             to.year = to.year,
                             required.scale = required.scale)
            
            # Store variables
            private$i.calculate.proportion.leaving = calculate.proportion.leaving
        },
        
        compile = function(specification, error.prefix)
        {
            if (private$i.value$value.type=='character')
            {
                quantity = specification$get.quantity(private$i.value$depends.on)
                if (!is.null(quantity$scale) && quantity$scale != 'rate')
                {
                    stop(paste0(error.prefix, "Outcome ",
                                self$get.original.name(specification$version),
                                ", a rate-to-proportion outcome, needs to integrate a *rate*, but the scale for quantity ",
                                quantity$get.original.name(specification$version),
                                " is *", quantity$scale, "*"))
                }
            }
            
            super$compile(specification, error.prefix)
        },
        
        get.calculate.values.function = function(parent.environment,
                                                 error.prefix)
        {
            env = new.env(parent = parent.environment)
            env$super.calculate.values.function = super$get.calculate.values.function(parent.environment = parent.environment,
                                                                                      error.prefix = error.prefix)
            env$calculate.proportion.leaving = private$i.calculate.proportion.leaving
            
            fn = function(desired.times,
                          bindings,
                          binding.times,
                          cumulative.interval = 1,
                          error.prefix)
            {
                rates = super.calculate.values.function(desired.times = binding.times,
                                                        bindings = bindings,
                                                        binding.times = binding.times,
                                                        cumulative.interval = cumulative.interval,
                                                        error.prefix = error.prefix)
                
                n.binding.times = length(binding.times)
                
                if (n.binding.times==1)
                {
                    r = rates[[1]]
                    
                    if (calculate.proportion.leaving)
                        val = 1 - exp(-r * t)
                    else
                        val = exp(-r * t)
                    
                    rv = lapply(desired.times, function(time){val})
                }
                else
                {
                    first.desired.time = desired.times[1]
                    last.desired.time = desired.times[length(desired.times)]
                    
                    mask = binding.times[-1] > (first.desired.time + 1 - cumulative.interval) & 
                        binding.times[-n.binding.times] < (last.desired.time + 1)
    
                    n.intervals = sum(mask)
                    r0.all = rates[-n.binding.times][mask]
                    r1.all = rates[-1][mask]
                    
                    t0.all = binding.times[-n.binding.times][mask]
                    t1.all = binding.times[-1][mask]
                    
                    # indexed [time, stratum]
                    m.all = t(sapply(1:n.intervals, function(i){
                        (r1.all[[i]] - r0.all[[i]]) / (t1.all[i] - t0.all[i])
                    }))
                    dim(m.all) = c(n.intervals, length(m.all)/n.intervals)
                    
                    b.all = t(sapply(1:n.intervals, function(i){
                        r0.all[[i]]
                    }))
                    dim(b.all) = c(n.intervals, length(b.all)/n.intervals)
                    
                    rv = lapply(desired.times, function(time){
                        
                        last.integrate.to.time = time + 1
                        first.integrate.from.time = last.integrate.to.time - cumulative.interval
                        
                        sub.mask = t0.all < last.integrate.to.time & t1.all > first.integrate.from.time
                        
                        b = b.all[sub.mask,,drop=F]
                        m = m.all[sub.mask,,drop=F]
                        
                        t0 = pmax(first.integrate.from.time, t0.all[sub.mask]) - t0.all[sub.mask]
                        t1 = pmin(t1.all[sub.mask], last.integrate.to.time) - t0.all[sub.mask]
                        
                        # The solution to the diffeq:
                        #   dPt / dt = -(mt + b) * Pt
                        #   is
                        #   Pt = P0 * exp(-(mt)^2/2 + bt))
                        p.remaining.by.interval = exp(-(m*t1^2/2 + b*t1)) / exp(-(m*t0^2/2 + b*t0))
                        
                        # This was the prior code - a mistake
                      #  p.remaining.by.interval = (m * t1 * exp(-m*t1^2/2) + exp(-b*t1)) /
                      #      (m * t0 * exp(-m*t0^2/2) + exp(-b*t0))
                        
                        p.remaining = apply(p.remaining.by.interval, 2, prod)
                        
                        
                        if (calculate.proportion.leaving)
                            1 - p.remaining
                        else
                            p.remaining
                    })
                }
                
                names(rv) = as.character(desired.times)
                rv
            }
            
            environment(fn) = env
            fn
        }
    ),
    
    active = list(
        
        descriptor = function(value)
        {
            if (missing(value))
                'rate-to-cumulative-proportion outcome'
            else
                stop(paste0("Cannot modify a model outcome's 'descriptor' - it is read-only"))
        },
        
        is.cumulative = function(value)
        {
            if (missing(value))
                T
            else
                stop(paste0("Cannot modify a model outcome's 'is.cumulative' - it is read-only"))
        },
        
        depends.on.cumulative = function(value)
        {
            if (missing(value))
            {
                private$i.denominator.outcome
            }
            else
                stop(paste0("Cannot modify a model outcome's 'is.cumulative' - it is read-only"))
        }
    ),
    
    private = list(
        
        i.calculate.proportion.leaving = NULL
    )
)


##-------------------------------------##
##-------------------------------------##
##-- DYNAMIC TRACKER CLASS HIERARCHY --##
##-------------------------------------##
##-------------------------------------##

DYNAMIC.TRACKER.SCHEMA = R6::R6Class(
    'dynamic.tracker.schema',
    
    public = list(
        
        initialize = function(type,
                              aliases=character(),
                              group.applies)
        {
            if (!is.character(type) || length(type)==0 || is.na(type))
                stop("'type' must be a single, non-NA character value")
            
            if (!is.character(aliases) || any(is.na(aliases)))
                stop("'aliases' must be a character vector with no NA values")
            
            if (!is.logical(group.applies) || length(group.applies)!=1 || is.na(group.applies))
                stop("'group.applies' must be a single, non-NA logical value")
            
            private$i.type = type
            private$i.aliases = aliases
            private$i.group.applies = group.applies
        }
    ),
    
    active = list(
        
        type = function(value)
        {
            if (missing(value))
                private$i.type
            else
                stop(paste0("Cannot modify a dynamic-tracker-schema's 'type' - it is read-only"))
        },
        
        aliases = function(value)
        {
            if (missing(value))
                private$i.aliases
            else
                stop(paste0("Cannot modify a dynamic-tracker-schema's 'aliases' - they are read-only"))
        },
        
        group.applies = function(value)
        {
            if (missing(value))
                private$i.group.applies
            else
                stop(paste0("Cannot modify a dynamic-tracker-schema's 'group.applies' - it is read-only"))
        }
    ),
    
    private = list(
        i.type = NULL,
        i.aliases = NULL,
        i.group.applies = NULL
    )
)

DYNAMIC.TRACKER.SCHEMATA = list(
    DYNAMIC.TRACKER.SCHEMA$new('mortality',
                               group.applies=T),
    DYNAMIC.TRACKER.SCHEMA$new('births.from',
                               group.applies=T),
    DYNAMIC.TRACKER.SCHEMA$new('births.to',
                               aliases='births',
                               group.applies=T),
    DYNAMIC.TRACKER.SCHEMA$new('incidence.to',
                               aliases='incidence',
                               group.applies=F),
    DYNAMIC.TRACKER.SCHEMA$new('incidence.by',
                               group.applies=F),
    DYNAMIC.TRACKER.SCHEMA$new('incidence.from',
                               group.applies=F),
    DYNAMIC.TRACKER.SCHEMA$new('transition',
                               group.applies=T),
    DYNAMIC.TRACKER.SCHEMA$new('remission.from',
                               group.applies=F),
    DYNAMIC.TRACKER.SCHEMA$new('remission.to',
                               aliases='remission',
                               group.applies=F),
    DYNAMIC.TRACKER.SCHEMA$new('population',
                               group.applies=T)
)
names(DYNAMIC.TRACKER.SCHEMATA) = sapply(DYNAMIC.TRACKER.SCHEMATA, function(sch){sch$type})

for (sch in DYNAMIC.TRACKER.SCHEMATA)
{
    for (alias in sch$aliases)
    {
        to.add = list(sch)
        names(to.add) = alias
        DYNAMIC.TRACKER.SCHEMATA = c(DYNAMIC.TRACKER.SCHEMATA, to.add)
    }
}


missing.tracker.schema.types = setdiff(unique(unlist(lapply(CORE.COMPONENT.SCHEMATA, function(sch){sch$trackable.types}))),
                                       names(DYNAMIC.TRACKER.SCHEMATA))
if (length(missing.tracker.schema.types)>0)
    stop(paste0("We are missing dynamic tracker schemata for ",
                paste0("'", missing.tracker.schema.types, "'", collapse=', ')))


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
    'source',
    'all'
)

RESERVED.DIMENSION.POSTFIXES = c(
    '.from',
    '.to'
)

RESERVED.COMPARTMENT.VALUES = character()

RESERVED.COMPARTMENT.VALUE.ALIASES = c(
    'all.ages',
    'all.ages.but.last'
)

# Quantity and element names cannot EQUAL these
RESERVED.QUANTITY.NAMES = c(
    "infected",
    "uninfected",
    "incidence",
    "births",
    "infected.mortality",
    "infected.specific.mortality",
    "uninfected.mortality",
    "population"
)

# Quantity and element names cannot BEGIN with these
RESERVED.QUANTITY.NAME.PREFIXES = c(
    'super',
    'this',
    'location',
    'specification',
    'outcome',
    'quant',
    'quantity' # this is redundant (already covered by) 'quant' above - but we also use this, so want to
                # document here in case we remove 'quant' in the future
)

RESERVED.OUTCOME.NAMES = c(
#    'infected',  <- instead of reserving these now, we are going to create outcome objects for them right up front, before anything else can be registered in their name
#    'uninfected',
#    'population'
)

RESERVED.OUTCOME.NAME.PREFIXES = c(
    RESERVED.QUANTITY.NAME.PREFIXES
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
                                "Cannot use '", reserved.in.use, "' as ",
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

validate.outcome.name <- function(name, 
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
                                reserved.values = RESERVED.OUTCOME.NAMES,
                                reserved.prefixes = RESERVED.OUTCOME.NAME.PREFIXES,
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
                         arguments.to.be.supplied.later = c('specification.metadata','location'),
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

    invalid.dimensions = setdiff(names(dimension.values), names(ontology))
    if (length(invalid.dimensions)>0)
        stop(paste0(error.prefix,
                    "Cannot resolve dimension values for ",
                    variable.name.for.error, " for ",
                    ifelse(length(invalid.dimensions)==1, "dimension ", "dimensions "),
                    collapse.with.and("'", invalid.dimensions, "'"), 
                    " - ",
                    ifelse(length(invalid.dimensions)==1, "it is", "they are"),
                    " not present in ", ontology.name.for.error))
    
    rv = lapply(names(dimension.values), function(d){
        
        template = ontology[[d]]

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
    
    if (is.ontology(dimension.values))
        rv = as.ontology(rv, incomplete.dimensions = incomplete.dimensions(dimension.values))
    
    rv
}

apply.aliases <- function(apply.to, aliases)
{
    if (is.character(apply.to))
        substitute.aliases.into.vector(values=apply.to, aliases=aliases)
    else if (is.ontology(apply.to))
    {
        rv = apply.aliases(as.list(apply.to), aliases=aliases)
        as.ontology(rv, incomplete.dimensions = incomplete.dimensions(apply.to))
    }
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
    if (is.null(quantity$original.name))
        original.name = quantity$get.original.name(wrt.version)
    else
        original.name = quantity$original.name
    
    if (is.null(quantity$max.dim.names))
    {}
    else if (length(quantity$max.dim.names)==0)
    {
        if (length(dim.names)!=0)
            stop(paste0(error.prefix,
                        ifelse(component.index==1, '', paste0("The ", get.ordinal(component.index-1), " subset of ")),
                        original.name,
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
                        original.name,
                        ", which expects dimensions ",
                        collapse.with.and("'", names(quantity$max.dim.names), "'")))
        
        mismatched.dimensions = sapply(names(dim.names), function(d){
            !setequal(dim.names[[d]], quantity$max.dim.names[[d]])
        })
        
        if (any(mismatched.dimensions))
            stop(paste0(error.prefix,
                        variable.name.for.error, " does not match expected dimnames for ",
                        ifelse(component.index==1, '', paste0("the ", get.ordinal(component.index-1), " subset of ")),
                        "model quantity ",
                        original.name, " for ",
                        ifelse(sum(mismatched.dimensions)==1, "dimension ", "dimensions "), ":\n",
                        paste0(sapply(names(dim.names)[mismatched.dimensions], function(d){
                            paste0("- Dimension '", d, "' expects values ", 
                                   collapse.with.and("'", quantity$max.dim.names[[d]], "'"),
                                   " but ", variable.name.for.error, " has values ",
                                   collapse.with.and("'", dim.names[[d]], "'"))
                        }), collapse='\n') ))
    }
}