
# test code
if (1==2)
{
    source('code/source_code.R')
    source('code/core_code/ontologies/ontology_mappings.R')
    sourceCpp('code/core_code/ontologies/ontology_mappings.cpp')
    
    m = create.other.catchall.ontology.mapping('race', ALL.DATA.MANAGERS$census.collapsed$races, c('black','hispanic','other'))
    
    test = get.census.data(ALL.DATA.MANAGERS$census.collapsed, fips=counties.for.msa('12580'),
                           years=2010:2015)
    
    old.collapsed = do.collapse.races(test, c('black','hispanic','other'))
    new.collapsed = apply.ontology.mapping(mapping=m, arr=test)
    
    all(old.collapsed==new.collapsed)
    
    N.TEST = 1000
    
    old.start = Sys.time()
    for (i in 1:N.TEST)
        old.collapsed = do.collapse.races(test, c('black','hispanic','other'))
    old.end = Sys.time()
    
    new.start = Sys.time()
    for (i in 1:N.TEST)
        new.collapsed = apply.ontology.mapping(mapping=m, arr=test)
    new.end = Sys.time()
    
    print(paste0("new vs old: ",
                 round((as.numeric(new.end)-as.numeric(new.start))/
                           (as.numeric(old.end)-as.numeric(old.start)), 3)))
        
}

create.age.mapping <- function(from.ages, to.ages)
{
    
}

create.year.mapping <- function(from.ages, to.ages)
{
    
}

# Creates a mapping object that is a 'pass through' - changes nothing
create.trivial.ontology.mapping <- function()
{
    mapping = list(is.trivial=T)
    class(mapping) = 'ontology.mapping'
    
    mapping
}

create.cdc.sex.risk.ontology.mapping <- function(non.idu.states='never_IDU',
                                                 idu.states=c('active_IDU','IDU_in_remission'))
{
    rv = list(
        is.trivial = F,
        dimensions = c('sex', 'risk'),
        to.ontology = list(
            sex = c('female','male'),
            risk = c('msm','idu','msm_idu','heterosexual')),
        from = list(sex=character(), risk=character()),
        to = list(sex=character(), risk=character())
    )
    
    
    # MSM
    rv$from$sex = c(rv$from$sex, rep('msm', length(non.idu.states)))
    rv$from$risk = c(rv$from$risk, non.idu.states)
    
    rv$to$sex = c(rv$to$sex, rep('male', length(non.idu.states)))
    rv$to$risk = c(rv$to$risk, rep('msm', length(non.idu.states)))
    
    
    # MSM-IDU
    rv$from$sex = c(rv$from$sex, rep('msm', length(idu.states)))
    rv$from$risk = c(rv$from$risk, idu.states)
    
    rv$to$sex = c(rv$to$sex, rep('male', length(idu.states)))
    rv$to$risk = c(rv$to$risk, rep('msm_idu', length(idu.states)))
    
    
    # IDU Female
    rv$from$sex = c(rv$from$sex, rep('female', length(idu.states)))
    rv$from$risk = c(rv$from$risk, idu.states)
    
    rv$to$sex = c(rv$to$sex, rep('female', length(idu.states)))
    rv$to$risk = c(rv$to$risk, rep('idu', length(idu.states)))
    
    
    # IDU Male
    rv$from$sex = c(rv$from$sex, rep('heterosexual_male', length(idu.states)))
    rv$from$risk = c(rv$from$risk, idu.states)
    
    rv$to$sex = c(rv$to$sex, rep('male', length(idu.states)))
    rv$to$risk = c(rv$to$risk, rep('idu', length(idu.states)))
    
    
    # Het Female
    rv$from$sex = c(rv$from$sex, rep('female', length(non.idu.states)))
    rv$from$risk = c(rv$from$risk, non.idu.states)
    
    rv$to$sex = c(rv$to$sex, rep('female', length(non.idu.states)))
    rv$to$risk = c(rv$to$risk, rep('heterosexual', length(non.idu.states)))
    
    # Het Male
    rv$from$sex = c(rv$from$sex, rep('heterosexual_male', length(non.idu.states)))
    rv$from$risk = c(rv$from$risk, non.idu.states)
    
    rv$to$sex = c(rv$to$sex, rep('male', length(non.idu.states)))
    rv$to$risk = c(rv$to$risk, rep('heterosexual', length(non.idu.states)))
    
    
    # Return
    class(rv) = 'ontology.mapping'
    rv
}

create.other.catchall.ontology.mapping <- function(dimension,
                                                   from.values,
                                                   to.values,
                                                   throw.error.if.trivial=T)
{
    if (!is.character(dimension) || length(dimension)!=1 || is.na(dimension))
        stop("'dimension' must be a single, non-NA character value")
    
    if (!is.character(from.values) || length(from.values)==0 || any(is.na(from.values)))
        stop("'from.values' must be a non-empty, non-NA character vector")
    
    if (!is.character(to.values) || length(to.values)==0 || any(is.na(to.values)))
        stop("'to.values' must be a non-empty, non-NA character vector")
    
    if (all(to.values!='other'))
        stop("'to.values' must contain the value 'other'")
    
    missing.from.from = setdiff(to.values, c(from.values, 'other'))
    if (length(missing.from.from)>0)
        stop("The following is/are present in 'to.values' but not in 'from.values': ",
             paste0("'", missing.from.from, "'", collapse=', '))
    
    if (length(from.values)==length(to.values) &&
        all(from.values==to.values))
    {
        if (throw.error.if.trivial)
            stop("'from.values' are the same as 'to.values'. You don't need a mapping, but if you want one, set throw.error.if.trivial=F")
        mapping = list(is.trivial=T)
    }
    else
    {
        mapping = list(is.trivial=F,
                       dimensions=dimension,
                       to.ontology = list(to.values),
                       from = list(from.values),
                       to = list(sapply(from.values, function(from){
                           if (any(from==to.values))
                               from
                           else
                               'other'
                       }))
        )
    }    
    names(mapping$to) = names(mapping$from) = names(mapping$to.ontology) = dimension
    
    class(mapping) = 'ontology.mapping'
    mapping
}

#'@export
apply.ontology.mapping <- function(arr, mapping)
{
    if (mapping$is.trivial)
        arr
    
    src.dim.names = dimnames(arr)
    dst.dim.names = src.dim.names
    dst.dim.names[mapping$dimensions] = mapping$to.ontology[1]
    
    src.dims = dim(arr)
    dst.dims = sapply(dst.dim.names, length)
    
    rv = array(0, dim = dst.dims, dimnames = dst.dim.names)
    
    # set up the indices
    map.dimensions = sapply(mapping$dimensions, function(dim){
        (1:length(src.dim.names))[names(src.dim.names)==dim][1]
    })
    
    if (any(is.na(map.dimensions)))
        stop(paste0("No dimension(s) named ",
                    paste0("'", mapping$dimensions[is.na(map.dimensions)], "'", collapse=', '),
                    " in the given array"))
    
    from.indices = lapply(names(mapping$from), function(dim){
        index.of.dim.value(needles=mapping$from[[dim]], haystack=src.dim.names[[dim]])
    })
    
    to.indices = lapply(names(mapping$to), function(dim){
        index.of.dim.value(needles=mapping$to[[dim]], haystack=dst.dim.names[[dim]])
    })
    
    # Call the cpp sub-function
    if (length(mapping$dimensions)==1)
    {
        apply_one_dim_ontology_mapping(src = arr,
                                       dst = rv,
                                       src_dims = src.dims,
                                       dst_dims = dst.dims,
                                       map_dimension = map.dimensions[1],
                                       from_indices = from.indices[[1]],
                                       to_indices = to.indices[[1]])
    }
    else if (length(mappings$dimensions)==2)
    {
        apply_one_dim_ontology_mapping(src = arr,
                                       dst = rv,
                                       src_dims = src.dims,
                                       dst_dims = dst_dims,
                                       map_dimension_1 = map.dimensions[1],
                                       map_dimension_2 = map.dimensions[2],
                                       from_indices_1 = from.indices[[1]],
                                       to_indices_1 = to.indices[[1]],
                                       from_indices_2 = from.indices[[2]],
                                       to_indices_2 = to.indices[[2]])
    }
    else
        
        stop("At this point, we have only implemented ontology mappings for two-dimension combos")
    
    # return
    rv
}

index.of.dim.value <- function(needles,
                               haystack)
{
    sapply(needles, function(needle){
        mask = needle == haystack
            (1:length(haystack))[mask][1]
    })
}