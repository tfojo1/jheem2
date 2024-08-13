get.metalocations = function(location,
                             all.locations.with.data,
                             minimum.geographic.resolution.type)
{
    # make a list with each location's minimum components
    all.locations.with.data = c('r3')
    minimum.components.list = list('state1' = as.character(1:9),
                                   'state2' = as.character(10:18),
                                   'region1' = as.character(1:3),
                                   'county8' = '8',
                                   'county17' = '17',
                                   'msa' = c('7','8', '10', '14'))
    
    # minimum.components.list = lapply(all.locations.with.data, function(location) {
    #     locations::get.sub.locations(locations = location, sub.type = minimum.geographic.resolution.type, limit.to.completely.enclosing = T) # because county will be our minimum geographic resolution type and we will never chop a county into pieces
    # })
    # names(minimum.components.list) = all.locations.with.data
    # browser()

    minimum.components = unique(unlist(minimum.components.list))

    # make matrix with membership of each minimum component to an observation location (state, substate region, EMA, county, MSA)
    # find unique sets of columns; these are metalocations.
    # retain a mapping from metalocations to observed locations
    obs.to.minimal.component.map = t(matrix(unlist(lapply(seq_along(minimum.components.list),
                                                          function(obs) {minimum.components %in% minimum.components.list[[obs]]})),
                                            ncol =length(minimum.components.list),
                                            nrow = length(minimum.components)))
    
    # Is logical. Todd's version is 0 and 1.
    metalocation.cols = unique(obs.to.minimal.component.map, MARGIN=2)
    
    metalocation.to.minimal.component.map = apply(metalocation.cols, MARGIN=2, function(metalocation.col) {
        minimum.components[which(apply(obs.to.minimal.component.map, MARGIN=2, function(obs.minimal.col) {
            identical(obs.minimal.col, metalocation.col)
        }))]
    })
    
    # give dimnames after checking if columns are identical, because their names certainly won't be
    dimnames(metalocation.cols) = list(obs.location = names(minimum.components.list), metalocation.number = 1:ncol(metalocation.cols))
    
    metalocation.type = sapply(metalocation.to.minimal.component.map, function(metalocation) {
        components.in.msa = intersect(metalocation, minimum.components.list[['msa']])
        if (length(components.in.msa)==0) 'minimum-out-of-msa'
        else if (setequal(components.in.msa, minimum.components.list[['msa']])) 'msa'
        else 'minimum-in-msa'
    })
    
    # This output isn't meaningful unless someone knows what those constituent minimum components actually ARE. --> They do. 'minimum.components' is the mapping from index to county code.
    # 'metalocation.to.minimal.component.map' tells us which counties are in each metalocation.
    # We will also want to know the metalocation to observation mapping.
    # browser()
    # Do I add in something like metalocation.type? If I know that 'msa' is the first (here last) observation location, and I already know which counties it has, I can figure out whether a metalocation is the msa or not.
    output = list(metalocation.to.minimal.component.map = metalocation.to.minimal.component.map,
                  metalocation.to.obs.location.mapping = metalocation.cols,
                  metalocation.type = metalocation.type)
    browser()
}

a=get.metalocations(NULL, NULL, NULL)