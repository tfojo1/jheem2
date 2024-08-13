# 
# one.remove.mask
# dim.names.corresponding.to.one.remove.mask
# 
# years.that.we.kept
# 
# for (stratification in stratifications)
# {
#     # Remove years not in this stratification
#     years.not.present.in.this.stratification
#     indices.for.years.not.present = 
#         get.array.access.indices(arr.dim.names = dim.names.used.to.make.transfomration.matrix,
#                                  dimension.values = list(year=as.character(years.not.present.in.this.stratification)))
#     
#     transformation.matrix = transformation.matrix[-indices.for.years.not.present,]
#     
#     # Remove random bits of data
#     years.that.we.threw.out.for.stratification =
#         setdiff(dim.names.corresponding.to.one.remove.mask$year,
#                 as.character(years.that.we.kept))
#     
#     indices.to.omit.from.one.remove.mask =
#         get.array.access.indices(arr.dim.names = dim.names.corresponding.to.one.remove.mask,
#                                  dimension.values = list(year=as.character(years.that.we.threw.out.for.stratification)))
#  
#     new.one.remove.mask = one.remove.mask[-indices.to.omit.from.one.remove.mask]
#     
#     transformation.matrix = transformation.matrix[!new.one.remove.mask,]
# }

generate.transformation.matrix.nested = function(mappings.list, dimnames.list, locations.list, remove.mask.list, n.strats, matrix.dimnames) {
    transformation.matrix = NULL
    for (i in 1:n.strats) {
        one.mapping = mappings.list[[i]]
        one.dimnames = dimnames.list[[i]]
        one.locations = locations.list[[i]]
        one.remove.mask = remove.mask.list[[i]]
        
        year.limited.dimnames = one.dimnames
        year.limited.dimnames$year = matrix.dimnames$year
        year.limited.dimnames$location = matrix.dimnames$location
        one.source.transformation.matrix = one.mapping$get.matrix(from.dim.names = matrix.dimnames,
                                                                  to.dim.names = year.limited.dimnames[names(year.limited.dimnames) != 'source'])
        
        # Remove rows for years and locations not in this stratification
        years.in.matrix.but.not.stratification = setdiff(matrix.dimnames$year, one.dimnames$year)
        locations.in.matrix.but.not.stratification = setdiff(matrix.dimnames$location, one.dimnames$location)
        indices.for.years.not.present = get.array.access.indices(year.limited.dimnames[names(year.limited.dimnames) != 'source'],
                                                                 list(year=years.in.matrix.but.not.stratification))
        indices.for.locations.not.present = get.array.access.indices(year.limited.dimnames[names(year.limited.dimnames) != 'source'],
                                                                     list(location=locations.in.matrix.but.not.stratification))
        indices.for.years.or.locations.not.present = union(indices.for.years.not.present, indices.for.locations.not.present)
        if (length(indices.for.years.or.locations.not.present) > 0)
            one.source.transformation.matrix = one.source.transformation.matrix[-indices.for.years.or.locations.not.present,]
        
        # Repeat the matrix for each source this stratification has
        one.transformation.matrix = NULL
        for (source in 1:length(one.dimnames$source)) one.transformation.matrix = rbind(one.transformation.matrix, one.source.transformation.matrix)
        
        # Align the matrix rows with the one.remove.mask rows, which may have extra years, so that rows for sporadically missing data can be masked out
        years.in.stratification.but.not.matrix = setdiff(one.dimnames$year, matrix.dimnames$year)
        if (length(years.in.stratification.but.not.matrix) > 0) {
            indices.to.omit.from.one.remove.mask = get.array.access.indices(one.dimnames, list(year=years.in.stratification.but.not.matrix))
            new.one.remove.mask = one.remove.mask[-indices.to.omit.from.one.remove.mask]
            one.transformation.matrix = one.transformation.matrix[!new.one.remove.mask,]
        }
        
        transformation.matrix = rbind(transformation.matrix, one.transformation.matrix)
    }
    transformation.matrix
}

for (s in seq_along(mappings.list)) {
    one.mapping = mappings.list[[s]]
    if (is.null(one.mapping)) next
    one.dimnames = dimnames.list[[s]]
    one.locations = locations.list[[s]]
    to.locations = union(one.locations, private$i.sim.required.dimnames$location)
    one.locations.mask = to.locations %in% one.locations
    to.dimnames = one.dimnames[names(one.dimnames) != 'source']
    to.dimnames$location = to.locations
    
    ### NEED LOCATION IN SIM.REQUIRED.DIMNAMES HERE BUT NOT LATER
    one.transformation.matrix = one.mapping$get.matrix(from.dim.names = private$i.sim.required.dimnames, to.dim.names = to.dimnames)
    
    for (source in 1:length(one.dimnames[['source']])) {
        one.source.transformation.matrix = matrix(one.transformation.matrix[rep(one.locations.mask, each=length(years))],
                                                  ncol = ncol(one.transformation.matrix)) # will this repeat to fill as many strata as we need?
        private$i.transformation.matrix = rbind(private$i.transformation.matrix, one.source.transformation.matrix)
    }
}

private$i.transformation.matrix = private$i.transformation.matrix[!remove.mask,]


generate.transformation.matrix = function(mappings.list, dimnames.list, remove.mask.list, n.strats, matrix.dimnames) {
    
    transformation.matrix = NULL
    for (i in 1:n.strats) {
        
        one.mapping = mappings.list[[i]]
        one.dimnames = dimnames.list[[i]]
        one.remove.mask = remove.mask.list[[i]]
        
        year.limited.dimnames = one.dimnames
        year.limited.dimnames$year = matrix.dimnames$year
        one.source.transformation.matrix = one.mapping$get.matrix(from.dim.names = matrix.dimnames,
                                                                  to.dim.names = year.limited.dimnames[names(year.limited.dimnames) != 'source'])
        
        # Remove rows for years not in this stratification
        years.in.matrix.but.not.stratification = setdiff(matrix.dimnames$year, one.dimnames$year)
        if (length(years.in.matrix.but.not.stratification) > 0) {
            indices.for.years.not.present = get.array.access.indices(year.limited.dimnames[names(year.limited.dimnames) != 'source'],
                                                                     list(year=years.in.matrix.but.not.stratification))
            one.source.transformation.matrix = one.source.transformation.matrix[-indices.for.years.not.present,]
        }
        
        # Repeat the matrix for each source this stratification has
        one.transformation.matrix = NULL
        for (source in 1:length(one.dimnames$source)) one.transformation.matrix = rbind(one.transformation.matrix, one.source.transformation.matrix)
        
        # Align the matrix rows with the one.remove.mask rows, which may have extra years, so that rows for sporadically missing data can be masked out
        years.in.stratification.but.not.matrix = setdiff(one.dimnames$year, matrix.dimnames$year)
        if (length(years.in.stratification.but.not.matrix) > 0) {
            indices.to.omit.from.one.remove.mask = get.array.access.indices(one.dimnames, list(year=years.in.stratification.but.not.matrix))
            new.one.remove.mask = one.remove.mask[-indices.to.omit.from.one.remove.mask]
            one.transformation.matrix = one.transformation.matrix[!new.one.remove.mask,]
        }
        
        transformation.matrix = rbind(transformation.matrix, one.transformation.matrix)
    }
    transformation.matrix
}

test.mappings.list = list(get.identity.ontology.mapping())
test.dimnames.list = list(list(year=c('2017', '2018'), age=c('young', 'old'), color=c('blue', 'green'), source=c('friend', 'expert')))
test.remove.mask.list = list(array(c(T,F,T,F,T,F,T,T,T,F,T,F,T,T,T,T), sapply(test.dimnames.list[[1]], length), test.dimnames.list[[1]]))
test.n.strats = 1
test.matrix.dimnames = list(year=c('2018', '2019'), age=c('young', 'old'), color=c('blue', 'green'))
# browser()
generate.transformation.matrix(test.mappings.list, test.dimnames.list, test.remove.mask.list, test.n.strats, test.matrix.dimnames)
