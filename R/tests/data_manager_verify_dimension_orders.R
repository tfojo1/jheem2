# verify the order of dimensions in the data manager

ss = SURVEILLANCE.MANAGER

outcome.names = names(ss$data)
dimensions.correct = lapply(outcome.names, function(outcome.name) {
    source.names = names(ss$data[[outcome.name]]$estimate)
    outcome.result=lapply(source.names, function(source.name) {
        ont.names = names(ss$data[[outcome.name]]$estimate[[source.name]])
        source.result=lapply(ont.names, function(ont.name) {
            stratification.names = names(ss$data[[outcome.name]]$estimate[[source.name]][[ont.name]])
            ont.result=lapply(stratification.names, function(stratification.name) {
                stratification.dimensions = unlist(strsplit(stratification.name, split="__"))
                data.dimensions = names(dim(ss$data[[outcome.name]]$estimate[[source.name]][[ont.name]][[stratification.name]]))
                return(identical(stratification.dimensions, data.dimensions))
            })
            return(setNames(ont.result, stratification.names))
        })
        return(setNames(source.result, ont.names))
    })
    return(setNames(outcome.result, source.names))
})
names(dimensions.correct) = outcome.names

(any(!unlist(dimensions.correct)))
