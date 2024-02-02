
p.bias.inside = 0
p.bias.outside = 0
p.bias.sd.inside = 0.03
p.bias.sd.outside = 0.03



location='C.12580'
ehe.partitioning.function = function(arr, version='ehe')
{
    # For now, partition the different dimensions separately. Later, could have it do only one partition, a custom combined one, if you have risk and sex together
    dimensions.to.partition = intersect(c('risk', 'sex'), names(dim(arr)))
    # if (identical(dimensions.to.partition, c('risk')))
    if ("risk" %in% names(dim(arr))) {
        risk.partition.dimnames = list(risk = c('active_IDU', 'IDU_in_remission'))
        risk.partition.arr = array(c(0.25, 0.75), dim=sapply(risk.partition.dimnames, length), risk.partition.dimnames)
        risk.modified = array.access(arr, risk.partition.dimnames)
        risk.modified = risk.modified * expand.array(risk.partition.arr, dimnames(risk.modified))
        array.access(arr, dimnames(risk.modified)) = risk.modified
    }
    if ("sex" %in% names(dim(arr))) {
        #sex.partition.dimnames = list(sex = c('heterosexual_male', 'msm'))
        #sex.partition.arr = array(c(0.5, 0.5), dim=sapply(sex.partition.dimnames, length), sex.partition.dimnames)
        
        specification.metadata = get.specification.metadata(version=version, location=location)
        sex.partition.arr = get.best.guess.msm.proportions.by.race(location,
                                                                   specification.metadata = specification.metadata,
                                                                   years = DEFAULT.POPULATION.YEARS,
                                                                   min.age = specification.metadata$age.lower.bounds[1],
                                                                   return.proportions = T)
        sex.partition.dimnames = dimnames(sex.partition.arr)
        browser()
        sex.modified = array.access(arr, sex.partition.dimnames)
        sex.modified = sex.modified * expand.array(sex.partition.arr, dimnames(sex.modified))
        array.access(arr, dimnames(sex.modified)) = sex.modified
    }
    arr
    
}

suppression.likelihood.instructions = 
  create.nested.proportion.likelihood.instructions(outcome.for.data = "suppression",
                                                   outcome.for.sim = "suppression",
                                                   denominator.outcome.for.data = 'diagnosed.prevalence',
                                                   denominator.outcome.for.sim = 'diagnosed.prevalence',
                                                   
                                                   location.types = c('COUNTY','STATE','CBSA'),
                                                   minimum.geographic.resolution.type = 'COUNTY',
                                                   
                                                   # dimensions = c("age","sex","race","risk"),
                                                   dimensions = c("sex"),
                                                   levels.of.stratification = c(0,1), 
                                                   from.year = as.integer(2008), 
                                                   
                                                   p.bias.inside.location = p.bias.inside,
                                                   p.bias.outside.location = p.bias.outside,
                                                   p.bias.sd.inside.location = p.bias.sd.inside,
                                                   p.bias.sd.outside.location = p.bias.sd.outside,
                                                   
                                                   within.location.p.error.correlation = 0.5,
                                                   within.location.n.error.correlation = 0.5,
                                                   
                                                   observation.correlation.form = 'compound.symmetry', 
                                                   measurement.error.sd = 0.03,
                                                   
                                                   partitioning.function = ehe.partitioning.function,
                                                   
                                                   weights = list(1), # upweight?
                                                   equalize.weight.by.year = T 
  )

supp.lik = suppression.likelihood.instructions$instantiate.likelihood('ehe', 'C.12580')
