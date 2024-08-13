

p.bias = list(in.mean=0, out.mean=0, in.sd=0.03, out.sd=0.03)

# specify a source when there is more than one source for a single pull, even if they don't happen to have data for the same strata. This occurs here for MSA-level diagnosed.prevalence.
#p.bias = get.p.bias.estimates(dimensions=c('sex', 'race'), levels.of.stratification = c(0,1), outcome.for.p = 'suppression', outcome.for.n='diagnosed.prevalence', sub.location.type='county', super.location.type='state', main.location.type = 'cbsa', main.location.type.n.source ='cdc.surveillance.reports')


ehe.partitioning.function = function(arr, version='ehe', location)
{
    if ("risk" %in% names(dim(arr)) &&
        all(array.access(arr, risk='active_IDU')==array.access(arr, risk='IDU_in_remission')))
    {
        risk.partition.dimnames = list(risk = c('active_IDU', 'IDU_in_remission'))
        risk.partition.arr = array(c(0.25, 0.75), dim=sapply(risk.partition.dimnames, length), risk.partition.dimnames)
        risk.modified = array.access(arr, risk.partition.dimnames)
        risk.modified = risk.modified * expand.array(risk.partition.arr, dimnames(risk.modified))
        array.access(arr, dimnames(risk.modified)) = risk.modified
    }
    if ("sex" %in% names(dim(arr)) &&
        all(array.access(arr, sex='msm')==array.access(arr, sex='heterosexual_male')))
    {
        #sex.partition.dimnames = list(sex = c('heterosexual_male', 'msm'))
        #sex.partition.arr = array(c(0.5, 0.5), dim=sapply(sex.partition.dimnames, length), sex.partition.dimnames)
        
        specification.metadata = get.specification.metadata(version=version, location=location)
        proportion.msm = get.best.guess.msm.proportions(location,
                                                        specification.metadata = specification.metadata,
                                                        keep.age = any(names(dim(arr))=='age'),
                                                        keep.race = any(names(dim(arr))=='race'))
        # sex.partition.arr = get.best.guess.msm.proportions.by.race(location,
        #                                                            specification.metadata = specification.metadata,
        #                                                            years = DEFAULT.POPULATION.YEARS,
        #                                                            min.age = specification.metadata$age.lower.bounds[1],
        #                                                            return.proportions = T)

        sex.partition.arr = c(as.numeric(proportion.msm), 1-as.numeric(proportion.msm))
        sex.partition.dimnames = c(dimnames(proportion.msm), list(sex=c('msm','heterosexual_male')))
        dim(sex.partition.arr) = sapply(sex.partition.dimnames, length)
        dimnames(sex.partition.arr) = sex.partition.dimnames
        
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
                                                   # denominator.outcome.for.sim = 'diagnosed.prevalence',
                                                   
                                                   location.types = c('COUNTY','STATE','CBSA'),
                                                   minimum.geographic.resolution.type = 'COUNTY',
                                                   
                                                   # dimensions = c("age","sex","race","risk"),
                                                   dimensions = c("sex"),
                                                   levels.of.stratification = c(0,1), 
                                                   from.year = as.integer(2008), 
                                                   
                                                   p.bias.inside.location = p.bias$in.mean,
                                                   p.bias.outside.location = p.bias$out.mean,
                                                   p.bias.sd.inside.location = p.bias$in.sd,
                                                   p.bias.sd.outside.location = p.bias$out.sd,
                                                   
                                                   within.location.p.error.correlation = 0.5,
                                                   within.location.n.error.correlation = 0.5,
                                                   
                                                   observation.correlation.form = 'compound.symmetry', 
                                                   measurement.error.sd = 0.03,
                                                   
                                                   partitioning.function = ehe.partitioning.function,
                                                   
                                                   weights = list(1), # upweight?
                                                   equalize.weight.by.year = T 
  )

supp.lik = suppression.likelihood.instructions$instantiate.likelihood('ehe', 'C.12580')
