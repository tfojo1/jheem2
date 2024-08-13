# Test for putting data of different metrics

dd = create.data.manager('test', 'test data manager')

dd$register.outcome(
    'prevalence_diagnosed',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = "Prevalence (Diagnosed)",
        axis.name = "People with Diagnosed HIV (n)",
        units = 'people',
        description = "Estimated Number of People with HIV aware of their Diagnosis"))

dd$register.parent.source('CDC', 'CDC', 'CDC')
dd$register.source('cdc', parent.source = 'CDC', full.name = "US Centers for Disease Control and Prevention", short.name='CDC')

dd$register.ontology(
    'CDC_bho',
    ont=ontology(
        year=NULL,
        age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years','55+ years'),
        race=c('black','hispanic','other'),
        sex=c('male','female'),
        risk=c('msm','idu','msm_idu','heterosexual','other')))


DIMNAMES.FOR.ESTIMATE.DATA = list(
    year=c('2021', '2022')
)

estimate.data = array(1, dim=sapply(DIMNAMES.FOR.ESTIMATE.DATA, length), dimnames=DIMNAMES.FOR.ESTIMATE.DATA)

dd$put(data = estimate.data, outcome = 'prevalence_diagnosed', metric = 'estimate', source = 'cdc', ontology.name = 'CDC_bho', dimension.values = list(), url = 'cdc.net', details = 'maybe cdc')

DIMNAMES.FOR.VAR.DATA = list(
    year=c('2008', '2021')
)
var.data = array(2, dim=sapply(DIMNAMES.FOR.VAR.DATA, length), dimnames=DIMNAMES.FOR.VAR.DATA)

dd$put(data = var.data, outcome = 'prevalence_diagnosed', metric = 'coefficient.of.variance', source = 'cdc', ontology.name = 'CDC_bho', dimension.values = list(), url = 'cdc.net', details = 'maybe cdc')

dd$pull(outcome='prevalence_diagnosed', keep.dimensions='year', append.attributes = 'url')
