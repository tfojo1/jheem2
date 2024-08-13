# new test

data.manager = create.data.manager('test', 'test data manager')

data.manager$register.outcome(
    'prevalence_diagnosed',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = "Prevalence (Diagnosed)",
        axis.name = "People with Diagnosed HIV (n)",
        units = 'people',
        description = "Estimated Number of People with HIV aware of their Diagnosis"))

data.manager$register.source('cdc', full.name = "US Centers for Disease Control and Prevention", short.name='CDC')

data.manager$register.ontology(
    'CDC_bho',
    ont=ontology(
        #location=NULL,
        year=NULL,
        age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years','55+ years'),
        race=c('black','hispanic','other'),
        sex=c('male','female'),
        risk=c('msm','idu','msm_idu','heterosexual','other')))


DIMNAMES.FOR.DATA = list(
    year=c('2021', '2022')
)

data = array(1, dim=sapply(DIMNAMES.FOR.DATA, length), dimnames=DIMNAMES.FOR.DATA)

put.data(
    data.manager,
    data,
    'prevalence_diagnosed',
    source='cdc',
    ontology='CDC_bho',
    url='cdc.com',
    details='cdc I guess',
    dimension.values = list()
)


# THIS BREAKS
x = pull.data(
    data.manager,
    outcome='prevalence_diagnosed',
    dimension.values=list(year='2023')
)
