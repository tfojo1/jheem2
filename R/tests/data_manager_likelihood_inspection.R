my.data.manager <<- create.data.manager('my.data.manager', 'testing')
my.data.manager$register.outcome(
    'diagnoses',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'New Diagnoses',
        axis.name = 'New Diagnoses (n)',
        units = 'cases',
        description = "New HIV Cases Diagnosed in a Year"))
my.data.manager$register.source('cdc', full.name = "US Centers for Disease Control and Prevention", short.name='CDC')

my.data.manager$register.ontology(
    'cdc',
    ont = ontology(
        location=NULL,
        year=NULL,
        age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years','55+ years'),
        race=c('black','hispanic', 'white', 'AAPI', 'other'),
        sex=c('male','female'),
        risk=c('msm','idu','msm_idu','heterosexual','other'))
)

data = pull.data(data.manager, 'diagnoses', keep.dimensions = c('location', 'year', strat), dimension.values = list(location=c('AZ', 'MD')))
data = drop(data)

# where data is pulled from the surveillance manager
my.data.manager$put(data, 'diagnoses', dimension.values = list(location=c('AZ','MD')), ontology.name = 'cdc', source='cdc', url='cdc', details='cdc')

# Every time I change the data manager code, I'll have to run the test_2.R, pull the data, and make the new "my.data.manager".

# Now try this:
pull.data(
    my.data.manager,
    outcome='diagnoses',
    keep.dimensions = c('year', 'sex', 'risk'),
    dimension.values = list(location='MD'),
    target.ontology = private$i.sim.ontology[c('location', 'year', 'sex', 'risk')],
    allow.mapping.from.target.ontology = T,
    append.attributes = 'details')

# It works! Although there's only 2008 data because I made the sim.ontology only have that year.