
data.manager = create.data.manager('test', description='a data manager to test with')
data.manager$register.outcome(
    'new',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'New Diagnoses',
        axis.name = 'New Diagnoses (n)',
        units = 'cases',
        description = "New HIV Cases Diagnosed in a Year"))

data.manager$register.parent.source('cdc', 'cdc', 'cdc')
data.manager$register.source('cdc', 'cdc', full.name = "US Centers for Disease Control and Prevention", short.name='CDC')

blahntology = ontology(
    location=NULL,
    year=NULL,
    sex=c('male','female'))
data.manager$register.ontology(
    'blah',
    ont=blahntology
)

blah.data.dimnames = list(location='US',
                          year='2010-2011',
                          sex=c('male', 'female'))
blah.data = array(1:prod(sapply(blah.data.dimnames, length)), sapply(blah.data.dimnames, length), blah.data.dimnames)

data.manager$put(blah.data, outcome = 'new', source = 'cdc', ontology.name = 'blah', url = 'blah.com', details='Blah')

conflicting.data.dimnames = list(location='US',
                                 year='2010',
                                 sex=c('male', 'female'))
conflicting.data = array(1:prod(sapply(conflicting.data.dimnames, length)), sapply(conflicting.data.dimnames, length), conflicting.data.dimnames)
data.manager$put(conflicting.data, outcome = 'new', source = 'cdc', ontology.name = 'blah', url = 'blah.com', details='Blah')

data.manager$put(3, outcome='new', source='cdc', ontology.name='blah', dimension.values = list(location='US', year='2010'), url='blah.com', details='Blah')
