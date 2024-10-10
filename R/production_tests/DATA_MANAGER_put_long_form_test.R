# Put Long Form Test

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

cdc.bho.ontology = ontology(
    location=NULL,
    year=NULL,
    age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years','55+ years'),
    race=c('black','hispanic','other'),
    sex=c('male','female'),
    risk=c('msm','idu','msm_idu','heterosexual','other'))
data.manager$register.ontology(
    'CDC_bho',
    ont=cdc.bho.ontology)


# Fake data
fake.data.ont = cdc.bho.ontology[c('location', 'year', 'race')]
fake.data.ont[['location']] = c("C.12580", "C.12060")
fake.data.ont[['year']] = as.character(2010:2011)
fake.array = array(1:prod(sapply(fake.data.ont, length)), sapply(fake.data.ont, length), fake.data.ont)

fake.dataset = reshape2::melt(fake.array, as.is=T)
fake.dataset$outcome = 'new'

data.manager$put.long.form(fake.dataset,
                           source = 'cdc',
                           ontology.name = 'CDC_bho',
                           url = 'www.fake.com',
                           details = 'Fake data')

# Test the Dimension Values to Distribute
fake.data.ont = ontology(location=fake.data.ont$location,
                         year=fake.data.ont$year,
                         race=c(fake.data.ont$race, 'unknown'),
                         sex=c(cdc.bho.ontology$sex, 'unknown', 'prefer not to say'),
                         incomplete.dimensions=c('year', 'location'))
fake.array = array(1:prod(sapply(fake.data.ont, length)), sapply(fake.data.ont, length), fake.data.ont)

fake.dataset = reshape2::melt(fake.array, as.is=T)
fake.dataset$outcome = 'new'

# data.manager$put.long.form(fake.dataset,
#                            source = 'cdc',
#                            ontology.name = 'CDC_bho',
#                            url = 'www.fake.com',
#                            details = 'Fake data',
#                            dimension.values.to.distribute= list(sex=c('unknown', 'prefer not to say'), race='unknown', job='none'))

fake.data.ont.less = as.list(fake.data.ont)
fake.data.ont.less$race = c('black', 'other', 'unknown')
fake.array = array(1:prod(sapply(fake.data.ont.less, length)), sapply(fake.data.ont.less, length), fake.data.ont.less)

fake.dataset = reshape2::melt(fake.array, as.is=T)
fake.dataset$outcome = 'new'
# data.manager$put.long.form(fake.dataset,
#                            source = 'cdc',
#                            ontology.name = 'CDC_bho',
#                            url = 'www.fake.com',
#                            details = 'Fake data',
#                            dimension.values.to.distribute= list(sex=c('unknown', 'prefer not to say'), race='unknown', job='none'))

# data.manager$put.long.form(fake.dataset,
#                            source = 'cdc',
#                            ontology.name = 'CDC_bho',
#                            url = 'www.fake.com',
#                            details = 'Fake data',
#                            dimension.values.to.distribute= list(year='2010'))

# data.manager$put.long.form(fake.dataset,
#                            source = 'cdc',
#                            ontology.name = 'CDC_bho',
#                            url = 'www.fake.com',
#                            details = 'Fake data',
#                            dimension.values.to.distribute= list(race=c('forgot', 'unknown')))
