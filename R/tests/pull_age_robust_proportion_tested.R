

target.ontology = ontology(
  year = as.character(2008:2022),
  location = c('C.12580','MD'),
  age = c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years'),
  race = c('black','hispanic','other'),
  sex = c('heterosexual_male', 'msm', 'female'),
  risk = c('never_IDU','active_IDU','IDU_in_remission'),
  incomplete.dimensions = c('year','location')
)


SURVEILLANCE.MANAGER$pull(outcome='proportion.tested', target.ontology = target.ontology)
# SURVEILLANCE.MANAGER$pull.age.robust(outcome='proportion.tested', target.ontology = target.ontology)

# What I think we really want to test. Works until the "restratify.age.counts" step, throwing an error related to the spline etc.
SURVEILLANCE.MANAGER$pull.age.robust(outcome='proportion.tested',
                                     keep.dimensions=c('year', 'location', 'age'),
                                     target.ontology = target.ontology,
                                     restratify.age=T,
                                     desired.age.brackets = target.ontology[['age']],
                                     allow.extrapolation = T)