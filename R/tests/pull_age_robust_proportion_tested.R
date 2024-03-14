

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
SURVEILLANCE.MANAGER$pull.age.robust(outcome='proportion.tested', target.ontology = target.ontology)
