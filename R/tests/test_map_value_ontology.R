
source('../jheem_analyses/source_code.R')

src.dim.names = list(
    sex = c('male','female'),
    race = c('black','white','hispanic','other')
)

target.dim.names = list(
    sex = c('heterosexual_male', 'msm', 'female'),
    race = c('black','hispanic','other')
)

src = array(1:prod(sapply(src.dim.names, length)), dim=sapply(src.dim.names, length), dimnames=src.dim.names)

map.value.ontology(src, target.dim.names)


get.mappings.to.align.ontologies(src.dim.names[1], target.dim.names[1])
get.mappings.to.align.ontologies(src.dim.names, target.dim.names)[[1]]$apply(src)
