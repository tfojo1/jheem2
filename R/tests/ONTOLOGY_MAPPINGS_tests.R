
source('../jheem2/R/HELPERS_array_helpers.R')
source('../jheem2/R/HELPERS_dim_names_helpers.R')
source('../jheem2/R/ONTOLOGY_ontology_mappings.R')
source('../jheem2/R/ONTOLOGY_ontology.R')
Rcpp::sourceCpp("../jheem2/src/ontology_mappings.cpp")

dim.names = list(age=c('age1','age2','age3'), race=c('black','hispanic','other'))

test.numeric.arr = array(1, dim=sapply(dim.names, length), dimnames=dim.names)
test.sequential.arr = array(1:prod(sapply(dim.names, length)), dim=sapply(dim.names, length), dimnames=dim.names)
test.sequential.char.arr = array(as.character(1:prod(sapply(dim.names, length))), dim=sapply(dim.names, length), dimnames=dim.names)

map1 = register.ontology.mapping(
    name='test.race.mapping',
    mappings = rbind(
        c("black", "black"),
        c("hispanic", "other"),
        c("other", "other")
    ),
    
    from.dimensions = 'race',
    to.dimensions = 'race'
)

map2 = register.ontology.mapping(
    name = 'test.age.mapping',
    mappings = rbind(
        c('age1', 'young'),
        c('age2', 'young'),
        c('age3', 'old')
    ),
    
    from.dimensions = 'age',
    to.dimensions = 'age'
)

map3 = register.ontology.mapping(
    name = 'test.pre.age.mapping',
    mappings = rbind(
        c('age1','age1'),
        c('age2','age2'),
        c('age3a', 'age3'),
        c('age3b', 'age3')
    ),
    
    from.dimensions = 'age',
    to.dimensions = 'age'
)

map12 = combine.ontology.mappings(map1, map2)

all.successful = T

cat("AGGREGATE NUMERIC TEST...")
aggregated.test = map1$apply(test.numeric.arr)
success = all(array.access(aggregated.test, race='black')==1) && all(array.access(aggregated.test, race='other')==2)
if (success)
    cat("SUCCESS!\n")
if (!success)
    cat("******* FAILED *********\n")
all.successful = all.successful && success

cat("CONCATENATE CHAR TEST...")
concatenated.char = map1$apply(test.sequential.char.arr, fun=function(x){paste0(x, collapse=', ')})
sucess = all(array.access(concatenated.char, race='black') == array.access(test.sequential.char.arr, race='black')) &&
    all(array.access(concatenated.char, race='other') == paste0(array.access(test.sequential.char.arr, race='hispanic'), ', ', array.access(test.sequential.char.arr, race='other')))
if (success)
    cat("SUCCESS!\n")
if (!success)
    cat("******* FAILED *********\n")
all.successful = all.successful && success


cat("AGGREGATE COMBO MAPPING TEST...")
aggregated.combo = map12$apply(test.numeric.arr)
success = all(array.access(aggregated.combo, race='black', age='old')==1) &&
    all(array.access(aggregated.combo, race='black', age='young')==2) &&
    all(array.access(aggregated.combo, race='other', age='old')==2) &&
    all(array.access(aggregated.combo, race='other', age='young')==4)
if (success)
    cat("SUCCESS!\n")
if (!success)
    cat("******* FAILED *********\n")
all.successful = all.successful && success


cat("GET ONTOLOGY MAPPING TESTS...")
ont = ont1 = as.ontology(dim.names)
ont2 = map12$apply.to.ontology(ont)
found = get.ontology.mapping(from.ontology = ont, to.ontology = ont2)
success.a = !is.null(found)

ont3 = as.ontology(c(ont1['age'], ont2['race']))
ont4 = as.ontology(c(ont2['age'], ont1['race']))

found1 = get.ontology.mapping(from.ontology = ont3, to.ontology = ont4)
success.b = is.null(found1)
found2 = get.mappings.to.align.ontologies(ont3, ont4)
success.c = !is.null(found2)

success = success.a && success.b && success.c
if (success)
    cat("SUCCESS!\n")
if (!success)
    cat("******* FAILED *********\n")
all.successful = all.successful && success


cat("COMBINE SERIAL TEST...")
combo.dim.names = list(age=c('age1','age2','age3a','age3b'), race=c('black','hispanic','other'))
combo.test.arr = array(1, dim=sapply(combo.dim.names, length), dimnames=combo.dim.names)

map.age2x = combine.ontology.mappings(map3, map2)
map.age2x.plus.race = combine.ontology.mappings(map3, map2, map1)

test1 = map.age2x$apply(combo.test.arr)
success.a = all(test1==2)

test2 = map.age2x.plus.race$apply(combo.test.arr)
success.b = all(array.access(test2, race='black')==2) && all(array.access(test2, race='other')==4)

post.dim.names = map.age2x.plus.race$apply.to.dim.names(combo.dim.names)
mat = map.age2x.plus.race$get.matrix(combo.dim.names, post.dim.names)
indices = map.age2x.plus.race$get.mapping.indices(combo.dim.names, post.dim.names)
success.c = all(sapply(1:length(indices), function(i){
    mat.indices = (1:dim(mat)[2])[mat[i,]==1]
    setequal(mat.indices, indices[[i]])
}))


success = success.a && success.b && success.c
if (success)
    cat("SUCCESS!\n")
if (!success)
    cat("******* FAILED *********\n")
all.successful = all.successful && success

if (all.successful)
    cat("DONE - ALL TESTS WERE SUCCESSFUL")
if (!all.successful)
    cat("***** SOME TESTS FAILED *****")

if (1==2)
{
    # Speed Test
    N.ITER = 1000
    
    start.direct = Sys.time()
    for (i in 1:N.ITER)
        x = map1$apply(test.numeric.arr)
    end.direct = Sys.time()
    
    start.indirect = Sys.time()
    for (i in 1:N.ITER)
        x = map1$apply(test.numeric.arr, fun=function(x){sum(x)})
    end.indirect = Sys.time()
    
    start.ref = Sys.time()
    for (i in 1:N.ITER)
        x = cbind(test.numeric.arr[,1],
                  test.numeric.arr[,2]+test.numeric.arr[,3])
    end.ref = Sys.time()
    
    direct.vs.indirect = (as.numeric(end.direct)-as.numeric(start.direct)) / (as.numeric(end.indirect) - as.numeric(start.indirect))
    direct.vs.ref = (as.numeric(end.direct)-as.numeric(start.direct)) / (as.numeric(end.ref) - as.numeric(start.ref))
    print(paste0("direct / indirect: ", round(direct.vs.indirect,3)) )
    print(paste0("direct / ref: ", round(direct.vs.ref,3)) )
}