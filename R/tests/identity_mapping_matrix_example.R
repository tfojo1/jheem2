mp = get.identity.ontology.mapping()
good.dimnames = list(x=c('x1', 'x2'), y=c('y1', 'y2'), z=c('z1', 'z2'))
mp$get.matrix(good.dimnames, good.dimnames)

bad.dimnames = list(x=c('x1', 'x2'), y=c('y1', 'y2'))
mp$get.matrix(bad.dimnames, bad.dimnames)
# "subscript out of bounds" error triggered in " rv[nonzero.indices] = 1 "
# for some reason probably related to R simplifying dimensions