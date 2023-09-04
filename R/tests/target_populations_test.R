
x = create.target.population(sex=c('heterosexual_male','msm'), age=1:3, risk='idu.states', name='test')

x$render.population.mask(specification.metadata)

y = create.target.population(sex=c('female','msm'), age=3:5, name='test2')

zu = union.target.populations(x, y)
zu$render.population.mask(specification.metadata)

zi = intersect.target.populations(x, y)
zi$render.population.mask(specification.metadata)

zd = diff.target.populations(x, y)
zd$render.population.mask(specification.metadata)

intersect.target.populations(zi, zd)
union.target.populations(zu, zi)

x$overlaps(y)
x$get.overlapping.dimension.values(y)

x$overlaps(zu)
x$overlaps(zi)
x$get.overlapping.dimension.values(zi)
x$get.overlapping.dimension.values(zu)
#x$get.overlapping.dimension.values(zd)


msm = create.target.population(sex='msm', name='female')
female = create.target.population(sex='female', name='female')
msm.and.female = union.target.populations(msm, female)
msm.and.female = create.target.population(sex=c('msm','female'), name='msm and female')
mf.minus.female = diff.target.populations(msm.and.female, female)

msm$overlaps(female)
mf.minus.female$overlaps(female)
mf.minus.female$overlaps(msm)

msm$overlaps(female)
