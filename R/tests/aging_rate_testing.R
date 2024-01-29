
specification.metadata = get.specification.metadata('ehe', 'C.12580')

us.full.age.rates = do.get.empiric.hiv.aging.rates(location = "US",
                                                   specification.metadata = specification.metadata)

us.five.age.rates = do.get.empiric.hiv.aging.rates(location = "US",
                                                   specification.metadata = specification.metadata,
                                                   force.match.age.brackets.before.smoothing=T)

md.five.age.rates = do.get.empiric.hiv.aging.rates(location = "MD",
                                                   specification.metadata = specification.metadata)

baltimore.per.census.rates = do.get.empiric.aging.rates(location = 'C.12580',
                                                        specification.metadata = specification.metadata)
baltimore.five.age.rates = do.get.empiric.aging.rates(location = 'C.12580',
                                                      specification.metadata = specification.metadata,
                                                      force.match.age.brackets.to.before.smoothing = specification.metadata$dim.names$age)
baltimore.multi.age.rates = do.get.empiric.aging.rates(location = 'C.12580',
                                                      specification.metadata = specification.metadata,
                                                      force.match.age.brackets.to.before.smoothing = SURVEILLANCE.MANAGER$ontologies$cdc.national$age)

time = 1

df = reshape2::melt(us.full.age.rates[[time]], value.name = 'national_full_age')
df$national_five_age = as.numeric(us.five.age.rates[[time]])
df$md_five_age = as.numeric(md.five.age.rates[[time]])

ggplot(df, aes(x=national_full_age, y=national_five_age, color=age)) +
  geom_point() + geom_abline(intercept=0, slope=1) + xlim(0,.5) + ylim(0,.5) +
  ggtitle("US Full-Age vs US Five-Age")


ggplot(df, aes(x=national_full_age, y=md_five_age, color=age)) +
  geom_point() + geom_abline(intercept=0, slope=1) + xlim(0,.5) + ylim(0,.5) +
  ggtitle("US Full-Age vs MD Five-Age")

delta = (us.five.age.rates[[time]] - us.full.age.rates[[time]])
rel.delta = ((us.five.age.rates[[time]] - us.full.age.rates[[time]]) / us.full.age.rates[[time]])

qplot(as.numeric(rel.delta))
qplot(as.numeric(delta))

apply(abs(rel.delta), 'age', max)


time = 1
df2 = reshape2::melt(baltimore.per.census.rates[[time]], value.name = 'baltimore_per_census')
df2$baltimore_five_age = as.numeric(baltimore.five.age.rates[[time]])
df2$baltimore_multi_age = as.numeric(baltimore.multi.age.rates[[time]])

ggplot(df2, aes(x=baltimore_per_census, y=baltimore_five_age, color=age)) +
  geom_point() + geom_abline(intercept=0, slope=1) + xlim(0,.5) + ylim(0,.5) +
  ggtitle("Baltimore Full Census vs Baltimore Five-Age")

ggplot(df2, aes(x=baltimore_per_census, y=baltimore_multi_age, color=age)) +
  geom_point() + geom_abline(intercept=0, slope=1) + xlim(0,.5) + ylim(0,.5) +
  ggtitle("Baltimore Full Census vs Baltimore Multi-Age (12 brackets)")

delta = (baltimore.five.age.rates[[time]] - baltimore.per.census.rates[[time]])
rel.delta = ((baltimore.five.age.rates[[time]] - baltimore.per.census.rates[[time]]) / baltimore.per.census.rates[[time]])

qplot(as.numeric(rel.delta))
qplot(as.numeric(delta))

apply(abs(rel.delta), 'age', max)


delta2 = (baltimore.multi.age.rates[[time]] - baltimore.per.census.rates[[time]])
rel.delta2 = ((baltimore.multi.age.rates[[time]] - baltimore.per.census.rates[[time]]) / baltimore.per.census.rates[[time]])

qplot(as.numeric(rel.delta2))
qplot(as.numeric(delta2))

round(cbind('five-year'=apply(abs(rel.delta), 'age', mean),
            'multi-year'=apply(abs(rel.delta2), 'age', mean)),3)
round(cbind('five-year'=apply(abs(rel.delta), 'age', max),
            'multi-year'=apply(abs(rel.delta2), 'age', max)),3)

round(cbind('five-year'=apply(abs(delta), 'age', mean),
            'multi-year'=apply(abs(delta2), 'age', mean)),3)
round(cbind('five-year'=apply(abs(delta), 'age', max),
            'multi-year'=apply(abs(delta2), 'age', max)),3)

round(cbind('census (truth)' = apply(baltimore.per.census.rates[[time]], 'age', mean),
      'five-year'=apply(baltimore.five.age.rates[[time]], 'age', mean),
      'multi-year'=apply(baltimore.multi.age.rates[[time]], 'age', mean)),3)
