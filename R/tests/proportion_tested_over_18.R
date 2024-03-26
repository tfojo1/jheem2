
#https://www.cdc.gov/hiv/library/reports/testing/2021/index.html#Table4
raw = c('0-12 years'=2910,'13-19 years'=88767, '20-29 years'=583150, '30-39 years'=466054)
x = restratify.age.counts(raw, desired.age.brackets = c('13-17 years','18-24 years'))
x[2]/sum(x)
y = restratify.age.counts(raw, desired.age.brackets = 1:30)

ggplot2::qplot(1:29, y, geom='point') + ggplot2::ylim(0,NA) + ggplot2::geom_vline(xintercept = c(13,18,24))
