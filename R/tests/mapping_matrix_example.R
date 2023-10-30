# mapping matrices are not as I expect them

register.ontology.mapping('jheem.to.cdc.sex.risk',
                          from.dimensions = c('sex', 'risk'),
                          to.dimensions = c('sex', 'risk'),
                          mappings = rbind(c('msm', 'never_IDU', 'male', 'msm'),
                                           c('msm', 'active_IDU', 'male', 'msm_idu'),
                                           c('msm', 'IDU_in_remission', 'male', 'msm_idu'),
                                           c('heterosexual_male', 'never_IDU', 'male', 'heterosexual'),
                                           c('heterosexual_male', 'active_IDU', 'male', 'idu'),
                                           c('heterosexual_male', 'IDU_in_remission', 'male', 'idu'),
                                           c('female', 'never_IDU', 'female', 'heterosexual'),
                                           c('female', 'active_IDU', 'female', 'idu'),
                                           c('female', 'IDU_in_remission', 'female', 'idu'))
)

jheem.sex.risk = ontology(sex = c('heterosexual_male', 'msm', 'female'), risk = c('never_IDU', 'active_IDU', 'IDU_in_remission'))
cdc.sex.risk = ontology(sex = c('male', 'female'), risk = c('msm', 'msm_idu', 'heterosexual', 'idu'))
mp = get.ontology.mapping(jheem.sex.risk, cdc.sex.risk)

cdc.only.sex = list(sex = c('male', 'female'))

mat1 = mp$get.matrix(jheem.sex.risk, cdc.sex.risk)
mat2 = mp$get.matrix(jheem.sex.risk, cdc.only.sex) # all zeroes


# mat 1 looks like:
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
# [1,]       1
# [2,]
# [3,]                      1              1
# [4,]
# [5,]  1
# [6,]            1
# [7,]                 1              1
# [8,]                           1              1

# meaning our columns are in the expected order
# but our rows are male/msm, female/msm, male/msm_idu, female/msm_idu, male/het, female/het, male/idu, female/idu
# which is not the expected order of sex and risk dimensions (risk is first but sex was expected to be first)