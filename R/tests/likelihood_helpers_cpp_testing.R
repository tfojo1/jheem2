# /*** R
# 
# mapping.matrix = rbind(c(1,1,0,0),c(0,0,1,1),c(1,0,1,0))
# sim.numerator.data = c(30, 40, 50, 45)
# sim.denominator.data = c(200, 200, 300, 300)
# measurement.error.covariance.matrix = matrix(0, nrow=3, ncol=3)
# mean = rep(0,3)
# sigma = matrix(0, nrow=3, ncol=3)
# 
# out=get_basic_likelihood_elements(sim.numerator.data,
#                                   sim.denominator.data,
#                                   mapping.matrix,
#                                   measurement.error.covariance.matrix,
#                                   mean,
#                                   sigma)
# browser()
# 
# trans.sim.num = mapping.matrix %*% sim.numerator.data
# trans.sim.denom = mapping.matrix %*% sim.denominator.data
# model.imp.error.var = sim.numerator.data * (1 - sim.numerator.data / sim.denominator.data)
# model.imp.error.cov.mat = diag(model.imp.error.var, nrow=length(model.imp.error.var), ncol=length(model.imp.error.var))
# trans.model.imp.error.cov.mat = mapping.matrix %*% model.imp.error.cov.mat %*% t(mapping.matrix)
# */

mat = rbind(c(1,1,0,0),
            c(0,0,1,1),
            c(0,1,0,1))
smat = generate_transformation_matrix_indices(mat, 3,4)
srow = generate_transformation_matrix_row_oriented_indices(mat, 3, 4)
sim.numerator.d = rep(10,4)
sim.denominator.d = rep(20,4)
measure_error_matrix = matrix(0, nrow=3, ncol=3)
mean = numeric(3)

mean = get_basic_likelihood_mean(
    sim.numerator.d,
    srow,
    3,
    mean
)

sigma = matrix(0, nrow=3, ncol=3)

sigma = get_basic_likelihood_sigma(
    sim.numerator.d,
    sim.denominator.d,
    smat,
    measure_error_matrix,
    3,
    sigma
)

model.imp.cov.matrix = diag(
    sim.numerator.d * (1 - sim.numerator.d / sim.denominator.d),
    nrow=4, ncol=4)
mat %*% model.imp.cov.matrix %*% t(mat)