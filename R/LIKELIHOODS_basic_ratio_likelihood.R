#' Create Basic Ratio Likelihood Instructions
#' @inheritParams create.basic.likelihood.instructions
#' @export
create.basic.ratio.likelihood.instructions <- function(outcome.for.data,
                                                       outcome.for.sim, dimensions = character(0),
                                                       denominator.dimensions = dimensions,
                                                       dimension.values = NULL, # EXPERIMENTAL
                                                       levels.of.stratification = NULL,
                                                       from.year = -Inf,
                                                       to.year = Inf,
                                                       omit.years = NULL,
                                                       sources.to.use = NULL,
                                                       correlation.different.years = 0.5,
                                                       correlation.different.strata = 0.1,
                                                       correlation.different.sources = 0.3,
                                                       correlation.same.source.different.details = 0.3,
                                                       observation.correlation.form = c("compound.symmetry", "autoregressive.1")[1],
                                                       error.variance.term = NULL,
                                                       error.variance.type = NULL,
                                                       ratio.cv = NULL,
                                                       ratio.correlation = NULL,
                                                       weights = list(),
                                                       equalize.weight.by.year = T) {
    JHEEM.BASIC.RATIO.LIKELIHOOD.INSTRUCTIONS$new(
        outcome.for.data = outcome.for.data,
        outcome.for.sim = outcome.for.sim,
        outcome.value = NULL,
        dimensions = dimensions,
        denominator.dimensions = denominator.dimensions,
        dimension.values = dimension.values, # EXPERIMENTAL
        levels.of.stratification = levels.of.stratification,
        from.year = from.year,
        to.year = to.year,
        omit.years = omit.years,
        sources.to.use = sources.to.use,
        included.multiplier = NULL,
        included.multiplier.sd = NULL,
        included.multiplier.correlation = NULL,
        included.multiplier.correlation.structure = c("compound.symmetry", "autoregressive.1")[1],
        correlation.different.years = correlation.different.years,
        correlation.different.strata = correlation.different.strata,
        correlation.different.sources = correlation.different.sources,
        correlation.same.source.different.details = correlation.same.source.different.details,
        observation.correlation.form = observation.correlation.form,
        error.variance.term = error.variance.term,
        error.variance.type = error.variance.type,
        ratio.cv = ratio.cv,
        ratio.correlation = ratio.correlation,
        weights = weights,
        equalize.weight.by.year = equalize.weight.by.year,
        use.lognormal.approximation = F,
        calculate.lagged.difference = TRUE,
        is.basic.ratio.likelihood = TRUE
    )
}

JHEEM.BASIC.RATIO.LIKELIHOOD.INSTRUCTIONS <- R6::R6Class(
    "jheem.basic.ratio.likelihood.instructions",
    inherit = JHEEM.BASIC.LIKELIHOOD.INSTRUCTIONS
)

JHEEM.BASIC.RATIO.LIKELIHOOD <- R6::R6Class(
    "jheem.basic.ratio.likelihood",
    inherit = JHEEM.BASIC.LIKELIHOOD,
    portable = F,
    public = list(
        initialize = function(instructions,
                              version,
                              location, # combinations of version and location tell us sublocations
                              sub.version,
                              data.manager,
                              throw.error.if.no.data,
                              error.prefix) {
            # 'super' here is the basic likelihood, not main likelihood
            super$initialize(
                instructions = instructions,
                version = version,
                sub.version = sub.version,
                location = location,
                data.manager = data.manager,
                throw.error.if.no.data = throw.error.if.no.data,
                error.prefix = error.prefix
            )
            
            # We must have a denominator, so we cannot be a "count", just "rate" or "proportion"... is that what Todd said? Maybe not?
            if (private$i.outcome.is.count) {
                stop(paste0(error.prefix, "outcome must have scale 'rate' or 'proportion' to use 'jheem.basic.ratio.likelihood'"))
            }
            
            ## ---- GENERATE SIM LAG MATRIX ---- ##
            sim.years.to <- private$i.years[-1] # private$i.years is already sorted and same as the sim.required.dimnames$year
            sim.years.from <- private$i.years[-length(private$i.years)]
            private$i.to.indices <- get.array.access.indices(private$i.sim.required.dimnames, dimension.values = list(year = sim.years.to))
            private$i.from.indices <- get.array.access.indices(private$i.sim.required.dimnames, dimension.values = list(year = sim.years.from))
            private$i.sim.lag.matrix <- t(sapply(1:length(private$i.to.indices), function(i) {
                row <- rep(0, prod(sapply(private$i.sim.required.dimnames, length)))
                row[private$i.to.indices[i]] <- 1
                row[private$i.from.indices[i]] <- -1
                row
            }))
            
            ## ---- FIND LAGGED TRANSFORMATION MATRIX ---- ##
            # ASSUMPTION: sim years are contiguous (no gaps) according to Todd
            private$i.obs.indices <- private$i.lagged.pairs[2 * (1:(length(private$i.lagged.pairs) / 2)) - 1] + 1
            private$i.transformation.matrix <- private$i.transformation.matrix[private$i.obs.indices, private$i.to.indices]
            
            ## ---- GENERATE SPARSE REPRESENTATIONS OF TRANSFORMATION MATRIX ---- ##
            private$i.transformation.matrix.indices <- generate_transformation_matrix_indices(
                private$i.transformation.matrix,
                private$i.n.lagged.obs,
                length(private$i.transformation.matrix) / private$i.n.lagged.obs
            )
            
            private$i.transformation.matrix.row.oriented.indices <- generate_transformation_matrix_row_oriented_indices(
                private$i.transformation.matrix,
                private$i.n.lagged.obs,
                length(private$i.transformation.matrix) / private$i.n.lagged.obs
            )
            
            ## ---- LOG AND LAG DATA ---- ##
            log.obs.sigma <- log(private$i.measurement.error.covariance.matrix / private$i.obs.vector + 1) # we're going to abuse slightly by treating the obs themselves as the mean of the LN dist
            log.obs.mean <- log(private$i.obs.vector) - diag(log.obs.sigma) / 2
            log.obs <- log(private$i.obs.vector)
            
            lagged.log.obs <- apply_lag_to_vector(
                log.obs,
                private$i.lagged.pairs,
                rep(0, private$i.n.lagged.obs),
                private$i.n.obs
            )
            lagged.log.obs.mean <- apply_lag_to_vector(
                log.obs.mean,
                private$i.lagged.pairs,
                rep(0, private$i.n.lagged.obs),
                private$i.n.obs
            )
            lagged.log.obs.sigma <- apply_lag_to_matrix(
                log.obs.sigma,
                private$i.lagged.pairs,
                rep(0, private$i.n.lagged.obs**2),
                private$i.n.obs
            )
            
            # lagged.log.obs = obs.lag.matrix %*% log.obs
            # lagged.log.obs.sigma = obs.lag.matrix %*% log.obs.sigma %*% t(obs.lag.matrix)
            
            private$i.lagged.obs <- exp(lagged.log.obs)
            private$i.lagged.obs.sigma <- lagged.log.obs.mean %*% t(lagged.log.obs.mean) * (exp(lagged.log.obs.sigma) - 1)
            
            ## ---- MAKE A COMPOUND SYMMETRY MATRIX ---- ## to be renamed once I understand what it's for
            if (!is.null(private$i.parameters$ratio.cv)) {
                val = (log(private$i.parameters$ratio.cv) / qnorm(0.975))**2
                compound.symmetry.matrix = matrix(
                    val * private$i.parameters$ratio.correlation,
                    nrow = nrow(private$i.lagged.obs.sigma),
                    ncol = ncol(private$i.lagged.obs.sigma))
                diag(compound.symmetry.matrix) = val
                
                private$i.lagged.obs.sigma = private$i.lagged.obs.sigma + compound.symmetry.matrix
            }
        }
    ),
    private = list(
        do.compute = function(sim, log, use.optimized.get, check.consistency, debug) {
            # browser()
            use.poisson <- private$i.outcome.is.rate || private$i.outcome.is.count
            use.denominator <- private$i.outcome.is.count
            
            if (use.optimized.get) {
                sim.numerator.data <- sim$optimized.get(private$i.optimized.get.instructions[["sim.num.instr"]])
            } else {
                sim.numerator.data <- sim$get(
                    outcomes = private$i.outcome.for.sim,
                    keep.dimensions = names(private$i.sim.required.dimnames),
                    dimension.values = private$i.sim.dimension.values,
                    output = "numerator",
                    drop.single.sim.dimension = T
                )
            }
            sim.dim.names <- dimnames(sim.numerator.data)
            sim.numerator.data <- as.numeric(sim.numerator.data)
            
            if (use.denominator) {
                if (use.optimized.get) {
                    sim.denominator.data <- as.numeric(sim$optimized.get(private$i.optimized.get.instructions[["sim.denom.instr"]]))
                } else {
                    sim.denominator.data <- as.numeric(sim$get(
                        outcome = private$i.outcome.for.sim,
                        keep.dimensions = names(private$i.denominator.required.dimnames),
                        dimension.values = private$i.denominator.dimension.values,
                        output = "denominator",
                        drop.single.sim.dimension = T
                    ))
                }
            } else {
                sim.denominator.data <- 1
            }
            
            raw.sim.mean <- sim.numerator.data / sim.denominator.data
            
            if (use.poisson) {
                raw.sim.variance <- sim.numerator.data / sim.denominator.data^2
            } # x / n^2
            else {
                raw.sim.variance <- raw.sim.mean * (1 - raw.sim.mean) / sim.denominator.data
            } # mu * (1 - mu) / n which is np(1-p) / n^2
            
            log.sim.variance <- log(raw.sim.variance / (raw.sim.mean^2) + 1)
            log.sim.mean <- log(raw.sim.mean) - log.sim.variance / 2
            log.sim.sigma <- diag(log.sim.variance)
            
            lagged.log.sim.mean <- private$i.sim.lag.matrix %*% log.sim.mean
            lagged.log.sim.sigma <- private$i.sim.lag.matrix %*% log.sim.sigma %*% t(private$i.sim.lag.matrix)
            
            lagged.sim.mean <- exp(lagged.log.sim.mean + diag(lagged.log.sim.sigma) / 2)
            lagged.sim.sigma <- lagged.sim.mean %*% t(lagged.sim.mean) * (exp(lagged.log.sim.sigma) - 1)
            
            lagged.n <- (sim.denominator.data[private$i.to.indices] + sim.denominator.data[private$i.from.indices]) / 2
            
            # lagged.log.sim.mean = apply_lag_to_vector(log.sim.mean,
            #                                           private$i.lagged.pairs,
            #                                           rep(0, private$i.n.lagged.obs),
            #                                           private$i.n.obs)
            # lagged.log.sim.sigma = apply_lag_to_matrix(log.sim.sigma,
            #                                            private$i.lagged.pairs,
            #                                            rep(0, private$i.n.lagged.obs**2),
            #                                            private$i.n.obs)
            #
            # lagged.sim.mean = exp(lagged.log.sim.mean + diag(lagged.log.sim.sigma)/2)
            # lagged.sim.sigma = lagged.sim.mean %*% t(lagged.sim.mean) * (exp(lagged.log.sim.sigma) - 1)
            
            aggregated.lagged.sim.n <- get_basic_likelihood_mean(
                lagged.n,
                private$i.transformation.matrix.row.oriented.indices,
                private$i.n.lagged.obs,
                numeric(private$i.n.lagged.obs)
            )
            aggregated.lagged.sim.mean <- get_basic_likelihood_mean(
                lagged.sim.mean,
                private$i.transformation.matrix.row.oriented.indices,
                private$i.n.lagged.obs,
                numeric(private$i.n.lagged.obs)
            )
            
            # @Andrew This can probably be optimized since it is sparse. Unfortunately, "lagged.sim.sigma" is not diagonal, so we can't use the get_basic_likelihood_sigma algorithm.
            aggregated.lagged.sim.sigma <- private$i.transformation.matrix %*% lagged.sim.sigma %*% t(private$i.transformation.matrix)
            # aggregated.lagged.sim.n = sim.aggregation.matrix %*% lagged.n
            # aggregated.lagged.sim.mean = sim.aggregation.matrix %*% lagged.sim.mean
            
            final.sigma <- private$i.lagged.obs.sigma + aggregated.lagged.sim.sigma
            final.mean <- aggregated.lagged.sim.mean
            
            likelihood <- mvtnorm::dmvnorm(private$i.lagged.obs,
                                           mean = final.mean,
                                           sigma = final.sigma,
                                           log = T,
                                           checkSymmetry = F
            )
            
            if (debug) {
                lik.summary <- cbind(private$i.metadata.for.lag, obs = private$i.lagged.obs, mean = final.mean, sd = sqrt(diag(final.sigma)))
                lik.summary$z <- (lik.summary$obs - lik.summary$mean) / lik.summary$sd
                rownames(lik.summary) <- 1:nrow(lik.summary)
                browser()
            }
            return(likelihood)
        },
        i.sim.lag.matrix = NULL,
        i.to.indices = NULL,
        i.from.indices = NULL,
        i.obs.indices = NULL,
        i.lagged.obs = NULL,
        i.lagged.obs.sigma = NULL
    )
)
