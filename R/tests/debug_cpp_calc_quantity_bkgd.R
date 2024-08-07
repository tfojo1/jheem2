load('R/tests/calc.quantity.bkgd.debug.Rdata')
Rcpp::sourceCpp("src/calculate_quantity_background_value.cpp")

calculated.values = do_calculate_quantity_background_value(quantity = args$quantity,
                                                           missing_times = args$missing.times,
                                                           specification_metadata = args$specification_metadata,
                                                           location = args$location,
                                                           engine = args$engine,
                                                           check_consistency = args$check.consistency,
                                                           error_prefix = args$error.prefix,
                                                           dim_names_equal = args$dim.names.equal)
