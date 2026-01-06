library(jheem2)

# Load data and run current workflow
age_data <- get(load(file = "restratify_age_counts_reprex_for_Nick.Rdata"))

# Helper function from colleague
do_prepare_for_restratify <- function(arr, top.age = 100) {
    original_dimensions <- names(dim(arr))
    reordered_dimensions <- c(original_dimensions[original_dimensions != "age"], "age")
    arr_reordered <- apply(arr, reordered_dimensions, function(x) {
        x
    })
    new_dimnames <- dimnames(arr_reordered)
    new_dimnames$age[5] <- paste0("55-", as.character(top.age), " years")
    new_dimnames$age <- c(new_dimnames$age, paste0(as.character(top.age + 1), "-", as.character(top.age + 10), " years"))
    arr_reordered <- c(arr_reordered, rep(0, dim(arr_reordered)["year"]))
    arr_restored <- array(arr_reordered, sapply(new_dimnames, length), new_dimnames)
    apply(arr_restored, original_dimensions, function(x) {
        x
    })
}

# Run the workflow
prepared <- do_prepare_for_restratify(age_data)
print("Original data:")
print(age_data)
print("\nPrepared data:")
print(prepared)

# Implement Steffen's monotonic interpolation (based on Julia Interpolations.jl)
steffen_splinefun <- function(x, y) {
    n <- length(x)

    # Calculate secant slopes
    delta <- numeric(n - 1)
    for (k in 1:(n - 1)) {
        delta[k] <- (y[k + 1] - y[k]) / (x[k + 1] - x[k])
    }

    # Calculate tangents using Steffen's method
    m <- numeric(n)
    m[1] <- delta[1] # Left endpoint

    for (k in 2:(n - 1)) {
        # Weighted average
        p <- ((x[k + 1] - x[k]) * delta[k - 1] + (x[k] - x[k - 1]) * delta[k]) / (x[k + 1] - x[k - 1])

        # Steffen's tangent formula
        m[k] <- (sign(delta[k - 1]) + sign(delta[k])) *
            min(abs(delta[k - 1]), abs(delta[k]), 0.5 * abs(p))
    }

    m[n] <- delta[n - 1] # Right endpoint

    # Create cubic Hermite interpolating function
    function(xi, deriv = 0) {
        result <- numeric(length(xi))

        for (i in seq_along(xi)) {
            # Find interval
            if (xi[i] <= x[1]) {
                idx <- 1
            } else if (xi[i] >= x[n]) {
                idx <- n - 1
            } else {
                idx <- max(which(x <= xi[i]))
            }

            # Cubic Hermite interpolation
            h <- x[idx + 1] - x[idx]
            t <- (xi[i] - x[idx]) / h

            if (deriv == 0) {
                # Function value
                result[i] <- y[idx] * (2 * t^3 - 3 * t^2 + 1) +
                    y[idx + 1] * (-2 * t^3 + 3 * t^2) +
                    m[idx] * h * (t^3 - 2 * t^2 + t) +
                    m[idx + 1] * h * (t^3 - t^2)
            } else if (deriv == 1) {
                # First derivative
                result[i] <- (y[idx] * (6 * t^2 - 6 * t) +
                    y[idx + 1] * (-6 * t^2 + 6 * t) +
                    m[idx] * h * (3 * t^2 - 4 * t + 1) +
                    m[idx + 1] * h * (3 * t^2 - 2 * t)) / h
            } else if (deriv == 2) {
                # Second derivative
                result[i] <- (y[idx] * (12 * t - 6) +
                    y[idx + 1] * (-12 * t + 6) +
                    m[idx] * h * (6 * t - 4) +
                    m[idx + 1] * h * (6 * t - 2)) / h^2
            }
        }
        result
    }
}

# Custom function to test different spline methods including Steffen
test_spline_method <- function(counts, method_name) {
    # Manual endpoints based on the prepared data structure
    # "13-24 years", "25-34 years", "35-44 years", "45-54 years", "55-100 years", "101-110 years"
    endpoints <- c(13, 25, 35, 45, 55, 101, 111)
    obs.cum.n <- cumsum(counts)

    spline_x <- endpoints
    spline_y <- c(0, obs.cum.n)

    if (method_name == "steffen") {
        fn <- steffen_splinefun(spline_x, spline_y)
    } else {
        fn <- splinefun(x = spline_x, y = spline_y, method = method_name)
    }

    # Apply to desired age brackets (13-100)
    result <- fn(14:101) - fn(13:100) # Differences to get PDF
    names(result) <- paste0(13:100, " years")
    return(result)
}

# Test all three methods
results_fc <- test_spline_method(prepared[1, ], "monoH.FC")
results_hyman <- test_spline_method(prepared[1, ], "hyman")
results_steffen <- test_spline_method(prepared[1, ], "steffen")

# Create focused plots for tail behavior analysis
layout(matrix(c(1, 2, 3, 3), nrow = 2, byrow = TRUE), heights = c(4, 1))

# Plot 1: Full Range (13-100) - Focus on Tail Behavior
ages_full <- 13:100
fc_values_full <- results_fc[1:88] # Ages 13-100
hyman_values_full <- results_hyman[1:88]
steffen_values_full <- results_steffen[1:88]

plot(ages_full, fc_values_full,
    type = "l", lwd = 3, col = "blue",
    main = "Age Distributions: Full Range",
    xlab = "Age", ylab = "Count per Year",
    xlim = c(13, 100), ylim = range(c(fc_values_full, hyman_values_full, steffen_values_full))
)

lines(ages_full, hyman_values_full, lwd = 3, col = "red")
lines(ages_full, steffen_values_full, lwd = 3, col = "green")

# Original data boundaries
original_boundaries <- c(25, 35, 45, 55)
abline(v = original_boundaries, lty = 2, col = "gray")

# Plot 2: Focus on Ages 40-100
ages_tail <- 40:100
tail_start_idx <- 40 - 13 + 1 # Index for age 40
tail_end_idx <- min(88, 100 - 13 + 1) # Index for age 100 or end of data

fc_tail <- fc_values_full[tail_start_idx:tail_end_idx]
hyman_tail <- hyman_values_full[tail_start_idx:tail_end_idx]
steffen_tail <- steffen_values_full[tail_start_idx:tail_end_idx]
ages_tail_actual <- ages_full[tail_start_idx:tail_end_idx]

plot(ages_tail_actual, fc_tail,
    type = "l", lwd = 4, col = "blue",
    main = "Ages 40-100: Tail Behavior",
    xlab = "Age", ylab = "Count per Year",
    xlim = c(40, 100), ylim = range(c(fc_tail, hyman_tail, steffen_tail))
)

lines(ages_tail_actual, hyman_tail, lwd = 4, col = "red")
lines(ages_tail_actual, steffen_tail, lwd = 4, col = "green")

# Add vertical line at age 55 (end of original data)
abline(v = 55, lty = 2, col = "gray")

# # Add points to show the discrete nature - COMMENTED OUT TO CHECK FOR VISUAL ARTIFACT
# points(ages_tail_actual, fc_tail, pch=16, col="blue", cex=0.5)
# points(ages_tail_actual, hyman_tail, pch=16, col="red", cex=0.5)
# points(ages_tail_actual, steffen_tail, pch=16, col="green", cex=0.5)

# Plot 3: Legend at bottom
par(mar = c(0, 0, 0, 0))
plot.new()
legend("center", c("Fritsch-Carlson (current)", "Hyman", "Steffen"),
    col = c("blue", "red", "green"), lwd = 3, horiz = TRUE, bty = "n"
)

# # COMMENTED OUT FOR MEETING - CDF AND CURVATURE PLOTS:
# # Plot 2: CDF Comparison (Cumulative Distributions - where splines actually fit)
# # Get the CDF data using manual endpoints
# endpoints <- c(13, 25, 35, 45, 55, 101, 111)
# obs.cum.n <- cumsum(prepared[1,])
# spline_x <- endpoints
# spline_y <- c(0, obs.cum.n)
#
# # Create spline functions for CDF plotting
# fn_fc <- splinefun(x=spline_x, y=spline_y, method="monoH.FC")
# fn_hyman <- splinefun(x=spline_x, y=spline_y, method="hyman")
# fn_steffen <- steffen_splinefun(spline_x, spline_y)
#
# # Plot CDF curves
# x_fine <- seq(13, 60, by=0.5)
# plot(spline_x, spline_y, pch=16, cex=1.2, col="black",
#      main="CDF: Cumulative Distributions\n(Where splines are fitted)",
#      xlab="Age", ylab="Cumulative Count",
#      xlim=c(13, 60))
#
# lines(x_fine, fn_fc(x_fine), col="blue", lwd=2)
# lines(x_fine, fn_hyman(x_fine), col="red", lwd=2)
# lines(x_fine, fn_steffen(x_fine), col="green", lwd=2)
#
# abline(v=original_boundaries, lty=2, col="gray")
# legend("bottomright", c("Data Points", "Fritsch-Carlson", "Hyman", "Steffen"),
#        col=c("black", "blue", "red", "green"),
#        pch=c(16, NA, NA, NA), lty=c(NA, 1, 1, 1), lwd=c(NA, 2, 2, 2), cex=0.8)
#
# # Plot 4: Second derivatives (curvature) - shows smoothness
# curv_fc <- fn_fc(x_fine, deriv=2)
# curv_hyman <- fn_hyman(x_fine, deriv=2)
# curv_steffen <- fn_steffen(x_fine, deriv=2)
#
# plot(x_fine, curv_fc, type="l", col="blue", lwd=2,
#      main="Second Derivatives\n(Lower = smoother)",
#      xlab="Age", ylab="Curvature",
#      ylim=range(c(curv_fc, curv_hyman, curv_steffen), na.rm=TRUE))
# lines(x_fine, curv_hyman, col="red", lwd=2)
# lines(x_fine, curv_steffen, col="green", lwd=2)
# abline(h=0, lty=2, col="gray")
# abline(v=original_boundaries, lty=2, col="gray")
#
# legend("topright", c("Fritsch-Carlson", "Hyman", "Steffen"),
#        col=c("blue", "red", "green"), lwd=2, cex=0.8)

cat("\n=== TAIL BEHAVIOR ANALYSIS ===\n")
cat("Two plots focusing on extrapolation behavior:\n")
cat("1. LEFT: Full range (13-100)\n")
cat("2. RIGHT: Ages 40-100 focusing on tail behavior\n")
cat("\nColor coding shown in legend below plots\n")
cat("Gray dashed lines: Original age bracket boundaries\n")

# Check count preservation for all methods
print("=== COUNT PRESERVATION ANALYSIS ===")
original_total <- sum(prepared[1, ])
fc_total <- sum(results_fc)
hyman_total <- sum(results_hyman)
steffen_total <- sum(results_steffen)

print(paste("Original total:", round(original_total, 2)))
print(paste(
    "Fritsch-Carlson total:", round(fc_total, 2),
    "(loss:", round(original_total - fc_total, 2), ")"
))
print(paste(
    "Hyman total:", round(hyman_total, 2),
    "(loss:", round(original_total - hyman_total, 2), ")"
))
print(paste(
    "Steffen total:", round(steffen_total, 2),
    "(loss:", round(original_total - steffen_total, 2), ")"
))

# SMOOTHNESS ANALYSIS:
# # Calculate smoothness metrics (integral of squared curvature)
# x_fine <- seq(13, 60, by=0.5)
# fn_fc <- splinefun(x=spline_x, y=spline_y, method="monoH.FC")
# fn_hyman <- splinefun(x=spline_x, y=spline_y, method="hyman")
# fn_steffen <- steffen_splinefun(spline_x, spline_y)
#
# smoothness_fc <- sum(fn_fc(x_fine, deriv=2)^2, na.rm=TRUE)
# smoothness_hyman <- sum(fn_hyman(x_fine, deriv=2)^2, na.rm=TRUE)
# smoothness_steffen <- sum(fn_steffen(x_fine, deriv=2)^2, na.rm=TRUE)
#
# print("\n=== SMOOTHNESS ANALYSIS ===")
# print("(Lower values = mathematically smoother)")
# print(paste("Fritsch-Carlson smoothness:", round(smoothness_fc, 1)))
# print(paste("Hyman smoothness:", round(smoothness_hyman, 1),
#            "(", round((smoothness_fc - smoothness_hyman)/smoothness_fc * 100, 1), "% improvement)"))
# print(paste("Steffen smoothness:", round(smoothness_steffen, 1),
#            "(", round((smoothness_fc - smoothness_steffen)/smoothness_fc * 100, 1), "% improvement)"))
