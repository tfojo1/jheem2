library(jheem2)

# Test the 85+ constraint approach for better tail behavior
# Based on national HIV prevalence data: 0.84% of 55+ population is 85+

# Load data
age_data <- get(load(file = "restratify_age_counts_reprex_for_Nick.Rdata"))

# Original helper function (for reference/comparison)
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

# NEW: Modified helper function with 85+ split
do_prepare_for_restratify_with_85plus <- function(arr, count_85plus = 190, top.age = 100) {
    # Apply original transformation first
    prepared_original <- do_prepare_for_restratify(arr, top.age)
    
    # Extract the counts for the first year to work with
    counts_original <- prepared_original[1, ]  # Assuming first year
    
    # Split the 55-100 bin (5th element) into 55-84 and 85-100
    count_55_100 <- counts_original[5]  # Should be 22555
    count_55_84 <- count_55_100 - count_85plus  # 22555 - 190 = 22365
    
    # Create new 6-element count vector
    new_counts <- c(
        counts_original[1:4],  # Keep 13-24, 25-34, 35-44, 45-54
        count_55_84,           # New: 55-84
        count_85plus           # New: 85-100
    )
    
    # Create new dimension names for 6 bins
    new_age_names <- c(
        "13-24 years",
        "25-34 years", 
        "35-44 years",
        "45-54 years",
        "55-84 years",   # Split the original 55+ bin
        "85-100 years"   # Small tail mass
    )
    
    # Create new array structure
    new_arr <- array(
        new_counts,
        dim = c(year = 1, age = 6),
        dimnames = list(year = "2020", age = new_age_names)
    )
    
    return(new_arr)
}

# Prepare both versions for comparison
cat("=== PREPARING DATA FOR COMPARISON ===\n")

# Original 5-bin approach
prepared_original <- do_prepare_for_restratify(age_data)
cat("Original 5-bin counts:\n")
print(prepared_original[1, ])
cat("Total:", sum(prepared_original[1, ]), "\n\n")

# New 6-bin approach with 85+ constraint
prepared_new <- do_prepare_for_restratify_with_85plus(age_data, count_85plus = 190)
cat("New 6-bin counts (with 85+ split):\n")
print(prepared_new[1, ])
cat("Total:", sum(prepared_new[1, ]), "\n")
cat("85+ count:", prepared_new[1, 6], "( =", round(prepared_new[1, 6]/sum(prepared_new[1, ])*100, 2), "% of total )\n\n")

# Steffen spline implementation (from original workflow)
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

# Function to test spline methods
test_spline_method <- function(counts, method_name, bin_structure = "5bin") {
    if (bin_structure == "5bin") {
        # Original 5-bin structure: endpoints for 5 bins + 1 zero endpoint = 6 points
        endpoints <- c(13, 25, 35, 45, 55, 101)
        obs.cum.n <- cumsum(counts[1:5])  # Only use first 5 counts (ignore the zero padding)
    } else {
        # New 6-bin structure: endpoints for 6 bins + 1 zero endpoint = 7 points  
        endpoints <- c(13, 25, 35, 45, 55, 85, 101)
        obs.cum.n <- cumsum(counts)  # Use all 6 counts
    }
    
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

cat("=== TESTING SPLINE METHODS ===\n")

# Test original 5-bin approach (for comparison)
cat("Testing original 5-bin approach...\n")
results_5bin_hyman <- test_spline_method(prepared_original[1, ], "hyman", "5bin")
results_5bin_fc <- test_spline_method(prepared_original[1, ], "monoH.FC", "5bin")

# Test new 6-bin approach  
cat("Testing new 6-bin approach with 85+ constraint...\n")
results_6bin_hyman <- test_spline_method(prepared_new[1, ], "hyman", "6bin")
results_6bin_fc <- test_spline_method(prepared_new[1, ], "monoH.FC", "6bin")

# Verify count preservation
cat("\n=== COUNT PRESERVATION CHECK ===\n")
cat("Original total:", round(sum(prepared_original[1, ]), 2), "\n")
cat("5-bin Hyman total:", round(sum(results_5bin_hyman), 2), "(diff:", round(sum(prepared_original[1, ]) - sum(results_5bin_hyman), 3), ")\n")
cat("6-bin Hyman total:", round(sum(results_6bin_hyman), 2), "(diff:", round(sum(prepared_new[1, ]) - sum(results_6bin_hyman), 3), ")\n")

# Create comparison plots
pdf("85plus_constraint_comparison.pdf", width = 14, height = 10)

# Set up 2x2 layout
layout(matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE))

ages <- 13:100

# Plot 1: Full range comparison (13-100)
plot(ages, results_5bin_hyman, 
     type = "l", lwd = 2, col = "red", lty = 2,
     main = "Full Range: 5-bin vs 6-bin (Hyman method)",
     xlab = "Age", ylab = "Count per Year",
     xlim = c(13, 100))
lines(ages, results_6bin_hyman, col = "blue", lwd = 3)
abline(v = c(25, 35, 45, 55, 85), lty = 3, col = "gray")
legend("topright", 
       c("5-bin (original)", "6-bin (85+ split)"),
       col = c("red", "blue"), lty = c(2, 1), lwd = c(2, 3))

# Plot 2: Focus on tail behavior (ages 50-100)
tail_ages <- 50:100
tail_idx_start <- 50 - 13 + 1
tail_idx_end <- 100 - 13 + 1

plot(tail_ages, results_5bin_hyman[tail_idx_start:tail_idx_end],
     type = "l", lwd = 2, col = "red", lty = 2,
     main = "Tail Behavior Focus (Ages 50-100)",
     xlab = "Age", ylab = "Count per Year",
     xlim = c(50, 100))
lines(tail_ages, results_6bin_hyman[tail_idx_start:tail_idx_end], col = "blue", lwd = 3)
abline(v = c(55, 85), lty = 3, col = "gray")
legend("topright",
       c("5-bin (original)", "6-bin (85+ split)", "Age 55 boundary", "Age 85 boundary"),
       col = c("red", "blue", "gray", "gray"), 
       lty = c(2, 1, 3, 3), lwd = c(2, 3, 1, 1))

# Plot 3: Compare both spline methods with 6-bin structure
plot(ages, results_6bin_fc,
     type = "l", lwd = 2, col = "darkgreen",
     main = "6-bin Results: Fritsch-Carlson vs Hyman",
     xlab = "Age", ylab = "Count per Year")
lines(ages, results_6bin_hyman, col = "blue", lwd = 2)
abline(v = c(25, 35, 45, 55, 85), lty = 3, col = "gray")
legend("topright",
       c("Fritsch-Carlson", "Hyman"),
       col = c("darkgreen", "blue"), lwd = 2)

# Plot 4: Diagnostic - PDF values at key ages
key_ages <- c(50, 55, 60, 65, 70, 75, 80, 85, 90, 95)
key_idx <- key_ages - 13 + 1

plot(key_ages, results_5bin_hyman[key_idx],
     type = "b", pch = 16, col = "red", cex = 1.2,
     main = "PDF Values at Key Ages",
     xlab = "Age", ylab = "Count per Year",
     ylim = range(c(results_5bin_hyman[key_idx], results_6bin_hyman[key_idx])))
lines(key_ages, results_6bin_hyman[key_idx], type = "b", pch = 17, col = "blue", cex = 1.2)
abline(v = c(55, 85), lty = 3, col = "gray")
legend("topright",
       c("5-bin", "6-bin", "Data boundaries"),
       col = c("red", "blue", "gray"), 
       pch = c(16, 17, NA), lty = c(1, 1, 3))

dev.off()

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Plots saved to: 85plus_constraint_comparison.pdf\n")
cat("\nKey diagnostics:\n")
cat("PDF at age 55:\n")
cat("  5-bin:", round(results_5bin_hyman[55-13+1], 1), "\n")
cat("  6-bin:", round(results_6bin_hyman[55-13+1], 1), "\n")
cat("PDF at age 85:\n")
cat("  5-bin:", round(results_5bin_hyman[85-13+1], 1), "\n")
cat("  6-bin:", round(results_6bin_hyman[85-13+1], 1), "\n")
cat("PDF at age 95:\n")
cat("  5-bin:", round(results_5bin_hyman[95-13+1], 1), "\n")
cat("  6-bin:", round(results_6bin_hyman[95-13+1], 1), "\n")