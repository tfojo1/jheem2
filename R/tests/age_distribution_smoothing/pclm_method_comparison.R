library(jheem2)

# PCLM Method Comparison for Age Distribution Smoothing
# Compares 6-bin Hyman spline vs 5-bin and 6-bin PCLM approaches

# Install ungroup package if needed
if (!require(ungroup)) {
    cat("Installing ungroup package...\n")
    install.packages("ungroup")
    library(ungroup)
}

# Load data
age_data <- get(load(file = "restratify_age_counts_reprex_for_Nick.Rdata"))

cat("=== PCLM METHOD COMPARISON ===\n")
cat("Testing age distribution smoothing methods\n\n")

# Helper functions for data preparation
prepare_5bin_data <- function(arr, top.age = 100) {
    original_dimensions <- names(dim(arr))
    reordered_dimensions <- c(original_dimensions[original_dimensions != "age"], "age")
    arr_reordered <- apply(arr, reordered_dimensions, function(x) {x})
    new_dimnames <- dimnames(arr_reordered)
    new_dimnames$age[5] <- paste0("55-", as.character(top.age), " years")
    new_dimnames$age <- c(new_dimnames$age, paste0(as.character(top.age + 1), "-", as.character(top.age + 10), " years"))
    arr_reordered <- c(arr_reordered, rep(0, dim(arr_reordered)["year"]))
    arr_restored <- array(arr_reordered, sapply(new_dimnames, length), new_dimnames)
    apply(arr_restored, original_dimensions, function(x) {x})
}

prepare_6bin_data <- function(arr, count_85plus = 190, top.age = 100) {
    prepared_5bin <- prepare_5bin_data(arr, top.age)
    counts_5bin <- prepared_5bin[1, 1:5]  # Extract 5 bins, ignore zero padding
    
    # Split the 55-100 bin into 55-84 and 85-100
    count_55_100 <- counts_5bin[5]
    count_55_84 <- count_55_100 - count_85plus
    
    new_counts <- c(counts_5bin[1:4], count_55_84, count_85plus)
    new_age_names <- c("13-24 years", "25-34 years", "35-44 years", 
                       "45-54 years", "55-84 years", "85-100 years")
    
    new_arr <- array(new_counts, dim = c(year = 1, age = 6),
                     dimnames = list(year = "2020", age = new_age_names))
    return(new_arr)
}

# Prepare datasets
data_5bin <- prepare_5bin_data(age_data)
counts_5bin <- as.numeric(data_5bin[1, 1:5])  # Only first 5 bins

data_6bin <- prepare_6bin_data(age_data, count_85plus = 190)
counts_6bin <- as.numeric(data_6bin[1, ])

cat("Input data comparison:\n")
cat("5-bin structure (original):\n")
cat("  Bins: 13-24, 25-34, 35-44, 45-54, 55-100\n")
cat("  Counts:", paste(round(counts_5bin, 1), collapse = ", "), "\n")
cat("  Total:", round(sum(counts_5bin), 1), "\n\n")

cat("6-bin structure (85+ split using national HIV prevalence):\n")
cat("  Bins: 13-24, 25-34, 35-44, 45-54, 55-84, 85-100\n")
cat("  Counts:", paste(round(counts_6bin, 1), collapse = ", "), "\n")
cat("  Total:", round(sum(counts_6bin), 1), "\n")
cat("  85+ percentage:", round(counts_6bin[6]/sum(counts_6bin)*100, 2), "%\n\n")

# Extract PCLM results
extract_pclm_results <- function(pclm_model) {
    pclm_fitted <- fitted(pclm_model)
    interval_names <- names(pclm_fitted)
    
    # Extract starting age from interval notation
    pclm_ages <- numeric(length(interval_names))
    for (i in seq_along(interval_names)) {
        start_age <- as.numeric(gsub("\\[([0-9]+),.*", "\\1", interval_names[i]))
        pclm_ages[i] <- start_age
    }
    
    # Map to standard age range 13-100
    age_range <- 13:100
    pclm_results <- numeric(length(age_range))
    names(pclm_results) <- paste0(age_range, " years")
    
    for (i in seq_along(age_range)) {
        age <- age_range[i]
        if (age %in% pclm_ages) {
            age_idx <- which(pclm_ages == age)[1]
            pclm_results[i] <- pclm_fitted[age_idx]
        } else if (age >= min(pclm_ages) && age <= max(pclm_ages)) {
            age_idx <- which.min(abs(pclm_ages - age))
            pclm_results[i] <- pclm_fitted[age_idx]
        } else {
            pclm_results[i] <- 0
        }
    }
    return(pclm_results)
}

# Fit models
cat("Fitting models...\n")

# 6-bin Hyman spline (for comparison)
cat("  6-bin Hyman spline...\n")
spline_function <- function(counts, method_name, bin_structure = "6bin") {
    if (bin_structure == "6bin") {
        endpoints <- c(13, 25, 35, 45, 55, 85, 101)
        obs.cum.n <- cumsum(counts)
    }
    spline_x <- endpoints
    spline_y <- c(0, obs.cum.n)
    fn <- splinefun(x = spline_x, y = spline_y, method = method_name)
    result <- fn(14:101) - fn(13:100)
    names(result) <- paste0(13:100, " years")
    return(result)
}
spline_6bin_results <- spline_function(counts_6bin, "hyman", "6bin")

# 5-bin PCLM
cat("  5-bin PCLM...\n")
pclm_5bin_model <- pclm(x = c(13, 25, 35, 45, 55), 
                        y = counts_5bin, 
                        nlast = 45,  # 55-100 span
                        out.step = 1, verbose = FALSE)
pclm_5bin_results <- extract_pclm_results(pclm_5bin_model)

# 6-bin PCLM
cat("  6-bin PCLM...\n")
pclm_6bin_model <- pclm(x = c(13, 25, 35, 45, 55, 85), 
                        y = counts_6bin, 
                        nlast = 15,  # 85-100 span
                        out.step = 1, verbose = FALSE)
pclm_6bin_results <- extract_pclm_results(pclm_6bin_model)

cat("Model fitting complete.\n\n")

# Verify count preservation - TOTAL counts
cat("=== TOTAL COUNT PRESERVATION ===\n")
cat("Original 5-bin total:", round(sum(counts_5bin), 2), "\n")
cat("PCLM 5-bin total:    ", round(sum(pclm_5bin_results), 2), 
    " (diff: ", round(sum(counts_5bin) - sum(pclm_5bin_results), 3), ")\n")
cat("Original 6-bin total:", round(sum(counts_6bin), 2), "\n") 
cat("Spline 6-bin total:  ", round(sum(spline_6bin_results), 2), 
    " (diff: ", round(sum(counts_6bin) - sum(spline_6bin_results), 3), ")\n")
cat("PCLM 6-bin total:    ", round(sum(pclm_6bin_results), 2), 
    " (diff: ", round(sum(counts_6bin) - sum(pclm_6bin_results), 3), ")\n\n")

# Verify BIN-WISE count preservation
cat("=== BIN-WISE COUNT PRESERVATION ===\n")

# Check 5-bin PCLM
cat("5-bin PCLM bin preservation:\n")
bin_boundaries_5 <- c(13, 25, 35, 45, 55, 101)
ages <- 13:100
for (i in 1:5) {
    age_start <- bin_boundaries_5[i]
    age_end <- bin_boundaries_5[i + 1] - 1
    age_indices <- which(ages >= age_start & ages <= age_end)
    pclm_bin_total <- sum(pclm_5bin_results[age_indices])
    cat(sprintf("  Bin %d (%d-%d): Original=%.1f, PCLM=%.1f (diff=%.2f)\n",
               i, age_start, age_end, counts_5bin[i], pclm_bin_total, 
               counts_5bin[i] - pclm_bin_total))
}

cat("\n6-bin methods bin preservation:\n")
bin_boundaries_6 <- c(13, 25, 35, 45, 55, 85, 101)
for (i in 1:6) {
    age_start <- bin_boundaries_6[i]
    age_end <- bin_boundaries_6[i + 1] - 1
    age_indices <- which(ages >= age_start & ages <= age_end)
    
    spline_bin_total <- sum(spline_6bin_results[age_indices])
    pclm_bin_total <- sum(pclm_6bin_results[age_indices])
    
    cat(sprintf("  Bin %d (%d-%d): Original=%.1f, Spline=%.1f, PCLM=%.1f\n",
               i, age_start, age_end, counts_6bin[i], spline_bin_total, pclm_bin_total))
    cat(sprintf("    Spline diff=%.2f, PCLM diff=%.2f\n",
               counts_6bin[i] - spline_bin_total, counts_6bin[i] - pclm_bin_total))
}

# Generate comparison plots
cat("\n=== GENERATING COMPARISON PLOTS ===\n")
pdf("pclm_method_comparison.pdf", width = 14, height = 10)

# Set up 2x2 layout
layout(matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE))
ages <- 13:100

# Plot 1: Full range comparison
y_max <- max(c(spline_6bin_results, pclm_5bin_results, pclm_6bin_results)) * 1.1
plot(ages, spline_6bin_results, 
     type = "l", lwd = 2, col = "blue", lty = 2,
     main = "Age Distribution Methods Comparison",
     xlab = "Age", ylab = "Count per Year",
     xlim = c(13, 100), ylim = c(0, y_max))
lines(ages, pclm_5bin_results, col = "red", lwd = 3)
lines(ages, pclm_6bin_results, col = "darkgreen", lwd = 3, lty = 3)
abline(v = c(25, 35, 45, 55, 85), lty = 3, col = "lightgray")
legend("topright", 
       c("6-bin Hyman spline", "5-bin PCLM", "6-bin PCLM"),
       col = c("blue", "red", "darkgreen"), 
       lty = c(2, 1, 3), lwd = c(2, 3, 3))

# Plot 2: Tail behavior focus (ages 50-100)
tail_ages <- 50:100
tail_idx <- 50:100 - 13 + 1

plot(tail_ages, spline_6bin_results[tail_idx],
     type = "l", lwd = 2, col = "blue", lty = 2,
     main = "Tail Behavior Comparison (Ages 50-100)",
     xlab = "Age", ylab = "Count per Year")
lines(tail_ages, pclm_5bin_results[tail_idx], col = "red", lwd = 3)
lines(tail_ages, pclm_6bin_results[tail_idx], col = "darkgreen", lwd = 3, lty = 3)
abline(v = c(55, 85), lty = 1, col = "gray")
legend("topright",
       c("6-bin Hyman spline", "5-bin PCLM", "6-bin PCLM", "Data boundaries"),
       col = c("blue", "red", "darkgreen", "gray"), 
       lty = c(2, 1, 3, 1), lwd = c(2, 3, 3, 1))

# Plot 3: Key age comparison
key_ages <- c(50, 55, 60, 65, 70, 75, 80, 85, 90, 95)
key_idx <- key_ages - 13 + 1

plot(key_ages, spline_6bin_results[key_idx],
     type = "b", pch = 16, col = "blue", cex = 1.2,
     main = "PDF Values at Key Ages",
     xlab = "Age", ylab = "Count per Year",
     ylim = range(c(spline_6bin_results[key_idx], pclm_5bin_results[key_idx], 
                   pclm_6bin_results[key_idx])))
lines(key_ages, pclm_5bin_results[key_idx], type = "b", pch = 17, col = "red", cex = 1.2)
lines(key_ages, pclm_6bin_results[key_idx], type = "b", pch = 18, col = "darkgreen", cex = 1.2)
abline(v = c(55, 85), lty = 1, col = "gray")
legend("topright",
       c("6-bin spline", "5-bin PCLM", "6-bin PCLM"),
       col = c("blue", "red", "darkgreen"), 
       pch = c(16, 17, 18))

# Plot 4: Smoothness comparison
smooth_spline <- diff(diff(spline_6bin_results))
smooth_pclm_5 <- diff(diff(pclm_5bin_results))
smooth_pclm_6 <- diff(diff(pclm_6bin_results))
smooth_ages <- ages[2:(length(ages)-1)]

plot(smooth_ages, smooth_spline,
     type = "l", col = "blue", lwd = 2, lty = 2,
     main = "Smoothness Comparison (Second Differences)",
     xlab = "Age", ylab = "Second Difference",
     ylim = range(c(smooth_spline, smooth_pclm_5, smooth_pclm_6), na.rm = TRUE))
lines(smooth_ages, smooth_pclm_5, col = "red", lwd = 2)
lines(smooth_ages, smooth_pclm_6, col = "darkgreen", lwd = 2, lty = 3)
abline(h = 0, lty = 1, col = "gray")
abline(v = c(55, 85), lty = 1, col = "gray")
legend("topright",
       c("6-bin spline", "5-bin PCLM", "6-bin PCLM"),
       col = c("blue", "red", "darkgreen"), 
       lty = c(2, 1, 3), lwd = c(2, 2, 2))

dev.off()

# Summary statistics
cat("=== SUMMARY STATISTICS ===\n")
cat("Tail behavior assessment (mean PDF in ages 80-95):\n")
tail_critical <- 80:95 - 13 + 1
mean_spline_tail <- mean(spline_6bin_results[tail_critical])
mean_pclm_5_tail <- mean(pclm_5bin_results[tail_critical])
mean_pclm_6_tail <- mean(pclm_6bin_results[tail_critical])

cat("  6-bin Hyman spline:", round(mean_spline_tail, 1), "\n")
cat("  5-bin PCLM:        ", round(mean_pclm_5_tail, 1), "\n")
cat("  6-bin PCLM:        ", round(mean_pclm_6_tail, 1), "\n")

cat("\nSmoothing assessment (sum of absolute second differences):\n")
smoothness_spline <- sum(abs(smooth_spline), na.rm = TRUE)
smoothness_pclm_5 <- sum(abs(smooth_pclm_5), na.rm = TRUE)
smoothness_pclm_6 <- sum(abs(smooth_pclm_6), na.rm = TRUE)

cat("  6-bin spline:", round(smoothness_spline, 1), "\n")
cat("  5-bin PCLM:  ", round(smoothness_pclm_5, 1), "(", 
    round((smoothness_spline - smoothness_pclm_5)/smoothness_spline * 100, 1), "% smoother)\n")
cat("  6-bin PCLM:  ", round(smoothness_pclm_6, 1), "(", 
    round((smoothness_spline - smoothness_pclm_6)/smoothness_spline * 100, 1), "% smoother)\n")

cat("\nPlots saved to: pclm_method_comparison.pdf\n")
cat("Analysis complete.\n")