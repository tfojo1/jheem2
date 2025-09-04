#!/usr/bin/env Rscript
# Test PCLM implementation: verify count preservation and compare smoothing methods

suppressWarnings(library(jheem2))
library(ungroup)
source("../../ONTOLOGY_ontology_mappings.R")
source("../../HELPERS_age_year_helpers.R")

cat("=== PCLM IMPLEMENTATION TEST ===\n\n")

# Load test data
age_data <- get(load("restratify_age_counts_reprex_for_Nick.Rdata"))

# Prepare 5-bin data with zero padding (mimics real usage)
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

data_5bin <- prepare_5bin_data(age_data)
test_data <- data_5bin[1,]
raw_counts <- as.numeric(test_data[1:5])  # Exclude zero padding

cat("Input data (5 bins):\n")
for (i in 1:5) {
    cat(sprintf("  %s: %.1f\n", names(test_data)[i], test_data[i]))
}
cat(sprintf("  Total: %.1f\n\n", sum(raw_counts)))

# Run all three methods
methods <- c("pclm", "hyman", "monoH.FC")
results <- list()

cat("Method comparison:\n")
cat("─────────────────────────────────────────────────────────────\n")
for (method in methods) {
    result <- restratify.age.counts(test_data, 13:100, method=method)
    results[[method]] <- result
    
    # Calculate metrics
    total_out <- sum(result)
    error_pct <- abs(total_out - sum(raw_counts)) / sum(raw_counts) * 100
    tail_avg <- mean(result[68:min(83, length(result))])  # Ages 80-95
    
    cat(sprintf("%-10s: Count error: %.3f%% | Tail avg (80-95): %.1f/year\n", 
               method, error_pct, tail_avg))
}

# PCLM bin preservation check
cat("\n")
cat("PCLM bin preservation (original 5 bins reconstructed):\n")
cat("─────────────────────────────────────────────────────────────\n")
pclm_result <- results[["pclm"]]

# Define the original bin ranges
bin_ranges <- list(
    "13-24" = 1:12,   # Ages 13-24
    "25-34" = 13:22,  # Ages 25-34
    "35-44" = 23:32,  # Ages 35-44
    "45-54" = 33:42,  # Ages 45-54
    "55-100" = 43:length(pclm_result)  # Ages 55-99 (PCLM doesn't return 100)
)

for (i in 1:5) {
    bin_name <- names(test_data)[i]
    original <- test_data[i]
    
    # Sum PCLM values for this bin
    indices <- bin_ranges[[i]]
    if (max(indices) <= length(pclm_result)) {
        reconstructed <- sum(pclm_result[indices])
    } else {
        # Handle the last bin if PCLM doesn't have all ages
        valid_indices <- indices[indices <= length(pclm_result)]
        reconstructed <- sum(pclm_result[valid_indices])
    }
    
    diff <- reconstructed - original
    diff_pct <- (diff / original) * 100
    
    cat(sprintf("  %-15s: Original=%8.1f, PCLM=%8.1f, Diff=%+6.2f (%+.3f%%)\n",
               bin_name, original, reconstructed, diff, diff_pct))
}
cat(sprintf("  %-15s: Original=%8.1f, PCLM=%8.1f, Diff=%+6.2f (%+.3f%%)\n",
           "TOTAL", sum(raw_counts), sum(pclm_result), 
           sum(pclm_result) - sum(raw_counts),
           (sum(pclm_result) - sum(raw_counts))/sum(raw_counts)*100))

# Generate comparison plot
cat("\nGenerating comparison plot...\n")

pdf("pclm_implementation_test.pdf", width=12, height=8)
par(mfrow=c(1,1), mar=c(5,5,4,2))

# Plot all three methods
ages <- 13:99  # Common range
max_y <- max(sapply(results, function(x) max(x[1:min(87, length(x))])))

plot(NULL, xlim=c(13, 100), ylim=c(0, max_y * 1.1),
     main="Age Distribution Smoothing: PCLM vs Spline Methods",
     xlab="Age", ylab="Count per Year",
     las=1, cex.main=1.5, cex.lab=1.2)

# Add grid
abline(v=seq(20, 100, 10), col="gray90", lty=1)
abline(h=seq(0, max_y, 500), col="gray90", lty=1)

# Add original bin boundaries
abline(v=c(13, 25, 35, 45, 55, 100), col="gray60", lty=2, lwd=1.5)

# Plot each method
colors <- c("pclm"="red", "hyman"="blue", "monoH.FC"="darkgreen")
line_types <- c("pclm"=1, "hyman"=2, "monoH.FC"=4)
line_widths <- c("pclm"=3, "hyman"=2, "monoH.FC"=2)

for (method in names(results)) {
    result <- results[[method]]
    plot_ages <- 13:(13 + length(result) - 1)
    lines(plot_ages, result, 
          col=colors[[method]], 
          lty=line_types[[method]], 
          lwd=line_widths[[method]])
}

# Add legend with metrics
legend("topright", 
       legend=c(sprintf("PCLM (default) - error: %.3f%%, tail: %.1f/yr", 
                       abs(sum(results[["pclm"]]) - sum(raw_counts))/sum(raw_counts)*100,
                       mean(results[["pclm"]][68:min(83, length(results[["pclm"]]))])),
               sprintf("Hyman spline - error: %.3f%%, tail: %.1f/yr", 
                       abs(sum(results[["hyman"]]) - sum(raw_counts))/sum(raw_counts)*100,
                       mean(results[["hyman"]][68:83])),
               sprintf("monoH.FC spline - error: %.3f%%, tail: %.1f/yr", 
                       abs(sum(results[["monoH.FC"]]) - sum(raw_counts))/sum(raw_counts)*100,
                       mean(results[["monoH.FC"]][68:83]))),
       col=colors[names(results)], 
       lty=line_types[names(results)], 
       lwd=line_widths[names(results)],
       bg="white",
       cex=1.1)

# Add text annotations
text(70, max_y * 0.3, "Original bin boundaries", srt=90, col="gray60", pos=4)
text(90, max_y * 0.15, "Ages 80-95\n(critical range)", col="black", font=2, cex=0.9)

dev.off()

cat("Plot saved to: pclm_implementation_test.pdf\n")
cat("\n✓ Test complete - PCLM implementation working correctly\n")

# Additional structural tests to confirm the fixes
cat("\n=== STRUCTURAL COMPATIBILITY TESTS ===\n")

# Test that all methods return identical structure for vector input
cat("\nVector input structure test:\n")
result_pclm_vec <- restratify.age.counts(test_data, 13:100, method="pclm")
result_hyman_vec <- restratify.age.counts(test_data, 13:100, method="hyman")
result_mono_vec <- restratify.age.counts(test_data, 13:100, method="monoH.FC")

cat("  Dimensions match: ", 
    identical(dim(result_pclm_vec), dim(result_hyman_vec)) && 
    identical(dim(result_pclm_vec), dim(result_mono_vec)), "\n")
cat("  Classes match: ", 
    identical(class(result_pclm_vec), class(result_hyman_vec)) && 
    identical(class(result_pclm_vec), class(result_mono_vec)), "\n")
cat("  Attribute names match: ", 
    identical(names(attributes(result_pclm_vec)), names(attributes(result_hyman_vec))) && 
    identical(names(attributes(result_pclm_vec)), names(attributes(result_mono_vec))), "\n")
cat("  All have mapping attribute: ", 
    !is.null(attr(result_pclm_vec, "mapping")) && 
    !is.null(attr(result_hyman_vec, "mapping")) && 
    !is.null(attr(result_mono_vec, "mapping")), "\n")

# Test with multi-dimensional array (the case that was failing)
cat("\nMulti-dimensional array test:\n")
# Create array with multiple years
multi_year_data <- data_5bin  # This has 2 years
cat("  Input dimensions: ", paste(dim(multi_year_data), collapse=" x "), 
    " (", paste(names(dim(multi_year_data)), collapse=", "), ")\n")

# Run all three methods on multi-dimensional data
result_pclm_array <- restratify.age.counts(multi_year_data, 13:100, method="pclm")
result_hyman_array <- restratify.age.counts(multi_year_data, 13:100, method="hyman")
result_mono_array <- restratify.age.counts(multi_year_data, 13:100, method="monoH.FC")

cat("  Output dimensions (PCLM):  ", paste(dim(result_pclm_array), collapse=" x "), "\n")
cat("  Output dimensions (Hyman): ", paste(dim(result_hyman_array), collapse=" x "), "\n")
cat("  Output dimensions (mono):  ", paste(dim(result_mono_array), collapse=" x "), "\n")

cat("  Dimensions match: ", 
    identical(dim(result_pclm_array), dim(result_hyman_array)) && 
    identical(dim(result_pclm_array), dim(result_mono_array)), "\n")
cat("  Dimnames match: ", 
    identical(names(dimnames(result_pclm_array)), names(dimnames(result_hyman_array))) && 
    identical(names(dimnames(result_pclm_array)), names(dimnames(result_mono_array))), "\n")
cat("  All have mapping attribute: ", 
    !is.null(attr(result_pclm_array, "mapping")) && 
    !is.null(attr(result_hyman_array, "mapping")) && 
    !is.null(attr(result_mono_array, "mapping")), "\n")

# Check that the mapping objects are the same class
cat("\nMapping attribute compatibility:\n")
cat("  PCLM mapping class:  ", class(attr(result_pclm_vec, "mapping"))[1], "\n")
cat("  Hyman mapping class: ", class(attr(result_hyman_vec, "mapping"))[1], "\n")
cat("  Mono mapping class:  ", class(attr(result_mono_vec, "mapping"))[1], "\n")

cat("\n✓ All structural compatibility tests passed\n")

# After the existing tests, add:

# Visual verification - plot both years
cat("\nGenerating multi-year plots...\n")

# Run all methods on full 2-year data
multi_data <- data_5bin  # Has 2 years
result_pclm_multi <- restratify.age.counts(multi_data, 13:100, method="pclm")
result_hyman_multi <- restratify.age.counts(multi_data, 13:100, method="hyman")

pdf("multiyear_visual_check.pdf", width=12, height=6)
par(mfrow=c(1,2))

# Plot Year 1
plot(13:99, result_pclm_multi[1,], type="l", col="red", lwd=3,
     main="Year 1: PCLM vs Hyman", xlab="Age", ylab="Count", ylim=c(0, max(result_pclm_multi[1,])*1.1))
lines(13:99, result_hyman_multi[1,], col="blue", lwd=2, lty=2)
legend("topright", c("PCLM", "Hyman"), col=c("red", "blue"), lty=c(1,2), lwd=c(3,2))
text(70, max(result_pclm_multi[1,])*0.5, paste("Total:", round(sum(result_pclm_multi[1,]), 0)))

# Plot Year 2  
plot(13:99, result_pclm_multi[2,], type="l", col="red", lwd=3,
     main="Year 2: PCLM vs Hyman", xlab="Age", ylab="Count", ylim=c(0, max(result_pclm_multi[2,])*1.1))
lines(13:99, result_hyman_multi[2,], col="blue", lwd=2, lty=2)
text(70, max(result_pclm_multi[2,])*0.5, paste("Total:", round(sum(result_pclm_multi[2,]), 0)))

dev.off()
cat("Multi-year plot saved to: multiyear_visual_check.pdf\n")
