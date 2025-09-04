library(jheem2)
library(ungroup)

# Quick analysis of PCLM CDF properties
age_data <- get(load(file = "restratify_age_counts_reprex_for_Nick.Rdata"))

# Prepare 5-bin data (original)
prepare_5bin <- function(arr) {
    arr_reordered <- apply(arr, c("year", "age"), function(x) x)
    new_dimnames <- dimnames(arr_reordered)
    new_dimnames$age[5] <- "55-100 years"
    new_dimnames$age <- c(new_dimnames$age, "101-110 years")
    arr_reordered <- c(arr_reordered, rep(0, dim(arr_reordered)["year"]))
    arr_restored <- array(arr_reordered, sapply(new_dimnames, length), new_dimnames)
    apply(arr_restored, c("year", "age"), function(x) x)
}

data_5bin <- prepare_5bin(age_data)
counts_5bin <- as.numeric(data_5bin[1, 1:5])

cat("=== PCLM CDF ANALYSIS ===\n")
cat("Comparing CDF properties: Spline vs PCLM\n\n")

# Fit both methods
cat("Fitting models...\n")

# Traditional spline approach (CDF-first)
endpoints <- c(13, 25, 35, 45, 55, 101)
obs_cum_n <- cumsum(counts_5bin)
spline_cdf_fn <- splinefun(x = endpoints, y = c(0, obs_cum_n), method = "hyman")

# PCLM approach (PDF-first)
pclm_model <- pclm(x = c(13, 25, 35, 45, 55), y = counts_5bin, nlast = 45, verbose = FALSE)
pclm_pdf <- fitted(pclm_model)

# Extract ages from PCLM interval names
interval_names <- names(pclm_pdf)
pclm_ages <- sapply(interval_names, function(x) as.numeric(gsub("\\[([0-9]+),.*", "\\1", x)))

# Create PCLM CDF by integration
pclm_cdf <- cumsum(pclm_pdf)
names(pclm_cdf) <- pclm_ages

cat("Model fitting complete.\n\n")

# Create fine-grained age sequences for comparison
ages_fine <- seq(13, 100, by = 0.1)
ages_int <- 13:100

# Evaluate spline CDF at fine resolution
spline_cdf_values <- spline_cdf_fn(ages_fine)
spline_pdf_values <- spline_cdf_fn(ages_int[-1]) - spline_cdf_fn(ages_int[-length(ages_int)])

# Map PCLM to integer ages
pclm_pdf_int <- numeric(length(ages_int))
pclm_cdf_int <- numeric(length(ages_int))
for (i in seq_along(ages_int)) {
    age <- ages_int[i]
    if (age %in% pclm_ages) {
        idx <- which(pclm_ages == age)[1]
        pclm_pdf_int[i] <- pclm_pdf[idx]
        pclm_cdf_int[i] <- pclm_cdf[idx]
    }
}

# Check monotonicity
cat("=== MONOTONICITY CHECK ===\n")

# Spline CDF monotonicity
spline_cdf_diffs <- diff(spline_cdf_values)
spline_monotonic <- all(spline_cdf_diffs >= 0)
cat("Spline CDF monotonic:", spline_monotonic, "\n")
if (!spline_monotonic) {
    cat("  Violations:", sum(spline_cdf_diffs < 0), "points\n")
    cat("  Min diff:", round(min(spline_cdf_diffs), 6), "\n")
}

# PCLM CDF monotonicity (should be guaranteed by construction)
pclm_cdf_diffs <- diff(pclm_cdf)
pclm_monotonic <- all(pclm_cdf_diffs >= 0)
cat("PCLM CDF monotonic:", pclm_monotonic, "\n")
if (!pclm_monotonic) {
    cat("  Violations:", sum(pclm_cdf_diffs < 0), "points\n")
    cat("  Min diff:", round(min(pclm_cdf_diffs), 6), "\n")
}

# Check endpoint values
cat("\n=== ENDPOINT ANALYSIS ===\n")
cat("Total counts:\n")
cat("  Original:", round(sum(counts_5bin), 2), "\n")
cat("  Spline PDF sum:", round(sum(spline_pdf_values), 2), "\n")
cat("  PCLM PDF sum:", round(sum(pclm_pdf), 2), "\n")

cat("\nCDF endpoint values:\n")
cat("  Spline CDF at 100:", round(spline_cdf_fn(100), 2), "\n")
cat("  PCLM CDF at end:", round(max(pclm_cdf), 2), "\n")

# Check smoothness in CDF
cat("\n=== CDF SMOOTHNESS (Second Derivatives) ===\n")
spline_cdf_smooth <- sum(abs(diff(diff(spline_cdf_values))), na.rm = TRUE)
pclm_cdf_smooth <- sum(abs(diff(diff(pclm_cdf))), na.rm = TRUE)

cat("CDF smoothness (sum abs 2nd derivatives):\n")
cat("  Spline CDF:", round(spline_cdf_smooth, 1), "\n")
cat("  PCLM CDF: ", round(pclm_cdf_smooth, 1), "\n")

# Create comparison plot
pdf("pclm_cdf_analysis.pdf", width = 12, height = 8)
layout(matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE))

# Plot 1: CDF Comparison
plot(ages_fine, spline_cdf_values, type = "l", lwd = 2, col = "blue",
     main = "CDF Comparison", xlab = "Age", ylab = "Cumulative Count")
lines(pclm_ages, pclm_cdf, col = "red", lwd = 2)
points(endpoints[-length(endpoints)], c(0, obs_cum_n[-length(obs_cum_n)]), 
       pch = 16, cex = 1.2, col = "black")
legend("bottomright", c("Spline CDF", "PCLM CDF", "Data Points"),
       col = c("blue", "red", "black"), lty = c(1, 1, NA), 
       pch = c(NA, NA, 16), lwd = 2)

# Plot 2: PDF Comparison  
plot(ages_int[-length(ages_int)], spline_pdf_values, type = "l", lwd = 2, col = "blue",
     main = "PDF Comparison", xlab = "Age", ylab = "Count per Year")
lines(pclm_ages, pclm_pdf, col = "red", lwd = 2)
legend("topright", c("Spline PDF", "PCLM PDF"), 
       col = c("blue", "red"), lwd = 2)

# Plot 3: CDF First Derivatives (PDFs)
spline_pdf_fine <- spline_cdf_fn(ages_fine[-1], deriv = 1)
plot(ages_fine[-1], spline_pdf_fine, type = "l", lwd = 2, col = "blue",
     main = "CDF First Derivatives", xlab = "Age", ylab = "PDF from CDF derivative")
lines(pclm_ages, pclm_pdf, col = "red", lwd = 2)
legend("topright", c("d/dx Spline CDF", "PCLM PDF"), 
       col = c("blue", "red"), lwd = 2)

# Plot 4: CDF Second Derivatives (Curvature)
spline_cdf_curv <- spline_cdf_fn(ages_fine, deriv = 2)
pclm_cdf_curv <- diff(diff(c(0, pclm_cdf)))  # Approximate 2nd derivative
plot(ages_fine, spline_cdf_curv, type = "l", lwd = 2, col = "blue",
     main = "CDF Second Derivatives (Curvature)", xlab = "Age", ylab = "Curvature")
if(length(pclm_cdf_curv) > 2) {
    lines(pclm_ages[2:(length(pclm_ages)-1)], pclm_cdf_curv, col = "red", lwd = 2)
}
abline(h = 0, lty = 2, col = "gray")
legend("topright", c("Spline CDF curvature", "PCLM CDF curvature"), 
       col = c("blue", "red"), lwd = 2)

dev.off()

cat("\n=== INTEGRATION IMPLICATIONS ===\n")
cat("For restratify.age.counts() integration:\n")
cat("1. PCLM naturally produces monotonic CDF via integration\n")
cat("2. Current interface expects smoother(lowers, uppers) function\n") 
cat("3. PCLM can provide this via: sum(pdf[lowers[i]:uppers[i]])\n")
cat("4. Count preservation guaranteed by construction\n")
cat("\nPlots saved to: pclm_cdf_analysis.pdf\n")