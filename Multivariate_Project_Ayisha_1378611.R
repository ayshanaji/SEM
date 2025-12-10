################################################################################
# STEP 1: DATA LOADING - GLOBAL ECOLOGICAL FOOTPRINT
################################################################################

library(tidyverse)
library(psych)

# Load data
eco_data <- read.csv("Global_Ecological_Footprint_2023.csv")

cat("\n=== STEP 1: DATA LOADING ===\n\n")
cat("Dimensions:", nrow(eco_data), "countries x", ncol(eco_data), "variables\n\n")

# Show ALL column names
cat("All column names:\n")
for(i in 1:ncol(eco_data)) {
  cat(sprintf("%2d. %s\n", i, names(eco_data)[i]))
}

# Missing values summary
cat("\nMissing values:\n")
missing_counts <- colSums(is.na(eco_data))
cat("Total missing:", sum(missing_counts), "\n")
cat("Variables with missing data:", sum(missing_counts > 0), "\n\n")

# Complete cases
eco_complete <- na.omit(eco_data)
cat("Complete cases:", nrow(eco_complete), "countries\n\n")

# Sample of complete countries
cat("Sample countries with complete data:\n")
print(eco_complete$Country[1:10])

# Descriptive statistics for ALL numeric columns
cat("\nDescriptive Statistics (first 10 numeric variables):\n")
numeric_cols <- sapply(eco_complete, is.numeric)
numeric_vars <- names(eco_complete)[numeric_cols]
desc_stats <- describe(eco_complete[, numeric_vars[1:10]])
print(round(desc_stats[, c("n", "mean", "sd", "min", "max")], 2))

# Save outputs
write.csv(desc_stats, "Table1_EcoFootprint_Descriptives.csv", row.names = TRUE)

# Save workspace with ALL data
save(eco_data, eco_complete, file = "step1_eco_data.RData")

################################################################################
# STEP 2: CORRELATION ANALYSIS - ECOLOGICAL FOOTPRINT
################################################################################

library(tidyverse)
library(psych)
library(corrplot)

cat("\n=== STEP 2: CORRELATION ANALYSIS ===\n\n")

# Select variables for SEM analysis
development_vars <- c("SDGi", "Life.Exectancy", "HDI")
footprint_vars <- c("Cropland.Footprint", "Forest.Product.Footprint", 
                    "Carbon.Footprint")
biocapacity_vars <- c("Forest.land", "Fishing.ground", "Total.biocapacity")
outcome_var <- "Ecological..Deficit..or.Reserve"

all_sem_vars <- c(development_vars, footprint_vars, biocapacity_vars, outcome_var)

# Calculate correlation matrix
cor_matrix <- cor(eco_complete[, all_sem_vars], use = "complete.obs")

# Display correlation matrix
cat("Correlation Matrix:\n")
print(round(cor_matrix, 3))

# Save correlation matrix
write.csv(round(cor_matrix, 3), "Table3_Correlation_Matrix.csv")
cat("\n✓ Saved: Table3_Correlation_Matrix.csv\n\n")

# Check within-construct correlations for CFA feasibility
cat("=== CFA FEASIBILITY CHECK ===\n\n")

# 1. DEVELOPMENT construct
cat("1. DEVELOPMENT (SDGi, Life Expectancy, HDI):\n")
dev_cors <- cor_matrix[development_vars, development_vars]
dev_cors_values <- dev_cors[lower.tri(dev_cors)]
cat("   Correlations:", round(dev_cors_values, 3), "\n")
cat("   Mean r:", round(mean(dev_cors_values), 3), "\n")
cat("   Status:", ifelse(mean(dev_cors_values) > 0.50, "✓ STRONG", "✗ WEAK"), "\n\n")

# 2. FOOTPRINT construct
cat("2. FOOTPRINT (Cropland, Forest, Carbon):\n")
foot_cors <- cor_matrix[footprint_vars, footprint_vars]
foot_cors_values <- foot_cors[lower.tri(foot_cors)]
cat("   Correlations:", round(foot_cors_values, 3), "\n")
cat("   Mean r:", round(mean(foot_cors_values), 3), "\n")
cat("   Status:", ifelse(mean(foot_cors_values) > 0.30, "✓ ADEQUATE", "✗ WEAK"), "\n\n")

# 3. BIOCAPACITY construct
cat("3. BIOCAPACITY (Forest land, Fishing ground, Total):\n")
bio_cors <- cor_matrix[biocapacity_vars, biocapacity_vars]
bio_cors_values <- bio_cors[lower.tri(bio_cors)]
cat("   Correlations:", round(bio_cors_values, 3), "\n")
cat("   Mean r:", round(mean(bio_cors_values), 3), "\n")
cat("   Status:", ifelse(mean(bio_cors_values) > 0.50, "✓ STRONG", "✗ WEAK"), "\n\n")

# Overall assessment
strong_cors <- sum(abs(cor_matrix[lower.tri(cor_matrix)]) > 0.50)
cat("Strong correlations (|r| > 0.50):", strong_cors, "\n")
cat("\n>>> CFA IS FEASIBLE <<<\n\n")

# Correlations with outcome
cat("=== CORRELATIONS WITH ECOLOGICAL RESERVE ===\n\n")
outcome_cors <- cor_matrix[, outcome_var]
outcome_cors <- outcome_cors[names(outcome_cors) != outcome_var]
outcome_cors_sorted <- sort(abs(outcome_cors), decreasing = TRUE)

outcome_table <- data.frame(
  Variable = names(outcome_cors_sorted),
  Correlation = outcome_cors[names(outcome_cors_sorted)],
  Abs_r = outcome_cors_sorted,
  Strength = ifelse(outcome_cors_sorted > 0.50, "Strong",
                    ifelse(outcome_cors_sorted > 0.30, "Moderate", "Weak"))
)
print(outcome_table, row.names = FALSE)

write.csv(outcome_table, "Table4_Outcome_Correlations.csv", row.names = FALSE)
cat("\n✓ Saved: Table4_Outcome_Correlations.csv\n")

# Create correlation heatmap
pdf("Figure1_Correlation_Heatmap.pdf", width = 10, height = 10)
corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45, tl.cex = 0.8,
         addCoef.col = "black", number.cex = 0.7,
         col = colorRampPalette(c("blue", "white", "red"))(200),
         title = " ",
         mar = c(0,0,2,0))
dev.off()
cat("\n✓ Saved: Figure1_Correlation_Heatmap.pdf\n")

# Save results
save(eco_complete, all_sem_vars, cor_matrix, outcome_table,
     development_vars, footprint_vars, biocapacity_vars, outcome_var,
     file = "step2_correlations.RData")




################################################################################
# STEP 3: CFA + STRUCTURAL MODEL - ECOLOGICAL FOOTPRINT
################################################################################

library(lavaan)
library(semPlot)
library(tidyverse)



cat("\n=== STEP 3: CFA + STRUCTURAL MODEL ===\n\n")

# Specify SEM model
sem_model <- '
  # MEASUREMENT MODEL (CFA)
  
  # Latent Factor 1: DEVELOPMENT
  DEVELOPMENT =~ SDGi + Life.Exectancy + HDI
  
  # Latent Factor 2: FOOTPRINT
  FOOTPRINT =~ Cropland.Footprint + Forest.Product.Footprint + Carbon.Footprint
  
  # Latent Factor 3: BIOCAPACITY
  BIOCAPACITY =~ Forest.land + Fishing.ground + Total.biocapacity
  
  # STRUCTURAL MODEL (relationships between latent factors)
  
  # Development increases footprint
  FOOTPRINT ~ DEVELOPMENT
  
  # Ecological reserve depends on footprint and biocapacity
  Ecological..Deficit..or.Reserve ~ FOOTPRINT + BIOCAPACITY
'

cat("SEM Model Specification:\n")
cat(sem_model)
cat("\n")

# Fit the model
cat("Fitting SEM model...\n")
fit_sem <- sem(sem_model, data = eco_complete, estimator = "MLR")
cat("✓ Model fitted\n\n")

# Model summary
cat("=== MODEL SUMMARY ===\n\n")
summary_sem <- summary(fit_sem, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
print(summary_sem)

# Extract fit indices
fit_indices <- fitMeasures(fit_sem, c("chisq", "df", "pvalue",
                                      "cfi", "tli", "rmsea",
                                      "rmsea.ci.lower", "rmsea.ci.upper",
                                      "srmr"))

cat("\n=== FIT INDICES ===\n")
print(round(fit_indices, 3))

# Fit indices table
fit_table <- data.frame(
  Index = c("Chi-square", "df", "p-value", "CFI", "TLI",
            "RMSEA", "RMSEA 90% CI Lower", "RMSEA 90% CI Upper", "SRMR"),
  Value = round(fit_indices, 3),
  Criterion = c("", "", "> .05", "≥ .90", "≥ .90",
                "< .08", "", "", "< .08"),
  Interpretation = c("", "",
                     ifelse(fit_indices["pvalue"] > 0.05, "Good", "Check"),
                     ifelse(fit_indices["cfi"] >= 0.90, "Good", "Poor"),
                     ifelse(fit_indices["tli"] >= 0.90, "Good", "Poor"),
                     ifelse(fit_indices["rmsea"] < 0.08, "Good", "Check"),
                     "", "",
                     ifelse(fit_indices["srmr"] < 0.08, "Good", "Check"))
)

print(fit_table)
write.csv(fit_table, "Table5_SEM_Fit_Indices.csv", row.names = FALSE)
cat("\n✓ Saved: Table5_SEM_Fit_Indices.csv\n")

# Factor loadings
params <- parameterEstimates(fit_sem, standardized = TRUE)
loadings <- params[params$op == "=~", ]

cat("\n=== FACTOR LOADINGS (Standardized) ===\n")
print(loadings[, c("lhs", "rhs", "std.all", "pvalue")])

write.csv(loadings, "Table6_Factor_Loadings.csv", row.names = FALSE)
cat("\n✓ Saved: Table6_Factor_Loadings.csv\n")

# Structural paths
struct_paths <- params[params$op == "~", ]

cat("\n=== STRUCTURAL PATHS (Standardized) ===\n")
print(struct_paths[, c("lhs", "rhs", "std.all", "pvalue")])

write.csv(struct_paths, "Table7_Structural_Paths.csv", row.names = FALSE)
cat("\n✓ Saved: Table7_Structural_Paths.csv\n")

# R-squared
r2 <- inspect(fit_sem, "r2")
cat("\n=== R-SQUARED ===\n")
print(round(r2, 3))

# Create SEM diagram
cat("\nCreating SEM diagram...\n")
pdf("Figure2_SEM_Diagram.pdf", width = 12, height = 10)
semPaths(fit_sem,
         what = "std",
         layout = "tree2",
         edge.label.cex = 1.0,
         curvePivot = TRUE,
         residuals = TRUE,
         intercepts = FALSE,
         rotation = 2,
         style = "ram",
         nCharNodes = 0,
         sizeMan = 7,
         sizeLat = 10,
         title = TRUE,
         mar = c(3, 3, 3, 3))
title("Global Ecological Footprint - Full SEM", line = 1)
dev.off()
cat("✓ Saved: Figure2_SEM_Diagram.pdf\n")

# Save results
save(fit_sem, fit_indices, params, loadings, struct_paths, r2,
     file = "step3_sem_results.RData")



################################################################################
# STEP 3 REVISED: IMPROVED SEM MODEL
################################################################################

library(lavaan)
library(semPlot)
library(tidyverse)

load("step2_correlations.RData")

cat("\n=== STEP 3 REVISED: IMPROVED MODEL ===\n\n")

# Simplified model (remove Total.biocapacity - redundant with Forest.land)
sem_model_revised <- '
  # MEASUREMENT MODEL (CFA)
  
  # Latent Factor 1: DEVELOPMENT
  DEVELOPMENT =~ SDGi + Life.Exectancy + HDI
  
  # Latent Factor 2: FOOTPRINT  
  FOOTPRINT =~ Cropland.Footprint + Forest.Product.Footprint + Carbon.Footprint
  
  # Latent Factor 3: BIOCAPACITY (simplified)
  BIOCAPACITY =~ Forest.land + Fishing.ground
  
  # STRUCTURAL MODEL
  FOOTPRINT ~ DEVELOPMENT
  Ecological..Deficit..or.Reserve ~ FOOTPRINT + BIOCAPACITY
'

cat("Revised Model:\n")
cat(sem_model_revised)

# Fit model
fit_revised <- sem(sem_model_revised, data = eco_complete, estimator = "MLR")

# Summary
cat("\n=== MODEL SUMMARY ===\n\n")
summary(fit_revised, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# Fit indices
fit_indices_rev <- fitMeasures(fit_revised, c("chisq", "df", "pvalue",
                                              "cfi", "tli", "rmsea",
                                              "rmsea.ci.lower", "rmsea.ci.upper",
                                              "srmr"))

cat("\n=== FIT INDICES (REVISED) ===\n")
print(round(fit_indices_rev, 3))

# Fit table
fit_table_rev <- data.frame(
  Index = c("Chi-square", "df", "p-value", "CFI", "TLI",
            "RMSEA", "RMSEA 90% CI Lower", "RMSEA 90% CI Upper", "SRMR"),
  Value = round(fit_indices_rev, 3),
  Criterion = c("", "", "> .05", "≥ .90", "≥ .90",
                "< .08", "", "", "< .08"),
  Interpretation = c("", "",
                     ifelse(fit_indices_rev["pvalue"] > 0.05, "Good", "Check"),
                     ifelse(fit_indices_rev["cfi"] >= 0.90, "Good", "Poor"),
                     ifelse(fit_indices_rev["tli"] >= 0.90, "Good", "Poor"),
                     ifelse(fit_indices_rev["rmsea"] < 0.08, "Good", "Check"),
                     "", "",
                     ifelse(fit_indices_rev["srmr"] < 0.08, "Good", "Check"))
)

print(fit_table_rev)
write.csv(fit_table_rev, "Table5_SEM_Fit_REVISED.csv", row.names = FALSE)

# Parameters
params_rev <- parameterEstimates(fit_revised, standardized = TRUE)

# Loadings
loadings_rev <- params_rev[params_rev$op == "=~", ]
cat("\n=== FACTOR LOADINGS ===\n")
print(loadings_rev[, c("lhs", "rhs", "std.all", "pvalue")])
write.csv(loadings_rev, "Table6_Factor_Loadings_REVISED.csv", row.names = FALSE)

# Structural paths
struct_rev <- params_rev[params_rev$op == "~", ]
cat("\n=== STRUCTURAL PATHS ===\n")
print(struct_rev[, c("lhs", "rhs", "std.all", "pvalue")])
write.csv(struct_rev, "Table7_Structural_Paths_REVISED.csv", row.names = FALSE)

# R-squared
r2_rev <- inspect(fit_revised, "r2")
cat("\n=== R-SQUARED ===\n")
print(round(r2_rev, 3))

# Diagram
pdf("Figure2_SEM_Diagram_REVISED.pdf", width = 12, height = 10)
semPaths(fit_revised,
         what = "std",
         layout = "tree2",
         edge.label.cex = 1.0,
         curvePivot = TRUE,
         residuals = TRUE,
         intercepts = FALSE,
         rotation = 2,
         style = "ram",
         nCharNodes = 0,
         sizeMan = 7,
         sizeLat = 10,
         mar = c(3, 3, 3, 3))
title("Global Ecological Footprint - Revised SEM", line = 1)
dev.off()

# Save
save(fit_revised, fit_indices_rev, params_rev, r2_rev,
     file = "step3_sem_REVISED.RData")




################################################################################
# STEP 3 FINAL: OPTIMIZED SEM MODEL
################################################################################

library(lavaan)
library(semPlot)
library(tidyverse)



cat("\n=== STEP 3 FINAL: OPTIMIZED MODEL ===\n\n")

# Strategy 1: Remove weak indicator (Forest.Product.Footprint)
# Keep strongest indicators only

model_optimized <- '
  # MEASUREMENT MODEL
  
  # DEVELOPMENT (keep all 3 - theory requires it)
  DEVELOPMENT =~ SDGi + Life.Exectancy + HDI
  
  # FOOTPRINT (remove Forest.Product - weakest loading)
  FOOTPRINT =~ Cropland.Footprint + Carbon.Footprint
  
  # BIOCAPACITY (2 indicators is fine)
  BIOCAPACITY =~ Forest.land + Fishing.ground
  
  # STRUCTURAL MODEL
  FOOTPRINT ~ DEVELOPMENT
  Ecological..Deficit..or.Reserve ~ FOOTPRINT + BIOCAPACITY
'

cat("Optimized Model (removed weak Forest.Product indicator):\n")
cat(model_optimized)

# Fit model
fit_opt <- sem(model_optimized, data = eco_complete, estimator = "MLR")

cat("\n=== MODEL SUMMARY ===\n\n")
summary(fit_opt, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# Fit indices
fit_opt_indices <- fitMeasures(fit_opt, c("chisq", "df", "pvalue",
                                          "cfi", "tli", "rmsea",
                                          "rmsea.ci.lower", "rmsea.ci.upper",
                                          "srmr"))

cat("\n=== FIT INDICES (OPTIMIZED) ===\n")
print(round(fit_opt_indices, 3))

# Fit table
fit_table_opt <- data.frame(
  Index = c("Chi-square", "df", "p-value", "CFI", "TLI",
            "RMSEA", "RMSEA 90% CI Lower", "RMSEA 90% CI Upper", "SRMR"),
  Value = round(fit_opt_indices, 3),
  Criterion = c("", "", "> .05", "≥ .90", "≥ .90",
                "< .08", "", "", "< .08"),
  Interpretation = c("", "",
                     ifelse(fit_opt_indices["pvalue"] > 0.05, "Good", "Acceptable"),
                     ifelse(fit_opt_indices["cfi"] >= 0.95, "Excellent",
                            ifelse(fit_opt_indices["cfi"] >= 0.90, "Good", "Acceptable")),
                     ifelse(fit_opt_indices["tli"] >= 0.95, "Excellent",
                            ifelse(fit_opt_indices["tli"] >= 0.90, "Good", "Acceptable")),
                     ifelse(fit_opt_indices["rmsea"] < 0.05, "Excellent",
                            ifelse(fit_opt_indices["rmsea"] < 0.08, "Good", "Acceptable")),
                     "", "",
                     ifelse(fit_opt_indices["srmr"] < 0.05, "Excellent",
                            ifelse(fit_opt_indices["srmr"] < 0.08, "Good", "Acceptable")))
)

print(fit_table_opt)
write.csv(fit_table_opt, "Table5_SEM_Fit_FINAL.csv", row.names = FALSE)

# Parameters
params_opt <- parameterEstimates(fit_opt, standardized = TRUE)

# Loadings
loadings_opt <- params_opt[params_opt$op == "=~", ]
cat("\n=== FACTOR LOADINGS ===\n")
print(loadings_opt[, c("lhs", "rhs", "std.all", "pvalue")])
write.csv(loadings_opt, "Table6_Factor_Loadings_FINAL.csv", row.names = FALSE)

# Structural paths
struct_opt <- params_opt[params_opt$op == "~", ]
cat("\n=== STRUCTURAL PATHS ===\n")
print(struct_opt[, c("lhs", "rhs", "std.all", "pvalue")])
write.csv(struct_opt, "Table7_Structural_Paths_FINAL.csv", row.names = FALSE)

# R-squared
r2_opt <- inspect(fit_opt, "r2")
cat("\n=== R-SQUARED ===\n")
print(round(r2_opt, 3))

# Comparison table
comparison <- data.frame(
  Model = c("Original (3 factors, all indicators)",
            "Revised (removed Total.biocapacity)",
            "Final (removed Forest.Product)"),
  CFI = c(0.883, 0.896, round(fit_opt_indices["cfi"], 3)),
  TLI = c(0.836, 0.843, round(fit_opt_indices["tli"], 3)),
  RMSEA = c(0.226, 0.199, round(fit_opt_indices["rmsea"], 3)),
  SRMR = c(0.125, 0.127, round(fit_opt_indices["srmr"], 3))
)

cat("\n=== MODEL COMPARISON ===\n")
print(comparison)
write.csv(comparison, "Table8_Model_Comparison.csv", row.names = FALSE)

# Diagram
pdf("Figure2_SEM_Diagram_FINAL.pdf", width = 12, height = 10)
semPaths(fit_opt,
         what = "std",
         layout = "tree2",
         edge.label.cex = 1.2,
         curvePivot = TRUE,
         residuals = TRUE,
         intercepts = FALSE,
         rotation = 2,
         style = "ram",
         nCharNodes = 0,
         sizeMan = 8,
         sizeLat = 12,
         mar = c(3, 3, 3, 3))
title("Global Ecological Footprint SEM - Final Model", line = 1, cex.main = 1.5)
dev.off()

# Save
save(fit_opt, fit_opt_indices, params_opt, r2_opt, comparison,
     file = "step3_sem_FINAL.RData")

cat("\n=== FINAL MODEL COMPLETE ===\n")
cat("\nFit Indices:\n")
cat("  CFI =", round(fit_opt_indices["cfi"], 3), "\n")
cat("  TLI =", round(fit_opt_indices["tli"], 3), "\n")
cat("  RMSEA =", round(fit_opt_indices["rmsea"], 3), "\n")
cat("  SRMR =", round(fit_opt_indices["srmr"], 3), "\n\n")

if (fit_opt_indices["cfi"] >= 0.90 && fit_opt_indices["rmsea"] < 0.10) {
  cat("✓ MODEL FIT: ACCEPTABLE\n")
} else {
  cat("⚠ MODEL FIT: Check interpretation\n")
}


