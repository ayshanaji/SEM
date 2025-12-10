# Structural Equation Modeling of Global Ecological Footprint

**Author:** Ayisha 
**Course:** STAT*6821 - Multivariate Statistical Methods  
**Institution:** University of Guelph  
**Date:** December 2024

---

## 📊 Project Overview

This repository contains a comprehensive Structural Equation Modeling (SEM) analysis investigating the interplay between human development, resource consumption (ecological footprint), biocapacity, and environmental sustainability across 139 countries.

### Research Questions

1. Does human development predict increased ecological footprint consumption?
2. How do consumption patterns relate to ecological balance outcomes?
3. What is the relative importance of ecological footprint versus biocapacity in determining sustainability?

### Key Findings

- **Development → Footprint**: Strong positive effect (γ = 0.72, p < .001, R² = 0.52)
- **Footprint → Ecological Reserve**: Negative effect (β = -0.34, p < .001)
- **Biocapacity → Ecological Reserve**: Dominant effect (β = 1.03, p < .001)
- **Model Fit**: Excellent (CFI = 0.945, TLI = 0.910)
- **Critical Insight**: Biocapacity's effect is **3× larger** than footprint's, indicating natural resource endowments currently constrain sustainability more than consumption patterns

---

## 📁 Repository Structure

```
SEM/
├── README.md                           # This file
├── R_script                    # Complete R analysis script
├── Report    # LaTeX report source
├── SEM_References.bib                  # BibTeX references
├── data/
│   └── ecological_footprint_2023.csv   # Dataset
├── figures/
│   ├── Figure1_Correlation_Heatmap.pdf
│   └── Figure2_SEM_Diagram_FINAL.pdf
├── output/
│   └── SEM_Project_Report.pdf          # Final compiled report

```

---

## 🔬 Methodology

### Statistical Framework
- **Method**: Structural Equation Modeling (SEM) with Confirmatory Factor Analysis (CFA)
- **Software**: R (lavaan package)
- **Estimator**: Maximum Likelihood with Robust Standard Errors (MLR)
- **Sample**: N = 139 countries

### Model Specification

**Latent Variables:**
- **DEVELOPMENT** (ξ₁): SDGi, Life Expectancy, HDI
- **FOOTPRINT** (η₁): Cropland Footprint, Carbon Footprint  
- **BIOCAPACITY** (ξ₂): Forest Land, Fishing Grounds

**Structural Equations:**
```
FOOTPRINT = γ₁₁ · DEVELOPMENT + ζ₁
Ecological Reserve = β₂₁ · FOOTPRINT + β₂₂ · BIOCAPACITY + ε
```

---

## 📦 Data Source

**Primary Source:** Global Footprint Network (2023). *National Footprint and Biocapacity Accounts, 2023 Edition*

**Access:** Dataset accessed via Kaggle compilation:  
[EDA on 2023 Global Ecological Footprint Data](https://www.kaggle.com/code/jainaru/eda-on-2023-global-ecological-footprint-data) (Jain, 2023)

**Variables:**
- Human Development Indicators: SDGi, Life Expectancy, HDI
- Ecological Footprint: Cropland, Carbon, Forest Product footprints
- Biocapacity: Forest Land, Fishing Grounds, Total Biocapacity
- Outcome: Ecological Reserve/Deficit (gha per capita)

---

## 🛠️ Requirements

### R Packages
```r
install.packages(c(
  "lavaan",      # SEM estimation
  "semPlot",     # SEM diagrams
  "psych",       # Descriptive statistics
  "corrplot",    # Correlation plots
  "ggplot2",     # Visualization
  "dplyr",       # Data manipulation
  "tidyr"        # Data reshaping
))
```

### LaTeX Requirements
- Full LaTeX distribution 
- Packages: `amsmath`, `graphicx`, `booktabs`, `natbib`, `geometry`, `float`, `titlesec`

---



**Last Updated:** December 2024
