# Mercury Levels in the Caribbean – Analysis Script

**Author:** Julia Saltzman  
**For:** Dr. Catherine Macdonald, University of Miami  

This repository contains an R script for analyzing mercury (Hg) levels in human populations across the Caribbean. The script uses statistical analysis and visualization to explore how mercury concentrations vary by geography, demographic factors, health conditions, symptoms, and dietary habits.

---

## 📄 Data
The script expects a CSV file named `Caribbean.csv` containing the following types of variables:
- **Mercury concentration** (`Hg..ppm.`)
- **Demographics:** `COUNTRY`, `GENDER`, `AGE`, `EDUCATION`
- **Health conditions:** e.g., `CANCER`, `HEART`, `IMMUNE`, etc.
- **Symptoms:** e.g., `BREATHING`, `MEMORY`, `TREMOR`, etc.
- **Seafood consumption:** Monthly frequency of specific species (e.g., `Tuna.month`, `Snapper.month`)

---

## 📊 Analyses Included

### 1. Mercury Levels by Country
- Summary statistics: mean, standard deviation, standard error
- Bar plot with error bars
- **ANOVA** and **Tukey's HSD** post-hoc comparisons

### 2. Mercury Levels by Gender
- Similar summary and plotting as above
- **ANOVA** and **Tukey's HSD** tests

### 3. Health Conditions
- Summarizes presence of various health conditions (`Yes` / `No` / `Unsure`)
- Bar plots of mean mercury levels for each condition
- Removes rows with missing or uncertain data

### 4. Symptoms
- Converts symptom counts to binary (`Yes` if ≥1, else `No`)
- Performs **t-tests** for differences in mercury levels between `Yes` and `No` groups
- Plots with significance markers (`*`, `**`, `***`)

### 5. Seafood Consumption
- **Linear regression** of mercury levels vs total monthly fish consumption
- Bar plots comparing `Yes` vs `No` consumption for each species
- Grouped analysis across all species

### 6. Age & Gender Interaction
- Aggregated mean and SE by age group and gender
- **Two-way ANOVA** with post-hoc Tukey's tests
- Bar plot with grouped error bars

### 7. Mixed-Effects Models
- Models include fixed effects for age, gender, and total fish consumption
- Random effects for `COUNTRY` and `COLLECTION.SITE`
- Implemented with `lme4` and `lmerTest`

### 8. Trophic Level Consumption
- Separates shark consumption from other fish
- Examines marine mammal consumption (porpoise, whale, blackfish)
- Scatterplots with regression lines

### 9. Education Level
- Boxplots of mercury levels by education category

---

## 📦 Required R Packages
The script uses:
```r
library(ggplot2)
library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest)
library(car)
library(DescTools)
