# R SCRIPT FOR MERCURY LEVELS IN THE CARRIBBEAN 
# MADE BY JULIA SALTZMAN FOR CATHERINE MACDONALD
# SET UP AND READ IN DATA  ------------------------------------------------


# libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(lme4)
# MERCURY LEVELS BY COUNTRY  ----------------------------------------------
df<- read.csv("Caribbean.csv")


# Summarize mercury levels by location (COUNTRY)
df_location <- df %>%
  group_by(COUNTRY) %>%
  summarize(mean_mercury = mean(Hg..ppm., na.rm = TRUE),
            sd_mercury = sd(Hg..ppm., na.rm = TRUE),
            n = n()) %>%
  mutate(se_mercury = sd_mercury / sqrt(n))

# Create the bar plot with error bars for mercury levels by location
ggplot(df_location, aes(x = COUNTRY, y = mean_mercury)) +
  geom_bar(stat = "identity", width = 0.7, fill = "#B8D3D9") +
  geom_errorbar(aes(ymin = mean_mercury - se_mercury, ymax = mean_mercury + se_mercury),
                width = 0.25) +
  labs(title = "Mercury Levels by Country",
       x = "Country",
       y = "Mean Mercury Level (ppm)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Increase x-axis text size
        axis.text.y = element_text(size = 14),  # Increase y-axis text size
        axis.title.x = element_text(size = 16),  # Increase x-axis title size
        axis.title.y = element_text(size = 16),  # Increase y-axis title size
        plot.title = element_text(size = 18, face = "bold"))  # Increase plot title size


# Perform ANOVA
anova_result <- aov( Hg..ppm.~ COUNTRY, data = df)

# Display ANOVA table
summary(anova_result)

# If ANOVA is significant, perform post-hoc Tukey's HSD test
TukeyHSD(anova_result)






# MERCURY LEVELS BY GENDER  -----------------------------------------------


# Filter out rows with missing mercury levels or gender and remove "Not Stated" gender
df_gender <- df %>%
  filter(!is.na(Hg..ppm.), !is.na(GENDER), GENDER != "Not stated")

# Summarize mercury levels by gender
df_summary_gender <- df_gender %>%
  group_by(GENDER) %>%
  summarize(mean_mercury = mean(Hg..ppm., na.rm = TRUE),
            sd_mercury = sd(Hg..ppm., na.rm = TRUE),
            n = n()) %>%
  mutate(se_mercury = sd_mercury / sqrt(n))

# Create the bar plot with error bars for mercury levels by gender
ggplot(df_summary_gender, aes(x = GENDER, y = mean_mercury)) +
  geom_bar(stat = "identity", width = 0.7, fill = "#B8D3D9") +
  geom_errorbar(aes(ymin = mean_mercury - se_mercury, ymax = mean_mercury + se_mercury),
                width = 0.25) +
  labs(
       x = "Gender",
       y = "Mean Mercury Level (ppm)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Increase x-axis text size
        axis.text.y = element_text(size = 14),  # Increase y-axis text size
        axis.title.x = element_text(size = 16),  # Increase x-axis title size
        axis.title.y = element_text(size = 16),  # Increase y-axis title size
        plot.title = element_text(size = 18, face = "bold"))  # Increase plot title size

# Perform ANOVA to test for differences in mercury levels by gender
anova_result <- aov(Hg..ppm. ~ GENDER, data = df_gender)

# Display ANOVA table
summary(anova_result)

# If ANOVA is significant, perform post-hoc Tukey's HSD test
TukeyHSD(anova_result)

# CONDITIONS  -------------------------------------------------------------

# Summarize 

# List of health conditions to summarize
conditions <- c("CANCER", "DEVELOPMENT", "HEART", "IMMUNE", "KIDNEY", 
                "PARKINSON", "OTHER.NEURO", "REPRODUCTIVE")

# Function to summarize the counts of 'Yes', 'No', 'Unsure', and blanks for each condition
summarize_conditions <- function(df, conditions) {
  # Loop through each condition and count 'Yes', 'No', 'Unsure', and blanks
  summary_list <- lapply(conditions, function(condition) {
    # Get counts for each value in the condition column
    df %>%
      summarize(
        Yes = sum(get(condition) == "Yes", na.rm = TRUE),
        No = sum(get(condition) == "No", na.rm = TRUE),
        Unsure = sum(get(condition) == "Unsure", na.rm = TRUE),
        Blank = sum(get(condition) == "", na.rm = TRUE),
        Total = n()  # Total number of entries in the column
      ) %>%
      mutate(Category = condition)
  })
  
  # Combine the results into one dataframe
  summary_df <- bind_rows(summary_list)
  return(summary_df)
}

# Call the function to summarize the conditions in your original dataframe
condition_summary <- summarize_conditions(df, conditions)

# Display the summary
print(condition_summary)


# List of health conditions to visualize
conditions <- c("CANCER", "DEVELOPMENT", "HEART", "IMMUNE", "KIDNEY", 
                "PARKINSON", "OTHER.NEURO", "REPRODUCTIVE")

# Clean the data: replace "Unsure" with NA and remove rows with blanks or NA in the relevant columns
df_clean <- df %>%
  filter(!is.na(Hg..ppm.)) %>%                      # Remove rows with missing mercury levels
  mutate(across(all_of(conditions), ~ ifelse(. == "Unsure" | . == "", NA, .))) %>%  # Convert "Unsure" and blanks to NA
  filter(complete.cases(.[conditions]))   # Remove rows where any condition column is NA or blank

# Function to create plots for each condition
plot_mercury_by_condition <- function(condition) {
  # Summarize data: calculate mean and standard error of mercury levels by condition
  df_summary <- df_clean %>%
    group_by(across(all_of(condition))) %>%
    summarize(mean_mercury = mean(Hg..ppm., na.rm = TRUE),
              sd_mercury = sd(Hg..ppm., na.rm = TRUE),
              n = n()) %>%
    mutate(se_mercury = sd_mercury / sqrt(n))
  
  # Create the bar plot with error bars for mercury levels by condition
  ggplot(df_summary, aes_string(x = condition, y = "mean_mercury")) +
    geom_bar(stat = "identity", width = 0.7, fill = "#B8D3D9") +
    geom_errorbar(aes(ymin = mean_mercury - se_mercury, ymax = mean_mercury + se_mercury),
                  width = 0.25) +
    labs(title = paste("Mean Mercury Levels by", condition, "Condition"),
         x = paste(condition, "Condition"),
         y = "Mean Mercury Level (ppm)") +
    theme_classic() +
    theme(axis.text.x = element_text(size = 14),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          plot.title = element_text(size = 18, face = "bold"))
}

# Loop through each condition and generate the plots
for (condition in conditions) {
  print(plot_mercury_by_condition(condition))
}


# SYMPTOMS ----------------------------------------------------------------

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Read the data
data <- read.csv("Caribbean.csv")

# Define the symptom columns
symptoms <- c("BREATHING", "HEARING", "WALKING", "EMOTION", "HEADACHE", "INSOMNIA", 
              "COORDNATION", "PERIPHERAL.VISION", "MEMORY", "TWITCH", "WEAKNESS", 
              "RASH", "SPEECH", "TINGLING", "TREMOR")

# Convert the symptom columns to "Yes" (if >=1) or "No" (if 0)
data[symptoms] <- lapply(data[symptoms], function(x) ifelse(x >= 1, "Yes", "No"))

# Remove rows where any of the symptoms are missing or empty data
data <- data %>% filter(complete.cases(data[symptoms]))

# Perform t-tests for each symptom and store the p-values
t_test_results <- lapply(symptoms, function(symptom) {
  # Filter data for the current symptom
  yes_group <- data %>% filter(!!sym(symptom) == "Yes") %>% pull(Hg..ppm.)
  no_group <- data %>% filter(!!sym(symptom) == "No") %>% pull(Hg..ppm.)
  
  # Perform the t-test
  t_test <- t.test(yes_group, no_group)
  
  # Return the results (mean values, p-value, and significance markers)
  significance <- ifelse(t_test$p.value < 0.001, "***",
                         ifelse(t_test$p.value < 0.01, "**",
                                ifelse(t_test$p.value < 0.05, "*", "")))
  
  return(data.frame(
    Symptom = symptom,
    Mean_Mercury_Yes = mean(yes_group, na.rm = TRUE),
    Mean_Mercury_No = mean(no_group, na.rm = TRUE),
    P_Value = t_test$p.value,
    Significance = significance
  ))
})

# Combine all results into a data frame
t_test_results_df <- do.call(rbind, t_test_results)

# Reshape the data to long format for easier graphing
long_data_symptoms <- data %>%
  select(Hg..ppm., all_of(symptoms)) %>%
  gather(key = "Symptom", value = "Present", -Hg..ppm.) %>%
  group_by(Symptom, Present) %>%
  summarise(mean_mercury = mean(Hg..ppm., na.rm = TRUE),
            se_mercury = sd(Hg..ppm., na.rm = TRUE) / sqrt(n())) %>%
  ungroup()

# Merge the t-test results with the long data for plotting
long_data_symptoms <- long_data_symptoms %>%
  left_join(t_test_results_df, by = c("Symptom"))

# Plot the mercury levels by symptom presence (Yes/No) with error bars and significance markers
ggplot(long_data_symptoms, aes(x = Present, y = mean_mercury, fill = Present)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_mercury - se_mercury, ymax = mean_mercury + se_mercury),
                width = 0.2, position = position_dodge(0.9)) +
  facet_wrap(~ Symptom, scales = "free_y") +
  geom_text(aes(x = 1.5, y = max(mean_mercury + se_mercury), label = Significance),
            inherit.aes = FALSE, data = long_data_symptoms, vjust = -1, size = 5, fontface = "bold") +  # Add significance markers
  labs(
       x = "Symptom Presence",
       y = "Mean Mercury Levels (ppm)") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove the legend

# Save the plot
ggsave("mercury_symptom_yes_no_with_significance.png", width = 12, height = 8)



# Load necessary libraries
library(ggplot2)
library(dplyr)

# Read the data
data <- read.csv("Caribbean.csv")

# Define the symptom columns
symptoms <- c("BREATHING", "HEARING", "WALKING", "EMOTION", "HEADACHE", "INSOMNIA", 
              "COORDNATION", "PERIPHERAL.VISION", "MEMORY", "TWITCH", "WEAKNESS", 
              "RASH", "SPEECH", "TINGLING", "TREMOR")

# Convert the symptom columns to "Yes" (if >=1) or "No" (if 0)
data[symptoms] <- lapply(data[symptoms], function(x) ifelse(x >= 1, "Yes", "No"))

# Remove rows where any of the symptoms are missing or empty data
data <- data %>% filter(complete.cases(data[symptoms]))

# Perform t-tests for each symptom
t_test_results <- lapply(symptoms, function(symptom) {
  # Filter data for the current symptom
  yes_group <- data %>% filter(!!sym(symptom) == "Yes") %>% pull(Hg..ppm.)
  no_group <- data %>% filter(!!sym(symptom) == "No") %>% pull(Hg..ppm.)
  
  # Perform the t-test
  t_test <- t.test(yes_group, no_group)
  
  # Return the results (mean values, p-value, and other stats)
  return(data.frame(
    Symptom = symptom,
    Mean_Mercury_Yes = mean(yes_group, na.rm = TRUE),
    Mean_Mercury_No = mean(no_group, na.rm = TRUE),
    P_Value = t_test$p.value
  ))
})

# Combine all results into a data frame
t_test_results_df <- do.call(rbind, t_test_results)

# Print the t-test results
print(t_test_results_df)

# Save the results to a CSV file if needed
write.csv(t_test_results_df, "t_test_mercury_symptoms_results.csv", row.names = FALSE)

# MERCURY BY CONSUMPTION --------------------------------------------


# Load necessary libraries
library(ggplot2)
library(dplyr)

# Read the data
data <- read.csv("Caribbean.csv")

# Convert fish consumption columns to numeric (replace missing values with 0)
fish_columns <- grep("month", names(data), value = TRUE)
data[fish_columns] <- lapply(data[fish_columns], function(x) as.numeric(as.character(x)))
data[fish_columns][is.na(data[fish_columns])] <- 0

# Create a new column summing up all the fish consumption values
data$Total_Fish_Consumption <- rowSums(data[fish_columns])

# Perform a linear regression: Mercury level vs Total Fish Consumption
lm_model <- lm(Hg..ppm. ~ Total_Fish_Consumption, data = data)
summary(lm_model)

# Plot the linear regression of Mercury levels vs Total Fish Consumption
ggplot(data, aes(x = Total_Fish_Consumption, y = Hg..ppm.)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "blue", size = 0.8) +
  labs(title = "Mercury Levels by Total Fish Consumption",
       x = "Total Fish Consumption (Times per Month)",
       y = "Mercury Levels (ppm)") +
  theme_minimal()

# Save the plot
ggsave("mercury_total_fish_consumption.png", width = 8, height = 6)

# Yes/No Consumption ------------------------------------------------------

# Convert fish consumption columns to numeric (replace missing values with 0)
fish_columns <- grep("month", names(data), value = TRUE)
data[fish_columns] <- lapply(data[fish_columns], function(x) as.numeric(as.character(x)))
data[fish_columns][is.na(data[fish_columns])] <- 0

# Create binary columns for each fish species, indicating whether the fish is consumed
for (fish in fish_columns) {
  data[[paste(fish, "Consumed", sep = "_")]] <- ifelse(data[[fish]] > 0, "Yes", "No")
}

# Melt the data into long format to group by fish species and consumption status
long_data <- data %>%
  select(Hg..ppm., ends_with("Consumed")) %>%
  gather(key = "Fish_Consumed", value = "Consumed", -Hg..ppm.) %>%
  mutate(Fish = gsub("_Consumed", "", Fish_Consumed))  # Clean up the column names

# Remove the ".month" suffix from fish names
long_data$Fish <- gsub(".month", "", long_data$Fish)

# Calculate mean mercury levels and standard error for "Yes" and "No" consumption
mean_mercury_levels <- long_data %>%
  group_by(Fish, Consumed) %>%
  summarise(
    mean_mercury = mean(Hg..ppm., na.rm = TRUE),
    se_mercury = sd(Hg..ppm., na.rm = TRUE) / sqrt(n())
  )

# Plot the mercury levels by consumption status for each fish species with error bars
ggplot(mean_mercury_levels, aes(x = Consumed, y = mean_mercury, fill = Consumed)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_mercury - se_mercury, ymax = mean_mercury + se_mercury),
                width = 0.2, position = position_dodge(0.9)) +
  facet_wrap(~ Fish, scales = "free") +
  labs(title = "Mercury Levels by Fish Consumption",
       x = "Fish Consumption",
       y = "Mean Mercury Levels (ppm)") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove the legend again

# Save the plot
ggsave("mercury_fish_consumption_yes_no_error_bars_cleaned_labels.png", width = 12, height = 8)


# Grouped Consumption -----------------------------------------------------



# Convert fish consumption columns to numeric (replace missing values with 0)
fish_columns <- grep("month", names(data), value = TRUE)
data[fish_columns] <- lapply(data[fish_columns], function(x) as.numeric(as.character(x)))
data[fish_columns][is.na(data[fish_columns])] <- 0

# Create binary columns for each fish species, indicating whether the fish is consumed
for (fish in fish_columns) {
  data[[paste(fish, "Consumed", sep = "_")]] <- ifelse(data[[fish]] > 0, "Yes", "No")
}

# Melt the data into long format to group by fish species and consumption status
long_data <- data %>%
  select(Hg..ppm., ends_with("Consumed")) %>%
  gather(key = "Fish_Consumed", value = "Consumed", -Hg..ppm.) %>%
  mutate(Fish = gsub("_Consumed", "", Fish_Consumed))  # Clean up the column names

# Remove the ".month" suffix from fish names
long_data$Fish <- gsub(".month", "", long_data$Fish)

# Calculate mean mercury levels and standard error for "Yes" and "No" consumption, grouped across all fish
mean_mercury_levels_grouped <- long_data %>%
  group_by(Consumed) %>%
  summarise(
    mean_mercury = mean(Hg..ppm., na.rm = TRUE),
    se_mercury = sd(Hg..ppm., na.rm = TRUE) / sqrt(n())
  )

# Plot the mercury levels by consumption status, grouped across all fish species, with error bars
ggplot(mean_mercury_levels_grouped, aes(x = Consumed, y = mean_mercury, fill = Consumed)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_mercury - se_mercury, ymax = mean_mercury + se_mercury),
                width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Mean Mercury Levels by Seafood Consumption Across All Species",
       x = "Seafood Consumption (Yes/No)",
       y = "Mean Mercury Levels (ppm)") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove the legend

# Save the plot
ggsave("mercury_fish_consumption_grouped_error_bars.png", width = 8, height = 6)

# consumption by age ------------------------------------------------------
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Read the data
data <- read.csv("Caribbean.csv")

# Ensure that "AGE" and "GENDER" columns are factors for easier grouping
data$AGE <- as.factor(data$AGE)
data$GENDER <- as.factor(data$GENDER)

# Remove rows with missing or invalid mercury, age, gender, and remove "Not stated" age/gender
data <- data %>%
  filter(complete.cases(Hg..ppm., AGE, GENDER)) %>%
  filter(AGE != "Not stated", GENDER != "Not stated")

# Combine "70-79" and "80-89" into one category "70+"
data$AGE <- ifelse(data$AGE %in% c("70-79", "80-89"), "70+", as.character(data$AGE))

# Convert AGE back to factor with ordered levels
data$AGE <- factor(data$AGE, levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70+"))

# Calculate mean mercury levels and standard error for each age group and gender
mean_mercury_age_gender <- data %>%
  group_by(AGE, GENDER) %>%
  summarise(mean_mercury = mean(Hg..ppm., na.rm = TRUE),
            se_mercury = sd(Hg..ppm., na.rm = TRUE) / sqrt(n())) %>%
  ungroup()

# Plot the mercury levels by age group and gender with error bars
ggplot(mean_mercury_age_gender, aes(x = AGE, y = mean_mercury, fill = GENDER)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_mercury - se_mercury, ymax = mean_mercury + se_mercury),
                width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Mean Mercury Levels by Age Group and Gender",
       x = "Age Group",
       y = "Mean Mercury Levels (ppm)",
       fill = "Gender") +
  theme_classic() 

# Save the plot
ggsave("mercury_age_gender_filtered_error_bars_70plus.png", width = 10, height = 6)


# stats consumption by age  -----------------------------------------------

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(car)         # For ANOVA
library(DescTools)   # For post-hoc Tukey test

# Read the data
data <- read.csv("Caribbean.csv")

# Ensure that "AGE" and "GENDER" columns are factors for easier grouping
data$AGE <- as.factor(data$AGE)
data$GENDER <- as.factor(data$GENDER)

# Remove rows with missing or invalid mercury, age, gender, and remove "Not stated" age/gender
data <- data %>%
  filter(complete.cases(Hg..ppm., AGE, GENDER)) %>%
  filter(AGE != "Not stated", GENDER != "Not stated")

# Combine "70-79" and "80-89" into one category "70+"
data$AGE <- ifelse(data$AGE %in% c("70-79", "80-89"), "70+", as.character(data$AGE))

# Convert AGE back to factor with ordered levels
data$AGE <- factor(data$AGE, levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70+"))

# Perform a two-way ANOVA for Age and Gender on mercury levels
anova_result <- aov(Hg..ppm. ~ AGE * GENDER, data = data)
summary(anova_result)

# Perform post-hoc Tukey's HSD test if ANOVA shows significant results
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

# Get summary statistics for each group (mean, standard deviation, and count)
summary_stats <- data %>%
  group_by(AGE, GENDER) %>%
  summarise(
    mean_mercury = mean(Hg..ppm., na.rm = TRUE),
    sd_mercury = sd(Hg..ppm., na.rm = TRUE),
    count = n()
  )

# Print the summary statistics
print(summary_stats)

# Save the ANOVA results and summary statistics to a file
write.csv(summary_stats, "summary_stats_age_gender.csv")
write.csv(anova_result, "anova_results_age_gender.csv")


# Mixed Effect Model (Gender and Age) -----------------------------------------------------
# MIXED EFFECT MODEL (DIET) -----------------------------------------------


library(lme4)


# Read the data
data <- read.csv("Caribbean.csv")

# Ensure that "AGE" and "GENDER" columns are factors for easier grouping
data$AGE <- as.factor(data$AGE)
data$GENDER <- as.factor(data$GENDER)

# Remove rows with missing or invalid mercury, age, gender, and remove "Not stated" age/gender
data <- data %>%
  filter(complete.cases(Hg..ppm., AGE, GENDER)) %>%
  filter(AGE != "Not stated", GENDER != "Not stated")

# Combine "70-79" and "80-89" into one category "70+"
data$AGE <- ifelse(data$AGE %in% c("70-79", "80-89"), "70+", as.character(data$AGE))

# Convert AGE back to factor with ordered levels
data$AGE <- factor(data$AGE, levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70+"))

# Mixed-effects model: Fixed effects for age and gender, random effects for country and collection site
# Here we assume random effects for country and collection site
mixed_model <- lmer(Hg..ppm. ~ AGE * GENDER + (1 | COUNTRY) + (1 | `COLLECTION.SITE`), data = data)

# Display summary of the mixed model
summary(mixed_model)

# Check for significance of fixed effects (using likelihood ratio tests)
anova(mixed_model)

# Optionally, check random effects
ranef(mixed_model)

# Save the model summary
capture.output(summary(mixed_model), file = "mixed_effect_model_summary.txt")


# Load necessary libraries
# Load necessary libraries
library(lme4)
library(lmerTest)

# Read the data
data <- read.csv("Caribbean.csv")

# Ensure that "AGE" and "GENDER" columns are factors for easier grouping
data$AGE <- as.factor(data$AGE)
data$GENDER <- as.factor(data$GENDER)

# Remove rows with missing or invalid mercury, age, gender, and remove "Not stated" age/gender
data <- data %>%
  filter(complete.cases(Hg..ppm., AGE, GENDER)) %>%
  filter(AGE != "Not stated", GENDER != "Not stated")

# Combine "70-79" and "80-89" into one category "70+"
data$AGE <- ifelse(data$AGE %in% c("70-79", "80-89"), "70+", as.character(data$AGE))

# Convert AGE back to factor with ordered levels
data$AGE <- factor(data$AGE, levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70+"))

# Get all fish consumption columns (they contain "month" in the name)
fish_columns <- grep("month", names(data), value = TRUE)

# Convert fish consumption columns to numeric (if they aren't already)
data[fish_columns] <- lapply(data[fish_columns], function(x) as.numeric(as.character(x)))

# Create a new variable for total fish consumption by summing relevant columns
data$Total_Fish_Consumption <- rowSums(data[fish_columns], na.rm = TRUE)

# Mixed-effects model: Fixed effects for age, gender, and total fish consumption
# Random effects for country and collection site
mixed_model_fish <- lmer(Hg..ppm. ~ AGE * GENDER + Total_Fish_Consumption + (1 | COUNTRY) + (1 | `COLLECTION.SITE`), data = data)

# Display summary of the mixed model with fish consumption
summary(mixed_model_fish)

# Check for significance of fixed effects (using likelihood ratio tests)
anova(mixed_model_fish)

# Optionally, check random effects
ranef(mixed_model_fish)

# Save the model summary
capture.output(summary(mixed_model_fish), file = "mixed_effect_model_with_fish_summary.txt")


# TROPHIC LEBELS  ---------------------------------------------------------


# Get all fish consumption columns (they contain "month" in the name)
fish_columns <- grep("month", names(data), value = TRUE)

# Separate shark consumption from the rest of the fish
fish_columns_no_shark <- setdiff(fish_columns, c("Shark.month"))

# Combine all fish (except shark) into a single column
data$Total_Fish_Excluding_Shark <- rowSums(data[fish_columns_no_shark], na.rm = TRUE)

# Combine marine mammals into a single column (assuming columns like "Porpoise", "Whale")
marine_mammal_columns <- c("Porpoise.month", "Whale.month", "Blackfish.month")  # Adjust this list based on your dataset
data$Total_Marine_Mammals <- rowSums(data[marine_mammal_columns], na.rm = TRUE)

# Visualize the total fish (excluding shark) and marine mammals consumption in relation to mercury levels

# Plot Total Fish (excluding shark) vs Mercury Levels
ggplot(data, aes(x = Total_Fish_Excluding_Shark, y = Hg..ppm.)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "blue") +
  labs(title = "Mercury Levels vs Fish Consumption (Excluding Shark)",
       x = "Total Fish Consumption (Excluding Shark)",
       y = "Mercury Levels (ppm)") +
  theme_minimal()

# Plot Total Marine Mammals vs Mercury Levels
ggplot(data, aes(x = Total_Marine_Mammals, y = Hg..ppm.)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "green") +
  labs(title = "Mercury Levels vs Marine Mammal Consumption",
       x = "Total Marine Mammal Consumption",
       y = "Mercury Levels (ppm)") +
  theme_minimal()


# Plot Total Marine Mammals vs Mercury Levels
ggplot(data, aes(x = Total, y = Hg..ppm.)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "green") +
  labs(title = "Mercury Levels vs Seafood",
       x = "Total Seafood Consumption",
       y = "Mercury Levels (ppm)") +
  theme_minimal()

# Save the plots
ggsave("mercury_vs_fish_excluding_shark.png", width = 8, height = 6)
ggsave("mercury_vs_marine_mammals.png", width = 8, height = 6)




# Load necessary libraries
library(ggplot2)
library(dplyr)

# Read the data
data <- read.csv("Caribbean.csv")

# Define the species columns (replace slashes with dots)
species <- c("Barracuda.month", "Blackfish.month", "Bonito.month", "Dolphinfish.month", 
             "Flying.fish.month", "Grouper.month", "Jackfish.month", "Kingfish.month", 
             "Marlin.month", "Porpoise.month", "Redfish.month", "Sailfish.month", 
             "Shark.month", "Snapper.month", "Swordfish.month", "Tuna.month", 
             "Wahoo.month", "Whale.month")


# Read the data
data <- read.csv("Caribbean.csv")


ggplot(data, aes(x = EDUCATION, y = Hg..ppm.)) +
  geom_boxplot() +
  labs(title = "Boxplot of Mercury Levels by Education Level",
       x = "Education Level",
       y = "Mercury Levels (Hg [ppm])") +
  theme_minimal()