# Worked example from my presentation at the 2019 ESSA Forum
# Data set from the Australian Bureau of Statistics

# https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/4364.0.55.0012017-18?OpenDocument

# Load libraries ---------------------------------------------------------------

library(here)
library(readxl)
library(dplyr)
library(ggplot2)

# Import data ------------------------------------------------------------------

# This worked example focuses on the data in the named sheet:
# 'Table 13.1 Physical activity, Persons'

source_data <- read_excel(
  here("data/4364055001do013_20172018.xls"),
  sheet = "Table 13.1_Estimates, persons", range = cell_rows(7:100))

# Wrangle data -----------------------------------------------------------------

clean_data <- source_data

# Add column names
colnames(clean_data) <- c(
  "variable", "age_15_17", "age_18_24", "age_25_34", "age_35_44", "age_45_54",
  "age_55_64", "age_65_74", "grouped_75_plus", "drop1", "age_75_84",
  "age_85_plus", "drop2", "grouped_18_64", "grouped_65_plus", "drop3",
  "grouped_15_plus", "grouped_18_plus")

# Start tidying up the data set
clean_data <- clean_data %>%
  select(-starts_with("drop")) %>%
  filter(!is.na(variable)) %>%
  filter(variable != "Persons")

# Extract measure names
measure_names <- clean_data %>%
  mutate(measure = case_when(
    is.na(age_15_17) == TRUE ~ variable,
    TRUE                         ~ "replace")) %>%
  select(measure)

# In measure_names, revalue "replace" string with NA
measure_names[measure_names == "replace"] <- NA

# Copy measure names down to empty rows immediately below
measure_names <- measure_names %>%
  mutate(measure = zoo::na.locf(measure, na.rm = FALSE))

# Bind measure names with data set
clean_data <- cbind(measure_names, clean_data)

# Remove rows used for measures as subheaders
clean_data <- clean_data %>%
  filter(!is.na(age_15_17))

# In clean_data, revalue "na" string with NA
clean_data[clean_data == "na"] <- NA

# Display abbreviated numeric values in full
numeric_data_only <- clean_data %>%
  select(-measure, -variable) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate_all(list(full = ~.*1000)) %>%
  select(ends_with("full"))

# Merge measure and variable columns with numeric_data_only
measure_variable_cols <- clean_data %>%
  select(measure, variable)
clean_data <- cbind(measure_variable_cols, numeric_data_only)

# Pass measures list to a vector for easy subsetting later on
measure_vec <- unique(clean_data$measure)

# Subset data ------------------------------------------------------------------

# Separate age range data from grouped ages data
clean_data_age <- clean_data %>%
  select(measure, variable, starts_with("age"))
clean_data_grouped <- clean_data %>%
  select(measure, variable, starts_with("grouped"))

# Build plot -------------------------------------------------------------------
