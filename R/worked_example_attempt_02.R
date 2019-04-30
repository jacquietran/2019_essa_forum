# Worked example from my presentation at the 2019 ESSA Forum
# Data set from the Australian Bureau of Statistics

# https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/4364.0.55.0012017-18?OpenDocument

# Load libraries ---------------------------------------------------------------

library(here)
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

# Import data ------------------------------------------------------------------

# This worked example focuses on the data in the named sheet:
# 'Table 2.1_Estimates'

source_data <- read_excel(
  here("data/4364055001do002_20172018.xls"),
  sheet = "Table 2.1_Estimates", range = cell_rows(27:39), col_names = FALSE)

headers <- read_excel(
  here("data/4364055001do002_20172018.xls"),
  sheet = "Table 2.1_Estimates", range = cell_rows(6), col_names = FALSE)

# Clean data -------------------------------------------------------------------

clean_data <- source_data

# Define column names using headers data frame
colnames(clean_data) <- c(
  "health_condition", headers)

# Tidy up health condition naming (remove footnote references)
clean_data <- clean_data %>%
  mutate(health_condition = str_replace(health_condition, "\\(.*\\)", "")) %>%
  mutate(health_condition = str_trim(health_condition, side = "both"))

# Wrangle data -----------------------------------------------------------------

# Extract per condition, all country totals to a separate data frame
aus_totals <- clean_data %>%
  select(health_condition, Australia) %>%
  filter(health_condition != "Total persons, all ages")

# Extract per state, all conditions totals to a separate data frame
state_totals <- clean_data %>%
  filter(health_condition == "Total persons, all ages") %>%
  select(-Australia)

# Omit per condition, all country & per state, all conditions totals
# From per condition, per state data frame
per_condition_per_state <- clean_data %>%
  filter(health_condition != "Total persons, all ages") %>%
  select(-Australia)

# Convert per state, all conditions totals from wide to long
state_totals_long <- state_totals %>%
  gather("state", "pop_estimate", -health_condition) %>%
  select(-health_condition)

# Convert per condition, per state data frame from wide to long
per_condition_per_state_long <- per_condition_per_state %>%
  gather("state", "estimate", -health_condition)

# Into per_condition_per_state_long,
# merge estimates for total respondents per state
per_condition_per_state_long <- left_join(
  per_condition_per_state_long, state_totals_long) %>%
  mutate(pct_of_state_pop = round(estimate / pop_estimate * 100, 1))

# Build plot -------------------------------------------------------------------

# Sort levels of per_condition_per_state_long$health_condition, based on
# national prevalence as stored in aus_totals data frame
aus_totals <- aus_totals %>%
  arrange(desc(Australia))
per_condition_per_state_long$health_condition <- factor(
  per_condition_per_state_long$health_condition,
  levels = rev(unique(aus_totals$health_condition)))

# Build plot
p <- ggplot(per_condition_per_state_long,
            aes(x = health_condition, y = pct_of_state_pop,
                fill = health_condition))
p <- p + geom_bar(stat = "identity")
p <- p + facet_wrap(vars(state))
p <- p + coord_flip()
p <- p + theme(
  legend.position = "none")