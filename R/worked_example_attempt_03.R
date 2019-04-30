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
# 'Table 13.1_Estimates, persons'

source_data <- read_excel(
  here("data/4364055001do013_20172018.xls"),
  sheet = "Table 13.1_Estimates, persons",
  range = "A21:L26", col_names = FALSE)

headers <- read_excel(
  here("data/4364055001do013_20172018.xls"),
  sheet = "Table 13.1_Estimates, persons",
  range = "B7:L7", col_names = FALSE)

# Clean data -------------------------------------------------------------------

activity_data <- source_data

# Define column names by referring to headers data frame
colnames(activity_data) <- c(
  "duration", "age_15_17", "age_18_24", "age_25_34", "age_35_44", "age_45_54",
  "age_55_64", "age_65_74", "drop1", "drop2", "age_75_84", "age_85_plus")

# Drop unnecessary variables
activity_data <- activity_data %>%
  select(-starts_with("drop"))

# Remove footnote references
activity_data <- activity_data %>%
  mutate(duration = str_replace(duration, "\\(.*\\)", "")) %>%
  mutate(duration = str_trim(duration, side = "both"))

# Extract age segment totals to separate data frame
# and convert wide to long
age_segment_totals <- activity_data %>%
  filter(duration == "Total") %>%
  gather("segment", "resp_estimate", -duration) %>%
  select(-duration)

# Remove rows that present totals data (different scale!)
# and convert wide to long
activity_data_long <- activity_data %>%
  filter(!grepl("Total", duration)) %>%
  gather("segment", "estimate", -duration)

# Merge data frames together
activity_data_long <- left_join(
  activity_data_long, age_segment_totals)

# Calculate estimates as percentage of respondents per age segment
activity_data_long <- activity_data_long %>%
  mutate(pct_of_resp = round(estimate / resp_estimate * 100, 1))

# Specify factor levels in duration
activity_data_long$duration <- factor(
  activity_data_long$duration,
  levels = rev(unique(activity_data_long$duration)))

# Build plot -------------------------------------------------------------------

p <- ggplot(activity_data_long,
            aes(x = duration, y = pct_of_resp,
                fill = duration,
                label = paste0(pct_of_resp, "%")))
p <- p + facet_wrap(vars(segment))
p <- p + geom_bar(stat = "identity")
p <- p + geom_text(hjust = -0.2)
p <- p + scale_y_continuous(
  limits = c(0, 65),
  breaks = seq(0, 60, by = 20))
p <- p + labs(
  title = "~50% of Australians aged 85+ reported 0 min of physical activity in the last week",
  caption = "Source: National Health Survey 2017–2018",
  x = NULL, y = NULL)
p <- p + scale_fill_brewer(palette = "RdBu", direction = -1)
p <- p + coord_flip()
p <- p + theme(
  legend.position = "none")