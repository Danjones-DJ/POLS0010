library(tidyverse); library(skimr); library(forcats)

# Load the data

aps24 <- read_csv("APS2024.csv") %>% janitor::clean_names()
election24 <- read_csv("GE2024.csv") %>% janitor::clean_names()

# Merge the data
election24aps24 <- merge(election24, aps24, by="ons_id")

# Create england binary variable
election24aps24$england <- election24aps24$country_name
election24aps24$england <- recode(election24aps24$england, Scotland = "Other", Wales = "Other")

