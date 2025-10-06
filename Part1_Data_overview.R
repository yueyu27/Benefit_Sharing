#####################################
# Project: Benefit sharing
#
# Part 1: Overview of Data from 2022 - 2025
#
# 2025 October
# by: Yue Yu
#
#####################################


# Local R studio
# R version: 4.4.0

# -- Step 1: Load raw data and clean

getwd()
setwd("/Users/yueyu/Desktop/Benefit_sharing")

# ----------
#   2023
# ----------

raw_2023 <- read.csv("raw_benefit_data_2023.csv",
                      header = TRUE,
                      fileEncoding = "latin1",
                      stringsAsFactors = FALSE)
# fileEncoding = "latin1" allows special characters to be recognized correctly
head(raw_2023)
dim(raw_2023)


# Ê is introduced to spaces, adjust this by substituting it back and remove any double space
raw_2023$author <- gsub("Ê", " ", raw_2023$author, fixed = TRUE)
raw_2023$author <- gsub("\\s+", " ", trimws(raw_2023$author))

raw_2023$benefit <- gsub("Ê", " ", raw_2023$benefit, fixed = TRUE)
raw_2023$benefit <- gsub("\\s+", " ", trimws(raw_2023$benefit))

#any string with "none" set to blank
raw_2023$benefit <- gsub("none", "", raw_2023$benefit)

# adjust any country name with mis-spelling
raw_2023$countries <- gsub("The Neatherlands", "The Netherlands", raw_2023$countries, fixed = TRUE)
raw_2023$countries <- gsub("The Netherland", "The Netherlands", raw_2023$countries, fixed = TRUE)
raw_2023$countries <- gsub(".", ",", raw_2023$countries, fixed = TRUE)


# ----------
#   2024
# ----------
raw_2024 <- read.csv("raw_benefit_data_2024.csv",
                      header = TRUE,
                      fileEncoding = "latin1",
                      stringsAsFactors = FALSE)
head(raw_2024)
dim(raw_2024)

raw_2024$author <- gsub("Ê", " ", raw_2024$author, fixed = TRUE)
raw_2024$author <- gsub("\\s+", " ", trimws(raw_2024$author))

raw_2024$benefit <- gsub("Ê", " ", raw_2024$benefit, fixed = TRUE)
raw_2024$benefit <- gsub("\\s+", " ", trimws(raw_2024$benefit))

raw_2024$benefit <- gsub("none", "", raw_2024$benefit)

raw_2024$countries <- gsub("The Neatherlands", "The Netherlands", raw_2024$countries, fixed = TRUE)
raw_2024$countries <- gsub("The Netherland", "The Netherlands", raw_2024$countries, fixed = TRUE)
raw_2024$countries <- gsub(".", ",", raw_2024$countries, fixed = TRUE)


# ----------
#   2025
# ----------
raw_2025 <- read.csv("raw_benefit_data_2025.csv",
                      header = TRUE,
                      fileEncoding = "latin1",
                      stringsAsFactors = FALSE)
head(raw_2025)
dim(raw_2025)

raw_2025$author <- gsub("Ê", " ", raw_2025$author, fixed = TRUE)
raw_2025$author <- gsub("\\s+", " ", trimws(raw_2025$author))

raw_2025$benefit <- gsub("Ê", " ", raw_2025$benefit, fixed = TRUE)
raw_2025$benefit <- gsub("\\s+", " ", trimws(raw_2025$benefit))

raw_2025$benefit <- gsub("none", "", raw_2025$benefit)

raw_2025$countries <- gsub("The Neatherlands", "The Netherlands", raw_2025$countries, fixed = TRUE)
raw_2025$countries <- gsub("The Netherland", "The Netherlands", raw_2025$countries, fixed = TRUE)
raw_2025$countries <- gsub(".", ",", raw_2025$countries, fixed = TRUE)





# -- Step 2: # of papers with benefit statement

library(dplyr)
# ----------
#   2023
# ----------
raw_2023 %>% summarise(blank_or_none = sum(trimws(tolower(benefit)) %in% c("", "none"), na.rm = TRUE))
# 290 blank or none

# ----------
#   2024
# ----------
raw_2024 %>% summarise(blank_or_none = sum(trimws(tolower(benefit)) %in% c("", "none"), na.rm = TRUE))
# 183 blank or none

# ----------
#   2025
# ----------
raw_2025 %>% summarise(blank_or_none = sum(trimws(tolower(benefit)) %in% c("", "none"), na.rm = TRUE))
# 183 blank or none



# -- Step 3: Extract by topic category

library(dplyr)

# ----------
#   2023
# ----------
summary_by_catgory_2023 <- raw_2023 %>%
								  group_by(category) %>%
								  summarise(
								    count = n(),
								    no_benefit = sum(trimws(benefit) == "" | is.na(benefit)),
								    yes_benefit = count - no_benefit,
								    yes_benefit_percentage = round((yes_benefit / count) * 100, digits = 3)
								  ) %>%
								  ungroup() %>%
								  arrange(desc(yes_benefit_percentage))

write.table(summary_by_catgory_2023, "summary_by_catgory_2023.txt", sep = "\t", row.names = F, col.names = T, quote = F)


# ----------
#   2024
# ----------

summary_by_catgory_2024 <- raw_2024 %>%
								  group_by(category) %>%
								  summarise(
								    count = n(),
								    no_benefit = sum(trimws(benefit) == "" | is.na(benefit)),
								    yes_benefit = count - no_benefit,
								    yes_benefit_percentage = round((yes_benefit / count) * 100, digits = 3)
								  ) %>%
								  ungroup() %>%
								  arrange(desc(yes_benefit_percentage))
write.table(summary_by_catgory_2024, "summary_by_catgory_2024.txt", sep = "\t", row.names = F, col.names = T, quote = F)


# ----------
#   2025
# ----------

summary_by_catgory_2025 <- raw_2025 %>%
								  group_by(category) %>%
								  summarise(
								    count = n(),
								    no_benefit = sum(trimws(benefit) == "" | is.na(benefit)),
								    yes_benefit = count - no_benefit,
								    yes_benefit_percentage = round((yes_benefit / count) * 100, digits = 3)
								  ) %>%
								  ungroup() %>%
								  arrange(desc(yes_benefit_percentage))

write.table(summary_by_catgory_2025, "summary_by_catgory_2025.txt", sep = "\t", row.names = F, col.names = T, quote = F)




# -- Step 4: Extract by country

library(stringr)

# ----------
#   2023
# ----------
raw_2023 <- raw_2023 %>%
  mutate(n_countries = str_count(countries, ",") + 1)

summary_by_num_country_2023 <- raw_2023 %>%
								  group_by(n_countries) %>%
								  summarise(
								    count = n(),
								    no_benefit = sum(trimws(benefit) == "" | is.na(benefit)),
								    yes_benefit = count - no_benefit,
								    yes_benefit_percentage = round((yes_benefit / count) * 100, digits = 3)
								  ) %>%
								  ungroup() %>%
								  arrange(n_countries)

write.table(summary_by_num_country_2023, "summary_by_num_country_2023.txt", sep = "\t", row.names = F, col.names = T, quote = F)

# ---------
#   2024
# ----------
raw_2024 <- raw_2024 %>%
  mutate(n_countries = str_count(countries, ",") + 1)

summary_by_num_country_2024 <- raw_2024 %>%
								  group_by(n_countries) %>%
								  summarise(
								    count = n(),
								    no_benefit = sum(trimws(benefit) == "" | is.na(benefit)),
								    yes_benefit = count - no_benefit,
								    yes_benefit_percentage = round((yes_benefit / count) * 100, digits = 3)
								  ) %>%
								  ungroup() %>%
								  arrange(n_countries)

write.table(summary_by_num_country_2024, "summary_by_num_country_2024.txt", sep = "\t", row.names = F, col.names = T, quote = F)



# ---------
#   2025
# ----------
raw_2025 <- raw_2025 %>%
  mutate(n_countries = str_count(countries, ",") + 1)

summary_by_num_country_2025 <- raw_2025 %>%
								  group_by(n_countries) %>%
								  summarise(
								    count = n(),
								    no_benefit = sum(trimws(benefit) == "" | is.na(benefit)),
								    yes_benefit = count - no_benefit,
								    yes_benefit_percentage = round((yes_benefit / count) * 100, digits = 3)
								  ) %>%
								  ungroup() %>%
								  arrange(n_countries)

write.table(summary_by_num_country_2025, "summary_by_num_country_2025.txt", sep = "\t", row.names = F, col.names = T, quote = F)



# ---------
#   MERGE + PLOT
# ----------
library(dplyr)
library(ggplot2)

df_2025 <- summary_by_num_country_2025 %>%
  select(n_countries, yes_benefit_percentage) %>%
  mutate(year = 2025)

df_2024 <- summary_by_num_country_2024 %>%
  select(n_countries, yes_benefit_percentage) %>%
  mutate(year = 2024)

df_2023 <- summary_by_num_country_2023 %>%
	select(n_countries,yes_benefit_percentage) %>%
	mutate(year = 2023)

final_df <- bind_rows(df_2023,df_2024,df_2025)

ggplot(final_df, aes(x = n_countries, y = yes_benefit_percentage, color = factor(year))) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    x = "Number of Countries",
    y = "Yes Benefit Percentage",
    color = "Year",
    title = "Trend of Yes Benefit Percentage by Number of Countries"
  ) +
  theme_minimal()




# -- Step 5: Extract country NAME in with/withOUT benefit sharing

library(dplyr)
library(tidyr)


# ----------
#   2023
# ----------

name_NO_benefit_2023 <- raw_2023 %>%
  filter(benefit == "") %>%
  select(countries) %>%
  separate_rows(countries, sep = ",") %>%
  mutate(countries = trimws(countries), countries = str_to_title(countries)) %>%
  count(countries, name = "count") %>%
  arrange(desc(count))

write.table(name_NO_benefit_2023, "NO_benefit_country_2023.txt", sep = "\t", row.names = F, col.names = T, quote = F)


name_YES_beenfit_2023 <- raw_2023 %>%
  filter(benefit != "") %>%
  select(countries) %>%
  separate_rows(countries, sep = ",") %>%
  mutate(countries = trimws(countries), countries = str_to_title(countries)) %>%
  count(countries, name = "count") %>%
  arrange(desc(count))

write.table(name_YES_beenfit_2023, "YES_benefit_country_2023.txt", sep = "\t", row.names = F, col.names = T, quote = F)




# ----------
#   2024
# ----------

name_NO_benefit_2024 <- raw_2024 %>%
  filter(benefit == "") %>%
  select(countries) %>%
  separate_rows(countries, sep = ",") %>%
  mutate(countries = trimws(countries), countries = str_to_title(countries)) %>%
  count(countries, name = "count") %>%
  arrange(desc(count))

write.table(name_NO_benefit_2024, "NO_benefit_country_2024.txt", sep = "\t", row.names = F, col.names = T, quote = F)


name_YES_beenfit_2024 <- raw_2024 %>%
  filter(benefit != "") %>%
  select(countries) %>%
  separate_rows(countries, sep = ",") %>%
  mutate(countries = trimws(countries), countries = str_to_title(countries)) %>%
  count(countries, name = "count") %>%
  arrange(desc(count))
  
write.table(name_YES_beenfit_2024, "YES_benefit_country_2024.txt", sep = "\t", row.names = F, col.names = T, quote = F)




# ----------
#   2025
# ----------

name_NO_benefit_2025 <- raw_2025 %>%
  filter(benefit == "") %>%
  select(countries) %>%
  separate_rows(countries, sep = ",") %>%
  mutate(countries = trimws(countries), countries = str_to_title(countries)) %>%
  count(countries, name = "count") %>%
  arrange(desc(count))

write.table(name_NO_benefit_2025, "NO_benefit_country_2025.txt", sep = "\t", row.names = F, col.names = T, quote = F)


name_YES_beenfit_2025 <- raw_2025 %>%
  filter(benefit != "") %>%
  select(countries) %>%
  separate_rows(countries, sep = ",") %>%
  mutate(countries = trimws(countries), countries = str_to_title(countries)) %>%
  count(countries, name = "count") %>%
  arrange(desc(count))
  
write.table(name_YES_beenfit_2025, "YES_benefit_country_2025.txt", sep = "\t", row.names = F, col.names = T, quote = F)










# -- Step XXX: Key words extract

collaboration
partner



# END