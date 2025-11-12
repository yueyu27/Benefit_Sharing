#####################################
# Project: Benefit sharing
#
# Part 1: Overview of Data from 2023 - 2025
#
# 2025 Nov 10
# by: Yue Yu
#
#####################################


# Local R studio
# R version: 4.4.0

# -------------------------------------------
#
# -- Step 1: Load raw data (Version 3 by Winnie) and clean 
#
# -------------------------------------------

getwd()
setwd("/Users/yueyu/Desktop/ABS/v3/txt_raw")

library(dplyr)
library(stringr)
library(ggplot2)

# ----------
#   2023
# ----------

raw_2023 <- read.delim("raw_2023.txt",
                      header = TRUE,
                      fileEncoding = "Latin1",
                      stringsAsFactors = FALSE)
# fileEncoding = "Latin1" allows special characters to be recognized correctly
head(raw_2023)
dim(raw_2023)


# Ê is introduced to spaces, adjust this by substituting it back and remove any double space
raw_2023$authors <- gsub("Ê", " ", raw_2023$authors, fixed = TRUE)
raw_2023$authors <- gsub("\\s+", " ", trimws(raw_2023$authors))

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
raw_2024 <- read.delim("raw_2024.txt",
                      header = TRUE,
                      fileEncoding = "Latin1",
                      stringsAsFactors = FALSE)
head(raw_2024)
dim(raw_2024)

raw_2024$authors <- gsub("Ê", " ", raw_2024$authors, fixed = TRUE)
raw_2024$authors <- gsub("\\s+", " ", trimws(raw_2024$authors))

raw_2024$benefit <- gsub("Ê", " ", raw_2024$benefit, fixed = TRUE)
raw_2024$benefit <- gsub("\\s+", " ", trimws(raw_2024$benefit))

raw_2024$benefit <- gsub("none", "", raw_2024$benefit)

raw_2024$countries <- gsub("The Neatherlands", "The Netherlands", raw_2024$countries, fixed = TRUE)
raw_2024$countries <- gsub("The Netherland", "The Netherlands", raw_2024$countries, fixed = TRUE)
raw_2024$countries <- gsub(".", ",", raw_2024$countries, fixed = TRUE)

# ----------
#   2025
# ----------
raw_2025 <- read.delim("raw_2025.txt",
                      header = TRUE,
                      fileEncoding = "Latin1",
                      stringsAsFactors = FALSE)
head(raw_2025)
dim(raw_2025)

raw_2025$authors <- gsub("Ê", " ", raw_2025$authors, fixed = TRUE)
raw_2025$authors <- gsub("\\s+", " ", trimws(raw_2025$authors))

raw_2025$benefit <- gsub("Ê", " ", raw_2025$benefit, fixed = TRUE)
raw_2025$benefit <- gsub("\\s+", " ", trimws(raw_2025$benefit))

raw_2025$benefit <- gsub("none", "", raw_2025$benefit)

raw_2025$countries <- gsub("The Neatherlands", "The Netherlands", raw_2025$countries, fixed = TRUE)
raw_2025$countries <- gsub("The Netherland", "The Netherlands", raw_2025$countries, fixed = TRUE)
raw_2025$countries <- gsub(".", ",", raw_2025$countries, fixed = TRUE)



# -------------------------------------------
#
# -- Step 2: Extract by NUMBER OF country 
#
# -------------------------------------------
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

#write.table(summary_by_num_country_2023, "../Results_202051020/summary_by_num_country_2023.txt", sep = "\t", row.names = F, col.names = T, quote = F)

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

#write.table(summary_by_num_country_2024, "../Results_202051020/summary_by_num_country_2024.txt", sep = "\t", row.names = F, col.names = T, quote = F)



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

#write.table(summary_by_num_country_2025, "../Results_202051020/summary_by_num_country_2025.txt", sep = "\t", row.names = F, col.names = T, quote = F)



# ---------
#   MERGE + PLOT
# ----------

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
final_df <- final_df[,-3] # remove year column, to pool across year

# -- Bin data (numerical into categorical)
# -- Add standard error to summarized data

summary_df <- final_df %>%
  mutate(
    country_bin = cut(
      n_countries,
      breaks = c(0, 2, 5, 8, 14),
      labels = c("0-2", "3-5", "6-8", "9-14"),
      include.lowest = TRUE,
      right = TRUE
    )
  ) %>%
  group_by(country_bin) %>%
  summarise(
    mean_yes_benefit = mean(yes_benefit_percentage, na.rm = TRUE),
    sd_yes_benefit   = sd(yes_benefit_percentage, na.rm = TRUE),
    se_yes_benefit   = sd_yes_benefit / sqrt(n()),  # standard error
    n = n()
  ) %>%
  arrange(country_bin)


# -- set levels to categorical data, then can plot as lines (to connect them)
summary_df$country_bin <- factor(summary_df$country_bin,
															  levels = c("0-2", "3-5", "6-8", "9-14"),
															  ordered = TRUE)


# Plot all in one
ggplot(summary_df, aes(x = country_bin, y = mean_yes_benefit, group = 1)) +
  geom_line(linewidth = 1.2, color = "#33271e") +  # line color
  geom_point(size = 3, color = "#33271e") +        # point color
  geom_errorbar(aes(
    ymin = mean_yes_benefit - se_yes_benefit,
    ymax = mean_yes_benefit + se_yes_benefit
  ), width = 0.2, color = "#33271e") + # same color as above
  labs(
    x = "Number of Countries",
    y = "% Benefit-Sharing Reporting"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(size = 15, margin = margin(t = 15)),
    axis.title.y = element_text(size = 15, margin = margin(r = 15)),
    axis.text.x  = element_text(size = 13),
    axis.text.y  = element_text(size = 13)
  )


# Figure 1 

# END