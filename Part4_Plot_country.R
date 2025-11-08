#####################################
# Project: Benefit sharing
#
# Part 4: Plot country + benefit cetegory
#
# 2025 Nov 4
# by: Yue Yu
#
#####################################

# -----------------------------------------------
#
#    Step 1: Load 142 countires signed Nygoya Protocol
#
# -----------------------------------------------
# Local R studio
# R version: 4.4.0

getwd()
setwd("/Users/yueyu/Desktop/ABS/v2/txt_country_benefit_plot")
library(dplyr)
library(tidyverse)
library(PNWColors)


sign <- read.delim("/Users/yueyu/Desktop/ABS/v2/Signed_country_list.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)

length(sign) # 142 countries signed


# -----------------------------------------------
#
#    Step 2: Load manual checked data (done during 2025 Oct 29-31)
#    Step 2.1: load title + country data
#    Step 2.2: load title + reference data (OPTOINAL, ADD LATER)
#
# -----------------------------------------------


# ----------
#   2023
# ----------
checked_2023 <- read.delim("/Users/yueyu/Desktop/ABS/v2/txt_benefit_code/ABS_code_2023_type_added_manual_checked.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)
# colClasses = "character": force all column to be characters to aviod "F" being read in as FALSE
head(checked_2023)
dim(checked_2023)
# 121 12

title_country_2023 <- read.delim("Title_Country_match_2023.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)


# merge two dataframes based on "title"
merged_2023 <- merge(checked_2023, title_country_2023, by = "title", all.x = TRUE)
merged_2023_subset <- merged_2023[,c(1,2,5,12,13)]

head(merged_2023_subset)
dim(merged_2023_subset)
# 121 12




# ----------
#   2024
# ----------
checked_2024 <- read.delim("/Users/yueyu/Desktop/ABS/v2/txt_benefit_code/ABS_code_2024_type_added_manual_checked.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)


head(checked_2024)
dim(checked_2024)
# 137 12

title_country_2024 <- read.delim("Title_Country_match_2024.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)


# merge two dataframes based on "title"
merged_2024 <- merge(checked_2024, title_country_2024, by = "title", all.x = TRUE)
merged_2024_subset <- merged_2024[,c(1,2,5,12,13)]

head(merged_2024)
dim(merged_2024)
# 137 


# ----------
#   2025
# ----------
checked_2025 <- read.delim("/Users/yueyu/Desktop/ABS/v2/txt_benefit_code/ABS_code_2025_type_added_manual_checked.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)


head(checked_2025)
dim(checked_2025)
# 97 12

title_country_2025 <- read.delim("Title_Country_match_2025.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)


# merge two dataframes based on "title"
merged_2025 <- merge(checked_2025, title_country_2025, by = "title", all.x = TRUE)
merged_2025_subset <- merged_2025[,c(1,2,5,12,13)]

head(merged_2025)
dim(merged_2025)
# 97


rm(checked_2023,checked_2024,checked_2025)
rm(title_country_2023,title_country_2024,title_country_2025)
rm(merged_2023,merged_2024,merged_2025)



# -----------------------------------------------
#
#    Step 3: Pool across 2023-2025
#
# -----------------------------------------------
colnames(merged_2024_subset)[3] <- "species_final"

pool <- rbind(merged_2023_subset,merged_2024_subset,merged_2025_subset)
pool <- pool[,c(4,5)]

  benefit_summary                               countries
1             A,B                             China,  USA
2             A,B                                     USA
3             A,B                   Brazil , Finland, USA
4             A,B                                     USA
5               A Belgium, Switzerland, Tanzania, UK, USA
6           B,C,E                                     USA

rm(merged_2023_subset,merged_2024_subset,merged_2025_subset)


# -- Recode all country name based on if they are in signed country list
# -- If all countires for a publication is within sign list -> YES_ABS_country
# -- If all countires for a publication is NOT within sign list -> NO_ABS_country
# -- If all countires for a publication is mixed with both -> mixed_ABS_country

library(dplyr)
library(stringr)

sign_countries <- str_trim(sign$countries)

pool <- pool %>%
  rowwise() %>%
  mutate(
    # split and trim, then wrap in list()
    country_list = list(str_split(countries, ",\\s*")[[1]] %>% str_trim()),
    all_in_sign = all(country_list %in% sign_countries),
    any_in_sign = any(country_list %in% sign_countries),
    country_type = case_when(
      all_in_sign ~ "Party (Full)",
      !any_in_sign ~ "Non-Party",
      TRUE ~ "Party (Partial)"
    )
  ) %>%
  ungroup() %>%
  select(-country_list, -all_in_sign, -any_in_sign)


# Nagoya Protocol Status
#- Party (Full)
#- Party (Partial)
#- Non-Party


# -------- -------- -------- -------- --------
# Option 1: Number of publications by ABS or non-ABS countries report benefit (COUNT)
# -------- -------- -------- -------- --------

# -- Split benefit summary column into long format, and then count country_type -> plot

# Split benefit_summary into multiple rows (one per benefit)
pool_long <- pool %>%
  separate_rows(benefit_summary, sep = ",\\s*")

# Summarize counts per benefit and country_type
summary_counts <- pool_long %>%
  group_by(benefit_summary, country_type) %>%
  summarise(count = n(), .groups = "drop")

summary_counts <- summary_counts[-1,]
# Note: One entry had no benefit type, thus dropped for further analyses



# -- Plotting!!

# -------- -------- -------- -------- --------
# Question: How likely does ABS or non-ABS countries report benefit (PROPORTION)
# -------- -------- -------- -------- --------

summary_prop <- summary_counts %>%
        group_by(country_type) %>%
        mutate(total = sum(count),prop = round((count / total), digits = 2))

summary_prop$country_type <- factor(
  summary_prop$country_type,
  levels = c("Party (Full)", "Party (Partial)", "Non-Party")
)

color <- rev(pnw_palette("Shuksan", 3))

category_labels <- c(
  "Party (Full)" = "Party (Full)",
  "Party (Partial)" = "Party (Partial)",
  "Non-Party" = "Non-Party"
)



ggplot(summary_prop, aes(x = benefit_summary, y = prop, fill = country_type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    values = color,
    name = "Nagoya Protocol Status",       # legend title
    labels = category_labels     # legend labels
  ) +
  labs(
    x = "Benefit category",
    y = "Proportion of publications"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    axis.title.x = element_text(size = 15, margin = margin(t = 8)),
    axis.title.y = element_text(size = 15, margin = margin(r = 8)),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

# Figure 3 generated



# END
