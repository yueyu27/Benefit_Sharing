#####################################
# Project: Benefit sharing
#
# Part 3: Plot manually checked final benefit types
#
# 2025 October 31
# by: Yue Yu
#
#####################################


# -----------------------------------------------
#
#    Step 1: Load manual checked data (done during 2025 Oct 29-31)
#
# -----------------------------------------------

# Local R studio
# R version: 4.4.0

getwd()
setwd("/Users/yueyu/Desktop/ABS/v2/txt_benefit_code")
library(dplyr)
library(tidyverse)

# ----------
#   2023
# ----------
checked_2023 <- read.delim("ABS_code_2023_type_added_manual_checked.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)

# colClasses = "character": force all column to be characters to aviod "F" being read in as FALSE

head(checked_2023)
dim(checked_2023)
# 121 12


# ----------
#   2024
# ----------
checked_2024 <- read.delim("ABS_code_2024_type_added_manual_checked.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)


head(checked_2024)
dim(checked_2024)
# 137 12


# ----------
#   2025
# ----------
checked_2025 <- read.delim("ABS_code_2025_type_added_manual_checked.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)


head(checked_2025)
dim(checked_2025)
# 97 12


# -----------------------------------------------
#
#    Step 2: Plot benefit type 
#
# -----------------------------------------------


# ----------
#   2023
# ----------
checked_2023 <- checked_2023[,c(1,4,5,12)]

# Reshape to long format
df_long <- checked_2023 %>%
  pivot_longer(cols = c(country_final, species_final, benefit_summary),
    		   names_to = "category_type",
    		   values_to = "category_value") %>%
  separate_rows(category_value, sep = ",")  # split comma-separated benefits




# Step 2: define custom colors for each category type

category_colors <- c(
  country_final = "#7bbcd5",   # blue
  species_final = "#d0e2af",  # green
  benefit_summary = "#d8aedd"  # pink
)


facet_labels <- c(
  country_final = "Author Country",
  species_final = "Species Category",
  benefit_summary = "Benefit Type"
)


ggplot(df_long, aes(x = fct_infreq(category_value), fill = category_type)) +
  geom_bar() +
  coord_flip() +
  facet_wrap(
    ~category_type,
    scales = "free_y",
    labeller = as_labeller(facet_labels)
  ) +
  scale_fill_manual(values = category_colors, guide = "none") +
  scale_y_continuous(
    limits = c(0, 137),
    breaks = seq(0, 137, by = 50)
  ) +
  labs(x = NULL, y = "Count") +
  theme_classic(base_size = 14) +
  theme(
    strip.background = element_blank(),  # remove facet strip box
    strip.text = element_text(size = 14, face = "bold", colour = "black"),
    axis.text.x = element_text(size = 10, colour = "black"),
    axis.text.y = element_text(size = 10, colour = "black"),
    axis.title.x = element_text(size = 12, colour = "black"),
    axis.title.y = element_text(size = 12, colour = "black"),
    panel.spacing = unit(1, "lines"),
    plot.title = element_text(size = 16, face = "bold", colour = "black"),
    panel.background = element_blank(),
    plot.background = element_blank()
  )



# ----------
#   2024
# ----------
checked_2024 <- checked_2024[,c(1,4,5,12)]
colnames(checked_2024)[3] <- "species_final"

rm(df_long)
df_long <- checked_2024 %>%
  pivot_longer(cols = c(country_final, species_final, benefit_summary),
    		   names_to = "category_type",
    		   values_to = "category_value") %>%
  separate_rows(category_value, sep = ",")  # split comma-separated benefits

# plot code same as 2023



# ----------
#   2025
# ----------
checked_2025 <- checked_2025[,c(1,4,5,12)]
colnames(checked_2025)[3] <- "species_final"

rm(df_long)
df_long <- checked_2025 %>%
  pivot_longer(cols = c(country_final, species_final, benefit_summary),
    		   names_to = "category_type",
    		   values_to = "category_value") %>%
  separate_rows(category_value, sep = ",")  # split comma-separated benefits

# plot code same as 2023

# END