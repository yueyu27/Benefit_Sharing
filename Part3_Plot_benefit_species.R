#####################################
# Project: Benefit sharing
#
# Part 3: Plot manually checked final benefit types (plot 2023-2025 in one)
#
# 2025 Nov 3
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
library(PNWColors)


# ----------
#   2023
# ----------
checked_2023 <- read.delim("ABS_code_2023_type_added_manual_checked.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)

# colClasses = "character": force all column to be characters to aviod "F" being read in as FALSE
colnames(checked_2023)[1] <- "year"

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
#    Step 2: Benefit category occurence among year
#
# -----------------------------------------------
checked_2023 <- checked_2023[,c(1,12)]
checked_2024 <- checked_2024[,c(1,12)]
checked_2025 <- checked_2025[,c(1,12)]

checked_all <- rbind(checked_2023,checked_2024,checked_2025)
dim(checked_all)
# 355 entry

# Reshape to long format
df_long <- checked_all %>%
  pivot_longer(cols = c(benefit_summary),
    		   names_to = "category_type",
    		   values_to = "category_value") %>%
  separate_rows(category_value, sep = ",")  # split comma-separated benefits

df_long_clean <- df_long %>%
  filter(!is.na(category_value) & category_value != "")

table(df_long_clean$category_value)
#  A   B   C   D   E   F 
# 317 308  45  11  90  20 


# Extract information from above dataframe
category_counts <- as.data.frame(table(df_long_clean$category_value))
colnames(category_counts) <- c("category", "count")



# -----------------------------------------------
#
#    Step 3: Species category occurence among year
#
# -----------------------------------------------
checked_2023_species <- checked_2023[,c(1,5)]
checked_2024_species <- checked_2024[,c(1,5)]
checked_2025_species <- checked_2025[,c(1,5)]

colnames(checked_2024_species)[2] <- "species_final"

checked_all_species <- rbind(checked_2023_species,checked_2024_species,checked_2025_species)
dim(checked_all_species)
# 355 entry

# Reshape to long format
df_long_species <- checked_all_species %>%
  pivot_longer(cols = c(species_final),
           names_to = "category_type",
           values_to = "category_value") %>%
  separate_rows(category_value, sep = ",")  # split comma-separated benefits

df_long_species_clean <- df_long_species %>%
  filter(!is.na(category_value) & category_value != "")

table(df_long_species_clean$category_value)

    bacteria    ecosystem        fungi invertebrate        other  plant_other 
          15           19           13           81            2            3 
  seed_plant   vertebrate 
          59          163 


# Extract information from above dataframe
species_counts <- as.data.frame(table(df_long_species_clean$category_value))
colnames(species_counts) <- c("category", "count")

species_counts_sorted <- species_counts %>% arrange(desc(count))

species_counts_sorted$category <- factor(
  species_counts_sorted$category,
  levels = species_counts_sorted$category
)

levels(species_counts_sorted$category)

species_counts_sorted

      category count
1   vertebrate   163
2 invertebrate    81
3   seed_plant    59
4    ecosystem    19
5     bacteria    15
6        fungi    13
7  plant_other     3
8        other     2









# -----------------------------------------------
#
#    Step 4: Plot benefit and species category
#
# -----------------------------------------------

# BENEFIT CATEGORY

pal <- pnw_palette("Sailboat", 7)
p1 <- ggplot(category_counts, aes(x = category, y = count, fill = category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = pal) +
  labs(
    x = "",
    y = "Number of publications",
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 15, margin = margin(t = 8)),
    axis.title.y = element_text(size = 15, margin = margin(r = 8)),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

# SPECIES CATEGORY

pal2 <- pnw_palette("Moth", 8)
p2 <- ggplot(species_counts_sorted, aes(x = category, y = count, fill = category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = pal2) +
  labs(
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 15, margin = margin(t = 8)),
    axis.title.y = element_text(size = 15, margin = margin(r = 8)),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )


# Combine plots next to each other
library(patchwork)

combined_plot <- p1 | p2  # use p1 | p2 for side-by-side
combined_plot

# Add text in ADOBE
# Y: Number of publications
# X1: Benefit category
# X2: Species category










# END