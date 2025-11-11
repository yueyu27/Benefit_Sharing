#####################################
# Project: Benefit sharing
#
# Part 0: data cleaning
#
# 2025 Nov 10
# by: Yue Yu
#
#####################################




# Local R studio
# R version: 4.4.0

getwd()
setwd("/Users/yueyu/Desktop/ABS/v3/txt_benefit_code")
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

species_2023 <- checked_2023[,c(2,5)]

new_raw_2023 <- raw_2023 %>%
  left_join(species_2023, by = "title") %>%
  mutate(species = coalesce(species_final, species)) %>%
  select(-species_final)

write.table(new_raw_2023, "/Users/yueyu/Desktop/ABS/v3/txt_raw_clean/raw_clean_2023.txt", sep = "\t", row.names = F, col.names = T, quote = F)

# Manual adjust any species not classified in the raw data

# Read back in
new_raw_2023_clean <- read.delim("/Users/yueyu/Desktop/ABS/v3/txt_raw_clean/raw_clean_2023.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)

unique(new_raw_2023_clean$species)
[1] "seed_plant"   "invertebrate" "ecosystem"    "other"       
[5] "vertebrate"   "fungi"        "bacteria"


# ----------
#   2024
# ----------
checked_2024 <- read.delim("ABS_code_2024_type_added_manually_checked.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)

species_2024 <- checked_2024[,c(2,5)]

new_raw_2024 <- raw_2024 %>%
  left_join(species_2024, by = "title") %>%
  mutate(species = coalesce(species_final, species)) %>%
  select(-species_final)

write.table(new_raw_2024, "/Users/yueyu/Desktop/ABS/v3/txt_raw_clean/raw_clean_2024.txt", sep = "\t", row.names = F, col.names = T, quote = F)

# Read back in
new_raw_2024_clean <- read.delim("/Users/yueyu/Desktop/ABS/v3/txt_raw_clean/raw_clean_2024.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)

unique(new_raw_2024_clean$species)
[1] "vertebrate"   "invertebrate" "bacteria"     "other"       
[5] "seed_plant"   "ecosystem"    "fungi"        " vertebrate" 


# ----------
#   2025
# ----------
checked_2025 <- read.delim("ABS_code_2025_type_added_manually_checked.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)


species_2025 <- checked_2025[,c(2,5)]

new_raw_2025 <- raw_2025 %>%
  left_join(species_2025, by = "title") %>%
  mutate(species = coalesce(species_final, species)) %>%
  select(-species_final)

write.table(new_raw_2025, "/Users/yueyu/Desktop/ABS/v3/txt_raw_clean/raw_clean_2025.txt", sep = "\t", row.names = F, col.names = T, quote = F)

# Read back in
new_raw_2025_clean <- read.delim("/Users/yueyu/Desktop/ABS/v3/txt_raw_clean/raw_clean_2025.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)

unique(new_raw_2025_clean$species)
[1] "vertebrate"   "other"        "invertebrate" "seed_plant"  
[5] "ecosystem"    "fungi"        "bacteria"



# END