
#####################################
# Project: Benefit sharing
#
# Part 5: Finalize table 
#
# 2025 Nov later
# by: Yue Yu
#
#####################################

getwd()
setwd("/Users/yueyu/Desktop/ABS/v2/txt_reference/")
library(dplyr)
library(tidyverse)
library(stringi)
library(PNWColors)


# -----------------------------------------------
#
#    Step 1: Load manual checked data (done during 2025 Oct 29-31)
#    Step 2: load title + reference data 
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

title_ref_2023 <- read.delim("/Users/yueyu/Desktop/ABS/v2/txt_reference/Title_ref_match_2023.txt",
                      header = TRUE,
                      fileEncoding = "Latin1",
                      stringsAsFactors = FALSE)
colnames(title_ref_2023)[1] <- "title"





#--- EXTACT MATCH ---

# Clean up both title before merging
checked_2023$title <- checked_2023$title |>
  stri_trans_general("Latin-ASCII") |>     # Normalize accents
  trimws() |>                              # Trim leading/trailing spaces
  tolower()                                # Lowercase for matching

title_ref_2023$title <- title_ref_2023$title |>
  stri_trans_general("Latin-ASCII") |>
  trimws() |>
  tolower()

# merge two dataframes based on "title" (EXACT MATCH)
merged_2023 <- merge(checked_2023, title_ref_2023, by = "title", all.x = TRUE)
merged_2023_subset <- merged_2023[,c(1,2,5,12,13,14)]

head(merged_2023_subset)
dim(merged_2023_subset)
# 121 6






# 2023 is working, need to do the rest









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

title_ref_2024 <- read.delim("Title_ref_match_2024.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)


# merge two dataframes based on "title"
merged_2024 <- merge(checked_2024, title_ref_2024, by = "title", all.x = TRUE)
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

title_ref_2025 <- read.delim("Title_ref_match_2025.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)


# merge two dataframes based on "title"
merged_2025 <- merge(checked_2025, title_ref_2025, by = "title", all.x = TRUE)
merged_2025_subset <- merged_2025[,c(1,2,5,12,13)]

head(merged_2025)
dim(merged_2025)
# 97


rm(checked_2023,checked_2024,checked_2025)
rm(title_country_2023,title_country_2024,title_country_2025)
rm(merged_2023,merged_2024,merged_2025)




# -----------------------------------------------
#
#    Step 3: Extract reference into names
#
# -----------------------------------------------












# -----------------------------------------------
#
#    Step 4: Finalized Table 3, 4, and 5
#
# -----------------------------------------------








# END