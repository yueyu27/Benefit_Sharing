#####################################
# Project: Benefit sharing
#
# Part 5: Submission data
#
# 2025 Nov 18
# by: Yue Yu
#
#####################################


# Note: we only focus on papers submitted to ME since 2023


# Local R studio
# R version: 4.4.0

getwd()
setwd("/Users/yueyu/Desktop/ABS/submit_raw_data")
library(dplyr)
library(tidyverse)
library(stringr)


#--------------
#
#  Benefit
#
#--------------

# -- Read in data
submit_raw <- read.delim("MEC_updated_report_with_benefit.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)

# -- Omit all rows without ID
submit_raw_2 <- submit_raw[submit_raw$ID != "",]

# -- Omit all rows with revision
submit_raw_3 <- submit_raw_2[!grepl("\\.R[0-9]+$", submit_raw_2$ID), ]

# -- Extract for 2023
submit_2023 <- submit_raw_3[grep("MEC-23",submit_raw_3$ID),]
dim(submit_2023)
# 811 papers - submitted to ME with beenfit sharing statement
tail(submit_2023)
# code for last paper: MEC-23-1243 --> assume there are 1243 paper submitted in total in 2023


# -- Extract for 2024
submit_2024 <- submit_raw_3[grep("MEC-24",submit_raw_3$ID),]
dim(submit_2024)
# 978 papers - submitted to ME with beenfit sharing statement
tail(submit_2024)
# code for last paper: MEC-24-1428 --> assume there are 1428 paper submitted in total in 2024


# -- Extract for 2025 (as of 2025 Nov 04)
submit_2025 <- submit_raw_3[grep("MEC-25",submit_raw_3$ID),]
dim(submit_2025)
# 391 papers - submitted to ME with beenfit sharing statement
tail(submit_2025)
# code for last paper: MEC-25-0602 --> assume there are 602 paper submitted in total in 2025




#--------------
#
#  Not Applicable
#
#--------------

# -- Read in data
submit_NA_raw <- read.delim("MEC_updated_report_with_NA.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)

# -- Omit all rows without ID
submit_NA_raw_2 <- submit_NA_raw[submit_NA_raw$ID != "",]

# -- Omit all rows with revision
submit_NA_raw_3 <- submit_NA_raw_2[!grepl("\\.R[0-9]+$", submit_NA_raw_2$ID), ]

# -- Omit all duplicated ID rows 
submit_NA_raw_4 <- submit_NA_raw_3[!duplicated(submit_NA_raw_3$ID), ]



# -- Extract for 2023
submit_NA_2023 <- submit_NA_raw_4[grep("MEC-23",submit_NA_raw_4$ID),]
dim(submit_NA_2023)
# 423 papers - submitted to ME stating N/A for complying with Nagoya Protocol

ID_2023 <- submit_NA_2023$ID
max(as.numeric(gsub("MEC-23-", "", ID_2023)))
# 1242 --> last code in this file indicates there are 1242 paper submitted in total in 2023


# -- Extract for 2024
submit_NA_2024 <- submit_NA_raw_4[grep("MEC-24",submit_NA_raw_4$ID),]
dim(submit_NA_2024)
# 430 papers - submitted to ME stating N/A for complying with Nagoya Protocol

ID_2024 <- submit_NA_2024$ID
max(as.numeric(gsub("MEC-24-", "", ID_2024)))
# 1425 --> last code in this file indicates there are 1425 paper submitted in total in 2024



# -- Extract for 2025 (as of 2025 Nov 04)
submit_NA_2025 <- submit_NA_raw_4[grep("MEC-25",submit_NA_raw_4$ID),]
dim(submit_NA_2025)
# 208 papers - submitted to ME stating N/A for complying with Nagoya Protocol

ID_2025 <- submit_NA_2025$ID
max(as.numeric(gsub("MEC-25-", "", ID_2025)))
# 604 --> last code in this file indicates there are 604 paper submitted in total in 2025


# END