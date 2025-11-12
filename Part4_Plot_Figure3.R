#####################################
# Project: Benefit sharing
#
# Part 4: Plot manually checked final benefit types (plot 2023-2025 in one) - Figure 3
#
# 2025 Nov 10
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
setwd("/Users/yueyu/Desktop/ABS/v3/txt_benefit_code")
library(dplyr)
library(tidyverse)
library(PNWColors)
library(stringr)



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
# 122 12


# ----------
#   2024
# ----------
checked_2024 <- read.delim("ABS_code_2024_type_added_manually_checked.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)


head(checked_2024)
dim(checked_2024)
# 137 12


# ----------
#   2025
# ----------
checked_2025 <- read.delim("ABS_code_2025_type_added_manually_checked.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)


head(checked_2025)
dim(checked_2025)
# 118 12




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
# 377 entry

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
# 338 325  49  12  95  22


# Extract information from above dataframe
category_counts <- as.data.frame(table(df_long_clean$category_value))
colnames(category_counts) <- c("category", "count")




# -----------------------------------------------
#
#    Step 3: Plot benefit category (Figure 3a)
#
# -----------------------------------------------

# BENEFIT CATEGORY

pal <- pnw_palette("Sailboat", 7)
p1 <- ggplot(category_counts, aes(x = category, y = count, fill = category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = pal) +
  labs(
    x = "Benefit category",
    y = "Number of publications",
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 14, margin = margin(t = 8)),
    axis.title.y = element_text(size = 14, margin = margin(r = 8)),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )






# -----------------------------------------------
#
#    Step 4.1: Plot benefit category per country type (Figure 3b)
#
# -----------------------------------------------


# ----------
#   2023
# ----------
checked_2023 <- read.delim("ABS_code_2023_type_added_manual_checked.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)

title_country_2023 <- read.delim("/Users/yueyu/Desktop/ABS/v3/txt_country_benefit_plot/Title_Country_match_2023.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)

title_ref_2023 <- read.delim("/Users/yueyu/Desktop/ABS/v3/txt_reference/Title_ref_match_2023.txt",
                      header = TRUE,
                      fileEncoding = "Latin1",
                      stringsAsFactors = FALSE)
colnames(title_ref_2023)[1] <- "title"


# merge two dataframes based on "title"
merged_2023 <- merge(checked_2023, title_country_2023, by = "title", all.x = TRUE)
merged_2023_all <- merge(merged_2023, title_ref_2023, by = "title", all.x = TRUE)
merged_2023_subset <- merged_2023_all[,c(1,2,5,12,13,14,15)]

# data cleaning
merged_2023_subset$Authors <- gsub("Ê","", merged_2023_subset$Authors)
merged_2023_subset$citation <- gsub("Ê","", merged_2023_subset$citation)

head(merged_2023_subset)
dim(merged_2023_subset)
# 122 7




# ----------
#   2024
# ----------
checked_2024 <- read.delim("/Users/yueyu/Desktop/ABS/v3/txt_benefit_code/ABS_code_2024_type_added_manually_checked.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)

title_country_2024 <- read.delim("/Users/yueyu/Desktop/ABS/v3/txt_country_benefit_plot/Title_Country_match_2024.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)


# merge two dataframes based on "title"
title_ref_2024 <- read.delim("/Users/yueyu/Desktop/ABS/v3/txt_reference/Title_ref_match_2024.txt",
                      header = TRUE,
                      fileEncoding = "Latin1",
                      stringsAsFactors = FALSE)
colnames(title_ref_2024)[1] <- "title"

# merge two dataframes based on "title"
merged_2024 <- merge(checked_2024, title_country_2024, by = "title", all.x = TRUE)
merged_2024_all <- merge(merged_2024, title_ref_2024, by = "title", all.x = TRUE)
merged_2024_subset <- merged_2024_all[,c(1,2,5,12,13,14,15)]

# data cleaning
merged_2024_subset$Authors <- gsub("Ê","", merged_2024_subset$Authors)
merged_2024_subset$citation <- gsub("Ê","", merged_2024_subset$citation)

head(merged_2024_subset)
dim(merged_2024_subset)
# 137 7 




# ----------
#   2025
# ----------
checked_2025 <- read.delim("/Users/yueyu/Desktop/ABS/v3/txt_benefit_code/ABS_code_2025_type_added_manually_checked.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)

title_country_2025 <- read.delim("/Users/yueyu/Desktop/ABS/v3/txt_country_benefit_plot/Title_Country_match_2025.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)


# merge two dataframes based on "title"
title_ref_2025 <- read.delim("/Users/yueyu/Desktop/ABS/v3/txt_reference/Title_ref_match_2025.txt",
                      header = TRUE,
                      fileEncoding = "Latin1",
                      stringsAsFactors = FALSE)
colnames(title_ref_2025)[1] <- "title"

# merge two dataframes based on "title"
merged_2025 <- merge(checked_2025, title_country_2025, by = "title", all.x = TRUE)
merged_2025_all <- merge(merged_2025, title_ref_2025, by = "title", all.x = TRUE)
merged_2025_subset <- merged_2025_all[,c(1,2,5,12,13,14,15)]

# data cleaning
merged_2025_subset$Authors <- gsub("Ê","", merged_2025_subset$Authors)
merged_2025_subset$citation <- gsub("Ê","", merged_2025_subset$citation)

head(merged_2025_subset)
dim(merged_2025_subset)
# 118 7


rm(checked_2023,checked_2024,checked_2025)
rm(title_country_2023,title_country_2024,title_country_2025)
rm(merged_2023,merged_2024,merged_2025)
rm(title_ref_2025,title_ref_2024,title_ref_2023)
rm(merged_2023_all,merged_2024_all,merged_2025_all)










# -----------------------------------------------
#
#    Step 4.2: Pool across 2023-2025  --->   Plot Figure 3b
#
# -----------------------------------------------

pool <- rbind(merged_2023_subset,merged_2024_subset,merged_2025_subset)
pool <- pool[,c(4,5)]

  benefit_summary                               countries
1             A,B                             China,  USA
2             A,B                                     USA
3             A,B                   Brazil , Finland, USA
4             A,B                                     USA
5               A Belgium, Switzerland, Tanzania, UK, USA
6           B,C,E                                     USA



# -- Recode all country name based on if they are in signed country list
# -- If all countires for a publication is within sign list -> YES_ABS_country
# -- If all countires for a publication is NOT within sign list -> NO_ABS_country
# -- If all countires for a publication is mixed with both -> mixed_ABS_country

sign <- read.delim("/Users/yueyu/Desktop/ABS/v3/Signed_country_list.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)

length(sign) # 142 countries signed

sign_countries <- str_trim(sign$countries)

pool <- pool %>%
  rowwise() %>%
  mutate(
    # split and trim, then wrap in list()
    country_list = list(str_split(countries, ",\\s*")[[1]] %>% str_trim()),
    all_in_sign = all(country_list %in% sign_countries),
    any_in_sign = any(country_list %in% sign_countries),
    country_type = case_when(
      all_in_sign ~ "Party (All)",
      !any_in_sign ~ "Non-Party",
      TRUE ~ "Party (Mixed)"
    )
  ) %>%
  ungroup() %>%
  select(-country_list, -all_in_sign, -any_in_sign)



# -- LONG FORMAT

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

summary_prop <- summary_counts %>%
        group_by(country_type) %>%
        mutate(
          total = sum(count),
          prop = round((count / total * 100), digits = 1))

summary_prop$country_type <- factor(
  summary_prop$country_type,
  levels = c("Party (Mixed)", "Non-Party", "Party (All)")
)

pal1 <- c("#81a9ad","#537380","#334540")

p2 <- ggplot(summary_prop, aes(x = benefit_summary, y = prop, fill = country_type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    values = pal1,
    name = ""     # legend title "Nagoya Protocol Status"
  ) +
  labs(
    x = "Benefit category",
    y = "Proportion of publications"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    axis.title.x = element_text(size = 14, margin = margin(t = 8)),
    axis.title.y = element_text(size = 14, margin = margin(r = 8)),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

# Combine plots next to each other
library(patchwork)

combined_plot <- p1 | p2  # use p1 | p2 for side-by-side
combined_plot

# Figure 3 done





# -----------------------------------------------
#
#    Step 5: Finalize Table
#
# -----------------------------------------------


# --------------------
#   2023 --> Table 3
# --------------------

# == Country finalized

merged_2023_subset <- merged_2023_subset %>%
  rowwise() %>%
  mutate(
    # split and trim, then wrap in list()
    country_list = list(str_split(countries, ",\\s*")[[1]] %>% str_trim()),
    all_in_sign = all(country_list %in% sign_countries),
    any_in_sign = any(country_list %in% sign_countries),
    country_type = case_when(
      all_in_sign ~ "Party (All)",
      !any_in_sign ~ "Non-Party",
      TRUE ~ "Party (Partial)"
    )
  ) %>%
  ungroup() %>%
  select(-country_list, -all_in_sign, -any_in_sign)

# == Reference finalized
library(dplyr)
library(stringr)
library(writexl)

merged_2023_ref <- merged_2023_subset %>%
  mutate(
    # Extract the full author string (before the first period, assuming APA format)
    authors_raw = str_extract(citation, "^[^\\(]+"),
    
    # Extract surnames (everything before initials and commas)
    surnames = str_extract_all(authors_raw, "[A-Z][a-zA-Z'’\\-]+(?=,)", simplify = TRUE),
    
    # Extract publication year  ## CHANGE PER YEAR
    year = "2023",
    
    # Count number of authors based on number of extracted surnames
    n_authors = apply(surnames, 1, function(x) sum(x != "")),
    
    # Create in-text citation based on number of authors
    intext_cite = case_when(
      n_authors == 1 ~ paste0(surnames[,1], " (", year, ")"),
      n_authors == 2 ~ paste0(surnames[,1], " & ", surnames[,2], " (", year, ")"),
      n_authors >= 3 ~ paste0(surnames[,1], " et al. (", year, ")"),
      TRUE ~ NA_character_
    )
  )



# == Subset
small_2023 <- merged_2023_ref[,c(4,3,8,12)]
colnames(small_2023) <- c("Benefit Category","Taxon", "Nagoya Protocol Status","Reference")
head(small_2023)
dim(small_2023)


# == SAVE
write.table(small_2023, "/Users/yueyu/Desktop/ABS/v3/Figure_Table/Table3.txt", sep = "\t", row.names = F, col.names = T, quote = F)
write.table(merged_2023_ref, "/Users/yueyu/Desktop/ABS/v3/Figure_Table/Final_Ref_Check/Table3_full_ref.txt", sep = "\t", row.names = F, col.names = T, quote = F)


# --------------------
#   2024 --> Table 4
# --------------------
# == Country finalized

merged_2024_subset <- merged_2024_subset %>%
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

# == Reference finalized
library(dplyr)
library(stringr)
library(writexl)

merged_2024_ref <- merged_2024_subset %>%
  mutate(
    # Extract the full author string (before the first period, assuming APA format)
    authors_raw = str_extract(citation, "^[^\\(]+"),
    
    # Extract surnames (everything before initials and commas)
    surnames = str_extract_all(authors_raw, "[A-Z][a-zA-Z'’\\-]+(?=,)", simplify = TRUE),
    
    # Extract publication year  ## CHANGE PER YEAR
    year = "2024",
    
    # Count number of authors based on number of extracted surnames
    n_authors = apply(surnames, 1, function(x) sum(x != "")),
    
    # Create in-text citation based on number of authors
    intext_cite = case_when(
      n_authors == 1 ~ paste0(surnames[,1], " (", year, ")"),
      n_authors == 2 ~ paste0(surnames[,1], " & ", surnames[,2], " (", year, ")"),
      n_authors >= 3 ~ paste0(surnames[,1], " et al. (", year, ")"),
      TRUE ~ NA_character_
    )
  )



# == Subset
small_2024 <- merged_2024_ref[,c(1,4,3,8,12)]
colnames(small_2024) <- c("Title","Benefit Category","Species Category", "Country Category","Reference")
head(small_2024)
dim(small_2024)


# == SAVE (EXCEL DITECTLY)
write.table(small_2024, "/Users/yueyu/Desktop/ABS/v3/Figure_Table/Table4.txt", sep = "\t", row.names = F, col.names = T, quote = F)
write.table(merged_2024_ref, "/Users/yueyu/Desktop/ABS/v3/Figure_Table/Final_Ref_Check/Table4_full_ref.txt", sep = "\t", row.names = F, col.names = T, quote = F)






# --------------------
#   2025 --> Table 5
# --------------------

# == Country finalized

merged_2025_subset <- merged_2025_subset %>%
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

# == Reference finalized
library(dplyr)
library(stringr)
library(writexl)

merged_2025_ref <- merged_2025_subset %>%
  mutate(
    # Extract the full author string (before the first period, assuming APA format)
    authors_raw = str_extract(citation, "^[^\\(]+"),
    
    # Extract surnames (everything before initials and commas)
    surnames = str_extract_all(authors_raw, "[A-Z][a-zA-Z'’\\-]+(?=,)", simplify = TRUE),
    
    # Extract publication year  ## CHANGE PER YEAR
    year = "2025",
    
    # Count number of authors based on number of extracted surnames
    n_authors = apply(surnames, 1, function(x) sum(x != "")),
    
    # Create in-text citation based on number of authors
    intext_cite = case_when(
      n_authors == 1 ~ paste0(surnames[,1], " (", year, ")"),
      n_authors == 2 ~ paste0(surnames[,1], " & ", surnames[,2], " (", year, ")"),
      n_authors >= 3 ~ paste0(surnames[,1], " et al. (", year, ")"),
      TRUE ~ NA_character_
    )
  )



# == Subset
small_2025 <- merged_2025_ref[,c(1,4,3,8,12)]
colnames(small_2025) <- c("Title","Benefit Category","Species Category", "Country Category","Reference")
head(small_2025)
dim(small_2025)


# == SAVE (EXCEL DITECTLY)
write.table(small_2025, "/Users/yueyu/Desktop/ABS/v3/Figure_Table/Table5.txt", sep = "\t", row.names = F, col.names = T, quote = F)
write.table(merged_2025_ref, "/Users/yueyu/Desktop/ABS/v3/Figure_Table/Final_Ref_Check/Table5_full_ref.txt", sep = "\t", row.names = F, col.names = T, quote = F)



# Manually check any NA for reference 
# Some First author last names can be in special character, not captured in this code, require manual check to fill in "Reference" column

# TABLE 3,4,5 DONE



# END