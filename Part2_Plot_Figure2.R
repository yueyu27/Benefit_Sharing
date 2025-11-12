
#####################################
# Project: Benefit sharing
#
# Part 2: Benefit VS NO-Benefit for countries & species
#
# 2025 Nov 10
# by: Yue Yu
#
#####################################


# Local R studio
# R version: 4.4.0

library(dplyr)
library(tidyr)
library(stringr)
library(PNWColors)
library(ggplot2)
library(patchwork)



# -------------------------------------------
#
# -- Step 1: Load raw cleaned data <--- from Part 0 code
#
# -------------------------------------------


# ----------
#   2023
# ----------
new_raw_2023_clean <- read.delim("/Users/yueyu/Desktop/ABS/v3/txt_raw_clean/raw_clean_2023.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)

unique(new_raw_2023_clean$species)
#[1] "seed_plant"   "invertebrate" "ecosystem"    "other"       
#[5] "vertebrate"   "fungi"        "bacteria"


# ----------
#   2024
# ----------
new_raw_2024_clean <- read.delim("/Users/yueyu/Desktop/ABS/v3/txt_raw_clean/raw_clean_2024.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)

new_raw_2024_clean$species <- gsub(" vertebrate", "vertebrate", new_raw_2024_clean$species)

unique(new_raw_2024_clean$species)
#[1] "vertebrate"   "invertebrate" "bacteria"     "other"       
#[5] "seed_plant"   "ecosystem"    "fungi"


# ----------
#   2025
# ----------
new_raw_2025_clean <- read.delim("/Users/yueyu/Desktop/ABS/v3/txt_raw_clean/raw_clean_2025.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)

unique(new_raw_2025_clean$species)
#[1] "vertebrate"   "other"        "invertebrate" "seed_plant"  
#[5] "ecosystem"    "fungi"        "bacteria"



# -----------------------------------------------
#
#    Step 2: Load 142 countires signed Nygoya Protocol
#
# -----------------------------------------------

sign <- read.delim("/Users/yueyu/Desktop/ABS/v3/Signed_country_list.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)

length(sign) # 142 countries signed




# -------------------------------------------
#
# -- Step 3: Pool --> Categorize countries according ot Nagoya
#
# -------------------------------------------

pool <- rbind(new_raw_2023_clean,new_raw_2024_clean,new_raw_2025_clean)
pool <- pool[,c(1,2,4,5)]

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





# -------------------------------------------
#
# -- Step 4: with/withOUT benefit sharing
#
# -------------------------------------------

# Create a new column to indicate whether 'benefit' is blank or not
pool <- pool %>%
  mutate(benefit_status = ifelse(benefit == "" | is.na(benefit),
                                 "blank", "not_blank"))

# 1. Summarize counts by country_type
country_summary <- pool %>%
  group_by(country_type, benefit_status) %>%
  summarise(count = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = benefit_status, values_from = count, values_fill = 0) %>%
  mutate(
    sum = blank + not_blank,
    non_benefit_ratio = round(not_blank / sum * 100, digits = 1),
    country_type = factor(country_type, levels = country_type[order(-not_blank)])
  )


# country_type    blank not_blank 
#  Non-Party         209       121
#  Party (Full)      231        97
#  Party (Mixed)   255       162



# 2. Summarize counts by species
species_summary <- pool %>%
  group_by(species, benefit_status) %>%
  summarise(count = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = benefit_status, values_from = count, values_fill = 0) %>%
  mutate(
    sum = blank + not_blank,
    non_benefit_ratio = round(not_blank / sum * 100, digits = 1),
    species = factor(species, levels = species[order(-not_blank)])
  )



# species      blank not_blank 
# bacteria        16        15  
# ecosystem       71        21 
# fungi           19        13 
# invertebrate   167        86 
# other           78         7 
# seed_plant      78        62 
# vertebrate     266       176 





# -------------------------------------------
#
# -- Step 5: Plotting
#
# -------------------------------------------


#  == Species plot

species_long <- species_summary %>%
  select(species, blank, not_blank) %>%
  pivot_longer(cols = c(blank, not_blank), names_to = "benefit_status", values_to = "count") %>%
  mutate(count = ifelse(benefit_status == "blank", -count, count))
# Blank == No benefit == negative

# Vertical bar plot
pal2 <- pnw_palette("Moth", 9)

a <- ggplot(species_long, aes(x = species, y = count, fill = species)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  scale_fill_manual(values = pal2) +        # defined color palette
    scale_y_continuous(
    limits = c(-300, 200),                     # set y-axis range
    breaks = seq(-300, 200, by = 100),          # define breaks
    labels = abs(seq(-300, 200, by = 100))      # show all labels as positive
  ) +     
  labs(
    x = "Taxon",
    y = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 15, margin = margin(t = 8), color = "black"),
    axis.title.y = element_text(size = 15, margin = margin(r = 8), color = "black"),
    axis.text.x = element_text(size = 12, angle = 30, hjust = 1, color = "black"),
    axis.text.y = element_text(size = 12, color = "black")
  )




#  == Country plot

country_long <- country_summary %>%
  select(country_type, blank, not_blank) %>%
  pivot_longer(cols = c(blank, not_blank), names_to = "benefit_status", values_to = "count") %>%
  mutate(count = ifelse(benefit_status == "blank", -count, count))
# Blank == No benefit == negative

# Vertical bar plot
pal1 <- c("#81a9ad","#537380","#334540")

b <- ggplot(country_long, aes(x = country_type, y = count, fill = country_type)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  scale_fill_manual(values = pal1) +        # defined color palette
    scale_y_continuous(
    limits = c(-300, 200),                     # set y-axis range
    breaks = seq(-300, 200, by = 100),          # define breaks
    labels = abs(seq(-300, 200, by = 100))      # show all labels as positive
  ) +     
  labs(
    x = "Nagoya Protocol Status",
    y = "Number of publications"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 15, margin = margin(t = 8), color = "black"),
    axis.title.y = element_text(size = 15, margin = margin(r = 8), color = "black"),
    axis.text.x = element_text(size = 12, angle = 30, hjust = 1, color = "black"),
    axis.text.y = element_text(size = 12, color = "black")
  )





# Combine plots next to each other

combined_plot <- b | a  # use p1 | p2 for side-by-side
combined_plot

# Figure 2 done


# END