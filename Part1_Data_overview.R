#####################################
# Project: Benefit sharing
#
# Part 1: Overview of Data from 2023 - 2025
#
# 2025 October 20
# by: Yue Yu
#
#####################################


# Local R studio
# R version: 4.4.0

# -- Step 1: Load raw data (Version 2 by Winnie) and clean

getwd()
setwd("/Users/yueyu/Desktop/ABS/v2/txt_per_year")

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





# -- Step 2: # of papers with benefit statement

library(dplyr)
# ----------
#   2023
# ----------
raw_2023 %>% summarise(blank_or_none = sum(trimws(tolower(benefit)) %in% c("", "none"), na.rm = TRUE))
# 291 blank or none (out of 419)


# ----------
#   2024
# ----------
raw_2024 %>% summarise(blank_or_none = sum(trimws(tolower(benefit)) %in% c("", "none"), na.rm = TRUE))
# 179 blank or none (out of 321)

# ----------
#   2025
# ----------
raw_2025 %>% summarise(blank_or_none = sum(trimws(tolower(benefit)) %in% c("", "none"), na.rm = TRUE))
# 161 blank or none (out of 260)



# -- Step 3: Extract by topic category (DO NOT INCLUDE)

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




# -- Step 4: Extract by NUMBER OF country 

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

write.table(summary_by_num_country_2023, "../Results_202051020/summary_by_num_country_2023.txt", sep = "\t", row.names = F, col.names = T, quote = F)

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

write.table(summary_by_num_country_2024, "../Results_202051020/summary_by_num_country_2024.txt", sep = "\t", row.names = F, col.names = T, quote = F)



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

write.table(summary_by_num_country_2025, "../Results_202051020/summary_by_num_country_2025.txt", sep = "\t", row.names = F, col.names = T, quote = F)



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

# -- Bin data (numerical into categorical)
summary_df <- final_df %>%
  mutate(
    country_bin = cut(
      n_countries,
      breaks = c(0, 2, 5, 8, 11, 14),
      labels = c("0-2", "3-5", "6-8", "9-11", "12-14"),
      include.lowest = TRUE,
      right = TRUE
    )
  ) %>%
  group_by(year, country_bin) %>%
  summarise(
    mean_yes_benefit = mean(yes_benefit_percentage, na.rm = TRUE),
    n = n()  # optional: number of observations per bin
  ) %>%
  arrange(year, country_bin)

# -- set levels to categorical data, then can plot as lines (to connect them)
summary_df$country_bin <- factor(summary_df$country_bin,
															  levels = c("0-2", "3-5", "6-8", "9-11", "12-14"),
															  ordered = TRUE)


ggplot(summary_df, aes(x = country_bin, y = mean_yes_benefit, group = factor(year), color = factor(year))) +
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
  count(countries, name = "NO_benefit_count") %>%
  arrange(desc(NO_benefit_count))

name_YES_benefit_2023 <- raw_2023 %>%
  filter(benefit != "") %>%
  select(countries) %>%
  separate_rows(countries, sep = ",") %>%
  mutate(countries = trimws(countries), countries = str_to_title(countries)) %>%
  count(countries, name = "YES_benefit_count") %>%
  arrange(desc(YES_benefit_count))

# Merge, show all countires with benefit and without 
name_country_all_2023 <- full_join(
															  name_NO_benefit_2023,
															  name_YES_benefit_2023,
															  by = "countries") %>%
															  # Replace missing values (if a country only appears in one df) with 0
															  mutate(
															    NO_benefit_count = replace_na(NO_benefit_count, 0),
															    YES_benefit_count = replace_na(YES_benefit_count, 0)) %>% 
															  mutate(sum = NO_benefit_count + YES_benefit_count) %>%
															  mutate(NO_benefit_pro = NO_benefit_count/sum) %>%
															  mutate(YES_benefit_pro = YES_benefit_count/sum) %>%
															  arrange(desc(YES_benefit_pro))


write.table(name_country_all_2023, "../Results_202051020/Ranked_country_name_2023.txt", sep = "\t", row.names = F, col.names = T, quote = F)




# ----------
#   2024
# ----------

# Regex wildcard replace odd country name inputs
raw_2024$countries <- gsub("T.?Rkiye", "Turkey", raw_2024$countries, ignore.case = TRUE)

name_NO_benefit_2024 <- raw_2024 %>%
  filter(benefit == "") %>%
  select(countries) %>%
  separate_rows(countries, sep = ",") %>%
  mutate(countries = trimws(countries), countries = str_to_title(countries)) %>%
  count(countries, name = "NO_benefit_count") %>%
  arrange(desc(NO_benefit_count))


name_YES_benefit_2024 <- raw_2024 %>%
  filter(benefit != "") %>%
  select(countries) %>%
  separate_rows(countries, sep = ",") %>%
  mutate(countries = trimws(countries), countries = str_to_title(countries)) %>%
  count(countries, name = "YES_benefit_count") %>%
  arrange(desc(YES_benefit_count))


name_country_all_2024 <- full_join(
															  name_NO_benefit_2024,
															  name_YES_benefit_2024,
															  by = "countries") %>%
															  # Replace missing values (if a country only appears in one df) with 0
															  mutate(
															    NO_benefit_count = replace_na(NO_benefit_count, 0),
															    YES_benefit_count = replace_na(YES_benefit_count, 0)) %>% 
															  mutate(sum = NO_benefit_count + YES_benefit_count) %>%
															  mutate(NO_benefit_pro = NO_benefit_count/sum) %>%
															  mutate(YES_benefit_pro = YES_benefit_count/sum) %>%
															  arrange(desc(YES_benefit_pro))

write.table(name_country_all_2024, "../Results_202051020/Ranked_country_name_2024.txt", sep = "\t", row.names = F, col.names = T, quote = F)




# ----------
#   2025
# ----------

# Regex wildcard replace odd country name inputs
raw_2025$countries <- gsub("C.?Te D'ivoire", "Cote dIvoire", raw_2025$countries, ignore.case = TRUE)
raw_2025$countries <- gsub("C.?Te Dõivoire", "Cote dIvoire", raw_2025$countries, ignore.case = TRUE)
raw_2025$countries <- gsub("R.?Publique De Guin.?E", "Republique de Guinee", raw_2025$countries, ignore.case = TRUE)


name_NO_benefit_2025 <- raw_2025 %>%
  filter(benefit == "") %>%
  select(countries) %>%
  separate_rows(countries, sep = ",") %>%
  mutate(countries = trimws(countries), countries = str_to_title(countries)) %>%
  count(countries, name = "NO_benefit_count") %>%
  arrange(desc(NO_benefit_count))

name_YES_benefit_2025 <- raw_2025 %>%
  filter(benefit != "") %>%
  select(countries) %>%
  separate_rows(countries, sep = ",") %>%
  mutate(countries = trimws(countries), countries = str_to_title(countries)) %>%
  count(countries, name = "YES_benefit_count") %>%
  arrange(desc(YES_benefit_count))
  
name_country_all_2025 <- full_join(
															  name_NO_benefit_2025,
															  name_YES_benefit_2025,
															  by = "countries") %>%
															  # Replace missing values (if a country only appears in one df) with 0
															  mutate(
															    NO_benefit_count = replace_na(NO_benefit_count, 0),
															    YES_benefit_count = replace_na(YES_benefit_count, 0)) %>% 
															  mutate(sum = NO_benefit_count + YES_benefit_count) %>%
															  mutate(NO_benefit_pro = NO_benefit_count/sum) %>%
															  mutate(YES_benefit_pro = YES_benefit_count/sum) %>%
															  arrange(desc(YES_benefit_pro))

unique(name_country_all_2025$countries)

write.table(name_country_all_2025, "../Results_202051020/Ranked_country_name_2025.txt", sep = "\t", row.names = F, col.names = T, quote = F)

# --- CONCLUSION --
# Can use Binomial Confidence Interval for each country and then see the observed value of YES-BENEFIT if it falls within the predicted range
# But this is not the focus of the study, so I will not do this step
# Conclusion for this part: it is not very compariable, just to get an idea of each country's publication count and proportion of including ABS









# -- Step 6: Key words extract (for Nagoya Protocol)
# Link to ABS: https://www.cbd.int/abs/text/articles?sec=abs-37

collaboration OR coauthorship
technology transfer
capacity building OR training
contributions to the local economy (hire a local guide etc)
research towards conservation
database






# END