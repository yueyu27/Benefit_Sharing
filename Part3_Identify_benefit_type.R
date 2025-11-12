#####################################
# Project: Benefit sharing
#
# Part 3: Extract Benefit type
#
# 2025 October 28
# by: Yue Yu
#
#####################################


# -----------------------------------------------
#
#    Step 1: Determine benefit type    
#
# -----------------------------------------------

# --- Benefit type code used for this analyses  --- 

# Non-monetary benefits	Keywords	Corresponding to ABS
# A	Collaboration and coauthorship	a,b,n,q,p
# B	Database sharing	e,k
# C	Capacity building and training, technology transfer	d,g,h,i, j,
# D	Contribution to local economy	c,l,o
# E	Reseach towards priority needs	m
# F	Consent and equality	f

# ----------------------------------------------- 




# -----------------------------------------------
#
#     Step 2: Load benefit type raw data   
#
# -----------------------------------------------

# Local R studio
# R version: 4.4.0

getwd()
setwd("/Users/yueyu/Desktop/ABS/v2/txt_benefit_code")
library(dplyr)

# ----------
#   2023
# ----------
b_2023 <- read.delim("ABS_code_2023.txt",
                      header = TRUE,
                      stringsAsFactors = FALSE)

b_2023 <- b_2023[,1:5]

head(b_2023)
dim(b_2023)
# 123 4
# There is a duplicated line, thus only 122 publications have benefit sharing statements


# ----------
#   2024
# ----------
b_2024 <- read.delim("ABS_code_2024.txt",
                      header = TRUE,
                      stringsAsFactors = FALSE)

b_2024 <- b_2024[,1:5]

head(b_2024)
dim(b_2024)
# 137 4


# ----------
#   2025
# ----------
b_2025 <- read.delim("ABS_code_2025.txt",
                      header = TRUE,
                      stringsAsFactors = FALSE)

b_2025 <- b_2025[,1:5]

head(b_2025)
dim(b_2025)
# 119 4
# There is a duplicated line, thus only 118 publications have benefit sharing statements




# -----------------------------------------------
#
#    Step 3: Use keywords to extract benefit type  
#
# -----------------------------------------------

#  -- Add benefit type A 
#  -- Method used: manually extracted from article by Winnie Chueng

b_2023 <- b_2023 %>%
  mutate(benefit_A = ifelse(coauthorship_ABS == "yes", "A", NA))


#  -- Add benefit type B 
#  -- Method used: keyword extraction "data" & ("sharing" | "share" | "shared" | "deposite" )

# Other keywords: available, archived 

b_2023 <- b_2023 %>%
	mutate(benefit_B = ifelse(grepl("data", benefit, ignore.case = TRUE) & (grepl("share[a-z]*", benefit,ignore.case = TRUE ) |  grepl("sharing", benefit,ignore.case = TRUE ) | grepl("deposite[a-z]*", benefit, ignore.case = TRUE)), "B", NA))

# "share[a-z]*" matches any word starting with “share” followed by zero or more letters: covering share, shares, shared, sharing, etc.
# & ensures both “data” and any “share*” appear in the same string.
# ignore.case = TRUE makes matching case-insensitive.


#  -- Add benefit type C
#  -- Method used: keyword extraction "training" | "train" | "capacity" 

b_2023 <- b_2023 %>%
	mutate(benefit_C = ifelse( grepl("train[a-z]*", benefit,ignore.case = TRUE ) |  grepl("training", benefit,ignore.case = TRUE) | grepl("capacity", benefit,ignore.case = TRUE), "C", NA))


#  -- Add benefit type D
#  -- Method used: keyword extraction "economy" | "hire" | "pay"
#  -- Require additional manual check as some key word is hard to capture this benefit

b_2023 <- b_2023 %>%
	mutate(benefit_D = ifelse( grepl("economy", benefit,ignore.case = TRUE) | grepl("hire[a-z]*", benefit,ignore.case = TRUE) | grepl("pay[a-z]*", benefit,ignore.case = TRUE) , "D", NA))


#  -- Add benefit type E
#  -- Method used: keyword extraction "priority" | "conservation" | "urgent"

b_2023 <- b_2023 %>%
	mutate(benefit_E = ifelse( grepl("priority", benefit,ignore.case = TRUE) | grepl("conservation", benefit,ignore.case = TRUE) | grepl("urgent", benefit,ignore.case = TRUE) , "E", NA))


#  -- Add benefit type F
#  -- Method used: keyword extraction "consent" | "permit" | "equal" 

b_2023 <- b_2023 %>%
	mutate(benefit_F = ifelse( grepl("consent", benefit,ignore.case = TRUE) | grepl("permit", benefit,ignore.case = TRUE)| grepl("equal", benefit,ignore.case = TRUE) , "F", NA))


#  -- Data cleaning before output
b_2023 <- b_2023 %>% select(-coauthorship_ABS)

# -----------------------------------------------
#
#    Step 4: Output & save as txt table
#
# -----------------------------------------------

write.table(b_2023, file="ABS_code_2023_type_added.txt", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE, na = "")


# -----------------------------------------------
#
#    Step 5: Manually check all assigned benefit type (EXCEL)
#
# -----------------------------------------------


# -----------------------------------------------
#
#    Step 6: Merge all benefit into a single column (EXCEL)
#
# -----------------------------------------------

# TEXTJOIN(",", TRUE, A2:C2)


# -----------------------------------------------
#
#    Step 7: Read in manually updated benefit summary to R and add reference column
#
# -----------------------------------------------





# -- same code applied in one line to 2024 and 2025

# ----------
#   2024
# ----------
b_2024 <- b_2024 %>%
  mutate(benefit_A = ifelse(coauthorship_ABS == "yes", "A", NA))

b_2024 <- b_2024 %>%
  mutate(benefit_B = ifelse(grepl("data", benefit, ignore.case = TRUE) & (grepl("share[a-z]*", benefit,ignore.case = TRUE ) |  grepl("sharing", benefit,ignore.case = TRUE ) | grepl("deposite[a-z]*", benefit, ignore.case = TRUE)), "B", NA)) %>%
  mutate(benefit_C = ifelse( grepl("train[a-z]*", benefit,ignore.case = TRUE ) |  grepl("training", benefit,ignore.case = TRUE) | grepl("capacity", benefit,ignore.case = TRUE), "C", NA)) %>%
  mutate(benefit_D = ifelse( grepl("economy", benefit,ignore.case = TRUE) | grepl("hire[a-z]*", benefit,ignore.case = TRUE) | grepl("pay[a-z]*", benefit,ignore.case = TRUE) , "D", NA)) %>%
  mutate(benefit_E = ifelse( grepl("priority", benefit,ignore.case = TRUE) | grepl("conservation", benefit,ignore.case = TRUE) | grepl("urgent", benefit,ignore.case = TRUE) , "E", NA)) %>%
  mutate(benefit_F = ifelse( grepl("consent", benefit,ignore.case = TRUE) | grepl("permit", benefit,ignore.case = TRUE)| grepl("equal", benefit,ignore.case = TRUE) , "F", NA)) %>% 
  select(-coauthorship_ABS)


write.table(b_2024, file="ABS_code_2024_type_added.txt", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE, na = "")



# ----------
#   2025
# ----------
b_2025 <- b_2025 %>%
  mutate(benefit_A = ifelse(coauthorship_ABS == "yes", "A", NA))

b_2025 <- b_2025 %>%
  mutate(benefit_B = ifelse(grepl("data", benefit, ignore.case = TRUE) & (grepl("share[a-z]*", benefit,ignore.case = TRUE ) |  grepl("sharing", benefit,ignore.case = TRUE ) | grepl("deposite[a-z]*", benefit, ignore.case = TRUE)), "B", NA)) %>%
  mutate(benefit_C = ifelse( grepl("train[a-z]*", benefit,ignore.case = TRUE ) |  grepl("training", benefit,ignore.case = TRUE) | grepl("capacity", benefit,ignore.case = TRUE), "C", NA)) %>%
  mutate(benefit_D = ifelse( grepl("economy", benefit,ignore.case = TRUE) | grepl("hire[a-z]*", benefit,ignore.case = TRUE) | grepl("pay[a-z]*", benefit,ignore.case = TRUE) , "D", NA)) %>%
  mutate(benefit_E = ifelse( grepl("priority", benefit,ignore.case = TRUE) | grepl("conservation", benefit,ignore.case = TRUE) | grepl("urgent", benefit,ignore.case = TRUE) , "E", NA)) %>%
  mutate(benefit_F = ifelse( grepl("consent", benefit,ignore.case = TRUE) | grepl("permit", benefit,ignore.case = TRUE)| grepl("equal", benefit,ignore.case = TRUE) , "F", NA)) %>% 
  select(-coauthorship_ABS)

write.table(b_2025, file="ABS_code_2025_type_added.txt", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE, na = "")





# END