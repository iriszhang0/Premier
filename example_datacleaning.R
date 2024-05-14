############################################
## Example data cleaning script
## Author: Zoe Haskell-Craig
## Date Created: May 2nd 2024
## Date Last Modified: May 2nd 2024
#############################################

# Packages ------------------

library(haven)
library(dplyr)
library(tidyr)


# Load and merge data ----------------------
setwd("/scratch/Premier/Raw_Data")

print("loading .... demo")
print(Sys.time())
demo <- read_dta("_patdemo/nyu_allyears_patdemo.dta")
print(Sys.time())

print("loading .... diagnosis")
setwd("/scratch/Premier/Raw_Data/_paticd_diag")
print(Sys.time())
load(file = "nyu_allyears_diagnosis.RData")
print(Sys.time())

print("merging")
merged_data <- left_join(demo, data, by = "pat_key")
print(Sys.time())

print("loading .... aprdrg")
setwd("/scratch/Premier/Raw_Data/_pataprdrg")
print(Sys.time())
load(file = "nyu_allyears_aprdrg.RData")
print(Sys.time())

print("merging")
merged_data <- left_join(merged_data, all_aprdrg, by = "pat_key")
print(Sys.time())

print("loading ... region")
setwd("/scratch/Premier/Raw_Data")
print(Sys.time())
region <- read.delim("nyu_providers.txt",sep = "|")
print(Sys.time())

print("merging")
#colnames(region) <- tolower(colnames(region))
merged_data <- left_join(merged_data, region, by = c("prov_id" = "PROV_ID"))
print(Sys.time())

## proc files ------------------------
# these are not merged due to size reasons 
# import ICD PROC Files takes ~ 15 mins
setwd("/scratch/Premier/Raw_Data/_paticd_proc")
print("loading .... proc")
print(Sys.time())
load("nyu_allyears_proc.RData")
print(Sys.time())

# Generate covariates ------------

## ARDS/J96 ----------------
print("creating ARDS")
merged_data <- merged_data %>%
  mutate(ARDS = if_else(stringr::str_detect(diagnoses_all, "J80"), 1, 0))

print("creating J96")
merged_data <- merged_data %>%
  mutate(J96 = if_else(stringr::str_detect(diagnoses_all, "J96"), 1, 0))

## Death ---------------
#create death (anytime) variable
print("creating death variable")
merged_data$death <- ifelse(merged_data$disc_status %in% c(20, 40, 41, 42), 1, 0)

#creating death or hospice transfer
print("creating death or hospice transfer variable")
merged_data$death_or_hospice <- ifelse(merged_data$disc_status %in% 
                                         c(20, 40, 41, 42, 50, 51), 1, 0)

#create in-hospital mortality
print("creating in-hospital mortality varibale")
merged_data$inhospital_death <- ifelse(merged_data$disc_status == 20, 1, 0)

## Race/ethnicity -----------------

#merge race and hispanicity
merged_data <- merged_data %>%
  mutate(race_ethnicity = case_when(
    hispanic_ind == "Y" ~ "Hispanic", 
    race == "B" & hispanic_ind != "Y" ~ "nonHispanic_Black",
    race == "W" & hispanic_ind != "Y" ~ "nonHispanic_White",
    race == "A" & hispanic_ind != "Y" ~ "Asian",
    race == "U" ~ "Unknown",
    .default = "Other"))
merged_data$race_ethnicity <- factor(merged_data$race_ethnicity,
                                     levels = c("nonHispanic_White", "nonHispanic_Black",
                                                "Hispanic", "Asian",  "Unknown",
                                                "Other"))

## Obesity --------------------------
# obesity diagnosis codes BMI > 30
# Z code of str_detect "Z68.3" OR
# diagnosis of obesity "E66.0", "E66.1" "E66.2" "E66.8" "E66.9"
print("creating obesity variable")
merged_data <- merged_data %>%
  mutate(E66 = if_else(stringr::str_detect(diagnoses_all, "E66"), 1, 0),
         E66.3 = if_else(stringr::str_detect(diagnoses_all, "E66.3"), 1, 0),
         obesity = if_else((E66 == 1) & (E66.3 == 0), 1, 0)) #obesity for any E66 diagnosis except E66.3


## Insurance -----------------
# insurance type
print("creating insurance variable")
merged_data <- merged_data %>%
  mutate(insurance = case_when(std_payor %in% c(300, 310, 320) ~ "medicare",
                               std_payor %in% c(330, 340, 350) ~ "medicaid",
                               std_payor %in% c(360, 370, 380) ~ "private",
                               .default = "other"))

merged_data$insurance <- factor(merged_data$insurance, 
                                levels = c("private", "medicaid", "medicare",
                                           "other"))

## Mechanical ventilation -------------------
# 2.filter for invasive mechanical ventilation codes
mech_vent_icd_codes = c('5A1935Z', '5A1945Z', '5A1955Z') # mechanical ventilation ICD codes

mech_vent_patients <- all_proc %>%
  filter(all_proc_codes %in% mech_vent_icd_codes)


# 3.select unique patient keys to identify which patients had a mech. vent. code
pat_with_mech_vent <- unique(mech_vent_patients$pat_key)

# 4. create dummy variable in dataset with 1 (or 0) for mech. vent (or not)
merged_data$mech_vent <- ifelse(merged_data$pat_key %in% pat_with_mech_vent, 1, 0)



## Hospital Region -------------------------------------
print("creating region variable for hosptials")
#merge middle atlantic and new england
merged_data[merged_data$PROV_DIVISION == "MIDDLE ATLANTIC",]$PROV_DIVISION <- "NEW ENGLAND"
#merge mountain and WEST NORTH CENTRAL
merged_data[merged_data$PROV_DIVISION == "MOUNTAIN",]$PROV_DIVISION <- "WEST NORTH CENTRAL"
table(merged_data$PROV_DIVISION)







# Other data cleaning operations --------------------
## filter to just ARDS (J80) patients ----------------
ARDS_data <- merged_data %>%
  filter(ARDS == 1)

## drop "unknown" as missing ----------------


ARDS_data <- ARDS_data %>%
  filter(gender != "U" & #dropped 47 observations
           race_ethnicity != "Unknown") 