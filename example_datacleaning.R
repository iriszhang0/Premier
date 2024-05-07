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
  mutate(ARDS = if_else(stringr::str_detect(all_diagnoses, "J80"), 1, 0))

print("creating J96")
merged_data <- merged_data %>%
  mutate(J96 = if_else(stringr::str_detect(all_diagnoses, "J96"), 1, 0))

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
  mutate(E66 = if_else(stringr::str_detect(all_diagnoses, "E66"), 1, 0),
         E66.3 = if_else(stringr::str_detect(all_diagnoses, "E66.3"), 1, 0),
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



## Charlson Comorbidity Index --------------------------
MI_codes <- c("I21", "I22", "I25.2")

congestive_heart_codes <- c("I11.0", "I13.0", "I13.2", "I25.5",
                            "I42.0", "I42.5", "I42.6", "I42.7",
                            "I42.8", "I42.9", "I43", "150", "P29.0")

peripheral_vascular_codes <- c("I70", "I71", "I73.1", "I73.8", "I73.9",
                               "I77.1", "I79.0", "I79.1", "I79.8",
                               "K55.1", "K55.8", "K55.9", "Z95.8", 
                               "Z95.9")

cerebrovascular_disease_codes <- c("G45", "G46", "H34.0", "H34.1",
                                   "H34.2", "I60", "I61", "I62", "I63",
                                   "I64", "I65", "I66", "I67", "I68")

dementia_codes <- c("F01", "F02", "F03", "F04", "F05", "F06.1", 
                    "F06.8", "G13.2", "G13.8", "G30", "G31.0",
                    "G31.2", "G91.4", "G94", "R41.81", "R54")

chronic_pulmonary_codes <- c("J40", "J41", "J42", "J43", "J44",
                             "J45", "J46", "J47", "J60", "J61",
                             "J62", "J63", "J64", "J65", "J66",
                             "J67", "J68.4", "J70.1", "J70.3")

rheumatic_disease_codes <- c("M05", "M06", "M31.5", "M32", "M33",
                             "M34", "M35.1", "M35.3", "M36.0")

peptic_ulcer_codes <- c("K25", "K26", "K27", "K28")

mild_liver_codes <- c("B18", "K70.0", "K70.1", "K70.2", "K70.3",
                      "K70.9", "K71.3", "K71.4", "K71.5",
                      "K71.7", "K73", "K74", "K76.0", "K76.2",
                      "K76.3", "K76.4", "K76.8", "K76.9", "Z94.4")

#note the wildcards in this set of codes
diabetes_wo_complications_codes <- c("E08", "E09", "E10", "E11", 
                                     "E13", "E..\\.0", "E..\\.1",
                                     "E..\\.6", "E..\\.8", "E..\\.9")

renal_mildmoderate_codes <- c("I12.9", "I13.0", "I13.10",
                              "N03", "N05", "N18.1", "N18.2",
                              "N18.3", "N18.4", "N18.9", "Z94.0")


AIDS_codes <- c("B37", "C53","B38", "B45", "A07.2", "B25", "G93.4",
                "B00", "B39", "A07.3", "C46", "C81", "C82", "C83",
                "C84", "C85", "C86", "C87", "C88", "C89", "C90",
                "C91", "C92", "C93", "C94", "C95", "C96", "A31",
                "A15", "A16", "A17", "A18", "A19", "B59", "Z87.01",
                "A81.2", "A02.1", "B58", "R64")

Metastatic_Solid_Tumor <- c("C77", "C78", "C79", "C80.0", "C80.2")

HIV_Infection <- c("B20")

Renal_Severe <- c("I12.0", "I13.11", "I13.2", "N18.5", "N18.6", "N19", "N25.0",
                  "Z49", "Z99.2")
Moderate_or_Severe_Liver_Disease <- c("I85.0", "I86.4", "K70.4", "K71.1",
                                      "K72.1", "K72.9", "K76.5", "K76.6",
                                      "K76.7")
Any_Malignancy_Lymphoma_Leukemia_except_Malignant_Nonmelanoma_Neoplasm_of_skin <- c("C0", "C1", "C2", "C30", "C31",
                                                                                    "C32", "C33", "C34", "C37", "C38",
                                                                                    "C39", "C40", "C41", "C43", "C45", 
                                                                                    "C46", "C47", "C48", "C49", "C50",
                                                                                    "C51", "C52", "C53", "C54", "C55",
                                                                                    "C56", "C57", "C58", "C60", "C61", 
                                                                                    "C62", "C63", "C76", "C80.1", "C81",
                                                                                    "C82", "C83", "C84", "C85", "C88", "C9")

Hemiplegia_or_Paraplegia <- c("G04.1", "G11.4", "G80.0",
                              "G80.1", "G80.2", "G81",
                              "G82", "G83")

Diabetes_with_Chronic_Complications <- c("E08", "E09", 
                                         "E10", "E11", "E13")




#Charlson Comorbidity Index (CCI) - hierarchy rule
###Rule 1
priority_dx1 <- function(diagnoses_all) {
  if ("hemiplegia" %in% diagnoses_all) {
    return("hemiplegia")
  } else if ("cerebrovascular disease" %in% diagnoses_all) {
    return("cerebrovascular disease")
  } else {
    return(NA)
  }
}

###Rule 2
priority_dx2 <- function(diagnoses_all) {
  if ("mod-sev liver dis" %in% diagnoses_all) {
    return("mod-sev liver dis")
  } else if ("mild liver dis" %in% diagnoses_all) {
    return("mild liver dis")
  } else {
    return(NA)
  }
}

###Rule 3
priority_dx3 <- function(diagnoses_all) {
  if ("complicated DM" %in% diagnoses_all) {
    return("complicated DM")
  } else if ("uncomplicated DM" %in% diagnoses_all) {
    return("uncomplicated DM")
  } else {
    return(NA)
  }
}

###Rule 4
priority_dx4 <- function(diagnoses_all) {
  if ("severe renal" %in% diagnoses_all) {
    return("severe renal")
  } else if ("mild-mod renal" %in% diagnoses_all) {
    return("mild-mod renal")
  } else {
    return(NA)
  }
}

###Rule 5
priority_dx5 <- function(diagnoses_all) {
  if ("solid tumor" %in% diagnoses_all) {
    return("solid tumor")
  } else if ("malignancy" %in% diagnoses_all) {
    return("malignancy")
  } else {
    return(NA)
  }
}


###Rule 6
priority_dx6 <- function(diagnoses_all) {
  if ("AIDS" %in% diagnoses_all) {
    return("AIDS")
  } else if ("HIV" %in% diagnoses_all) {
    return("HIV")
  } else {
    return(NA)
  }
}
      





# Other data cleaning operations --------------------
## filter to just ARDS (J80) patients ----------------
ARDS_data <- merged_data %>%
  filter(ARDS == 1)

## drop "unknown" as missing ----------------


ARDS_data <- ARDS_data %>%
  filter(gender != "U" & #dropped 47 observations
           race_ethnicity != "Unknown") 