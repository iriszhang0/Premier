###Setting working directory to shared folder in AWS
setwd("Y:\export_2025-01-08-193652")

# load and merge data ----------------------
setwd("/scratch/Premier/Raw_Data")


library(haven)
library(dplyr)
library(tidyr)
library(lme4) 
library(performance)
library(stringr)


print("loading .... demo")
print(Sys.time())
demo <- read_dta("_patdemo/nyu_allyears_patdemo.dta")
print(Sys.time())


print("loading .... diagnosis")
setwd("/scratch/Premier/Raw_Data/_paticd_diag")
print(Sys.time())
load(file = "nyu_allyears_diagnosis.RData")
print(Sys.time())


##For AWS
#print("loading .... diagnosis")
#print(Sys.time())
#load(file = "_paticd_diag/nyu_allyears_diagnosis.RData")
#print(Sys.time())

print("merging")
merged_data <- left_join(demo, data, by = "pat_key")
print(Sys.time())


print("loading .... aprdrg")
setwd("/scratch/Premier/Raw_Data/_pataprdrg")
print(Sys.time())
load(file = "nyu_allyears_aprdrg.RData")
print(Sys.time())

#For AWS
#print("loading .... aprdrg")
#print(Sys.time())
#load(file = "_pataprdrg/nyu_allyears_aprdrg.RData")
#print(Sys.time())

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


#create respiratory variables and covariates ------------

print("creating ARDS")
merged_data <- merged_data %>%
  mutate(ARDS = if_else(stringr::str_detect(diagnoses_all, "J80"), 1, 0))

print("creating J96")
merged_data <- merged_data %>%
  mutate(J96 = if_else(stringr::str_detect(diagnoses_all, "J96"), 1, 0))


#create death (anytime) variable
print("creating death variable")
merged_data$death <- ifelse(merged_data$disc_status %in% c(20, 40, 41, 42), 1, 0)

#creating death or hospice transfer
print("creating death or hospice transfer variable")
merged_data$death_or_hospice <- ifelse(merged_data$disc_status %in% 
                                         c(20, 40, 41, 42, 50, 51), 1, 0)

#create in-hospital mortality
print("creating in-hospital mortality variable")
merged_data$inhospital_death <- ifelse(merged_data$disc_status == 20, 1, 0)

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

#create outcome multinomial from disc_status
# (hospice = 40, 41, 42, 50, 51 death or not*is it okay if we are mixing expired or not), *separate death from hospice* 
# (interhospital = 2, 43, 66, 70)
# (transitional care = 61, 71 + 72 (outpatient), )
# (long term care = 3, 4 (intermediate care), 62, 63, 64)
# (home, no indication of hospice =  1, 6 (home health org), 8 (home IV provider))
# (other )
# what does "planned hospital readmission" mean? planned hospital readmission = c(81 - 95)
# * hospice, death (including died in hospice --> same as primary analysis), home other
#ARDS_data$outcome


# obesity diagnosis codes BMI > 30
# Z code of str_detect "Z68.3" OR
# diagnosis of obesity "E66.0", "E66.1" "E66.2" "E66.8" "E66.9"
print("creating obesity variable")
merged_data <- merged_data %>%
  mutate(E66 = if_else(stringr::str_detect(diagnoses_all, "E66"), 1, 0),
         E66.3 = if_else(stringr::str_detect(diagnoses_all, "E66.3"), 1, 0),
         obesity = if_else((E66 == 1) & (E66.3 == 0), 1, 0)) #obesity for any E66 diagnosis except E66.3

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


## Dropping outpatient visits
merged_data <- merged_data %>%
  filter(i_o_ind != "O")

## invasive mechanical ventilator coding, recreated from M. Ghous's code --------------

# 1.import ICD PROC Files
setwd("/scratch/Premier/Raw_Data/_paticd_proc")
print("loading .... proc")
print(Sys.time())
load("nyu_allyears_proc.RData")
print(Sys.time())

# 2.filter for invasive mechanical ventilation codes
mech_vent_icd_codes = c('5A1935Z', '5A1945Z', '5A1955Z') # mechanical ventilation ICD codes

mech_vent_patients <- all_proc %>%
  filter(all_proc_codes %in% mech_vent_icd_codes)
  

# 3.select unique patient keys to identify which patients had a mech. vent. code
pat_with_mech_vent <- unique(mech_vent_patients$pat_key)

# 4. create dummy variable in dataset with 1 (or 0) for mech. vent (or not)
merged_data$mech_vent <- ifelse(merged_data$pat_key %in% pat_with_mech_vent, 1, 0)

# add region factor to the dataset -------------------------------------
print("creating region variable for hosptials")
#merge middle atlantic and new england
merged_data[merged_data$PROV_DIVISION == "MIDDLE ATLANTIC",]$PROV_DIVISION <- "NEW ENGLAND"
#merge mountain and WEST NORTH CENTRAL
merged_data[merged_data$PROV_DIVISION == "MOUNTAIN",]$PROV_DIVISION <- "WEST NORTH CENTRAL"
table(merged_data$PROV_DIVISION)


# Add CCI -------------
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
                                     "E13", "E[:digit:][:digit:].0",
                                     "E[:digit:][:digit:].1",
                                     "E[:digit:][:digit:].6", 
                                     "E[:digit:][:digit:].8", 
                                     "E[:digit:][:digit:].9")


renal_mildmoderate_codes <- c("I12.9", "I13.0", "I13.10",
                              "N03", "N05", "N18.1", "N18.2",
                              "N18.3", "N18.4", "N18.9", "Z94.0")

#note the wildcards in this set of codes
Diabetes_with_Chronic_Complications <- c("E08", "E09", 
                                         "E10", "E11", "E13",
                                         "E[:digit:][:digit:].2",
                                         "E[:digit:][:digit:].3",
                                         "E[:digit:][:digit:].4",
                                         "E[:digit:][:digit:].5")

Hemiplegia_or_Paraplegia <- c("G04.1", "G11.4", "G80.0",
                              "G80.1", "G80.2", "G81",
                              "G82", "G83")

Any_Malignancy_except_skin <- c("C0", "C1", "C2", "C30", "C31",
                                "C32", "C33", "C34", "C37", "C38",
                                "C39", "C40", "C41", "C43", "C45", 
                                "C46", "C47", "C48", "C49", "C50",
                                "C51", "C52", "C53", "C54", "C55",
                                "C56", "C57", "C58", "C60", "C61", 
                                "C62", "C63", "C76", "C80.1", "C81",
                                "C82", "C83", "C84", "C85", "C88", "C9")

Moderate_or_Severe_Liver_Disease <- c("I85.0", "I86.4", "K70.4", "K71.1",
                                      "K72.1", "K72.9", "K76.5", "K76.6",
                                      "K76.7")

Renal_Severe <- c("I12.0", "I13.11", "I13.2", "N18.5", "N18.6", "N19", "N25.0",
                  "Z49", "Z99.2")

HIV_Infection <- c("B20")

Metastatic_Solid_Tumor <- c("C77", "C78", "C79", "C80.0", "C80.2")

AIDS_codes <- c("B37", "C53","B38", "B45", "A07.2", "B25", "G93.4",
                "B00", "B39", "A07.3", "C46", "C81", "C82", "C83",
                "C84", "C85", "C86", "C87", "C88", "C89", "C90",
                "C91", "C92", "C93", "C94", "C95", "C96", "A31",
                "A15", "A16", "A17", "A18", "A19", "B59", "Z87.01",
                "A81.2", "A02.1", "B58", "R64")


# filter by outcome (e.g. ARDS/J80) ----------------
ARDS_data <- merged_data %>%
  filter(ARDS == 1)

J96_data <- merged_data %>%
  filter(J96 == 1)

mechvent_data <- merged_data %>%
  filter(mech_vent == 1)

# drop "unknown" as missing ----------------


ARDS_data <- ARDS_data %>%
  filter(gender != "U" & #dropped 47 observations
         race_ethnicity != "Unknown") 

J96_data <- J96_data %>%
  filter(gender != "U" & #dropped xx observations
           race_ethnicity != "Unknown") 

mechvent_data <- mechvent_data %>%
  filter(gender != "U" & #dropped xx observations
           race_ethnicity != "Unknown") 


##---------CHECKING FOR DUPLICATES-----------------
duplicate_ids <- ARDS_data$pat_key[duplicated(ARDS_data$pat_key)]
print(duplicate_ids)

duplicates <- ARDS_data[ARDS_data$pat_key %in% duplicate_ids, ]
print(duplicates)
##no duplicates found

ARDS_data %>%
  group_by(pat_key) %>%
  summarise(count = n()) %>%
  filter(count > 1)


###-----------Managing Admission Month ----------------
##Looking at distribution of admission month
table(ARDS_data$adm_mon)

##Dropping any observations with adm_month before Jan 2017
ARDS_data <- ARDS_data %>%
  filter(adm_mon >= 2017101)
table(ARDS_data$adm_mon)


# create CCI in these smaller datasets -----------------
## create CCI score ------------------
# 
print(Sys.time())
ARDS_data <- ARDS_data %>%
  rowwise() %>%
  mutate(cond_1 = if_else(any(str_detect(diagnoses_all, MI_codes)), 1, 0),
         cond_2 = if_else(any(str_detect(diagnoses_all, congestive_heart_codes)), 1, 0),
         cond_3 = if_else(any(str_detect(diagnoses_all,peripheral_vascular_codes)), 1, 0),
         cond_4 = if_else(any(str_detect(diagnoses_all,cerebrovascular_disease_codes)), 1, 0),
         cond_5 = if_else(any(str_detect(diagnoses_all,dementia_codes)), 1, 0),
         cond_6 = if_else(any(str_detect(diagnoses_all,chronic_pulmonary_codes)), 1, 0),
         cond_7 = if_else(any(str_detect(diagnoses_all,rheumatic_disease_codes)), 1, 0),
         cond_8 = if_else(any(str_detect(diagnoses_all,peptic_ulcer_codes)), 1, 0),
         cond_9 = if_else(any(str_detect(diagnoses_all,mild_liver_codes)), 1, 0),
         cond_10 = if_else(any(str_detect(diagnoses_all, diabetes_wo_complications_codes)), 1, 0),
         cond_11 = if_else(any(str_detect(diagnoses_all,renal_mildmoderate_codes)), 1, 0),
         
         cond_12 = if_else(any(str_detect(diagnoses_all,Diabetes_with_Chronic_Complications)), 2, 0),
         cond_13 = if_else(any(str_detect(diagnoses_all,Hemiplegia_or_Paraplegia)), 2, 0),
         cond_14 = if_else(any(str_detect(diagnoses_all,Any_Malignancy_except_skin)), 2, 0),
         cond_15 = if_else(any(str_detect(diagnoses_all,Moderate_or_Severe_Liver_Disease)), 3, 0),
         cond_16 = if_else(any(str_detect(diagnoses_all,Renal_Severe)), 3, 0),
         cond_17 = if_else(any(str_detect(diagnoses_all,HIV_Infection)), 3, 0),
         cond_18 = if_else(any(str_detect(diagnoses_all,Metastatic_Solid_Tumor)), 6, 0),
         cond_19 = if_else(any(str_detect(diagnoses_all,AIDS_codes)) &
                             any(str_detect(diagnoses_all,HIV_Infection)), 6, 0)) %>% #HIV + opportunistic infect.
  rowwise() %>%
  mutate(CCI_raw = cond_1 + cond_2 + cond_3 + cond_4 + cond_5 + cond_6 + cond_7 +
           cond_8 + cond_9 + cond_10 + cond_11 + cond_12 + cond_13 + cond_14 +
           cond_15 + cond_16 + cond_17 + cond_18 + cond_19,
         #deal with hierarchy rules by subtracting score of lower-order condition
         CCI = case_when(
           cond_13 != 0 & cond_4 != 0 ~ CCI_raw - 1, #cond. 13 trumps cond.4
           cond_15 != 0 & cond_9 != 0 ~ CCI_raw - 1, #cond. 15 trumps cond. 9
           cond_12 != 0 & cond_10 != 0 ~ CCI_raw - 1,
           cond_16 != 0 & cond_11 != 0 ~ CCI_raw - 1,
           cond_18 != 0 & cond_14 != 0 ~ CCI_raw - 2,
           cond_19 != 0 & cond_17 != 0 ~ CCI_raw - 3,
           .default = CCI_raw))
print(Sys.time()) #aprox 13 mins

# Organ failure score -------------------------------------------
## Following Dombrovskiy et al. approach from Bosch et al., (2020)."Predictive Validity"


cvd_sofa <- c("R57", "I95.1", "I95.89", "I95.9", "R03.1", "R65.21", "I46.9")
resp_sofa <- c("J96.0", "J96.9", "J80", "R06.00", "R06.03", "R06.09",
               "R06.3", "R06.83", "R06.89", "R09.2")
neuro_sofa <- c("F05", "G93.1", "G93.40", "R40.1", "R40.2")
hema_sofa <- c("D65", "D68.8", "D68.9", "D69.59", "D69.6")
hepatic_sofa <- c("K72.00", "K72.01", "K72.91", "K76.2", "K76.3")
renal_sofa <- c("N17")

ARDS_data <- ARDS_data %>%
  rowwise() %>%
  mutate(cvd_score = if_else(any(str_detect(diagnoses_all, cvd_sofa)), 1, 0),
         resp_score = if_else(any(str_detect(diagnoses_all, resp_sofa)), 1, 0),
         neuro_score = if_else(any(str_detect(diagnoses_all, neuro_sofa)), 1, 0),
         hema_score = if_else(any(str_detect(diagnoses_all, hema_sofa)), 1, 0),
         hepatic_score = if_else(any(str_detect(diagnoses_all, hepatic_sofa)), 1, 0),
         renal_score = if_else(any(str_detect(diagnoses_all, renal_sofa)), 1, 0)) %>%
  mutate(organ_failure = cvd_score + resp_score + neuro_score + 
           hema_score + hepatic_score + renal_score)



# table 1a: sample size ----------------
#sample sizes
length(unique(merged_data$pat_key)) #total sample size
length(ARDS_data$pat_key) #ARDS patients

length(unique(ARDS_data$prov_id)) #number of hospitals

#gender
sum(ARDS_data$gender == "M") #number of men
table(ARDS_data$gender)
table(ARDS_data$gender)/length(ARDS_data$pat_key) #proportion

#race
table(ARDS_data$race) #distribution by race
table(ARDS_data$race)/length(ARDS_data$pat_key) #proportion by race

#hispanicity
table(ARDS_data$hispanic_ind)
table(ARDS_data$hispanic_ind)/length(ARDS_data$pat_key) #proportion

#race and ethnicity
table(ARDS_data$race_ethnicity)
table(ARDS_data$race_ethnicity)/length(ARDS_data$pat_key)

# age
summary(ARDS_data$age) #age distribution
mean(ARDS_data$age, na.rm = TRUE)
sd(ARDS_data$age, na.rm = TRUE)
sum(is.na(ARDS_data$age))

# length of stay
summary(ARDS_data$los)
sd(ARDS_data$los)


# death at discharge (expired, expired at home/medical facility/place unknown) or hospice
table(ARDS_data$death_or_hospice)
table(ARDS_data$death_or_hospice)/length(ARDS_data$pat_key) #proportion

#obesity
table(ARDS_data$obesity)
table(ARDS_data$obesity)/length(ARDS_data$pat_key) #proportion

#insurance type
table(ARDS_data$insurance)
table(ARDS_data$insurance)/length(ARDS_data$pat_key) #proportion

#CCI
mean(ARDS_data$CCI)
sd(ARDS_data$CCI)

#Organ failure
mean(ARDS_data$organ_failure)
sd(ARDS_data$organ_failure)


###---------Stratifying Table 1 by Race/ethnicity-----------------------
##STEP 1: Create stratified data sets (by race/ethnicity)
##### A) Non-Hispanic White
ARDS_data_White <- ARDS_data %>%
  filter(race_ethnicity == "nonHispanic_White")

##### B) Non-Hispanic Black
ARDS_data_Black <- ARDS_data %>%
  filter(race_ethnicity == "nonHispanic_Black")

##### C) Hispanic
ARDS_data_Hisp <- ARDS_data %>%
  filter(race_ethnicity == "Hispanic")

##### D) Asian
ARDS_data_Asian_pt <- ARDS_data %>%
  filter(race_ethnicity == "Asian")

##### E) Other
ARDS_data_Other <- ARDS_data %>%
  filter(race_ethnicity == "Other")


#NON-HISPANIC WHITE
#sample sizes
length(ARDS_data_White$pat_key) #White ARDS patients
#gender
sum(ARDS_data_White$gender == "M") #number of men
table(ARDS_data_White$gender)
table(ARDS_data_White$gender)/length(ARDS_data_White$pat_key) #proportion
#insurance type
table(ARDS_data_White$insurance)
table(ARDS_data_White$insurance)/length(ARDS_data_White$pat_key) #proportion
#obesity
table(ARDS_data_White$obesity)
table(ARDS_data_White$obesity)/length(ARDS_data_White$pat_key) #proportion
# death at discharge or hospice
table(ARDS_data_White$death_or_hospice)
table(ARDS_data_White$death_or_hospice)/length(ARDS_data_White$pat_key) #proportion
# length of stay
summary(ARDS_data_White$los)
sd(ARDS_data_White$los)
# age
summary(ARDS_data_White$age) #age distribution
mean(ARDS_data_White$age, na.rm = TRUE)
sd(ARDS_data_White$age, na.rm = TRUE)
sum(is.na(ARDS_data_White$age))
#CCI
mean(ARDS_data_White$CCI)
sd(ARDS_data_White$CCI)
#Organ failure
mean(ARDS_data_White$organ_failure)
sd(ARDS_data_White$organ_failure)


#NON-HISPANIC BLACK
#sample sizes
length(ARDS_data_Black$pat_key) #Black ARDS patients
#gender
sum(ARDS_data_Black$gender == "M") #number of men
table(ARDS_data_Black$gender)
table(ARDS_data_Black$gender)/length(ARDS_data_Black$pat_key) #proportion
#insurance type
table(ARDS_data_Black$insurance)
table(ARDS_data_Black$insurance)/length(ARDS_data_Black$pat_key) #proportion
#obesity
table(ARDS_data_Black$obesity)
table(ARDS_data_Black$obesity)/length(ARDS_data_Black$pat_key) #proportion
# death at discharge or hospice
table(ARDS_data_Black$death_or_hospice)
table(ARDS_data_Black$death_or_hospice)/length(ARDS_data_Black$pat_key) #proportion
# length of stay
summary(ARDS_data_Black$los)
sd(ARDS_data_Black$los)
# age
summary(ARDS_data_Black$age) #age distribution
mean(ARDS_data_Black$age, na.rm = TRUE)
sd(ARDS_data_Black$age, na.rm = TRUE)
#CCI
mean(ARDS_data_Black$CCI)
sd(ARDS_data_Black$CCI)
#Organ failure
mean(ARDS_data_Black$organ_failure)
sd(ARDS_data_Black$organ_failure)


#HISPANIC
#sample sizes
length(ARDS_data_Hisp$pat_key) #Hispanic ARDS patients
#gender
sum(ARDS_data_Hisp$gender == "M") #number of men
table(ARDS_data_Hisp$gender)
table(ARDS_data_Hisp$gender)/length(ARDS_data_Hisp$pat_key) #proportion
#insurance type
table(ARDS_data_Hisp$insurance)
table(ARDS_data_Hisp$insurance)/length(ARDS_data_Hisp$pat_key) #proportion
#obesity
table(ARDS_data_Hisp$obesity)
table(ARDS_data_Hisp$obesity)/length(ARDS_data_Hisp$pat_key) #proportion
# death at discharge or hospice
table(ARDS_data_Hisp$death_or_hospice)
table(ARDS_data_Hisp$death_or_hospice)/length(ARDS_data_Hisp$pat_key) #proportion
# length of stay
summary(ARDS_data_Hisp$los)
sd(ARDS_data_Hisp$los)
# age
summary(ARDS_data_Hisp$age) #age distribution
mean(ARDS_data_Hisp$age, na.rm = TRUE)
sd(ARDS_data_Hisp$age, na.rm = TRUE)
#CCI
mean(ARDS_data_Hisp$CCI)
sd(ARDS_data_Hisp$CCI)
#Organ failure
mean(ARDS_data_Hisp$organ_failure)
sd(ARDS_data_Hisp$organ_failure)


#ASIAN
#sample sizes
length(ARDS_data_Asian_pt$pat_key) #Asian ARDS patients
#gender
sum(ARDS_data_Asian_pt$gender == "M") #number of men
table(ARDS_data_Asian_pt$gender)
table(ARDS_data_Asian_pt$gender)/length(ARDS_data_Asian_pt$pat_key) #proportion
#insurance type
table(ARDS_data_Asian_pt$insurance)
table(ARDS_data_Asian_pt$insurance)/length(ARDS_data_Asian_pt$pat_key) #proportion
#obesity
table(ARDS_data_Asian_pt$obesity)
table(ARDS_data_Asian_pt$obesity)/length(ARDS_data_Asian_pt$pat_key) #proportion
# death at discharge or hospice
table(ARDS_data_Asian_pt$death_or_hospice)
table(ARDS_data_Asian_pt$death_or_hospice)/length(ARDS_data_Asian_pt$pat_key) #proportion
# length of stay
summary(ARDS_data_Asian_pt$los)
sd(ARDS_data_Asian_pt$los)
# age
summary(ARDS_data_Asian_pt$age) #age distribution
mean(ARDS_data_Asian_pt$age, na.rm = TRUE)
sd(ARDS_data_Asian_pt$age, na.rm = TRUE)
#CCI
mean(ARDS_data_Asian_pt$CCI)
sd(ARDS_data_Asian_pt$CCI)
#Organ failure
mean(ARDS_data_Asian_pt$organ_failure)
sd(ARDS_data_Asian_pt$organ_failure)


#OTHER
#sample sizes
length(ARDS_data_Other$pat_key) #Other race/ethn ARDS patients
#gender
sum(ARDS_data_Other$gender == "M") #number of men
table(ARDS_data_Other$gender)
table(ARDS_data_Other$gender)/length(ARDS_data_Other$pat_key) #proportion
#insurance type
table(ARDS_data_Other$insurance)
table(ARDS_data_Other$insurance)/length(ARDS_data_Other$pat_key) #proportion
#obesity
table(ARDS_data_Other$obesity)
table(ARDS_data_Other$obesity)/length(ARDS_data_Other$pat_key) #proportion
# death at discharge or hospice
table(ARDS_data_Other$death_or_hospice)
table(ARDS_data_Other$death_or_hospice)/length(ARDS_data_Other$pat_key) #proportion
# length of stay
summary(ARDS_data_Other$los)
sd(ARDS_data_Other$los)
# age
summary(ARDS_data_Other$age) #age distribution
mean(ARDS_data_Other$age, na.rm = TRUE)
sd(ARDS_data_Other$age, na.rm = TRUE)
#CCI
mean(ARDS_data_Other$CCI)
sd(ARDS_data_Other$CCI)
#Organ failure
mean(ARDS_data_Other$organ_failure)
sd(ARDS_data_Other$organ_failure)


# Table 1b ------------------

#death by race/ethnicity
table(ARDS_data$death, ARDS_data$race_ethnicity)

# Table 1c -----------------------

#death or hospic by race/ethnicity
table(ARDS_data$death_or_hospice, ARDS_data$race_ethnicity)


#in-hospital mortality by race/ethnicity
table(ARDS_data$inhospital_death, ARDS_data$race_ethnicity)

# Bivariate association Table 2a --------------------
# main predictor (race)
chisq.test(ARDS_data$death_or_hospice, ARDS_data$race_ethnicity) #death
chisq.test(ARDS_data$gender, ARDS_data$race_ethnicity) #gender
chisq.test(ARDS_data$obesity, ARDS_data$race_ethnicity) #obesity
chisq.test(ARDS_data$insurance, ARDS_data$race_ethnicity) #insurance type
#aov(los ~ race_ethnicity, data=ARDS_data) #LOS
kruskal.test(los ~ race_ethnicity, data=ARDS_data) #LOS (non-parametric alternative to ANOVA)



# outcome (death)
chisq.test(ARDS_data$race_ethnicity, ARDS_data$death_or_hospice) #race
#chisq.test(ARDS_data$hispanic_ind, ARDS_data$death) #ethnicity
chisq.test(ARDS_data$gender, ARDS_data$death_or_hospice) #gender
chisq.test(ARDS_data$obesity, ARDS_data$death_or_hospice) #obesity
chisq.test(ARDS_data$insurance, ARDS_data$death_or_hospice) #insurance type




t.test(filter(ARDS_data, death == 1)$age, filter(ARDS_data, death == 0)$age) #age
t.test(filter(ARDS_data, death == 1)$CCI, filter(ARDS_data, death == 0)$CCI) #CCI
t.test(filter(ARDS_data, death == 1)$organ_failure, 
       filter(ARDS_data, death == 0)$organ_failure) #organ failure


# Table 3a: Odds of in-hospital death -----------------------

## All patients ----------------------------
library(lme4) 
### null --------------
m_null <- glmer(death ~ 1 + (1 | prov_id),
                data = ARDS_data, family = binomial)
summary(m_null)

se_null <- sqrt(diag(vcov(m_null)))
# table of estimates with 95% CI
tab_null <- cbind(Est = fixef(m_null), 
                  LL = fixef(m_null) - 1.96 * se_null,
                  UL = fixef(m_null) + 1.96 * se_null)
exp(tab_null)



# ICC
# The ICC is calculated by dividing the random effect variance, σ2i, by the total variance, i.e. the sum of the random effect variance and the residual variance, σ2ε.

# hand calculation
sigma2_0 <- as.data.frame(VarCorr(m_null),comp="Variance")$vcov[1]
total_var <- sigma2_0 + (pi^2)/3
icc_hand <- sigma2_0/total_var
icc_hand


### unadjusted -------------------
print(Sys.time())
m0 <- glmer(death ~ race_ethnicity + (1 | prov_id), 
            data = ARDS_data, family = binomial)
print(Sys.time()) #approx 4 mins to run
summary(m0)
se_0 <- sqrt(diag(vcov(m0)))
# table of estimates with 95% CI
tab_0 <- cbind(Est = fixef(m0), 
                  LL = fixef(m0) - 1.96 * se_0,
                  UL = fixef(m0) + 1.96 * se_0)
exp(tab_0)

performance::icc(m0)


### adjusted ---------------------------
print(Sys.time())
m1 <- glmer(death ~ race_ethnicity + age + gender + insurance + (1 | prov_id), 
            data = ARDS_data, family = binomial)
print(Sys.time()) #approx 8 mins to run
summary(m1)

se_1 <- sqrt(diag(vcov(m1)))
# table of estimates with 95% CI
tab_1 <- cbind(Est = fixef(m1), 
               LL = fixef(m1) - 1.96 * se_1,
               UL = fixef(m1) + 1.96 * se_1)
exp(tab_1)

# Add ICC for adjusted model
performance::icc(m1)

### adjusted + CCI ---------------------------
print(Sys.time())
m2 <- glmer(death ~ race_ethnicity + age + gender + insurance + CCI +
              (1 | prov_id), 
            data = ARDS_data, family = binomial)
print(Sys.time()) #approx 8 mins to run
summary(m2)

se_2 <- sqrt(diag(vcov(m2)))
# table of estimates with 95% CI
tab_2 <- cbind(Est = fixef(m2), 
               LL = fixef(m2) - 1.96 * se_2,
               UL = fixef(m2) + 1.96 * se_2)
exp(tab_2)

# Add ICC for adjusted model
performance::icc(m2)

### adjusted + CCI + organ failure ---------------------------
print(Sys.time())
m3 <- glmer(death ~ race_ethnicity + age + gender + insurance + 
              CCI + organ_failure + (1 | prov_id), 
            data = ARDS_data, family = binomial)
print(Sys.time()) #approx 8 mins to run
summary(m3)

se_3 <- sqrt(diag(vcov(m3)))
# table of estimates with 95% CI
tab_3 <- cbind(Est = fixef(m3), 
               LL = fixef(m3) - 1.96 * se_3,
               UL = fixef(m3) + 1.96 * se_3)
exp(tab_3)

# Add ICC for adjusted model
performance::icc(m3)


## Restricted for hospitals > 10 patients ----------------
occurrences <- table(ARDS_data$prov_id)
ARDS_data$hospital.patient.count <- occurrences[ARDS_data$prov_id]
ARDS_data_hosp10 <- ARDS_data[ARDS_data$hospital.patient.count > 10, ]
## null model 
m_null_hosp10 <- glmer(death ~ 1 + (1 | prov_id),
                data = ARDS_data_hosp10, family = binomial)
summary(m_null_hosp10)
se_null_hosp10 <- sqrt(diag(vcov(m_null_hosp10)))
tab_null_hosp10 <- cbind(Est = fixef(m_null_hosp10), 
                  LL = fixef(m_null_hosp10) - 1.96 * se_null_hosp10,
                  UL = fixef(m_null_hosp10) + 1.96 * se_null_hosp10)
exp(tab_null_hosp10)
sigma2_0 <- as.data.frame(VarCorr(m_null_hosp10),comp="Variance")$vcov[1]
total_var <- sigma2_0 + (pi^2)/3
icc_hand <- sigma2_0/total_var
icc_hand
## unadjusted 
print(Sys.time())
m0_hosp10 <- glmer(death ~ race_ethnicity + (1 | prov_id), 
            data = ARDS_data_hosp10, family = binomial)
print(Sys.time()) 
summary(m0_hosp10)
se_0_hosp10 <- sqrt(diag(vcov(m0_hosp10)))
tab_0_hosp10 <- cbind(Est = fixef(m0_hosp10), 
               LL = fixef(m0_hosp10) - 1.96 * se_0_hosp10,
               UL = fixef(m0_hosp10) + 1.96 * se_0_hosp10)
exp(tab_0_hosp10)
performance::icc(m0_hosp10)

## adjusted 
print(Sys.time())
m1_hosp10 <- glmer(death ~ race_ethnicity + age + gender + insurance + (1 | prov_id), 
            data = ARDS_data_hosp10, family = binomial)
print(Sys.time()) 
summary(m1_hosp10)

se_1_hosp10 <- sqrt(diag(vcov(m1_hosp10)))
tab_1_hosp10 <- cbind(Est = fixef(m1_hosp10), 
               LL = fixef(m1_hosp10) - 1.96 * se_1_hosp10,
               UL = fixef(m1_hosp10) + 1.96 * se_1_hosp10)
exp(tab_1_hosp10)
performance::icc(m1_hosp10)



## Restricted for hospitals with at least one patient identified as Asian ----------------
asian_count <- ARDS_data %>%
  filter(race_ethnicity == "Asian") %>%
  pull(prov_id) %>%
  unique()

ARDS_data_Aisan <- ARDS_data %>%
  filter(prov_id %in% asian_count)


## null model 
m_null_Aisan <- glmer(death ~ 1 + (1 | prov_id),
                       data = ARDS_data_Aisan, family = binomial)
summary(m_null_Aisan)
se_null_Aisan <- sqrt(diag(vcov(m_null_Aisan)))
tab_null_Aisan <- cbind(Est = fixef(m_null_Aisan), 
                         LL = fixef(m_null_Aisan) - 1.96 * se_null_Aisan,
                         UL = fixef(m_null_Aisan) + 1.96 * se_null_Aisan)
exp(tab_null_Aisan)
sigma2_0 <- as.data.frame(VarCorr(m_null_Aisan),comp="Variance")$vcov[1]
total_var <- sigma2_0 + (pi^2)/3
icc_hand <- sigma2_0/total_var
icc_hand
## unadjusted 
print(Sys.time())
m0_Aisan <- glmer(death ~ race_ethnicity + (1 | prov_id), 
                   data = ARDS_data_Aisan, family = binomial)
print(Sys.time()) 
summary(m0_Aisan)
se_0_Aisan  <- sqrt(diag(vcov(m0_Aisan)))
tab_0_Aisan <- cbind(Est = fixef(m0_Aisan), 
                      LL = fixef(m0_Aisan) - 1.96 * se_0_Aisan,
                      UL = fixef(m0_Aisan) + 1.96 * se_0_Aisan)
exp(tab_0_Aisan)
performance::icc(m0_Aisan)

## adjusted 
print(Sys.time())
m1_Aisan <- glmer(death ~ race_ethnicity + age + gender + insurance + (1 | prov_id), 
                   data = ARDS_data_Aisan, family = binomial)
print(Sys.time()) 
summary(m1_Aisan)

se_1_Aisan <- sqrt(diag(vcov(m1_Aisan)))
tab_1_Aisan <- cbind(Est = fixef(m1_Aisan), 
                      LL = fixef(m1_Aisan) - 1.96 * se_1_Aisan,
                      UL = fixef(m1_Aisan) + 1.96 * se_1_Aisan)
exp(tab_1_Aisan)
performance::icc(m1_Aisan)



# Table 3b: Odds of death or hospice transfer -----------------------

## null --------------
print("null model")
m2_null <- glmer(death_or_hospice ~ 1 + (1 | prov_id),
                data = ARDS_data, family = binomial)
summary(m2_null)

se2_null <- sqrt(diag(vcov(m2_null)))
# table of estimates with 95% CI
tab2_null <- cbind(Est = fixef(m2_null), 
                  LL = fixef(m2_null) - 1.96 * se2_null,
                  UL = fixef(m2_null) + 1.96 * se2_null)
exp(tab2_null)



# ICC
# The ICC is calculated by dividing the random effect variance, σ2i, by the total variance, i.e. the sum of the random effect variance and the residual variance, σ2ε.

# hand calculation
sigma2_0 <- as.data.frame(VarCorr(m2_null),comp="Variance")$vcov[1]
total_var <- sigma2_0 + (pi^2)/3
icc_hand <- sigma2_0/total_var
icc_hand


## unadjusted -------------------
print("unadjusted model")
print(Sys.time())
m3 <- glmer(death_or_hospice ~ race_ethnicity + (1 | prov_id), 
            data = ARDS_data, family = binomial)
print(Sys.time()) #approx 4 mins to run
summary(m3)
se_3 <- sqrt(diag(vcov(m3)))
# table of estimates with 95% CI
tab_3 <- cbind(Est = fixef(m3), 
               LL = fixef(m3) - 1.96 * se_3,
               UL = fixef(m3) + 1.96 * se_3)
exp(tab_3)

performance::icc(m3)


## adjusted ---------------------------
print("adjusted model")
print(Sys.time())
m4 <- glmer(death_or_hospice ~ race_ethnicity + age + gender + insurance + (1 | prov_id), 
            data = ARDS_data, family = binomial)
print(Sys.time()) #approx 8 mins to run
summary(m4)

se_4 <- sqrt(diag(vcov(m4)))
# table of estimates with 95% CI
tab_4 <- cbind(Est = fixef(m4), 
               LL = fixef(m4) - 1.96 * se_4,
               UL = fixef(m4) + 1.96 * se_4)
exp(tab_4)

# Add ICC for adjusted model
performance::icc(m4)

## adjusted  + CCI ---------------------------
print("adjusted and CCI model")
print(Sys.time())
m_3b_2 <- glmer(death_or_hospice ~ race_ethnicity + age + gender + insurance + 
              CCI + (1 | prov_id), 
            data = ARDS_data, family = binomial)
print(Sys.time()) #approx 8 mins to run
summary(m_3b_2)

se_3b_2 <- sqrt(diag(vcov(m_3b_2)))
# table of estimates with 95% CI
tab_3b_2 <- cbind(Est = fixef(m_3b_2), 
               LL = fixef(m_3b_2) - 1.96 * se_3b_2,
               UL = fixef(m_3b_2) + 1.96 * se_3b_2)
exp(tab_3b_2)

# Add ICC for adjusted model
performance::icc(m_3b_2)

## adjusted + CCI + organ failure ---------------------------
print(Sys.time())
m_3b_3 <- glmer(death_or_hospice ~ race_ethnicity + age + gender + insurance + 
                  CCI + organ_failure + (1 | prov_id), 
            data = ARDS_data, family = binomial)
print(Sys.time()) #approx 8 mins to run
summary(m_3b_3)

se_3b_3 <- sqrt(diag(vcov(m_3b_3)))
# table of estimates with 95% CI
tab_3b_3 <- cbind(Est = fixef(m_3b_3), 
               LL = fixef(m_3b_3) - 1.96 * se_3b_3,
               UL = fixef(m_3b_3) + 1.96 * se_3b_3)
exp(tab_3b_3)

# Add ICC for adjusted model
performance::icc(m_3b_3)


#------Rerun final model for in-hospital death only--------------------
## adjusted + CCI + organ failure ---------------------------
print(Sys.time())
mod_hosp_death <- glmer(inhospital_death ~ race_ethnicity + age + gender + insurance + 
                  CCI + organ_failure + (1 | prov_id), 
                data = ARDS_data, family = binomial)
print(Sys.time()) #approx 8 mins to run
summary(mod_hosp_death)

se_hosp_death <- sqrt(diag(vcov(mod_hosp_death)))
# table of estimates with 95% CI
tab_hosp_death <- cbind(Est = fixef(mod_hosp_death), 
                  LL = fixef(mod_hosp_death) - 1.96 * se_hosp_death,
                  UL = fixef(mod_hosp_death) + 1.96 * se_hosp_death)
exp(tab_hosp_death)


#------STRATIFYING FINAL MODEL (adjusted+CCI+OF) BY RACE/ETHNICITY)-------------------

##STEP 1: Create stratified data sets (by race/ethnicity)
##### A) Non-Hispanic White
#ARDS_data_White <- ARDS_data %>%
#  filter(race_ethnicity == "nonHispanic_White")

##### B) Non-Hispanic Black
#ARDS_data_Black <- ARDS_data %>%
#  filter(race_ethnicity == "nonHispanic_Black")

##### C) Hispanic
#ARDS_data_Hisp <- ARDS_data %>%
#  filter(race_ethnicity == "Hispanic")

##### D) Asian
#ARDS_data_Asian_pt <- ARDS_data %>%
#  filter(race_ethnicity == "Asian")

##### E) Other
#ARDS_data_Other <- ARDS_data %>%
#  filter(race_ethnicity == "Other")


##STEP 2: Run final model in each new race/ethnicity dataset
##### A) Non-Hispanic White
print(Sys.time())
fin_mod_Wh <- glmer(death_or_hospice ~ age + gender + insurance + 
                  CCI + organ_failure + (1 | prov_id), 
                data = ARDS_data_White, family = binomial)
print(Sys.time()) #approx 2 mins to run
summary(fin_mod_Wh)

se_fin_mod_Wh <- sqrt(diag(vcov(fin_mod_Wh)))
# table of estimates with 95% CI
tab_fin_mod_Wh <- cbind(Est = fixef(fin_mod_Wh), 
                  LL = fixef(fin_mod_Wh) - 1.96 * se_fin_mod_Wh,
                  UL = fixef(fin_mod_Wh) + 1.96 * se_fin_mod_Wh)
exp(tab_fin_mod_Wh)

# Add ICC for adjusted model
performance::icc(fin_mod_Wh)


##### B) Non-Hispanic Black
print(Sys.time())
fin_mod_Bl <- glmer(death_or_hospice ~ age + gender + insurance + 
                      CCI + organ_failure + (1 | prov_id), 
                    data = ARDS_data_Black, family = binomial)
print(Sys.time()) #approx 1 min to run
summary(fin_mod_Bl)

se_fin_mod_Bl <- sqrt(diag(vcov(fin_mod_Bl)))
# table of estimates with 95% CI
tab_fin_mod_Bl <- cbind(Est = fixef(fin_mod_Bl), 
                        LL = fixef(fin_mod_Bl) - 1.96 * se_fin_mod_Bl,
                        UL = fixef(fin_mod_Bl) + 1.96 * se_fin_mod_Bl)
exp(tab_fin_mod_Bl)

# Add ICC for adjusted model
performance::icc(fin_mod_Bl)


##### C) Hispanic
print(Sys.time())
fin_mod_Hisp <- glmer(death_or_hospice ~ age + gender + insurance + 
                      CCI + organ_failure + (1 | prov_id), 
                    data = ARDS_data_Hisp, family = binomial)
print(Sys.time()) #approx 1 min to run
summary(fin_mod_Hisp)

se_fin_mod_Hisp <- sqrt(diag(vcov(fin_mod_Hisp)))
# table of estimates with 95% CI
tab_fin_mod_Hisp <- cbind(Est = fixef(fin_mod_Hisp), 
                        LL = fixef(fin_mod_Hisp) - 1.96 * se_fin_mod_Hisp,
                        UL = fixef(fin_mod_Hisp) + 1.96 * se_fin_mod_Hisp)
exp(tab_fin_mod_Hisp)

# Add ICC for adjusted model
performance::icc(fin_mod_Hisp)


##### D) Asian
print(Sys.time())
fin_mod_Asian <- glmer(death_or_hospice ~ age + gender + insurance + 
                        CCI + organ_failure + (1 | prov_id), 
                      data = ARDS_data_Asian_pt, family = binomial)
print(Sys.time()) #approx 10 sec to run
summary(fin_mod_Asian)

se_fin_mod_Asian <- sqrt(diag(vcov(fin_mod_Asian)))
# table of estimates with 95% CI
tab_fin_mod_Asian <- cbind(Est = fixef(fin_mod_Asian), 
                          LL = fixef(fin_mod_Asian) - 1.96 * se_fin_mod_Asian,
                          UL = fixef(fin_mod_Asian) + 1.96 * se_fin_mod_Asian)
exp(tab_fin_mod_Asian)

# Add ICC for adjusted model
performance::icc(fin_mod_Asian)


##### E) Other
print(Sys.time())
fin_mod_Oth <- glmer(death_or_hospice ~ age + gender + insurance + 
                         CCI + organ_failure + (1 | prov_id), 
                       data = ARDS_data_Other, family = binomial)
print(Sys.time()) #approx 30 sec to run
summary(fin_mod_Oth)

se_fin_mod_Oth <- sqrt(diag(vcov(fin_mod_Oth)))
# table of estimates with 95% CI
tab_fin_mod_Oth <- cbind(Est = fixef(fin_mod_Oth), 
                           LL = fixef(fin_mod_Oth) - 1.96 * se_fin_mod_Oth,
                           UL = fixef(fin_mod_Oth) + 1.96 * se_fin_mod_Oth)
exp(tab_fin_mod_Oth)

# Add ICC for adjusted model
performance::icc(fin_mod_Oth)



# Table 4: Models by Covid period ----------------

#dates
unique(ARDS_data$adm_mon)

# pre-covid
pre_covid <- c(2017101, 2017102, 2017103, 2017204, 2017205, 2017206, 
               2017307, 2017308, 2017309, 2017410, 2017411, 2017412, 
               2018101, 2018102, 2018103, 2018204, 2018205, 2018206, 
               2018307, 2018308, 2018309, 2018410, 2018411, 2018412, 
               2019101, 2019102, 2019103, 2019204, 2019205, 2019206, 
               2019307, 2019308, 2019309, 2019410, 2019411, 2019412, 
               2020101, 2020102) #up to Feb 2020

covid <- c(2020103, 2020204, 2020205, 2020206, 2020307, 2020308, 
           2020309, 2020410, 2020411, 2020412, 2021103, 2021204,
           2021102, 2021101, 2021205, 2021206, 2021307, 2021308,
           2021309)

# split ARDS into two dataset
ARDS_precovid <- ARDS_data %>%
  filter(adm_mon %in% pre_covid)

ARDS_covid <- ARDS_data %>%
  filter(adm_mon %in% covid)


# sample size
dim(ARDS_precovid)
dim(ARDS_covid)


# PRE-COVID DESCRIPTIVE STATISTICS ----------------
#sample sizes
length(ARDS_precovid$pat_key) #ARDS patients

length(unique(ARDS_precovid$prov_id)) #number of hospitals

#gender
sum(ARDS_precovid$gender == "M") #number of men
table(ARDS_precovid$gender)
table(ARDS_precovid$gender)/length(ARDS_precovid$pat_key) #proportion

#race and ethnicity
table(ARDS_precovid$race_ethnicity)
table(ARDS_precovid$race_ethnicity)/length(ARDS_precovid$pat_key)

# age
summary(ARDS_precovid$age) #age distribution
mean(ARDS_precovid$age, na.rm = TRUE)
sd(ARDS_precovid$age, na.rm = TRUE)
sum(is.na(ARDS_precovid$age))

# length of stay
summary(ARDS_precovid$los)
sd(ARDS_precovid$los)

# death at discharge (expired, expired at home/medical facility/place unknown) or hospice
table(ARDS_precovid$death_or_hospice)
table(ARDS_precovid$death_or_hospice)/length(ARDS_precovid$pat_key) #proportion

#obesity
table(ARDS_precovid$obesity)
table(ARDS_precovid$obesity)/length(ARDS_precovid$pat_key) #proportion

#insurance type
table(ARDS_precovid$insurance)
table(ARDS_precovid$insurance)/length(ARDS_precovid$pat_key) #proportion

#CCI
mean(ARDS_precovid$CCI)
sd(ARDS_precovid$CCI)

#Organ failure
mean(ARDS_precovid$organ_failure)
sd(ARDS_precovid$organ_failure)


###---------Stratifying Table 1 by Race/ethnicity-----------------------
##STEP 1: Create stratified data sets (by race/ethnicity)
##### A) Non-Hispanic White
ARDS_precovid_White <- ARDS_precovid %>%
  filter(race_ethnicity == "nonHispanic_White")

##### B) Non-Hispanic Black
ARDS_precovid_Black <- ARDS_precovid %>%
  filter(race_ethnicity == "nonHispanic_Black")

##### C) Hispanic
ARDS_precovid_Hisp <- ARDS_precovid %>%
  filter(race_ethnicity == "Hispanic")

##### D) Asian
ARDS_precovid_Asian_pt <- ARDS_precovid %>%
  filter(race_ethnicity == "Asian")

##### E) Other
ARDS_precovid_Other <- ARDS_precovid %>%
  filter(race_ethnicity == "Other")


#NON-HISPANIC WHITE
#sample sizes
length(ARDS_precovid_White$pat_key) #White ARDS patients
#gender
sum(ARDS_precovid_White$gender == "M") #number of men
table(ARDS_precovid_White$gender)
table(ARDS_precovid_White$gender)/length(ARDS_precovid_White$pat_key) #proportion
#insurance type
table(ARDS_precovid_White$insurance)
table(ARDS_precovid_White$insurance)/length(ARDS_precovid_White$pat_key) #proportion
#obesity
table(ARDS_precovid_White$obesity)
table(ARDS_precovid_White$obesity)/length(ARDS_precovid_White$pat_key) #proportion
# death at discharge or hospice
table(ARDS_precovid_White$death_or_hospice)
table(ARDS_precovid_White$death_or_hospice)/length(ARDS_precovid_White$pat_key) #proportion
# length of stay
summary(ARDS_precovid_White$los)
sd(ARDS_precovid_White$los)
# age
summary(ARDS_precovid_White$age) #age distribution
mean(ARDS_precovid_White$age, na.rm = TRUE)
sd(ARDS_precovid_White$age, na.rm = TRUE)
sum(is.na(ARDS_precovid_White$age))
#CCI
mean(ARDS_precovid_White$CCI)
sd(ARDS_precovid_White$CCI)
#Organ failure
mean(ARDS_precovid_White$organ_failure)
sd(ARDS_precovid_White$organ_failure)


#NON-HISPANIC BLACK
#sample sizes
length(ARDS_precovid_Black$pat_key) #Black ARDS patients
#gender
sum(ARDS_precovid_Black$gender == "M") #number of men
table(ARDS_precovid_Black$gender)
table(ARDS_precovid_Black$gender)/length(ARDS_precovid_Black$pat_key) #proportion
#insurance type
table(ARDS_precovid_Black$insurance)
table(ARDS_precovid_Black$insurance)/length(ARDS_precovid_Black$pat_key) #proportion
#obesity
table(ARDS_precovid_Black$obesity)
table(ARDS_precovid_Black$obesity)/length(ARDS_precovid_Black$pat_key) #proportion
# death at discharge or hospice
table(ARDS_precovid_Black$death_or_hospice)
table(ARDS_precovid_Black$death_or_hospice)/length(ARDS_precovid_Black$pat_key) #proportion
# length of stay
summary(ARDS_precovid_Black$los)
sd(ARDS_precovid_Black$los)
# age
summary(ARDS_precovid_Black$age) #age distribution
mean(ARDS_precovid_Black$age, na.rm = TRUE)
sd(ARDS_precovid_Black$age, na.rm = TRUE)
#CCI
mean(ARDS_precovid_Black$CCI)
sd(ARDS_precovid_Black$CCI)
#Organ failure
mean(ARDS_precovid_Black$organ_failure)
sd(ARDS_precovid_Black$organ_failure)


#HISPANIC
#sample sizes
length(ARDS_precovid_Hisp$pat_key) #Hispanic ARDS patients
#gender
sum(ARDS_precovid_Hisp$gender == "M") #number of men
table(ARDS_precovid_Hisp$gender)
table(ARDS_precovid_Hisp$gender)/length(ARDS_precovid_Hisp$pat_key) #proportion
#insurance type
table(ARDS_precovid_Hisp$insurance)
table(ARDS_precovid_Hisp$insurance)/length(ARDS_precovid_Hisp$pat_key) #proportion
#obesity
table(ARDS_precovid_Hisp$obesity)
table(ARDS_precovid_Hisp$obesity)/length(ARDS_precovid_Hisp$pat_key) #proportion
# death at discharge or hospice
table(ARDS_precovid_Hisp$death_or_hospice)
table(ARDS_precovid_Hisp$death_or_hospice)/length(ARDS_precovid_Hisp$pat_key) #proportion
# length of stay
summary(ARDS_precovid_Hisp$los)
sd(ARDS_precovid_Hisp$los)
# age
summary(ARDS_precovid_Hisp$age) #age distribution
mean(ARDS_precovid_Hisp$age, na.rm = TRUE)
sd(ARDS_precovid_Hisp$age, na.rm = TRUE)
#CCI
mean(ARDS_precovid_Hisp$CCI)
sd(ARDS_precovid_Hisp$CCI)
#Organ failure
mean(ARDS_precovid_Hisp$organ_failure)
sd(ARDS_precovid_Hisp$organ_failure)


#ASIAN
#sample sizes
length(ARDS_precovid_Asian_pt$pat_key) #Asian ARDS patients
#gender
sum(ARDS_precovid_Asian_pt$gender == "M") #number of men
table(ARDS_precovid_Asian_pt$gender)
table(ARDS_precovid_Asian_pt$gender)/length(ARDS_precovid_Asian_pt$pat_key) #proportion
#insurance type
table(ARDS_precovid_Asian_pt$insurance)
table(ARDS_precovid_Asian_pt$insurance)/length(ARDS_precovid_Asian_pt$pat_key) #proportion
#obesity
table(ARDS_precovid_Asian_pt$obesity)
table(ARDS_precovid_Asian_pt$obesity)/length(ARDS_precovid_Asian_pt$pat_key) #proportion
# death at discharge or hospice
table(ARDS_precovid_Asian_pt$death_or_hospice)
table(ARDS_precovid_Asian_pt$death_or_hospice)/length(ARDS_precovid_Asian_pt$pat_key) #proportion
# length of stay
summary(ARDS_precovid_Asian_pt$los)
sd(ARDS_precovid_Asian_pt$los)
# age
summary(ARDS_precovid_Asian_pt$age) #age distribution
mean(ARDS_precovid_Asian_pt$age, na.rm = TRUE)
sd(ARDS_precovid_Asian_pt$age, na.rm = TRUE)
#CCI
mean(ARDS_precovid_Asian_pt$CCI)
sd(ARDS_precovid_Asian_pt$CCI)
#Organ failure
mean(ARDS_precovid_Asian_pt$organ_failure)
sd(ARDS_precovid_Asian_pt$organ_failure)


#OTHER
#sample sizes
length(ARDS_precovid_Other$pat_key) #Other race/ethn ARDS patients
#gender
sum(ARDS_precovid_Other$gender == "M") #number of men
table(ARDS_precovid_Other$gender)
table(ARDS_precovid_Other$gender)/length(ARDS_precovid_Other$pat_key) #proportion
#insurance type
table(ARDS_precovid_Other$insurance)
table(ARDS_precovid_Other$insurance)/length(ARDS_precovid_Other$pat_key) #proportion
#obesity
table(ARDS_precovid_Other$obesity)
table(ARDS_precovid_Other$obesity)/length(ARDS_precovid_Other$pat_key) #proportion
# death at discharge or hospice
table(ARDS_precovid_Other$death_or_hospice)
table(ARDS_precovid_Other$death_or_hospice)/length(ARDS_precovid_Other$pat_key) #proportion
# length of stay
summary(ARDS_precovid_Other$los)
sd(ARDS_precovid_Other$los)
# age
summary(ARDS_precovid_Other$age) #age distribution
mean(ARDS_precovid_Other$age, na.rm = TRUE)
sd(ARDS_precovid_Other$age, na.rm = TRUE)
#CCI
mean(ARDS_precovid_Other$CCI)
sd(ARDS_precovid_Other$CCI)
#Organ failure
mean(ARDS_precovid_Other$organ_failure)
sd(ARDS_precovid_Other$organ_failure)


# COVID DESCRIPTIVE STATISTICS ----------------
#sample sizes
length(ARDS_covid$pat_key) #ARDS patients

length(unique(ARDS_covid$prov_id)) #number of hospitals

#gender
sum(ARDS_covid$gender == "M") #number of men
table(ARDS_covid$gender)
table(ARDS_covid$gender)/length(ARDS_covid$pat_key) #proportion

#race and ethnicity
table(ARDS_covid$race_ethnicity)
table(ARDS_covid$race_ethnicity)/length(ARDS_covid$pat_key)

# age
summary(ARDS_covid$age) #age distribution
mean(ARDS_covid$age, na.rm = TRUE)
sd(ARDS_covid$age, na.rm = TRUE)
sum(is.na(ARDS_covid$age))

# length of stay
summary(ARDS_covid$los)
sd(ARDS_covid$los)

# death at discharge (expired, expired at home/medical facility/place unknown) or hospice
table(ARDS_covid$death_or_hospice)
table(ARDS_covid$death_or_hospice)/length(ARDS_covid$pat_key) #proportion

#obesity
table(ARDS_covid$obesity)
table(ARDS_covid$obesity)/length(ARDS_covid$pat_key) #proportion

#insurance type
table(ARDS_covid$insurance)
table(ARDS_covid$insurance)/length(ARDS_covid$pat_key) #proportion

#CCI
mean(ARDS_covid$CCI)
sd(ARDS_covid$CCI)

#Organ failure
mean(ARDS_covid$organ_failure)
sd(ARDS_covid$organ_failure)


###---------Stratifying Table 1 by Race/ethnicity-----------------------
##STEP 1: Create stratified data sets (by race/ethnicity)
##### A) Non-Hispanic White
ARDS_covid_White <- ARDS_covid %>%
  filter(race_ethnicity == "nonHispanic_White")

##### B) Non-Hispanic Black
ARDS_covid_Black <- ARDS_covid %>%
  filter(race_ethnicity == "nonHispanic_Black")

##### C) Hispanic
ARDS_covid_Hisp <- ARDS_covid %>%
  filter(race_ethnicity == "Hispanic")

##### D) Asian
ARDS_covid_Asian_pt <- ARDS_covid %>%
  filter(race_ethnicity == "Asian")

##### E) Other
ARDS_covid_Other <- ARDS_covid %>%
  filter(race_ethnicity == "Other")


#NON-HISPANIC WHITE
#sample sizes
length(ARDS_covid_White$pat_key) #White ARDS patients
#gender
sum(ARDS_covid_White$gender == "M") #number of men
table(ARDS_covid_White$gender)
table(ARDS_covid_White$gender)/length(ARDS_covid_White$pat_key) #proportion
#insurance type
table(ARDS_covid_White$insurance)
table(ARDS_covid_White$insurance)/length(ARDS_covid_White$pat_key) #proportion
#obesity
table(ARDS_covid_White$obesity)
table(ARDS_covid_White$obesity)/length(ARDS_covid_White$pat_key) #proportion
# death at discharge or hospice
table(ARDS_covid_White$death_or_hospice)
table(ARDS_covid_White$death_or_hospice)/length(ARDS_covid_White$pat_key) #proportion
# length of stay
summary(ARDS_covid_White$los)
sd(ARDS_covid_White$los)
# age
summary(ARDS_covid_White$age) #age distribution
mean(ARDS_covid_White$age, na.rm = TRUE)
sd(ARDS_covid_White$age, na.rm = TRUE)
sum(is.na(ARDS_covid_White$age))
#CCI
mean(ARDS_covid_White$CCI)
sd(ARDS_covid_White$CCI)
#Organ failure
mean(ARDS_covid_White$organ_failure)
sd(ARDS_covid_White$organ_failure)


#NON-HISPANIC BLACK
#sample sizes
length(ARDS_covid_Black$pat_key) #Black ARDS patients
#gender
sum(ARDS_covid_Black$gender == "M") #number of men
table(ARDS_covid_Black$gender)
table(ARDS_covid_Black$gender)/length(ARDS_covid_Black$pat_key) #proportion
#insurance type
table(ARDS_covid_Black$insurance)
table(ARDS_covid_Black$insurance)/length(ARDS_covid_Black$pat_key) #proportion
#obesity
table(ARDS_covid_Black$obesity)
table(ARDS_covid_Black$obesity)/length(ARDS_covid_Black$pat_key) #proportion
# death at discharge or hospice
table(ARDS_covid_Black$death_or_hospice)
table(ARDS_covid_Black$death_or_hospice)/length(ARDS_covid_Black$pat_key) #proportion
# length of stay
summary(ARDS_covid_Black$los)
sd(ARDS_covid_Black$los)
# age
summary(ARDS_covid_Black$age) #age distribution
mean(ARDS_covid_Black$age, na.rm = TRUE)
sd(ARDS_covid_Black$age, na.rm = TRUE)
#CCI
mean(ARDS_covid_Black$CCI)
sd(ARDS_covid_Black$CCI)
#Organ failure
mean(ARDS_covid_Black$organ_failure)
sd(ARDS_covid_Black$organ_failure)


#HISPANIC
#sample sizes
length(ARDS_covid_Hisp$pat_key) #Hispanic ARDS patients
#gender
sum(ARDS_covid_Hisp$gender == "M") #number of men
table(ARDS_covid_Hisp$gender)
table(ARDS_covid_Hisp$gender)/length(ARDS_covid_Hisp$pat_key) #proportion
#insurance type
table(ARDS_covid_Hisp$insurance)
table(ARDS_covid_Hisp$insurance)/length(ARDS_covid_Hisp$pat_key) #proportion
#obesity
table(ARDS_covid_Hisp$obesity)
table(ARDS_covid_Hisp$obesity)/length(ARDS_covid_Hisp$pat_key) #proportion
# death at discharge or hospice
table(ARDS_covid_Hisp$death_or_hospice)
table(ARDS_covid_Hisp$death_or_hospice)/length(ARDS_covid_Hisp$pat_key) #proportion
# length of stay
summary(ARDS_covid_Hisp$los)
sd(ARDS_covid_Hisp$los)
# age
summary(ARDS_covid_Hisp$age) #age distribution
mean(ARDS_covid_Hisp$age, na.rm = TRUE)
sd(ARDS_covid_Hisp$age, na.rm = TRUE)
#CCI
mean(ARDS_covid_Hisp$CCI)
sd(ARDS_covid_Hisp$CCI)
#Organ failure
mean(ARDS_covid_Hisp$organ_failure)
sd(ARDS_covid_Hisp$organ_failure)


#ASIAN
#sample sizes
length(ARDS_covid_Asian_pt$pat_key) #Asian ARDS patients
#gender
sum(ARDS_covid_Asian_pt$gender == "M") #number of men
table(ARDS_covid_Asian_pt$gender)
table(ARDS_covid_Asian_pt$gender)/length(ARDS_covid_Asian_pt$pat_key) #proportion
#insurance type
table(ARDS_covid_Asian_pt$insurance)
table(ARDS_covid_Asian_pt$insurance)/length(ARDS_covid_Asian_pt$pat_key) #proportion
#obesity
table(ARDS_covid_Asian_pt$obesity)
table(ARDS_covid_Asian_pt$obesity)/length(ARDS_covid_Asian_pt$pat_key) #proportion
# death at discharge or hospice
table(ARDS_covid_Asian_pt$death_or_hospice)
table(ARDS_covid_Asian_pt$death_or_hospice)/length(ARDS_covid_Asian_pt$pat_key) #proportion
# length of stay
summary(ARDS_covid_Asian_pt$los)
sd(ARDS_covid_Asian_pt$los)
# age
summary(ARDS_covid_Asian_pt$age) #age distribution
mean(ARDS_covid_Asian_pt$age, na.rm = TRUE)
sd(ARDS_covid_Asian_pt$age, na.rm = TRUE)
#CCI
mean(ARDS_covid_Asian_pt$CCI)
sd(ARDS_covid_Asian_pt$CCI)
#Organ failure
mean(ARDS_covid_Asian_pt$organ_failure)
sd(ARDS_covid_Asian_pt$organ_failure)


#OTHER
#sample sizes
length(ARDS_covid_Other$pat_key) #Other race/ethn ARDS patients
#gender
sum(ARDS_covid_Other$gender == "M") #number of men
table(ARDS_covid_Other$gender)
table(ARDS_covid_Other$gender)/length(ARDS_covid_Other$pat_key) #proportion
#insurance type
table(ARDS_covid_Other$insurance)
table(ARDS_covid_Other$insurance)/length(ARDS_covid_Other$pat_key) #proportion
#obesity
table(ARDS_covid_Other$obesity)
table(ARDS_covid_Other$obesity)/length(ARDS_covid_Other$pat_key) #proportion
# death at discharge or hospice
table(ARDS_covid_Other$death_or_hospice)
table(ARDS_covid_Other$death_or_hospice)/length(ARDS_covid_Other$pat_key) #proportion
# length of stay
summary(ARDS_covid_Other$los)
sd(ARDS_covid_Other$los)
# age
summary(ARDS_covid_Other$age) #age distribution
mean(ARDS_covid_Other$age, na.rm = TRUE)
sd(ARDS_covid_Other$age, na.rm = TRUE)
#CCI
mean(ARDS_covid_Other$CCI)
sd(ARDS_covid_Other$CCI)
#Organ failure
mean(ARDS_covid_Other$organ_failure)
sd(ARDS_covid_Other$organ_failure)


## table 4a, pre-covid, outcome death -----------

#adjusted
print("adjusted")
print(Sys.time())
m_4a_1 <- glmer(death ~ race_ethnicity + age + gender + insurance +
                      (1 | prov_id), 
            data = ARDS_precovid, family = binomial)
print(Sys.time()) 
summary(m_4a_1) #2 mins

se_4a_1 <- sqrt(diag(vcov(m_4a_1)))
# table of estimates with 95% CI
tab_4a_1 <- cbind(Est = fixef(m_4a_1), 
               LL = fixef(m_4a_1) - 1.96 * se_4a_1,
               UL = fixef(m_4a_1) + 1.96 * se_4a_1)
exp(tab_4a_1)


#adjusted + CCI
print("adjusted + CCI")
print(Sys.time())
m_4a_2 <- glmer(death ~ race_ethnicity + age + gender + insurance +
                  CCI + (1 | prov_id), 
                data = ARDS_precovid, family = binomial)
print(Sys.time()) 
summary(m_4a_2) 

se_4a_2 <- sqrt(diag(vcov(m_4a_2)))
# table of estimates with 95% CI
tab_4a_2 <- cbind(Est = fixef(m_4a_2), 
                  LL = fixef(m_4a_2) - 1.96 * se_4a_2,
                  UL = fixef(m_4a_2) + 1.96 * se_4a_2)
exp(tab_4a_2)

#adjusted + CCI + organ failure
print("adjusted + CCI + organ failure")
print(Sys.time())
m_4a_3 <- glmer(death ~ race_ethnicity + age + gender + insurance +
                  CCI + organ_failure + (1 | prov_id), 
                data = ARDS_precovid, family = binomial)
print(Sys.time()) 
summary(m_4a_3) 

se_4a_3 <- sqrt(diag(vcov(m_4a_3)))
# table of estimates with 95% CI
tab_4a_3 <- cbind(Est = fixef(m_4a_3), 
                  LL = fixef(m_4a_3) - 1.96 * se_4a_3,
                  UL = fixef(m_4a_3) + 1.96 * se_4a_3)
exp(tab_4a_3)


## table 4b, pre-covid, outcome death or hospice -----------

#unadjusted - no MLM; regular LR
print("unadjusted")
print(Sys.time())
m_4b_glm <- glm(death_or_hospice ~ race_ethnicity, 
                   data = ARDS_precovid, family = binomial)
print(Sys.time()) 
summary(m_4b_glm)

se_4b_glm <- sqrt(diag(vcov(m_4b_glm)))
# table of estimates with 95% CI
tab_4b_glm <- cbind(Est = coef(m_4b_glm), 
                     LL = coef(m_4b_glm) - 1.96 * se_4b_glm,
                     UL = coef(m_4b_glm) + 1.96 * se_4b_glm)
exp(tab_4b_glm)


#unadjusted
print("unadjusted")
print(Sys.time())
m_4b_unad <- glmer(death_or_hospice ~ race_ethnicity +
                  (1 | prov_id), 
                data = ARDS_precovid, family = binomial)
print(Sys.time()) 
summary(m_4b_unad)

se_4b_unad <- sqrt(diag(vcov(m_4b_unad)))
# table of estimates with 95% CI
tab_4b_unad <- cbind(Est = fixef(m_4b_unad), 
                  LL = fixef(m_4b_unad) - 1.96 * se_4b_unad,
                  UL = fixef(m_4b_unad) + 1.96 * se_4b_unad)
exp(tab_4b_unad)


#adjusted, age
print("adjusted, race + age")
print(Sys.time())
m_4b_ag <- glmer(death_or_hospice ~ race_ethnicity + age +
                   (1 | prov_id), 
                 data = ARDS_precovid, family = binomial)
print(Sys.time()) 
summary(m_4b_ag) 

se_4b_ag <- sqrt(diag(vcov(m_4b_ag)))
# table of estimates with 95% CI
tab_4b_ag <- cbind(Est = fixef(m_4b_ag), 
                   LL = fixef(m_4b_ag) - 1.96 * se_4b_ag,
                   UL = fixef(m_4b_ag) + 1.96 * se_4b_ag)
exp(tab_4b_ag)


#Standardizing age
ARDS_precovid$age_std <- (ARDS_precovid$age - mean(ARDS_precovid$age)) / sd(ARDS_precovid$age)

#Rerun race+age model with standardized age
print("adjusted, race + std age")
print(Sys.time())
m_4b_agstd <- glmer(death_or_hospice ~ race_ethnicity + age_std +
                   (1 | prov_id), 
                 data = ARDS_precovid, family = binomial,
                 control = glmerControl(optimizer = "bobyqa")
)
print(Sys.time()) 
summary(m_4b_agstd) 

se_4b_agstd <- sqrt(diag(vcov(m_4b_agstd)))
# table of estimates with 95% CI
tab_4b_agstd <- cbind(Est = fixef(m_4b_agstd), 
                   LL = fixef(m_4b_agstd) - 1.96 * se_4b_agstd,
                   UL = fixef(m_4b_agstd) + 1.96 * se_4b_agstd)
exp(tab_4b_agstd)



#adjusted, std age + gender
print("adjusted, age + gender")
print(Sys.time())
m_4b_ag <- glmer(death_or_hospice ~ race_ethnicity + age_std + gender +
                  (1 | prov_id), 
                data = ARDS_precovid, family = binomial,
                control = glmerControl(optimizer = "bobyqa")
)
print(Sys.time()) 
summary(m_4b_ag) 

se_4b_ag <- sqrt(diag(vcov(m_4b_ag)))
# table of estimates with 95% CI
tab_4b_ag <- cbind(Est = fixef(m_4b_ag), 
                  LL = fixef(m_4b_ag) - 1.96 * se_4b_ag,
                  UL = fixef(m_4b_ag) + 1.96 * se_4b_ag)
exp(tab_4b_ag)


#adjusted, age + gender + ins
print("adjusted, age + gender + Ins")
print(Sys.time())
m_4b_1 <- glmer(death_or_hospice ~ race_ethnicity + age_std + gender + insurance +
                  (1 | prov_id), 
                data = ARDS_precovid, family = binomial,
                control = glmerControl(optimizer = "bobyqa")
)
print(Sys.time()) 
summary(m_4b_1) #2 mins

se_4b_1 <- sqrt(diag(vcov(m_4b_1)))
# table of estimates with 95% CI
tab_4b_1 <- cbind(Est = fixef(m_4b_1), 
                  LL = fixef(m_4b_1) - 1.96 * se_4b_1,
                  UL = fixef(m_4b_1) + 1.96 * se_4b_1)
exp(tab_4b_1)


#adjusted + CCI
print("adjusted + CCI")
print(Sys.time())
m_4b_2 <- glmer(death_or_hospice ~ race_ethnicity + age_std + gender + insurance +
                  CCI + (1 | prov_id), 
                data = ARDS_precovid, family = binomial,
                control = glmerControl(optimizer = "bobyqa")
                )
print(Sys.time()) 
summary(m_4b_2) 

se_4b_2 <- sqrt(diag(vcov(m_4b_2)))
# table of estimates with 95% CI
tab_4b_2 <- cbind(Est = fixef(m_4b_2), 
                  LL = fixef(m_4b_2) - 1.96 * se_4b_2,
                  UL = fixef(m_4b_2) + 1.96 * se_4b_2)
exp(tab_4b_2)

#adjusted + CCI + organ failure
print("adjusted + CCI + organ failure")
print(Sys.time())
m_4b_3 <- glmer(death_or_hospice ~ race_ethnicity + age_std + gender + insurance +
                  CCI + organ_failure + (1 | prov_id), 
                data = ARDS_precovid, family = binomial,
                control = glmerControl(optimizer = "bobyqa")
                )
print(Sys.time()) 
summary(m_4b_3) 

se_4b_3 <- sqrt(diag(vcov(m_4b_3)))
# table of estimates with 95% CI
tab_4b_3 <- cbind(Est = fixef(m_4b_3), 
                  LL = fixef(m_4b_3) - 1.96 * se_4b_3,
                  UL = fixef(m_4b_3) + 1.96 * se_4b_3)
exp(tab_4b_3)


## table 4c, Covid, outcome death -----------

#adjusted
print("adjusted")
print(Sys.time())
m_4c_1 <- glmer(death ~ race_ethnicity + age + gender + insurance +
                  (1 | prov_id), 
                data = ARDS_covid, family = binomial)
print(Sys.time()) 
summary(m_4c_1) #2 mins

se_4c_1 <- sqrt(diag(vcov(m_4c_1)))
# table of estimates with 95% CI
tab_4c_1 <- cbind(Est = fixef(m_4c_1), 
                  LL = fixef(m_4c_1) - 1.96 * se_4c_1,
                  UL = fixef(m_4c_1) + 1.96 * se_4c_1)
exp(tab_4c_1)


#adjusted + CCI
print("adjusted + CCI")
print(Sys.time())
m_4c_2 <- glmer(death ~ race_ethnicity + age + gender + insurance +
                  CCI + (1 | prov_id), 
                data = ARDS_covid, family = binomial)
print(Sys.time()) 
summary(m_4c_2) 

se_4c_2 <- sqrt(diag(vcov(m_4c_2)))
# table of estimates with 95% CI
tab_4c_2 <- cbind(Est = fixef(m_4c_2), 
                  LL = fixef(m_4c_2) - 1.96 * se_4c_2,
                  UL = fixef(m_4c_2) + 1.96 * se_4c_2)
exp(tab_4c_2)

#adjusted + CCI + organ failure
print("adjusted + CCI + organ failure")
print(Sys.time())
m_4c_3 <- glmer(death ~ race_ethnicity + age + gender + insurance +
                  CCI + organ_failure + (1 | prov_id), 
                data = ARDS_covid, family = binomial)
print(Sys.time()) 
summary(m_4c_3) 

se_4c_3 <- sqrt(diag(vcov(m_4c_3)))
# table of estimates with 95% CI
tab_4c_3 <- cbind(Est = fixef(m_4c_3), 
                  LL = fixef(m_4c_3) - 1.96 * se_4c_3,
                  UL = fixef(m_4c_3) + 1.96 * se_4c_3)
exp(tab_4c_3)

## table 4d, Covid, outcome death or hospice transfer -----------

#unadjusted - no MLM; regular LR
print("unadjusted")
print(Sys.time())
m_4d_glm <- glm(death_or_hospice ~ race_ethnicity, 
                data = ARDS_covid, family = binomial)
print(Sys.time()) 
summary(m_4d_glm)

se_4d_glm <- sqrt(diag(vcov(m_4d_glm)))
# table of estimates with 95% CI
tab_4d_glm <- cbind(Est = coef(m_4d_glm), 
                    LL = coef(m_4d_glm) - 1.96 * se_4d_glm,
                    UL = coef(m_4d_glm) + 1.96 * se_4d_glm)
exp(tab_4d_glm)


#unadjusted
print("unadjusted")
print(Sys.time())
m_4d_unad <- glmer(death_or_hospice ~ race_ethnicity +
                     (1 | prov_id), 
                   data = ARDS_covid, family = binomial)
print(Sys.time()) 
summary(m_4d_unad)

se_4d_unad <- sqrt(diag(vcov(m_4d_unad)))
# table of estimates with 95% CI
tab_4d_unad <- cbind(Est = fixef(m_4d_unad), 
                     LL = fixef(m_4d_unad) - 1.96 * se_4d_unad,
                     UL = fixef(m_4d_unad) + 1.96 * se_4d_unad)
exp(tab_4d_unad)


#Standardizing age
ARDS_covid$age_std <- (ARDS_covid$age - mean(ARDS_covid$age)) / sd(ARDS_covid$age)

#adjusted, age 
print("adjusted, age")
print(Sys.time())
m_4d_a <- glmer(death_or_hospice ~ race_ethnicity + age_std +
                   (1 | prov_id), 
                 data = ARDS_covid, family = binomial,
                control = glmerControl(optimizer = "bobyqa")
)
print(Sys.time()) 
summary(m_4d_a) 

se_4d_a <- sqrt(diag(vcov(m_4d_a)))
# table of estimates with 95% CI
tab_4d_a <- cbind(Est = fixef(m_4d_a), 
                   LL = fixef(m_4d_a) - 1.96 * se_4d_a,
                   UL = fixef(m_4d_a) + 1.96 * se_4d_a)
exp(tab_4d_a)

#adjusted, age + gender
print("adjusted, age + gender")
print(Sys.time())
m_4d_ag <- glmer(death_or_hospice ~ race_ethnicity + age_std + gender +
                   (1 | prov_id), 
                 data = ARDS_covid, family = binomial,
                 control = glmerControl(optimizer = "bobyqa")
)
print(Sys.time()) 
summary(m_4d_ag) 

se_4d_ag <- sqrt(diag(vcov(m_4d_ag)))
# table of estimates with 95% CI
tab_4d_ag <- cbind(Est = fixef(m_4d_ag), 
                   LL = fixef(m_4d_ag) - 1.96 * se_4d_ag,
                   UL = fixef(m_4d_ag) + 1.96 * se_4d_ag)
exp(tab_4d_ag)


#adjusted, age + gender + ins
print("adjusted, age + gender + Ins")
print(Sys.time())
m_4d_1 <- glmer(death_or_hospice ~ race_ethnicity + age_std + gender + insurance +
                  (1 | prov_id), 
                data = ARDS_covid, family = binomial,
                control = glmerControl(optimizer = "bobyqa")
)
print(Sys.time()) 
summary(m_4d_1) #2 mins

se_4d_1 <- sqrt(diag(vcov(m_4d_1)))
# table of estimates with 95% CI
tab_4d_1 <- cbind(Est = fixef(m_4d_1), 
                  LL = fixef(m_4d_1) - 1.96 * se_4d_1,
                  UL = fixef(m_4d_1) + 1.96 * se_4d_1)
exp(tab_4d_1)


#adjusted + CCI
print("adjusted + CCI")
print(Sys.time())
m_4d_2 <- glmer(death_or_hospice ~ race_ethnicity + age_std + gender + insurance +
                  CCI + (1 | prov_id), 
                data = ARDS_covid, family = binomial,
                control = glmerControl(optimizer = "bobyqa")
)
print(Sys.time()) 
summary(m_4d_2) 

se_4d_2 <- sqrt(diag(vcov(m_4d_2)))
# table of estimates with 95% CI
tab_4d_2 <- cbind(Est = fixef(m_4d_2), 
                  LL = fixef(m_4d_2) - 1.96 * se_4d_2,
                  UL = fixef(m_4d_2) + 1.96 * se_4d_2)
exp(tab_4d_2)

#adjusted + CCI + organ failure
print("adjusted + CCI + organ failure")
print(Sys.time())
m_4d_3 <- glmer(death_or_hospice ~ race_ethnicity + age_std + gender + insurance +
                  CCI + organ_failure + (1 | prov_id), 
                data = ARDS_covid, family = binomial,
                control = glmerControl(optimizer = "bobyqa")
)
print(Sys.time()) 
summary(m_4d_3) 

se_4d_3 <- sqrt(diag(vcov(m_4d_3)))
# table of estimates with 95% CI
tab_4d_3 <- cbind(Est = fixef(m_4d_3), 
                  LL = fixef(m_4d_3) - 1.96 * se_4d_3,
                  UL = fixef(m_4d_3) + 1.96 * se_4d_3)
exp(tab_4d_3)


##Final model with admission month as random intercept (for preCOVID and COVID)
#preCOVID - adjusted + CCI + organ failure
print("precovid - adjusted + CCI + organ failure")
print(Sys.time())
mod_precovid_time <- glmer(death_or_hospice ~ race_ethnicity + age + gender + insurance +
                  CCI + organ_failure + (1 | prov_id) + (1 | adm_mon), 
                data = ARDS_precovid, family = binomial)
print(Sys.time()) 
summary(mod_precovid_time) 

se_precovid_time <- sqrt(diag(vcov(mod_precovid_time)))
# table of estimates with 95% CI
tab_precovid_time <- cbind(Est = fixef(mod_precovid_time), 
                  LL = fixef(mod_precovid_time) - 1.96 * se_precovid_time,
                  UL = fixef(mod_precovid_time) + 1.96 * se_precovid_time)
exp(tab_precovid_time)

#COVID - adjusted + CCI + organ failure
print("covid - adjusted + CCI + organ failure")
print(Sys.time())
mod_covid_time <- glmer(death_or_hospice ~ race_ethnicity + age + gender + insurance +
                  CCI + organ_failure + (1 | prov_id) + (1 | adm_mon), 
                data = ARDS_covid, family = binomial)
print(Sys.time()) 
summary(mod_covid_time) 

se_covid_time <- sqrt(diag(vcov(mod_covid_time)))
# table of estimates with 95% CI
tab_covid_time <- cbind(Est = fixef(mod_covid_time), 
                  LL = fixef(mod_covid_time) - 1.96 * se_covid_time,
                  UL = fixef(mod_covid_time) + 1.96 * se_covid_time)
exp(tab_covid_time)



#--------Stratifying by COVID-19 diagnosis----------------------------------------------------
#STEP 1: Creating COVID variable (with ICD-10 code U07.01 --> used as of April 01, 2020) 
###and COVID_plus variable --> with U07.01 + B97.29 (Other coronavirus as the cause of diseases classified elsewhere - used pre-04/01/2020)
print("creating COVID and COVID_plus variables")
ARDS_data <- ARDS_data %>%
  mutate(COVID = if_else(stringr::str_detect(diagnoses_all, "U07.1"), 1, 0),
         oth_coron = if_else(stringr::str_detect(diagnoses_all, "B97.29"), 1, 0),
         COVID_plus = if_else((COVID == 1) | (oth_coron == 1), 1, 0))

ARDS_precovid <- ARDS_precovid %>%
  mutate(COVID = if_else(stringr::str_detect(diagnoses_all, "U07.1"), 1, 0),
         oth_coron = if_else(stringr::str_detect(diagnoses_all, "B97.29"), 1, 0),
         COVID_plus = if_else((COVID == 1) | (oth_coron == 1), 1, 0))

ARDS_covid <- ARDS_covid %>%
  mutate(COVID = if_else(stringr::str_detect(diagnoses_all, "U07.1"), 1, 0),
         oth_coron = if_else(stringr::str_detect(diagnoses_all, "B97.29"), 1, 0),
         COVID_plus = if_else((COVID == 1) | (oth_coron == 1), 1, 0))


##STEP 2: Create stratified data sets (by COVID)
##### A) COVID
ARDS_data_COVID <- ARDS_data %>%
  filter(COVID == 1)

##### B) non-COVID
ARDS_data_non_COVID <- ARDS_data %>%
  filter(COVID == 0)

##### C) COVID_plus
ARDS_data_COVID_plus <- ARDS_data %>%
  filter(COVID_plus == 1)

##### D) non-COVID_plus
ARDS_data_non_COVID_plus <- ARDS_data %>%
  filter(COVID_plus == 0)


##STEP 3: Run stratified analyses of final model (with CCI + OF)
##### A) COVID - adjusted + CCI + organ failure
print("adjusted + CCI + organ failure")
print(Sys.time())
mod_covid_dx <- glmer(death_or_hospice ~ race_ethnicity + age + gender + insurance +
                  CCI + organ_failure + (1 | prov_id), 
                data = ARDS_data_COVID, family = binomial)
print(Sys.time()) 
summary(mod_covid_dx) 

se_mod_covid_dx <- sqrt(diag(vcov(mod_covid_dx)))
# table of estimates with 95% CI
tab_mod_covid_dx <- cbind(Est = fixef(mod_covid_dx), 
                  LL = fixef(mod_covid_dx) - 1.96 * se_mod_covid_dx,
                  UL = fixef(mod_covid_dx) + 1.96 * se_mod_covid_dx)
exp(tab_mod_covid_dx)

##### B) non-COVID - adjusted + CCI + organ failure
print("adjusted + CCI + organ failure")
print(Sys.time())
mod_no_covid_dx <- glmer(death_or_hospice ~ race_ethnicity + age + gender + insurance +
                        CCI + organ_failure + (1 | prov_id), 
                      data = ARDS_data_non_COVID, family = binomial)
print(Sys.time()) 
summary(mod_no_covid_dx) 

se_mod_no_covid_dx <- sqrt(diag(vcov(mod_no_covid_dx)))
# table of estimates with 95% CI
tab_mod_no_covid_dx <- cbind(Est = fixef(mod_no_covid_dx), 
                          LL = fixef(mod_no_covid_dx) - 1.96 * se_mod_no_covid_dx,
                          UL = fixef(mod_no_covid_dx) + 1.96 * se_mod_no_covid_dx)
exp(tab_mod_no_covid_dx)

##### C) COVID_plus - adjusted + CCI + organ failure
print("adjusted + CCI + organ failure")
print(Sys.time())
mod_covid_plus <- glmer(death_or_hospice ~ race_ethnicity + age + gender + insurance +
                           CCI + organ_failure + (1 | prov_id), 
                         data = ARDS_data_COVID_plus, family = binomial)
print(Sys.time()) 
summary(mod_covid_plus) 

se_mod_covid_plus <- sqrt(diag(vcov(mod_covid_plus)))
# table of estimates with 95% CI
tab_mod_covid_plus <- cbind(Est = fixef(mod_covid_plus), 
                             LL = fixef(mod_covid_plus) - 1.96 * se_mod_covid_plus,
                             UL = fixef(mod_covid_plus) + 1.96 * se_mod_covid_plus)
exp(tab_mod_covid_plus)

##### D) No COVID_plus - adjusted + CCI + organ failure
print("adjusted + CCI + organ failure")
print(Sys.time())
mod_no_covid_plus <- glmer(death_or_hospice ~ race_ethnicity + age + gender + insurance +
                          CCI + organ_failure + (1 | prov_id), 
                        data = ARDS_data_non_COVID_plus, family = binomial)
print(Sys.time()) 
summary(mod_no_covid_plus) 

se_mod_no_covid_plus <- sqrt(diag(vcov(mod_no_covid_plus)))
# table of estimates with 95% CI
tab_mod_no_covid_plus <- cbind(Est = fixef(mod_no_covid_plus), 
                            LL = fixef(mod_no_covid_plus) - 1.96 * se_mod_no_covid_plus,
                            UL = fixef(mod_no_covid_plus) + 1.96 * se_mod_no_covid_plus)
exp(tab_mod_no_covid_plus)


###-------FULL MODEL WITH ADJUSTING FOR COVID-19 DIAGNOSIS---------
##### A) 2017-2021 - adjusted + CCI + organ failure + COVID
print("adjusted + CCI + organ failure + COVID")
print(Sys.time())
mod_full_mod <- glmer(death_or_hospice ~ race_ethnicity + age + gender + insurance +
                        CCI + organ_failure + COVID + (1 | prov_id), 
                      data = ARDS_data, family = binomial)
print(Sys.time()) 
summary(mod_full_mod) 

se_mod_full_mod <- sqrt(diag(vcov(mod_full_mod)))
# table of estimates with 95% CI
tab_mod_full_mod <- cbind(Est = fixef(mod_full_mod), 
                          LL = fixef(mod_full_mod) - 1.96 * se_mod_full_mod,
                          UL = fixef(mod_full_mod) + 1.96 * se_mod_full_mod)
exp(tab_mod_full_mod)


##### B) PRE-COVID PERIOD - adjusted + CCI + organ failure + COVID
print("adjusted + CCI + organ failure + COVID")
print(Sys.time())
mod_precovid <- glmer(death_or_hospice ~ race_ethnicity + age + gender + insurance +
                        CCI + organ_failure + COVID + (1 | prov_id), 
                      data = ARDS_precovid, family = binomial)
print(Sys.time()) 
summary(mod_precovid) 

se_mod_precovid <- sqrt(diag(vcov(mod_precovid)))
# table of estimates with 95% CI
tab_mod_precovid <- cbind(Est = fixef(mod_precovid), 
                          LL = fixef(mod_precovid) - 1.96 * se_mod_precovid,
                          UL = fixef(mod_precovid) + 1.96 * se_mod_precovid)
exp(tab_mod_precovid)


##### C) COVID PERIOD - adjusted + CCI + organ failure
print("adjusted + CCI + organ failure + COVID")
print(Sys.time())
mod_covid <- glmer(death_or_hospice ~ race_ethnicity + age + gender + insurance +
                        CCI + organ_failure + COVID + (1 | prov_id), 
                      data = ARDS_covid, family = binomial)
print(Sys.time()) 
summary(mod_covid) 

se_mod_covid <- sqrt(diag(vcov(mod_covid)))
# table of estimates with 95% CI
tab_mod_covid <- cbind(Est = fixef(mod_covid), 
                          LL = fixef(mod_covid) - 1.96 * se_mod_covid,
                          UL = fixef(mod_covid) + 1.96 * se_mod_covid)
exp(tab_mod_covid)


####------------ASSESSING PATTERNS OVER TIME (and other)---------------------------------

#MORTALITY count by month----
ARDS_data <- ARDS_data %>%				
  mutate(				
    adm_mon = as.numeric(adm_mon),  # Ensure it's numeric				
    year = as.integer(substr(adm_mon, 1, 4)),  # Extract the first 4 digits as year				
    month = as.integer(substr(adm_mon, 6, 7)))  # Extract the last 2 digits as month			

summary(ARDS_data$year)				
summary(ARDS_data$month)				
table(ARDS_data$year)				
table(ARDS_data$year, ARDS_data$month)	

deaths_per_year_month <- ARDS_data %>%                
  filter(death_or_hospice == 1) %>%
  group_by(year, month) %>%             
  summarise(num_deaths = n(), .groups = "drop") %>%               
  arrange(year, month)

print(deaths_per_year_month, n = Inf)


#Mortality (death or hospice) count by month --> Stratified by race/ethnicity
##Non-Hispanic White
table(ARDS_data_White$year)
table(ARDS_data_White$year, ARDS_data_White$month)

deaths_per_year_month_White <- ARDS_data_White %>%                
  filter(death_or_hospice == 1) %>%
  group_by(year, month) %>%             
  summarise(num_deaths = n(), .groups = "drop") %>%               
  arrange(year, month)

print(deaths_per_year_month_White, n = Inf)


##Non-Hispanic Black
table(ARDS_data_Black$year)
table(ARDS_data_Black$year, ARDS_data_Black$month)

deaths_per_year_month_Black <- ARDS_data_Black %>%                
  filter(death_or_hospice == 1) %>%
  group_by(year, month) %>%             
  summarise(num_deaths = n(), .groups = "drop") %>%               
  arrange(year, month)

print(deaths_per_year_month_Black, n = Inf)


##Hispanic
table(ARDS_data_Hisp$year)
table(ARDS_data_Hisp$year, ARDS_data_Hisp$month)

deaths_per_year_month_Hisp <- ARDS_data_Hisp %>%                
  filter(death_or_hospice == 1) %>%
  group_by(year, month) %>%             
  summarise(num_deaths = n(), .groups = "drop") %>%               
  arrange(year, month)

print(deaths_per_year_month_Hisp, n = Inf)


##Asian
table(ARDS_data_Asian_pt$year)
table(ARDS_data_Asian_pt$year, ARDS_data_Asian_pt$month)

deaths_per_year_month_Asian_pt <- ARDS_data_Asian_pt %>%                
  filter(death_or_hospice == 1) %>%
  group_by(year, month) %>%             
  summarise(num_deaths = n(), .groups = "drop") %>%               
  arrange(year, month)

print(deaths_per_year_month_Asian_pt, n = Inf)


##Other
table(ARDS_data_Other$year)
table(ARDS_data_Other$year, ARDS_data_Other$month)

deaths_per_year_month_Other <- ARDS_data_Other %>%                
  filter(death_or_hospice == 1) %>%
  group_by(year, month) %>%             
  summarise(num_deaths = n(), .groups = "drop") %>%               
  arrange(year, month)

print(deaths_per_year_month_Other, n = Inf)

###------EXPLORING MORTALITY BY YEAR----------------------
data_2017 <- c(2017101, 2017102, 2017103, 2017204, 2017205, 2017206, 
               2017307, 2017308, 2017309, 2017410, 2017411, 2017412)

data_2018 <- c(2018101, 2018102, 2018103, 2018204, 2018205, 2018206, 
               2018307, 2018308, 2018309, 2018410, 2018411, 2018412)

data_2019 <- c(2019101, 2019102, 2019103, 2019204, 2019205, 2019206, 
               2019307, 2019308, 2019309, 2019410, 2019411, 2019412)
              
data_2020 <- c(2020101, 2020102, 2020103, 2020204, 2020205, 2020206, 
               2020307, 2020308, 2020309, 2020410, 2020411, 2020412)
        
data_2021 <- c(2021103, 2021204, 2021102, 2021101, 2021205, 2021206, 
               2021307, 2021308, 2021309)

# split ARDS into datasets by year
ARDS_2017 <- ARDS_data %>%
  filter(adm_mon %in% data_2017)

ARDS_2018 <- ARDS_data %>%
  filter(adm_mon %in% data_2018)

ARDS_2019 <- ARDS_data %>%
  filter(adm_mon %in% data_2019)

ARDS_2020 <- ARDS_data %>%
  filter(adm_mon %in% data_2020)

ARDS_2021 <- ARDS_data %>%
  filter(adm_mon %in% data_2021)

# sample size
dim(ARDS_2017)
dim(ARDS_2018)
dim(ARDS_2019)
dim(ARDS_2020)
dim(ARDS_2021)

##2017
print("2017 - adjusted + CCI + organ failure")
print(Sys.time())
mod_2017 <- glmer(death_or_hospice ~ race_ethnicity + age + gender + insurance +
                             CCI + organ_failure + (1 | prov_id), 
                           data = ARDS_2017, family = binomial)
print(Sys.time()) 
summary(mod_2017) 

se_mod_2017 <- sqrt(diag(vcov(mod_2017)))
# table of estimates with 95% CI
tab_mod_2017 <- cbind(Est = fixef(mod_2017), 
                               LL = fixef(mod_2017) - 1.96 * se_mod_2017,
                               UL = fixef(mod_2017) + 1.96 * se_mod_2017)
exp(tab_mod_2017)


##2018
print("2018 - adjusted + CCI + organ failure")
print(Sys.time())
mod_2018 <- glmer(death_or_hospice ~ race_ethnicity + age + gender + insurance +
                    CCI + organ_failure + (1 | prov_id), 
                  data = ARDS_2018, family = binomial)
print(Sys.time()) 
summary(mod_2018) 

se_mod_2018 <- sqrt(diag(vcov(mod_2018)))
# table of estimates with 95% CI
tab_mod_2018 <- cbind(Est = fixef(mod_2018), 
                      LL = fixef(mod_2018) - 1.96 * se_mod_2018,
                      UL = fixef(mod_2018) + 1.96 * se_mod_2018)
exp(tab_mod_2018)


##2019
print("2019 - adjusted + CCI + organ failure")
print(Sys.time())
mod_2019 <- glmer(death_or_hospice ~ race_ethnicity + age + gender + insurance +
                    CCI + organ_failure + (1 | prov_id), 
                  data = ARDS_2019, family = binomial)
print(Sys.time()) 
summary(mod_2019) 

se_mod_2019 <- sqrt(diag(vcov(mod_2019)))
# table of estimates with 95% CI
tab_mod_2019 <- cbind(Est = fixef(mod_2019), 
                      LL = fixef(mod_2019) - 1.96 * se_mod_2019,
                      UL = fixef(mod_2019) + 1.96 * se_mod_2019)
exp(tab_mod_2019)


##2020
print("2020 - adjusted + CCI + organ failure")
print(Sys.time())
mod_2020 <- glmer(death_or_hospice ~ race_ethnicity + age + gender + insurance +
                    CCI + organ_failure + (1 | prov_id), 
                  data = ARDS_2020, family = binomial)
print(Sys.time()) 
summary(mod_2020) 

se_mod_2020 <- sqrt(diag(vcov(mod_2020)))
# table of estimates with 95% CI
tab_mod_2020 <- cbind(Est = fixef(mod_2020), 
                      LL = fixef(mod_2020) - 1.96 * se_mod_2020,
                      UL = fixef(mod_2020) + 1.96 * se_mod_2020)
exp(tab_mod_2020)


##2021
print("2021 - adjusted + CCI + organ failure")
print(Sys.time())
mod_2021 <- glmer(death_or_hospice ~ race_ethnicity + age + gender + insurance +
                    CCI + organ_failure + (1 | prov_id), 
                  data = ARDS_2021, family = binomial)
print(Sys.time()) 
summary(mod_2021) 

se_mod_2021 <- sqrt(diag(vcov(mod_2021)))
# table of estimates with 95% CI
tab_mod_2021 <- cbind(Est = fixef(mod_2021), 
                      LL = fixef(mod_2021) - 1.96 * se_mod_2021,
                      UL = fixef(mod_2021) + 1.96 * se_mod_2021)
exp(tab_mod_2021)

###---------MLM with fixed effect of time----------------
##### adjusted + CCI + organ failure
print("MLM eith FE for time; adjusted + CCI + organ failure")
print(Sys.time())
mod_mlm_time_fe <- glmer(death_or_hospice ~ race_ethnicity + age + gender + insurance +
                          CCI + organ_failure + adm_mon + (1 | prov_id), 
                        data = ARDS_data, family = binomial)
print(Sys.time()) 
summary(mod_mlm_time_fe) 

se_mod_mlm_time_fe <- sqrt(diag(vcov(mod_mlm_time_fe)))
# table of estimates with 95% CI
tab_mod_mlm_time_fe <- cbind(Est = fixef(mod_mlm_time_fe), 
                            LL = fixef(mod_mlm_time_fe) - 1.96 * se_mod_mlm_time_fe,
                            UL = fixef(mod_mlm_time_fe) + 1.96 * se_mod_mlm_time_fe)
exp(tab_mod_mlm_time_fe)

###---------MLM with random intercept for time while still clustering for hospital----------------
##### adjusted + CCI + organ failure
print("MLM with RI for time; adjusted + CCI + organ failure")
print(Sys.time())
mod_mlm_time_ri <- glmer(death_or_hospice ~ race_ethnicity + age + gender + insurance +
                            CCI + organ_failure + (1 | prov_id) + (1 | adm_mon), 
                          data = ARDS_data, family = binomial)
print(Sys.time()) 
summary(mod_mlm_time_ri) 

se_mod_mlm_time_ri <- sqrt(diag(vcov(mod_mlm_time_ri)))
# table of estimates with 95% CI
tab_mod_mlm_time_ri <- cbind(Est = fixef(mod_mlm_time_ri), 
                              LL = fixef(mod_mlm_time_ri) - 1.96 * se_mod_mlm_time_ri,
                              UL = fixef(mod_mlm_time_ri) + 1.96 * se_mod_mlm_time_ri)
exp(tab_mod_mlm_time_ri)

###---------MLM with random slope for time----------------
##### adjusted + CCI + organ failure
print("MLM with RS for time; adjusted + CCI + organ failure")
print(Sys.time())
mod_mlm_time_rs <- glmer(death_or_hospice ~ race_ethnicity + age + gender + insurance +
                           CCI + organ_failure + adm_mon + (adm_mon | prov_id), 
                         data = ARDS_data, family = binomial)
print(Sys.time()) 
summary(mod_mlm_time_rs) 

se_mod_mlm_time_rs <- sqrt(diag(vcov(mod_mlm_time_rs)))
# table of estimates with 95% CI
tab_mod_mlm_time_rs <- cbind(Est = fixef(mod_mlm_time_rs), 
                             LL = fixef(mod_mlm_time_rs) - 1.96 * se_mod_mlm_time_rs,
                             UL = fixef(mod_mlm_time_rs) + 1.96 * se_mod_mlm_time_rs)
exp(tab_mod_mlm_time_rs)

###---------MLM with random slope for time while still clustering for hospital----------------
##### adjusted + CCI + organ failure
print("MLM with RS for time; adjusted + CCI + organ failure")
print(Sys.time())
mod_mlm_time_rs2 <- glmer(death_or_hospice ~ race_ethnicity + age + gender + insurance +
                           CCI + organ_failure + adm_mon + (1 | prov_id) + (adm_mon | prov_id), 
                         data = ARDS_data, family = binomial)
print(Sys.time()) 
summary(mod_mlm_time_rs2) 

se_mod_mlm_time_rs2 <- sqrt(diag(vcov(mod_mlm_time_rs2)))
# table of estimates with 95% CI
tab_mod_mlm_time_rs2 <- cbind(Est = fixef(mod_mlm_time_rs2), 
                             LL = fixef(mod_mlm_time_rs2) - 1.96 * se_mod_mlm_time_rs2,
                             UL = fixef(mod_mlm_time_rs2) + 1.96 * se_mod_mlm_time_rs2)
exp(tab_mod_mlm_time_rs2)


###---------MLM with random slope for time and nested pts (time measured @ pt-level)----------------
##### adjusted + CCI + organ failure
#print("MLM with RS for time and patients; adjusted + CCI + organ failure") ##Got error!! (wouldn't run!!)
#print(Sys.time())
#mod_mlm_time_pt <- glmer(death_or_hospice ~ race_ethnicity + age + gender + insurance +
#                           CCI + organ_failure + adm_mon + (adm_mon | prov_id) + (adm_mon | pat_key), 
#                         data = ARDS_data, family = binomial)
#print(Sys.time()) 
#summary(mod_mlm_time_pt) 

#se_mod_mlm_time_rs <- sqrt(diag(vcov(mod_mlm_time_pt)))
# table of estimates with 95% CI
#tab_mod_mlm_time_pt <- cbind(Est = fixef(mod_mlm_time_pt), 
#                             LL = fixef(mod_mlm_time_pt) - 1.96 * se_mod_mlm_time_pt,
#                             UL = fixef(mod_mlm_time_pt) + 1.96 * se_mod_mlm_time_pt)
#exp(tab_mod_mlm_time_pt)

    
