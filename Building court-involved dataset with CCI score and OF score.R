#Building court-involved dataset with CCI score and OF score
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

#Creating D_prisoner variable, ARDS, acute_RF for filtering purposes----------------------------------
print(Sys.time())
print("creating D_prisoner")
merged_data <- merged_data %>%
  mutate(D_prisoner = ifelse(point_of_origin == 8 | diagnoses_all == "Z65.1", 1,0))

print("creating ARDS")
merged_data <- merged_data %>%
  mutate(ARDS = if_else(stringr::str_detect(diagnoses_all, "J80"), 1, 0))

print("creating acute_RF")
merged_data <- merged_data %>%
  mutate(acute_RF = if_else(stringr::str_detect(diagnoses_all, "J96.0"), 1, 0))

#Filter process---------------------------------------------------------------------------------------
incarcerated_count <- merged_data %>%
  filter(D_prisoner == 1) %>%
  pull(prov_id) %>%
  unique()

incarcerated_data <- merged_data %>%
  filter(prov_id %in% incarcerated_count)

merged_data <- incarcerated_data %>%
  filter(ARDS == 1 | acute_RF ==1)

merged_data <-subset(merged_data, i_o_ind !="O")

merged_data <-subset(merged_data, pat_type != "24")

merged_data <- na.omit(merged_data)

#Resulting observation-----------------------------------------------------------------------------------
length(unique(merged_data$pat_key))

# Add CCI -----------------------------------------------------------------------------------------------
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

#Create CCI score----------------------------------------------------------------------------------------
print(Sys.time())
merged_data <- merged_data %>%
  rowwise() %>%
  mutate(cond_1 = if_else(any(str_detect(diagnoses_all, MI_codes)), 1, 0))

merged_data <- merged_data %>%
  rowwise() %>%
  mutate(cond_2 = if_else(any(str_detect(diagnoses_all, congestive_heart_codes)), 1, 0),
         cond_3 = if_else(any(str_detect(diagnoses_all,peripheral_vascular_codes)), 1, 0))
print(Sys.time())

merged_data <- merged_data %>%
  rowwise() %>%
  mutate(cond_4 = if_else(any(str_detect(diagnoses_all,cerebrovascular_disease_codes)), 1, 0),
         cond_5 = if_else(any(str_detect(diagnoses_all,dementia_codes)), 1, 0),
         cond_6 = if_else(any(str_detect(diagnoses_all,chronic_pulmonary_codes)), 1, 0))
print(Sys.time())

merged_data <- merged_data %>%
  rowwise() %>%
  mutate(cond_7 = if_else(any(str_detect(diagnoses_all,rheumatic_disease_codes)), 1, 0),
         cond_8 = if_else(any(str_detect(diagnoses_all,peptic_ulcer_codes)), 1, 0),
         cond_9 = if_else(any(str_detect(diagnoses_all,mild_liver_codes)), 1, 0),
         cond_10 = if_else(any(str_detect(diagnoses_all, diabetes_wo_complications_codes)), 1, 0))


merged_data <- merged_data %>%
  rowwise() %>%
  mutate (cond_11 = if_else(any(str_detect(diagnoses_all,renal_mildmoderate_codes)), 1, 0),
          cond_12 = if_else(any(str_detect(diagnoses_all,Diabetes_with_Chronic_Complications)), 2, 0),
          cond_13 = if_else(any(str_detect(diagnoses_all,Hemiplegia_or_Paraplegia)), 2, 0))

merged_data <- merged_data %>%
  rowwise() %>%
  mutate (cond_14 = if_else(any(str_detect(diagnoses_all,Any_Malignancy_except_skin)), 2, 0),
          cond_15 = if_else(any(str_detect(diagnoses_all,Moderate_or_Severe_Liver_Disease)), 3, 0),
          cond_16 = if_else(any(str_detect(diagnoses_all,Renal_Severe)), 3, 0))

merged_data <- merged_data %>%
  rowwise() %>%
  mutate (cond_17 = if_else(any(str_detect(diagnoses_all,HIV_Infection)), 3, 0),
          cond_18 = if_else(any(str_detect(diagnoses_all,Metastatic_Solid_Tumor)), 6, 0),
          cond_19 = if_else(any(str_detect(diagnoses_all,AIDS_codes)) &
                              any(str_detect(diagnoses_all,HIV_Infection)), 6, 0)) %>% #HIV + opportunistic infect.
  
merged_data <- merged_data %>%  
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
print(Sys.time())

# Organ failure score --------------------------------------------------------------------------------------
## Following Dombrovskiy et al. approach from Bosch et al., (2020)."Predictive Validity"


cvd_sofa <- c("R57", "I95.1", "I95.89", "I95.9", "R03.1", "R65.21", "I46.9")
resp_sofa <- c("J96.0", "J96.9", "J80", "R06.00", "R06.03", "R06.09",
               "R06.3", "R06.83", "R06.89", "R09.2")
neuro_sofa <- c("F05", "G93.1", "G93.40", "R40.1", "R40.2")
hema_sofa <- c("D65", "D68.8", "D68.9", "D69.59", "D69.6")
hepatic_sofa <- c("K72.00", "K72.01", "K72.91", "K76.2", "K76.3")
renal_sofa <- c("N17")

merged_data <- merged_data %>%
  rowwise() %>%
  mutate(cvd_score = if_else(any(str_detect(diagnoses_all, cvd_sofa)), 1, 0),
         resp_score = if_else(any(str_detect(diagnoses_all, resp_sofa)), 1, 0),
         neuro_score = if_else(any(str_detect(diagnoses_all, neuro_sofa)), 1, 0),
         hema_score = if_else(any(str_detect(diagnoses_all, hema_sofa)), 1, 0),
         hepatic_score = if_else(any(str_detect(diagnoses_all, hepatic_sofa)), 1, 0),
         renal_score = if_else(any(str_detect(diagnoses_all, renal_sofa)), 1, 0)) %>%
  mutate(organ_failure = cvd_score + resp_score + neuro_score + 
           hema_score + hepatic_score + renal_score)

