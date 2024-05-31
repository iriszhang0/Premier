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
print("creating in-hospital mortality varibale")
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

print("Creating CCI ....")
print(Sys.time())
merged_data1 <- merged_data %>%
  filter(ARDS == 1 | J96 == 1 | mech_vent == 1) %>%
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
         cond_19 = if_else(any(str_detect(diagnoses_all,AIDS_codes) &
                                 any(str_detect(diagnoses_all,HIV_Infection)), 6, 0))) %>% #HIV + opportunistic infect.
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



# filter by outcome (e.g. ARDS/J80) ----------------
ARDS_data <- merged_data1 %>%
  filter(ARDS == 1)

J96_data <- merged_data1 %>%
  filter(J96 == 1)

mechvent_data <- merged_data1 %>%
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


# sample size ----------------
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


# death at discharge (expired, expired at home/medical facility/place unknown)
table(ARDS_data$death)
table(ARDS_data$death)/length(ARDS_data$pat_key) #proportion

#obesity
table(ARDS_data$obesity)
table(ARDS_data$obesity)/length(ARDS_data$pat_key) #proportion

#insurance type
table(ARDS_data$insurance)
table(ARDS_data$insurance)/length(ARDS_data$pat_key) #proportion


#death by race/ethnicity
table(ARDS_data$death, ARDS_data$race_ethnicity)



# Bivariate association Table 2a --------------------
chisq.test(ARDS_data$death, ARDS_data$race_ethnicity) #death
chisq.test(ARDS_data$gender, ARDS_data$race_ethnicity) #gender
chisq.test(ARDS_data$obesity, ARDS_data$race_ethnicity) #obesity

chisq.test(ARDS_data$insurance, ARDS_data$race_ethnicity) #insurance type




# Bivariate association Table 2b -----------------
chisq.test(ARDS_data$race_ethnicity, ARDS_data$death) #race
#chisq.test(ARDS_data$hispanic_ind, ARDS_data$death) #ethnicity
chisq.test(ARDS_data$gender, ARDS_data$death) #gender
chisq.test(ARDS_data$obesity, ARDS_data$death) #obesity

chisq.test(ARDS_data$insurance, ARDS_data$death) #insurance type




t.test(filter(ARDS_data, death == 1)$age, filter(ARDS_data, death == 0)$age) #age


# Odds of in-hospital death -----------------------


library(lme4) 
## null --------------
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


## unadjusted -------------------
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


## adjusted ---------------------------
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

# Models restricted for hospitals > 10 patients ----------------
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

## adjusted ---------------------------
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



# Models restricted for hospitals with at least one patient identified as Asian ----------------
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

## adjusted ---------------------------
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



# Odds of death or hospice transfer -----------------------

## null --------------
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


# Models by Covid period ----------------

#dates
unique(ARDS_data$adm_mon)

# pre-covid
pre_covid <- c(2017101, 2016412, 2016411, 2017102, 2016308, 2016410,
               2017103, 2017204, 2017205, 2017206, 2017307, 2017308,
               2017309, 2013309, 2017410, 2017411, 2017412, 2018101,
               2018102, 2018103, 2018204, 2018205, 2018206, 2018307,
               2018308, 2018309, 2018410, 2018411, 2018412, 2019101,
               2019102, 2019103, 2019204, 2019205, 2019206, 2019307,
               2019308, 2019309, 2019410, 2019411, 2019412, 2020101,
               2020102) #up to Feb 2020

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

## multivariate models, death -----------

## pre covid
print(Sys.time())
m_precovid <- glmer(death ~ race_ethnicity + age + gender + insurance + obesity + (1 | prov_id), 
            data = ARDS_precovid, family = binomial)
print(Sys.time()) 
summary(m_precovid) #2 mins

se_precovid <- sqrt(diag(vcov(m_precovid)))
# table of estimates with 95% CI
tab_precovid <- cbind(Est = fixef(m_precovid), 
               LL = fixef(m_precovid) - 1.96 * se_precovid,
               UL = fixef(m_precovid) + 1.96 * se_precovid)
exp(tab_precovid)


## covid
print(Sys.time())
m_covid <- glmer(death ~ race_ethnicity + age + gender + insurance + obesity + (1 | prov_id), 
                    data = ARDS_covid, family = binomial)
print(Sys.time()) #3 mins
summary(m_covid)

se_covid <- sqrt(diag(vcov(m_covid)))
# table of estimates with 95% CI
tab_covid <- cbind(Est = fixef(m_covid), 
                      LL = fixef(m_covid) - 1.96 * se_covid,
                      UL = fixef(m_covid) + 1.96 * se_covid)
exp(tab_covid)

## multivariate models, death or hospice -----------

## pre-covid, death OR hospice
print(Sys.time())
m2_precovid <- glmer(death_or_hospice ~ race_ethnicity + age + gender +
                      insurance + obesity + (1 | prov_id), 
                    data = ARDS_precovid, family = binomial)
print(Sys.time()) 
summary(m2_precovid) 

se2_precovid <- sqrt(diag(vcov(m2_precovid)))
# table of estimates with 95% CI
tab2_precovid <- cbind(Est = fixef(m2_precovid), 
                      LL = fixef(m2_precovid) - 1.96 * se2_precovid,
                      UL = fixef(m2_precovid) + 1.96 * se2_precovid)
round(exp(tab2_precovid),3)


## covid, death OR hospice
print(Sys.time())
m2_covid <- glmer(death_or_hospice ~ race_ethnicity + age + gender +
                    insurance + obesity + (1 | prov_id), 
                 data = ARDS_covid, family = binomial)
print(Sys.time()) 
summary(m2_covid)

se2_covid <- sqrt(diag(vcov(m2_covid)))
# table of estimates with 95% CI
tab2_covid <- cbind(Est = fixef(m2_covid), 
                   LL = fixef(m2_covid) - 1.96 * se2_covid,
                   UL = fixef(m2_covid) + 1.96 * se2_covid)
round(exp(tab2_covid),3)


#test
