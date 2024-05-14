############################################
## Asian respiratory failure outcome
## Author: Zoe Haskell-Craig
## Date Created: May 14th 2024
## Date Last Modified: May 14th 2024
#############################################

# Packages ------------------

library(haven)
library(tidyverse)
library(stringr)



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

print("loading ... region")
setwd("/scratch/Premier/Raw_Data")
print(Sys.time())
region <- read.delim("nyu_providers.txt",sep = "|")
print(Sys.time())

print("merging")
#colnames(region) <- tolower(colnames(region))
merged_data <- left_join(merged_data, region, by = c("prov_id" = "PROV_ID"))
print(Sys.time())


# Generate covariates ------------

## ARDS/J96 ----------------
print("creating ARDS")
merged_data <- merged_data %>%
  mutate(ARDS = if_else(stringr::str_detect(diagnoses_all, "J80"), 1, 0))

print("creating J96")
merged_data <- merged_data %>%
  mutate(J96 = if_else(stringr::str_detect(diagnoses_all, "J96"), 1, 0))

## other covariates ------------
#creating death or hospice transfer
print("creating death or hospice transfer variable")
merged_data$death_or_hospice <- ifelse(merged_data$disc_status %in% 
                                         c(20, 40, 41, 42, 50, 51), 1, 0)


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

## Hospital Region -------------------------------------
print("creating region variable for hosptials")
#merge middle atlantic and new england
merged_data[merged_data$PROV_DIVISION == "MIDDLE ATLANTIC",]$PROV_DIVISION <- "NEW ENGLAND"
#merge mountain and WEST NORTH CENTRAL
merged_data[merged_data$PROV_DIVISION == "MOUNTAIN",]$PROV_DIVISION <- "WEST NORTH CENTRAL"
table(merged_data$PROV_DIVISION)







## Other data cleaning operations --------------------
### filter to just ARDS (J80) patients ----------------
ARDS_data <- merged_data %>%
  filter(ARDS == 1)

### drop "unknown" as missing ----------------


ARDS_data <- ARDS_data %>%
  filter(gender != "U" & #dropped 47 observations
           race_ethnicity != "Unknown") 



## CCI ------------------------
## diagnostic codes per condition ----------------
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














## create CCI score ------------------
print("creating CCI score ...")
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
         cond_19 = if_else(any(str_detect(diagnoses_all,AIDS_codes)), 6, 0)) %>%
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

print(Sys.time()) #16 minutes

# descriptive statistics -----------------

## CCI
summary(ARDS_data$CCI)
mean(ARDS_data$CCI)
sd(ARDS_data$CCI)

#sample sizes
length(unique(merged_data$pat_key)) #total sample size
length(ARDS_data$pat_key) #ARDS patients

length(unique(ARDS_data$prov_id)) #number of hospitals

#gender
sum(ARDS_data$gender == "M") #number of men
table(ARDS_data$gender)
table(ARDS_data$gender)/length(ARDS_data$pat_key) #proportion


#race and ethnicity
table(ARDS_data$race_ethnicity)
table(ARDS_data$race_ethnicity)/length(ARDS_data$pat_key)

# age
summary(ARDS_data$age) #age distribution
mean(ARDS_data$age, na.rm = TRUE)
sd(ARDS_data$age, na.rm = TRUE)
sum(is.na(ARDS_data$age))

# death at discharge or hospice
table(ARDS_data$death_or_hospice)
table(ARDS_data$death_or_hospice)/length(ARDS_data$pat_key) #proportion

#obesity
table(ARDS_data$obesity)
table(ARDS_data$obesity)/length(ARDS_data$pat_key) #proportion

#insurance type
table(ARDS_data$insurance)
table(ARDS_data$insurance)/length(ARDS_data$pat_key) #proportion

# Bivariate association Table 2a --------------------
chisq.test(ARDS_data$death_or_hospice, ARDS_data$race_ethnicity) #death
chisq.test(ARDS_data$gender, ARDS_data$race_ethnicity) #gender
chisq.test(ARDS_data$obesity, ARDS_data$race_ethnicity) #obesity
chisq.test(ARDS_data$insurance, ARDS_data$race_ethnicity) #insurance type

t.test(filter(ARDS_data, race_ethnicity == "nonHispanic_White")$CCI,
       filter(ARDS_data, race_ethnicity == "Asian")$CCI) #CCI



# Bivariate association Table 2b -----------------
chisq.test(ARDS_data$race_ethnicity, ARDS_data$death_or_hospice) #race
chisq.test(ARDS_data$gender, ARDS_data$death_or_hospice) #gender
chisq.test(ARDS_data$obesity, ARDS_data$death_or_hospice) #obesity
chisq.test(ARDS_data$insurance, ARDS_data$death_or_hospice) #insurance type

t.test(filter(ARDS_data, death_or_hospice == 1)$age,
       filter(ARDS_data, death_or_hospice == 0)$age) #age
t.test(filter(ARDS_data, death_or_hospice == 1)$CCI,
       filter(ARDS_data, death_or_hospice == 0)$CCI) #CCI


# Odds of in-hospital death -----------------------
library(lme4) 

## split by Covid period -----------
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


## null --------------
### pre-covid -------------------
m_null_pre <- glmer(death_or_hospice ~ 1 + (1 | prov_id),
                data = ARDS_precovid, family = binomial)
summary(m_null_pre)

se_null_pre <- sqrt(diag(vcov(m_null_pre)))
# table of estimates with 95% CI
tab_null_pre <- cbind(Est = fixef(m_null_pre), 
                  LL = fixef(m_null_pre) - 1.96 * se_null_pre,
                  UL = fixef(m_null_pre) + 1.96 * se_null_pre)
exp(tab_null_pre)



# ICC
# The ICC is calculated by dividing the random effect variance, σ2i, by the total variance, i.e. the sum of the random effect variance and the residual variance, σ2ε.

# hand calculation
sigma2_0 <- as.data.frame(VarCorr(m_null_pre),comp="Variance")$vcov[1]
total_var <- sigma2_0 + (pi^2)/3
icc_hand <- sigma2_0/total_var
icc_hand

### covid -------------------
m_null_c <- glmer(death_or_hospice ~ 1 + (1 | prov_id),
                    data = ARDS_covid, family = binomial)
summary(m_null_c)

se_null_c <- sqrt(diag(vcov(m_null_c)))
# table of estimates with 95% CI
tab_null_c <- cbind(Est = fixef(m_null_c), 
                      LL = fixef(m_null_c) - 1.96 * se_null_c,
                      UL = fixef(m_null_c) + 1.96 * se_null_c)
exp(tab_null_c)



# ICC
# The ICC is calculated by dividing the random effect variance, σ2i, by the total variance, i.e. the sum of the random effect variance and the residual variance, σ2ε.

# hand calculation
sigma2_0 <- as.data.frame(VarCorr(m_null_c),comp="Variance")$vcov[1]
total_var <- sigma2_0 + (pi^2)/3
icc_hand <- sigma2_0/total_var
icc_hand

## unadjusted -------------------
print(Sys.time())
m1 <- glmer(death_or_hospice ~ race_ethnicity + (1 | prov_id), 
            data = ARDS_precovid, family = binomial)
print(Sys.time()) #approx 4 mins to run
summary(m1)
se_1 <- sqrt(diag(vcov(m1)))
# table of estimates with 95% CI
tab_1 <- cbind(Est = fixef(m1), 
               LL = fixef(m1) - 1.96 * se_1,
               UL = fixef(m1) + 1.96 * se_1)
exp(tab_1)

performance::icc(m1)

print(Sys.time())
m2 <- glmer(death_or_hospice ~ race_ethnicity + (1 | prov_id), 
            data = ARDS_covid, family = binomial)
print(Sys.time()) #approx 4 mins to run
summary(m2)
se_2 <- sqrt(diag(vcov(m2)))
# table of estimates with 95% CI
tab_2 <- cbind(Est = fixef(m2), 
               LL = fixef(m2) - 1.96 * se_2,
               UL = fixef(m2) + 1.96 * se_2)
exp(tab_2)

performance::icc(m2)


## adjusted -------------------

print(Sys.time())
m3 <- glmer(death_or_hospice ~ race_ethnicity + age + gender + 
              insurance + CCI + (1 | prov_id), 
            data = ARDS_precovid, family = binomial)
print(Sys.time()) 
summary(m3)

se_3 <- sqrt(diag(vcov(m3)))
# table of estimates with 95% CI
tab_3 <- cbind(Est = round(fixef(m3), digits = 3), 
               LL = round(fixef(m3) - 1.96 * se_3, digits = 3),
               UL = round(fixef(m3) + 1.96 * se_3, digits = 3))
round(exp(tab_3), 3)

# Add ICC for adjusted model
performance::icc(m3)

print(Sys.time())
m4 <- glmer(death_or_hospice ~ race_ethnicity + age + gender + 
              insurance + CCI + (1 | prov_id), 
            data = ARDS_covid, family = binomial)
print(Sys.time()) 
summary(m4)

se_4 <- sqrt(diag(vcov(m4)))
# table of estimates with 95% CI
tab_4 <- cbind(Est = fixef(m4), 
               LL = fixef(m4) - 1.96 * se_4,
               UL = fixef(m4) + 1.96 * se_4)
round(exp(tab_4), 3)

# Add ICC for adjusted model
performance::icc(m4)




