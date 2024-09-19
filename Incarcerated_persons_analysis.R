#------------------------------load and merge data ----------------------

setwd("/scratch/Premier/Raw_Data")


library(haven)
library(dplyr)
library(tidyr)
library(lme4) 

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

#------------------------------create respiratory and prisoner variables and covariates -------------------------

print("creating ARDS")
merged_data <- merged_data %>%
  mutate(ARDS = if_else(stringr::str_detect(diagnoses_all, "J80"), 1, 0))

print("creating acute_RF")
merged_data <- merged_data %>%
  mutate(acute_RF = if_else(stringr::str_detect(diagnoses_all, "J96.0"), 1, 0))


#Create derived prisoner variable 
print("creating D_prisoner")
merged_data <- merged_data %>%
  mutate(D_prisoner = ifelse(point_of_origin == 8 | diagnoses_all == "Z65.1", 1,0))


#create death (anytime) variable
print("creating death variable")
merged_data$death <- ifelse(merged_data$disc_status %in% c(20, 40, 41, 42), 1, 0)

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

#Create obesity variable
print("creating obesity variable")
merged_data <- merged_data %>%
  mutate(E66 = if_else(stringr::str_detect(diagnoses_all, "E66"), 1, 0),
         E66.3 = if_else(stringr::str_detect(diagnoses_all, "E66.3"), 1, 0),
         obesity = if_else((E66 == 1) & (E66.3 == 0), 1, 0)) #obesity for any E66 diagnosis except E66.3

#------------------------------Data restricted for hospitals with at least one patient identified as incarcerated ----------------
##Getting unique prov_id for hospitals with prisoners (D_prisoner = 1)
incarcerated_count <- merged_data %>%
  filter(D_prisoner == 1) %>%
  pull(prov_id) %>%
  unique()

##Subsetting merged_data to include only hospitals with prisoners
incarcerated_data <- merged_data %>%
  filter(prov_id %in% incarcerated_count)



#Filtering prisoner data to include just respiratory failure (J80 and J96.0) patients------
#RF_data <- incarcerated_data %>%
#  filter(ARDS == 1 | acute_RF ==1)
#Subset of only Outpatients---------
#RF_data <-subset(RF_data, i_o_ind =="O")

#Filtered of only Outpatients, unknown, and duplicates -----------
#RF_data1 <- incarcerated_data %>%
#  filter(ARDS == 1 | acute_RF ==1)
#RF_data1 <-subset(RF_data1, i_o_ind !="O")
#RF_data1 <- RF_data1 %>%
#  filter(gender != "U", #dropped 239 observations from gender, which were all from D_prisoner=0 category!!
#         race_ethnicity != "Unknown") #dropped 39,401 obs from race_ethnicity; 39,312 from D_prisoner=0 & 89 from D_prisoner=1
#RF_data_complete <- na.omit(RF_data1)


#Subset of only Psy patients----------------
#RF_datapsy <- incarcerated_data %>%
#  filter(ARDS == 1 | acute_RF ==1)
#RF_datapsy <-subset(RF_datapsy, pat_type == "24")
#length(unique(RF_datapsy$pat_key))

#------------------------------drop both outpatients and psych patients----RF_datafinal------
RF_datafinal <- incarcerated_data %>%
  filter(ARDS == 1 | acute_RF ==1)
length(unique(RF_datafinal$pat_key))

RF_datafinal <-subset(RF_datafinal, i_o_ind !="O")
length(unique(RF_datafinal$pat_key))

RF_datafinal <-subset(RF_datafinal, pat_type != "24")
length(unique(RF_datafinal$pat_key))

#------------------------------drop "unknown" category  for gender as missing ----------------
RF_datafinal <- RF_datafinal %>%
  filter(gender != "U", 
         race_ethnicity != "Unknown") 

#------------------------------Complete Case analysis--------------------
RF_data_complete <- na.omit(RF_datafinal)
length(unique(RF_data_complete$pat_key))


#------------------------------sample size ----------------
#sample sizes
length(unique(merged_data$pat_key)) #total sample size in full dataset
length(unique(incarcerated_data$pat_key)) #sample size in data w/ only hospitals that have at least 1 D_prisoner included
length(unique(RF_data_complete$pat_key)) #total sample size in data with only D_prisoner hospistals and J80/J96.0 pts

#Sample size - incarcerated vs non-incarcerated
table(RF_data_complete$D_prisoner)


#Sample size of complete cases
length(unique(RF_data_complete$pat_key)) ##Total
table(RF_data_complete$D_prisoner)  #Assessing # of prisoners and non-prisoners in complete case sample

#Comparing dropped cases to included cases to ensure no significant differences
##Extracting dropped cases from complete cases analysis
#dropped_cases <- RF_data[!rownames(RF_data) %in% rownames(RF_data_complete), ]

# Descriptive statistics for numeric variables (e.g., age)
#summary(RF_data_complete$age)
#summary(dropped_cases$age)

# Descriptive statistics for categorical variables
#table(RF_data_complete$race_ethnicity)   ## Race/ethnicity)
#table(dropped_cases$race_ethnicity)

#table(RF_data_complete$gender)    ##Gender
#table(dropped_cases$gender)

#table(RF_data_complete$death)    ##Disposition at discharge (alive or dead)
#table(dropped_cases$death)

#table(RF_data_complete$apr_sev)     ##Severity of illness
#table(dropped_cases$apr_sev)


#------------------------------Table 1: Descriptive Statistics in complete cases dataset (D_prisoner=1 vs D_prisoner=0)----------------
##Outcome: disposition at discharge (alive vs dead)
table(RF_data_complete$death, RF_data_complete$D_prisoner) #Death (N) by D_prisoner
table(RF_data_complete$death, RF_data_complete$D_prisoner)/length(RF_data_complete$pat_key) #Proportion of death (%) by D_prisoner

table(RF_data_complete$death)  #Death totals
table(RF_data_complete$death)/length(RF_data_complete$pat_key) #total proportions for death


###Covariates
##race/ethnicity
table(RF_data_complete$race_ethnicity, RF_data_complete$D_prisoner) #Race/ethnicity (N) by D_prisoner
table(RF_data_complete$race_ethnicity, RF_data_complete$D_prisoner)/length(RF_data_complete$pat_key) #Proportion of Race/ethnicity (%) by D_prisoner

table(RF_data_complete$race_ethnicity)  #Race/ethnicity totals
table(RF_data_complete$race_ethnicity)/length(RF_data_complete$pat_key) #total proportions for Race/ethnicity

##Combining Asian and Other categories of race_ethnicity d/t small sample sizes for each in D_prisoner=1)
RF_data_complete <- RF_data_complete %>%
  mutate(race_ethnicity = recode(race_ethnicity, 
                                 "Asian" = "nonHispanic_Other",
                                 "Other" = "nonHispanic_Other"))


##gender
table(RF_data_complete$gender, RF_data_complete$D_prisoner) #Gender (N) by D_prisoner
table(RF_data_complete$gender, RF_data_complete$D_prisoner)/length(RF_data_complete$pat_key) #Proportion of gender (%) by D_prisoner

table(RF_data_complete$gender)  #Gender totals
table(RF_data_complete$gender)/length(RF_data_complete$pat_key) #total proportions for gender


#Severity of Illness
table(RF_data_complete$apr_sev, RF_data_complete$D_prisoner) #Severity of Illness (N) by D_prisoner
table(RF_data_complete$apr_sev, RF_data_complete$D_prisoner)/length(RF_data_complete$pat_key) #Proportion of Severity of Illness (%) by D_prisoner

table(RF_data_complete$apr_sev)  #Severity of Illness totals
table(RF_data_complete$apr_sev)/length(RF_data_complete$pat_key) #total proportions for Severity of Illness

##age
by(RF_data_complete$age, RF_data_complete$D_prisoner, summary)  #Min, max, median, mean 
by(RF_data_complete$age, RF_data_complete$D_prisoner, sd)  #SD of age
#distribution of age for entire sample
summary(RF_data_complete$age) #age distribution
mean(RF_data_complete$age, na.rm = TRUE)
sd(RF_data_complete$age, na.rm = TRUE)
sum(is.na(RF_data_complete$age))  ##No missing data


##LOS
by(RF_data_complete$los, RF_data_complete$D_prisoner, summary)  #Min, max, median, mean 
by(RF_data_complete$los, RF_data_complete$D_prisoner, sd)  #SD of los
#distribution of los for entire sample
summary(RF_data_complete$los) #los distribution
mean(RF_data_complete$los, na.rm = TRUE)
sd(RF_data_complete$los, na.rm = TRUE)
sum(is.na(RF_data_complete$los))  ##No missing data


#------------------------------Table 2: Bivariate association------------------------
chisq.test(RF_data_complete$death, RF_data_complete$D_prisoner) #death
chisq.test(RF_data_complete$race_ethnicity, RF_data_complete$D_prisoner) #race_ethnicity
chisq.test(RF_data_complete$gender, RF_data_complete$D_prisoner) #gender
chisq.test(RF_data_complete$apr_sev, RF_data_complete$D_prisoner) #severity of illness

t.test(filter(RF_data_complete, D_prisoner == 1)$age, filter(RF_data_complete, D_prisoner == 0)$age) #age
t.test(filter(RF_data_complete, D_prisoner == 1)$los, filter(RF_data_complete, D_prisoner == 0)$los) #length of stay


#------------------------------Running simple logistic regression for variables----------------
##Race/ethnicity
# Convert race_ethnicity to factor
RF_data_complete$race_ethnicity <- factor(RF_data_complete$race_ethnicity)
# Re-level race_ethnicity with White as the reference category
RF_data_complete$race_ethnicity <- relevel(RF_data_complete$race_ethnicity, ref = "nonHispanic_White")

# Fit logistic regression model for race_ethnicity
logmod_re <- glm(D_prisoner ~ race_ethnicity, data = RF_data_complete, family = binomial)
summary(logmod_re)

#Finding ORs and 95% CIs for race_ethnicity
# Get the coefficients and their standard errors for each category
coef_and_se_re <- summary(logmod_re)$coefficients[c(2:4), c("Estimate", "Std. Error", "Pr(>|z|)")]
# Calculate odds ratios
odds_ratios_re <- exp(coef_and_se_re[, "Estimate"])
# Calculate lower and upper bounds of 95% confidence intervals
lower_ci_re <- exp(coef_and_se_re[, "Estimate"] - 1.96 * coef_and_se_re[, "Std. Error"])
upper_ci_re <- exp(coef_and_se_re[, "Estimate"] + 1.96 * coef_and_se_re[, "Std. Error"])
# Format the output
output_re <- data.frame(
  OR = odds_ratios_re,
  Lower_CI = lower_ci_re,
  Upper_CI = upper_ci_re,
  p_value = coef_and_se_re[, "Pr(>|z|)"]
)
# Print the output
print(output_re)


##Gender
# Fit logistic regression model for gender
logmod_gender <- glm(D_prisoner ~ gender, data = RF_data_complete, family = binomial)
summary(logmod_gender)
# Get the coefficients and their standard errors gender
coef_and_se_gend <- summary(logmod_gender )$coefficients
# Extract the estimate and standard error for gender
estimate_gender <- coef_and_se_gend[2, 1]
std_error_gender <- coef_and_se_gend[2, 2]
# Calculate the odds ratio
OR_gender <- exp(estimate_gender)
# Calculate the 95% confidence interval
lower_ci_gender <- exp(estimate_gender - 1.96 * std_error_gender)
upper_ci_gender <- exp(estimate_gender + 1.96 * std_error_gender)
# Print the results
cat("Odds Ratio for gender:", OR_gender, "\n")
cat("95% Confidence Interval for gender: [", lower_ci_gender, ", ", upper_ci_gender, "]\n")


##Severity of Illness
##Combining categories into "Not Extreme" and "Extreme" d/t small sample sizes for multiple in D_prisoner=1)
RF_data_complete <- RF_data_complete %>%
  mutate(sev_ill = recode(apr_sev, 
                          '0' = "Not_Extreme",
                          '1' = "Not_Extreme",
                          '2' = "Not_Extreme",
                          '3' = "Not_Extreme",
                          '4' = "Extreme"))
# Convert sev_ill to factor
RF_data_complete$sev_ill <- factor(RF_data_complete$sev_ill)
# Re-level sev_ill with Not Extreme as the reference category
RF_data_complete$sev_ill <- relevel(RF_data_complete$sev_ill, ref = "Not_Extreme")

# Fit logistic regression model for severity of illness
logmod_sev_ill <- glm(D_prisoner ~ sev_ill, data = RF_data_complete, family = binomial)
summary(logmod_sev_ill)
# Get the coefficients and their standard errors sev_ill
coef_and_se_sev <- summary(logmod_sev_ill)$coefficients
# Extract the estimate and standard error for sev_ill
estimate_sev_ill <- coef_and_se_sev[2, 1]
std_error_sev_ill <- coef_and_se_sev[2, 2]
# Calculate the odds ratio
OR_sev_ill <- exp(estimate_sev_ill)
# Calculate the 95% confidence interval
lower_ci_sev_ill <- exp(estimate_sev_ill - 1.96 * std_error_sev_ill)
upper_ci_sev_ill <- exp(estimate_sev_ill + 1.96 * std_error_sev_ill)
# Print the results
cat("Odds Ratio for Severity of Illness:", OR_sev_ill, "\n")
cat("95% Confidence Interval for Severity of Illness: [", lower_ci_sev_ill, ", ", upper_ci_sev_ill, "]\n")


##Age
# Fit logistic regression model for age
logmod_age <- glm(D_prisoner ~ age, data = RF_data_complete, family = binomial)
summary(logmod_age)
# Get the coefficients and their standard errors age
coef_and_se_age <- summary(logmod_age)$coefficients
# Extract the estimate and standard error for age
estimate_age <- coef_and_se_age[2, 1]
std_error_age <- coef_and_se_age[2, 2]
# Calculate the odds ratio
OR_age <- exp(estimate_age)
# Calculate the 95% confidence interval
lower_ci_age <- exp(estimate_age - 1.96 * std_error_age)
upper_ci_age <- exp(estimate_age + 1.96 * std_error_age)
# Print the results
cat("Odds Ratio for age:", OR_age, "\n")
cat("95% Confidence Interval for age: [", lower_ci_age, ", ", upper_ci_age, "]\n")


# Fit logistic regression model for death (as outcome) with D_prisoner
logmod_death <- glm(death ~ D_prisoner, data = RF_data_complete, family = binomial)
summary(logmod_death)
# Get the coefficients and their standard errors D_prisoner
coef_and_se_death <- summary(logmod_death)$coefficients
# Extract the estimate and standard error for D_prisoner
estimate_death <- coef_and_se_death[2, 1]
std_error_death <- coef_and_se_death[2, 2]
# Calculate the odds ratio
OR_death <- exp(estimate_death)
# Calculate the 95% confidence interval
lower_ci_death <- exp(estimate_death - 1.96 * std_error_death)
upper_ci_death <- exp(estimate_death + 1.96 * std_error_death)
# Print the results
cat("Odds Ratio for Death:", OR_death, "\n")
cat("95% Confidence Interval for Death: [", lower_ci_death, ", ", upper_ci_death, "]\n")



#------------------------------Mixed Effects Models----------------------------
library(lme4) 
## Null Model
m_null <- glmer(death ~ 1 + (1 | prov_id),
                data = RF_data_complete, family = binomial)
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


## Unadjusted Model for death, clustering by hospital
print(Sys.time())
mod0 <- glmer(death ~ D_prisoner + (1 | prov_id), 
              data = RF_data_complete, family = binomial)
print(Sys.time()) #approx 6-8 mins to run
summary(mod0)
se_0 <- sqrt(diag(vcov(mod0)))
# table of estimates with 95% CI
tab_0 <- cbind(Est = fixef(mod0), 
               LL = fixef(mod0) - 1.96 * se_0,
               UL = fixef(mod0) + 1.96 * se_0)
exp(tab_0)

performance::icc(mod0)


#Adjusted Model for death, clustering by hospital 
print(Sys.time())
mod1 <- glmer(death ~ D_prisoner + race_ethnicity + age + gender + sev_ill + (1 | prov_id), 
              data = RF_data_complete, family = binomial)
print(Sys.time()) #approx 25 mins to run
summary(mod1)

se_1 <- sqrt(diag(vcov(mod1)))
# table of estimates with 95% CI
tab_1 <- cbind(Est = fixef(mod1), 
               LL = fixef(mod1) - 1.96 * se_1,
               UL = fixef(mod1) + 1.96 * se_1)
exp(tab_1)

# Add ICC for adjusted model
performance::icc(mod1)



#------------------------------Adding CCI to RF_data_complete----------------------------------------------------------------------------------
library(haven)
library(dplyr)
library(tidyr)
library(lme4) 
library(performance)
library(stringr)

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


# CCI score

print(Sys.time())
RF_data_complete <- RF_data_complete %>%
  rowwise() %>%
  mutate(cond_1 = if_else(any(str_detect(diagnoses_all, MI_codes)), 1, 0),
         cond_2 = if_else(any(str_detect(diagnoses_all, congestive_heart_codes)), 1, 0),
         cond_3 = if_else(any(str_detect(diagnoses_all,peripheral_vascular_codes)), 1, 0),
         cond_4 = if_else(any(str_detect(diagnoses_all,cerebrovascular_disease_codes)), 1, 0),
         cond_5 = if_else(any(str_detect(diagnoses_all,dementia_codes)), 1, 0),
         cond_6 = if_else(any(str_detect(diagnoses_all,chronic_pulmonary_codes)), 1, 0),
         cond_7 = if_else(any(str_detect(diagnoses_all,rheumatic_disease_codes)), 1, 0),
         cond_8 = if_else(any(str_detect(diagnoses_all,peptic_ulcer_codes)), 1, 0),
         cond_9 = if_else(any(str_detect(diagnoses_all,mild_liver_codes)), 1, 0))

RF_data_complete <- RF_data_complete %>%
  rowwise() %>%
  mutate(cond_10 = if_else(any(str_detect(diagnoses_all, diabetes_wo_complications_codes)), 1, 0),
         cond_11 = if_else(any(str_detect(diagnoses_all,renal_mildmoderate_codes)), 1, 0))

RF_data_complete <- RF_data_complete %>%
  rowwise() %>%
  mutate (cond_12 = if_else(any(str_detect(diagnoses_all,Diabetes_with_Chronic_Complications)), 2, 0),
          cond_13 = if_else(any(str_detect(diagnoses_all,Hemiplegia_or_Paraplegia)), 2, 0),
          cond_14 = if_else(any(str_detect(diagnoses_all,Any_Malignancy_except_skin)), 2, 0))

RF_data_complete <- RF_data_complete %>%
  rowwise() %>%
  mutate (cond_15 = if_else(any(str_detect(diagnoses_all,Moderate_or_Severe_Liver_Disease)), 3, 0),
          cond_16 = if_else(any(str_detect(diagnoses_all,Renal_Severe)), 3, 0),
          cond_17 = if_else(any(str_detect(diagnoses_all,HIV_Infection)), 3, 0))

RF_data_complete <- RF_data_complete %>%
  rowwise() %>%
  mutate (cond_18 = if_else(any(str_detect(diagnoses_all,Metastatic_Solid_Tumor)), 6, 0),
          cond_19 = if_else(any(str_detect(diagnoses_all,AIDS_codes)) &
                              any(str_detect(diagnoses_all,HIV_Infection)), 6, 0))  #HIV + opportunistic infect.

RF_data_complete <- RF_data_complete %>%  
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


#------------------------------Adding OF to RF_data_complete------------------------------------------------------------------------------

cvd_sofa <- c("R57", "I95.1", "I95.89", "I95.9", "R03.1", "R65.21", "I46.9")
resp_sofa <- c("J96.0", "J96.9", "J80", "R06.00", "R06.03", "R06.09",
               "R06.3", "R06.83", "R06.89", "R09.2")
neuro_sofa <- c("F05", "G93.1", "G93.40", "R40.1", "R40.2")
hema_sofa <- c("D65", "D68.8", "D68.9", "D69.59", "D69.6")
hepatic_sofa <- c("K72.00", "K72.01", "K72.91", "K76.2", "K76.3")
renal_sofa <- c("N17")

RF_data_complete <- RF_data_complete %>%
  rowwise() %>%
  mutate(cvd_score = if_else(any(str_detect(diagnoses_all, cvd_sofa)), 1, 0),
         resp_score = if_else(any(str_detect(diagnoses_all, resp_sofa)), 1, 0),
         neuro_score = if_else(any(str_detect(diagnoses_all, neuro_sofa)), 1, 0),
         hema_score = if_else(any(str_detect(diagnoses_all, hema_sofa)), 1, 0),
         hepatic_score = if_else(any(str_detect(diagnoses_all, hepatic_sofa)), 1, 0),
         renal_score = if_else(any(str_detect(diagnoses_all, renal_sofa)), 1, 0)) %>%
  mutate(organ_failure = cvd_score + resp_score + neuro_score + 
           hema_score + hepatic_score + renal_score)


#------------------------------Nested Mixed Effects Models for RF_data_complete (adjusted by CCI and OF)------------------------------------

##Adjusted Model for death with CCI, clustering by hospital 

print(Sys.time())
mod1 <- glmer(death ~ D_prisoner + race_ethnicity + age + gender + CCI + (1 | prov_id), 
              data = RF_data_complete, family = binomial)
print(Sys.time()) #approx 25 mins to run
summary(mod1)

se_1 <- sqrt(diag(vcov(mod1)))
# table of estimates with 95% CI
tab_1 <- cbind(Est = fixef(mod1), 
               LL = fixef(mod1) - 1.96 * se_1,
               UL = fixef(mod1) + 1.96 * se_1)
exp(tab_1)

# Add ICC for adjusted model
performance::icc(mod1)

##Adjusted Model for death with CCI and Organ failure, clustering by hospital
print(Sys.time())
mod1 <- glmer(death ~ D_prisoner + race_ethnicity + age + gender + CCI + organ_failure + (1 | prov_id), 
              data = RF_data_complete, family = binomial)
print(Sys.time()) #approx 25 mins to run
summary(mod1)

se_1 <- sqrt(diag(vcov(mod1)))
# table of estimates with 95% CI
tab_1 <- cbind(Est = fixef(mod1), 
               LL = fixef(mod1) - 1.96 * se_1,
               UL = fixef(mod1) + 1.96 * se_1)
exp(tab_1)

# Add ICC for adjusted model
performance::icc(mod1)

#Adjusted Model for death with organ failure only, clustering by hospital 
print(Sys.time())
mod1 <- glmer(death ~ D_prisoner + race_ethnicity + age + gender + organ_failure + (1 | prov_id), 
              data = RF_data_complete, family = binomial)
print(Sys.time()) #approx 25 mins to run
summary(mod1)

se_1 <- sqrt(diag(vcov(mod1)))
# table of estimates with 95% CI
tab_1 <- cbind(Est = fixef(mod1), 
               LL = fixef(mod1) - 1.96 * se_1,
               UL = fixef(mod1) + 1.96 * se_1)
exp(tab_1)

# Add ICC for adjusted model
performance::icc(mod1)







#------------------------------Subsetting my race_ethnicity------------------------------
RF_data_Hispanic <-subset(RF_data_complete, race_ethnicity == "Hispanic")
length(unique(RF_data_Hispanic$pat_key))

RF_data_nonHispanicBlack <-subset(RF_data_complete, race_ethnicity == "nonHispanic_Black")
length(unique(RF_data_nonHispanicBlack$pat_key))

RF_data_nonHispanicWhite <-subset(RF_data_complete, race_ethnicity == "nonHispanic_White")
length(unique(RF_data_nonHispanicWhite$pat_key))

RF_data_nonHispanicOther <-subset(RF_data_complete, race_ethnicity == "nonHispanic_Other")
length(unique(RF_data_nonHispanicOther$pat_key))

#Adding CCI and OF to each subset

#Hispanic CCI
print(Sys.time())
RF_data_Hispanic <- RF_data_Hispanic %>%
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
         cond_11 = if_else(any(str_detect(diagnoses_all,renal_mildmoderate_codes)), 1, 0))

RF_data_Hispanic <- RF_data_Hispanic %>%
  rowwise() %>%
  mutate (cond_12 = if_else(any(str_detect(diagnoses_all,Diabetes_with_Chronic_Complications)), 2, 0),
          cond_13 = if_else(any(str_detect(diagnoses_all,Hemiplegia_or_Paraplegia)), 2, 0),
          cond_14 = if_else(any(str_detect(diagnoses_all,Any_Malignancy_except_skin)), 2, 0),
          cond_15 = if_else(any(str_detect(diagnoses_all,Moderate_or_Severe_Liver_Disease)), 3, 0),
          cond_16 = if_else(any(str_detect(diagnoses_all,Renal_Severe)), 3, 0),
          cond_17 = if_else(any(str_detect(diagnoses_all,HIV_Infection)), 3, 0),
          cond_18 = if_else(any(str_detect(diagnoses_all,Metastatic_Solid_Tumor)), 6, 0),
          cond_19 = if_else(any(str_detect(diagnoses_all,AIDS_codes)) &
                              any(str_detect(diagnoses_all,HIV_Infection)), 6, 0))  #HIV + opportunistic infect.

RF_data_Hispanic <- RF_data_Hispanic %>%  
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

#OF
RF_data_Hispanic <- RF_data_Hispanic %>%
  rowwise() %>%
  mutate(cvd_score = if_else(any(str_detect(diagnoses_all, cvd_sofa)), 1, 0),
         resp_score = if_else(any(str_detect(diagnoses_all, resp_sofa)), 1, 0),
         neuro_score = if_else(any(str_detect(diagnoses_all, neuro_sofa)), 1, 0),
         hema_score = if_else(any(str_detect(diagnoses_all, hema_sofa)), 1, 0),
         hepatic_score = if_else(any(str_detect(diagnoses_all, hepatic_sofa)), 1, 0),
         renal_score = if_else(any(str_detect(diagnoses_all, renal_sofa)), 1, 0)) %>%
  mutate(organ_failure = cvd_score + resp_score + neuro_score + 
           hema_score + hepatic_score + renal_score)

#nonHispanic_Black CCI
print(Sys.time())
RF_data_nonHispanicBlack <- RF_data_nonHispanicBlack %>%
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
         cond_11 = if_else(any(str_detect(diagnoses_all,renal_mildmoderate_codes)), 1, 0))

RF_data_nonHispanicBlack <- RF_data_nonHispanicBlack %>%
  rowwise() %>%
  mutate (cond_12 = if_else(any(str_detect(diagnoses_all,Diabetes_with_Chronic_Complications)), 2, 0),
          cond_13 = if_else(any(str_detect(diagnoses_all,Hemiplegia_or_Paraplegia)), 2, 0),
          cond_14 = if_else(any(str_detect(diagnoses_all,Any_Malignancy_except_skin)), 2, 0),
          cond_15 = if_else(any(str_detect(diagnoses_all,Moderate_or_Severe_Liver_Disease)), 3, 0),
          cond_16 = if_else(any(str_detect(diagnoses_all,Renal_Severe)), 3, 0),
          cond_17 = if_else(any(str_detect(diagnoses_all,HIV_Infection)), 3, 0),
          cond_18 = if_else(any(str_detect(diagnoses_all,Metastatic_Solid_Tumor)), 6, 0),
          cond_19 = if_else(any(str_detect(diagnoses_all,AIDS_codes)) &
                              any(str_detect(diagnoses_all,HIV_Infection)), 6, 0))  #HIV + opportunistic infect.

RF_data_nonHispanicBlack <- RF_data_nonHispanicBlack %>%  
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

#OF
RF_data_nonHispanicBlack <- RF_data_nonHispanicBlack %>%
  rowwise() %>%
  mutate(cvd_score = if_else(any(str_detect(diagnoses_all, cvd_sofa)), 1, 0),
         resp_score = if_else(any(str_detect(diagnoses_all, resp_sofa)), 1, 0),
         neuro_score = if_else(any(str_detect(diagnoses_all, neuro_sofa)), 1, 0),
         hema_score = if_else(any(str_detect(diagnoses_all, hema_sofa)), 1, 0),
         hepatic_score = if_else(any(str_detect(diagnoses_all, hepatic_sofa)), 1, 0),
         renal_score = if_else(any(str_detect(diagnoses_all, renal_sofa)), 1, 0)) %>%
  mutate(organ_failure = cvd_score + resp_score + neuro_score + 
           hema_score + hepatic_score + renal_score)


#nonHispanic_White CCI
print(Sys.time())
RF_data_nonHispanicWhite <- RF_data_nonHispanicWhite %>%
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
         cond_11 = if_else(any(str_detect(diagnoses_all,renal_mildmoderate_codes)), 1, 0))

RF_data_nonHispanicWhite <- RF_data_nonHispanicWhite %>%
  rowwise() %>%
  mutate (cond_12 = if_else(any(str_detect(diagnoses_all,Diabetes_with_Chronic_Complications)), 2, 0),
          cond_13 = if_else(any(str_detect(diagnoses_all,Hemiplegia_or_Paraplegia)), 2, 0),
          cond_14 = if_else(any(str_detect(diagnoses_all,Any_Malignancy_except_skin)), 2, 0),
          cond_15 = if_else(any(str_detect(diagnoses_all,Moderate_or_Severe_Liver_Disease)), 3, 0),
          cond_16 = if_else(any(str_detect(diagnoses_all,Renal_Severe)), 3, 0),
          cond_17 = if_else(any(str_detect(diagnoses_all,HIV_Infection)), 3, 0),
          cond_18 = if_else(any(str_detect(diagnoses_all,Metastatic_Solid_Tumor)), 6, 0),
          cond_19 = if_else(any(str_detect(diagnoses_all,AIDS_codes)) &
                              any(str_detect(diagnoses_all,HIV_Infection)), 6, 0))  #HIV + opportunistic infect.

RF_data_nonHispanicWhite <- RF_data_nonHispanicWhite %>%  
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

#OF
RF_data_nonHispanicWhite <- RF_data_nonHispanicWhite %>%
  rowwise() %>%
  mutate(cvd_score = if_else(any(str_detect(diagnoses_all, cvd_sofa)), 1, 0),
         resp_score = if_else(any(str_detect(diagnoses_all, resp_sofa)), 1, 0),
         neuro_score = if_else(any(str_detect(diagnoses_all, neuro_sofa)), 1, 0),
         hema_score = if_else(any(str_detect(diagnoses_all, hema_sofa)), 1, 0),
         hepatic_score = if_else(any(str_detect(diagnoses_all, hepatic_sofa)), 1, 0),
         renal_score = if_else(any(str_detect(diagnoses_all, renal_sofa)), 1, 0)) %>%
  mutate(organ_failure = cvd_score + resp_score + neuro_score + 
           hema_score + hepatic_score + renal_score)


#nonHispanic_Other CCI
print(Sys.time())
RF_data_nonHispanicOther <- RF_data_nonHispanicOther %>%
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
         cond_11 = if_else(any(str_detect(diagnoses_all,renal_mildmoderate_codes)), 1, 0))

RF_data_nonHispanicOther <- RF_data_nonHispanicOther %>%
  rowwise() %>%
  mutate (cond_12 = if_else(any(str_detect(diagnoses_all,Diabetes_with_Chronic_Complications)), 2, 0),
          cond_13 = if_else(any(str_detect(diagnoses_all,Hemiplegia_or_Paraplegia)), 2, 0),
          cond_14 = if_else(any(str_detect(diagnoses_all,Any_Malignancy_except_skin)), 2, 0),
          cond_15 = if_else(any(str_detect(diagnoses_all,Moderate_or_Severe_Liver_Disease)), 3, 0),
          cond_16 = if_else(any(str_detect(diagnoses_all,Renal_Severe)), 3, 0),
          cond_17 = if_else(any(str_detect(diagnoses_all,HIV_Infection)), 3, 0),
          cond_18 = if_else(any(str_detect(diagnoses_all,Metastatic_Solid_Tumor)), 6, 0),
          cond_19 = if_else(any(str_detect(diagnoses_all,AIDS_codes)) &
                              any(str_detect(diagnoses_all,HIV_Infection)), 6, 0))  #HIV + opportunistic infect.

RF_data_nonHispanicOther <- RF_data_nonHispanicOther %>%  
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

#OF
RF_data_nonHispanicOther <- RF_data_nonHispanicOther %>%
  rowwise() %>%
  mutate(cvd_score = if_else(any(str_detect(diagnoses_all, cvd_sofa)), 1, 0),
         resp_score = if_else(any(str_detect(diagnoses_all, resp_sofa)), 1, 0),
         neuro_score = if_else(any(str_detect(diagnoses_all, neuro_sofa)), 1, 0),
         hema_score = if_else(any(str_detect(diagnoses_all, hema_sofa)), 1, 0),
         hepatic_score = if_else(any(str_detect(diagnoses_all, hepatic_sofa)), 1, 0),
         renal_score = if_else(any(str_detect(diagnoses_all, renal_sofa)), 1, 0)) %>%
  mutate(organ_failure = cvd_score + resp_score + neuro_score + 
           hema_score + hepatic_score + renal_score)


#------------------------------Adjusted mixed Effects Model for Hispanic----------------------------
library(lme4) 

#Adjusted model
print(Sys.time())
mod1 <- glmer(death ~ D_prisoner + age + gender + sev_ill + (1 | prov_id), 
              data = RF_data_Hispanic, family = binomial)
print(Sys.time()) #approx 25 mins to run
summary(mod1)

se_1 <- sqrt(diag(vcov(mod1)))
# table of estimates with 95% CI
tab_1 <- cbind(Est = fixef(mod1), 
               LL = fixef(mod1) - 1.96 * se_1,
               UL = fixef(mod1) + 1.96 * se_1)
exp(tab_1)

# Add ICC for adjusted model
performance::icc(mod1)



#adjusted with CCI
print(Sys.time())
mod1 <- glmer(death ~ D_prisoner + age + gender + CCI + (1 | prov_id), 
              data = RF_data_Hispanic, family = binomial)
print(Sys.time()) #approx 25 mins to run
summary(mod1)

se_1 <- sqrt(diag(vcov(mod1)))
# table of estimates with 95% CI
tab_1 <- cbind(Est = fixef(mod1), 
               LL = fixef(mod1) - 1.96 * se_1,
               UL = fixef(mod1) + 1.96 * se_1)
exp(tab_1)

# Add ICC for adjusted model
performance::icc(mod1)



#adjusted with OF
print(Sys.time())
mod1 <- glmer(death ~ D_prisoner + age + gender + organ_failure + (1 | prov_id), 
              data = RF_data_Hispanic, family = binomial)
print(Sys.time()) #approx 25 mins to run
summary(mod1)

se_1 <- sqrt(diag(vcov(mod1)))
# table of estimates with 95% CI
tab_1 <- cbind(Est = fixef(mod1), 
               LL = fixef(mod1) - 1.96 * se_1,
               UL = fixef(mod1) + 1.96 * se_1)
exp(tab_1)

# Add ICC for adjusted model
performance::icc(mod1)

#adjusted with CCI and OF
print(Sys.time())
mod1 <- glmer(death ~ D_prisoner + age + gender + CCI + organ_failure + (1 | prov_id), 
              data = RF_data_Hispanic, family = binomial)
print(Sys.time()) #approx 25 mins to run
summary(mod1)

se_1 <- sqrt(diag(vcov(mod1)))
# table of estimates with 95% CI
tab_1 <- cbind(Est = fixef(mod1), 
               LL = fixef(mod1) - 1.96 * se_1,
               UL = fixef(mod1) + 1.96 * se_1)
exp(tab_1)

# Add ICC for adjusted model
performance::icc(mod1)

#------------------------------Adjusted mixed Effects Models for nonHispanic_Black----------

#sev_ill
print(Sys.time())
mod1 <- glmer(death ~ D_prisoner + age + gender + sev_ill + (1 | prov_id), 
              data = RF_data_nonHispanicBlack, family = binomial)
print(Sys.time()) #approx 25 mins to run
summary(mod1)
se_1 <- sqrt(diag(vcov(mod1)))
# table of estimates with 95% CI
tab_1 <- cbind(Est = fixef(mod1), 
               LL = fixef(mod1) - 1.96 * se_1,
               UL = fixef(mod1) + 1.96 * se_1)
exp(tab_1)
# Add ICC for adjusted model
performance::icc(mod1)


# CCI
print(Sys.time())
mod1 <- glmer(death ~ D_prisoner + age + gender + CCI + (1 | prov_id), 
              data = RF_data_nonHispanicBlack, family = binomial)
print(Sys.time()) #approx 25 mins to run
summary(mod1)
se_1 <- sqrt(diag(vcov(mod1)))
# table of estimates with 95% CI
tab_1 <- cbind(Est = fixef(mod1), 
               LL = fixef(mod1) - 1.96 * se_1,
               UL = fixef(mod1) + 1.96 * se_1)
exp(tab_1)
# Add ICC for adjusted model
performance::icc(mod1)


# OF
print(Sys.time())
mod1 <- glmer(death ~ D_prisoner + age + gender + organ_failure + (1 | prov_id), 
              data = RF_data_nonHispanicBlack, family = binomial)
print(Sys.time()) #approx 25 mins to run
summary(mod1)
se_1 <- sqrt(diag(vcov(mod1)))
# table of estimates with 95% CI
tab_1 <- cbind(Est = fixef(mod1), 
               LL = fixef(mod1) - 1.96 * se_1,
               UL = fixef(mod1) + 1.96 * se_1)
exp(tab_1)
# Add ICC for adjusted model
performance::icc(mod1)


#CCI and OF
print(Sys.time())
mod1 <- glmer(death ~ D_prisoner + age + gender + CCI + organ_failure + (1 | prov_id), 
              data = RF_data_nonHispanicBlack, family = binomial)
print(Sys.time()) #approx 25 mins to run
summary(mod1)

se_1 <- sqrt(diag(vcov(mod1)))
# table of estimates with 95% CI
tab_1 <- cbind(Est = fixef(mod1), 
               LL = fixef(mod1) - 1.96 * se_1,
               UL = fixef(mod1) + 1.96 * se_1)
exp(tab_1)

# Add ICC for adjusted model
performance::icc(mod1)

#------------------------------Adjusted mixed Effects Models for nonHispanic_White----------------------------
#Adjusted model


print(Sys.time())
mod1 <- glmer(death ~ D_prisoner + age + gender + sev_ill + (1 | prov_id), 
              data = RF_data_nonHispanicWhite, family = binomial)
print(Sys.time()) #approx 25 mins to run
summary(mod1)
se_1 <- sqrt(diag(vcov(mod1)))
# table of estimates with 95% CI
tab_1 <- cbind(Est = fixef(mod1), 
               LL = fixef(mod1) - 1.96 * se_1,
               UL = fixef(mod1) + 1.96 * se_1)
exp(tab_1)
# Add ICC for adjusted model
performance::icc(mod1)


# CCI
print(Sys.time())
mod1 <- glmer(death ~ D_prisoner + age + gender + CCI + (1 | prov_id), 
              data = RF_data_nonHispanicWhite, family = binomial)
print(Sys.time()) #approx 25 mins to run
summary(mod1)
se_1 <- sqrt(diag(vcov(mod1)))
# table of estimates with 95% CI
tab_1 <- cbind(Est = fixef(mod1), 
               LL = fixef(mod1) - 1.96 * se_1,
               UL = fixef(mod1) + 1.96 * se_1)
exp(tab_1)
# Add ICC for adjusted model
performance::icc(mod1)


# OF
print(Sys.time())
mod1 <- glmer(death ~ D_prisoner + age + gender + organ_failure + (1 | prov_id), 
              data = RF_data_nonHispanicWhite, family = binomial)
print(Sys.time()) #approx 25 mins to run
summary(mod1)
se_1 <- sqrt(diag(vcov(mod1)))
# table of estimates with 95% CI
tab_1 <- cbind(Est = fixef(mod1), 
               LL = fixef(mod1) - 1.96 * se_1,
               UL = fixef(mod1) + 1.96 * se_1)
exp(tab_1)
# Add ICC for adjusted model
performance::icc(mod1)


#CCI and OF
print(Sys.time())
mod1 <- glmer(death ~ D_prisoner + age + gender + CCI + organ_failure + (1 | prov_id), 
              data = RF_data_nonHispanicWhite, family = binomial)
print(Sys.time()) #approx 25 mins to run
summary(mod1)

se_1 <- sqrt(diag(vcov(mod1)))
# table of estimates with 95% CI
tab_1 <- cbind(Est = fixef(mod1), 
               LL = fixef(mod1) - 1.96 * se_1,
               UL = fixef(mod1) + 1.96 * se_1)
exp(tab_1)

# Add ICC for adjusted model
performance::icc(mod1)


#------------------------------Adjusted mixed Effects Models for nonHispanic_Other----------------------------

#sev_ill
print(Sys.time())
mod1 <- glmer(death ~ D_prisoner + age + gender + sev_ill + (1 | prov_id), 
              data = RF_data_nonHispanicOther, family = binomial)
print(Sys.time()) #approx 25 mins to run
summary(mod1)
se_1 <- sqrt(diag(vcov(mod1)))
# table of estimates with 95% CI
tab_1 <- cbind(Est = fixef(mod1), 
               LL = fixef(mod1) - 1.96 * se_1,
               UL = fixef(mod1) + 1.96 * se_1)
exp(tab_1)
# Add ICC for adjusted model
performance::icc(mod1)


# CCI
print(Sys.time())
mod1 <- glmer(death ~ D_prisoner + age + gender + CCI + (1 | prov_id), 
              data = RF_data_nonHispanicOther, family = binomial)
print(Sys.time()) #approx 25 mins to run
summary(mod1)
se_1 <- sqrt(diag(vcov(mod1)))
# table of estimates with 95% CI
tab_1 <- cbind(Est = fixef(mod1), 
               LL = fixef(mod1) - 1.96 * se_1,
               UL = fixef(mod1) + 1.96 * se_1)
exp(tab_1)
# Add ICC for adjusted model
performance::icc(mod1)


# OF
print(Sys.time())
mod1 <- glmer(death ~ D_prisoner + age + gender + organ_failure + (1 | prov_id), 
              data = RF_data_nonHispanicOther, family = binomial)
print(Sys.time()) #approx 25 mins to run
summary(mod1)
se_1 <- sqrt(diag(vcov(mod1)))
# table of estimates with 95% CI
tab_1 <- cbind(Est = fixef(mod1), 
               LL = fixef(mod1) - 1.96 * se_1,
               UL = fixef(mod1) + 1.96 * se_1)
exp(tab_1)
# Add ICC for adjusted model
performance::icc(mod1)


#CCI and OF
print(Sys.time())
mod1 <- glmer(death ~ D_prisoner + age + gender + CCI + organ_failure + (1 | prov_id), 
              data = RF_data_nonHispanicOther, family = binomial)
print(Sys.time()) #approx 25 mins to run
summary(mod1)

se_1 <- sqrt(diag(vcov(mod1)))
# table of estimates with 95% CI
tab_1 <- cbind(Est = fixef(mod1), 
               LL = fixef(mod1) - 1.96 * se_1,
               UL = fixef(mod1) + 1.96 * se_1)
exp(tab_1)

# Add ICC for adjusted model
performance::icc(mod1)


#-------------------------------EXPLORING ORGAN FAILURE AS OUTCOME--------------------------------------------------
#-----------------------Descriptive Stats--------------------------------------------------------------------------
##Organ Failure
by(RF_data_complete$organ_failure, RF_data_complete$D_prisoner, summary)  #Min, max, median, mean 
by(RF_data_complete$organ_failure, RF_data_complete$D_prisoner, sd)  #SD of organ failure score
#distribution of OF score for entire sample
summary(RF_data_complete$organ_failure) #OF score distribution
mean(RF_data_complete$organ_failure, na.rm = TRUE)
sd(RF_data_complete$organ_failure, na.rm = TRUE)
sum(is.na(RF_data_complete$organ_failure))  ##No missing data

#------------------------------Table 2: Bivariate association for OF------------------------
t.test(filter(RF_data_complete, D_prisoner == 1)$organ_failure, filter(RF_data_complete, D_prisoner == 0)$organ_failure) #organ failure
#chisq.test(RF_data_complete$organ_failure, RF_data_complete$D_prisoner) #organ failure
chisq.test(RF_data_complete$race_ethnicity, RF_data_complete$D_prisoner) #race_ethnicity
chisq.test(RF_data_complete$gender, RF_data_complete$D_prisoner) #gender
chisq.test(RF_data_complete$CCI, RF_data_complete$D_prisoner) #CCI

t.test(filter(RF_data_complete, D_prisoner == 1)$age, filter(RF_data_complete, D_prisoner == 0)$age) #age
t.test(filter(RF_data_complete, D_prisoner == 1)$los, filter(RF_data_complete, D_prisoner == 0)$los) #length of stay


#------------------------------Running simple logistic regression for variables----------------
##Race/ethnicity
# Convert race_ethnicity to factor
RF_data_complete$race_ethnicity <- factor(RF_data_complete$race_ethnicity)
# Re-level race_ethnicity with White as the reference category
RF_data_complete$race_ethnicity <- relevel(RF_data_complete$race_ethnicity, ref = "nonHispanic_White")

# Fit logistic regression model for race_ethnicity
logmod_re <- glm(D_prisoner ~ race_ethnicity, data = RF_data_complete, family = binomial)
summary(logmod_re)

#Finding ORs and 95% CIs for race_ethnicity
# Get the coefficients and their standard errors for each category
coef_and_se_re <- summary(logmod_re)$coefficients[c(2:4), c("Estimate", "Std. Error", "Pr(>|z|)")]
# Calculate odds ratios
odds_ratios_re <- exp(coef_and_se_re[, "Estimate"])
# Calculate lower and upper bounds of 95% confidence intervals
lower_ci_re <- exp(coef_and_se_re[, "Estimate"] - 1.96 * coef_and_se_re[, "Std. Error"])
upper_ci_re <- exp(coef_and_se_re[, "Estimate"] + 1.96 * coef_and_se_re[, "Std. Error"])
# Format the output
output_re <- data.frame(
  OR = odds_ratios_re,
  Lower_CI = lower_ci_re,
  Upper_CI = upper_ci_re,
  p_value = coef_and_se_re[, "Pr(>|z|)"]
)
# Print the output
print(output_re)


##Gender
# Fit logistic regression model for gender
logmod_gender <- glm(D_prisoner ~ gender, data = RF_data_complete, family = binomial)
summary(logmod_gender)
# Get the coefficients and their standard errors gender
coef_and_se_gend <- summary(logmod_gender )$coefficients
# Extract the estimate and standard error for gender
estimate_gender <- coef_and_se_gend[2, 1]
std_error_gender <- coef_and_se_gend[2, 2]
# Calculate the odds ratio
OR_gender <- exp(estimate_gender)
# Calculate the 95% confidence interval
lower_ci_gender <- exp(estimate_gender - 1.96 * std_error_gender)
upper_ci_gender <- exp(estimate_gender + 1.96 * std_error_gender)
# Print the results
cat("Odds Ratio for gender:", OR_gender, "\n")
cat("95% Confidence Interval for gender: [", lower_ci_gender, ", ", upper_ci_gender, "]\n")


##Charleston Comorbidity Index

# Convert sev_ill to factor
#RF_data_complete$sev_ill <- factor(RF_data_complete$sev_ill)
# Re-level sev_ill with Not Extreme as the reference category
#RF_data_complete$sev_ill <- relevel(RF_data_complete$sev_ill, ref = "Not_Extreme")

# Fit logistic regression model for severity of illness
logmod_CCI <- glm(D_prisoner ~ CCI, data = RF_data_complete, family = binomial)
summary(logmod_CCI)
# Get the coefficients and their standard errors sev_ill
coef_and_se_CCI <- summary(logmod_CCI)$coefficients
# Extract the estimate and standard error for sev_ill
estimate_CCI <- coef_and_se_CCI[2, 1]
std_error_CCI <- coef_and_se_CCI[2, 2]
# Calculate the odds ratio
OR_CCI <- exp(estimate_CCI)
# Calculate the 95% confidence interval
lower_ci_CCI <- exp(estimate_CCI - 1.96 * std_error_CCI)
upper_ci_CCI <- exp(estimate_CCI + 1.96 * std_error_CCI)
# Print the results
cat("Odds Ratio for CCI:", OR_CCI, "\n")
cat("95% Confidence Interval for CCI: [", lower_ci_CCI, ", ", upper_ci_CCI, "]\n")


##Age
# Fit logistic regression model for age
logmod_age <- glm(D_prisoner ~ age, data = RF_data_complete, family = binomial)
summary(logmod_age)
# Get the coefficients and their standard errors age
coef_and_se_age <- summary(logmod_age)$coefficients
# Extract the estimate and standard error for age
estimate_age <- coef_and_se_age[2, 1]
std_error_age <- coef_and_se_age[2, 2]
# Calculate the odds ratio
OR_age <- exp(estimate_age)
# Calculate the 95% confidence interval
lower_ci_age <- exp(estimate_age - 1.96 * std_error_age)
upper_ci_age <- exp(estimate_age + 1.96 * std_error_age)
# Print the results
cat("Odds Ratio for age:", OR_age, "\n")
cat("95% Confidence Interval for age: [", lower_ci_age, ", ", upper_ci_age, "]\n")


# Fit linear regression model for Organ Failure (as outcome) with D_prisoner  **OF is a score!!!*
mod_of <- lm(organ_failure ~ D_prisoner, data = RF_data_complete)
summary(mod_of)
# Get the coefficients and their standard errors D_prisoner
coef_and_se_of <- summary(mod_of)$coefficients
# Extract the estimate and standard error for D_prisoner
estimate_of <- coef_and_se_of[2, 1]
std_error_of <- coef_and_se_of[2, 2]
# Calculate the odds ratio
OR_of <- exp(estimate_of)
# Calculate the 95% confidence interval
lower_ci_of <- exp(estimate_of - 1.96 * std_error_of)
upper_ci_of <- exp(estimate_of + 1.96 * std_error_of)
# Print the results
cat("Odds Ratio for Organ Failure:", OR_of, "\n")
cat("95% Confidence Interval for Organ Failure: [", lower_ci_of, ", ", upper_ci_of, "]\n")


#------------------------------Mixed Effects Models----------------------------
library(lme4) 
## Null Model
of_null <- lmer(organ_failure ~ 1 + (1 | prov_id),
                data = RF_data_complete)
summary(of_null)

confint(of_null)  #approx 4 min to run

#se_null <- sqrt(diag(vcov(of_null)))
# table of estimates with 95% CI
#tab_null <- cbind(Est = fixef(of_null), 
#                  LL = fixef(of_null) - 1.96 * se_null,
#                  UL = fixef(of_null) + 1.96 * se_null)
#exp(tab_null)

# ICC
# The ICC is calculated by dividing the random effect variance, σ2i, by the total variance, i.e. the sum of the random effect variance and the residual variance, σ2ε.
# hand calculation
sigma2_0 <- as.data.frame(VarCorr(of_null),comp="Variance")$vcov[1]
total_var <- sigma2_0 + (pi^2)/3
icc_hand <- sigma2_0/total_var
icc_hand


## Unadjusted Model for organ failure, clustering by hospital
print(Sys.time())
mod0 <- lmer(organ_failure ~ D_prisoner + (1 | prov_id), 
              data = RF_data_complete)
print(Sys.time()) #approx 30 sec to run
summary(mod0)

confint(mod0) #approx 6-7 min to run


#se_0 <- sqrt(diag(vcov(mod0)))
# table of estimates with 95% CI
#tab_0 <- cbind(Est = fixef(mod0), 
#               LL = fixef(mod0) - 1.96 * se_0,
#               UL = fixef(mod0) + 1.96 * se_0)
#exp(tab_0)

performance::icc(mod0)


#Adjusted Model for organ failure, clustering by hospital 
print(Sys.time())
mod1 <- lmer(organ_failure ~ D_prisoner + race_ethnicity + age + gender + CCI + (1 | prov_id), 
              data = RF_data_complete)
print(Sys.time()) #approx 30 sec to run
summary(mod1)

confint(mod1) #approx 30 min to run

#se_1 <- sqrt(diag(vcov(mod1)))
# table of estimates with 95% CI
#tab_1 <- cbind(Est = fixef(mod1), 
#               LL = fixef(mod1) - 1.96 * se_1,
#               UL = fixef(mod1) + 1.96 * se_1)
#exp(tab_1)

# Add ICC for adjusted model
performance::icc(mod1)


#------------------------------Subset data by race_ethnicity------------------------------
RF_data_Hispanic <-subset(RF_data_complete, race_ethnicity == "Hispanic")
length(unique(RF_data_Hispanic$pat_key))

RF_data_nonHispanicBlack <-subset(RF_data_complete, race_ethnicity == "nonHispanic_Black")
length(unique(RF_data_nonHispanicBlack$pat_key))

RF_data_nonHispanicWhite <-subset(RF_data_complete, race_ethnicity == "nonHispanic_White")
length(unique(RF_data_nonHispanicWhite$pat_key))

RF_data_nonHispanicOther <-subset(RF_data_complete, race_ethnicity == "nonHispanic_Other")
length(unique(RF_data_nonHispanicOther$pat_key))


#------------------------------Adjusted mixed Effects Model for Hispanic----------------------------
library(lme4) 

#Adjusted model
print(Sys.time())
mod_his <- lmer(organ_failure ~ D_prisoner + age + gender + CCI + (1 | prov_id), 
              data = RF_data_Hispanic)
print(Sys.time()) #approx 2 mins to run
summary(mod_his)

#95% CI
confint(mod_his)

# Add ICC for adjusted model
performance::icc(mod_his)


#------------------------------Adjusted mixed Effects Models for nonHispanic_Black----------

print(Sys.time())
mod_nhBl <- lmer(organ_failure ~ D_prisoner + age + gender + CCI + (1 | prov_id), 
              data = RF_data_nonHispanicBlack)
print(Sys.time()) #approx 4 mins to run
summary(mod_nhBl)

#95% CI
confint(mod_nhBl)

# Add ICC for adjusted model
performance::icc(mod_nhBl)

#------------------------------Adjusted mixed Effects Models for nonHispanic_White----------------------------
#Adjusted model
print(Sys.time())
mod_wh <- lmer(organ_failure ~ D_prisoner + age + gender + CCI + (1 | prov_id), 
              data = RF_data_nonHispanicWhite)
print(Sys.time()) #approx 10 mins to run
summary(mod_wh)

#95% CI
confint(mod_wh)

# Add ICC for adjusted model
performance::icc(mod_wh)


#------------------------------Adjusted mixed Effects Models for nonHispanic_Other----------------------------

print(Sys.time())
mod_oth <- lmer(organ_failure ~ D_prisoner + age + gender + CCI + (1 | prov_id), 
              data = RF_data_nonHispanicOther)
print(Sys.time()) #approx 1-2 mins to run
summary(mod_oth)

#95% CI
confint(mod_oth)

# Add ICC for adjusted model
performance::icc(mod_oth)


#--------------------------------Ordinal Logistic Regression Models--------------------------------------
#Simple OLR
mod_olr <- polr(orgran_failure ~ D_prisoner, data = RF_data_complete, HESS = TRUE)
summary(mod_olr)


