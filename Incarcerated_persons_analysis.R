# load and merge data ----------------------

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

#------------------create respiratory and prisoner variables and covariates -------------------------

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



#-------------Data restricted for hospitals with at least one patient identified as incarcerated ----------------
  ##Getting unique prov_id for hospitals with prisoners (D_prisoner = 1)
incarcerated_count <- merged_data %>%
  filter(D_prisoner == 1) %>%
  pull(prov_id) %>%
  unique()

  ##Subsetting merged_data to include only hospitals with prisoners
incarcerated_data <- merged_data %>%
  filter(prov_id %in% incarcerated_count)



#--------Filtering prisoner data to include just respiratory failure (J80 and J96.0) patients ----------------
RF_data <- incarcerated_data %>%
  filter(ARDS == 1 | acute_RF ==1)


RF_data <-subset(RF_data, i_o_ind =="O")

#-------------DROP Outpatient Patients in RF_Data---RF_data1---------
RF_data1 <- incarcerated_data %>%
  filter(ARDS == 1 | acute_RF ==1)
RF_data1 <-subset(RF_data1, i_o_ind !="O")

RF_data1 <- RF_data1 %>%
  filter(gender != "U", #dropped 239 observations from gender, which were all from D_prisoner=0 category!!
         race_ethnicity != "Unknown") #dropped 39,401 obs from race_ethnicity; 39,312 from D_prisoner=0 & 89 from D_prisoner=1
RF_data_complete <- na.omit(RF_data1)


#------#Psych Patients in RF_Data---psy------
RF_datapsy <- incarcerated_data %>%
  filter(ARDS == 1 | acute_RF ==1)
#------------------Subset to only Psy patients
RF_datapsy <-subset(RF_datapsy, pat_type == "24")
length(unique(RF_datapsy$pat_key))

#--------------drop both outpatients and psych patients----RF_datafinal------
RF_datafinal <- incarcerated_data %>%
  filter(ARDS == 1 | acute_RF ==1)
length(unique(RF_datafinal$pat_key))

RF_datafinal <-subset(RF_datafinal, i_o_ind !="O")
length(unique(RF_datafinal$pat_key))

RF_datafinal <-subset(RF_datafinal, pat_type != "24")
length(unique(RF_datafinal$pat_key))

RF_datafinal <- RF_datafinal %>%
  filter(gender != "U", 
         race_ethnicity != "Unknown") 
RF_data_finalcomplete <- na.omit(RF_datafinal)
length(unique(RF_data_finalcomplete$pat_key))


#---------------------sample size ----------------
#sample sizes
length(unique(merged_data$pat_key)) #total sample size in full dataset
length(unique(incarcerated_data$pat_key)) #sample size in data w/ only hospitals that have at least 1 D_prisoner included
length(unique(RF_data$pat_key)) #total sample size in data with only D_prisoner hospistals and J80/J96.0 pts

#Sample size - incarcerated vs non-incarcerated
table(RF_data$D_prisoner)



#-----drop "unknown" category  for gender as missing ----------------
RF_data <- RF_data %>%
  filter(gender != "U", #dropped 239 observations from gender, which were all from D_prisoner=0 category!!
         race_ethnicity != "Unknown") #dropped 39,401 obs from race_ethnicity; 39,312 from D_prisoner=0 & 89 from D_prisoner=1


#----------------Complete Case analysis--------------------
RF_data_complete <- na.omit(RF_data)
print(RF_data_complete)

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


#-------------Descriptive Statistics in complete cases dataset (D_prisoner=1 vs D_prisoner=0)----------------------
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


#-----------------------------Bivariate association Table 2------------------------
chisq.test(RF_data_complete$death, RF_data_complete$D_prisoner) #death
chisq.test(RF_data_complete$race_ethnicity, RF_data_complete$D_prisoner) #race_ethnicity
chisq.test(RF_data_complete$gender, RF_data_complete$D_prisoner) #gender
chisq.test(RF_data_complete$apr_sev, RF_data_complete$D_prisoner) #severity of illness

t.test(filter(RF_data_complete, D_prisoner == 1)$age, filter(RF_data_complete, D_prisoner == 0)$age) #age
t.test(filter(RF_data_complete, D_prisoner == 1)$los, filter(RF_data_complete, D_prisoner == 0)$los) #length of stay


###Running simple logistic regression for variables----------------
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
## Null Model --------------
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


##Adjusted Model for death, clustering by hospital ---------------------------
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



