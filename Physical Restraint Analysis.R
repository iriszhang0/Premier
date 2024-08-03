# load and merge data ----------------------

setwd("/scratch/Premier/Raw_Data")
#one comment

library(haven)
library(dplyr)
library(tidyr)
library(lme4) 
library(performance)
library(txtplot)

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


#---------------create respiratory variables and covariates ------------

print("creating ARDS")
merged_data <- merged_data %>%
  mutate(ARDS = if_else(stringr::str_detect(diagnoses_all, "J80"), 1, 0))

print("creating Acute RF")
merged_data <- merged_data %>%
  mutate(Acute_RF = if_else(stringr::str_detect(diagnoses_all, "J96.0"), 1, 0))


#create death (anytime) variable
print("creating death variable")
merged_data$death <- ifelse(merged_data$disc_status %in% c(20, 40, 41, 42), 1, 0)

#create in-hospital mortality
print("creating in-hospital mortality varibale")
merged_data$inhospital_death <- ifelse(merged_data$disc_status == 20, 1, 0)

#merge race and hispanicity
merged_data <- merged_data %>%
  mutate(race_ethnicity = case_when(
    race == "B" & hispanic_ind == "Y" ~ "Hispanic_Black",
    hispanic_ind == "Y" ~ "Hispanic", 
    race == "B" & hispanic_ind != "Y" ~ "nonHispanic_Black",
    race == "W" & hispanic_ind != "Y" ~ "nonHispanic_White",
    race == "A" & hispanic_ind != "Y" ~ "Asian",
    race == "U" ~ "Unknown",
    .default = "Other"))

print("creating obesity variable")
merged_data <- merged_data %>%
  mutate(E66 = if_else(stringr::str_detect(diagnoses_all, "E66"), 1, 0),
         E66.3 = if_else(stringr::str_detect(diagnoses_all, "E66.3"), 1, 0),
         obesity = if_else((E66 == 1) & (E66.3 == 0), 1, 0)) #obesity for any E66 diagnosis except E66.3

# insurance type
merged_data <- merged_data %>%
  mutate(insurance = case_when(std_payor %in% c(300, 310, 320) ~ "medicare",
                               std_payor %in% c(330, 340, 350) ~ "medicaid",
                               std_payor %in% c(360, 370, 380) ~ "private",
                               .default = "other"))


#Creating Physical restraint variable (phys_restr)
print("creating phys_restr")
merged_data <- merged_data %>%
  mutate(phys_restr = if_else(stringr::str_detect(diagnoses_all, "Z78.1"), 1, 0))


#Creating Dementia variable (dementia)
#F03=unspecified dementia; F02=Dementia in other diseases classified elsewhere; G30=Alzheimer's Disease
print("creating dementia")
merged_data <- merged_data %>%
  mutate(dementia = if_else(stringr::str_detect(diagnoses_all, "F03|F02|G30"), 1, 0))


#Create derived prisoner variable 
print("creating D_prisoner")
merged_data <- merged_data %>%
  mutate(D_prisoner = ifelse(point_of_origin == 8 | diagnoses_all == "Z65.1", 1,0))

#-----------------------------------------------------------------------
#Remove outpatient cases (keeping only inpatient)
merged_data <- merged_data %>%
  filter(i_o_ind != "O")


#--------------- sample size ----------------
#sample sizes
length(unique(merged_data$pat_key)) #total sample size


#------------------------limiting dataset-----------------------------

# filter to just ARDS (J80) patients ----------------
ARDS_data <- merged_data %>%
  filter(ARDS == 1)
length(unique(ARDS_data$pat_key))  #sample size for ARDS patients


# filter to just phys_restr patients ----------------
phys_restr_data <- merged_data %>%
  filter(phys_restr == 1)
length(unique(phys_restr_data$pat_key)) #sample size for physically restrained patients



#--------------------2x2 tables comparing variable frequency--------------------

# Create a joint frequency table for ARDS and phys_restr
joint_freq_table <- table(merged_data$ARDS, merged_data$phys_restr)
# Print joint frequency table
print("Joint Frequency table for ARDS and phys_restr:")
print(joint_freq_table)


# Create a joint frequency table for Acute_RF and phys_restr
joint_freq_table_2 <- table(merged_data$Acute_RF, merged_data$phys_restr)
# Print joint frequency table
print("Joint Frequency table for Acute_RF and phys_restr:")
print(joint_freq_table_2)


# Create a joint frequency table for gender and phys_restr in ARDS dataset
ARDS_gen_pr_freq <- table(ARDS_data$gender, ARDS_data$phys_restr)
# Print joint frequency table
print("Joint Frequency table for gender and phys_restr in ARDS dataset")
print(ARDS_gen_pr_freq)

# Create a joint frequency table for race and phys_restr in ARDS dataset
ARDS_race_pr_freq <- table(ARDS_data$race, ARDS_data$phys_restr)
# Print joint frequency table
print("Joint Frequency table for race and phys_restr in ARDS dataset")
print(ARDS_race_pr_freq)

# Create a joint frequency table for race_ethnicity and phys_restr in ARDS dataset
ARDS_race_ethn_pr_freq <- table(ARDS_data$race_ethnicity, ARDS_data$phys_restr)
# Print joint frequency table
print("Joint Frequency table for race and phys_restr in ARDS dataset")
print(ARDS_race_ethn_pr_freq)



# filter to just ARDS (J80) and phys_restr patients ----------------
ARDS_pr_data <- merged_data %>%
  filter(ARDS == 1, phys_restr ==1)


#--------------------Table 1a.1: Physical Restraint among ARDS pts------------------------------------
#sample size
length(unique(ARDS_pr_data$pat_key))

#gender
table(ARDS_pr_data$gender)
table(ARDS_pr_data$gender)/length(ARDS_pr_data$pat_key) #proportion

#race and ethnicity
table(ARDS_pr_data$race_ethnicity)
table(ARDS_pr_data$race_ethnicity)/length(ARDS_pr_data$pat_key) #proportion

#race
table(ARDS_pr_data$race) #distribution by race
table(ARDS_pr_data$race)/length(ARDS_pr_data$pat_key) #proportion by race

#hispanicity
table(ARDS_pr_data$hispanic_ind)
table(ARDS_pr_data$hispanic_ind)/length(ARDS_pr_data$pat_key) #proportion

#insurance type
table(ARDS_pr_data$insurance)
table(ARDS_pr_data$insurance)/length(ARDS_pr_data$pat_key) #proportion

#obesity
table(ARDS_pr_data$obesity)
table(ARDS_pr_data$obesity)/length(ARDS_pr_data$pat_key) #proportion

#dementia
table(ARDS_pr_data$dementia)
table(ARDS_pr_data$dementia)/length(ARDS_pr_data$pat_key) #proportion

# death at discharge (expired, expired at home/medical facility/place unknown)
table(ARDS_pr_data$death)
table(ARDS_pr_data$death)/length(ARDS_pr_data$pat_key) #proportion

# age
summary(ARDS_pr_data$age) #age distribution
mean(ARDS_pr_data$age, na.rm = TRUE)
sd(ARDS_pr_data$age, na.rm = TRUE)
sum(is.na(ARDS_pr_data$age))

# length of stay
summary(ARDS_pr_data$los)
mean(ARDS_pr_data$los, na.rm = TRUE)
sd(ARDS_pr_data$los, na.rm = TRUE)
sum(is.na(ARDS_pr_data$los))


#----------------------Table 1a.2: Comparison of ARDS patients with and w/o physical restraint---------------

#sample size
length(unique(ARDS_data$pat_key))

#gender
table(ARDS_data$gender, ARDS_data$phys_restr) #Gender (N) by phys_restr
table(ARDS_data$gender, ARDS_data$phys_restr)/length(ARDS_data$pat_key) #proportion of gender (%) by phys_rest

table(ARDS_data$gender)  #Gender totals
table(ARDS_data$gender)/length(ARDS_data$pat_key) #total proportions for gender

#race and ethnicity
table(ARDS_data$race_ethnicity, ARDS_data$phys_restr) #Race/ethnicity (N) by phys_restr
table(ARDS_data$race_ethnicity, ARDS_data$phys_restr)/length(ARDS_data$pat_key) #proportion (%) by phys_rest

table(ARDS_data$race_ethnicity)  #race_ethnicity totals
table(ARDS_data$race_ethnicity)/length(ARDS_data$pat_key) #total proportions for race_ethnicity

#race
table(ARDS_data$race, ARDS_data$phys_restr) #Race (N) by phys_restr
table(ARDS_data$race, ARDS_data$phys_restr)/length(ARDS_data$pat_key) #proportion of race (%) by phys_rest

table(ARDS_data$race)  #race totals
table(ARDS_data$race)/length(ARDS_data$pat_key) #total proportions for race

#hispanicity
table(ARDS_data$hispanic_ind, ARDS_data$phys_restr) #Hispanic ethnicity (N) by phys_restr
table(ARDS_data$hispanic_ind, ARDS_data$phys_restr)/length(ARDS_data$pat_key) #proportion (%) by phys_rest

table(ARDS_data$hispanic_ind)  #hispanic_ind totals
table(ARDS_data$hispanic_ind)/length(ARDS_data$pat_key) #total proportions for hispanic_ind

#insurance type
table(ARDS_data$insurance, ARDS_data$phys_restr) #Insurance type (N) by phys_restr
table(ARDS_data$insurance, ARDS_data$phys_restr)/length(ARDS_data$pat_key) #proportion (%) by phys_rest

table(ARDS_data$insurance)  #insurance totals
table(ARDS_data$insurance)/length(ARDS_data$pat_key) #total proportions for insurance

#obesity
table(ARDS_data$obesity, ARDS_data$phys_restr) #Obesity (N) by phys_restr
table(ARDS_data$obesity, ARDS_data$phys_restr)/length(ARDS_data$pat_key) #proportion (%) by phys_rest

table(ARDS_data$obesity)  #obesity totals
table(ARDS_data$obesity)/length(ARDS_data$pat_key) #total proportions for obesity

#dementia
table(ARDS_data$dementia, ARDS_data$phys_restr) #dementia (N) by phys_restr
table(ARDS_data$dementia, ARDS_data$phys_restr)/length(ARDS_data$pat_key) #proportion (%) by phys_rest

table(ARDS_data$dementia)  #dementia totals
table(ARDS_data$dementia)/length(ARDS_data$pat_key) #total proportions for dementia


##Outcome: disposition at discharge (alive vs dead)
table(ARDS_data$death, ARDS_data$phys_restr) #Death (N) by phys_restr
table(ARDS_data$death, ARDS_data$phys_restr)/length(ARDS_data$pat_key) #Proportion of death (%) by phys_restr

table(ARDS_data$death)  #Death totals
table(ARDS_data$death)/length(ARDS_data$pat_key) #total proportions for death

##age
by(ARDS_data$age, ARDS_data$phys_restr, summary)  #Min, max, median, mean by phys_restr
by(ARDS_data$age, ARDS_data$phys_restr, sd)  #SD of age
#distribution of age for entire sample
summary(ARDS_data$age) #age distribution
mean(ARDS_data$age, na.rm = TRUE)
sd(ARDS_data$age, na.rm = TRUE)
sum(is.na(ARDS_data$age))  ##No missing data

##LOS
by(ARDS_data$los, ARDS_data$phys_restr, summary)  #Min, max, median, mean 
by(ARDS_data$los, ARDS_data$phys_restr, sd)  #SD of los
#distribution of los for entire sample
summary(ARDS_data$los) #los distribution
mean(ARDS_data$los, na.rm = TRUE)
sd(ARDS_data$los, na.rm = TRUE)
sum(is.na(ARDS_data$los))  ##No missing data


#--------------------Reshaping variables to eliminate small sample sizes in categories---------------------

#Creating slightly different race/ethnicity variable, merging Hispanic and Hispanic Black categories d/t small n
ARDS_pr_data <- ARDS_pr_data %>%
  mutate(race_ethnicity = ifelse(race_ethnicity %in% c("Hispanic", "Hispanic_Black"), "Hispanic", race_ethnicity))

ARDS_data <- ARDS_data %>%
  mutate(race_ethnicity = ifelse(race_ethnicity %in% c("Hispanic", "Hispanic_Black"), "Hispanic", race_ethnicity))

# Drop rows where gender is 'unknown'
ARDS_data <- ARDS_data %>%
  filter(gender != 'U')


#------------------Assessing distribution in Age and LOS (continuous variables)--------------------------
#Creating dataframe for age
ARDS_age <- ARDS_data$age
# Create a histogram of the age variable
ards_hist_age <- hist(ARDS_age, plot = FALSE)
# Display the histogram as a text-based plot (age is x-axis; frequency on y-axis)
txtplot(ards_hist_age$counts)
#______________________________________________________________________


# Bivariate association Table 2a - phys_restr among ARDS pts --------------------
chisq.test(ARDS_data$death, ARDS_data$phys_restr) #death
#chisq.test(ARDS_data$hispanic_ind, ARDS_data$phys_restr) #Hispanic ethnicity
#chisq.test(ARDS_data$race, ARDS_data$phys_restr) #race
chisq.test(ARDS_data$race_ethnicity, ARDS_data$phys_restr) #race_ethnicity
chisq.test(ARDS_data$gender, ARDS_data$phys_restr) #gender
chisq.test(ARDS_data$obesity, ARDS_data$phys_restr) #obesity
chisq.test(ARDS_data$dementia, ARDS_data$phys_restr) #dementia
chisq.test(ARDS_data$insurance, ARDS_data$phys_restr) #insurance type

t.test(filter(ARDS_data, phys_restr == 1)$age, filter(ARDS_data, phys_restr == 0)$age) #age
t.test(filter(ARDS_data, phys_restr == 1)$los, filter(ARDS_data, phys_restr == 0)$los) #length of stay


###Running simple logistic regression for variables with >2 categories ----------------
# Convert race_ethnicity to factor
ARDS_data$race_ethnicity <- factor(ARDS_data$race_ethnicity)
# Re-level race_ethnicity with White as the reference category
ARDS_data$race_ethnicity <- relevel(ARDS_data$race_ethnicity, ref = "nonHispanic_White")

# Fit logistic regression model for race_ethnicity
logmod_re <- glm(phys_restr ~ race_ethnicity, data = ARDS_data, family = binomial)
summary(logmod_re)

#Finding ORs and 95% CIs for race_ethnicity
# Get the coefficients and their standard errors for each level of insurance
coef_and_se_re <- summary(logmod_re)$coefficients[c(2:6), c("Estimate", "Std. Error", "Pr(>|z|)")]
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



# Convert insurance to factor
ARDS_data$insurance <- factor(ARDS_data$insurance)
# Re-level insurance with Private as the reference category
ARDS_data$insurance <- relevel(ARDS_data$insurance, ref = "private")
# Fit logistic regression model for insurance
logmod_insur <- glm(phys_restr ~ insurance, data = ARDS_data, family = binomial)
summary(logmod_insur)

#Finding ORs and 95% CIs for insurance
# Get the coefficients and their standard errors for each level of insurance
coef_and_se_p <- summary(logmod_insur)$coefficients[c(2:4), c("Estimate", "Std. Error", "Pr(>|z|)")]
# Calculate odds ratios
odds_ratios <- exp(coef_and_se_p[, "Estimate"])
# Calculate lower and upper bounds of 95% confidence intervals
lower_ci <- exp(coef_and_se_p[, "Estimate"] - 1.96 * coef_and_se_p[, "Std. Error"])
upper_ci <- exp(coef_and_se_p[, "Estimate"] + 1.96 * coef_and_se_p[, "Std. Error"])
# Format the output
output <- data.frame(
  OR = odds_ratios,
  Lower_CI = lower_ci,
  Upper_CI = upper_ci,
  p_value = coef_and_se_p[, "Pr(>|z|)"]
)
# Print the output
print(output)


# Fit logistic regression model for gender
logmod_gender <- glm(phys_restr ~ gender, data = ARDS_data, family = binomial)
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


# Fit logistic regression model for obesity
logmod_obesity <- glm(phys_restr ~ obesity, data = ARDS_data, family = binomial)
summary(logmod_obesity)
# Get the coefficients and their standard errors obesity
coef_and_se_obesity <- summary(logmod_obesity )$coefficients
# Extract the estimate and standard error for gender
estimate_obesity <- coef_and_se_obesity[2, 1]
std_error_obesity <- coef_and_se_obesity[2, 2]
# Calculate the odds ratio
OR_obesity <- exp(estimate_obesity)
# Calculate the 95% confidence interval
lower_ci_obesity <- exp(estimate_obesity - 1.96 * std_error_obesity)
upper_ci_obesity <- exp(estimate_obesity + 1.96 * std_error_obesity)
# Print the results
cat("Odds Ratio for obesity:", OR_obesity, "\n")
cat("95% Confidence Interval for obesity: [", lower_ci_obesity, ", ", upper_ci_obesity, "]\n")


# Fit logistic regression model for dementia
logmod_dem <- glm(phys_restr ~ dementia, data = ARDS_data, family = binomial)
summary(logmod_dem)
# Get the coefficients and their standard errors dementia
coef_and_se_dem <- summary(logmod_dem )$coefficients
# Extract the estimate and standard error for dementia
estimate_dem <- coef_and_se_dem[2, 1]
std_error_dem <- coef_and_se_dem[2, 2]
# Calculate the odds ratio
OR_dem <- exp(estimate_dem)
# Calculate the 95% confidence interval
lower_ci_dem <- exp(estimate_dem - 1.96 * std_error_dem)
upper_ci_dem <- exp(estimate_dem + 1.96 * std_error_dem)
# Print the results
cat("Odds Ratio for Dementia:", OR_dem, "\n")
cat("95% Confidence Interval for Dementia: [", lower_ci_dem, ", ", upper_ci_dem, "]\n")


# Fit logistic regression model for age
logmod_age <- glm(phys_restr ~ age, data = ARDS_data, family = binomial)
summary(logmod_age)
# Get the coefficients and their standard errors age
coef_and_se_age <- summary(logmod_age )$coefficients
# Extract the estimate and standard error for age
estimate_age <- coef_and_se_age[2, 1]
std_error_age <- coef_and_se_age[2, 2]
# Calculate the odds ratio
OR_age <- exp(estimate_age)
# Calculate the 95% confidence interval
lower_ci_age <- exp(estimate_age - 1.96 * std_error_age)
upper_ci_age <- exp(estimate_age + 1.96 * std_error_age)
# Print the results
cat("Odds Ratio for Age:", OR_age, "\n")
cat("95% Confidence Interval for Age: [", lower_ci_age, ", ", upper_ci_age, "]\n")


# Fit logistic regression model for LOS
logmod_los <- glm(phys_restr ~ los, data = ARDS_data, family = binomial)
summary(logmod_los)
# Get the coefficients and their standard errors LOS
coef_and_se_los <- summary(logmod_los )$coefficients
# Extract the estimate and standard error for LOS
estimate_los <- coef_and_se_los[2, 1]
std_error_los <- coef_and_se_los[2, 2]
# Calculate the odds ratio
OR_los <- exp(estimate_los)
# Calculate the 95% confidence interval
lower_ci_los <- exp(estimate_los - 1.96 * std_error_los)
upper_ci_los <- exp(estimate_los + 1.96 * std_error_los)
# Print the results
cat("Odds Ratio for LOS:", OR_los, "\n")
cat("95% Confidence Interval for LOS: [", lower_ci_los, ", ", upper_ci_los, "]\n")

#-------------------------------------------------------------------------------
# Fit logistic regression model for death (as outcome) with physical restraint
logmod_pr_death <- glm(death ~ phys_restr, data = ARDS_data, family = binomial)
summary(logmod_pr_death)
# Get the coefficients and their standard errors phys_restr
coef_and_se_pr <- summary(logmod_pr_death)$coefficients
# Extract the estimate and standard error for phys_restr
estimate_phys_restr <- coef_and_se_pr[2, 1]
std_error_phys_restr <- coef_and_se_pr[2, 2]
# Calculate the odds ratio
OR_phys_restr <- exp(estimate_phys_restr)
# Calculate the 95% confidence interval
lower_ci_phys_restr <- exp(estimate_phys_restr - 1.96 * std_error_phys_restr)
upper_ci_phys_restr <- exp(estimate_phys_restr + 1.96 * std_error_phys_restr)
# Print the results
cat("Odds Ratio for phys_restr:", OR_phys_restr, "\n")
cat("95% Confidence Interval for phys_restr: [", lower_ci_phys_restr, ", ", upper_ci_phys_restr, "]\n")



#----------------------Assessing relationship between obesity and gender--------------------
chisq.test(ARDS_data$obesity, ARDS_data$gender) 

# Fit logistic regression model for gender with obesity as outcome
logmod_gen_obs <- glm(obesity ~ gender, data = ARDS_data, family = binomial)
summary(logmod_gen_obs)
# Get the coefficients and their standard errors
coef_and_se_genobs <- summary(logmod_gen_obs )$coefficients
# Extract the estimate and standard error 
estimate_genobs <- coef_and_se_genobs[2, 1]
std_error_genobs <- coef_and_se_genobs[2, 2]
# Calculate the odds ratio
OR_genobs <- exp(estimate_genobs)
# Calculate the 95% confidence interval
lower_ci_genobs <- exp(estimate_genobs - 1.96 * std_error_genobs)
upper_ci_genobs <- exp(estimate_genobs + 1.96 * std_error_genobs)
# Print the results
cat("Odds Ratio for gender:", OR_genobs, "\n")
cat("95% Confidence Interval for gender: [", lower_ci_genobs, ", ", upper_ci_genobs, "]\n")

#-----------------------------------------------------------------------------------------

#---------------------------------------Logistic Regression Model for phys_restr among ARDS patients----------------

#Model 1: age, race.ethnicity, gender
ARDS_Mod_1 <- glm(phys_restr ~ age + race_ethnicity + gender, data = ARDS_data, family = binomial)
summary(ARDS_Mod_1)

#Model 1 Coefficients
# Get the coefficients and their standard errors
coef_and_se_ards_mod_1 <- summary(ARDS_Mod_1)$coefficients
# Extract the estimate and standard error 
estimate_ards_mod_1 <- coef_and_se_ards_mod_1[2, 1]
std_error_ards_mod_1 <- coef_and_se_ards_mod_1[2, 2]
# Calculate the odds ratio
OR_ards_mod_1 <- exp(estimate_ards_mod_1)
# Calculate the 95% confidence interval
lower_ci_ards_mod_1 <- exp(estimate_ards_mod_1 - 1.96 * std_error_ards_mod_1)
upper_ci_ards_mod_1 <- exp(estimate_ards_mod_1 + 1.96 * std_error_ards_mod_1)
# Print the results
cat("Odds Ratio for ARDS Model 1:", OR_ards_mod_1, "\n")
cat("95% Confidence Interval for ARDS Model 1: [", lower_ci_ards_mod_1, ", ", upper_ci_ards_mod_1, "]\n")


#_______________________________NOW REDO ANALYSIS IN J96.0 (Acute RF) POPULATION__________________
# filter to just J96 patients ----------------
Acute_RF_data <- merged_data %>%
  filter(Acute_RF == 1)


# filter to just ARDS (J80) and phys_restr patients ----------------
Acute_RF_pr_data <- merged_data %>%
  filter(Acute_RF == 1, phys_restr ==1)


#----------------------------Table 2a.1: Descriptive Statistics for Acute_RF and phys_restr--------------
#sample size
length(unique(Acute_RF_pr_data$pat_key))

#gender
table(Acute_RF_pr_data$gender)
table(Acute_RF_pr_data$gender)/length(Acute_RF_pr_data$pat_key) #proportion

#race and ethnicity
table(Acute_RF_pr_data$race_ethnicity)
table(Acute_RF_pr_data$race_ethnicity)/length(Acute_RF_pr_data$pat_key) #proportion

#race
table(Acute_RF_pr_data$race) #distribution by race
table(Acute_RF_pr_data$race)/length(Acute_RF_pr_data$pat_key) #proportion by race

#hispanicity
table(Acute_RF_pr_data$hispanic_ind)
table(Acute_RF_pr_data$hispanic_ind)/length(Acute_RF_pr_data$pat_key) #proportion

#insurance type
table(Acute_RF_pr_data$insurance)
table(Acute_RF_pr_data$insurance)/length(Acute_RF_pr_data$pat_key) #proportion

#obesity
table(Acute_RF_pr_data$obesity)
table(Acute_RF_pr_data$obesity)/length(Acute_RF_pr_data$pat_key) #proportion

#dementia
table(Acute_RF_pr_data$dementia)
table(Acute_RF_pr_data$dementia)/length(Acute_RF_pr_data$pat_key) #proportion

# death at discharge (expired, expired at home/medical facility/place unknown)
table(Acute_RF_pr_data$death)
table(Acute_RF_pr_data$death)/length(Acute_RF_pr_data$pat_key) #proportion

# age
summary(Acute_RF_pr_data$age) #age distribution
mean(Acute_RF_pr_data$age, na.rm = TRUE)
sd(Acute_RF_pr_data$age, na.rm = TRUE)
sum(is.na(Acute_RF_pr_data$age))

# length of stay
summary(Acute_RF_pr_data$los)
mean(Acute_RF_pr_data$los, na.rm = TRUE)
sd(Acute_RF_pr_data$los, na.rm = TRUE)
sum(is.na(Acute_RF_pr_data$los))


#----------------------Table 2a.2: Comparison of Acute_RF patients with and w/o physical restraint---------------

#sample size
length(unique(Acute_RF_data$pat_key))

#gender
table(Acute_RF_data$gender, Acute_RF_data$phys_restr) #Gender (N) by phys_restr
table(Acute_RF_data$gender, Acute_RF_data$phys_restr)/length(Acute_RF_data$pat_key) #proportion of gender (%) by phys_rest

table(Acute_RF_data$gender)  #Gender totals
table(Acute_RF_data$gender)/length(Acute_RF_data$pat_key) #total proportions for gender

#race and ethnicity
table(Acute_RF_data$race_ethnicity, Acute_RF_data$phys_restr) #Race/ethnicity (N) by phys_restr
table(Acute_RF_data$race_ethnicity, Acute_RF_data$phys_restr)/length(Acute_RF_data$pat_key) #proportion (%) by phys_rest

table(Acute_RF_data$race_ethnicity)  #race_ethnicity totals
table(Acute_RF_data$race_ethnicity)/length(Acute_RF_data$pat_key) #total proportions for race_ethnicity

#race
table(Acute_RF_data$race, Acute_RF_data$phys_restr) #Race (N) by phys_restr
table(Acute_RF_data$race, Acute_RF_data$phys_restr)/length(Acute_RF_data$pat_key) #proportion of race (%) by phys_rest

table(Acute_RF_data$race)  #race totals
table(Acute_RF_data$race)/length(Acute_RF_data$pat_key) #total proportions for race

#hispanicity
table(Acute_RF_data$hispanic_ind, Acute_RF_data$phys_restr) #Hispanic ethnicity (N) by phys_restr
table(Acute_RF_data$hispanic_ind, Acute_RF_data$phys_restr)/length(Acute_RF_data$pat_key) #proportion (%) by phys_rest

table(Acute_RF_data$hispanic_ind)  #hispanic_ind totals
table(Acute_RF_data$hispanic_ind)/length(Acute_RF_data$pat_key) #total proportions for hispanic_ind

#insurance type
table(Acute_RF_data$insurance, Acute_RF_data$phys_restr) #Insurance type (N) by phys_restr
table(Acute_RF_data$insurance, Acute_RF_data$phys_restr)/length(Acute_RF_data$pat_key) #proportion (%) by phys_rest

table(Acute_RF_data$insurance)  #insurance totals
table(Acute_RF_data$insurance)/length(Acute_RF_data$pat_key) #total proportions for insurance

#obesity
table(Acute_RF_data$obesity, Acute_RF_data$phys_restr) #Obesity (N) by phys_restr
table(Acute_RF_data$obesity, Acute_RF_data$phys_restr)/length(Acute_RF_data$pat_key) #proportion (%) by phys_rest

table(Acute_RF_data$obesity)  #obesity totals
table(Acute_RF_data$obesity)/length(Acute_RF_data$pat_key) #total proportions for obesity

#dementia
table(Acute_RF_data$dementia, Acute_RF_data$phys_restr) #dementia (N) by phys_restr
table(Acute_RF_data$dementia, Acute_RF_data$phys_restr)/length(Acute_RF_data$pat_key) #proportion (%) by phys_rest

table(Acute_RF_data$dementia)  #dementia totals
table(Acute_RF_data$dementia)/length(Acute_RF_data$pat_key) #total proportions for dementia


##Outcome: disposition at discharge (alive vs dead)
table(Acute_RF_data$death, Acute_RF_data$phys_restr) #Death (N) by phys_restr
table(Acute_RF_data$death, Acute_RF_data$phys_restr)/length(Acute_RF_data$pat_key) #Proportion of death (%) by phys_restr

table(Acute_RF_data$death)  #Death totals
table(Acute_RF_data$death)/length(Acute_RF_data$pat_key) #total proportions for death

##age
by(Acute_RF_data$age, Acute_RF_data$phys_restr, summary)  #Min, max, median, mean 
by(Acute_RF_data$age, Acute_RF_data$phys_restr, sd)  #SD of age
#distribution of age for entire sample
summary(Acute_RF_data$age) #age distribution
mean(Acute_RF_data$age, na.rm = TRUE)
sd(Acute_RF_data$age, na.rm = TRUE)
sum(is.na(Acute_RF_data$age))  ##No missing data

##LOS
by(Acute_RF_data$los, Acute_RF_data$phys_restr, summary)  #Min, max, median, mean 
by(Acute_RF_data$los, Acute_RF_data$phys_restr, sd)  #SD of los
#distribution of los for entire sample
summary(Acute_RF_data$los) #los distribution
mean(Acute_RF_data$los, na.rm = TRUE)
sd(Acute_RF_data$los, na.rm = TRUE)
sum(is.na(Acute_RF_data$los))  ##No missing data



#Creating slightly different race/ethnicity variable, merging Hispanic and Hispanic Black categories d/t small n
Acute_RF_pr_data <- Acute_RF_pr_data %>%
  mutate(race_ethnicity = ifelse(race_ethnicity %in% c("Hispanic", "Hispanic_Black"), "Hispanic", race_ethnicity))

Acute_RF_data <- Acute_RF_data %>%
  mutate(race_ethnicity = ifelse(race_ethnicity %in% c("Hispanic", "Hispanic_Black"), "Hispanic", race_ethnicity))

# Drop rows where gender is 'unknown'
Acute_RF_data <- Acute_RF_data %>%
  filter(gender != 'U')


#------------------Assessing distribution in Age and LOS (continuous variables)--------------------------
#Creating dataframe for age
ARF_age <- Acute_RF_data$age
# Create a histogram of the age variable
rf_hist_age <- hist(ARF_age, plot = FALSE)
# Display the histogram as a text-based plot (age is x-axis; frequency on y-axis)
txtplot(rf_hist_age$counts)
#______________________________________________________________________


# Bivariate association Table 2a - phys_restr among Acute RF pts --------------------
chisq.test(Acute_RF_data$death, Acute_RF_data$phys_restr) #death
#chisq.test(Acute_RF_data$hispanic_ind, Acute_RF_data$phys_restr) #Hispanic ethnicity
#chisq.test(Acute_RF_data$race, Acute_RF_data$phys_restr) #race
chisq.test(Acute_RF_data$gender, Acute_RF_data$phys_restr) #gender
chisq.test(Acute_RF_data$race_ethnicity, Acute_RF_data$phys_restr) #race_ethnicity
chisq.test(Acute_RF_data$obesity, Acute_RF_data$phys_restr) #obesity
chisq.test(Acute_RF_data$dementia, Acute_RF_data$phys_restr) #dementia
chisq.test(Acute_RF_data$insurance, Acute_RF_data$phys_restr) #insurance type

t.test(filter(Acute_RF_data, phys_restr == 1)$age, filter(Acute_RF_data, phys_restr == 0)$age) #age
t.test(filter(Acute_RF_data, phys_restr == 1)$los, filter(Acute_RF_data, phys_restr == 0)$los) #length of stay


###Running simple logistic regression for variables with >2 categories ----------------
# Convert race_ethnicity to factor
Acute_RF_data$race_ethnicity <- factor(Acute_RF_data$race_ethnicity)
# Re-level race_ethnicity with White as the reference category
Acute_RF_data$race_ethnicity <- relevel(Acute_RF_data$race_ethnicity, ref = "nonHispanic_White")
# Fit logistic regression model for race_ethnicity
logmod_re2 <- glm(phys_restr ~ race_ethnicity, data = Acute_RF_data, family = binomial)
summary(logmod_re2)

#Finding ORs and 95% CIs for race_ethnicity
# Get the coefficients and their standard errors for each level of insurance
coef_and_se_re <- summary(logmod_re2)$coefficients[c(2:6), c("Estimate", "Std. Error", "Pr(>|z|)")]
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



# Convert insurance to factor
Acute_RF_data$insurance <- factor(Acute_RF_data$insurance)
# Re-level insurance with Private as the reference category
Acute_RF_data$insurance <- relevel(Acute_RF_data$insurance, ref = "private")
# Fit logistic regression model for insurance
logmod_insur2 <- glm(phys_restr ~ insurance, data = Acute_RF_data, family = binomial)
summary(logmod_insur2)

#Finding ORs and 95% CIs for insurance
# Get the coefficients and their standard errors for each level of insurance
coef_and_se_p <- summary(logmod_insur2)$coefficients[c(2:4), c("Estimate", "Std. Error", "Pr(>|z|)")]
# Calculate odds ratios
odds_ratios <- exp(coef_and_se_p[, "Estimate"])
# Calculate lower and upper bounds of 95% confidence intervals
lower_ci <- exp(coef_and_se_p[, "Estimate"] - 1.96 * coef_and_se_p[, "Std. Error"])
upper_ci <- exp(coef_and_se_p[, "Estimate"] + 1.96 * coef_and_se_p[, "Std. Error"])
# Format the output
output <- data.frame(
  OR = odds_ratios,
  Lower_CI = lower_ci,
  Upper_CI = upper_ci,
  p_value = coef_and_se_p[, "Pr(>|z|)"]
)
# Print the output
print(output)


# Fit logistic regression model for gender
logmod_gender2 <- glm(phys_restr ~ gender, data = Acute_RF_data, family = binomial)
summary(logmod_gender2)
# Get the coefficients and their standard errors gender
coef_and_se_gend <- summary(logmod_gender2)$coefficients
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


# Fit logistic regression model for obesity
logmod_obesity2 <- glm(phys_restr ~ obesity, data = Acute_RF_data, family = binomial)
summary(logmod_obesity2)
# Get the coefficients and their standard errors obesity
coef_and_se_obesity <- summary(logmod_obesity2)$coefficients
# Extract the estimate and standard error for gender
estimate_obesity <- coef_and_se_obesity[2, 1]
std_error_obesity <- coef_and_se_obesity[2, 2]
# Calculate the odds ratio
OR_obesity <- exp(estimate_obesity)
# Calculate the 95% confidence interval
lower_ci_obesity <- exp(estimate_obesity - 1.96 * std_error_obesity)
upper_ci_obesity <- exp(estimate_obesity + 1.96 * std_error_obesity)
# Print the results
cat("Odds Ratio for obesity:", OR_obesity, "\n")
cat("95% Confidence Interval for obesity: [", lower_ci_obesity, ", ", upper_ci_obesity, "]\n")


# Fit logistic regression model for dementia
logmod_dem2 <- glm(phys_restr ~ dementia, data = Acute_RF_data, family = binomial)
summary(logmod_dem2)
# Get the coefficients and their standard errors dementia
coef_and_se_dem <- summary(logmod_dem2 )$coefficients
# Extract the estimate and standard error for dementia
estimate_dem <- coef_and_se_dem[2, 1]
std_error_dem <- coef_and_se_dem[2, 2]
# Calculate the odds ratio
OR_dem <- exp(estimate_dem)
# Calculate the 95% confidence interval
lower_ci_dem <- exp(estimate_dem - 1.96 * std_error_dem)
upper_ci_dem <- exp(estimate_dem + 1.96 * std_error_dem)
# Print the results
cat("Odds Ratio for Dementia:", OR_dem, "\n")
cat("95% Confidence Interval for Dementia: [", lower_ci_dem, ", ", upper_ci_dem, "]\n")


# Fit logistic regression model for age
logmod_age2 <- glm(phys_restr ~ age, data = Acute_RF_data, family = binomial)
summary(logmod_age2)
# Get the coefficients and their standard errors age
coef_and_se_age <- summary(logmod_age2 )$coefficients
# Extract the estimate and standard error for age
estimate_age <- coef_and_se_age[2, 1]
std_error_age <- coef_and_se_age[2, 2]
# Calculate the odds ratio
OR_age <- exp(estimate_age)
# Calculate the 95% confidence interval
lower_ci_age <- exp(estimate_age - 1.96 * std_error_age)
upper_ci_age <- exp(estimate_age + 1.96 * std_error_age)
# Print the results
cat("Odds Ratio for Age:", OR_age, "\n")
cat("95% Confidence Interval for Age: [", lower_ci_age, ", ", upper_ci_age, "]\n")


# Fit logistic regression model for LOS
logmod_los2 <- glm(phys_restr ~ los2, data = Acute_RF_data, family = binomial)
summary(logmod_los2)
# Get the coefficients and their standard errors LOS
coef_and_se_los <- summary(logmod_los2 )$coefficients
# Extract the estimate and standard error for LOS
estimate_los <- coef_and_se_los[2, 1]
std_error_los <- coef_and_se_los[2, 2]
# Calculate the odds ratio
OR_los <- exp(estimate_los)
# Calculate the 95% confidence interval
lower_ci_los <- exp(estimate_los - 1.96 * std_error_los)
upper_ci_los <- exp(estimate_los + 1.96 * std_error_los)
# Print the results
cat("Odds Ratio for LOS:", OR_los, "\n")
cat("95% Confidence Interval for LOS: [", lower_ci_los, ", ", upper_ci_los, "]\n")


#-------------------------------------------------------------------------------
# Fit logistic regression model for death (as outcome) with physical restraint
logmod_pr_death2 <- glm(death ~ phys_restr, data = Acute_RF_data, family = binomial)
summary(logmod_pr_death2)
# Get the coefficients and their standard errors phys_restr
coef_and_se_pr <- summary(logmod_pr_death2)$coefficients
# Extract the estimate and standard error for phys_restr
estimate_phys_restr <- coef_and_se_pr[2, 1]
std_error_phys_restr <- coef_and_se_pr[2, 2]
# Calculate the odds ratio
OR_phys_restr <- exp(estimate_phys_restr)
# Calculate the 95% confidence interval
lower_ci_phys_restr <- exp(estimate_phys_restr - 1.96 * std_error_phys_restr)
upper_ci_phys_restr <- exp(estimate_phys_restr + 1.96 * std_error_phys_restr)
# Print the results
cat("Odds Ratio for phys_restr:", OR_phys_restr, "\n")
cat("95% Confidence Interval for phys_restr: [", lower_ci_phys_restr, ", ", upper_ci_phys_restr, "]\n")


#----------------------Assessing relationship between obesity and gender--------------------
chisq.test(Acute_RF_data$obesity, Acute_RF_data$gender) 

# Fit logistic regression model for gender with obesity as outcome
logmod_gen_obs2 <- glm(obesity ~ gender, data = Acute_RF_data, family = binomial)
summary(logmod_gen_obs2)
# Get the coefficients and their standard errors
coef_and_se_genobs <- summary(logmod_gen_obs2 )$coefficients
# Extract the estimate and standard error 
estimate_genobs <- coef_and_se_genobs[2, 1]
std_error_genobs <- coef_and_se_genobs[2, 2]
# Calculate the odds ratio
OR_genobs <- exp(estimate_genobs)
# Calculate the 95% confidence interval
lower_ci_genobs <- exp(estimate_genobs - 1.96 * std_error_genobs)
upper_ci_genobs <- exp(estimate_genobs + 1.96 * std_error_genobs)
# Print the results
cat("Odds Ratio for gender:", OR_genobs, "\n")
cat("95% Confidence Interval for gender: [", lower_ci_genobs, ", ", upper_ci_genobs, "]\n")

#-----------------------------------------------------------------------------------------




