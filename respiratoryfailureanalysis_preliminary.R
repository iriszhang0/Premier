# load and merge data ----------------------
setwd("/scratch/Premier/Raw_Data")


library(haven)
library(dplyr)
library(tidyr)
library(lme4) 
library(performance)


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

#create respiratory variables and covariates ------------

print("creating ARDS")
merged_data <- merged_data %>%
  mutate(ARDS = if_else(stringr::str_detect(all_diagnoses, "J80"), 1, 0))

print("creating J96")
merged_data <- merged_data %>%
  mutate(J96 = if_else(stringr::str_detect(all_diagnoses, "J96"), 1, 0))


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

# BMI as covariate
#merged_data$BMI

# obesity diagnosis codes BMI > 30
# Z code of str_detect "Z68.3" OR
# diagnosis of obesity "E66.0", "E66.1" "E66.2" "E66.8" "E66.9"
print("creating obesity variable")
merged_data <- merged_data %>%
  mutate(E66 = if_else(stringr::str_detect(all_diagnoses, "E66"), 1, 0),
         E66.3 = if_else(stringr::str_detect(all_diagnoses, "E66.3"), 1, 0),
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

# filter to just ARDS (J80) patients ----------------
ARDS_data <- merged_data %>%
  filter(ARDS == 1)

# drop "unknown" as missing ----------------


ARDS_data <- ARDS_data %>%
  filter(gender != "U", #dropped 47 observations
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
# table of estimates with 95% CI
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
print(Sys.time()) #approx 4 mins to run
summary(m0_hosp10)
se_0_hosp10 <- sqrt(diag(vcov(m0_hosp10)))
# table of estimates with 95% CI
tab_0_hosp10 <- cbind(Est = fixef(m0_hosp10), 
               LL = fixef(m0_hosp10) - 1.96 * se_0_hosp10,
               UL = fixef(m0_hosp10) + 1.96 * se_0_hosp10)
exp(tab_0_hosp10)
performance::icc(m0_hosp10)

## adjusted ---------------------------
print(Sys.time())
m1_hosp10 <- glmer(death ~ race_ethnicity + age + gender + insurance + (1 | prov_id), 
            data = ARDS_data_hosp10, family = binomial)
print(Sys.time()) #approx 8 mins to run
summary(m1_hosp10)

se_1_hosp10 <- sqrt(diag(vcov(m1_hosp10)))
# table of estimates with 95% CI
tab_1_hosp10 <- cbind(Est = fixef(m1_hosp10), 
               LL = fixef(m1_hosp10) - 1.96 * se_1_hosp10,
               UL = fixef(m1_hosp10) + 1.96 * se_1_hosp10)
exp(tab_1_hosp10)
performance::icc(m1_hosp10)


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

## multivariate models -----------

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

